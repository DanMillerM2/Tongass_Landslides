# =============================================================================
# R/functions.R
# Shared functions for Tongass landslide susceptibility modeling
# Wrangell and Mitkof Islands, SE Alaska
# =============================================================================

library(terra)
library(ranger)
library(mgcv)
library(survival)
library(Rcpp)
library(car)
library(tidyverse)
library(broom)
library(ggcorrplot)
library(pROC)
library(patchwork)
library(blockCV)
library(sf)
library(png)
library(jsonlite)

extract <- terra::extract   # avoid conflict with dplyr::extract


# =============================================================================
# 0. RASTER FILE MANAGEMENT
# =============================================================================
# Standard products: "initiation", "initiation_prop", "runout", "width",
#                    "runout_smooth", "width_smooth", "inundation",
#                    "inundation_prop"
#
# Workflow:
#   1. Production functions write to temp/ (overwritten every run)
#   2. Inspect the temp output
#   3. Call promote_to_stable() when satisfied
#   4. Subsequent notebooks load from stable/ via load_stable()

temp_path <- function(island, product) {
  dir <- file.path(island$raster_dir, "predictions", "temp")
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  file.path(dir, paste0(tolower(island$name), "_", product, ".tif"))
}

stable_path <- function(island, product) {
  dir <- file.path(island$raster_dir, "predictions", "stable")
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  file.path(dir, paste0(tolower(island$name), "_", product, ".tif"))
}

# Copy temp -> stable. Warns if overwriting an existing stable file.
promote_to_stable <- function(island, product) {
  src  <- temp_path(island, product)
  dest <- stable_path(island, product)
  if (!file.exists(src))
    stop("Temp raster not found: ", src)
  if (file.exists(dest))
    cat("  [!] Overwriting existing stable raster:", basename(dest), "\n")
  file.copy(src, dest, overwrite = TRUE)
  cat("  Promoted:", basename(src), "->", file.path("stable", basename(dest)), "\n")
  invisible(rast(dest))
}

# Load the stable version of a product. Errors with a helpful message if absent.
load_stable <- function(island, product) {
  p <- stable_path(island, product)
  if (!file.exists(p))
    stop("Stable raster not found: ", p,
         "\n  Generate and promote the temp version first.")
  rast(p)
}

# Save island state to raster_dir, stripping rstack and test_stack which are
# always rebuilt from disk by load_rstack(). Path is derived automatically
# from island$raster_dir and island$name so notebooks don't need to manage it.
save_island <- function(island) {
  island$rstack     <- NULL
  island$test_stack <- NULL
  path <- file.path(island$raster_dir,
                    paste0(tolower(island$name), "_state.rds"))
  saveRDS(island, path)
  cat("State saved:", path, "\n")
  invisible(path)
}

# Write a JSON sidecar alongside a raster recording model provenance.
# Stored as <raster_basename>_meta.json in the same directory.
write_raster_meta <- function(path, island, product, model = NULL, extra = list()) {
  meta <- c(
    list(
      island      = island$name,
      product     = product,
      timestamp   = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      model_class = if (!is.null(model)) class(model)[1]        else NA,
      formula     = if (!is.null(model) && !inherits(model, "ranger"))
                      paste(deparse(formula(model)), collapse = " ") else NA
    ),
    extra
  )
  meta_path <- paste0(tools::file_path_sans_ext(path), "_meta.json")
  writeLines(jsonlite::toJSON(meta, auto_unbox = TRUE, pretty = TRUE), meta_path)
  invisible(meta_path)
}


# =============================================================================
# 1. ISLAND LIST AND RASTER STACK
# =============================================================================

# Construct a new island list with standardised fields and directory structure.
# raster_dir points to the island raster directory containing all source and
# derived rasters. A predictions/ subdirectory is created there for model outputs.
new_island <- function(name, raster_dir, shape_dir, test_ext) {
  raster_dir <- normalizePath(raster_dir, mustWork = FALSE)
  shape_dir  <- normalizePath(shape_dir,  mustWork = FALSE)

  for (d in c(file.path("predictions", "temp"),
              file.path("predictions", "stable"))) {
    dir.create(file.path(raster_dir, d), showWarnings = FALSE, recursive = TRUE)
  }

  list(
      # Identity and paths
      name       = name,
      raster_dir = raster_dir,
      shape_dir  = shape_dir,

      # Training data
      df       = NULL,   # labeled dataframe (landslide + background points)
      fold_ids = NULL,   # spatial CV fold assignments

      # Test slice
      test_ext = test_ext, # c(xmin, xmax, ymin, ymax)
      test_df  = NULL,     # pixel-level predictor dataframe for test slice
      inset    = NULL,     # inset map display parameters

      # Initiation model
      init_fit  = NULL,  # fitted model object
      init_form = NULL,  # model formula
      init_type = NULL,  # "glm" | "gam" | "rf"

      # Runout stopping model
      stop_fit = NULL,   # fitted Cox model
      stop_h0  = NULL,   # baseline hazard scalar

      # Width / inundation model
      width_fit = NULL,  # fitted lm
      width_se  = NULL,  # residual SE scalar

      # Performance
      full_auc = NULL
    )
}

# Load the full raster stack for an island.
# Source rasters are read directly from raster_dir. Derived rasters (northness,
# eastness, log_grad, ls_poly, flowdir) are loaded from raster_dir if already
# saved there, otherwise computed and saved so subsequent calls are instant.
# Always returns the updated island list with both $rstack and $test_stack set.
load_rstack <- function(island) {
  raster_dir <- island$raster_dir

  cat("Loading source rasters for", island$name, "...\n")
  raster_files <- c(
    "aspect_30.flt", "FoS_pca72.flt", "FoS_pca6.flt", "Grad_30.flt",
    "logaccum.flt",  "mean_30.flt",   "norm_30.flt",   "pca_72.flt",
    "pca_6.flt",     "Prof_30.flt",   "Tan_30.flt",    "mask.flt",
    "init.flt",      "hs.tif"
  )
  rstack <- rast(file.path(raster_dir, raster_files))
  names(rstack) <- c(
    "aspect", "fos_72h", "fos_6h", "gradient", "logaccum",
    "mean_curv", "norm_curv", "area_72h", "area_6h", "profile_curv",
    "tangential_curv", "mask", "init_zone", "hs"
  )
  # rstack <- terra::mask(rstack, rstack$logaccum)

  # Load from raster_dir if already saved, otherwise compute and save.
  # Stores derived rasters alongside source rasters so they survive R restarts.
  get_derived <- function(filename, compute_fn, datatype = "FLT4S") {
    path <- file.path(raster_dir, filename)
    if (!file.exists(path)) {
      cat("  Computing", filename, "...\n")
      writeRaster(compute_fn(), path, datatype = datatype, overwrite = FALSE)
    }
    rast(path)
  }

  # Depression-filled DEM — required for valid flow routing.
  # epsilon = TRUE adds a tiny gradient to flat areas to ensure drainage.
  # Takes ~8 minutes on the full raster; saved to disk on first run.
  rstack$dem <- get_derived("dem_filled.tif", function() {
    raw_dem <- rast(file.path(raster_dir, "elev_2m.flt"))
    fill(raw_dem, epsilon = TRUE)
  }, datatype = "FLT4S")

  # Flow direction (D8) — computed from the filled DEM.
  # Takes ~25 seconds; saved to disk on first run.
  rstack$flowdir <- get_derived("flowdir.tif", function() {
    terrain(rstack$dem, v = "flowdir")
  }, datatype = "INT2S")

  rstack$northness <- get_derived("northness.tif", function() cos(rstack$aspect))
  rstack$eastness  <- get_derived("eastness.tif",  function() sin(rstack$aspect))
  rstack$log_grad  <- get_derived("log_grad.tif",  function() log10(rstack$gradient))

  rstack$ls_poly <- get_derived("ls_poly.tif", function() {
    ls_poly <- vect(file.path(island$shape_dir, "LS_poly.shp"))
    init_poly <- vect(file.path(island$shape_dir, "init_poly.shp"))
    keep <- rowSums(relate(ls_poly, init_poly, relation = "intersects", pairs = FALSE)) > 0
    ls_poly <- ls_poly[keep,]
    rasterize(ls_poly, rstack$dem)
  }, datatype = "INT2S")

  cat("  rstack ready:", nlyr(rstack), "layers\n")
  island$rstack     <- rstack
  island$test_stack <- crop(rstack, ext(island$test_ext))
  cat("  test_stack cropped to test extent\n")

  island
}

# Sample background and landslide training points and extract all predictor
# values. This is the slow step (~minutes). Run once, then save the island
# list and reload — load_rstack() rebuilds the rstack separately each session.
# Returns the updated island list with $df set.
sample_training_data <- function(island, N_bg = 5000, n_ls = 10) {
  rstack    <- island$rstack
  shape_dir <- island$shape_dir

  cat("Loading shapefiles...\n")
  ls_pnt  <- vect(file.path(shape_dir, "LS_pnts.shp"))
  ls_poly <- vect(file.path(shape_dir, "LS_poly.shp"))
  soils   <- vect(file.path(shape_dir, "soils.shp"))
  lith    <- vect(file.path(shape_dir, "lith.shp"))
  forest  <- vect(file.path(shape_dir, "cover.shp"))
  init_r  <- rast(file.path(island$raster_dir, "init.flt"))

  cat("Updating mask to exclude known slide areas...\n")
  slides  <- rbind(forest[forest$fprod == "S"], ls_poly)
  ls_rast <- rasterize(slides, rstack$mask, field = 1, background = NA)
  mask_r  <- mask(rstack$mask, ls_rast, maskvalues = 1)

  cat(paste("Sampling", N_bg, "background points...\n"))
  bg_cells  <- sample(which(!is.na(values(mask_r))), N_bg)
  bg_coords <- vect(xyFromCell(mask_r, bg_cells), crs = crs(mask_r))

  if (n_ls > 0) {
    cat(paste("Sampling", n_ls, "points per initiation zone...\n"))
    init_pts <- as.points(init_r, values = TRUE, na.rm = TRUE)
    sampled_idx <- unlist(tapply(seq_len(nrow(init_pts)), init_pts$init,
                                 function(idx)
                                   if (length(idx) <= n_ls) idx
                                   else sample(idx, n_ls)))
    ls_pnt    <- init_pts[sampled_idx, ]
    ex_method <- "simple"
  } else {
    ex_method <- "bilinear"
  }

  b_inds <- !names(rstack) %in% c("mask", "init_zone", "hs", "flowdir", "ls_poly")

  cat("Extracting predictors to landslide points...\n")
  ls_vals        <- extract(rstack[[b_inds]], ls_pnt, bind = FALSE, method = ex_method)
  ls_vals$lith   <- extract(lith,   ls_pnt)$STATE_UNIT
  ls_vals$soils  <- extract(soils,  ls_pnt)$FOR_SMU
  forest_ls      <- extract(forest, ls_pnt)
  forest_ls      <- forest_ls[!duplicated(forest_ls$id.y), ]
  ls_vals$forest <- forest_ls$ftype
  ls_vals$label  <- 1L

  cat("Extracting predictors to background points...\n")
  bg_vals        <- extract(rstack[[b_inds]], bg_coords, bind = FALSE, method = "simple")
  bg_vals$lith   <- extract(lith,   bg_coords)$STATE_UNIT
  bg_vals$soils  <- extract(soils,  bg_coords)$FOR_SMU
  forest_bg      <- extract(forest, bg_coords)
  forest_bg      <- forest_bg[!duplicated(forest_bg$id.y), ]
  bg_vals$forest <- forest_bg$ftype
  bg_vals$label  <- 0L

  ls_vals$forest[is.na(ls_vals$forest)] <- "U"
  bg_vals$forest[is.na(bg_vals$forest)] <- "U"

  ls_coords    <- as.data.frame(geom(ls_pnt)[, c("x", "y")])
  bg_coords_df <- as.data.frame(crds(bg_coords))
  ls_vals$x <- ls_coords$x;    ls_vals$y <- ls_coords$y
  bg_vals$x <- bg_coords_df$x; bg_vals$y <- bg_coords_df$y

  df       <- rbind(ls_vals, bg_vals) |> na.omit()
  df$label <- as.factor(df$label)

  cat("Done.", nrow(df), "total points (", sum(df$label == 1), "LS /",
      sum(df$label == 0), "BG )\n")
  island$df <- df
  island
}


# Collapse low-frequency categorical levels to "U" to prevent missing-level
# errors during prediction.
recode_rare <- function(df, cols, threshold = 0.10) {
  df[cols] <- lapply(df[cols], function(col) {
    freq <- prop.table(table(col))
    rare <- names(freq[freq < threshold])
    col[col %in% rare] <- "U"
    col
  })
  df
}

# Plot the test-slice extent with mask, initiation zones, and training points.
test_map <- function(island) {
  test_stack <- island$test_stack
  e <- ext(test_stack)
  df_crop <- island$df[island$df$x >= e[1] & island$df$x <= e[2] &
                       island$df$y >= e[3] & island$df$y <= e[4], ]

  par(mar = c(4, 4, 4, 8))
  plot(test_stack$hs, col = gray.colors(256), legend = FALSE,
       main = paste(island$name, "Test Map"))
  plot(test_stack$mask, col = adjustcolor("purple", alpha.f = 0.2),
       legend = FALSE, axes = FALSE, add = TRUE)
  plot(test_stack$init_zone, col = adjustcolor("red", alpha.f = 0.4),
       legend = FALSE, axes = FALSE, add = TRUE)
  points(df_crop$x, df_crop$y, pch = 21,
         bg = ifelse(df_crop$label == 0, "blue", "red"),
         col = "black", cex = 0.5)
  legend("right", inset = c(-0.05, 0),
         legend = c("Mask", "Initiation Zone", "Training BG", "Training LS"),
         fill   = c(adjustcolor("purple", alpha.f = 0.1),
                    adjustcolor("red",    alpha.f = 0.2), NA, NA),
         border = c("purple", "red", NA, NA),
         pch    = c(NA, NA, 21, 21),
         pt.bg  = c(NA, NA, "blue", "red"),
         bty = "n", title = "Layers", xpd = NA)
}

# Build pixel-level predictor dataframe for the test slice.
# Each row is one masked pixel; includes categorical predictors from shapefiles.
# shape_dir is read from island$shape_dir.
test_df <- function(island) {
  stack     <- island$test_stack
  shape_dir <- island$shape_dir
  all_cells <- which(!is.na(values(stack$mask)))
  coords    <- vect(xyFromCell(stack$mask, all_cells), crs = crs(stack$mask))

  b_inds <- !names(stack) %in% c("mask", "init_zone", "hs", "flowdir", "ls_poly")
  df     <- as.data.frame(extract(stack[[b_inds]], coords, method = "simple"))

  lith   <- vect(file.path(shape_dir, "lith.shp"))
  soils  <- vect(file.path(shape_dir, "soils.shp"))
  forest <- vect(file.path(shape_dir, "cover.shp"))

  df$lith  <- extract(lith,  coords)$STATE_UNIT
  df$soils <- extract(soils, coords)$FOR_SMU
  forest_e <- extract(forest, coords)
  forest_e <- forest_e[!duplicated(forest_e$id.y), ]
  df$forest <- forest_e$ftype
  df$forest[is.na(df$forest)] <- "U"

  df$ind <- all_cells   # cell indices for mapping predictions back to raster
  df
}


# =============================================================================
# 2. INITIATION MODEL FITTING
# =============================================================================

glm_fit <- function(formula, train_df) {
  glm(formula, train_df, family = binomial())
}

gam_fit <- function(formula, train_df) {
  mgcv::gam(formula, data = train_df, family = binomial, method = "REML")
}

rf_fit <- function(formula, train_df, ntrees = 500, min_node = 10) {
  tryCatch(
    ranger(formula, data = train_df, num.trees = ntrees,
           min.node.size = min_node, mtry = 1, importance = "impurity",
           num.threads = parallel::detectCores() - 1, probability = TRUE),
    error = function(e) NULL
  )
}

# Dispatch prediction across GLM, GAM, and RF model classes.
predi <- function(model, df) {
  if (class(model)[1] == "ranger") {
    predict(model, data = df,
            num.threads = parallel::detectCores() - 1)$predictions[, 2]
  } else {
    predict(model, newdata = df, type = "response")
  }
}


# =============================================================================
# 3. INITIATION MODEL EVALUATION
# =============================================================================

# Remap any susceptibility raster to landslide percentile proportions (0-100).
# A value of 100 means this pixel exceeds all observed landslide cells in
# susceptibility. Allows fair comparison across model types and islands.
suscept_prop <- function(init, susceptibility) {
  init_vals <- values(init)
  susc_vals <- values(susceptibility)
  init_vals[susc_vals == 0] <- NA
  ls_susc   <- susc_vals[!is.na(init_vals)]
  ls_susc   <- ls_susc[!is.na(ls_susc)]
  ecdf_fn   <- ecdf(ls_susc)
  app(susceptibility, function(x) ecdf_fn(x) * 100)
}

# Compute AUC from paired proportion and initiation-zone rasters,
# without loading all values into a single object.
full_auc <- function(init, prop) {
  init_mask <- !is.na(values(init))
  susc_vals <- values(prop)
  pos_s <- sort(susc_vals[ init_mask & !is.na(susc_vals)])
  neg_s <- sort(susc_vals[!init_mask & !is.na(susc_vals)])
  thresholds <- seq(0, 100, length.out = 500)
  tpr <- 1 - findInterval(thresholds, pos_s) / length(pos_s)
  fpr <- 1 - findInterval(thresholds, neg_s) / length(neg_s)
  tpr[1] <- 1; fpr[1] <- 1
  abs(sum(diff(fpr) * (head(tpr, -1) + tail(tpr, -1)) / 2))
}

# Three-panel covariate diagnostic: correlation heatmap, marginal GLM
# coefficients, and variance inflation factors. Grouped by predictor type.
cov_check <- function(island) {
  vars <- c("gradient", "fos_72h", "fos_6h",
            "logaccum", "area_72h", "area_6h",
            "mean_curv", "tangential_curv", "norm_curv",
            "aspect", "northness", "eastness")
  groups <- list(
    Slope     = c("gradient", "fos_72h", "fos_6h"),
    Area      = c("logaccum", "area_72h", "area_6h"),
    Curvature = c("mean_curv", "tangential_curv", "norm_curv"),
    Aspect    = c("aspect", "northness", "eastness")
  )
  if (island$name == "Mitkof") {
    vars <- c(vars, "profile_curv")
    groups$Curvature <- c(groups$Curvature, "profile_curv")
  }

  train_df     <- island$df
  group_lookup <- imap_dfr(groups, ~ tibble(term = .x, group = .y))
  train_scaled <- train_df |>
    mutate(across(all_of(vars), ~ as.numeric(scale(.))))

  cor_matrix <- cor(train_scaled[, vars], use = "complete.obs")
  p1 <- ggcorrplot(cor_matrix, hc.order = FALSE, type = "lower",
                   lab = TRUE, lab_size = 2.5,
                   colors = c("#4575b4", "white", "#d73027"),
                   title = "Feature Correlation")

  marginal_coefs <- map_dfr(vars, function(v) {
    tidy(glm_fit(as.formula(paste("label ~", v)), train_scaled)) |>
      filter(term == v)
  })
  marginal_df <- marginal_coefs |>
    left_join(group_lookup, by = "term") |>
    mutate(term = fct_reorder(term, abs(estimate)))
  p2 <- ggplot(marginal_df, aes(x = term, y = abs(estimate), fill = group)) +
    geom_col() + coord_flip() +
    labs(title = "Scaled individual fit coefficient",
         x = NULL, y = "|Coefficient|", fill = "Group") +
    theme_minimal()

  full_formula <- as.formula(paste("label ~", paste(vars, collapse = " + ")))
  vif_df <- tibble(term = names(vif(glm_fit(full_formula, train_scaled))),
                   VIF  = vif(glm_fit(full_formula, train_scaled))) |>
    left_join(group_lookup, by = "term") |>
    mutate(term = fct_reorder(term, desc(VIF)))
  p3 <- ggplot(vif_df, aes(x = term, y = VIF, fill = group)) +
    geom_col() +
    geom_hline(yintercept = 5,  linetype = "dashed", colour = "orange") +
    geom_hline(yintercept = 10, linetype = "dashed", colour = "red") +
    coord_flip(ylim = c(0, 15)) +
    labs(title = "Variance Inflation Factors",
         x = NULL, y = "VIF", fill = "Group") +
    theme_minimal()

  (p1 | (p2 + theme(legend.position = "none") | p3)) +
    plot_annotation(title = island$name)
}

# Spatial k-fold CV grid search over predictor combinations and (for RF)
# hyperparameters. Returns results sorted by descending mean AUC.
grid_search_cv <- function(island, predictor_list, model,
                           ntrees_list = c(300, 500),
                           min_nodes_list = c(5, 10)) {
  df       <- island$df
  fold_ids <- island$fold_ids
  folds    <- unique(fold_ids)

  param_grid <- if (model == "rf") {
    expand.grid(pred_idx  = seq_along(predictor_list),
                ntrees    = ntrees_list,
                min_nodes = min_nodes_list)
  } else {
    data.frame(pred_idx = seq_along(predictor_list))
  }

  total   <- nrow(param_grid)
  counter <- 0L
  cat(paste("Evaluating", model, "models for", island$name, "\n"))

  results <- pmap_dfr(param_grid, function(pred_idx,
                                           ntrees    = NULL,
                                           min_nodes = NULL) {
    counter <<- counter + 1L
    cat(sprintf("\r[%d / %d] (%.1f%%)", counter, total, 100 * counter / total))
    flush.console()

    predictors <- predictor_list[[pred_idx]]
    formula    <- as.formula(paste("label ~", paste(predictors, collapse = " + ")))

    fold_aucs <- map_dbl(folds, function(k) {
      train <- df[fold_ids != k, ]
      test  <- df[fold_ids == k, ]
      model_fit <- if (model == "rf") {
        rf_fit(formula, train, ntrees = ntrees, min_node = min_nodes)
      } else if (model == "gam") {
        gam_terms <- ifelse(sapply(train[predictors], is.numeric),
                            paste0("s(", predictors, ", k=4)"), predictors)
        gam_fit(as.formula(paste("label ~", paste(gam_terms, collapse = " + "))), train)
      } else {
        glm_fit(formula, train)
      }
      probs   <- predi(model_fit, test)
      roc_obj <- pROC::roc(test$label, probs, quiet = TRUE)
      as.numeric(pROC::auc(roc_obj))
    })

    row <- tibble(island     = island$name,
                  model      = model,
                  predictors = paste(predictors, collapse = ", "),
                  mean_auc   = mean(fold_aucs),
                  sd_auc     = sd(fold_aucs))
    if (model == "rf") { row$ntrees <- ntrees; row$min_nodes <- min_nodes }
    row
  })
  arrange(results, desc(mean_auc))
}


# =============================================================================
# 4. RUNOUT SURVIVAL MODEL FITTING
# =============================================================================

# Load and fully prepare the centerline node dataset for survival modeling.
# Handles renaming, dem extraction, drop calculation, tstart/tstop/event setup,
# log_grad, start_grad, and rolling gradient/curvature means.
# Returns a data.frame ready for coxph() or glm() fitting.
prepare_nodes <- function(nodes_path, rstack) {
  nodes <- vect(nodes_path)
  nodes$logaccum <- log10(nodes$UpArea_m2)
  names(nodes)[names(nodes) == "Grad"] <- "gradient"
  names(nodes)[names(nodes) == "Tan"]  <- "tangential_curv"

  nodes <- extract(rstack$dem, nodes, bind = TRUE, method = "simple")
  nodes <- nodes[!is.na(nodes$dem), ]

  # Elevation drop relative to start of each flow path
  start_elev  <- tapply(nodes$dem[nodes$UpDist_m == 0],
                        nodes$Polygon[nodes$UpDist_m == 0], mean)
  nodes$drop  <- start_elev[as.character(nodes$Polygon)] - nodes$dem

  # Convert to data.frame and sort
  nodes_df <- as.data.frame(nodes)
  nodes_df <- nodes_df[order(nodes_df$Polygon, nodes_df$UpDist_m), ]
  nodes_df$log_grad <- log10(nodes_df$gradient)

  # Add survival time columns and event indicator
  nodes_df <- do.call(rbind, lapply(split(nodes_df, nodes_df$Polygon), function(df) {
    df$tstart <- df$UpDist_m
    df$tstop  <- c(df$UpDist_m[-1], df$UpDist_m[nrow(df)] + 1)
    df$event  <- c(rep(0L, nrow(df) - 1), 1L)
    df
  }))
  nodes_df <- nodes_df[nodes_df$tstart < nodes_df$tstop, ]

  # Start gradient (at initiation point, UpDist_m == 0)
  start_grad <- tapply(nodes_df$gradient[nodes_df$UpDist_m == 0],
                       nodes_df$Polygon[nodes_df$UpDist_m == 0], mean)
  nodes_df$start_grad <- start_grad[as.character(nodes_df$Polygon)]

  # Rolling means: grad_roll over 5 nodes, tan_roll over 16 nodes
  nodes_df <- do.call(rbind, lapply(split(nodes_df, nodes_df$Polygon), function(df) {
    df$grad_roll <- sapply(seq_len(nrow(df)), function(i)
      if (i == 1) df$gradient[1]
      else mean(df$gradient[max(1, i - 4):i], na.rm = TRUE))
    df$tan_roll <- sapply(seq_len(nrow(df)), function(i)
      if (i == 1) df$tangential_curv[1]
      else mean(df$tangential_curv[max(1, i - 15):i], na.rm = TRUE))
    df
  }))

  # Log-transformed width — used for width modeling (NA where Width_m <= 0)
  # Filter to Width_m > 0 in the modeling notebook before fitting width model
  nodes_df$log_width <- ifelse(nodes_df$Width_m > 0, log10(nodes_df$Width_m), NA)

  # Drop start nodes — deposition cannot occur at the initiation pixel
  nodes_df <- nodes_df[nodes_df$UpDist_m != 0, ]
  nodes_df
}

# Compute the mean baseline hazard h0 from a fitted Cox model.
# Used to scale the linear predictor back to an absolute hazard rate
# when applying the model to a raster.
compute_h0 <- function(cox_model) {
  lp_train    <- predict(cox_model, type = "lp")
  y           <- cox_model$y
  event_times <- sort(unique(y[, "stop"][y[, "status"] == 1]))
  h0 <- sapply(event_times, function(t) {
    in_risk <- (y[, "start"] < t) & (y[, "stop"] >= t)
    sum(y[, "status"] == 1 & y[, "stop"] == t) / sum(exp(lp_train[in_risk]))
  })
  mean(h0, na.rm = TRUE)
}

# Plot cumulative survival curves for all flow paths with endpoint histogram.
# Supports Cox (coxph) and logistic (glm) models.
plot_runout_test <- function(form,island) {
  df <- island$nodes_df
  if (startsWith(deparse(form), "Surv(")[1]) {
    model <- coxph(form,
          data = df,
          na.action = na.omit,
          model = TRUE)
    m_name     <- "Survival Analysis"
    df$hazard  <- predict(model, type = "expected")
    island$stop_h0 <- compute_h0(model)
  } else {
    model <- glm(event ~ log_grad + tangential_curv + logaccum+dem,
                 data = df,
                 family = binomial,
                 model = TRUE)
    m_name     <- "Logistic Regression"
    df$hazard  <- predict(model, type = "response")
  }
  summary(model)
  
  df <- do.call(rbind, lapply(split(df, df$Polygon), function(d) {
    d <- d[order(d$UpDist_m), ]
    d$surv_prob <- exp(-cumsum(d$hazard))
    d
  }))
  endpoints <- do.call(rbind, lapply(split(df, df$Polygon), function(d) d[nrow(d), ]))

  cat(paste("Runout nodes with low probability (bad!):",
            sum(df$surv_prob <= 0.1, na.rm = TRUE), "\n"))
  cat(paste("Endpoints with low probability (good!):",
            sum(endpoints$surv_prob <= 0.1, na.rm = TRUE), "\n"))

  layout(matrix(c(1, 2), nrow = 1), widths = c(3, 1))
  par(mar = c(5, 4, 4, 0))
  plot(NULL, xlim = range(df$UpDist_m, na.rm = TRUE), ylim = c(0, 1),
       xlab = "Runout distance (m)", ylab = "P(flow passes node)",
       main = paste(island$name,m_name))
  invisible(lapply(split(df, df$Polygon), function(d) {
    d <- d[order(d$UpDist_m), ]
    lines(d$UpDist_m, d$surv_prob, col = "black", lwd = 0.5)
  }))
  points(endpoints$UpDist_m, endpoints$surv_prob,
         pch = 21, cex = 1.2, col = "black", bg = "yellow")

  h <- hist(endpoints$surv_prob, breaks = 20, plot = FALSE)
  par(mar = c(5, 0, 4, 2))
  barplot(h$counts, horiz = TRUE, space = 0,
          xlim = c(0, 30), ylim = c(0, length(h$breaks) - 1),
          col = "yellow", border = "black",
          xlab = "Endpoint\nprobabilities", axes = TRUE)
  layout(1)
  
  run_test <- runout(island$test_stack$ls_prob,island,island$test_stack,model)
  plot(run_test)
  
  return(model)
}


# =============================================================================
# 5. RUNOUT PREDICTION
# =============================================================================
buffer_streams <- function(ls_prob,logaccum){
  # 1. Find cell indices where logaccum > 6
  stream_cells <- which(values(logaccum) > 6)
  
  # 2. Convert to row/col, expand by 50 m radius in pixel units
  pixel_res <- res(logaccum)[1]
  r_px      <- ceiling(50 / pixel_res)
  nc        <- ncol(logaccum)
  nr        <- nrow(logaccum)
  
  rc        <- rowColFromCell(logaccum, stream_cells)
  
  # 3. Generate all neighbor cells within the radius
  offsets    <- expand.grid(dr = -r_px:r_px, dc = -r_px:r_px)
  offsets    <- offsets[sqrt(offsets$dr^2 + offsets$dc^2) <= r_px, ]  # circular
  
  buf_cells <- unique(unlist(lapply(seq_len(nrow(rc)), function(i) {
    rows <- rc[i, 1] + offsets$dr
    cols <- rc[i, 2] + offsets$dc
    keep <- rows >= 1 & rows <= nr & cols >= 1 & cols <= nc
    cellFromRowCol(logaccum, rows[keep], cols[keep])
  })))
  
  # 4. Apply the mask
  ls_prob_masked        <- ls_prob
  ls_prob_masked[buf_cells] <- NA
  return(ls_prob_masked)
}



# C++ core: D8 flow routing with integer-scaled probabilities.
# Processes cells from highest to lowest elevation, decaying flow probability
# by local survival and combining probabilities from multiple upslope sources.
cppFunction('
NumericVector route_flow_cpp(IntegerVector flow_prob_int,
                              IntegerVector surv_vals_int,
                              IntegerVector fdir_vals,
                              IntegerVector sorted_idx,
                              int nrows, int ncols) {
  int n = flow_prob_int.size();
  std::vector<float> result(n);
  std::vector<float> surv(n);

  for (int i = 0; i < n; i++) {
    result[i] = flow_prob_int[i] == NA_INTEGER ?
      std::numeric_limits<float>::quiet_NaN() : flow_prob_int[i] / 10000000.0f;
    surv[i] = surv_vals_int[i] == NA_INTEGER ?
      std::numeric_limits<float>::quiet_NaN() : surv_vals_int[i] / 10000000.0f;
  }

  for (int k = 0; k < sorted_idx.size(); k++) {
    int i = sorted_idx[k] - 1;
    if (std::isnan(result[i]) || std::isnan(surv[i])) continue;

    result[i] = surv[i] * result[i];

    int fdir = fdir_vals[i];
    int row  = i / ncols;
    int col  = i % ncols;
    int j    = -1;

    if      (fdir == 1)   { if (col < ncols-1)                  j = i + 1; }
    else if (fdir == 2)   { if (row < nrows-1 && col < ncols-1) j = i + ncols + 1; }
    else if (fdir == 4)   { if (row < nrows-1)                  j = i + ncols; }
    else if (fdir == 8)   { if (row < nrows-1 && col > 0)       j = i + ncols - 1; }
    else if (fdir == 16)  { if (col > 0)                        j = i - 1; }
    else if (fdir == 32)  { if (row > 0 && col > 0)             j = i - ncols - 1; }
    else if (fdir == 64)  { if (row > 0)                        j = i - ncols; }
    else if (fdir == 128) { if (row > 0 && col < ncols-1)       j = i - ncols + 1; }

    if (j >= 0 && j < n && !std::isnan(result[j]))
      result[j] = 1.0f - (1.0f - result[j]) * (1.0f - result[i]);
  }

  NumericVector out(n);
  for (int i = 0; i < n; i++)
    out[i] = std::isnan(result[i]) ? NA_REAL : (double)result[i];
  return out;
}
')

# Route initiation probabilities downslope using a Cox or GLM stopping model.
# init_pred:  raster of initiation probabilities from the initiation model
# island:     island list (must contain stop_h0 for Cox models)
# stack:      raster stack to predict from (test_stack or rstack)
# hazard_mod: fitted Cox or GLM stopping model
runout <- function(init_pred, island, stack, hazard_mod, w = FALSE) {

  ls_prob   <- ifel(is.na(stack$dem), NA, ifel(is.na(init_pred), 0, init_pred / 500))
  rm(init_pred); gc()
  flow_prob <- as.integer(values(ls_prob) * 10000000L)
  rm(ls_prob); gc()

  if (inherits(hazard_mod, "coxph")) {
    has_spline <- any(sapply(names(hazard_mod$pterms), function(x) x != ""))

    if (has_spline) {
      pred_vars  <- all.vars(hazard_mod$formula)
      pred_vars  <- pred_vars[pred_vars %in% names(stack)]
      n          <- ncell(stack$dem)
      nchunks    <- 200
      chunk_size <- ceiling(n / nchunks)
      lp_vals    <- rep(NA_real_, n)

      for (i in seq_len(nchunks)) {
        idx_start <- (i - 1) * chunk_size + 1
        idx_end   <- min(i * chunk_size, n)
        cells     <- idx_start:idx_end
        df_chunk  <- as.data.frame(
          lapply(pred_vars, function(v) stack[[v]][cells]),
          col.names = pred_vars)
        valid <- complete.cases(df_chunk)
        if (sum(valid) == 0) next
        lp_vals[idx_start:idx_end][valid] <- predict(hazard_mod,
                                                      newdata = df_chunk[valid, ],
                                                      type = "lp")
        rm(df_chunk); gc()
      }
      lp <- rast(stack$dem); values(lp) <- lp_vals; rm(lp_vals); gc()
    } else {
      lp <- predict(stack, hazard_mod, type = "lp")
    }

    hazard <- 1 - exp(-island$stop_h0 * exp(lp))
    rm(lp); gc()

  } else if (inherits(hazard_mod, "glm")) {
    hazard <- predict(stack, hazard_mod, type = "response")
  }

  surv_vals  <- as.integer((1 - values(hazard)) * 10000000L); rm(hazard); gc()
  fdir_vals  <- as.integer(values(stack$flowdir))
  sorted_idx <- as.integer(order(values(stack$dem), decreasing = TRUE, na.last = NA))
  gc()

  out_prob <- route_flow_cpp(
    flow_prob_int = flow_prob, surv_vals_int = surv_vals,
    fdir_vals     = fdir_vals, sorted_idx    = sorted_idx,
    nrows = nrow(stack$dem),  ncols = ncol(stack$dem))
  rm(flow_prob, surv_vals, fdir_vals, sorted_idx); gc()

  out_rast <- rast(stack$dem)
  values(out_rast) <- out_prob
  
  if(w == TRUE){
    p <- temp_path(island, "runout")
    writeRaster(out_rast, p, overwrite = TRUE)
    write_raster_meta(p, island, "runout", hazard_mod)
    cat("  Runout written to temp:", basename(p), "\n")
  }
  return(out_rast)
}


# =============================================================================
# 6. INUNDATION / WIDTH
# =============================================================================

# C++ core: lateral spreading kernel.
# For each non-zero runout cell, spreads probability outward using a circular
# kernel with log-normal width decay. Takes max across overlapping spreads.
cppFunction('
NumericVector inundate_cpp(NumericVector out_vals,
                            IntegerVector width_vals,
                            IntegerVector radius,
                            int nrows, int ncols,
                            double res_m, double se) {
  NumericVector result = clone(out_vals);
  int n = out_vals.size();

  for (int i = 0; i < n; i++) {
    if (NumericVector::is_na(out_vals[i]) || out_vals[i] == 0) continue;

    int r   = radius[i];
    int row = i / ncols;
    int col = i % ncols;

    int row_min = std::max(0, row - r);
    int row_max = std::min(nrows - 1, row + r);
    int col_min = std::max(0, col - r);
    int col_max = std::min(ncols - 1, col + r);

    for (int nr = row_min; nr <= row_max; nr++) {
      for (int nc = col_min; nc <= col_max; nc++) {
        int j = nr * ncols + nc;
        double dist_m = sqrt(pow((double)(nr - row), 2) +
                             pow((double)(nc - col), 2)) * res_m;
        if (dist_m == 0) dist_m = res_m * 0.5;
        double p_reach = 1 - R::pnorm((log10(2 * dist_m) -
                                       log10(width_vals[i])) / se,
                                       0, 1, 1, 0);
        double decayed = out_vals[i] * p_reach;
        if (decayed > result[j]) result[j] = decayed;
      }
    }
  }
  return result;
}
')

# C++ core: joint flowpath smoother for runout AND width together.
# Walks upstream (half_window steps) and partially downstream (half_window/2)
# from each high-runout cell, averaging both runout probability and width.
# Returns a List with named elements "runout" and "width".
cppFunction('
List smooth_flowpath_cpp(NumericVector vals,
                                IntegerVector width_vals,
                                IntegerVector fdir_vals,
                                IntegerVector sorted_idx,
                                int nrows, int ncols,
                                int half_window,
                                double threshold) {
  int n = vals.size();

  std::vector<int> downstream(n, -1);
  for (int i = 0; i < n; i++) {
    if (IntegerVector::is_na(fdir_vals[i])) continue;
    int fdir = fdir_vals[i];
    int row  = i / ncols;
    int col  = i % ncols;
    int j    = -1;
    if      (fdir == 1)   { if (col < ncols-1)                  j = i + 1; }
    else if (fdir == 2)   { if (row < nrows-1 && col < ncols-1) j = i + ncols + 1; }
    else if (fdir == 4)   { if (row < nrows-1)                  j = i + ncols; }
    else if (fdir == 8)   { if (row < nrows-1 && col > 0)       j = i + ncols - 1; }
    else if (fdir == 16)  { if (col > 0)                        j = i - 1; }
    else if (fdir == 32)  { if (row > 0 && col > 0)             j = i - ncols - 1; }
    else if (fdir == 64)  { if (row > 0)                        j = i - ncols; }
    else if (fdir == 128) { if (row > 0 && col < ncols-1)       j = i - ncols + 1; }
    if (j >= 0 && j < n) downstream[i] = j;
  }

  std::vector<int> upstream(n, -1);
  for (int i = 0; i < n; i++) {
    int j = downstream[i];
    if (j < 0 || j >= n) continue;
    if (upstream[j] == -1) {
      upstream[j] = i;
    } else {
      int cur_best = upstream[j];
      bool i_better = !NumericVector::is_na(vals[i]) &&
                      (NumericVector::is_na(vals[cur_best]) || vals[i] > vals[cur_best]);
      if (i_better) upstream[j] = i;
    }
  }

  NumericVector result_vals  = clone(vals);
  IntegerVector result_width = clone(width_vals);

  for (int k = 0; k < sorted_idx.size(); k++) {
    int i = sorted_idx[k] - 1;
    if (NumericVector::is_na(vals[i]) || vals[i] < threshold) continue;

    double sum_v = 0.0, sum_w = 0.0;
    int count = 0;

    int cur = i;
    for (int w = 0; w <= half_window; w++) {
      if (cur < 0 || cur >= n) break;
      if (!NumericVector::is_na(vals[cur])) {
        sum_v += vals[cur];
        sum_w += NumericVector::is_na(width_vals[cur]) ? 0.0 : width_vals[cur];
        count++;
      }
      cur = upstream[cur];
      if (cur < 0) break;
    }

    cur = downstream[i];
    for (int w = 0; w < half_window/2; w++) {
      if (cur < 0 || cur >= n) break;
      if (!NumericVector::is_na(vals[cur])) {
        sum_v += vals[cur];
        sum_w += NumericVector::is_na(width_vals[cur]) ? 0.0 : width_vals[cur];
        count++;
      }
      cur = downstream[cur];
    }

    if (count > 0) {
      result_vals[i]  = sum_v / count;
      result_width[i] = sum_w / count;
    }
  }

  return List::create(Named("runout") = result_vals,
                      Named("width")  = result_width);
}
')

# Expand runout probabilities laterally using log-normal width uncertainty.
# to_disk: if TRUE, write to temp_path(island, "inundation").
inundate <- function(runout, width, se, island, w = FALSE) {
  inundation <- rast(runout)
  out_vals   <- values(runout)[, 1]
  width_vals <- as.integer(values(width)[, 1])
  nrows      <- nrow(runout)
  ncols      <- ncol(runout)
  res_m      <- res(runout)[1]
  radius     <- round(10^(log10(width_vals) + 2 * se) / 2 / res_m)
  radius[is.na(radius)] <- 0L

  result <- inundate_cpp(out_vals, width_vals, as.integer(radius),
                         nrows, ncols, res_m, se)
  values(inundation) <- result
  
  if(w==TRUE){
    p <- temp_path(island, "inundation")
    writeRaster(inundation, p, overwrite = TRUE)
    write_raster_meta(p, island, "inundation")
    cat("  Inundation written to temp:", basename(p), "\n")
  }
  return(inundation)
}

# Smooth runout probabilities and width predictions jointly along flow paths.
# Writes intermediate outputs to temp/ for memory management — these are
# always overwritten and are not intended as stable products.
# Returns list(runout, width) as file-backed rasters.
smooth_flowpath <- function(runout, width, dem, flowdir, island,
                            half_window = 10, threshold = 0.3, w = FALSE) {
  rows       <- nrow(dem)
  cols       <- ncol(dem)
  sorted_idx <- as.integer(order(values(dem), decreasing = TRUE, na.last = NA))
  rm(dem); gc()

  vals       <- values(runout)[, 1]
  width_vals <- as.integer(values(width)[, 1])
  fdir_vals  <- as.integer(values(flowdir))

  out <- smooth_flowpath_cpp(vals, width_vals, fdir_vals, sorted_idx,
                                   rows, cols, half_window, threshold)
  rm(vals, width_vals, fdir_vals, sorted_idx); gc()
  
  if(w == TRUE){
    runout_path <- temp_path(island, "runout_smooth")
    width_path  <- temp_path(island, "width_smooth")
  
    runout_out <- rast(runout); values(runout_out) <- out$runout
    writeRaster(runout_out, runout_path, overwrite = TRUE)
  
    width_out  <- rast(width);  values(width_out)  <- out$width
    writeRaster(width_out, width_path, overwrite = TRUE)
    
    return(list(runout = rast(runout_path), width = rast(width_path)))
  } else {
    return(list(runout = runout_out, width = width_out))
  }
  rm(out); gc()
}


# =============================================================================
# 7. VISUALIZATION
# =============================================================================

# Map initiation model predictions on the test slice with optional inset.
# Computes and displays AUC for the test-slice area.
map_test_pred <- function(model, island, inset = TRUE) {
  df     <- island$test_df
  tstack <- island$test_stack
  mclass <- class(model)[1]

  pred <- predi(model, df)
  if (mclass == "ranger") {
    preds  <- model$forest$independent.variable.names
    mclass <- "RF"
  } else {
    preds <- all.vars(formula(model))[-1]
  }

  suscept <- rast(tstack$mask); values(suscept) <- NA
  suscept[df$ind] <- as.vector(pred)
  prop <- suscept_prop(tstack$init_zone, suscept)
  auc  <- full_auc(tstack$init_zone, prop)

  mars <- c(1, 1, 4, 1)
  plot(tstack$hs, col = gray.colors(256), legend = FALSE,
       main = paste(island$name, toupper(mclass), "Prediction Map\n",
                    "Preds:", paste(preds, collapse = ", "),
                    "\nAUC:", round(auc, 3)),
       cex.main = 1, mar = mars)
  plot(prop, col = rev(hcl.colors(256, "RdYlGn")),
       add = TRUE, axes = FALSE, alpha = 0.35, mar = mars)
  plot(as.polygons(tstack$init_zone, dissolve = TRUE),
       add = TRUE, axes = FALSE, legend = FALSE, mar = mars)

  if (isTRUE(inset)) {
    rect(island$inset$xlim[1], island$inset$ylim[1],
         island$inset$xlim[2], island$inset$ylim[2],
         border = "black", lwd = 2, lty = 2)

    inset_ext <- ext(island$inset$xlim[1], island$inset$xlim[2],
                     island$inset$ylim[1], island$inset$ylim[2])
    hs_crop   <- crop(tstack$hs,        inset_ext)
    prop_crop <- crop(prop,             inset_ext)
    zone_crop <- crop(tstack$init_zone, inset_ext)

    tmp <- tempfile(fileext = ".png")
    png(tmp, width = 300, height = 300)
    par(mar = c(0, 0, 0, 0))
    image(hs_crop,   col = gray.colors(256), axes = FALSE, xlab = "", ylab = "")
    image(prop_crop, col = adjustcolor(rev(hcl.colors(256, "RdYlGn")), alpha.f = 0.5),
          add = TRUE)
    plot(as.polygons(zone_crop, dissolve = TRUE), add = TRUE,
         axes = FALSE, legend = FALSE, col = NA, border = "black", lwd = 3)
    dev.off()

    img     <- readPNG(tmp)
    usr     <- par("usr")
    x_range <- usr[2] - usr[1]; y_range <- usr[4] - usr[3]
    pad_x   <- x_range * 0.1;   pad_y   <- y_range * 0.05
    w       <- x_range * island$inset$size[1]
    h       <- y_range * island$inset$size[2]

    inset_pos <- switch(island$inset$loc,
      bottomright = c(usr[2]-w-pad_x, usr[3]+pad_y,   usr[2]-pad_x,   usr[3]+h+pad_y),
      bottomleft  = c(usr[1]+pad_x,   usr[3]+pad_y,   usr[1]+w+pad_x, usr[3]+h+pad_y),
      topright    = c(usr[2]-w-pad_x, usr[4]-h-pad_y, usr[2]-pad_x,   usr[4]-pad_y),
      topleft     = c(usr[1]+pad_x,   usr[4]-h-pad_y, usr[1]+w+pad_x, usr[4]-pad_y))

    rasterImage(img, inset_pos[1], inset_pos[2], inset_pos[3], inset_pos[4])
    rect(inset_pos[1], inset_pos[2], inset_pos[3], inset_pos[4],
         border = "black", lwd = 2)
  }
  invisible(NULL)
}

# Map runout hazard (or survival) on the test slice with optional inset.
map_test_hazard <- function(model, island, inset = TRUE) {
  stack <- island$test_stack

  if (inherits(model, "coxph")) {
    m_name <- "Survival Analysis"
    lp     <- predict(stack, model, type = "lp")
    hazard <- 1 - exp(-island$stop_h0 * exp(lp))
  } else if (inherits(model, "glm")) {
    m_name <- "Logistic Regression"
    hazard <- predict(stack, model, type = "response")
  }

  surv <- 1 - hazard
  mars <- c(1, 1, 4, 1)
  plot(stack$hs, col = gray.colors(256), legend = FALSE,
       main = paste(island$name, m_name, "Hazard Map\n"),
       cex.main = 1, mar = mars)
  plot(surv, col = hcl.colors(256, "RdYlGn"), range = c(0.9, 1),
       add = TRUE, axes = FALSE, alpha = 0.35, mar = mars)

  if (isTRUE(inset)) {
    rect(island$inset$xlim[1], island$inset$ylim[1],
         island$inset$xlim[2], island$inset$ylim[2],
         border = "black", lwd = 2, lty = 2)

    inset_ext   <- ext(island$inset$xlim[1], island$inset$xlim[2],
                       island$inset$ylim[1], island$inset$ylim[2])
    hs_crop     <- crop(stack$hs, inset_ext)
    hazard_crop <- crop(hazard,   inset_ext)

    tmp <- tempfile(fileext = ".png")
    png(tmp, width = 300, height = 300)
    par(mar = c(0, 0, 0, 0))
    image(hs_crop,     col = gray.colors(256), axes = FALSE, xlab = "", ylab = "")
    image(hazard_crop, col = adjustcolor(hcl.colors(256, "RdYlGn"), alpha.f = 0.5),
          add = TRUE)
    dev.off()

    img     <- readPNG(tmp)
    usr     <- par("usr")
    x_range <- usr[2] - usr[1]; y_range <- usr[4] - usr[3]
    pad_x   <- x_range * 0.1;   pad_y   <- y_range * 0.05
    w       <- x_range * island$inset$size[1]
    h       <- y_range * island$inset$size[2]

    inset_pos <- switch(island$inset$loc,
      bottomright = c(usr[2]-w-pad_x, usr[3]+pad_y,   usr[2]-pad_x,   usr[3]+h+pad_y),
      bottomleft  = c(usr[1]+pad_x,   usr[3]+pad_y,   usr[1]+w+pad_x, usr[3]+h+pad_y),
      topright    = c(usr[2]-w-pad_x, usr[4]-h-pad_y, usr[2]-pad_x,   usr[4]-pad_y),
      topleft     = c(usr[1]+pad_x,   usr[4]-h-pad_y, usr[1]+w+pad_x, usr[4]-pad_y))

    rasterImage(img, inset_pos[1], inset_pos[2], inset_pos[3], inset_pos[4])
    rect(inset_pos[1], inset_pos[2], inset_pos[3], inset_pos[4],
         border = "black", lwd = 2)
  }
  invisible(NULL)
}


# =============================================================================
# 8. PRODUCTION
# =============================================================================

# Generate full-island initiation susceptibility map in chunks to manage memory.
# Always writes to temp_path(island, "initiation") and
# temp_path(island, "initiation_prop"). Call promote_to_stable() when satisfied.
# Returns updated island list with full_auc recorded.
full_pred_map <- function(island, model) {

  if (class(model)[1] == "ranger") {
    preds <- model$forest$independent.variable.names
  } else {
    preds <- all.vars(formula(model))[-1]
  }

  b_inds    <- names(island$rstack) %in% preds
  # Include all mask pixels AND all init_zone pixels so that every observed
  # landslide cell receives a prediction regardless of the background mask.
  all_cells <- which(!is.na(values(island$rstack$mask)) |
                     !is.na(values(island$rstack$init_zone)))
  shape_dir <- island$shape_dir

  outfile <- temp_path(island, "initiation")
  cat("Initializing susceptibility map ->", basename(outfile), "\n")
  susceptibility <- rast(island$rstack$mask)
  values(susceptibility) <- NA
  writeRaster(susceptibility, outfile, overwrite = TRUE)
  susceptibility <- rast(outfile)

  chunk_size <- 5e5
  n_chunks   <- ceiling(length(all_cells) / chunk_size)

  cat_vars <- c("soils", "lith", "forest")
  if (any(cat_vars %in% preds)) {
    cat("Loading shapefiles...\n")
    soils  <- vect(file.path(shape_dir, "soils.shp"))
    lith   <- vect(file.path(shape_dir, "lith.shp"))
    forest <- vect(file.path(shape_dir, "cover.shp"))

    soil_vals <- unique(island$df$soils)
    lith_vals <- unique(island$df$lith)
    for_vals  <- unique(island$df$forest)
    soils$FOR_SMU[!soils$FOR_SMU %in% soil_vals]     <- "U"
    lith$STATE_UNIT[!lith$STATE_UNIT %in% lith_vals] <- "U"
    forest$ftype[!forest$ftype %in% for_vals]         <- "U"
  }

  cat("Making susceptibility predictions...\n")
  for (i in seq_len(n_chunks)) {
    cat(sprintf("\r  [%d / %d] (%.1f%%)", i, n_chunks, 100 * i / n_chunks))
    flush.console()

    idx      <- ((i - 1) * chunk_size + 1):min(i * chunk_size, length(all_cells))
    coords   <- vect(xyFromCell(island$rstack$mask, all_cells[idx]),
                     crs = crs(island$rstack$mask))
    chunk_df <- as.data.frame(extract(island$rstack[[b_inds]], coords,
                                      method = "simple"))
    if (any(cat_vars %in% preds)) {
      chunk_df$lith   <- extract(lith,   coords)$STATE_UNIT
      chunk_df$soils  <- extract(soils,  coords)$FOR_SMU
      forest_chunk    <- extract(forest, coords)
      forest_chunk    <- forest_chunk[!duplicated(forest_chunk$id.y), ]
      chunk_df$forest <- forest_chunk$ftype
    }

    chunk_pred <- predi(model, chunk_df)
    susceptibility[all_cells[idx]] <- as.vector(chunk_pred)
    rm(coords, chunk_df, chunk_pred); gc()
  }
  cat("\n")

  cat("Converting probabilities to proportions...\n")
  susc_prop <- suscept_prop(island$rstack$init_zone, susceptibility)

  prop_file <- temp_path(island, "initiation_prop")
  writeRaster(susceptibility, outfile,   overwrite = TRUE)
  writeRaster(susc_prop,      prop_file, overwrite = TRUE)
  write_raster_meta(outfile,   island, "initiation",      model)
  write_raster_meta(prop_file, island, "initiation_prop", model)
  cat("Written to temp: initiation | initiation_prop\n")

  island$rstack$ls_prob <- susceptibility
  rm(susceptibility); gc()

  island$full_auc <- full_auc(island$rstack$init_zone, susc_prop)
  cat(paste("AUC (full dataset):", round(island$full_auc, 4), "\n"))
  island
}


# =============================================================================
# 9. UTILITY — RETAINED FOR FUTURE USE
# =============================================================================
# These functions are not part of the current production pipeline but are
# retained as they may be useful for future analyses.

# Smooth width predictions using a focal mean, restricted to high-runout cells.
smooth_width <- function(width_map, runout, w, threshold = 0.3) {
  width_runout  <- ifel(runout > threshold, width_map, NA)
  width_smoothed <- focal(width_runout, w = w, fun = "mean",
                          na.policy = "omit", na.rm = TRUE)
  ifel(runout > threshold, width_smoothed, width_map)
}

# Smooth runout probabilities using a focal mean, restricted to high-runout cells.
smooth_runout <- function(runout, w, threshold = 0.3) {
  runout_high   <- ifel(runout > threshold, runout, NA)
  runout_smooth <- focal(runout_high, w = w, fun = "mean",
                         na.policy = "omit", na.rm = TRUE)
  ifel(runout > threshold, runout_smooth, runout)
}

# C++ topographic position index: elevation minus mean of N equally-spaced
# points on a circle of diameter D centred on each pixel.
cppFunction('
NumericVector tpi_cpp(NumericVector elev_vals,
                      int nrows, int ncols,
                      double res, double D,
                      int n_pts = 8) {
  const double R   = D / 2.0;
  const double PI2 = 2.0 * M_PI;
  const int    n   = nrows * ncols;
  NumericVector out(n, NA_REAL);

  std::vector<int> drow(n_pts), dcol(n_pts);
  for (int k = 0; k < n_pts; k++) {
    double angle = PI2 * k / n_pts;
    drow[k] = (int)std::round(-(R * std::cos(angle)) / res);
    dcol[k] = (int)std::round( (R * std::sin(angle)) / res);
  }

  for (int i = 0; i < n; i++) {
    if (NumericVector::is_na(elev_vals[i])) continue;
    int row = i / ncols;
    int col = i % ncols;
    double sum_n = 0.0;
    int    cnt   = 0;
    for (int k = 0; k < n_pts; k++) {
      int nr = row + drow[k];
      int nc = col + dcol[k];
      if (nr < 0 || nr >= nrows || nc < 0 || nc >= ncols) continue;
      double val = elev_vals[nr * ncols + nc];
      if (NumericVector::is_na(val)) continue;
      sum_n += val;
      cnt++;
    }
    if (cnt > 0) out[i] = elev_vals[i] - (sum_n / cnt);
  }
  return out;
}
')

# R wrapper for tpi_cpp. D is the diameter of the comparison circle in map units.
tpi_terra <- function(dem, D, n_pts = 8) {
  tpi_vals <- tpi_cpp(
    elev_vals = values(dem)[, 1],
    nrows     = nrow(dem), ncols = ncol(dem),
    res       = res(dem)[1], D = D, n_pts = n_pts)
  out <- rast(dem)
  values(out) <- tpi_vals
  names(out)  <- paste0("tpi_D", D)
  out
}
