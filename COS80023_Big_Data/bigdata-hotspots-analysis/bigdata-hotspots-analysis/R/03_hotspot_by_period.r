# R/03_hotspots_by_period.R — DBSCAN hotspots per fixed period + KDE validation
suppressPackageStartupMessages({
  library(tidyverse)
  library(dbscan)
  library(spatstat.geom)
  library(spatstat.core)
  library(scales)
})

message("Step 3: Hotspots by Period (DBSCAN + KDE)")

# -------- Config --------
# Period labels must match what Step 2 produced:
periods <- c("2012–2015","2016–2019","2020–2024")

# Minimum rows to attempt clustering (adjust to your dataset size)
min_rows_per_period <- 50L

# DBSCAN grid (on SCALED coordinates)
eps_grid    <- c(0.03, 0.05, 0.07, 0.09)
minpts_grid <- c(8, 12, 20, 30)

# Scoring target: prefer a moderate number of clusters (tunable)
target_clusters <- 10

# Quicklook KDE PNGs?
save_kde_png <- TRUE

# -------- Helpers --------
safe_key <- function(lbl) gsub("[^0-9A-Za-z]+", "_", lbl)  # for filenames

score_combo <- function(cl) {
  # cl is the return from dbscan() with $cluster
  k <- max(cl$cluster) # number of clusters (noise = 0)
  if (k <= 0) return(list(n_clusters = 0, clustered_share = 0, largest_cluster = 0, score = 0))
  tbl <- table(cl$cluster[cl$cluster > 0])
  clustered_share <- sum(tbl) / length(cl$cluster)
  largest_cluster <- max(tbl)
  # composite score: prefer decent clustered share and a moderate #clusters
  score <- clustered_share * (1 - abs(k - target_clusters) / target_clusters)
  list(n_clusters = as.integer(k),
       clustered_share = as.numeric(clustered_share),
       largest_cluster = as.integer(largest_cluster),
       score = as.numeric(score))
}

# -------- Load input --------
corep <- readRDS("data_work/core_enriched_periods.rds")

# -------- Prepare outputs --------
dir.create("data_work", showWarnings = FALSE)
params_out <- list()
results_out <- list()

# -------- Loop over periods --------
for (pg in periods) {
  message("Processing period: ", pg)
  key <- safe_key(pg)
  
  d <- corep |>
    filter(period_group == pg, !is.na(amg_x), !is.na(amg_y)) |>
    select(accident_no, period_group, amg_x, amg_y, lga_name, postcode,
           accident_date, accident_time, year, dow, hour, tod,
           # keep enriched fields that may help later summaries
           atmosph_cond, surface_cond, sub_dca_code, sub_dca_desc, movement_type,
           speed_zone_final, speed_zone_bin, vehicle_type_mode, vehicle_year_mode,
           traffic_control_mode, sex_share_male, age_band_mode, road_user_mode, injury_max)
  
  n <- nrow(d)
  if (n < min_rows_per_period) {
    warning("Skipping period ", pg, " (rows=", n, " < ", min_rows_per_period, ").")
    next
  }
  
  # 1) Prepare scaled coordinates for DBSCAN
  coords <- as.matrix(d[, c("amg_x", "amg_y")])
  coords_scaled <- scale(coords)
  
  # 2) Grid search DBSCAN
  grid <- expand.grid(eps = eps_grid, minPts = minpts_grid, KEEP.OUT.ATTRS = FALSE)
  grid$score <- NA_real_
  grid$n_clusters <- NA_integer_
  grid$clustered_share <- NA_real_
  grid$largest_cluster <- NA_integer_
  
  for (i in seq_len(nrow(grid))) {
    e <- grid$eps[i]; m <- grid$minPts[i]
    cl <- dbscan(coords_scaled, eps = e, minPts = m)
    met <- score_combo(cl)
    grid$n_clusters[i]     <- met$n_clusters
    grid$clustered_share[i] <- met$clustered_share
    grid$largest_cluster[i] <- met$largest_cluster
    grid$score[i]          <- met$score
  }
  
  # pick best by score
  best_idx <- which.max(grid$score)
  best <- grid[best_idx, , drop = FALSE]
  
  message(sprintf("Best params for %s: eps=%.3f, minPts=%d | clusters=%d, clustered=%.1f%%",
                  pg, best$eps, best$minPts, best$n_clusters, 100*best$clustered_share))
  
  # 3) Fit final DBSCAN with the best params
  final_cl <- dbscan(coords_scaled, eps = best$eps, minPts = best$minPts)
  d$cluster <- as.integer(final_cl$cluster)
  d$hotspot <- d$cluster > 0
  
  # 4) KDE validation (on original-metre coords)
  # Create spatstat window and point pattern
  x_range <- range(d$amg_x, na.rm = TRUE)
  y_range <- range(d$amg_y, na.rm = TRUE)
  win <- owin(xrange = x_range, yrange = y_range)
  pp  <- ppp(x = d$amg_x, y = d$amg_y, window = win)
  
  # Bandwidth selection (Diggle is robust for clustering)
  bw  <- bw.diggle(pp)
  dens <- density.ppp(pp, sigma = bw)  # pixel image class 'im'
  
  # 5) Save per-period KDE
  saveRDS(dens, file = file.path("data_work", paste0("kde_", key, ".rds")))
  
  # Optional quicklook PNG (small heatmap)
  if (save_kde_png) {
    png(file.path("data_work", paste0("kde_", key, ".png")), width = 1200, height = 900, res = 150)
    plot(dens, main = paste0("KDE (", pg, ")"))
    points(pp, pch = 20, cex = 0.3)
    dev.off()
  }
  
  # Collect outputs
  results_out[[pg]] <- d
  params_out[[pg]] <- tibble(
    period_group     = pg,
    n_points         = n,
    best_eps_scaled  = best$eps,
    best_minPts      = best$minPts,
    n_clusters       = best$n_clusters,
    clustered_share  = best$clustered_share,
    largest_cluster  = best$largest_cluster
  )
}

# -------- Save combined outputs --------
if (length(results_out)) {
  labelled_all <- bind_rows(results_out)
  saveRDS(labelled_all, "data_work/labelled_hotspots_all_periods.rds")
  message("Saved: data_work/labelled_hotspots_all_periods.rds (rows=", nrow(labelled_all), ")")
} else {
  warning("No period produced clustering results. Check input sizes and period filters.")
}

if (length(params_out)) {
  params_tbl <- bind_rows(params_out) |>
    arrange(period_group)
  readr::write_csv(params_tbl, "data_work/dbscan_params_by_period.csv")
  message("Saved: data_work/dbscan_params_by_period.csv")
}

message("Step 3 complete.")
