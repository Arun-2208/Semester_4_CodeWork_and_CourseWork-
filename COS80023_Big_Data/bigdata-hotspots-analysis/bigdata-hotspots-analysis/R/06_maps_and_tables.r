# R/06_maps_and_tables.R — Figures & Tables for the report
suppressPackageStartupMessages({
  library(tidyverse)
  library(readr)
  library(scales)
  library(rlang)
  # spatstat 'im' plotting via as.data.frame
})

message("Step 6: Maps & Tables")

dir.create("assets", showWarnings = FALSE)

# ------- Config -------
PERIODS <- c("2012–2015","2016–2019","2020–2024")
PERIOD_KEYS <- setNames(gsub("[^0-9A-Za-z]+", "_", PERIODS), PERIODS)

POINT_ALPHA_NOISE   <- 0.15
POINT_ALPHA_CLUSTER <- 0.50
POINT_SIZE          <- 0.35

# ------- Load inputs -------
corep <- readRDS("data_work/core_enriched_periods.rds")
lab   <- readRDS("data_work/labelled_hotspots_all_periods.rds")
params <- tryCatch(read_csv("data_work/dbscan_params_by_period.csv"), error = function(e) NULL)

# ------- 1) Cluster scatter plots per period -------
plot_clusters_period <- function(d, period_label, out_png) {
  p <- ggplot(d, aes(x = amg_x, y = amg_y)) +
    geom_point(data = d |> filter(cluster == 0),
               color = "grey70", size = POINT_SIZE, alpha = POINT_ALPHA_NOISE) +
    geom_point(data = d |> filter(cluster > 0),
               aes(color = factor(cluster)),
               size = POINT_SIZE, alpha = POINT_ALPHA_CLUSTER, show.legend = FALSE) +
    coord_equal() +
    labs(title = paste0("DBSCAN Clusters — ", period_label),
         x = "AMG X (metres)", y = "AMG Y (metres)") +
    theme_minimal(base_size = 12)
  ggsave(out_png, p, width = 11, height = 8, dpi = 150)
}

for (pg in PERIODS) {
  key <- PERIOD_KEYS[[pg]]
  d <- lab |> filter(period_group == pg, !is.na(amg_x), !is.na(amg_y))
  if (nrow(d) == 0 || !("cluster" %in% names(d))) {
    warning("No cluster data for period ", pg, " — skipping cluster plot.")
    next
  }
  out_png <- file.path("assets", paste0("cluster_scatter_", key, ".png"))
  plot_clusters_period(d, pg, out_png)
}

# ------- 2) KDE heatmaps per period -------
# Each saved as data_work/kde_<key>.rds (spatstat 'im' object)
plot_kde_period <- function(kde_im, pts, period_label, out_png) {
  # Convert 'im' to data.frame for ggplot
  # as.data.frame.im returns columns x, y, value
  kde_df <- as.data.frame(kde_im)
  names(kde_df) <- c("x","y","z")
  p <- ggplot() +
    geom_raster(data = kde_df, aes(x = x, y = y, fill = z), alpha = 0.9, interpolate = TRUE) +
    scale_fill_viridis_c(option = "magma", trans = "sqrt", name = "Density") +
    geom_point(data = pts, aes(x = amg_x, y = amg_y), color = "white", alpha = 0.15, size = 0.2) +
    coord_equal() +
    labs(title = paste0("KDE Heatmap — ", period_label),
         x = "AMG X (metres)", y = "AMG Y (metres)") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "right")
  ggsave(out_png, p, width = 11, height = 8, dpi = 150)
}

for (pg in PERIODS) {
  key <- PERIOD_KEYS[[pg]]
  kde_path <- file.path("data_work", paste0("kde_", key, ".rds"))
  if (!file.exists(kde_path)) {
    warning("KDE file missing for ", pg, ": ", kde_path)
    next
  }
  kde_im <- readRDS(kde_path)
  pts <- lab |> filter(period_group == pg, !is.na(amg_x), !is.na(amg_y))
  out_png <- file.path("assets", paste0("kde_heat_", key, ".png"))
  plot_kde_period(kde_im, pts, pg, out_png)
}

# ------- 3) Facet plot of clusters across periods -------
# Keep only needed columns
lab_small <- lab |>
  select(period_group, amg_x, amg_y, cluster) |>
  mutate(is_noise = cluster == 0L,
         cluster_f = factor(ifelse(cluster == 0L, NA_integer_, cluster)))

# If very large, optionally downsample for plotting
max_points <- 400000
if (nrow(lab_small) > max_points) {
  set.seed(42)
  lab_small <- lab_small |> slice_sample(n = max_points)
  message("Downsampled points for facet plot: ", max_points)
}

facet_p <- ggplot(lab_small, aes(x = amg_x, y = amg_y)) +
  geom_point(data = lab_small |> filter(is_noise),
             color = "grey80", size = 0.2, alpha = 0.15) +
  geom_point(data = lab_small |> filter(!is_noise),
             aes(color = cluster_f), size = 0.2, alpha = 0.5, show.legend = FALSE) +
  coord_equal() +
  facet_wrap(~ period_group, ncol = 2, scales = "free") +
  labs(title = "DBSCAN Clusters Across Periods",
       x = "AMG X (metres)", y = "AMG Y (metres)") +
  theme_minimal(base_size = 12)
ggsave("assets/cluster_facets.png", facet_p, width = 12, height = 9, dpi = 150)

# ------- 4) Top hotspot areas by LGA & Postcode (per period) -------
# Only count points labelled as hotspots (cluster > 0)
hot <- lab |> filter(hotspot)

top_lga <- hot |>
  count(period_group, lga_name, sort = TRUE) |>
  group_by(period_group) |>
  slice_head(n = 10) |>
  ungroup()

top_pcode <- hot |>
  count(period_group, postcode, sort = TRUE) |>
  group_by(period_group) |>
  slice_head(n = 10) |>
  ungroup()

write_csv(top_lga,  "data_work/top_hotspots_by_lga_per_period.csv")
write_csv(top_pcode, "data_work/top_hotspots_by_postcode_per_period.csv")

# ------- 5) Persistence figures -------
tiles <- tryCatch(readRDS("data_work/hotspot_persistence.rds"), error = function(e) NULL)
if (!is.null(tiles)) {
  # Use the summary CSV from Step 4 if present; else build inline
  ps_path <- "data_work/persistence_summary.csv"
  if (file.exists(ps_path)) {
    persistence_summary <- read_csv(ps_path, show_col_types = FALSE)
  } else {
    n_periods <- hot |> distinct(period_group) |> nrow()
    persistence_summary <- tiles |>
      count(persistence_score, name = "n_tiles") |>
      tidyr::complete(persistence_score = 0:n_periods, fill = list(n_tiles = 0)) |>
      arrange(persistence_score)
  }
  
  p_bar <- ggplot(persistence_summary,
                  aes(x = factor(persistence_score), y = n_tiles)) +
    geom_col(fill = "#4E79A7") +
    labs(title = "Tiles by Persistence Score",
         x = "Persistence score (# periods flagged as hotspot)",
         y = "Number of tiles") +
    theme_minimal(base_size = 12)
  ggsave("assets/persistence_bar.png", p_bar, width = 8, height = 5, dpi = 150)
} else {
  warning("hotspot_persistence.rds not found — skipping persistence figure.")
}

# ------- 6) Optional: Leaflet maps (OFF by default) -------
MAKE_LEAFLET <- FALSE
if (MAKE_LEAFLET) {
  suppressPackageStartupMessages({
    library(sf); library(leaflet)
  })
  # Assumed analysis CRS (metres). Change if your AMG is different.
  CRS_METRES <- 3111 # VicGrid
  for (pg in PERIODS) {
    key <- PERIOD_KEYS[[pg]]
    d <- lab |> filter(period_group == pg, !is.na(amg_x), !is.na(amg_y))
    if (nrow(d) == 0) next
    sf_m <- st_as_sf(d, coords = c("amg_x","amg_y"), crs = CRS_METRES)
    sf_ll <- st_transform(sf_m, 4326)
    m <- leaflet(sf_ll) |>
      addTiles() |>
      addCircleMarkers(radius = 3, stroke = FALSE,
                       color = ifelse(sf_ll$cluster > 0, "#E15759", "#9D9D9D"),
                       fillOpacity = 0.5,
                       popup = ~paste0("Cluster: ", cluster, "<br>Hotspot: ", hotspot,
                                       "<br>LGA: ", lga_name, "<br>Postcode: ", postcode))
    htmlwidgets::saveWidget(m, file.path("assets", paste0("hotspots_map_", key, ".html")),
                            selfcontained = TRUE)
  }
}

message("Step 6 complete. Figures saved to /assets and tables to /data_work.")
