# R/04_persistence.R — tile-based hotspot persistence across fixed periods
suppressPackageStartupMessages({
  library(tidyverse)
  library(readr)
})

message("Step 4: Hotspot Persistence (tile-based)")

# -------- Config --------
tile_size_m <- 300L          # tile size in metres (adjust 200–500 if needed)
min_points_per_tile <- 1L    # keep tiles that have at least this many points across all periods
top_n_export <- 200L         # how many top persistent tiles to export as a quick table

# -------- Helpers --------
cell <- function(x, w = 300L) floor(x / w) * w

# -------- Load labelled points from Step 3 --------
lab_all <- readRDS("data_work/labelled_hotspots_all_periods.rds")

# Basic safety checks
req_cols <- c("period_group","amg_x","amg_y","hotspot")
if (!all(req_cols %in% names(lab_all))) {
  stop("labelled_hotspots_all_periods.rds is missing required columns: ",
       paste(setdiff(req_cols, names(lab_all)), collapse = ", "))
}

# -------- Assign tiles --------
d_tiles <- lab_all |>
  filter(!is.na(amg_x), !is.na(amg_y)) |>
  mutate(
    tile_x = cell(amg_x, tile_size_m),
    tile_y = cell(amg_y, tile_size_m)
  )

# -------- Compute per-tile, per-period hotspot indicators --------
tile_period <- d_tiles |>
  group_by(tile_x, tile_y, period_group) |>
  summarise(
    n_points   = n(),                 # number of crashes in this tile & period
    n_hotspots = sum(hotspot, na.rm = TRUE),
    is_hotspot = any(hotspot, na.rm = TRUE),
    .groups = "drop"
  )

# -------- Compute persistence (across all periods for each tile) --------
tile_persist <- tile_period |>
  group_by(tile_x, tile_y) |>
  summarise(
    n_periods_present = n_distinct(period_group),
    persistence_score = sum(is_hotspot, na.rm = TRUE),  # #periods flagged as hotspot
    total_points      = sum(n_points, na.rm = TRUE),
    total_hotspots    = sum(n_hotspots, na.rm = TRUE),
    .groups = "drop"
  ) |>
  filter(total_points >= min_points_per_tile)

# -------- Attach locality labels (modal LGA & postcode) --------
tile_locality <- d_tiles |>
  group_by(tile_x, tile_y) |>
  summarise(
    lga_name = {
      v <- lga_name[!is.na(lga_name) & trimws(lga_name) != ""]
      if (length(v)) names(sort(table(v), decreasing = TRUE))[1] else NA_character_
    },
    postcode = {
      v <- postcode[!is.na(postcode) & trimws(postcode) != ""]
      if (length(v)) names(sort(table(v), decreasing = TRUE))[1] else NA_character_
    },
    .groups = "drop"
  )

tiles <- tile_persist |>
  left_join(tile_locality, by = c("tile_x","tile_y")) |>
  arrange(desc(persistence_score), desc(total_hotspots), desc(total_points))

# -------- Summaries --------
n_periods <- d_tiles |> distinct(period_group) |> nrow()

persistence_summary <- tiles |>
  count(persistence_score, name = "n_tiles") |>
  complete(persistence_score = 0:n_periods, fill = list(n_tiles = 0)) |>
  arrange(persistence_score)

# -------- Exports --------
dir.create("data_work", showWarnings = FALSE)
saveRDS(tiles, "data_work/hotspot_persistence.rds")
write_csv(tiles, "data_work/hotspot_persistence.csv")
write_csv(persistence_summary, "data_work/persistence_summary.csv")

# Export a quick "top persistent tiles" table for the report
persistent_tiles_top <- tiles |>
  filter(persistence_score >= 2) |>
  slice_head(n = top_n_export)

write_csv(persistent_tiles_top, "data_work/persistent_tiles_top.csv")

# -------- Console diagnostics --------
message("Saved: hotspot_persistence.rds / .csv (rows=", nrow(tiles), ")")
message("Tile size (m): ", tile_size_m, 
        " | Distinct periods found: ", n_periods)
print(persistence_summary)

# Acceptance checks
if (!any(tiles$persistence_score >= 2, na.rm = TRUE)) {
  warning("No tiles have persistence_score >= 2 (no chronic hotspots detected). ",
          "Consider adjusting tile_size_m (e.g., 400–500) or re-check DBSCAN parameters.")
}

message("Step 4 complete.")
