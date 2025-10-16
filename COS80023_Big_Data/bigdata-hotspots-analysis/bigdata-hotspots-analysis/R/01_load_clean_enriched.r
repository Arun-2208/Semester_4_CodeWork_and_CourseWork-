# R/01_load_clean_enriched.R — integrate all raw tables into one crash-level table
suppressPackageStartupMessages({
  library(data.table)
  library(tidyverse)
  library(janitor)
  library(lubridate)
  library(stringr)
})

# ---------- Helpers ----------
pick_col <- function(df, patterns, default = NULL) {
  # Return the first column name that matches any of the regex patterns (case-insensitive)
  for (pat in patterns) {
    m <- grep(pat, names(df), ignore.case = TRUE, value = TRUE)
    if (length(m) > 0) return(m[1])
  }
  default
}

mode1 <- function(v) {
  v <- v[!is.na(v) & trimws(as.character(v)) != ""]
  if (!length(v)) return(NA_character_)
  names(sort(table(v), decreasing = TRUE))[1]
}

# Convert possibly character numeric to numeric safely
num_safely <- function(x) suppressWarnings(as.numeric(as.character(x)))

# ---------- Read raw ----------
acc  <- fread("data_raw/accident.csv")           |> clean_names()
loc  <- fread("data_raw/accident_location.csv")  |> clean_names()
node <- fread("data_raw/node.csv")               |> clean_names()
atm  <- fread("data_raw/atmospheric_cond.csv")   |> clean_names()
surf <- fread("data_raw/road_surface_cond.csv")  |> clean_names()
subd <- fread("data_raw/sub_dca.csv")            |> clean_names()
veh  <- fread("data_raw/vehicle.csv")            |> clean_names()
prs  <- fread("data_raw/person.csv")             |> clean_names()

# ---------- Identify key columns (robust to naming) ----------
# Accident keys/time
acc_id <- pick_col(acc, c("^accident_no$"))
acc_dt <- pick_col(acc, c("^accident_date$"))
acc_tm <- pick_col(acc, c("^accident_time$"))
if (is.null(acc_id) || is.null(acc_dt)) stop("accident.csv missing ACCIDENT_NO or ACCIDENT_DATE.")
if (is.null(acc_tm)) message("NOTE: accident_time not found; defaulting to 00:00 for time.")

# Location keys
loc_id   <- pick_col(loc,  c("^accident_no$"))
loc_node <- pick_col(loc,  c("^node_id$"))
if (is.null(loc_id) || is.null(loc_node)) stop("accident_location.csv missing ACCIDENT_NO or NODE_ID.")

# Node: coords + locality
node_id_acc <- pick_col(node, c("^accident_no$"))
node_id     <- pick_col(node, c("^node_id$"))
node_x      <- pick_col(node, c("^amg_x$"))
node_y      <- pick_col(node, c("^amg_y$"))
if (is.null(node_x) || is.null(node_y)) stop("node.csv missing AMG_X/AMG_Y columns.")

# Locality fields (optional)
node_lga      <- pick_col(node, c("^lga(_name)?$", "lga.*name", "local.*gov.*area"))
node_postcode <- pick_col(node, c("^postcode$", "post_?code$", "poa_?code", "poa_?name"))

# Atmospheric
atm_id   <- pick_col(atm,  c("^accident_no$"))
atm_code <- pick_col(atm,  c("^atmosph(_)?cond$","^atmospheric.*cond"))
atm_desc <- pick_col(atm,  c("atmosph.*desc","atmospheric.*desc"))

# Surface
surf_id   <- pick_col(surf, c("^accident_no$"))
surf_code <- pick_col(surf, c("^surface(_)?cond$"))
surf_desc <- pick_col(surf, c("surface.*desc"))

# Sub-DCA (behaviour)
subd_id   <- pick_col(subd, c("^accident_no$"))
subd_code <- pick_col(subd, c("^sub_?dca_?code$"))
subd_desc <- pick_col(subd, c("^sub_?dca_?desc$","^description$"))

# Vehicle fields (many rows per crash)
veh_id       <- pick_col(veh, c("^accident_no$"))
veh_speed    <- pick_col(veh, c("^speed_?zone$","speed.*limit"))
veh_type     <- pick_col(veh, c("^vehicle_?type(_desc)?$","^veh_?type"))
veh_year     <- pick_col(veh, c("^vehicle_?year_?manuf$","year_?of_?manuf","^veh_?year"))
veh_control  <- pick_col(veh, c("^traffic_?control(_desc)?$","control.*device"))

# Person fields (many rows per crash)
prs_id      <- pick_col(prs, c("^accident_no$"))
prs_age     <- pick_col(prs, c("^age$","^age_?years$"))
prs_agegrp  <- pick_col(prs, c("^age_?group$","^agegrp$"))
prs_sex     <- pick_col(prs, c("^sex$","^gender$"))
prs_role    <- pick_col(prs, c("^road_?user(_type)?$","^role$"))
prs_injury  <- pick_col(prs, c("^inj(_)?level(_desc)?$","^injury.*(level|desc)$"))

# ---------- Core accident subset ----------
acc_keep <- acc |>
  transmute(
    accident_no   = .data[[acc_id]],
    accident_date = .data[[acc_dt]],
    accident_time = if (!is.null(acc_tm)) .data[[acc_tm]] else NA_character_
  )

# ---------- Location join: accident -> node (coords + locality) ----------
node_slim <- node |>
  transmute(
    accident_no = .data[[node_id_acc]],
    node_id     = .data[[node_id]],
    amg_x       = num_safely(.data[[node_x]]),
    amg_y       = num_safely(.data[[node_y]]),
    lga_name    = if (!is.null(node_lga))      as.character(.data[[node_lga]])      else NA_character_,
    postcode    = if (!is.null(node_postcode)) as.character(.data[[node_postcode]]) else NA_character_
  )

acc_nodes <- loc |>
  transmute(
    accident_no = .data[[loc_id]],
    node_id     = .data[[loc_node]]
  ) |>
  left_join(node_slim, by = c("accident_no","node_id"))

# Collapse to 1 row per crash: first coords, modal locality
acc_loc <- acc_nodes |>
  group_by(accident_no) |>
  summarise(
    amg_x    = first(amg_x),
    amg_y    = first(amg_y),
    lga_name = mode1(lga_name),
    postcode = mode1(postcode),
    .groups  = "drop"
  )

# ---------- Merge core with location ----------
core <- acc_keep |>
  left_join(acc_loc, by = "accident_no")

# ---------- Add atmospheric & surface conditions ----------
if (!is.null(atm_id) && (!is.null(atm_code) || !is.null(atm_desc))) {
  atm_keep <- atm |>
    transmute(
      accident_no       = .data[[atm_id]],
      atmosph_cond      = if (!is.null(atm_code)) .data[[atm_code]] else NA,
      atmosph_cond_desc = if (!is.null(atm_desc)) .data[[atm_desc]] else NA
    )
  core <- core |> left_join(atm_keep, by = "accident_no")
}

if (!is.null(surf_id) && (!is.null(surf_code) || !is.null(surf_desc))) {
  surf_keep <- surf |>
    transmute(
      accident_no       = .data[[surf_id]],
      surface_cond      = if (!is.null(surf_code)) .data[[surf_code]] else NA,
      surface_cond_desc = if (!is.null(surf_desc)) .data[[surf_desc]] else NA
    )
  core <- core |> left_join(surf_keep, by = "accident_no")
}

# ---------- Add Sub-DCA (behaviour) & movement_type ----------
if (!is.null(subd_id) && (!is.null(subd_code) || !is.null(subd_desc))) {
  subd_keep <- subd |>
    transmute(
      accident_no = .data[[subd_id]],
      sub_dca_code = if (!is.null(subd_code)) .data[[subd_code]] else NA,
      sub_dca_desc = if (!is.null(subd_desc)) .data[[subd_desc]] else NA
    )
  core <- core |> left_join(subd_keep, by = "accident_no")
}

# movement type mapping (simple keywords on sub_dca_desc)
core <- core |>
  mutate(
    sub_dca_desc_l = tolower(as.character(sub_dca_desc)),
    movement_type = case_when(
      str_detect(sub_dca_desc_l, "right[- ]?turn|rt")        ~ "Right-turn",
      str_detect(sub_dca_desc_l, "rear[- ]?end")             ~ "Rear-end",
      str_detect(sub_dca_desc_l, "side[- ]?swipe")           ~ "Side-swipe",
      str_detect(sub_dca_desc_l, "head[- ]?on")              ~ "Head-on",
      str_detect(sub_dca_desc_l, "off[- ]?path|run[- ]?off") ~ "Off-path",
      TRUE ~ "Other"
    )
  ) |>
  select(-sub_dca_desc_l)

# ---------- Vehicle aggregates (per crash) ----------
if (!is.null(veh_id)) {
  veh_keep <- veh |>
    transmute(
      accident_no = .data[[veh_id]],
      speed_zone  = if (!is.null(veh_speed)) num_safely(.data[[veh_speed]]) else NA_real_,
      vehicle_type_desc = if (!is.null(veh_type)) as.character(.data[[veh_type]]) else NA_character_,
      vehicle_year_manuf = if (!is.null(veh_year)) num_safely(.data[[veh_year]]) else NA_real_,
      traffic_control_desc = if (!is.null(veh_control)) as.character(.data[[veh_control]]) else NA_character_
    )
  
  veh_agg <- veh_keep |>
    group_by(accident_no) |>
    summarise(
      speed_zone_mode = {
        v <- speed_zone[!is.na(speed_zone)]
        if (length(v)) as.numeric(names(sort(table(v), decreasing=TRUE))[1]) else NA_real_
      },
      speed_zone_max  = suppressWarnings(max(speed_zone, na.rm = TRUE)) |> {ifelse(is.infinite(.), NA_real_, .)},
      vehicle_type_mode = mode1(vehicle_type_desc),
      vehicle_year_mode = {
        v <- vehicle_year_manuf[!is.na(vehicle_year_manuf)]
        if (length(v)) as.numeric(names(sort(table(v), decreasing=TRUE))[1]) else NA_real_
      },
      traffic_control_mode = mode1(traffic_control_desc),
      .groups = "drop"
    ) |>
    mutate(
      # choose mode first, fall back to max for speed
      speed_zone_final = ifelse(!is.na(speed_zone_mode), speed_zone_mode, speed_zone_max),
      speed_zone_bin = case_when(
        is.na(speed_zone_final)          ~ NA_character_,
        speed_zone_final <= 40           ~ "≤40",
        speed_zone_final <= 60           ~ "50–60",
        speed_zone_final <= 80           ~ "70–80",
        TRUE                             ~ "≥100"
      )
    )
  
  core <- core |>
    left_join(veh_agg, by = "accident_no")
}

# ---------- Person aggregates (per crash) ----------
if (!is.null(prs_id)) {
  prs_keep <- prs |>
    transmute(
      accident_no = .data[[prs_id]],
      age        = if (!is.null(prs_age)) num_safely(.data[[prs_age]]) else NA_real_,
      age_group  = if (!is.null(prs_agegrp)) as.character(.data[[prs_agegrp]]) else NA_character_,
      sex        = if (!is.null(prs_sex)) as.character(.data[[prs_sex]]) else NA_character_,
      road_user  = if (!is.null(prs_role)) as.character(.data[[prs_role]]) else NA_character_,
      injury     = if (!is.null(prs_injury)) as.character(.data[[prs_injury]]) else NA_character_
    )
  
  # derive a numeric-ish age if only age_group given (very rough mapping; adjust if you have specific bins)
  map_age_band <- function(age, age_group) {
    if (!is.na(age)) {
      if (age < 21) "Under-21" else if (age <= 30) "21–30" else if (age <= 40) "31–40" else if (age <= 60) "41–60" else "60+"
    } else if (!is.na(age_group)) {
      ag <- tolower(age_group)
      if (str_detect(ag, "0|under|<")) "Under-21"
      else if (str_detect(ag, "2[0-9]|20|30")) "21–30"
      else if (str_detect(ag, "3[0-9]|40")) "31–40"
      else if (str_detect(ag, "5[0-9]")) "41–60"
      else "60+"
    } else NA_character_
  }
  
  prs_keep <- prs_keep |>
    rowwise() |>
    mutate(age_band = map_age_band(age, age_group)) |>
    ungroup()
  
  # injury ordering
  injury_rank <- function(x) {
    x <- tolower(trimws(as.character(x)))
    ifelse(str_detect(x, "fatal"), 3L,
           ifelse(str_detect(x, "serious"), 2L,
                  ifelse(str_detect(x, "minor|other|unknown"), 1L, 0L)))
  }
  
  prs_agg <- prs_keep |>
    group_by(accident_no) |>
    summarise(
      sex_share_male = {
        v <- tolower(sex)
        if (!length(v)) NA_real_ else {
          n_m <- sum(v %in% c("m","male"))
          n_t <- sum(v %in% c("m","male","f","female"))
          ifelse(n_t > 0, n_m / n_t, NA_real_)
        }
      },
      age_band_mode  = mode1(age_band),
      road_user_mode = mode1(road_user),
      injury_max     = {
        # pick the most severe text by rank
        if (!length(injury)) NA_character_ else {
          df <- tibble(txt = injury, r = injury_rank(injury))
          df$txt[which.max(df$r)]
        }
      },
      .groups = "drop"
    )
  
  core <- core |> left_join(prs_agg, by = "accident_no")
}

# ---------- Time features ----------
# Parse date/time robustly; switch to dmy() if your dates are day-month-year.
core <- core |>
  mutate(
    accident_date = suppressWarnings(ymd(accident_date)),
    accident_time = ifelse(is.na(accident_time) | accident_time == "", "00:00", accident_time),
    accident_dt   = suppressWarnings(accident_date + hm(accident_time)),
    year = year(accident_dt),
    dow  = wday(accident_dt, label = TRUE, week_start = 1),
    hour = hour(accident_dt),
    tod  = case_when(
      is.na(hour)           ~ NA_character_,
      hour %in% 6:9         ~ "AM Peak",
      hour %in% 16:19       ~ "PM Peak",
      hour %in% 20:23       ~ "Evening",
      hour %in% 0:5         ~ "Late Night",
      TRUE                  ~ "Daytime"
    )
  )

# ---------- Tidy text fields ----------
core <- core |>
  mutate(
    lga_name = ifelse(is.na(lga_name), NA, str_squish(as.character(lga_name))),
    postcode = ifelse(is.na(postcode), NA, str_squish(as.character(postcode))),
    vehicle_type_mode = ifelse(is.na(vehicle_type_mode), NA, str_squish(as.character(vehicle_type_mode))),
    traffic_control_mode = ifelse(is.na(traffic_control_mode), NA, str_squish(as.character(traffic_control_mode))),
    movement_type = factor(movement_type,
                           levels = c("Right-turn","Rear-end","Side-swipe","Head-on","Off-path","Other"))
  )

# ---------- Vehicle age (needs year + vehicle_year_mode) ----------
core <- core |>
  mutate(
    vehicle_age = ifelse(!is.na(year) & !is.na(vehicle_year_mode),
                         pmax(0, pmin(60, year - vehicle_year_mode)),
                         NA_real_)
  )

# ---------- Save ----------
dir.create("data_work", showWarnings = FALSE)
saveRDS(core, "data_work/core_enriched.rds")

# simple console summary
message("Saved: data_work/core_enriched.rds")
message("Rows: ", nrow(core))
message("With coords: ", sum(!is.na(core$amg_x) & !is.na(core$amg_y)))
message("Unique LGAs: ", length(unique(na.omit(core$lga_name))))
message("Unique postcodes: ", length(unique(na.omit(core$postcode))))
