# R/02_time_windows.R — assign fixed 4-year periods to each crash
suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(readr)
})

message("Step 2: Period Splitting (Fixed 4-Year Windows)")

# --------- Config (edit if needed) ---------
# Fixed, inclusive periods:
A_start <- as_date("2012-01-01"); A_end <- as_date("2015-12-31")
B_start <- as_date("2016-01-01"); B_end <- as_date("2019-12-31")
C_start <- as_date("2020-01-01"); C_end <- as_date("2024-12-31")

# What to do with records outside the fixed windows?
drop_outside <- TRUE   # TRUE = drop; FALSE = keep as "Outside Study Window"

# --------- Load input from Step 1 ---------
core <- readRDS("data_work/core_enriched.rds")

# --------- Ensure we have a proper accident_dt & accident_date ---------
# Prefer existing accident_dt if present
if (!("accident_dt" %in% names(core)) || all(is.na(core$accident_dt))) {
  # Try to parse accident_date intelligently (ymd first, then dmy if needed)
  ad0 <- suppressWarnings(ymd(core$accident_date))
  # If too many NAs, try dmy
  na_ymd <- sum(is.na(ad0))
  ad1 <- suppressWarnings(dmy(core$accident_date))
  na_dmy <- sum(is.na(ad1))
  accident_date_parsed <- if (na_dmy < na_ymd) ad1 else ad0
  
  # Accident time: default to "00:00" if missing/blank
  if (!("accident_time" %in% names(core))) core$accident_time <- NA_character_
  acc_time <- ifelse(is.na(core$accident_time) | core$accident_time == "", "00:00", core$accident_time)
  
  core <- core |>
    mutate(
      accident_date = as_date(accident_date_parsed),
      accident_dt   = suppressWarnings(accident_date + hm(acc_time))
    )
} else {
  # Ensure accident_date exists as Date for boundary comparisons
  if (!("accident_date" %in% names(core)) || !inherits(core$accident_date, "Date")) {
    core <- core |> mutate(accident_date = as_date(accident_dt))
  }
}

# Derive year if missing
if (!("year" %in% names(core))) core <- core |> mutate(year = year(accident_dt))

# --------- Assign fixed period labels ---------
core <- core |>
  mutate(
    period_group = case_when(
      accident_date >= A_start & accident_date <= A_end ~ "2012–2015",
      accident_date >= B_start & accident_date <= B_end ~ "2016–2019",
      accident_date >= C_start & accident_date <= C_end ~ "2020–2024",
      TRUE ~ NA_character_
    )
  )

# Drop or keep outside window rows
if (drop_outside) {
  n_before <- nrow(core)
  core <- core |> filter(!is.na(period_group))
  n_after <- nrow(core)
  message("Dropped rows outside fixed windows: ", n_before - n_after)
} else {
  core <- core |> mutate(period_group = replace_na(period_group, "Outside Study Window"))
}

# Optional: make period_group an ordered factor (nice for plots/tables)
pg_levels <- c("2012–2015","2016–2019","2020–2024")
if (!drop_outside) pg_levels <- c(pg_levels, "Outside Study Window")
core <- core |> mutate(period_group = factor(period_group, levels = pg_levels, ordered = TRUE))

# --------- Outputs & acceptance checks ---------
dir.create("data_work", showWarnings = FALSE)

# Save labelled table
saveRDS(core, "data_work/core_enriched_periods.rds")

# Period counts
period_counts <- core |>
  count(period_group, name = "n_crashes") |>
  arrange(period_group)

write_csv(period_counts, "data_work/period_counts.csv")

# Console diagnostics
message("Saved: data_work/core_enriched_periods.rds")
message("Saved: data_work/period_counts.csv")
message("Unique period groups (kept): ", paste(levels(core$period_group), collapse = ", "))

print(period_counts)

# Acceptance checks
if (any(is.na(core$period_group))) {
  warning("Some rows still have NA period_group (unexpected with drop_outside=TRUE). Inspect date parsing.")
}

# Warn if any period is empty or very small
tiny_threshold <- 50L  # adjust if your dataset is small
empty_periods <- period_counts |> filter(n_crashes == 0) |> pull(period_group)
tiny_periods  <- period_counts |> filter(n_crashes > 0 & n_crashes < tiny_threshold) |> pull(period_group)
if (length(empty_periods)) warning("Empty periods: ", paste(empty_periods, collapse = ", "))
if (length(tiny_periods))  warning("Tiny periods (<", tiny_threshold, " rows): ", paste(tiny_periods, collapse = ", "))

message("Step 2 complete.")
