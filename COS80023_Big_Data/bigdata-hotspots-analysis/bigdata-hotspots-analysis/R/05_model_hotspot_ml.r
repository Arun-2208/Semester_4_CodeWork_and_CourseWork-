# R/05_model_hotspot_ml.R — Hotspot classification (Conditions-only & Full), pooled + per-period
suppressPackageStartupMessages({
  library(tidyverse)
  library(tidymodels)
  library(vip)
  library(forcats)
  library(readr)
})

message("Step 5: Modelling (Hotspot vs Non-Hotspot)")

# ---------- Config ----------
set.seed(42)

# Feature lumping sizes (adjust if categories explode)
LUMP_LGA        <- 15
LUMP_POSTCODE   <- 20
LUMP_VEH_TYPE   <- 12
LUMP_MOVEMENT   <- 8
LUMP_ATMOSPH    <- 10
LUMP_SURFACE    <- 10
LUMP_ROAD_USER  <- 10
LUMP_TRAFFICCTL <- 10
LUMP_DOW        <- 7   # Mon..Sun already, but keep for robustness
LUMP_TOD        <- 6

# Periods
PERIODS <- c("2012–2015","2016–2019","2020–2024")

# Train/test split ratio
TEST_PROP <- 0.25

# Grouped CV folds by LGA if available
FOLDS <- 5

# ---------- Load data ----------
d_all <- readRDS("data_work/labelled_hotspots_all_periods.rds")

# Safety: ensure hotspot is factor (yes/no)
d_all <- d_all |>
  mutate(hotspot = factor(ifelse(hotspot, "yes", "no"), levels = c("no","yes")))

# ---------- Utility functions ----------
lump_features <- function(df) {
  df |>
    mutate(
      # Lump high-cardinality categoricals
      lga_name            = fct_lump_n(factor(lga_name), n = LUMP_LGA),
      postcode            = fct_lump_n(factor(postcode), n = LUMP_POSTCODE),
      vehicle_type_mode   = fct_lump_n(factor(vehicle_type_mode), n = LUMP_VEH_TYPE),
      movement_type       = fct_lump_n(factor(movement_type), n = LUMP_MOVEMENT),
      atmosph_cond        = fct_lump_n(factor(atmosph_cond), n = LUMP_ATMOSPH),
      surface_cond        = fct_lump_n(factor(surface_cond), n = LUMP_SURFACE),
      road_user_mode      = fct_lump_n(factor(road_user_mode), n = LUMP_ROAD_USER),
      traffic_control_mode= fct_lump_n(factor(traffic_control_mode), n = LUMP_TRAFFICCTL),
      tod                 = fct_lump_n(factor(tod), n = LUMP_TOD),
      dow                 = fct_lump_n(factor(dow), n = LUMP_DOW),
      period_group        = factor(period_group, levels = PERIODS)
    )
}

# Build Conditions-only & Full predictor sets
select_features_conditions <- function(df) {
  df |>
    select(
      hotspot, period_group,
      # time
      dow, hour, tod,
      # environment
      atmosph_cond, surface_cond,
      # behaviour
      movement_type, sub_dca_desc,
      # vehicle context
      speed_zone_bin, vehicle_age, vehicle_type_mode, traffic_control_mode,
      # person context
      sex_share_male, age_band_mode, road_user_mode, injury_max
    )
}

select_features_full <- function(df) {
  df |>
    select_features_conditions() |>
    bind_cols(df |> select(amg_x, amg_y, lga_name, postcode))
}

# Recipe builder
make_recipe <- function(df) {
  recipe(hotspot ~ ., data = df) |>
    step_zv(all_predictors()) |>
    step_dummy(all_nominal_predictors(), one_hot = TRUE)
}

# Grouped CV if lga_name exists and has > 1 level, else standard vfold
make_resamples <- function(df, strata = hotspot, folds = FOLDS) {
  if ("lga_name" %in% names(df)) {
    if (n_distinct(df$lga_name) > 1) {
      return(group_vfold_cv(df, group = lga_name, v = folds, strata = {{strata}}))
    }
  }
  vfold_cv(df, v = folds, strata = {{strata}})
}

# Fit & evaluate a model set (glmnet + RF) on a given dataset
fit_and_eval <- function(df, tag, outdir = "data_work") {
  dir.create(outdir, showWarnings = FALSE)
  
  # Train/test split (stratified)
  split <- initial_split(df, prop = 1 - TEST_PROP, strata = hotspot)
  train <- training(split)
  test  <- testing(split)
  
  # Resamples (grouped if possible)
  folds <- make_resamples(train)
  
  # ---- Models ----
  # 1) Regularised Logistic (glmnet)
  rec_glm <- make_recipe(train)
  
  mod_glm <- logistic_reg(
    penalty = tune(),  # lambda
    mixture = tune()   # alpha (0=ridge .. 1=lasso)
  ) |>
    set_engine("glmnet") |>
    set_mode("classification")
  
  wf_glm <- workflow() |> add_model(mod_glm) |> add_recipe(rec_glm)
  
  grid_glm <- grid_regular(
    penalty(range = c(-4, 0)),   # lambda = 10^seq(-4, 0)
    mixture(), levels = 5
  )
  
  tune_glm <- tune_grid(
    wf_glm, resamples = folds, grid = grid_glm,
    metrics = metric_set(roc_auc, pr_auc, f_meas, accuracy)
  )
  
  best_glm <- select_best(tune_glm, "roc_auc")
  final_glm <- finalize_workflow(wf_glm, best_glm)
  fit_glm <- fit(final_glm, train)
  
  # Evaluate on test
  pred_glm_prob <- predict(fit_glm, test, type = "prob")
  pred_glm_cls  <- predict(fit_glm, test)
  glm_metrics <- bind_cols(test, pred_glm_prob, pred_glm_cls) |>
    metrics(truth = hotspot, estimate = .pred_class) |>
    bind_rows(roc_auc(bind_cols(test, pred_glm_prob), truth = hotspot, .pred_yes)) |>
    bind_rows(pr_auc(bind_cols(test, pred_glm_prob), truth = hotspot, .pred_yes)) |>
    mutate(model = "glmnet", dataset = tag)
  
  # Export glmnet coefficients (optional)
  try({
    glm_fit <- extract_fit_parsnip(fit_glm)$fit
    glm_coefs <- as.matrix(coef(glm_fit, s = glm_fit$lambdaOpt))[,1, drop = FALSE]
    glm_tbl <- tibble(feature = rownames(glm_coefs), coefficient = glm_coefs[,1]) |>
      arrange(desc(abs(coefficient)))
    write_csv(glm_tbl, file.path(outdir, paste0("glmnet_coefs_", tag, ".csv")))
  }, silent = TRUE)
  
  # 2) Random Forest (ranger)
  rec_rf <- make_recipe(train)
  
  mod_rf <- rand_forest(
    mtry  = tune(),
    min_n = tune(),
    trees = 600
  ) |>
    set_engine("ranger", importance = "impurity") |>
    set_mode("classification")
  
  wf_rf <- workflow() |> add_model(mod_rf) |> add_recipe(rec_rf)
  
  grid_rf <- grid_regular(
    mtry(range = c(5, 20)),
    min_n(range = c(2, 20)),
    levels = 4
  )
  
  tune_rf <- tune_grid(
    wf_rf, resamples = folds, grid = grid_rf,
    metrics = metric_set(roc_auc, pr_auc, f_meas, accuracy)
  )
  
  best_rf <- select_best(tune_rf, "roc_auc")
  final_rf <- finalize_workflow(wf_rf, best_rf)
  fit_rf <- fit(final_rf, train)
  
  # Evaluate on test
  pred_rf_prob <- predict(fit_rf, test, type = "prob")
  pred_rf_cls  <- predict(fit_rf, test)
  rf_metrics <- bind_cols(test, pred_rf_prob, pred_rf_cls) |>
    metrics(truth = hotspot, estimate = .pred_class) |>
    bind_rows(roc_auc(bind_cols(test, pred_rf_prob), truth = hotspot, .pred_yes)) |>
    bind_rows(pr_auc(bind_cols(test, pred_rf_prob), truth = hotspot, .pred_yes)) |>
    mutate(model = "random_forest", dataset = tag)
  
  # VIP (feature importance) for RF
  try({
    rf_fit <- extract_fit_parsnip(fit_rf)$fit
    png(file.path(outdir, paste0("vip_rf_", tag, ".png")), width = 1400, height = 900, res = 140)
    vip::vip(rf_fit, num_features = 20)
    dev.off()
  }, silent = TRUE)
  
  # Return combined metrics
  bind_rows(glm_metrics, rf_metrics)
}

# ---------- Build pooled datasets ----------
# CONDITIONS-ONLY (drop location leakage)
pooled_conditions <- d_all |>
  lump_features() |>
  select_features_conditions() |>
  drop_na(hotspot)

# FULL (includes location features)
pooled_full <- d_all |>
  lump_features() |>
  select_features_full() |>
  drop_na(hotspot)

# ---------- Fit pooled models ----------
metrics_pooled_conditions <- fit_and_eval(pooled_conditions, tag = "pooled_conditions")
metrics_pooled_full       <- fit_and_eval(pooled_full,       tag = "pooled_full")

metrics_pooled <- bind_rows(metrics_pooled_conditions, metrics_pooled_full)
write_csv(metrics_pooled, "data_work/ml_metrics_pooled.csv")
message("Saved: data_work/ml_metrics_pooled.csv")

# ---------- Per-period models (Conditions-only) ----------
metrics_list <- list()
for (pg in PERIODS) {
  key <- gsub("[^0-9A-Za-z]+", "_", pg)
  
  dtp <- d_all |>
    filter(period_group == pg) |>
    lump_features() |>
    select_features_conditions() |>
    drop_na(hotspot)
  
  # Skip tiny periods
  if (nrow(dtp) < 100) {
    warning("Skipping per-period model for ", pg, " (n < 100).")
    next
  }
  
  metrics_list[[pg]] <- fit_and_eval(dtp, tag = paste0("period_", key))
}

if (length(metrics_list)) {
  metrics_by_period <- bind_rows(metrics_list)
  write_csv(metrics_by_period, "data_work/ml_metrics_by_period.csv")
  message("Saved: data_work/ml_metrics_by_period.csv")
} else {
  warning("No per-period metrics were produced (insufficient rows?).")
}

message("Step 5 complete.")
