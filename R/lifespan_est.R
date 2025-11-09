# ===============================================================
# Lifespan_est.R (Enhanced — XGBoost with Reddit Fallback + Warm Start + Normalized Targets)
# Trend Dissection Engine — Trend Lifespan Estimation
# ===============================================================

suppressPackageStartupMessages({
  library(xgboost)
  library(dplyr)
  library(jsonlite)
  library(caret)
  library(Metrics)
  library(scales)
})

message("⏳ Lifespan estimation (XGBoost, normalized) — starting...")

# ------------------------------------------------------------------
# 0️⃣ Paths & Dates
# ------------------------------------------------------------------
today      <- format(Sys.Date(), "%Y-%m-%d")
yesterday  <- format(Sys.Date() - 1, "%Y-%m-%d")

feat_path   <- file.path("data_features", paste0("trend_features_", today, ".rds"))
trends_path <- file.path("data_clean",    paste0("trends_clean_", today, ".rds"))
reddit_path <- file.path("data_clean",    paste0("reddit_posts_clean_", today, ".rds"))
model_dir   <- "data_lifespan"

if (!file.exists(feat_path)) stop("❌ Feature file missing: ", feat_path)
if (!file.exists(trends_path)) stop("❌ Trends clean file missing: ", trends_path)

dir.create(model_dir, showWarnings = FALSE)

today_model_file      <- file.path(model_dir, paste0("xgb_lifespan_model_", today, ".rds"))
yesterday_model_file  <- file.path(model_dir, paste0("xgb_lifespan_model_", yesterday, ".rds"))
source("R/utils_load_data.R")
data_all <- get_all_trend_data("data_clean")


feat   <- readRDS(feat_path)
trends <- data_all

# ------------------------------------------------------------------
# 1️⃣ Sanity check
# ------------------------------------------------------------------
if (!"topic" %in% colnames(feat)) stop("❌ 'topic' missing in features.")
if (!"trend_lifespan_hours" %in% colnames(trends)) {
  warning("⚠️ 'trend_lifespan_hours' missing — creating fallback column.")
  trends$trend_lifespan_hours <- NA_real_
}

# ------------------------------------------------------------------
# 2️⃣ Reddit Fallback Lifespans (with scaling)
# ------------------------------------------------------------------
if (file.exists(reddit_path)) {
  message("📥 Using Reddit fallback data for lifespan estimation...")
  reddit_posts <- readRDS(reddit_path)

  # Compute raw fallback lifespans (hours)
  lifespans_fallback <- reddit_posts %>%
    group_by(topic) %>%
    summarise(est_lifespan_hours = max(post_age_hours, na.rm = TRUE), .groups = "drop")

  # Guarantee column before mutate
  if (!"trend_lifespan_hours" %in% colnames(trends))
    trends$trend_lifespan_hours <- NA_real_

  # Normalize and scale fallback to realistic range (6–240 hours)
  max_cap <- 240  # 10 days max lifespan
  min_cap <- 6    # at least 6 hours
  lifespans_fallback <- lifespans_fallback %>%
    mutate(
      # Clip large values
      est_lifespan_hours = pmin(est_lifespan_hours, quantile(est_lifespan_hours, 0.95, na.rm = TRUE)),
      # Scale to [min_cap, max_cap] range
      est_lifespan_hours = rescale(est_lifespan_hours,
                                   to = c(min_cap, max_cap),
                                   from = range(est_lifespan_hours, na.rm = TRUE))
    )

  # Merge with trends and add small noise
  trends <- trends %>%
    left_join(lifespans_fallback, by = "topic") %>%
    mutate(
      trend_lifespan_hours = coalesce(trend_lifespan_hours, est_lifespan_hours, 12) +
        rnorm(n(), mean = 0, sd = 1)
    )
} else {
  warning("⚠️ No Reddit fallback file found — applying default lifespan = 12 ± 2h.")
  if (!"trend_lifespan_hours" %in% colnames(trends))
    trends$trend_lifespan_hours <- NA_real_
  trends$trend_lifespan_hours <- coalesce(trends$trend_lifespan_hours, 12 + rnorm(nrow(trends), 0, 2))
}

# ------------------------------------------------------------------
# 3️⃣ Merge features and targets
# ------------------------------------------------------------------
trends <- trends %>% select(topic, trend_lifespan_hours)
feat   <- feat   %>% select(-any_of("trend_lifespan_hours"))
df     <- feat   %>% left_join(trends, by = "topic")

df <- df %>%
  mutate(trend_lifespan_hours = coalesce(trend_lifespan_hours, 12 + rnorm(n(), 0, 2)))

# ------------------------------------------------------------------
# 4️⃣ Normalize target variable (0–1 scale for regression stability)
# ------------------------------------------------------------------
y_raw <- df$trend_lifespan_hours
y_scaled <- rescale(y_raw, to = c(0, 1))

X <- df %>% select(-topic, -trend_lifespan_hours) %>% as.matrix()

set.seed(42)
n        <- nrow(X)
train_id <- sample(seq_len(n), size = 0.8 * n, replace = FALSE)
valid_id <- setdiff(seq_len(n), train_id)

dtrain <- xgb.DMatrix(data = X[train_id, ], label = y_scaled[train_id])
dvalid <- xgb.DMatrix(data = X[valid_id, ], label = y_scaled[valid_id])

# ------------------------------------------------------------------
# 5️⃣ Model parameters
# ------------------------------------------------------------------
params <- list(
  objective        = "reg:squarederror",
  eta              = 0.05,
  max_depth        = 4,
  subsample        = 0.8,
  colsample_bytree = 0.8,
  gamma            = 0.1,
  min_child_weight = 3,
  reg_lambda       = 1
)

# ------------------------------------------------------------------
# 6️⃣ Warm-start or Cold-start model training
# ------------------------------------------------------------------
if (file.exists(yesterday_model_file)) {
  message("🔄 Warm-starting from yesterday’s model → ", yesterday_model_file)
  prev_model <- readRDS(yesterday_model_file)
  nrounds <- 50
  model_xgb <- xgb.train(
    params   = params,
    data     = dtrain,
    nrounds  = nrounds,
    xgb_model = prev_model,
    watchlist = list(train = dtrain, valid = dvalid),
    early_stopping_rounds = 10,
    maximize = FALSE,
    verbose  = 0
  )
} else {
  message("🚧 No previous model found → training from scratch")
  nrounds <- 150
  model_xgb <- xgb.train(
    params   = params,
    data     = dtrain,
    nrounds  = nrounds,
    watchlist = list(train = dtrain, valid = dvalid),
    early_stopping_rounds = 20,
    maximize = FALSE,
    verbose  = 0
  )
}

# ------------------------------------------------------------------
# 7️⃣ Predict and Rescale to Real Hours
# ------------------------------------------------------------------
preds_scaled <- predict(model_xgb, X)

# Convert back to real hours in the [min_cap, max_cap] range
preds_real <- rescale(preds_scaled, to = c(min_cap, max_cap))
y_real     <- y_raw

out_df <- tibble(
  topic = df$topic,
  pred_lifespan_hours = round(preds_real, 2),
  actual_lifespan_hours = round(y_real, 2)
)

# ------------------------------------------------------------------
# 8️⃣ Evaluation metrics (now on real scale)
# ------------------------------------------------------------------
mae_val  <- mae(y_real, preds_real)
rmse_val <- rmse(y_real, preds_real)
r2_val   <- summary(lm(preds_real ~ y_real))$r.squared

message("📊 Model Performance (Real-scale):")
message("   • MAE  = ", round(mae_val, 2))
message("   • RMSE = ", round(rmse_val, 2))
message("   • R²    = ", round(r2_val, 3))

# ------------------------------------------------------------------
# 9️⃣ Save outputs
# ------------------------------------------------------------------
write.csv(out_df,
          file.path(model_dir, paste0("lifespan_predictions_", today, ".csv")),
          row.names = FALSE)

write_json(out_df,
           file.path(model_dir, paste0("lifespan_predictions_", today, ".json")),
           pretty = TRUE, auto_unbox = TRUE)

saveRDS(model_xgb, today_model_file)

message("✅ Lifespan estimation complete. Outputs saved → ", model_dir)
