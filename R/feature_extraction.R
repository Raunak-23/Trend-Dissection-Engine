# ===============================================================
# Feature_extraction.R
# Trend Dissection Engine – Feature Engineering Module
# ===============================================================
# Loads clean trends data from data_clean and produces 
# normalized numeric feature matrices for clustering and forecasting.
# ===============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(caret)
  library(jsonlite)
})

source("R/utils_load_data.R")
data_all <- get_all_trend_data("data_clean")

message("⚙️ Starting Feature Extraction...")

today <- format(Sys.Date(), "%Y-%m-%d")
infile <- file.path("data_clean", paste0("trends_clean_", today, ".rds"))
if (!file.exists(infile)) stop("❌ Clean data not found: ", infile)
df <- data_all

# ---------------------------------------------------------------
# 1️⃣ Select Numeric Features
# ---------------------------------------------------------------
features <- df %>%
  select(
    avg_score, avg_comments, avg_sentiment, avg_velocity,
    trend_score, trend_lifespan_hours, external_interest_index,
    engagement_ratio, sentiment_weighted_velocity,
    trend_intensity, insta_influence, insta_engagement
  )

# ---------------------------------------------------------------
# 2️⃣ Handle Missing Values and Scaling
# ---------------------------------------------------------------
features[is.na(features)] <- 0
preproc <- preProcess(features, method = c("center", "scale"))
scaled_features <- predict(preproc, features)

# Attach topic for reference
scaled_features$topic <- df$topic

# ---------------------------------------------------------------
# 3️⃣ Save Feature Matrix
# ---------------------------------------------------------------
dir.create("data_features", showWarnings = FALSE)
saveRDS(scaled_features, file.path("data_features", paste0("trend_features_", today, ".rds")))
write_json(scaled_features, file.path("data_features", paste0("trend_features_", today, ".json")),
           pretty = TRUE, auto_unbox = TRUE)

message("✅ Feature extraction complete → data_features/trend_features_", today)
