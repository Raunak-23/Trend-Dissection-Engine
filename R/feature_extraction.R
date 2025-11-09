# ===============================================================
# Feature_extraction.R
# Trend Dissection Engine – Feature Engineering Module
# ===============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(caret)
  library(jsonlite)
})

source("R/utils_load_data.R")

message("📂 Found files in data_clean")
data_all <- get_all_trend_data("data_clean")
message("⚙️ Starting Feature Extraction...")

today <- format(Sys.Date(), "%Y-%m-%d")

# ---------------------------------------------------------------
# 1️⃣ Select and Align Numeric Features
# ---------------------------------------------------------------
expected_cols <- c(
  "average_score", "average_comments", "average_sentiment", "average_velocity",
  "trend_score", "trend_lifespan_hours", "external_interest_index",
  "insta_influence", "insta_engagement"
)

existing_cols <- intersect(expected_cols, names(data_all))
if (length(existing_cols) == 0) stop("❌ No numeric features found in preprocessed data!")

# explicitly use dplyr::select
features <- dplyr::select(data_all, dplyr::all_of(existing_cols))

# ---------------------------------------------------------------
# 2️⃣ Compute Derived Features
# ---------------------------------------------------------------
features <- features %>%
  dplyr::mutate(
    engagement_ratio = if ("average_comments" %in% names(features) && "average_score" %in% names(features))
      average_comments / (average_score + 1) else 0,
    sentiment_weighted_velocity = if ("average_velocity" %in% names(features) && "average_sentiment" %in% names(features))
      average_velocity * (1 + average_sentiment) else 0,
    trend_intensity = if ("trend_score" %in% names(features) && "average_sentiment" %in% names(features))
      trend_score * (1 + average_sentiment) else 0
  )

# ---------------------------------------------------------------
# 3️⃣ Handle Missing Values and Scaling
# ---------------------------------------------------------------
features <- features %>% dplyr::mutate(dplyr::across(where(is.numeric), ~tidyr::replace_na(., 0)))

preproc <- caret::preProcess(features, method = c("center", "scale"))
scaled_features <- predict(preproc, features)

# Attach topic for reference
scaled_features$topic <- data_all$topic

# ---------------------------------------------------------------
# 4️⃣ Save Feature Matrix
# ---------------------------------------------------------------
dir.create("data_features", showWarnings = FALSE)

saveRDS(scaled_features, file.path("data_features", paste0("trend_features_", today, ".rds")))
jsonlite::write_json(scaled_features, file.path("data_features", paste0("trend_features_", today, ".json")),
                     pretty = TRUE, auto_unbox = TRUE)

message("✅ Feature extraction complete → data_features/trend_features_", today)
