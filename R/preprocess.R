# ===============================================================
# preprocess.R
# Trend Dissection Engine - Data Preprocessing Module
# ===============================================================

suppressPackageStartupMessages({
  library(jsonlite)
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(stringr)
  library(stringi)
})

today <- Sys.Date()
json_path <- file.path("data_raw", paste0("trends_", today, ".json"))

if (!file.exists(json_path)) {
  source("R/acquire_data.R")
}

safe_num <- function(x) {
  # Converts to numeric safely, returning 0 if conversion fails
  out <- suppressWarnings(as.numeric(x))
  if (any(is.na(out))) out[is.na(out)] <- 0
  return(out)
}

safe_chr <- function(x) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) return("Unknown")
  return(as.character(x))
}


message("🧹 Starting Data Preprocessing...")

# ---------------------------------------------------------------
# 1️⃣ Read and Flatten JSON
# ---------------------------------------------------------------
today <- format(Sys.Date(), "%Y-%m-%d")
input_path <- file.path("data_raw", paste0("trends_", today, ".json"))
if (!file.exists(input_path)) {
  stop("❌ Input file not found: ", input_path)
}

raw_data <- fromJSON(input_path, simplifyVector = FALSE)
message("📥 Loaded JSON file with ", length(raw_data), " records.")

# Flatten to two dataframes
trends_df <- bind_rows(lapply(raw_data, function(x) {
  tibble(
    topic = x$topic,
    platform_source = x$platform_source,
    category = x$category,
    time_fetched = ymd_hms(x$time_fetched),
    trend_score = as.numeric(x$trend_score),
    trend_lifespan_hours = as.numeric(x$trend_lifespan_hours),
    external_interest_index = as.numeric(x$external_interest_index),
    avg_score = as.numeric(x$reddit_metrics$average_score),
    avg_comments = as.numeric(x$reddit_metrics$average_comments),
    avg_sentiment = as.numeric(x$reddit_metrics$average_sentiment),
    avg_velocity = as.numeric(x$reddit_metrics$average_velocity),
    post_count = as.integer(x$reddit_metrics$post_count),
    insta_likes = as.numeric(x$instagram_metrics$avg_likes),
    insta_comments = as.numeric(x$instagram_metrics$avg_comments),
    insta_sentiment = as.numeric(x$instagram_metrics$sentiment),
    insta_engagement = as.numeric(x$instagram_metrics$engagement_rate)
  )
}))

reddit_posts_df <- bind_rows(lapply(raw_data, function(x) {
  if (length(x$reddit_posts) == 0) return(NULL)
  bind_rows(lapply(x$reddit_posts, function(p) {
    tibble(
      topic = x$topic,
      title = p$title,
      score = as.numeric(p$score),
      num_comments = as.numeric(p$num_comments),
      upvote_ratio = as.numeric(p$upvote_ratio),
      engagement_velocity = as.numeric(p$engagement_velocity),
      sentiment = as.numeric(p$sentiment),
      post_age_hours = as.numeric(p$post_age_hours)
    )
  }))
}))

# ---------------------------------------------------------------
# 2️⃣ Handle Missing or Null Values
# ---------------------------------------------------------------
num_cols <- sapply(trends_df, is.numeric)
trends_df[num_cols] <- lapply(trends_df[num_cols], function(x) ifelse(is.na(x), 0, x))
trends_df[!num_cols] <- lapply(trends_df[!num_cols], function(x) ifelse(is.na(x), "Unknown", x))

# ---------------------------------------------------------------
# 3️⃣ Normalize Time Columns
# ---------------------------------------------------------------
trends_df$time_fetched <- with_tz(trends_df$time_fetched, "Asia/Kolkata")
trends_df$trend_date <- as.Date(trends_df$time_fetched)

# ---------------------------------------------------------------
# 4️⃣ Clean and Tokenize Titles
# ---------------------------------------------------------------
reddit_posts_df <- reddit_posts_df %>%
  mutate(title_clean = str_to_lower(title)) %>%
  mutate(title_clean = str_remove_all(title_clean, "http[s]?://\\S+")) %>%
  mutate(title_clean = stri_replace_all_regex(title_clean, "[^\\p{L}\\p{N}\\s]", "")) %>%
  mutate(title_clean = str_squish(title_clean))

# ---------------------------------------------------------------
# 5️⃣ Feature Engineering
# ---------------------------------------------------------------
trends_df <- trends_df %>%
  mutate(
    engagement_ratio = ifelse(avg_comments > 0, avg_velocity / avg_comments, 0),
    sentiment_weighted_velocity = avg_velocity * (1 + avg_sentiment),
    trend_intensity = (trend_score + external_interest_index) / 2,
    insta_influence = (insta_likes * 0.7 + insta_comments * 0.3) / 1000
  )

# ---------------------------------------------------------------
# 6️⃣ Data Validation
# ---------------------------------------------------------------
num_cols <- sapply(trends_df, is.numeric)   # <- now correct length

message("🔍 Validating preprocessed data...")
missing_ratio <- sum(is.na(trends_df)) / prod(dim(trends_df))
outlier_counts <- vapply(trends_df[num_cols],
                         function(x) length(boxplot.stats(x)$out),
                         integer(1))
message("📈 Missing Ratio: ", round(missing_ratio, 4))
message("📊 Avg Outliers per Numeric Column: ", round(mean(outlier_counts), 2))


# ---------------------------------------------------------------
# 7️⃣ Add Version & Timestamp
# ---------------------------------------------------------------
trends_df$data_version <- paste0("v", format(Sys.Date(), "%Y%m%d"))
trends_df$processed_at <- Sys.time()

# ---------------------------------------------------------------
# 8️⃣ Save Clean Data
# ---------------------------------------------------------------
dir.create("data_clean", showWarnings = FALSE)
saveRDS(trends_df, file.path("data_clean", paste0("trends_clean_", today, ".rds")))
saveRDS(reddit_posts_df, file.path("data_clean", paste0("reddit_posts_clean_", today, ".rds")))

write_json(trends_df, file.path("data_clean", paste0("trends_clean_", today, ".json")),
           pretty = TRUE, auto_unbox = TRUE)

message("✅ Preprocessing complete. Clean files saved to data_clean/")