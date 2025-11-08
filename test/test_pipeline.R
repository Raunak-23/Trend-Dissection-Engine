# Test Pipeline Script
library(testthat)

source("../R/acquire_data.R")
source("../R/preprocess.R")
source("../R/feature_extraction.R")
source("../R/pipeline.R")

# Test YouTube Trending Data Fetch
test_that("YouTube trending data fetch works", {
  yt_data <- get_youtube_trending(region_code = "IN", max_results = 5)
  expect_type(yt_data, "list")
})

# Test Reddit Data Analysis
test_that("Reddit analysis works", {
  reddit_data <- analyze_reddit_posts("technology", limit = 5)
  expect_type(reddit_data, "list")
})

# Test News API
test_that("News API works", {
  news_data <- get_news_metrics("technology")
  expect_type(news_data, "list")
  expect_true("article_count" %in% names(news_data))
})

# Test Feature Extraction
test_that("Feature extraction works", {
  # Create sample topic series
  sample_df <- data.frame(
    period = seq(as.POSIXct("2025-01-01"), by = "hour", length.out = 24),
    engagement_sum = runif(24, 0, 100),
    sentiment_mean = runif(24, -1, 1)
  )
  sample_series <- list(
    topic = "test_topic",
    df = sample_df,
    metadata = list()
  )
  
  features <- extract_features_for_topic(sample_series)
  expect_type(features, "list")
  expect_true("summary" %in% names(features))
})

# Test Full Pipeline
test_that("Full pipeline runs", {
  result <- run_pipeline(
    json_dir = "../data-clean/",
    time_unit = "hour",
    prophet_periods = 48,
    clustering_len = 50,
    k_clusters = 4
  )
  expect_type(result, "list")
})