# ===============================================================
# Trend Dissection Engine — Evaluation Metrics Toolkit
# Compatible with TrendR folder structure and variable names
# ===============================================================
# Author: Adapted by ChatGPT for Raunak's TrendR project
# Date: 2025-11-09
# ===============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(cluster)
  library(clusterSim)
  library(Metrics)
  library(moments)
  library(lsa)
  library(zoo)
  library(tidyr)
  library(stringr)
})

message("📊 TrendR Evaluation Toolkit loaded successfully.")

# ---------------------------------------------------------------
# 1️⃣ Data Quality Metrics
# ---------------------------------------------------------------
data_quality_metrics <- function(df) {
  stopifnot(is.data.frame(df))
  n <- nrow(df)
  res <- list()
  # Missing ratio per column
  res$missing_ratio_by_col <- sapply(df, function(x) sum(is.na(x)) / length(x))
  res$completeness_score <- mean(1 - res$missing_ratio_by_col)
  # Outlier count (numeric cols only)
  num_cols <- names(df)[sapply(df, is.numeric)]
  if (length(num_cols) > 0) {
    outlier_counts <- sapply(num_cols, function(colname) {
      col <- df[[colname]]
      col <- col[!is.na(col)]
      if (length(col) < 3) return(0)
      length(boxplot.stats(col)$out)
    })
    res$outlier_counts <- outlier_counts
    res$correlation_matrix <- cor(df[num_cols], use = "pairwise.complete.obs")
  } else {
    res$outlier_counts <- NULL
    res$correlation_matrix <- NULL
  }
  return(res)
}

# ---------------------------------------------------------------
# 2️⃣ Sentiment–Engagement Relationship
# ---------------------------------------------------------------
sentiment_engagement_metrics <- function(df,
                                         sentiment_col = "avg_sentiment",
                                         velocity_col = "avg_velocity") {
  if (!(sentiment_col %in% names(df) && velocity_col %in% names(df))) {
    warning("Columns not found; skipping sentiment–engagement metrics.")
    return(NULL)
  }
  df2 <- df %>% select(all_of(c(sentiment_col, velocity_col))) %>% na.omit()
  if (nrow(df2) < 3) return(list(message = "Not enough valid pairs"))
  cor_val <- cor(df2[[sentiment_col]], df2[[velocity_col]], use = "complete.obs", method = "pearson")
  skew_sent <- moments::skewness(df2[[sentiment_col]])
  var_velo <- var(df2[[velocity_col]])
  list(
    sentiment_engagement_correlation = cor_val,
    sentiment_skewness = skew_sent,
    velocity_variance = var_velo,
    n_pairs = nrow(df2)
  )
}

# ---------------------------------------------------------------
# 3️⃣ Forecasting / Regression Metrics
# ---------------------------------------------------------------
forecast_metrics <- function(actual, predicted) {
  actual <- as.numeric(actual)
  predicted <- as.numeric(predicted)
  valid_idx <- !is.na(actual) & !is.na(predicted)
  actual <- actual[valid_idx]
  predicted <- predicted[valid_idx]
  if (length(actual) == 0) return(NULL)
  mae_val <- mae(actual, predicted)
  rmse_val <- rmse(actual, predicted)
  mape_val <- mean(abs((actual - predicted) / ifelse(actual == 0, NA, actual)), na.rm = TRUE) * 100
  ss_res <- sum((actual - predicted)^2)
  ss_tot <- sum((actual - mean(actual))^2)
  r2_val <- 1 - (ss_res / ss_tot)
  list(MAE = mae_val, RMSE = rmse_val, MAPE_pct = mape_val, R2 = r2_val, n = length(actual))
}

# Reuse for lifespan regression
regression_metrics <- forecast_metrics

# ---------------------------------------------------------------
# 4️⃣ Clustering Evaluation Metrics
# ---------------------------------------------------------------
clustering_metrics <- function(feature_df, cluster_labels = NULL, compute_cosine = TRUE) {
  mat <- as.matrix(feature_df)
  mat[is.na(mat)] <- 0
  if (is.null(cluster_labels)) {
    message("⚠️ No cluster labels provided, skipping silhouette/DB index.")
    return(list(silhouette_avg = NA, davies_bouldin = NA, cosine_similarity = NULL))
  }
  sil <- tryCatch({
    silhouette(cluster_labels, dist(mat))
  }, error = function(e) NULL)
  sil_avg <- if (!is.null(sil)) mean(sil[, "sil_width"]) else NA
  dbi <- tryCatch({
    index.DB(mat, cluster_labels, centrotypes = "centroids")$DB
  }, error = function(e) NA)
  cos_sim <- if (compute_cosine) tryCatch(cosine(t(mat)), error = function(e) NULL) else NULL
  list(silhouette_avg = sil_avg, davies_bouldin = dbi, cosine_similarity = cos_sim)
}

# ---------------------------------------------------------------
# 5️⃣ Cross-platform Overlap (optional)
# ---------------------------------------------------------------
cross_platform_overlap <- function(trend_lists) {
  if (length(trend_lists) < 2) return(NA)
  names_platforms <- names(trend_lists)
  m <- matrix(NA, nrow = length(trend_lists), ncol = length(trend_lists),
              dimnames = list(names_platforms, names_platforms))
  for (i in seq_along(trend_lists)) {
    for (j in seq_along(trend_lists)) {
      A <- unique(trend_lists[[i]]); B <- unique(trend_lists[[j]])
      inter <- length(intersect(A, B))
      uni <- length(union(A, B))
      m[i, j] <- ifelse(uni == 0, NA, inter / uni)
    }
  }
  avg_jaccard <- mean(m[lower.tri(m)], na.rm = TRUE)
  list(pairwise_matrix = m, avg_pairwise_jaccard = avg_jaccard)
}

# ---------------------------------------------------------------
# 6️⃣ Rolling Slope Detection (optional reuse in saturation)
# ---------------------------------------------------------------
rolling_slope_detection <- function(series, window = 5, threshold = 0.01) {
  series <- as.numeric(series)
  if (length(series) < window) return(list(flatten_index = NA, slopes = NA))
  slopes <- zoo::rollapply(series, width = window, FUN = function(x) {
    lm.fit <- lm(x ~ seq_along(x))
    coef(lm.fit)[2]
  }, align = "right", fill = NA)
  flat_idx <- which(slopes <= threshold)
  list(flatten_index = ifelse(length(flat_idx) == 0, NA, flat_idx[1]), slopes = slopes)
}

# ---------------------------------------------------------------
# 7️⃣ Keyword Frequency Helper (for Insight Generation)
# ---------------------------------------------------------------
keyword_frequency <- function(text_vector, top_n = 30) {
  df_text <- data.frame(text = as.character(text_vector), stringsAsFactors = FALSE)
  tokens <- df_text %>%
    unnest_tokens(word, text) %>%
    filter(!word %in% tidytext::stop_words$word) %>%
    mutate(word = str_replace_all(word, "[^[:alnum:]]", "")) %>%
    filter(nchar(word) > 2)
  freq <- tokens %>% count(word, sort = TRUE)
  head(freq, top_n)
}

# ---------------------------------------------------------------
# 8️⃣ Save Metrics Summary
# ---------------------------------------------------------------
save_metrics <- function(metrics_list, filepath = "data_evaluation/metrics_summary.RDS") {
  dir.create(dirname(filepath), showWarnings = FALSE)
  saveRDS(metrics_list, file = filepath)
  message("✅ Metrics summary saved to ", filepath)
  return(normalizePath(filepath))
}

# ---------------------------------------------------------------
# 9️⃣ Quick loader for TrendR files
# ---------------------------------------------------------------
load_trendr_file <- function(path_pattern, folder) {
  files <- list.files(folder, pattern = path_pattern, full.names = TRUE)
  if (length(files) == 0) return(NULL)
  latest <- files[which.max(file.mtime(files))]
  message("📂 Loaded latest file: ", basename(latest))
  if (grepl("\\.rds$", latest, ignore.case = TRUE)) return(readRDS(latest))
  if (grepl("\\.csv$", latest, ignore.case = TRUE)) return(read.csv(latest))
  if (grepl("\\.json$", latest, ignore.case = TRUE)) return(jsonlite::fromJSON(latest))
  return(NULL)
}

# ---------------------------------------------------------------
# 10️⃣ Summary of available functions
# ---------------------------------------------------------------
message("Functions available in evaluation_toolkit.R:")
cat("
 • data_quality_metrics(df)
 • sentiment_engagement_metrics(df)
 • forecast_metrics(actual, predicted)
 • regression_metrics(actual, predicted)
 • clustering_metrics(feature_df, cluster_labels)
 • cross_platform_overlap(trend_lists)
 • rolling_slope_detection(series, window, threshold)
 • keyword_frequency(text_vector)
 • save_metrics(metrics_list, filepath)
 • load_trendr_file(path_pattern, folder)
")
