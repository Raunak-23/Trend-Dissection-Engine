# ===============================================================
# Insight_generation.R (Robust Version)
# Trend Dissection Engine — Insight Generation & Keyword Analysis
# ===============================================================

suppressPackageStartupMessages({
  library(jsonlite)
  library(dplyr, warn.conflicts = FALSE)
  library(tidytext)
  library(tidyr)
  library(stringr)
  library(ggplot2)
  library(lubridate)
})

message("🧠 Insight Generation — starting...")

# ---- Load helper ----
source("R/utils_load_data.R")
trends_all <- get_all_trend_data("data_clean")

# create folder
dir.create("data_insights", showWarnings = FALSE)
today <- format(Sys.Date(), "%Y-%m-%d")

# ---------------------------------------------------------------
# 1️⃣ Normalize & prepare text columns
# ---------------------------------------------------------------
# Handle nested reddit_metrics if present
if ("reddit_metrics" %in% names(trends_all)) {
  reddit_cols <- c("average_score", "average_comments", "average_sentiment", "average_velocity")
  for (col in reddit_cols) {
    trends_all[[col]] <- purrr::map_dbl(trends_all$reddit_metrics, ~ .x[[col]] %||% NA_real_)
  }
}

# Ensure required columns exist (fill NAs if missing)
needed_cols <- c(
  "topic", "platform_source", "avg_sentiment", "avg_velocity",
  "trend_intensity", "insta_engagement", "category"
)
for (col in needed_cols) {
  if (!col %in% names(trends_all)) {
    trends_all[[col]] <- NA
  }
}

# Feedback
message("✅ Columns available for insight generation: ", 
        paste(intersect(needed_cols, names(trends_all)), collapse = ", "))

# Clean and tokenize
text_df <- trends_all %>%
  dplyr::select(dplyr::any_of(needed_cols)) %>%
  dplyr::mutate(
    topic_clean = stringr::str_to_lower(topic),
    topic_clean = stringr::str_replace_all(topic_clean, "http\\S+|www\\S+", ""),
    topic_clean = stringr::str_replace_all(topic_clean, "[^a-z\\s]", " ")
  ) %>%
  tidytext::unnest_tokens(word, topic_clean)


# ---------------------------------------------------------------
# 2️⃣ Remove stopwords & short words
# ---------------------------------------------------------------
data("stop_words")
tokens <- text_df %>%
  anti_join(stop_words, by = "word") %>%
  filter(str_length(word) > 2)

# ---------------------------------------------------------------
# 3️⃣ Keyword frequency + tf-idf
# ---------------------------------------------------------------
keyword_freq <- tokens %>%
  count(word, sort = TRUE)

keyword_tfidf <- tokens %>%
  count(platform_source, word, sort = TRUE) %>%
  bind_tf_idf(word, platform_source, n) %>%
  arrange(desc(tf_idf))

# ---------------------------------------------------------------
# 4️⃣ Keyword-level correlations
# ---------------------------------------------------------------
keyword_metrics <- tokens %>%
  group_by(word) %>%
  summarise(
    avg_sentiment = mean(avg_sentiment, na.rm = TRUE),
    avg_velocity = mean(avg_velocity, na.rm = TRUE),
    avg_intensity = mean(trend_intensity, na.rm = TRUE),
    avg_insta_eng = mean(insta_engagement, na.rm = TRUE),
    count = n()
  ) %>%
  filter(count > 1) %>%
  mutate(
    engagement_rank = rank(-avg_velocity),
    sentiment_rank = rank(-avg_sentiment),
    combined_score = 0.5 * scale(avg_velocity) + 0.5 * scale(avg_sentiment)
  )

# ---------------------------------------------------------------
# 5️⃣ Visualization (optional)
# ---------------------------------------------------------------
if (nrow(keyword_metrics) > 0) {
  top_keywords <- keyword_metrics %>% arrange(desc(combined_score)) %>% head(15)
  p <- ggplot(top_keywords, aes(x = reorder(word, combined_score), y = combined_score)) +
    geom_col(fill = "#3E7DD2") +
    coord_flip() +
    labs(
      title = "Top Keywords by Sentiment–Engagement Score",
      x = "Keyword", y = "Score (Scaled)"
    ) +
    theme_minimal()
  ggsave(file.path("data_insights", paste0("keyword_insights_plot_", today, ".png")), p, width = 7, height = 5)
}

# ---------------------------------------------------------------
# 6️⃣ Save outputs
# ---------------------------------------------------------------
write.csv(keyword_metrics,
          file.path("data_insights", paste0("keyword_insights_", today, ".csv")),
          row.names = FALSE)

write_json(keyword_metrics,
           file.path("data_insights", paste0("keyword_insights_", today, ".json")),
           pretty = TRUE, auto_unbox = TRUE)

message("✅ Keyword insights saved → data_insights/")
