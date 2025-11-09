# ===============================================================
# Insight_generation.R
# Trend Dissection Engine — Insight Generation & Keyword Analysis
# ===============================================================
# Extract interpretable insights linking keywords to engagement,
# sentiment, and intensity metrics.
# Outputs: data_insights/keyword_insights_<date>.csv and .json
# ===============================================================

suppressPackageStartupMessages({
  library(jsonlite)
  library(dplyr)
  library(tidytext)
  library(tidyr)
  library(stringr)
  library(ggplot2)
  library(lubridate)
})

message("🧠 Insight Generation — starting...")

# helper: load combined trend data
source("R/utils_load_data.R")
trends_all <- get_all_trend_data("data_clean")

# create folder
dir.create("data_insights", showWarnings = FALSE)
today <- format(Sys.Date(), "%Y-%m-%d")

# ---------------------------------------------------------------
# 1️⃣ Text Cleaning and Tokenization
# ---------------------------------------------------------------
text_df <- trends_all %>%
  select(topic, platform_source, avg_sentiment, avg_velocity,
         trend_intensity, insta_engagement, category) %>%
  mutate(topic_clean = str_to_lower(topic),
         topic_clean = str_replace_all(topic_clean, "http\\S+|www\\S+", ""),
         topic_clean = str_replace_all(topic_clean, "[^a-z\\s]", " ")) %>%
  unnest_tokens(word, topic_clean)

# remove stopwords
data("stop_words")
tokens <- text_df %>%
  anti_join(stop_words, by = "word") %>%
  filter(str_length(word) > 2)

# ---------------------------------------------------------------
# 2️⃣ Keyword Frequency and tf-idf
# ---------------------------------------------------------------
keyword_freq <- tokens %>%
  count(word, sort = TRUE)

keyword_tfidf <- tokens %>%
  count(platform_source, word, sort = TRUE) %>%
  bind_tf_idf(word, platform_source, n) %>%
  arrange(desc(tf_idf))

# ---------------------------------------------------------------
# 3️⃣ Keyword-Level Correlations
# ---------------------------------------------------------------
# Compute average engagement/sentiment per word
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
# 4️⃣ Visual Preview (optional for static report)
# ---------------------------------------------------------------
top_keywords <- keyword_metrics %>%
  arrange(desc(combined_score)) %>%
  head(15)

ggplot(top_keywords, aes(x = reorder(word, combined_score), y = combined_score)) +
  geom_col(fill = "#3E7DD2") +
  coord_flip() +
  labs(
    title = "Top Keywords by Combined Sentiment–Engagement Score",
    x = "Keyword", y = "Score (Scaled)"
  ) +
  theme_minimal()
ggsave(file.path("data_insights", paste0("keyword_insights_plot_", today, ".png")), width = 7, height = 5)

# ---------------------------------------------------------------
# 5️⃣ Save Outputs
# ---------------------------------------------------------------
write.csv(keyword_metrics, file.path("data_insights", paste0("keyword_insights_", today, ".csv")), row.names = FALSE)
write_json(keyword_metrics, file.path("data_insights", paste0("keyword_insights_", today, ".json")),
           pretty = TRUE, auto_unbox = TRUE)

message("✅ Keyword insights saved → data_insights/")
