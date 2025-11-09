# ===============================================================
# insights.R — Robust Insight Generation (tokenize, tf-idf, keyword correlations)
# Safeguards empty inputs and always writes output files (may be empty).
# ===============================================================
suppressPackageStartupMessages({
  library(jsonlite)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(tidytext)
  library(ggplot2)
  library(lubridate)
  library(scales)
})

message("🧠 Insight Generation — starting...")

# Ensure data_insights folder exists
dir.create("data_insights", showWarnings = FALSE)

# Try to obtain trends data via get_all_trend_data() if available, else fallback to latest trends file
trends_all <- NULL
if (file.exists("R/utils_load_data.R")) {
  try({
    source("R/utils_load_data.R", local = TRUE)
    trends_all <- tryCatch(get_all_trend_data("data_clean"), error = function(e) NULL)
  }, silent = TRUE)
}

# fallback: load latest trends_clean JSON or RDS if get_all_trend_data wasn't available
if (is.null(trends_all)) {
  files_json <- list.files("data_clean", pattern = "^trends_clean_.*\\.(json|rds|csv)$", full.names = TRUE)
  if (length(files_json) > 0) {
    latest <- files_json[which.max(file.mtime(files_json))]
    message("📂 Loading latest trends file fallback: ", latest)
    if (grepl("\\.rds$", latest, ignore.case = TRUE)) {
      trends_all <- readRDS(latest)
    } else if (grepl("\\.json$", latest, ignore.case = TRUE)) {
      trends_all <- jsonlite::fromJSON(latest, simplifyVector = TRUE)
    } else if (grepl("\\.csv$", latest, ignore.case = TRUE)) {
      trends_all <- read.csv(latest, stringsAsFactors = FALSE)
    }
  } else {
    message("⚠️ No trends files found in data_clean/. Creating empty output and exiting insights.R")
    # create empty outputs and exit gracefully
    empty_df <- tibble(word = character(), count = integer(), combined_score = numeric())
    write.csv(empty_df, file.path("data_insights", paste0("keyword_insights_", format(Sys.Date(), "%Y-%m-%d"), ".csv")), row.names = FALSE)
    jsonlite::write_json(list(), file.path("data_insights", paste0("keyword_insights_", format(Sys.Date(), "%Y-%m-%d"), ".json")), auto_unbox = TRUE, pretty = TRUE)
    quit(save = "no")
  }
}

# Ensure it's a tibble / dataframe
trends_all <- as.data.frame(trends_all, stringsAsFactors = FALSE)

# Normalize column names (lowercase) and make sure commonly used cols exist
names(trends_all) <- tolower(names(trends_all))

# If topic field is not present try 'title' or 'topic'
if (!"topic" %in% names(trends_all)) {
  possible <- intersect(c("title", "name"), names(trends_all))
  if (length(possible) > 0) trends_all$topic <- trends_all[[possible[1]]] else trends_all$topic <- NA_character_
}

# Ensure fields used for grouping/metrics exist (create NA columns if absent)
safe_cols <- c("platform_source", "avg_sentiment", "avg_velocity", "trend_intensity", "insta_engagement")
for (c in safe_cols) if (!c %in% names(trends_all)) trends_all[[c]] <- NA

# Only keep rows with non-empty topics
trends_all <- trends_all %>%
  mutate(topic = as.character(topic),
         topic_clean = topic %>% str_to_lower() %>% str_replace_all("http\\S+|www\\S+", "") %>%
           str_replace_all("[^a-z\\s]", " ") %>% str_squish()) %>%
  filter(!is.na(topic_clean) & topic_clean != "")

# If after cleaning there's nothing, write empty outputs and exit gracefully
if (nrow(trends_all) == 0) {
  message("⚠️ No non-empty topics after cleaning. Writing empty insight outputs and exiting.")
  empty_df <- tibble(word = character(), count = integer(), combined_score = numeric())
  write.csv(empty_df, file.path("data_insights", paste0("keyword_insights_", format(Sys.Date(), "%Y-%m-%d"), ".csv")), row.names = FALSE)
  jsonlite::write_json(list(), file.path("data_insights", paste0("keyword_insights_", format(Sys.Date(), "%Y-%m-%d"), ".json")), auto_unbox = TRUE, pretty = TRUE)
  quit(save = "no")
}

# Tokenization and stopword removal (safe flow)
tryCatch({
  tokens <- trends_all %>%
    dplyr::select(topic, platform_source, avg_sentiment, avg_velocity, trend_intensity, insta_engagement, topic_clean) %>%
    unnest_tokens(word, topic_clean) %>%
    anti_join(tidytext::stop_words, by = "word") %>%
    filter(!str_detect(word, "^[0-9]+$")) %>%
    filter(str_length(word) > 2)
}, error = function(e) {
  message("❌ Error during tokenization: ", e$message)
  tokens <- tibble(word = character(), topic = character(), platform_source = character(),
                   avg_sentiment = numeric(), avg_velocity = numeric(), trend_intensity = numeric(),
                   insta_engagement = numeric())
})

# Compute keyword frequencies and tf-idf per platform
if (nrow(tokens) == 0) {
  message("⚠️ Token set is empty after tokenization/stopword removal. Writing empty outputs.")
  empty_df <- tibble(word = character(), count = integer(), combined_score = numeric())
  write.csv(empty_df, file.path("data_insights", paste0("keyword_insights_", format(Sys.Date(), "%Y-%m-%d"), ".csv")), row.names = FALSE)
  jsonlite::write_json(list(), file.path("data_insights", paste0("keyword_insights_", format(Sys.Date(), "%Y-%m-%d"), ".json")), auto_unbox = TRUE, pretty = TRUE)
  quit(save = "no")
}

keyword_freq <- tokens %>%
  count(word, sort = TRUE) %>%
  rename(count = n)

keyword_tfidf <- tokens %>%
  count(platform_source, word, sort = TRUE) %>%
  bind_tf_idf(word, platform_source, n) %>%
  arrange(desc(tf_idf))

# Per-word metrics: average sentiment, velocity, intensity, insta_engagement
keyword_metrics <- tokens %>%
  group_by(word) %>%
  summarise(
    avg_sentiment = mean(as.numeric(avg_sentiment), na.rm = TRUE),
    avg_velocity = mean(as.numeric(avg_velocity), na.rm = TRUE),
    avg_intensity = mean(as.numeric(trend_intensity), na.rm = TRUE),
    avg_insta_eng = mean(as.numeric(insta_engagement), na.rm = TRUE),
    count = n(),
    .groups = "drop"
  ) %>%
  filter(count >= 1) %>%
  mutate(
    combined_score = ifelse(is.na(avg_velocity) & is.na(avg_sentiment), 0,
                            scale(ifelse(is.na(avg_velocity), 0, avg_velocity))[,1] * 0.5 +
                              scale(ifelse(is.na(avg_sentiment), 0, avg_sentiment))[,1] * 0.5)
  ) %>%
  arrange(desc(combined_score))

# Protect combined_score NAs
keyword_metrics$combined_score[is.na(keyword_metrics$combined_score)] <- 0

# Save outputs (CSV + JSON)
date_str <- format(Sys.Date(), "%Y-%m-%d")
out_csv <- file.path("data_insights", paste0("keyword_insights_", date_str, ".csv"))
out_json <- file.path("data_insights", paste0("keyword_insights_", date_str, ".json"))

write.csv(keyword_metrics, out_csv, row.names = FALSE)
jsonlite::write_json(keyword_metrics, out_json, auto_unbox = TRUE, pretty = TRUE)

message("✅ Keyword insights saved → ", out_csv, " and ", out_json)
message("🧠 Insight Generation — completed.")
