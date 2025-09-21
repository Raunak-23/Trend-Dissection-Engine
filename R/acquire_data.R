# acquire_data.R
library(tidyverse)
library(lubridate)
library(mongolite)

# Standard platforms
STANDARD_PLATFORMS <- c("youtube", "reddit", "facebook", "twitter", "gtrends", "mastodon", "rss")

normalize_platform <- function(x) {
  x <- tolower(x)
  ifelse(x %in% STANDARD_PLATFORMS, x, NA_character_)
}

# ---- Main Data Getter ----
get_data <- function(mongo_conn = NULL, date_from = Sys.Date()-30, date_to = Sys.Date()) {
  if (!is.null(mongo_conn)) {
    # Uncomment & adapt for real Mongo:
    # q <- sprintf('{"created_utc": {"$gte": {"$date": "%s"}, "$lte": {"$date": "%s"}}}',
    #              paste0(as.character(as.POSIXct(date_from)), "T00:00:00Z"),
    #              paste0(as.character(as.POSIXct(date_to)), "T23:59:59Z"))
    # data_json <- mongo_conn$find(query = q)
    # return(as_tibble(data_json))
    
    stop("Mongo mode not configured. Using fallback demo data.")
  }
  
  # --- Demo fallback data ---
  set.seed(42)
  days <- seq.Date(date_from, date_to, by = "day")
  sample_df <- expand.grid(
    date = days,
    platform = c("youtube", "reddit", "facebook"),
    keyword = c("AI", "chatgpt", "shiny"),
    stringsAsFactors = FALSE
  ) %>%
    as_tibble() %>%
    mutate(
      created_utc = as.POSIXct(date) + sample(0:86400, nrow(.), replace = TRUE),
      ingest_tstamp = Sys.time(),
      api_unique_id = paste0(platform, "_", row_number()),
      url = paste0("https://example.com/", api_unique_id),
      text_body = paste(keyword, "demo text"),
      lang = "en",
      author_id = paste0("a", sample(1:1000, nrow(.), replace = TRUE)),
      author_name = paste0("Author", sample(1:1000, nrow(.), replace = TRUE)),
      author_followers = sample(c(NA, 50:10000), nrow(.), replace = TRUE),
      is_verified = sample(c(TRUE, FALSE), nrow(.), replace = TRUE),
      view_count = sample(c(NA, 100:100000), nrow(.), replace = TRUE),
      like_count = sample(0:2000, nrow(.), replace = TRUE),
      dislike_count = NA_integer_,
      comment_count = sample(0:500, nrow(.), replace = TRUE),
      share_count = sample(0:300, nrow(.), replace = TRUE),
      score = NA_integer_,
      upvote_ratio = NA_real_,
      tags = list(c("demo", keyword)),
      topic_category = NA_character_,
      lat = NA_real_,
      lon = NA_real_,
      country = sample(c("US", "IN", "GB", NA), nrow(.), replace = TRUE),
      thumbnail_url = NA_character_,
      duration_sec = NA_integer_,
      live_status = "none",
      api_raw = NA
    ) %>%
    select(-date)
  
  return(sample_df)
}
