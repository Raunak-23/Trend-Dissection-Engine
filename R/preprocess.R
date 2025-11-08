library(jsonlite)
library(dplyr)
library(lubridate)
library(purrr)
library(tidyr)
library(zoo)
library(stringr)

# ---- Helper function: read all json files in a directory ----
read_topics_from_dir <- function(dir = "data/", time_unit = "hour") {
  files <- list.files(dir, pattern = "\\.json$", full.names = TRUE)
  if(length(files) == 0) stop("No JSON files found in dir: ", dir)
  
  topics <- map(files, function(f) {
    raw <- fromJSON(f, simplifyVector = FALSE)
    # raw might be a list of topics or single topic; normalize to list
    # Each raw element is expected to have: topic (string), reddit_posts (list)
    # keep also file path as source
    tibble(file = f, payload = list(raw))
  }) %>% bind_rows()
  
  # We'll expand each payload's topics into a row-per-topic
  topics_expanded <- topics %>% 
    mutate(topic_list = map(payload, function(pl) {
      # if pl is already a list of topic objects
      if(is.list(pl) && length(pl) > 0 && is.list(pl[[1]]) && !is.null(pl[[1]]$topic)) {
        return(pl)
      } else {
        # fallback: treat entire payload as single topic object
        return(list(pl))
      }
    })) %>%
    select(-payload) %>%
    unnest_longer(topic_list) %>%
    mutate(topic_obj = topic_list) %>%
    select(-topic_list)
  
  build_series <- function(topic_obj) {
    tname <- topic_obj$title %||% "unknown_topic"
    
    # Process Reddit posts
    reddit_posts <- topic_obj$reddit_insights
    if (!is.null(reddit_posts) && length(reddit_posts) > 0) {
      reddit_df <- bind_rows(reddit_posts) %>%
        transmute(
          timestamp = as_datetime(created_utc),
          engagement = score + num_comments,
          sentiment = 0 # Placeholder, sentiment analysis can be added here
        )
    } else {
      reddit_df <- tibble(timestamp = POSIXct(), engagement = double(), sentiment = double())
    }
    
    # Process News articles
    news_articles <- topic_obj$news_insights
    if (!is.null(news_articles) && length(news_articles) > 0) {
      news_df <- bind_rows(news_articles) %>%
        transmute(
          timestamp = ymd_hms(publishedAt, tz = "UTC"),
          engagement = 1, # Each article counts as an engagement of 1
          sentiment = 0 # Placeholder
        )
    } else {
      news_df <- tibble(timestamp = POSIXct(), engagement = double(), sentiment = double())
    }
    
    # Combine sources
    combined_df <- bind_rows(reddit_df, news_df) %>%
      filter(!is.na(timestamp))

    if(nrow(combined_df) == 0) {
      return(NULL)
    }
    
    # Aggregation logic (remains the same)
    agg <- combined_df %>%
      mutate(period = floor_date(timestamp, unit = ifelse(time_unit == "hour", "hour", "day")))
    
    agg <- agg %>%
      group_by(period) %>%
      summarise(
        n_posts = n(),
        engagement_sum = sum(engagement, na.rm=TRUE),
        engagement_mean = mean(engagement, na.rm=TRUE),
        sentiment_mean = mean(sentiment, na.rm=TRUE)
      ) %>% ungroup() %>% 
      arrange(period)
    
    # pad missing periods with zeros to make contiguous series
    if(nrow(agg) > 0) {
      full_periods <- tibble(period = seq(min(agg$period), max(agg$period), by = ifelse(time_unit=="hour","hour","day")))
      agg_full <- full_join(full_periods, agg, by = "period") %>%
        arrange(period) %>%
        mutate(across(c(n_posts,engagement_sum,engagement_mean,sentiment_mean), ~replace_na(.x, 0)))
    } else {
      agg_full <- tibble(period = POSIXct(character()), n_posts = integer(), engagement_sum=double(), engagement_mean=double(), sentiment_mean=double())
    }
    list(topic = tname, df = agg_full, metadata = list(topic_meta = topic_obj))
  }
  
  topic_series <- map(topics_expanded$topic_obj, build_series)
  # filter out nulls
  topic_series <- compact(topic_series)
  return(topic_series)
}

# Example usage:
# topics <- read_topics_from_dir("path/to/json_dir", time_unit = "hour")
# inspect first
# str(topics[[1]]$df)
