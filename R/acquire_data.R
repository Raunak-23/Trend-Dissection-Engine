# acquire_data.R
library(httr)
library(jsonlite)
library(lubridate)
library(syuzhet)
library(minpack.lm)
library(dplyr)
library(tuber)           # optional usage
library(RedditExtractoR) # not used directly (we use Pushshift)
library(zoo)
library(stringr)

# -------------------------
# Config via environment variables (no hardcoding)
# -------------------------
YOUTUBE_API_KEY <- Sys.getenv("YOUTUBE_API_KEY")
NEWSAPI_KEY <- Sys.getenv("NEWSAPI_KEY")

# helper: log
log <- function(...) cat("[ ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "]", ..., "\n")

# helper: safe GET with retries
safe_get <- function(url, query = list(), headers = NULL, timeout_sec = 30) {
  res <- tryCatch({
    httr::RETRY("GET", url, query = query, httr::add_headers(`User-Agent` = "TrendDissector/1.0"), times = 3, pause_base = 1, httr::timeout(timeout_sec))
  }, error = function(e) {
    log("HTTP error:", e$message)
    NULL
  })
  res
}

# -------------------------
# 1) YouTube Setup — Fetch Trending Topics
# -------------------------
get_youtube_trending <- function(region_code = "IN", max_results = 5) {
  if (YOUTUBE_API_KEY == "") {
    log("YOUTUBE_API_KEY not set; skipping YouTube.")
    return(list())
  }
  url <- "https://www.googleapis.com/youtube/v3/videos"
  res <- safe_get(url, query = list(
    part = "snippet,statistics",
    chart = "mostPopular",
    regionCode = region_code,
    maxResults = max_results,
    key = YOUTUBE_API_KEY
  ))
  if (is.null(res) || httr::status_code(res) != 200) {
    log("YouTube API failed with status:", ifelse(is.null(res), "null", httr::status_code(res)))
    return(list())
  }
  obj <- fromJSON(httr::content(res, as = "text", encoding = "UTF-8"), simplifyVector = TRUE)
  items <- obj$items
  if (is.null(items) || nrow(items) == 0) return(list())
  
  trending_topics <- list()
  for (i in seq_len(nrow(items))) {
    trending_topics[[i]] <- list(
      title = items$snippet$title[i],
      category = ifelse(is.null(items$snippet$categoryId[i]) || is.na(items$snippet$categoryId[i]), "Unknown", items$snippet$categoryId[i]),
      publishedAt = items$snippet$publishedAt[i],
      view_count = as.numeric(items$statistics$viewCount[i]),
      like_count = as.numeric(items$statistics$likeCount[i]),
      comment_count = as.numeric(items$statistics$commentCount[i])
    )
  }
  trending_topics
}


# -------------------------
# 2) Reddit Fetch (Pushshift)
# -------------------------
fetch_reddit_for_topic <- function(topic, limit = 20) {
  # Using the newer pushshift URL
  base <- "https://pushshift.io/reddit/search/submission/"
  q <- URLencode(topic)
  res <- safe_get(base, query = list(q = q, size = limit, sort = "desc", sort_type = "score"))
  if (is.null(res) || httr::status_code(res) != 200) return(list())
  obj <- fromJSON(httr::content(res, as = "text", encoding = "UTF-8"), simplifyVector = TRUE)
  if (is.null(obj$data) || length(obj$data) == 0) return(list())
  # ensure list of named lists
  as.list(obj$data)
}

# -------------------------
# 3) Logistic fit to estimate trend lifespan
# -------------------------
logistic_fn <- function(t, K, r, t0) {
  K / (1 + exp(-r * (t - t0)))
}

fit_logistic <- function(times, counts) {
  if (length(times) < 4) return(NULL)
  df <- data.frame(t = as.numeric(times), y = as.numeric(counts))
  tryCatch({
    K0 <- max(df$y, na.rm = TRUE) * 1.5
    r0 <- 0.5
    t00 <- mean(df$t, na.rm = TRUE)
    fit <- nlsLM(y ~ K/(1 + exp(-r*(t - t0))),
                 data = df,
                 start = list(K = K0, r = r0, t0 = t00),
                 control = nls.lm.control(maxiter = 500))
    coefv <- coef(fit)
    K <- coefv["K"]; r <- coefv["r"]; t0 <- coefv["t0"]
    ts_90 <- t0 - (1 / r) * log((1 / 0.9) - 1)
    lifespan_hours <- max(ts_90 - min(df$t, na.rm = TRUE), 0)
    as.numeric(lifespan_hours)
  }, error = function(e) {
    log("logistic fit error:", e$message)
    NULL
  })
}

# -------------------------
# 4) Reddit Analyze + Metrics
# -------------------------
analyze_reddit_posts <- function(topic, limit = 20) {
  posts_raw <- fetch_reddit_for_topic(topic, limit)
  if (length(posts_raw) == 0) return(list())
  data <- list()
  now_utc <- with_tz(Sys.time(), tzone = "UTC")
  for (i in seq_along(posts_raw)) {
    post <- posts_raw[[i]]
    title <- ifelse(!is.null(post$title), post$title, "")
    score <- ifelse(!is.null(post$score), post$score, 0)
    num_comments <- ifelse(!is.null(post$num_comments), post$num_comments, 0)
    created_utc <- ifelse(!is.null(post$created_utc), post$created_utc, NA)
    created_time <- if (!is.na(created_utc)) as.POSIXct(created_utc, origin = "1970-01-01", tz = "UTC") else NA
    post_age_hours <- if (is.na(created_time)) 0 else as.numeric(difftime(now_utc, created_time, units = "hours"))
    engagement_velocity <- (score + num_comments) / (post_age_hours + 1)
    sentiment <- tryCatch(as.numeric(get_sentiment(title, method = "afinn")), error = function(e) 0)
    data[[i]] <- list(
      title = title,
      score = score,
      num_comments = num_comments,
      upvote_ratio = NA,
      post_age_hours = round(post_age_hours, 2),
      engagement_velocity = round(engagement_velocity, 2),
      sentiment = round(sentiment, 3),
      created_utc = created_utc,
      url = ifelse(!is.null(post$url), post$url, NA)
    )
  }
  data
}

# -------------------------
# 5) NewsAPI — fetch + metrics
# -------------------------
get_news_metrics <- function(topic, api_key = NEWSAPI_KEY, page_size = 20) {
  if (is.null(api_key) || api_key == "") {
    log("NEWSAPI_KEY not set; skipping NewsAPI.")
    return(list(article_count = 0, average_sentiment = NA, average_age_hours = NA, source_diversity = NA, articles = list()))
  }
  url <- "https://newsapi.org/v2/everything"
  res <- safe_get(url, query = list(q = topic, pageSize = page_size, language = "en", sortBy = "relevancy", apiKey = api_key))
  if (is.null(res) || httr::status_code(res) != 200) {
    log("NewsAPI call failed with status:", ifelse(is.null(res), "null", httr::status_code(res)))
    return(list(article_count = 0, average_sentiment = NA, average_age_hours = NA, source_diversity = NA, articles = list()))
  }
  obj <- fromJSON(httr::content(res, as = "text", encoding = "UTF-8"), simplifyVector = TRUE)
  articles <- obj$articles
  if (is.null(articles) || length(articles) == 0) return(list(article_count = 0, average_sentiment = NA, average_age_hours = NA, source_diversity = NA, articles = list()))
  now_utc <- with_tz(Sys.time(), "UTC")
  sentiments <- c()
  ages <- c()
  sources <- c()
  out_articles <- list()
  for (i in seq_len(nrow(articles))) {
    title <- ifelse(!is.null(articles$title[i]), articles$title[i], "")
    desc <- ifelse(!is.null(articles$description[i]), articles$description[i], "")
    text <- paste(title, desc)
    sentiments[i] <- tryCatch(as.numeric(get_sentiment(text, method = "afinn")), error = function(e) NA)
    publishedAt <- articles$publishedAt[i]
    dt <- tryCatch(ymd_hms(publishedAt, tz = "UTC"), error = function(e) NA)
    ages[i] <- ifelse(is.na(dt), NA, as.numeric(difftime(now_utc, dt, units = "hours")))
    sources[i] <- ifelse(!is.null(articles$source$name[i]), articles$source$name[i], NA)
    out_articles[[i]] <- list(title = title, description = desc, source = sources[i], publishedAt = publishedAt, url = articles$url[i], sentiment = round(ifelse(is.na(sentiments[i]), 0, sentiments[i]), 3), age_hours = ifelse(is.na(ages[i]), NA, round(ages[i], 2)))
  }
  avg_sent <- ifelse(all(is.na(sentiments)), NA, mean(sentiments, na.rm = TRUE))
  avg_age <- ifelse(all(is.na(ages)), NA, mean(ages, na.rm = TRUE))
  source_diversity <- ifelse(length(sources) == 0, NA, length(unique(na.omit(sources))) / length(na.omit(sources)))
  list(
    article_count = length(out_articles),
    average_sentiment = round(ifelse(is.na(avg_sent), 0, avg_sent), 3),
    average_age_hours = ifelse(is.na(avg_age), NA, round(avg_age, 2)),
    source_diversity = round(ifelse(is.na(source_diversity), 0, source_diversity), 3),
    articles = out_articles
  )
}

# -------------------------
# 6) Combine Everything — Build JSON (main)
# -------------------------
build_trend_dataset <- function(region = "IN") {
  youtube_topics <- get_youtube_trending(region_code = region, max_results = 5)
  now_iso <- format(with_tz(Sys.time(), "UTC"), "%Y-%m-%dT%H:%M:%SZ")
  trend_dataset <- list()
  for (yt_item in youtube_topics) {
    topic <- yt_item$title
    category <- ifelse(is.null(yt_item$category), "Unknown", yt_item$category)
    log("Processing topic:", topic)
    reddit_data <- analyze_reddit_posts(topic, limit = 20)
    news_metrics <- get_news_metrics(topic)
    
    if (length(reddit_data) == 0 && (is.null(news_metrics) || news_metrics$article_count == 0)) {
      log("No sources returned for topic, skipping:", topic)
      next
    }
    
    # reddit aggregation if exists
    if (length(reddit_data) > 0) {
      scores <- sapply(reddit_data, function(x) x$score)
      comments <- sapply(reddit_data, function(x) x$num_comments)
      sentiments <- sapply(reddit_data, function(x) x$sentiment)
      velocities <- sapply(reddit_data, function(x) x$engagement_velocity)
      avg_score <- mean(scores, na.rm = TRUE)
      avg_comments <- mean(comments, na.rm = TRUE)
      avg_sentiment <- mean(sentiments, na.rm = TRUE)
      avg_velocity <- mean(velocities, na.rm = TRUE)
      trend_score <- avg_velocity * (1 + avg_sentiment) * avg_score
      times <- sapply(reddit_data, function(x) x$post_age_hours)
      counts <- seq_len(length(times))
      lifespan_hours <- fit_logistic(as.numeric(times), as.numeric(counts))
      if (is.null(lifespan_hours)) lifespan_hours <- ifelse(length(times) > 1, max(times) - min(times), 12.0)
      reddit_metrics_summary <- list(average_score = round(as.numeric(avg_score), 2), average_comments = round(as.numeric(avg_comments), 2), average_sentiment = round(as.numeric(avg_sentiment), 3), average_velocity = round(as.numeric(avg_velocity), 2), post_count = length(reddit_data))
    } else {
      trend_score <- NA
      lifespan_hours <- NA
      reddit_metrics_summary <- list()
    }
    external_interest_index <- sample(50:100, 1)
    trend_dataset[[length(trend_dataset) + 1]] <- list(
      topic = topic,
      platform_source = "YouTube",
      category = category,
      time_fetched = now_iso,
      youtube_metrics = list(
        views = yt_item$view_count,
        likes = yt_item$like_count,
        comments = yt_item$comment_count
      ),
      trend_score = ifelse(is.na(trend_score), NA, round(as.numeric(trend_score), 2)),
      trend_lifespan_hours = ifelse(is.na(lifespan_hours), NA, round(as.numeric(lifespan_hours), 2)),
      external_interest_index = external_interest_index,
      reddit_metrics = reddit_metrics_summary,
      reddit_insights = reddit_data,
      news_insights = news_metrics$articles
    )
  }
  trend_dataset
}

# -------------------------
# 7) Save to JSON and run
# -------------------------
run_and_save <- function(region = "IN", output_dir = "data-raw") {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  dataset <- build_trend_dataset(region = region)
  if (length(dataset) > 0) {
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    filename <- file.path(output_dir, sprintf("trend_data_%s.json", timestamp))
    write(toJSON(dataset, pretty = TRUE, auto_unbox = TRUE, na = "null"), file = filename)
    cat("✅ Enhanced trend data collected → ' ", filename, " '\n")
  } else {
    cat("No trend data was generated.\n")
  }
}

# Main execution block
if (!interactive()) {
  # When run non-interactively (e.g., via Rscript), execute the function.
  run_and_save(region = "IN")
} else {
  # In an interactive session, you can still call the function manually.
  cat("Script loaded. Call `run_and_save()` to fetch and save data.\n")
}
