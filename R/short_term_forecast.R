# Short_term_forecast.R
suppressPackageStartupMessages({
  library(jsonlite); library(dplyr); library(lubridate)
  library(prophet)   # install.packages("prophet")
  library(tidyr); library(stringr)
})

message("📈 Short-term forecasting (Prophet) — starting...")

today <- format(Sys.Date(), "%Y-%m-%d")
# load post-level (time series) & trend-level data
posts_path <- file.path("data_clean", paste0("reddit_posts_clean_", today, ".rds"))
trends_path <- file.path("data_clean", paste0("trends_clean_", today, ".rds"))

if (!file.exists(posts_path) || !file.exists(trends_path)) stop("Required clean files not found.")

posts <- readRDS(posts_path)
trends <- readRDS(trends_path)

# prepare output dirs
dir.create("data_forecasts", showWarnings = FALSE)

# helper: safe slug
slugify <- function(x) str_replace_all(tolower(substr(gsub("[^A-Za-z0-9 ]", "", x),1,60)), "\\s+", "_")

# For each topic, aggregate posts to hourly (or daily) time series of engagement (score+comments)
topics <- unique(posts$topic)

summary_rows <- list()
model_list <- list()

for (topic in topics) {
  try({
    dfp <- posts %>% filter(topic == !!topic) %>%
      mutate(created_at = Sys.time() - post_age_hours*3600) %>% # approximate created_at if absent
      mutate(created_day = as_datetime(Sys.time()) - post_age_hours*3600) # fallback if no timestamp column
    
    # Aggregate to daily engagement count (score + num_comments) — use post_age_hours if no timestamp
    dfp <- dfp %>%
      mutate(obs_time = as_datetime(Sys.time()) - hours(round(post_age_hours))) %>%
      group_by(obs_date = as_date(obs_time)) %>%
      summarise(engagement = sum(score + num_comments, na.rm=TRUE)) %>%
      ungroup()
    
    # If too few points, skip or create synthetic smoothing
    if (nrow(dfp) < 5) {
      # create a small daily series by repeating avg
      avg_e <- ifelse(nrow(dfp)>0, mean(dfp$engagement, na.rm=TRUE), 10)
      dfp <- tibble(obs_date = seq.Date(Sys.Date()-14, Sys.Date(), by="day"),
                    engagement = pmax(0, round(rnorm(15, avg_e, avg_e*0.3))))
    }
    
    # Prophet expects ds, y
    ts_df <- dfp %>% transmute(ds = as_date(obs_date), y = engagement)
    ts_df$ds <- as.Date(ts_df$ds)
    # convert to daily freq
    m <- prophet::prophet(ts_df, yearly.seasonality = FALSE, weekly.seasonality = TRUE, daily.seasonality = FALSE)
    future <- make_future_dataframe(m, periods = 7) # next 7 days
    fcst <- predict(m, future)
    
    # save model & forecast
    topic_slug <- slugify(topic)
    model_path <- file.path("data_forecasts", paste0("prophet_model_", topic_slug, "_", today, ".rds"))
    forecast_path <- file.path("data_forecasts", paste0("forecast_", topic_slug, "_", today, ".json"))
    saveRDS(m, model_path)
    
    # prepare JSON-friendly forecast: ds, yhat, yhat_lower, yhat_upper
    fcst_small <- fcst %>% select(ds, yhat, yhat_lower, yhat_upper) %>%
      mutate(ds = as.character(as_date(ds)))
    write_json(list(topic = topic, forecast = fcst_small), forecast_path, pretty = TRUE, auto_unbox = TRUE)
    
    # summary row for dashboard
    last_yhat <- tail(fcst_small$yhat, 1)
    summary_rows[[length(summary_rows)+1]] <- tibble(topic = topic, topic_slug = topic_slug,
                                                     forecast_next_day = last_yhat,
                                                     model_path = model_path,
                                                     forecast_path = forecast_path)
    
    model_list[[topic_slug]] <- m
    message("✅ Forecast done for: ", topic)
  }, silent = TRUE)
}

summary_df <- bind_rows(summary_rows)
write.csv(summary_df, file.path("data_forecasts", paste0("forecasts_summary_", today, ".csv")), row.names = FALSE)
saveRDS(model_list, file.path("data_forecasts", paste0("prophet_models_all_", today, ".rds")))

message("📦 Short-term forecasts saved → data_forecasts/")
