library(prophet)

forecast_with_prophet_for_topic <- function(topic_features, periods = 24, freq = "hour") {
  df <- topic_features$timeseries %>% select(period, engagement_sum)
  if(nrow(df) < 5) {
    warning("Not enough points for Prophet model for topic: ", topic_features$summary$topic)
    return(NULL)
  }
  mdf <- df %>% rename(ds = period, y = engagement_sum)
  # Prophet expects numeric y; fill NAs
  mdf$y[is.na(mdf$y)] <- 0
  
  # Fit model
  m <- prophet(mdf, weekly.seasonality = TRUE, daily.seasonality = (freq == "hour"), yearly.seasonality = FALSE)
  
  # make future dataframe
  if(freq == "hour") {
    future <- make_future_dataframe(m, periods = periods, freq = "hour")
  } else {
    future <- make_future_dataframe(m, periods = periods, freq = "day")
  }
  fc <- predict(m, future)
  
  # attach back into a tidy frame and compute simple metrics
  result <- fc %>% select(ds, yhat, yhat_lower, yhat_upper)
  list(model = m, forecast = result)
}

# Batch apply prophet to all topics, returning forecasts
batch_forecast_prophet <- function(features_list, periods = 24, freq = "hour") {
  res <- map(features_list, function(fx) {
    tryCatch({
      fc_res <- forecast_with_prophet_for_topic(fx, periods = periods, freq = freq)
      list(topic = fx$summary$topic, forecast = fc_res$forecast, model = fc_res$model)
    }, error = function(e) {
      message("Prophet failed for topic: ", fx$summary$topic, " - ", e$message)
      return(NULL)
    })
  })
  compact(res)
}

# Example usage:
# feats_list <- map(topics, extract_features_for_topic)
# prophet_results <- batch_forecast_prophet(feats_list, periods = 48, freq = "hour")
