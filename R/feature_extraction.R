library(dplyr)
library(zoo)
#Feature extraction (velocity, rolling averages, early-window metrics)

extract_features_for_topic <- function(topic_series, roll_window = 3) {
  # topic_series$df must have period and engagement_sum columns
  df <- topic_series$df %>% arrange(period)
  if(nrow(df) == 0) return(NULL)
  
  df <- df %>%
    mutate(
      t_index = row_number(),
      cum_eng = cumsum(engagement_sum),
      # velocity: difference per step (use small epsilon to avoid NaN)
      velocity = c(NA, diff(engagement_sum)),
      # relative velocity (pct)
      rel_velocity = ifelse(lag(engagement_sum,1)==0, NA, (engagement_sum - lag(engagement_sum,1)) / (abs(lag(engagement_sum,1)) + 1e-9)),
      # rolling averages
      roll_mean_eng = rollapply(engagement_sum, width = roll_window, FUN = mean, align = "right", fill = NA),
      roll_sd_eng = rollapply(engagement_sum, width = roll_window, FUN = sd, align = "right", fill = NA),
      roll_mean_sent = rollapply(sentiment_mean, width = roll_window, FUN = mean, align = "right", fill = NA)
    )
  
  # summary features that many models use:
  # peak, time to peak, mean velocity first N periods
  peak_val <- max(df$engagement_sum, na.rm = TRUE)
  peak_idx <- which.max(df$engagement_sum)
  time_to_peak <- ifelse(length(peak_idx)>0, peak_idx - 1, NA)
  first_window <- head(df$engagement_sum, n = min(5, nrow(df)))
  first_window_velocity <- ifelse(length(first_window) > 1, mean(diff(first_window)), NA)
  total_volume <- sum(df$engagement_sum, na.rm=TRUE)
  
  features <- list(
    timeseries = df,
    summary = tibble(
      topic = topic_series$topic,
      peak_val = peak_val,
      time_to_peak = time_to_peak,
      first_window_velocity = first_window_velocity,
      total_volume = total_volume,
      avg_sentiment = mean(df$sentiment_mean, na.rm=TRUE),
      n_periods = nrow(df)
    )
  )
  return(features)
}

# Example usage:
# feats <- extract_features_for_topic(topics[[1]], roll_window = 3)
# print(feats$summary)
