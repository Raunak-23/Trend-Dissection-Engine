library(zoo)
detect_saturation <- function(topic_features, roll_win = 5, slope_threshold = 0.1) {
  df <- topic_features$timeseries %>% arrange(period)
  # compute rolling slope via linear regression in window
  n <- nrow(df)
  if(n < roll_win) return(tibble(period = df$period, saturation_flag = FALSE))
  
  slopes <- rep(NA, n)
  for(i in seq(roll_win, n)) {
    window_vals <- df$engagement_sum[(i-roll_win+1):i]
    x <- 1:length(window_vals)
    fit <- lm(window_vals ~ x)
    slopes[i] <- coef(fit)[2]
  }
  # saturation: slope falling below threshold and not increasing on average
  sat_flag <- ifelse(!is.na(slopes) & slopes < slope_threshold, TRUE, FALSE)
  out <- tibble(period = df$period, engagement = df$engagement_sum, slope = slopes, saturation = sat_flag)
  return(out)
}

# Example usage:
# sat <- detect_saturation(feats_list[[1]], roll_win = 6, slope_threshold = 0.5)
# plot(sat$period, sat$slope, type='l'); points(sat$period[sat$saturation], sat$slope[sat$saturation], col='red')
