# timeseries_dsp.R
library(tidyverse)
library(pracma)
library(stats)

# ---- Build time series ----
build_timeseries <- function(token_counts_df, token = "ai", platform = "youtube") {
  df <- token_counts_df %>%
    filter(word == token, platform == platform) %>%
    rename(date = created_date) %>%
    group_by(date) %>%
    summarise(count = sum(n), .groups = "drop")
  
  if (nrow(df) == 0) return(NULL)
  
  full <- tibble(date = seq(min(df$date), max(df$date), by = "day")) %>%
    left_join(df, by = "date") %>%
    mutate(count = replace_na(count, 0))
  
  ts_obj <- ts(full$count, frequency = 7, start = c(year(min(full$date)), yday(min(full$date))))
  attr(ts_obj, "dates") <- full$date
  return(list(ts = ts_obj, df = full))
}

# ---- FFT analysis ----
fft_analysis <- function(ts_vector) {
  if (length(ts_vector) < 4) return(NULL)
  fr <- fft(ts_vector)
  n <- length(fr)
  freq <- (0:(n - 1)) / n
  amplitude <- Mod(fr)
  tibble(Frequency = freq[1:floor(n/2)], Amplitude = amplitude[1:floor(n/2)])
}
