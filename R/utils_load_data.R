# ============================================================
# utils_load_data.R
# Helper for auto-loading all daily trend data
# ============================================================

suppressPackageStartupMessages({
  library(jsonlite)
  library(dplyr)
  library(lubridate)
})

get_all_trend_data <- function(folder = "data_clean") {
  files <- list.files(folder, pattern = "^trends_clean_\\d{4}-\\d{2}-\\d{2}\\.json$", full.names = TRUE)
  if (length(files) == 0) stop("No trend files found in ", folder)

  message("📂 Found ", length(files), " files in ", folder)
  all_data <- bind_rows(lapply(files, function(f) {
    d <- fromJSON(f)
    d$source_file <- basename(f)
    d$trend_date <- as.Date(d$trend_date)
    d
  }))
  all_data <- all_data %>%
    arrange(topic, trend_date) %>%
    distinct(topic, trend_date, .keep_all = TRUE)
  return(all_data)
}
