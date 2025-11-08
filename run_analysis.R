# Required packages
required_packages <- c("jsonlite", "dplyr", "lubridate", "ggplot2", "prophet", 
                      "cluster", "tidyr", "stringr", "purrr", "zoo")

# Install missing packages
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load required packages
lapply(required_packages, library, character.only = TRUE)

# Set working directory to script location
if(interactive()) {
  script_path <- dirname(rstudioapi::getSourceEditorContext()$path)
  setwd(script_path)
}

# Create required directories
for(dir in c("data-clean", "visualizations", "data-raw")) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
}

# Process YouTube data
message("Step 1: Processing YouTube data...")
source("R/process_youtube.R")
input_file <- "youtube_trending_IN_20251106_003758_raw.json"
if(!file.exists(input_file)) {
  stop("Input file not found: ", input_file)
}

youtube_data <- tryCatch({
  process_youtube_data(input_file, "data-clean/processed_youtube.json")
}, error = function(e) {
  message("Error processing YouTube data: ", e$message)
  stop(e)
})

# Extract features
message("Step 2: Extracting features...")
source("R/feature_extraction.R")

# Convert YouTube data to time series format
youtube_series <- lapply(youtube_data, function(item) {
  # Create time series data frame
  df <- data.frame(
    period = as.POSIXct(item$metadata$publishedAt),
    engagement_sum = item$engagement$views + item$engagement$likes + item$engagement$comments,
    sentiment_mean = 0,  # placeholder for sentiment
    stringsAsFactors = FALSE
  )
  
  list(
    df = df,
    topic = item$topic,
    metadata = item$metadata
  )
})

# Extract features for each video
features <- lapply(youtube_series, function(series) {
  tryCatch({
    extract_features_for_topic(series)
  }, error = function(e) {
    message("Error extracting features for topic: ", series$topic)
    message("Error: ", e$message)
    NULL
  })
})

# Remove NULL results
features <- features[!sapply(features, is.null)]

# Generate visualizations
message("Step 3: Generating visualizations...")

# 1. Engagement over time plot
png("visualizations/engagement_trends.png", width = 1200, height = 800)
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))
for(i in 1:min(4, length(features))) {
  f <- features[[i]]
  plot(f$timeseries$engagement_sum, type = "l",
       main = substr(f$summary$topic, 1, 30),
       xlab = "Time Index", ylab = "Engagement")
}
dev.off()

# 2. Feature summary heatmap
if(requireNamespace("pheatmap", quietly = TRUE)) {
  library(pheatmap)
  feature_matrix <- do.call(rbind, lapply(features, function(f) {
    unlist(f$summary[c("peak_val", "time_to_peak", "first_window_velocity", "total_volume")])
  }))
  rownames(feature_matrix) <- sapply(features, function(f) substr(f$summary$topic, 1, 30))
  png("visualizations/feature_heatmap.png", width = 1200, height = 800)
  pheatmap(scale(feature_matrix))
  dev.off()
}

# Generate summary report
message("Step 4: Generating summary report...")
summary_df <- do.call(rbind, lapply(features, function(f) {
  data.frame(
    Topic = f$summary$topic,
    Peak_Engagement = f$summary$peak_val,
    Time_to_Peak = f$summary$time_to_peak,
    Total_Volume = f$summary$total_volume,
    Average_Sentiment = mean(f$timeseries$sentiment_mean, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}))

# Save summary report
write.csv(summary_df, "data-clean/trend_analysis_summary.csv", row.names = FALSE)

# Save full analysis results
saveRDS(list(
  youtube_data = youtube_data,
  features = features,
  summary = summary_df
), "data-clean/full_analysis_results.rds")

message("✅ Analysis complete! Check the following files:")
message("📊 Visualizations: visualizations/")
message("📑 Summary Report: data-clean/trend_analysis_summary.csv")
message("💾 Full Results: data-clean/full_analysis_results.rds")