# Step-by-step execution script
library(jsonlite)
library(dplyr)
library(lubridate)
library(ggplot2)

# Set working directory to project root
setwd("d:/TrendR")

# Step 1: Process YouTube data
source("R/process_youtube.R")
youtube_data <- process_youtube_data(
  input_file = "youtube_trending_IN_20251106_003758_raw.json",
  output_file = "data-clean/processed_youtube.json"
)

# Step 2: Analyze trends
source("R/feature_extraction.R")
features <- lapply(youtube_data, function(item) {
  extract_features_for_topic(list(
    df = data.frame(
      period = as.POSIXct(item$metadata$publishedAt),
      engagement_sum = item$engagement$views + item$engagement$likes + item$engagement$comments,
      sentiment_mean = 0  # We'll add sentiment analysis later
    ),
    topic = item$topic,
    metadata = item$metadata
  ))
})

# Step 3: Visualize results
# Create visualizations directory
dir.create("visualizations", showWarnings = FALSE)

# Plot engagement over time for top videos
top_features <- features[!sapply(features, is.null)][1:5]  # Take first 5 valid features

# Create engagement plot
png("visualizations/top_trends_engagement.png", width = 1200, height = 800)
par(mfrow = c(2, 3), mar = c(4, 4, 2, 1))
for(i in seq_along(top_features)) {
  feature <- top_features[[i]]
  plot(feature$timeseries$t_index, 
       feature$timeseries$engagement_sum,
       type = "l",
       main = substr(feature$summary$topic, 1, 30),
       xlab = "Time Index",
       ylab = "Engagement")
}
dev.off()

# Save processed results
saveRDS(features, "data-clean/processed_features.rds")

# Generate summary report
summary_report <- data.frame(
  Topic = sapply(features[!sapply(features, is.null)], function(f) substr(f$summary$topic, 1, 50)),
  Peak_Engagement = sapply(features[!sapply(features, is.null)], function(f) f$summary$peak_val),
  Total_Volume = sapply(features[!sapply(features, is.null)], function(f) f$summary$total_volume),
  Time_to_Peak = sapply(features[!sapply(features, is.null)], function(f) f$summary$time_to_peak)
)

# Write summary report
write.csv(summary_report, "data-clean/trend_summary.csv", row.names = FALSE)

# Print completion message
cat("\n✅ Analysis complete! Results saved in data-clean/ and visualizations/ directories\n")