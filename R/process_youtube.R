library(jsonlite)
library(dplyr)
library(lubridate)

# Function to process YouTube data
process_youtube_data <- function(input_file, output_file) {
  # Read raw JSON from build_trend_dataset output
  raw_data <- fromJSON(input_file, simplifyDataFrame = FALSE)
  
  clean_data <- lapply(raw_data, function(item) {
    # Extract directly from the enriched item
    topic <- item$topic
    platform_source <- item$platform_source
    category <- item$category
    time_fetched <- item$time_fetched
    
    # Extract engagement metrics from reddit_metrics if available
    # Fallback to 0 if not present or not applicable
    reddit_metrics <- item$reddit_metrics
    views <- ifelse(!is.null(reddit_metrics$average_score), reddit_metrics$average_score, 0)
    likes <- ifelse(!is.null(reddit_metrics$average_score), reddit_metrics$average_score, 0) # Using score as a proxy for likes
    comments <- ifelse(!is.null(reddit_metrics$average_comments), reddit_metrics$average_comments, 0)
    
    # Extract metadata from reddit_posts or news_metrics if available
    # This part is more complex as it depends on what metadata is desired
    # For simplicity, let's take some basic info from the first reddit post if available
    publishedAt <- NA
    channelTitle <- NA # Not directly available in this enriched format
    duration <- NA # Not directly available
    description <- NA
    tags <- list()
    
    if (length(item$reddit_posts) > 0) {
      first_post <- item$reddit_posts[[1]]
      publishedAt <- as.POSIXct(first_post$created_utc, origin = "1970-01-01", tz = "UTC")
      description <- first_post$title # Using reddit post title as description proxy
    }
    
    list(
      topic = topic,
      platform_source = platform_source,
      category = category,
      time_fetched = time_fetched,
      engagement = list(
        views = as.numeric(views),
        likes = as.numeric(likes),
        comments = as.numeric(comments)
      ),
      metadata = list(
        publishedAt = publishedAt,
        channelTitle = channelTitle,
        duration = duration,
        description = description,
        tags = tags
      )
    )
  })
  
  # Create directory if it doesn't exist
  dir.create(dirname(output_file), showWarnings = FALSE, recursive = TRUE)
  
  # Write to JSON file
  write(toJSON(clean_data, pretty=TRUE, auto_unbox=TRUE), file=output_file)
  cat("✅ Processed YouTube data saved to:", output_file, "\n")
  
  return(clean_data)
}

# Run if this script is executed directly
if (interactive()) {
  input_file <- "youtube_trending_IN_20251106_003758_raw.json"
  output_file <- file.path("data-clean", "processed_youtube.json")
  data <- process_youtube_data(input_file, output_file)
}