run_pipeline <- function(json_dir = "data/", 
                         time_unit = "hour", 
                         prophet_periods = 48, 
                         clustering_len = 50,
                         k_clusters = 4) {
  
  # 1️⃣ Load & preprocess
  topic_series <- read_topics_from_dir(json_dir, time_unit = time_unit)
  
  # Exit gracefully if no topics with data were found
  if (length(topic_series) == 0) {
    message("⚠️ No topics with sufficient data found to build time series. Halting analysis.")
    return(list(
      topic_series = list(),
      features_list = list(),
      clustering = NULL,
      prophet = NULL,
      xgb = NULL,
      saturation = NULL,
      timestamp = Sys.time()
    ))
  }
  
  # 2️⃣ Feature extraction (keep metadata)
  features_list <- purrr::map(topic_series, function(ts) {
    f <- extract_features_for_topic(ts)
    f$metadata <- ts$metadata     # ⭐ needed for lifespan training
    return(f)
  })
  
  # 3️⃣ Clustering for similarity visualization
  # Check if there are enough features for clustering
  message(paste("DEBUG: length(features_list) =", length(features_list)))
  if (length(features_list) < 2) { # Need at least 2 topics to form clusters
    message("⚠️ Not enough topics with features for clustering — skipping.")
    clusters <- NULL
  } else {
    mat <- build_matrix_for_clustering(features_list, max_len = clustering_len)
    
    message(paste("DEBUG: nrow(mat) =", nrow(mat)))
    if (nrow(mat) < 2) { # This will be true if nrow(mat) is 1
      message("⚠️ Not enough valid data points in matrix for clustering after filtering — skipping.")
      clusters <- NULL
    } else {
      # Check for unique rows in mat
      unique_rows_count <- nrow(unique(mat))
      message(paste("DEBUG: unique_rows_count =", unique_rows_count))

      actual_k_clusters <- min(k_clusters, unique_rows_count)
      message(paste("DEBUG: actual_k_clusters (after unique check) =", actual_k_clusters))
      
      if (actual_k_clusters < 2) {
        message("⚠️ Not enough distinct data points for meaningful clustering (need at least 2) — skipping.")
        clusters <- NULL
      } else {
        clusters <- cluster_trends(mat, k = actual_k_clusters)
      }
    }
  }
  
  # 4️⃣ Prophet forecasting
  prophet_res <- batch_forecast_prophet(features_list, 
                                        periods = prophet_periods, 
                                        freq = time_unit)
  
  # 5️⃣ Lifespan model training (only if labels exist)
  train_tbl <- build_training_table(features_list)
  xgb_fit <- NULL
  if (nrow(train_tbl %>% dplyr::filter(!is.na(label_lifespan))) >= 10) {
    xgb_fit <- train_xgb_lifespan(train_tbl)
  } else {
    message("⚠️ Not enough labeled topics for XGBoost lifespan model — skipping.")
  }
  
  # 6️⃣ Saturation detection for each trend
  sat_results <- purrr::map(features_list, detect_saturation, 
                            roll_win = 5, slope_threshold = 0.1)
  
  # 7️⃣ Combine output
  list(
    topic_series   = topic_series,
    features_list  = features_list,
    clustering     = clusters,
    prophet        = prophet_res,
    xgb            = xgb_fit,
    saturation     = sat_results,
    timestamp      = Sys.time()    # ⭐ helps version tracking in dashboard
  )
}
