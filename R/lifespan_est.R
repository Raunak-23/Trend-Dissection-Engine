library(xgboost)
library(caret)

# Build a training table across topics: features from the early window + label from metadata
build_training_table <- function(features_list, early_window = 5) {
  rows <- list()
  for(fx in features_list) {
    s <- fx$summary
    ts <- fx$timeseries
    # label from metadata if available
    metadata <- fx$timeseries  # we need to retrieve original metadata; but we stored it earlier maybe in topic_series$metadata
    # In our pipeline, original metadata is stored in topic_series$metadata
    # So pass in original topic list keeping metadata available. Here assume fx includes metadata:
    label <- NA
    if(!is.null(fx$metadata) && !is.null(fx$metadata$topic_meta$trend_lifespan_hours)) {
      label <- as.numeric(fx$metadata$topic_meta$trend_lifespan_hours)
    } else if(!is.null(fx$summary$topic)) {
      # try to find metadata elsewhere - skip if not found
      label <- NA
    }
    # compute early window features
    early_vals <- head(ts$engagement_sum, early_window)
    early_mean <- mean(early_vals, na.rm = TRUE)
    early_sd <- sd(early_vals, na.rm = TRUE)
    early_vel <- if(length(early_vals) >= 2) mean(diff(early_vals), na.rm = TRUE) else NA
    
    rows <- append(rows, list(tibble(
      topic = fx$summary$topic,
      label_lifespan = label,
      early_mean = early_mean,
      early_sd = early_sd,
      early_vel = early_vel,
      peak_val = fx$summary$peak_val,
      total_volume = fx$summary$total_volume,
      avg_sentiment = fx$summary$avg_sentiment,
      n_periods = fx$summary$n_periods
    )))
  }
  train_tbl <- bind_rows(rows)
  return(train_tbl)
}

# Train XGBoost model (regression)
train_xgb_lifespan <- function(train_tbl, test_fraction = 0.2) {
  train_tbl <- train_tbl %>% filter(!is.na(label_lifespan)) # keep only labeled
  if(nrow(train_tbl) < 10) {
    stop("Not enough labeled topics to train lifespan model. Need more labeled topics.")
  }
  # prepare dataset
  set.seed(42)
  train_idx <- createDataPartition(train_tbl$label_lifespan, p = 1 - test_fraction, list = FALSE)
  train <- train_tbl[train_idx, ]
  test <- train_tbl[-train_idx, ]
  
  dtrain <- xgb.DMatrix(data = as.matrix(train %>% select(-topic, -label_lifespan)), label = train$label_lifespan)
  dtest <- xgb.DMatrix(data = as.matrix(test %>% select(-topic, -label_lifespan)), label = test$label_lifespan)
  
  params <- list(objective = "reg:squarederror", eval_metric = "rmse", eta = 0.1, max_depth = 4)
  watchlist <- list(train = dtrain, eval = dtest)
  
  bst <- xgb.train(params = params, data = dtrain, nrounds = 200, watchlist = watchlist, early_stopping_rounds = 20, verbose = 1)
  
  # get predictions and evaluate
  preds <- predict(bst, newdata = dtest)
  rmse <- sqrt(mean((preds - test$label_lifespan)^2, na.rm = TRUE))
  list(model = bst, rmse = rmse, test = test, preds = preds)
}

# Example usage:
# - To preserve metadata, ensure extract_features_for_topic returns metadata. If not, adapt earlier pipeline to keep metadata.
# feats_list_with_meta <- map(topic_series, function(ts) { f <- extract_features_for_topic(ts); f$metadata <- ts$metadata; return(f) })
# train_tbl <- build_training_table(feats_list_with_meta)
# fit <- train_xgb_lifespan(train_tbl)
# print(fit$rmse)
