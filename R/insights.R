library(dplyr)
library(DT)
library(ggplot2)
library(plotly)
library(xgboost)

# ---- 1️⃣ Feature Importance for Lifespan Model ----
get_feature_importance_plot <- function(xgb_model) {
  if (is.null(xgb_model)) return(NULL)
  imp <- xgb.importance(model = xgb_model)
  p <- ggplot(imp, aes(x = reorder(Feature, Gain), y = Gain)) +
    geom_col(fill = "#6C63FF") +
    coord_flip() +
    theme_minimal(base_size = 14) +
    labs(title = "Feature Importance (Lifespan Model)",
         x = "Feature", y = "Relative Importance")
  ggplotly(p)
}

# ---- 2️⃣ Prophet Trend Components Plot (for a single topic) ----
get_prophet_component_plot <- function(prophet_result) {
  if (is.null(prophet_result) || is.null(prophet_result$model)) return(NULL)
  m <- prophet_result$model
  fc <- prophet_result$forecast
  p <- plot(m, fc) +
    ggtitle("Prophet Forecast with Changepoints") +
    theme_minimal(base_size = 14)
  ggplotly(p)
}

# ---- 3️⃣ Combined Summary Table for Dashboard ----
generate_summary_table <- function(features_list, prophet_res, xgb_fit = NULL, sat_results = NULL) {
  n <- length(features_list)
  df <- data.frame(
    Topic = character(n),
    Peak_Engagement = numeric(n),
    Time_to_Peak = numeric(n),
    Avg_Sentiment = numeric(n),
    Saturated = logical(n),
    Predicted_Lifespan = numeric(n)
  )
  
  for (i in seq_along(features_list)) {
    fx <- features_list[[i]]
    df$Topic[i] <- fx$summary$topic
    df$Peak_Engagement[i] <- fx$summary$peak_val
    df$Time_to_Peak[i] <- fx$summary$time_to_peak
    df$Avg_Sentiment[i] <- round(fx$summary$avg_sentiment, 3)
    
    # saturation
    if (!is.null(sat_results) && length(sat_results) >= i) {
      df$Saturated[i] <- tail(sat_results[[i]]$saturation, 1)
    } else {
      df$Saturated[i] <- NA
    }
    
    # predicted lifespan (XGBoost)
    if (!is.null(xgb_fit)) {
      test_vec <- as.matrix(df[i, c("Peak_Engagement", "Time_to_Peak", "Avg_Sentiment")])
      pred <- predict(xgb_fit$model, test_vec)
      df$Predicted_Lifespan[i] <- round(pred, 2)
    } else {
      df$Predicted_Lifespan[i] <- NA
    }
  }
  
  datatable(df, options = list(pageLength = 10, autoWidth = TRUE))
}

# ---- 4️⃣ Forecast Comparison Plot (Actual vs Predicted) ----
plot_forecast_comparison <- function(actual_df, prophet_forecast) {
  if (is.null(prophet_forecast)) return(NULL)
  
  pred_df <- prophet_forecast$forecast %>%
    rename(date = ds, pred = yhat) %>%
    select(date, pred)
  
  df <- merge(actual_df, pred_df, by = "date", all = TRUE)
  df_long <- df %>%
    tidyr::pivot_longer(cols = c("engagement_sum", "pred"),
                        names_to = "type", values_to = "value")
  
  p <- ggplot(df_long, aes(x = date, y = value, color = type)) +
    geom_line(size = 1.2) +
    theme_minimal(base_size = 14) +
    labs(x = "Time", y = "Engagement",
         title = "Actual vs Forecasted Engagement",
         color = "Series")
  ggplotly(p)
}
