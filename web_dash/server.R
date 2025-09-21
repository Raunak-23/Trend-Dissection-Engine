# server.R

# Load the necessary libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(stats)
library(forecast)

# Define the server logic
server <- function(input, output, session) {
  
  # Step 1: (Disabled MongoDB connection)
  # mongo_connection <- mongo(
  #   collection = "trends",
  #   db = "trend_dissection",
  #   url = "mongodb://localhost:27017/admin"
  # )
  
  # -------------------------
  # SAMPLE DATA (for demo use only)
  # -------------------------
  sample_data <- data.frame(
    timestamp = rep(seq.Date(Sys.Date()-9, Sys.Date(), by="day"), each=3),
    platform = rep(c("Twitter", "Instagram", "Reddit"), times=10),
    keyword = rep(c("AI", "ChatGPT", "Shiny"), times=10),
    count = sample(50:150, 30, replace = TRUE)
  )
  
  # Step 2: Create a reactive data source
  trend_data <- reactive({
    req(input$date_range)
    
    # Filter sample_data based on date range
    sample_data %>%
      filter(timestamp >= input$date_range[1],
             timestamp <= input$date_range[2]) %>%
      group_by(timestamp, platform, keyword) %>%
      summarise(count = sum(count), .groups = "drop")
  })
  
  # Step 3: Reactive keyword dropdown (uses sample data instead of mongo)
  observeEvent(input$dsp_platform, {
    platform_keywords <- sample_data %>%
      filter(platform == input$dsp_platform) %>%
      pull(keyword) %>%
      unique()
    
    updateSelectInput(
      session,
      "dsp_keyword",
      choices = platform_keywords
    )
  })
  
  # Output: Trending Keywords Timeline Plot
  output$keyword_timeline <- renderPlotly({
    req(trend_data())
    p <- trend_data() %>%
      ggplot(aes(x = timestamp, y = count, color = platform)) +
      geom_line() +
      labs(title = "Keyword Popularity Over Time", y = "Count", x = "Date") +
      theme_minimal()
    ggplotly(p)
  })
  
  # Output: Spectral Analysis (FFT) Plot
  output$fft_plot <- renderPlotly({
    req(input$dsp_keyword)
    
    keyword_ts_data <- trend_data() %>%
      filter(keyword == input$dsp_keyword) %>%
      arrange(timestamp)
    
    if (nrow(keyword_ts_data) == 0) return(NULL)
    
    keyword_ts <- ts(keyword_ts_data$count)
    fft_result <- fft(keyword_ts)
    
    n <- length(fft_result)
    frequency <- (0:(n-1)) / n
    amplitude <- abs(fft_result)
    
    plot_data <- data.frame(Frequency = frequency[1:(n/2)],
                            Amplitude = amplitude[1:(n/2)])
    
    p <- plot_ly(data = plot_data, x = ~Frequency, y = ~Amplitude,
                 type = 'scatter', mode = 'lines') %>%
      layout(title = "Spectral Analysis (FFT) for Trending Keyword",
             xaxis = list(title = "Frequency"),
             yaxis = list(title = "Amplitude"))
    p
  })
  
  # Placeholders for other outputs
  output$platform_heatmap <- renderPlotly({
    # Example heatmap with dummy values
    heatmap_data <- sample_data %>%
      group_by(platform, keyword) %>%
      summarise(total = sum(count), .groups = "drop")
    
    p <- ggplot(heatmap_data, aes(x = platform, y = keyword, fill = total)) +
      geom_tile() +
      labs(title = "Platform vs Keyword Heatmap") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$sentiment_gauge <- renderPlotly({
    # Example gauge (random sentiment score)
    score <- runif(1, 0, 1)
    plot_ly(
      type = "indicator",
      mode = "gauge+number",
      value = score * 100,
      title = list(text = "Sentiment Score"),
      gauge = list(axis = list(range = list(0, 100)))
    )
  })
  
  output$network_graph <- renderPlot({
    # Placeholder static plot
    plot(igraph::make_ring(5), main = "Dummy Co-occurrence Network")
  })
  
  output$forecast_plot <- renderPlotly({
    ts_data <- ts(sample_data$count, frequency = 7)
    fit <- forecast(auto.arima(ts_data), h = 7)
    p <- autoplot(fit) + labs(title = "Keyword Count Forecast")
    ggplotly(p)
  })
}
