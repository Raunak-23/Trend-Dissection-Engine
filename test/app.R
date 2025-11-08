library(shiny)
library(shinydashboard)
library(plotly)
library(ggplot2)
library(DT)
library(ggraph)
library(igraph)

# Hardcoded top 20 trending topics from Google India Trends
google_trends <- c(
  "Indian Premier League", "T20 World Cup", "Bharatiya Janata Party", "Election Results 2024", "Olympics 2024",
  "Vinesh Phogat", "Nitish Kumar", "Chirag Paswan", "Hardik Pandya", "Pawan Kalyan",
  "Oppenheimer", "Barbie", "Guardians of the Galaxy", "Everything Everywhere All at Once", "The Menu",
  "Happy Valley", "The Last of Us", "The Rig", "Ginny and Georgia", "Top Boy"
)

# Placeholder data
platforms <- c("YouTube", "Reddit", "Meta", "News")
days <- seq(Sys.Date()-29, Sys.Date(), by="day")
trend_data <- data.frame(
  date = rep(days, each=20),
  keyword = rep(google_trends, times=length(days)),
  platform = sample(platforms, 20*length(days), replace = TRUE),
  freq = sample(1:100, 20*length(days), replace=TRUE)
)

# ------------------- UI -------------------
ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(title = "📊 Trend Dissection Engine"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("eye")),
      menuItem("Signal Analysis DSP", tabName = "dsp", icon = icon("wave-square")),
      menuItem("Context Similarity", tabName = "similarity", icon = icon("project-diagram")),
      menuItem("Trend Prediction", tabName = "prediction", icon = icon("line-chart"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side { background-color: #f8f9fa; }
        .box { border-radius: 15px; }
        .box-title { font-size: 18px; font-weight: bold; }
      "))
    ),
    tabItems(
      # ---------------- Overview ----------------
      tabItem(tabName = "overview",
              fluidRow(
                valueBoxOutput("total_posts"), 
                valueBoxOutput("top_keyword"), 
                valueBoxOutput("top_platform"),
                valueBoxOutput("trend_score")
              ),
              fluidRow(
                box(title="Platform Keyword Heatmap", width=12, height="600px", solidHeader=TRUE, status="primary",
                    plotlyOutput("heatmap", height="500px")
                )
              ),
              fluidRow(
                box(title="Platform Activity Timeline", width=12, height="600px", solidHeader=TRUE, status="info",
                    plotlyOutput("activity_timeline", height="500px")
                )
              ),
              fluidRow(
                box(title="Trend Growth/Decay Timelines", width=12, height="600px", solidHeader=TRUE, status="warning",
                    plotlyOutput("trend_timelines", height="500px")
                )
              )
      ),
      
      # ---------------- DSP ----------------
      tabItem(tabName = "dsp",
              sidebarLayout(
                sidebarPanel(
                  selectInput("dsp_keyword", "Select Trend", choices=google_trends),
                  sliderInput("dsp_days", "Time Range (days)", min=1, max=30, value=30),
                  sliderInput("dsp_filter", "Low-pass filter cutoff", min=0.1, max=1, value=0.5)
                ),
                mainPanel(
                  fluidRow(
                    box(title="Raw Time-Series Signal", width=12, plotOutput("raw_signal", height="350px"))
                  ),
                  fluidRow(
                    box(title="Spectral Plot (FFT)", width=6, plotOutput("fft_plot", height="300px")),
                    box(title="Filtered Signal Plot", width=6, plotOutput("filtered_signal", height="300px"))
                  )
                )
              )
      ),
      
      # ---------------- Context Similarity ----------------
      tabItem(tabName = "similarity",
              sidebarLayout(
                sidebarPanel(
                  selectInput("sim_keyword", "Select Trend", choices=google_trends),
                  selectInput("sim_platform1", "Platform A", choices=platforms),
                  selectInput("sim_platform2", "Platform B", choices=platforms, selected=platforms[2])
                ),
                mainPanel(
                  fluidRow(
                    box(title="Sentiment Analysis Gauge", width=6, plotlyOutput("sentiment_gauge")),
                    box(title="Co-occurring Topics Network", width=6, plotOutput("network_graph"))
                  ),
                  fluidRow(
                    box(title="Time-Series Similarity (DTW)", width=12, plotOutput("dtw_plot"))
                  )
                )
              )
      ),
      
      # ---------------- Prediction ----------------
      tabItem(tabName = "prediction",
              sidebarLayout(
                sidebarPanel(
                  selectInput("pred_keyword", "Select Trend", choices=google_trends),
                  numericInput("pred_horizon", "Forecast Horizon (days)", value=7, min=1, max=30)
                ),
                mainPanel(
                  fluidRow(
                    box(title="Forecast Plot", width=8, plotlyOutput("forecast_plot")),
                    valueBoxOutput("growth_velocity", width=4)
                  ),
                  fluidRow(
                    box(title="Prediction Summary Table", width=12, DTOutput("pred_table"))
                  )
                )
              )
      )
    )
  )
)

# ------------------- Server -------------------
server <- function(input, output, session) {
  
  # Summary Boxes
  output$total_posts <- renderValueBox({
    valueBox(format(sum(trend_data$freq), big.mark=","), "Total Posts (30 Days)", icon = icon("hashtag"), color="yellow")
  })
  output$top_keyword <- renderValueBox({
    topkw <- names(sort(table(trend_data$keyword), decreasing=TRUE)[1])
    valueBox(topkw, "Top Trending Keyword", icon = icon("fire"), color="red")
  })
  output$top_platform <- renderValueBox({
    topplat <- names(sort(table(trend_data$platform), decreasing=TRUE)[1])
    valueBox(topplat, "Highest Velocity Platform", icon = icon("rocket"), color="blue")
  })
  output$trend_score <- renderValueBox({
    valueBox(round(runif(1,70,100),2), "Current Trend Index Score", icon = icon("tachometer"), color="orange")
  })
  
  # Heatmap
  output$heatmap <- renderPlotly({
    df <- aggregate(freq ~ platform + keyword, trend_data, sum)
    p <- ggplot(df, aes(x=reorder(keyword, -freq), y=platform, fill=freq, 
                        text=paste("Keyword:", keyword, "<br>Freq:", freq))) +
      geom_tile(color="white", size=0.2) +
      scale_fill_viridis_c(option="C", direction=1) +
      theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(angle=45, hjust=1),
            axis.text=element_text(size=12),
            plot.title=element_text(size=16, face="bold")) +
      labs(x="Keywords", y="Platform", fill="Frequency", title="Keyword Frequency Heatmap Across Platforms")
    ggplotly(p, tooltip="text")
  })
  
  # Activity Timeline
  output$activity_timeline <- renderPlotly({
    df <- aggregate(freq ~ date + platform, trend_data, sum)
    p <- ggplot(df, aes(x=date, y=freq, color=platform)) + 
      geom_line(size=1.2) + geom_point(size=2) +
      theme_minimal(base_size = 14) +
      labs(x="Date", y="Posts", title="Platform Activity Over Time") +
      theme(legend.title=element_blank(),
            plot.title=element_text(size=16, face="bold"))
    ggplotly(p)
  })
  
  # Trend Timelines
  output$trend_timelines <- renderPlotly({
    topkws <- names(sort(table(trend_data$keyword), decreasing=TRUE)[1:5])
    df <- trend_data[trend_data$keyword %in% topkws,]
    df_sum <- aggregate(freq ~ date + keyword, df, sum)
    p <- ggplot(df_sum, aes(x=date, y=freq, color=keyword)) + 
      geom_line(size=1.2) + geom_point(size=2) +
      theme_minimal(base_size = 14) +
      labs(x="Date", y="Frequency", title="Top 5 Trends Growth & Decay Over Time") +
      theme(legend.title=element_blank(),
            plot.title=element_text(size=16, face="bold"))
    ggplotly(p)
  })
  
  # DSP Tab
  selected_trend <- reactive({
    trend_data[trend_data$keyword == input$dsp_keyword & trend_data$date >= (Sys.Date()-input$dsp_days+1),]
  })
  
  output$raw_signal <- renderPlot({
    df <- selected_trend()
    plot(df$date, df$freq, type="l", col="blue", lwd=2, main="Raw Time-Series Signal", xlab="Date", ylab="Frequency")
  })
  
  output$fft_plot <- renderPlot({
    df <- selected_trend()
    signal <- df$freq
    fft_vals <- Mod(fft(signal))
    plot(fft_vals, type="l", col="darkgreen", lwd=2, main="FFT Spectral Plot", xlab="Frequency Index", ylab="Magnitude")
  })
  
  output$filtered_signal <- renderPlot({
    df <- selected_trend()
    cutoff <- input$dsp_filter
    window <- max(2, round(length(df$freq)*cutoff))
    filtered <- stats::filter(df$freq, rep(1/window, window), sides=2)
    plot(df$date, filtered, type="l", col="orange", lwd=2, main="Filtered Signal", xlab="Date", ylab="Filtered Frequency")
  })
  
  # Context Similarity Tab
  output$sentiment_gauge <- renderPlotly({
    sentiment <- runif(1, -1, 1)
    plot_ly(
      type = "indicator",
      mode = "gauge+number",
      value = sentiment,
      title = list(text = "Sentiment Score"),
      gauge = list(axis=list(range = list(-1,1)),
                   steps = list(list(range = c(-1,0), color="#FF4C4C"),
                                list(range = c(0,1), color="#4CFF4C")))
    )
  })
  
  output$network_graph <- renderPlot({
    g <- make_ring(10)
    plot(g, main="Co-occurring Topics Network", vertex.color="skyblue", vertex.size=25, vertex.label.color="black")
  })
  
  output$dtw_plot <- renderPlot({
    x <- rnorm(30)
    y <- rnorm(30)
    plot(x, type="l", col="blue", lwd=2, main="Time-Series Similarity (DTW)", xlab="Time", ylab="Value")
    lines(y, col="red", lwd=2)
    legend("topright", legend=c(input$sim_platform1, input$sim_platform2), col=c("blue","red"), lty=1, lwd=2)
  })
  
  # Prediction Tab
  output$forecast_plot <- renderPlotly({
    future_days <- seq(Sys.Date(), by="day", length.out=input$pred_horizon)
    hist <- data.frame(date=days, freq=sample(1:100, length(days), replace=TRUE))
    pred <- data.frame(date=future_days, freq=runif(input$pred_horizon, 50,150))
    df <- rbind(hist, pred)
    p <- ggplot(df, aes(x=date, y=freq)) +
      geom_line(size=1.2) + geom_point(data=pred, aes(x=date, y=freq), color='red', size=2) +
      theme_minimal(base_size = 14) + 
      labs(x="Date", y="Frequency", title="Forecasted Trend Growth") +
      theme(plot.title=element_text(size=16, face="bold"))
    ggplotly(p)
  })
  
  output$growth_velocity <- renderValueBox({
    v <- runif(1, -1, 1)
    category <- ifelse(v>0.25,"Emerging",ifelse(v<-0.25,"Decaying","Stable"))
    valueBox(category, "Growth Velocity", icon = icon("chart-line"), color="purple")
  })
  
  output$pred_table <- renderDT({
    future_days <- seq(Sys.Date()+1, by="day", length.out=3)
    pred <- data.frame(Date=future_days, Frequency=round(runif(3, 70,140),0))
    datatable(pred, options=list(pageLength=5, autoWidth=TRUE))
  })
}

# Run App
shinyApp(ui, server)