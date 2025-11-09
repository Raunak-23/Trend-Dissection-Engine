# ============================================================
# Trend Dissection Engine — FINAL DASHBOARD (Integrated & Enhanced)
# ============================================================

# ---- Libraries ----
suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(ggplot2)
  library(plotly)
  library(DT)
  library(dplyr)
  library(tidyr)
  library(jsonlite)
  library(shinycssloaders)
  library(wordcloud)
  library(RColorBrewer)
  library(stringr)
})

# ------------------------------------------------------------
# Helper: load latest file by pattern
# ------------------------------------------------------------
load_latest <- function(pattern, folder, read_fun) {
  files <- list.files(folder, pattern = pattern, full.names = TRUE)
  if (length(files) == 0) return(NULL)
  latest <- files[which.max(file.mtime(files))]
  message("📂 Loaded file: ", latest)
  read_fun(latest)
}

# ------------------------------------------------------------
# UI
# ------------------------------------------------------------
ui <- dashboardPage(
  dashboardHeader(title = span("Trend Dissection Engine", style = "font-weight:bold;")),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("chart-line")),
      menuItem("Trend Details", tabName = "details", icon = icon("info-circle")),
      menuItem("Clustering", tabName = "clusters", icon = icon("project-diagram")),
      menuItem("Forecasting & Lifespan", tabName = "forecast", icon = icon("chart-area")),
      menuItem("Insights", tabName = "insights", icon = icon("lightbulb")),
      menuItem("Evaluation", tabName = "evaluation", icon = icon("table"))
    )
  ),
  
  dashboardBody(
    tags$style(HTML("
      body, .content-wrapper {background-color: #f8fafc;}
      .box {border-radius: 10px; box-shadow: 0 2px 8px rgba(0,0,0,0.05);}
      h3.box-title {font-weight: 600;}
      .value-box {border-radius: 10px;}
    ")),
    
    tabItems(
      # ------------------------------------------------ Overview
      tabItem(tabName = "overview",
              fluidRow(
                box(title = "Top 10 Trending Topics", width = 12,
                    withSpinner(plotlyOutput("top_trends_plot", height = "500px")))
              ),
              fluidRow(
                box(title = "Platform Breakdown", width = 6,
                    withSpinner(plotlyOutput("platform_pie", height = "400px"))),
                box(title = "Engagement vs Sentiment", width = 6,
                    withSpinner(plotlyOutput("eng_sent_plot", height = "400px")))
              )
      ),
      
      # ------------------------------------------------ Trend Details
      tabItem(tabName = "details",
              fluidRow(
                box(width = 12, solidHeader = TRUE, title = "Select Trend",
                    selectInput("trend_select", NULL, choices = NULL, width = "100%"))
              ),
              fluidRow(
                box(title = "Engagement Timeline", width = 6,
                    withSpinner(plotlyOutput("eng_timeline", height = "350px"))),
                box(title = "Sentiment Distribution", width = 6,
                    withSpinner(plotlyOutput("sent_dist", height = "350px")))
              ),
              fluidRow(
                box(title = "Forecast (Next 7 Days)", width = 6,
                    withSpinner(plotlyOutput("forecast_plot", height = "350px"))),
                box(title = "Lifespan Gauge", width = 3,
                    withSpinner(plotlyOutput("lifespan_gauge", height = "350px"))),
                box(title = "Saturation Status", width = 3,
                    uiOutput("saturation_badge"))
              ),
              fluidRow(
                box(title = "Trend Summary", width = 12,
                    verbatimTextOutput("trend_info"))
              )
      ),
      
      # ------------------------------------------------ Clustering
      tabItem(tabName = "clusters",
              fluidRow(
                box(title = "Cluster Visualization", width = 6,
                    withSpinner(plotlyOutput("cluster_plot", height = "400px"))),
                box(title = "Cluster Summary", width = 6,
                    withSpinner(DTOutput("cluster_table")))
              )
      ),
      
      # ------------------------------------------------ Forecasting & Lifespan
      tabItem(tabName = "forecast",
              fluidRow(
                valueBoxOutput("mae_box"),
                valueBoxOutput("rmse_box"),
                valueBoxOutput("r2_box")
              ),
              fluidRow(
                box(title = "Lifespan vs Sentiment", width = 8,
                    withSpinner(plotlyOutput("life_sent_plot", height = "400px"))),
                box(title = "Trend Stage Distribution", width = 4,
                    withSpinner(plotlyOutput("trend_stage", height = "400px")))
              )
      ),
      
      # ------------------------------------------------ Insights
      tabItem(tabName = "insights",
              fluidRow(
                box(title = "Keyword Frequency", width = 6,
                    withSpinner(plotlyOutput("keyword_bar", height = "400px"))),
                box(title = "Sentiment vs Engagement Correlation", width = 6,
                    withSpinner(plotlyOutput("sent_corr", height = "400px")))
              )
      ),
      
      # ------------------------------------------------ Evaluation
      tabItem(tabName = "evaluation",
              fluidRow(
                box(title = "Model Evaluation Summary", width = 6,
                    withSpinner(DTOutput("eval_summary"))),
                box(title = "Data Quality", width = 6,
                    withSpinner(DTOutput("data_quality")))
              ),
              fluidRow(
                box(title = "Forecast vs Actual (Prophet)", width = 12,
                    withSpinner(plotlyOutput("forecast_eval_plot", height = "400px")))
              )
      )
    )
  )
)

# ------------------------------------------------------------
# SERVER
# ------------------------------------------------------------
server <- function(input, output, session) {
  
  # ---------- Load Data ----------
  trends <- reactive({
    load_latest("^trends_clean_", "data_clean", jsonlite::fromJSON)
  })
  clusters <- reactive({
    load_latest("^clustered_trends_", "data_clusters", jsonlite::fromJSON)
  })
  insights <- reactive({
    load_latest("^keyword_insights_", "data_insights", jsonlite::fromJSON)
  })
  evaluation <- reactive({
    load_latest("^model_evaluation_", "data_evaluation", jsonlite::fromJSON)
  })
  
  # ---------- Overview ----------
  output$top_trends_plot <- renderPlotly({
    df <- trends()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    df <- df %>% arrange(desc(trend_score)) %>% head(10)
    ggplotly(
      ggplot(df, aes(x = reorder(topic, trend_score), y = trend_score, fill = platform_source)) +
        geom_col() + coord_flip() +
        labs(x = "", y = "Trend Score", title = "Top 10 Trending Topics") +
        theme_minimal(base_size = 15)
    )
  })
  
  output$platform_pie <- renderPlotly({
    df <- trends()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    df %>% count(platform_source) %>%
      plot_ly(labels = ~platform_source, values = ~n, type = "pie")
  })
  
  output$eng_sent_plot <- renderPlotly({
    df <- trends()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    plot_ly(df, x = ~insta_engagement, y = ~avg_sentiment, color = ~platform_source,
            text = ~topic, mode = "markers")
  })
  
  # ---------- Trend Details ----------
  observe({
    df <- trends()
    if (!is.null(df)) updateSelectInput(session, "trend_select", choices = df$topic)
  })
  
  output$trend_info <- renderPrint({
    df <- trends()
    sel <- input$trend_select
    if (is.null(sel) || !(sel %in% df$topic)) return("Select a trend.")
    t <- df %>% filter(topic == sel)
    cat("📊 Trend:", sel, "\nPlatform:", t$platform_source, "\nScore:", round(t$trend_score, 2),
        "\nSentiment:", round(t$avg_sentiment, 3),
        "\nVelocity:", round(t$avg_velocity, 2),
        "\nEngagement:", round(t$insta_engagement, 3),
        "\nLifespan (hrs):", round(t$trend_lifespan_hours, 2),
        "\nIntensity:", round(t$trend_intensity, 2),
        "\nFetched:", t$time_fetched, "\n")
  })
  
  output$lifespan_gauge <- renderPlotly({
    df <- trends(); sel <- input$trend_select
    if (is.null(sel)) return(NULL)
    t <- df %>% filter(topic == sel)
    val <- round(t$trend_lifespan_hours / 24, 1)
    plot_ly(type = "indicator", mode = "gauge+number", value = val,
            title = list(text = "Lifespan (days)"),
            gauge = list(axis = list(range = list(NULL, max(15, val * 1.5))),
                         bar = list(color = "#3E8E7E")))
  })
  
  output$saturation_badge <- renderUI({
    df <- trends(); sel <- input$trend_select
    if (is.null(sel)) return(NULL)
    t <- df %>% filter(topic == sel)
    if (t$trend_intensity > 0.7) {
      span("🔥 Viral", style = "color:white; background:red; padding:6px; border-radius:5px; font-weight:bold;")
    } else if (t$trend_intensity > 0.3) {
      span("📈 Growing", style = "color:white; background:orange; padding:6px; border-radius:5px; font-weight:bold;")
    } else {
      span("🌱 Stable", style = "color:white; background:green; padding:6px; border-radius:5px; font-weight:bold;")
    }
  })
  
  # ---------- Clustering ----------
  output$cluster_plot <- renderPlotly({
    df <- clusters()
    if (is.null(df)) return(NULL)
    plot_ly(df, x = ~avg_sentiment, y = ~avg_velocity, color = ~as.factor(cluster),
            text = ~topic, mode = "markers") %>%
      layout(title = "Cluster Visualization (Sentiment vs Velocity)")
  })
  
  output$cluster_table <- renderDT({
    df <- clusters()
    if (is.null(df)) return(NULL)
    df %>% group_by(cluster) %>% summarise(Count = n(), Avg_Score = mean(trend_score, na.rm = TRUE)) %>% datatable()
  })
  
  # ---------- Forecasting & Lifespan ----------
  output$life_sent_plot <- renderPlotly({
    df <- trends()
    if (is.null(df)) return(NULL)
    plot_ly(df, x = ~avg_sentiment, y = ~trend_lifespan_hours, color = ~platform_source,
            text = ~topic, mode = "markers") %>%
      layout(title = "💫 Lifespan vs Sentiment")
  })
  
  output$trend_stage <- renderPlotly({
    df <- trends()
    if (is.null(df)) return(NULL)
    df <- df %>%
      mutate(stage = case_when(
        trend_lifespan_hours < 72 ~ "Emerging",
        trend_lifespan_hours < 168 ~ "Growing",
        TRUE ~ "Saturated"
      ))
    plot_ly(df %>% count(stage), x = ~stage, y = ~n, type = "bar",
            marker = list(color = c("#5DADE2", "#F5B041", "#95A5A6"))) %>%
      layout(title = "Trend Stage Distribution")
  })
  
  # ---------- Insights ----------
  output$keyword_bar <- renderPlotly({
    df <- insights()
    if (is.null(df)) return(NULL)
    top <- head(df %>% arrange(desc(combined_score)), 15)
    ggplotly(
      ggplot(top, aes(x = reorder(word, combined_score), y = combined_score)) +
        geom_col(fill = "#7B68EE") + coord_flip() +
        labs(title = "Top Keywords by Combined Score", x = "", y = "Score") +
        theme_minimal()
    )
  })
  
  output$sent_corr <- renderPlotly({
    df <- trends()
    if (is.null(df)) return(NULL)
    plot_ly(df, x = ~avg_sentiment, y = ~insta_engagement,
            mode = "markers", color = ~platform_source)
  })
  
  # ---------- Evaluation ----------
  output$eval_summary <- renderDT({
    df <- evaluation()
    if (is.null(df)) return(NULL)
    datatable(df)
  })
  
  output$data_quality <- renderDT({
    dq_path <- list.files("data_evaluation", pattern = "^evaluation_summary_.*\\.RDS$", full.names = TRUE)
    if (length(dq_path) == 0) return(NULL)
    dq <- readRDS(dq_path[length(dq_path)])
    datatable(as.data.frame(dq$data_quality))
  })
  
  output$forecast_eval_plot <- renderPlotly({
    df <- evaluation()
    if (is.null(df)) return(NULL)
    plot_ly(df, x = ~MAE, y = ~RMSE, text = ~model, mode = "markers+text",
            textposition = "top center", marker = list(size = 14, color = "#5DADE2")) %>%
      layout(title = "Forecast & Regression Performance")
  })
}

# ------------------------------------------------------------
# Run App
# ------------------------------------------------------------
shinyApp(ui, server)
