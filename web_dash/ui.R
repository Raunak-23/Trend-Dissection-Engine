# ui.R

# Load the required libraries
library(shiny)
library(shinydashboard)
library(plotly)

# Define the user interface
dashboardPage(
  skin = "blue",  # Available: blue, black, purple, green, red, yellow
  dashboardHeader(
    title = span("⚡ Trend Dissection Engine", style = "font-weight:600; font-size:20px;")
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("📊 Trend Overview", tabName = "overview", icon = icon("globe")),
      menuItem("🔍 Keyword Analysis", tabName = "keywords", icon = icon("search")),
      menuItem("📡 Time-Series & DSP", tabName = "dsp", icon = icon("wave-square")),
      menuItem("📈 Prediction", tabName = "prediction", icon = icon("chart-line"))
    ),
    hr(),
    dateRangeInput("date_range", "📅 Select Date Range:",
                   start = Sys.Date() - 30, end = Sys.Date(),
                   separator = " to ",
                   format = "yyyy-mm-dd")
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        /* Custom styling */
        .content-wrapper {
          background-color: #f4f6f9;
        }
        .box {
          border-radius: 15px !important;
          box-shadow: 0 3px 8px rgba(0,0,0,0.15);
          border-top: 3px solid #3c8dbc;
        }
        .box-header {
          font-weight: bold;
          font-size: 16px;
        }
        h2 {
          font-weight: 700;
          margin-bottom: 15px;
        }
      "))
    ),
    
    tabItems(
      
      # -------------------------------
      tabItem(tabName = "overview",
              h2("🌐 Trend Overview & Sentiment"),
              fluidRow(
                box(
                  title = "🔥 Popularity by Platform", 
                  width = 6, solidHeader = TRUE, status = "primary",
                  plotlyOutput("platform_heatmap", height = "300px")
                ),
                box(
                  title = "💬 Overall Sentiment",
                  width = 6, solidHeader = TRUE, status = "success",
                  plotlyOutput("sentiment_gauge", height = "300px")
                )
              )
      ),
      
      # -------------------------------
      tabItem(tabName = "keywords",
              h2("🔑 Detailed Keyword Analysis"),
              fluidRow(
                box(
                  title = "📈 Trending Keywords Timeline", 
                  width = 7, solidHeader = TRUE, status = "info",
                  plotlyOutput("keyword_timeline", height = "350px")
                ),
                box(
                  title = "🌐 Co-occurring Topics",
                  width = 5, solidHeader = TRUE, status = "warning",
                  plotOutput("network_graph", height = "350px")
                )
              ),
              br(),
              textInput("keyword_search", "🔎 Search for a Specific Keyword:", placeholder = "Type a keyword...")
      ),
      
      # -------------------------------
      tabItem(tabName = "dsp",
              h2("🎶 Time-Series and DSP Insights"),
              fluidRow(
                box(
                  title = "📊 Time-Series Signal", 
                  width = 6, solidHeader = TRUE, status = "primary",
                  plotlyOutput("time_series_plot", height = "320px")
                ),
                box(
                  title = "🔬 Spectral Analysis (FFT)", 
                  width = 6, solidHeader = TRUE, status = "danger",
                  plotlyOutput("fft_plot", height = "320px")
                )
              ),
              br(),
              fluidRow(
                column(6, selectInput("dsp_platform", "Select Platform:", 
                                      choices = c("Twitter", "Instagram", "Reddit"))),
                column(6, selectInput("dsp_keyword", "Select Keyword:", choices = NULL))
              )
      ),
      
      # -------------------------------
      tabItem(tabName = "prediction",
              h2("📈 Trend Forecasting"),
              fluidRow(
                box(
                  title = "⏳ Short-Term Forecast", 
                  width = 12, solidHeader = TRUE, status = "info",
                  plotlyOutput("forecast_plot", height = "400px")
                )
              )
      )
    )
  )
)
