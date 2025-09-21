pkgs <- c(
  "tidyverse", "lubridate", "jsonlite", "httr", "rvest", "xml2",
  "tidytext", "tm", "text2vec", "stopwords", "SnowballC",
  "signal", "pracma", "forecast", "prophet", "xts", "zoo",
  "dtwclust", "proxy", "igraph", "ggraph",
  "syuzhet", "sentimentr",
  "ggplot2", "plotly", "shiny", "shinydashboard", "viridis",
  "taskscheduleR", "cronR",
  "mongolite",
  "usethis", "gitcreds"
)

install.packages(setdiff(pkgs, rownames(installed.packages())))
