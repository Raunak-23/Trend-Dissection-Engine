library(renv)

# Initialize the project with renv for dependency management
init()

# Install required packages if not already installed
pkgs <- c(
  "httr",
  "jsonlite", 
  "lubridate",
  "syuzhet",
  "minpack.lm",
  "dplyr",
  "tuber",
  "RedditExtractoR",
  "zoo",
  "stringr",
  "purrr",
  "tidyr",
  "forecast",
  "prophet",
  "cluster",
  "factoextra",
  "testthat",
  "dotenv"
)

for(pkg in pkgs) {
  if(!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}