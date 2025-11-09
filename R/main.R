########################################################################
# main.R — TrendR pipeline runner
# Runs all R scripts in sequence and performs simple sanity checks.
# Usage: run from project root:
#   Rscript main.R
#
# This script ensures dplyr (and other core packages) are loaded first so
# functions like select() behave as expected in downstream scripts.
########################################################################

# ---- Environment / options ----
options(stringsAsFactors = FALSE, scipen = 999)
Sys.setenv(TZ = "Asia/Kolkata")

# ---- Useful packages to ensure consistent namespace behaviour ----
required <- c(
  "jsonlite", "dplyr", "tidyr", "lubridate", "stringr", "ggplot2",
  "zoo", "Kendall", "Metrics", "cluster"
)

install_if_missing <- function(pkgs) {
  for (p in pkgs) {
    if (!suppressWarnings(requireNamespace(p, quietly = TRUE))) {
      message("Installing missing package: ", p)
      install.packages(p, repos = "https://cloud.r-project.org")
    }
    # load into session
    suppressPackageStartupMessages(library(p, character.only = TRUE))
  }
}
install_if_missing(required)

# Force dplyr to be attached so select() and other verbs behave properly
suppressPackageStartupMessages(library(dplyr))

# ---- helper to run a step and log errors ----
run_step <- function(script_path, check_fn = NULL) {
  start <- Sys.time()
  message("\n-------------------------------")
  message("▶ Running: ", script_path)
  message("-------------------------------")
  tryCatch({
    source(script_path, local = new.env())  # run in its own environment
    dur <- difftime(Sys.time(), start, units = "secs")
    message("✔ Completed: ", script_path, " (", round(as.numeric(dur), 1), "s )")
    # optional post-check
    if (!is.null(check_fn)) {
      tryCatch({
        ok <- check_fn()
        if (isTRUE(ok)) {
          message("✔ Post-check OK for: ", script_path)
        } else {
          warning("⚠️ Post-check for ", script_path, " returned FALSE/NULL")
        }
      }, error = function(e) {
        warning("⚠️ Post-check for ", script_path, " failed: ", e$message)
      })
    }
    invisible(TRUE)
  }, error = function(e) {
    message("\n❌ ERROR while running: ", script_path)
    message("Error message: ", e$message)
    message("Traceback:")
    traceback()
    stop("Pipeline stopped due to error in ", script_path, ". Fix the error and re-run main.R or re-run the failing script.")
  })
}

# ---- sanity check functions (basic expectations) ----
check_data_raw <- function() {
  dir.exists("data_raw") && length(list.files("data_raw")) > 0
}
check_data_clean <- function() {
  dir.exists("data_clean") && length(list.files("data_clean")) > 0
}
check_features <- function() {
  dir.exists("data_features") && length(list.files("data_features")) > 0
}
check_clusters <- function() {
  dir.exists("data_clusters") && length(list.files("data_clusters")) > 0
}
check_forecasts <- function() {
  dir.exists("data_forecasts") && length(list.files("data_forecasts")) > 0
}
check_lifespan <- function() {
  dir.exists("data_lifespan") && length(list.files("data_lifespan")) > 0
}
check_saturation <- function() {
  dir.exists("data_saturation") && length(list.files("data_saturation")) > 0
}
check_insights <- function() {
  dir.exists("data_insights") && length(list.files("data_insights")) > 0
}
check_evaluation <- function() {
  dir.exists("data_evaluation") && length(list.files("data_evaluation")) > 0
}

# ---- Execution order (as you specified) ----
steps <- list(
  list(script = "R/acquire_data.R",       check = check_data_raw),
  list(script = "R/preprocess.R",         check = check_data_clean),
  list(script = "R/feature_extraction.R", check = check_features),
  list(script = "R/trend_similarity.R",   check = check_clusters),
  list(script = "R/short_term_forecast.R",check = check_forecasts),
  list(script = "R/lifespan_est.R",       check = check_lifespan),
  list(script = "R/saturation_detection.R",check = check_saturation),
  list(script = "R/insights.R",           check = check_insights),
  list(script = "R/model_evaluation.R",   check = check_evaluation)
)

# ---- auto-source utils and toolkit first (they are used by other scripts) ----
utils_files <- c("R/utils_load_data.R", "R/evaluation_toolkit.R")
for (f in utils_files) {
  if (file.exists(f)) {
    message("Loading helper file: ", f)
    tryCatch({
      source(f, local = new.env())
    }, error = function(e) {
      stop("Failed to source helper file ", f, ": ", e$message)
    })
  } else {
    message("Note: helper file not found (safe to ignore if intentionally absent): ", f)
  }
}

# ---- Run each step ----
for (s in steps) {
  script_path <- s$script
  check_fn <- s$check
  if (!file.exists(script_path)) {
    stop("Required script not found: ", script_path, "\nMake sure you are running main.R from the project root where R/ exists.")
  }
  run_step(script_path, check_fn)
}

message("\n🎉 All pipeline steps completed successfully (unless a step stopped earlier).")
message("Now you can run the Shiny app (app.R) from the project root.")
message("Example: in RStudio -> Run App OR run: Rscript -e \"shiny::runApp('app.R', launch.browser=TRUE)\"")
