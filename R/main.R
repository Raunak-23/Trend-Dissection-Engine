# TrendR unified main pipeline orchestrator
suppressPackageStartupMessages({
  library(jsonlite)
  # Load environment variables from .env file if it exists
  if (requireNamespace("dotenv", quietly = TRUE)) {
    if (file.exists(".env")) {
      dotenv::load_dot_env()
    }
  }
})

# Helper: simple logger
log_msg <- function(level, msg) {
  ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  message(sprintf("[%s] [%s] %s", ts, level, msg))
}

# Read config
read_config <- function(path = "config/config.yml") {
  if (!requireNamespace("yaml", quietly = TRUE)) {
    install.packages("yaml", repos = "https://cloud.r-project.org")
  }
  yaml::read_yaml(path)
}

# Ensure directories
ensure_dirs <- function(cfg) {
  dirs <- c(cfg$paths$raw_dir, cfg$paths$clean_dir, cfg$paths$logs_dir, "visualizations")
  for (d in dirs) {
    if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }
}

# Copy latest pointers
update_latest <- function(src, dest) {
  if (isTRUE(nzchar(src)) && file.exists(src)) {
    file.copy(src, dest, overwrite = TRUE)
  }
}

# Main
main <- function() {
  cfg <- read_config()
  ensure_dirs(cfg)

  # Source modular R scripts if present
  if (file.exists("R/process_youtube.R")) source("R/process_youtube.R")
  if (file.exists("R/acquire_data.R")) source("R/acquire_data.R")
  if (file.exists("R/preprocess.R")) source("R/preprocess.R")
  if (file.exists("R/feature_extraction.R")) source("R/feature_extraction.R")
  if (file.exists("R/trend_similarity.R")) source("R/trend_similarity.R")
  if (file.exists("R/short_term_forecast.R")) source("R/short_term_forecast.R")
  if (file.exists("R/lifespan_est.R")) source("R/lifespan_est.R")
  if (file.exists("R/saturation_detection.R")) source("R/saturation_detection.R")
  if (file.exists("R/metrics_eval.R")) source("R/metrics_eval.R")
  if (file.exists("R/insights.R")) source("R/insights.R")
  if (file.exists("R/pipeline.R")) source("R/pipeline.R")

  # 1) Acquire data using R script
  log_msg("INFO", "Acquiring data using R script...")
  run_and_save(region = cfg$params$country_code, output_dir = cfg$paths$raw_dir)

  # Find the latest raw data file created by the R script
  raw_files <- file.info(list.files(cfg$paths$raw_dir, pattern = "trend_data_.*\\.json$", full.names = TRUE))
  if (nrow(raw_files) == 0) {
    log_msg("ERROR", "R data fetching script did not produce an output file. Halting execution.")
    quit(status = 1)
  }
  latest_raw <- rownames(raw_files)[which.max(raw_files$mtime)]
  log_msg("INFO", paste("Using latest raw data file:", latest_raw))

  if (is.null(latest_raw) || !file.exists(latest_raw)) {
    log_msg("ERROR", "No raw data found to process.")
    quit(status = 1)
  }

  # 2) Processing step is skipped as it is buggy and removes necessary data
  # if (!exists("process_youtube_data")) {
  #   log_msg("WARN", "process_youtube_data() not found; skipping processing step.")
  # } else {
  #   out_file <- cfg$paths$latest_processed
  #   log_msg("INFO", paste("Processing raw ->", out_file))
  #   tryCatch({
  #     process_youtube_data(input_file = latest_raw, output_file = out_file)
  #   }, error = function(e) {
  #     log_msg("ERROR", paste("Processing failed:", e$message))
  #     quit(status = 1)
  #   })
  # }



  # 3) Run R analysis pipeline on raw data
  log_msg("INFO", "Running R analysis pipeline on raw data...")
  analysis_output <- run_pipeline(json_dir = cfg$paths$raw_dir, time_unit = "day", prophet_periods = 30)
  saveRDS(analysis_output, file.path(cfg$paths$clean_dir, "analysis_output.rds"))
  log_msg("INFO", paste("R analysis output saved to:", file.path(cfg$paths$clean_dir, "analysis_output.rds")))
}

suppressWarnings(main())