# saturation_detection.R
# Robust saturation detection using multi-day trend snapshots (and reddit posts when available)
# Saves: data_saturation/saturation_<date>.json (.csv) + per-topic diag JSONs
suppressPackageStartupMessages({
  library(jsonlite)
  library(dplyr)
  library(lubridate)
  library(tidyr)
  library(zoo)
  library(Kendall)
  library(scales)
})

message("🛰️ Saturation detection — starting...")

# load helper that defines get_all_trend_data()
if (!file.exists("R/utils_load_data.R")) stop("Missing R/utils_load_data.R helper.")
source("R/utils_load_data.R")

# combine all trend-level daily files into a single dataframe
data_all <- get_all_trend_data("data_clean")

# ensure columns exist and have correct types
data_all <- data_all %>%
  mutate(
    trend_date = as.Date(trend_date),
    post_count = as.integer(ifelse(is.null(post_count) | is.na(post_count), NA, post_count)),
    avg_score = as.numeric(ifelse(is.null(avg_score) | is.na(avg_score), NA, avg_score)),
    avg_comments = as.numeric(ifelse(is.null(avg_comments) | is.na(avg_comments), NA, avg_comments)),
    trend_intensity = as.numeric(ifelse(is.null(trend_intensity) | is.na(trend_intensity), 0, trend_intensity))
  )

# replace NA post_count with 0 but keep a flag for proxy use
data_all$post_count_proxy_used <- FALSE
data_all <- data_all %>%
  group_by(topic) %>%
  arrange(topic, trend_date) %>%
  mutate(
    post_count = ifelse(is.na(post_count), 0L, post_count),
    post_count = ifelse(post_count == 0 & (!is.na(avg_score) & avg_score > 0), 1L, post_count),
    post_count_proxy_used = ifelse(post_count == 1 & (!is.na(avg_score) & avg_score > 0), TRUE, post_count_proxy_used)
  ) %>%
  ungroup()

# create output dir
dir.create("data_saturation", showWarnings = FALSE)

today <- format(Sys.Date(), "%Y-%m-%d")
saturation_rows <- list()
lifespan_candidates_all <- c()  # store all raw lifespan estimates for later global scaling

# helper: logistic fit in hours (times in hours or days; we will use days * 24 later)
logistic_fit_hours_safe <- function(times, counts) {
  if (length(times) < 4) return(NA_real_)
  tryCatch({
    times_num <- as.numeric(times)
    counts_num <- as.numeric(counts)
    K0 <- max(counts_num, na.rm = TRUE) * 1.5
    r0 <- 0.5
    t00 <- mean(times_num, na.rm = TRUE)
    fm <- nls(counts_num ~ K / (1 + exp(-r * (times_num - t0))),
              start = list(K = K0, r = r0, t0 = t00),
              control = nls.control(maxiter = 500), algorithm = "port")
    co <- coef(fm)
    K <- as.numeric(co["K"]); r <- as.numeric(co["r"]); t0 <- as.numeric(co["t0"])
    ts90 <- t0 - (1 / r) * log((1 / 0.9) - 1)
    lifespan_days <- max(ts90 - min(times_num, na.rm = TRUE), 0)
    lifespan_hours <- lifespan_days * 24
    return(as.numeric(lifespan_hours))
  }, error = function(e) {
    return(NA_real_)
  })
}

# iterate topics
topics <- unique(data_all$topic)

for (topic in topics) {
  tryCatch({
    df_t <- data_all %>% filter(topic == !!topic) %>% arrange(trend_date)
    if (nrow(df_t) == 0) {
      message("⚠️ No records for topic: ", topic)
      next
    }

    # Use post_count as per-day counts (we filled proxies earlier)
    day_counts <- df_t$post_count
    if (all(day_counts == 0, na.rm = TRUE)) {
      proxy_counts <- round(scales::rescale(df_t$trend_score + 1, to = c(0, 3)))
      proxy_counts[is.na(proxy_counts)] <- 0
      day_counts <- as.integer(proxy_counts)
    }

    n_days <- nrow(df_t)
    times_days <- seq(0, n_days - 1)
    cum_counts <- cumsum(replace_na(as.numeric(day_counts), 0))
    if (length(cum_counts) == 0 || all(is.na(cum_counts))) {
      message("⚠️ Skipping topic with no usable counts: ", topic)
      next
    }

    # compute logistic fit (requires >= 4 points)
    lifespan_hours_raw <- logistic_fit_hours_safe(times_days, cum_counts)

    # fallback heuristics if logistic failed or absurd
    observed_span_hours <- (max(times_days) - min(times_days) + 1) * 24
    recent_window <- min(3, length(cum_counts))
    recent_inc <- mean(diff(tail(cum_counts, recent_window)), na.rm = TRUE)
    if (is.na(recent_inc) || recent_inc <= 0) recent_inc <- 0
    heuristic_hours <- NA_real_
    if (recent_inc > 0) {
      assumed_K <- max(cum_counts, na.rm = TRUE) * 2
      remaining <- max(assumed_K - tail(cum_counts, 1), 0)
      heuristic_days <- remaining / (recent_inc)
      heuristic_hours <- heuristic_days * 24
    }

    lifespan_candidate <- lifespan_hours_raw
    if (is.na(lifespan_candidate) || !is.finite(lifespan_candidate) || lifespan_candidate > (24 * 365 * 5)) {
      if (!is.na(heuristic_hours) && is.finite(heuristic_hours) && heuristic_hours > 0 && heuristic_hours < (24 * 90)) {
        lifespan_candidate <- heuristic_hours
      } else {
        lifespan_candidate <- max(observed_span_hours, 24) * 2
      }
    }

    # store raw candidate for later scaling
    lifespan_candidates_all <- c(lifespan_candidates_all, lifespan_candidate)
    lifespan_hours_scaled <- lifespan_candidate  # temporary placeholder

    # ---------- rolling slope ---------- #
    win <- min(5, length(cum_counts))
    if (length(cum_counts) >= 2) {
      last_y <- tail(cum_counts, win)
      t_idx <- seq_len(length(last_y))
      if (length(unique(last_y)) == 1) {
        last_slope <- 0
      } else {
        last_slope <- as.numeric(coef(lm(last_y ~ t_idx))[2])
      }
    } else {
      last_slope <- 0
    }

    # ---------- Kendall tau ---------- #
    recent_idx <- tail(cum_counts, win)
    if (length(unique(recent_idx)) >= 2) {
      kt_raw <- tryCatch(MannKendall(recent_idx)$tau[1], error = function(e) NA_real_)
    } else {
      if (length(recent_idx) == 1) kt_raw <- NA_real_ else {
        if (all(diff(recent_idx) == 0)) kt_raw <- 0 else kt_raw <- 1
      }
    }
    kt_val <- ifelse(is.na(kt_raw), 0, round(as.numeric(kt_raw), 3))

    # ---------- saturation decision ---------- #
    slope_threshold <- 1e-2
    tau_threshold <- 0.05
    saturated_flag <- FALSE
    reasons <- c()

    if (!is.na(lifespan_hours_scaled) && lifespan_hours_scaled < 72) {
      saturated_flag <- TRUE
      reasons <- c(reasons, "logistic_or_heuristic_short")
    }
    if (!is.na(last_slope) && last_slope < slope_threshold) {
      reasons <- c(reasons, "low_rolling_slope")
      if (last_slope <= 0) saturated_flag <- TRUE
    }
    if (!is.na(kt_val) && kt_val < tau_threshold) {
      reasons <- c(reasons, "kendall_flat_or_decreasing")
      if (kt_val < 0) saturated_flag <- TRUE
    }
    if (length(recent_idx) >= win && (max(recent_idx) - min(recent_idx) == 0)) {
      saturated_flag <- TRUE
      reasons <- c(reasons, "no_recent_growth")
    }

    diag <- list(
      topic = topic,
      n_days = n_days,
      dates = as.character(df_t$trend_date),
      day_counts = as.integer(day_counts),
      cum_counts = as.integer(cum_counts),
      lifespan_hours_raw = ifelse(is.na(lifespan_hours_raw), NA, round(lifespan_hours_raw, 2)),
      lifespan_hours = lifespan_hours_scaled,
      last_rolling_slope = round(as.numeric(last_slope), 6),
      kendall_tau = kt_val,
      saturated = saturated_flag,
      saturation_reason = paste(unique(reasons), collapse = ";")
    )

    topic_slug <- gsub("[^[:alnum:]]+", "_", tolower(substr(topic, 1, 50)))
    topic_slug <- substr(topic_slug, 1, 50)
    write_json(diag, file.path("data_saturation", paste0("diag_", topic_slug, "_", today, ".json")),
               auto_unbox = TRUE, pretty = TRUE)

    saturation_rows[[length(saturation_rows) + 1]] <- tibble(
      topic = topic,
      n_days = n_days,
      lifespan_hours = diag$lifespan_hours,
      last_rolling_slope = diag$last_rolling_slope,
      kendall_tau = diag$kendall_tau,
      saturated = diag$saturated,
      reason = diag$saturation_reason
    )

    message("  • checked: ", topic, " (days=", n_days, ", cum_last=", tail(cum_counts,1), ")")
  }, error = function(e) {
    message("❌ Error in topic: ", topic, " — ", e$message)
  })
}

# combine and save master file
if (length(saturation_rows) > 0) {
  sat_df <- bind_rows(saturation_rows)
} else {
  sat_df <- tibble(topic = character(), n_days = integer(), lifespan_hours = numeric(),
                   last_rolling_slope = numeric(), kendall_tau = numeric(),
                   saturated = logical(), reason = character())
}

# ---- Global realistic scaling after all topics ----
if (length(lifespan_candidates_all) > 0) {
  max_cap <- 240
  min_cap <- 6
  cap_value <- quantile(lifespan_candidates_all, 0.95, na.rm = TRUE)
  scaled_vals <- pmin(lifespan_candidates_all, cap_value)
  scaled_vals <- scales::rescale(scaled_vals,
                                 to = c(min_cap, max_cap),
                                 from = range(scaled_vals, na.rm = TRUE))
  sat_df$lifespan_hours <- round(scaled_vals, 2)
}

write.csv(sat_df, file.path("data_saturation", paste0("saturation_", today, ".csv")), row.names = FALSE)
write_json(sat_df, file.path("data_saturation", paste0("saturation_", today, ".json")),
           pretty = TRUE, auto_unbox = TRUE)
saveRDS(sat_df, file.path("data_saturation", paste0("saturation_", today, ".rds")))

message("✅ Saturation detection outputs saved → data_saturation/")
