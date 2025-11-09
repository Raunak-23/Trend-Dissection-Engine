# ===============================================================
# Trend_similarity.R - Debugged Version
# ===============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(cluster)
  library(factoextra)
  library(lsa)
  library(clusterSim)
  library(jsonlite)
  library(tidyr)
})

message("🔍 Starting Trend Similarity Analysis...")

source("R/utils_load_data.R")

# ---------------------------------------------------------------
# 1️⃣ Load Data
# ---------------------------------------------------------------
data_all <- get_all_trend_data("data_clean")

# CRITICAL: Ensure data frame structure
if (!is.data.frame(data_all)) {
  data_all <- as.data.frame(data_all)
}

# ---------------------------------------------------------------
# 🔧 NEW: Force Numeric Conversion (THE KEY FIX)
# ---------------------------------------------------------------
message("⚙️ Converting columns to numeric types...")

# Define all fields that MUST be numeric
numeric_field_names <- c(
  "trend_score", "trend_lifespan_hours", "external_interest_index", 
  "avg_score", "avg_comments", "avg_sentiment", "avg_velocity",
  "post_count", "insta_likes", "insta_comments", "insta_sentiment",
  "insta_engagement", "engagement_ratio", "sentiment_weighted_velocity",
  "trend_intensity", "insta_influence"
)

# Force conversion (handles characters, factors, and list remnants)
for (col in numeric_field_names) {
  if (col %in% names(data_all)) {
    # Convert to character first to handle any type, then to numeric
    data_all[[col]] <- suppressWarnings(as.numeric(as.character(data_all[[col]])))
  }
}

# ---------------------------------------------------------------
# 🔧 Flatten any remaining list columns
# ---------------------------------------------------------------
flatten_lists <- function(df) {
  if (!is.data.frame(df)) return(as.data.frame(df))
  
  for (col in names(df)) {
    if (is.list(df[[col]]) && !is.data.frame(df[[col]])) {
      df[[col]] <- sapply(df[[col]], function(x) {
        if (is.null(x) || length(x) == 0) return(NA)
        if (is.atomic(x)) return(x[1])
        return(NA)
      })
    }
  }
  return(df)
}

data_all <- flatten_lists(data_all)

# ---------------------------------------------------------------
# ✅ DEBUG: Verify column types
# ---------------------------------------------------------------
message("\n📊 Column Type Report:")
type_report <- sapply(data_all, function(x) paste(class(x), collapse = "/"))
print(data.frame(Column = names(type_report), Type = type_report))

# ---------------------------------------------------------------
# 2️⃣ Prepare Numeric Matrix
# ---------------------------------------------------------------
message("\n⚙️ Preparing feature matrix...")

feat <- distinct(data_all)

# Select ONLY numeric columns
numeric_cols <- sapply(feat, is.numeric)
message("📈 Numeric columns found: ", sum(numeric_cols))

# CRITICAL CHECK - Stop with clear message if no numeric columns
if (sum(numeric_cols) == 0) {
  stop(
    "❌ NO NUMERIC COLUMNS DETECTED!\n\n",
    "Column types:\n",
    paste(names(feat), "=", sapply(feat, class), collapse = "\n"),
    "\n\nThis usually means:\n",
    "1. JSON data was loaded as strings\n",
    "2. The flatten_lists() function failed\n",
    "3. Data has inconsistent types across files"
  )
}

feat_matrix <- feat[, numeric_cols, drop = FALSE]

# ENSURE it's a matrix (not a data frame or list)
feat_matrix <- as.matrix(feat_matrix)

# Validate matrix structure
if (!is.matrix(feat_matrix)) {
  stop("❌ feat_matrix is not a matrix. Actual class: ", class(feat_matrix))
}

# ---------------------------------------------------------------
# 3️⃣ Clean Matrix Values
# ---------------------------------------------------------------
# Replace Inf, -Inf, NA with 0
feat_matrix[!is.finite(feat_matrix)] <- 0
feat_matrix[is.na(feat_matrix)] <- 0

# Remove constant columns (zero variance)
non_constant <- apply(feat_matrix, 2, function(col) sd(col, na.rm = TRUE) > 0)
if (any(!non_constant)) {
  message("🗑️ Removing constant columns: ", paste(colnames(feat_matrix)[!non_constant], collapse = ", "))
  feat_matrix <- feat_matrix[, non_constant, drop = FALSE]
}

# Remove rows with all zeros
nonzero_rows <- rowSums(abs(feat_matrix)) > 0
feat_matrix <- feat_matrix[nonzero_rows, , drop = FALSE]
feat <- feat[nonzero_rows, , drop = FALSE]

# Final validation
if (nrow(feat_matrix) < 3) {
  stop("❌ Not enough valid rows for clustering. Only ", nrow(feat_matrix), " rows remain.")
}

message("✅ Feature matrix ready: ", nrow(feat_matrix), " rows × ", ncol(feat_matrix), " features")

# ---------------------------------------------------------------
# 4️⃣ K-Means Clustering (rest of your code continues...)
# ---------------------------------------------------------------
set.seed(123)
max_k <- min(10, nrow(feat_matrix) - 1)
if (max_k < 3) max_k <- 3

message("\n📊 Estimating optimal k...")
wcss <- sapply(2:max_k, function(k) {
  suppressWarnings(kmeans(feat_matrix, centers = k, nstart = 10)$tot.withinss)
})
opt_k <- which.min(diff(diff(wcss))) + 1
opt_k <- max(2, min(opt_k, max_k))
message("✅ Estimated optimal k = ", opt_k)

# Apply K-means
k_model <- kmeans(feat_matrix, centers = opt_k, nstart = 25)
feat$cluster <- k_model$cluster

# ---------------------------------------------------------------
# 5️⃣ Compute Similarity
# ---------------------------------------------------------------
message("\n🧮 Computing cosine similarity...")
cos_sim <- tryCatch({
  cosine(t(scale(feat_matrix)))
}, error = function(e) {
  warning("⚠️ Cosine similarity failed: ", e$message)
  NULL
})

# ---------------------------------------------------------------
# 6️⃣ Evaluate Clusters
# ---------------------------------------------------------------
sil_score <- NA; db_index <- NA
try({
  sil <- silhouette(k_model$cluster, dist(feat_matrix))
  sil_score <- mean(sil[, "sil_width"])
}, silent = TRUE)

try({
  db_index <- index.DB(feat_matrix, k_model$cluster)$DB
}, silent = TRUE)

message("✅ Silhouette Score = ", round(sil_score, 3))
message("✅ Davies–Bouldin Index = ", round(db_index, 3))

# ---------------------------------------------------------------
# 7️⃣ Save Results
# ---------------------------------------------------------------
dir.create("data_clusters", showWarnings = FALSE)
today <- format(Sys.Date(), "%Y-%m-%d")

saveRDS(feat, file.path("data_clusters", paste0("clustered_trends_", today, ".rds")))
write_json(feat, file.path("data_clusters", paste0("clustered_trends_", today, ".json")),
           pretty = TRUE, auto_unbox = TRUE)

if (!is.null(cos_sim)) {
  write_json(as.data.frame(cos_sim),
             file.path("data_clusters", paste0("cosine_similarity_", today, ".json")),
             pretty = TRUE, auto_unbox = TRUE)
}

message("\n📦 Results saved to data_clusters/")
message("✅ Trend Similarity Analysis completed successfully!")