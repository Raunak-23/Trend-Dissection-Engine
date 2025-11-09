# ===============================================================
# Trend_similarity.R
# Trend Dissection Engine – Clustering and Similarity Analysis
# ===============================================================
# Performs K-Means clustering and cosine similarity on feature matrix.
# Evaluates clusters with Silhouette and Davies–Bouldin indices.
# ===============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(cluster)
  library(factoextra)
  library(lsa)
  library(clusterSim)
  library(jsonlite)
})

message("🔍 Starting Trend Similarity Analysis...")

today <- format(Sys.Date(), "%Y-%m-%d")
feat_path <- file.path("data_features", paste0("trend_features_", today, ".rds"))
if (!file.exists(feat_path)) stop("❌ Feature file not found: ", feat_path)
feat <- readRDS(feat_path)

# ---------------------------------------------------------------
# 1️⃣ Prepare Numeric Matrix
# ---------------------------------------------------------------
topic_names <- feat$topic
feat_matrix <- feat %>% dplyr::select(-topic) %>% as.matrix()

# ---------------------------------------------------------------
# 2️⃣ Determine Optimal Number of Clusters (k)
# ---------------------------------------------------------------
set.seed(123)
wcss <- sapply(2:10, function(k) { kmeans(feat_matrix, centers = k, nstart = 10)$tot.withinss })
opt_k <- which.min(diff(diff(wcss))) + 1
message("📊 Estimated optimal k = ", opt_k)

# ---------------------------------------------------------------
# 3️⃣ Apply K-Means
# ---------------------------------------------------------------
k_model <- kmeans(feat_matrix, centers = opt_k, nstart = 25)
feat$cluster <- k_model$cluster

# ---------------------------------------------------------------
# 4️⃣ Compute Cosine Similarity Matrix
# ---------------------------------------------------------------
cos_sim <- cosine(t(feat_matrix))
sim_df <- as.data.frame(cos_sim)
rownames(sim_df) <- colnames(sim_df) <- topic_names

# ---------------------------------------------------------------
# 5️⃣ Cluster Evaluation Metrics
# ---------------------------------------------------------------
sil <- silhouette(k_model$cluster, dist(feat_matrix))
sil_score <- mean(sil[, "sil_width"])
db_index <- index.DB(feat_matrix, k_model$cluster)$DB

message("✅ Silhouette Score = ", round(sil_score, 3))
message("✅ Davies–Bouldin Index = ", round(db_index, 3))

# ---------------------------------------------------------------
# 6️⃣ Visualization (optional)
# ---------------------------------------------------------------
# fviz_cluster(k_model, data = feat_matrix, labelsize = 7, geom = "point")

# ---------------------------------------------------------------
# 7️⃣ Save Outputs
# ---------------------------------------------------------------
dir.create("data_clusters", showWarnings = FALSE)
saveRDS(feat, file.path("data_clusters", paste0("clustered_trends_", today, ".rds")))
write_json(feat, file.path("data_clusters", paste0("clustered_trends_", today, ".json")),
           pretty = TRUE, auto_unbox = TRUE)

message("📦 Clustered dataset saved → data_clusters/clustered_trends_", today)
