library(text2vec) # for cosine sim matrix (or we can compute manually)
library(cluster)
library(stats)

# create a matrix where rows = topics and columns = aligned time bins (we will align by relative positions)
build_matrix_for_clustering <- function(features_list, align_by = "first_nonzero", max_len = 100) {
  # features_list: list of results from extract_features_for_topic
  rows <- list()
  topics <- c()
  for(x in features_list) {
    df <- x$timeseries
    vec <- df$engagement_sum
    # optionally align by first nonzero (start of trend)
    if(align_by == "first_nonzero") {
      nz <- which(vec > 0)
      if(length(nz) == 0) next
      start <- nz[1]
      rel <- vec[start:min(length(vec), start + max_len - 1)]
    } else {
      rel <- head(vec, max_len)
    }
    # pad to max_len
    padded <- c(rel, rep(0, max_len - length(rel)))
    # normalize (z-score) but avoid dividing by zero
    if(sd(padded) == 0) {
      normed <- rep(0, length(padded))
    } else {
      normed <- (padded - mean(padded)) / sd(padded)
    }
    rows <- append(rows, list(normed))
    topics <- c(topics, x$summary$topic)
  }
  if(length(rows) == 0) stop("No valid rows for clustering")
  mat <- do.call(rbind, rows)
  rownames(mat) <- topics
  return(mat)
}

cluster_trends <- function(mat, k = 4) {
  # Option 1: k-means on matrix rows
  set.seed(42)
  km <- kmeans(mat, centers = k, nstart = 20)
  # Optionally compute cosine distances and hierarchical cluster
  cosine_sim <- sim2(x = mat, method = "cosine", norm = "l2")
  dist_cos <- as.dist(1 - cosine_sim)
  hc <- hclust(dist_cos, method = "ward.D2")
  return(list(kmeans = km, hc = hc, dist_cos = dist_cos))
}

# Example usage
# feats_list <- map(topics, extract_features_for_topic)
# mat <- build_matrix_for_clustering(feats_list, max_len = 50)
# cl <- cluster_trends(mat, k=3)
# table(cl$kmeans$cluster)
