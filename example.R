# Load pruning functions
source('R/tree_ops.R')
source('R/pruning.R')
source('R/utils.R')
source('R/gap_test.R')

set.seed(1)

#### Example 0: Show sequence of membership for simple data
cat("=== Example 0: Membership Sequence ===\n")

data0 = data.frame(value = c(0, 2, 10, 13))
hc0 = hclust(dist(data0), method = "average")

# Get the pruning sequence using paper settings: average loss, L1 distance
prune_info = get_pruned_seq(hc0, data0, loss_type = "average", use_squared = FALSE)
membership_seq = prune_info$membership_sequence

# Display the sequence (note: some k values may be skipped)
for (i in seq_along(membership_seq)) {
  membership = membership_seq[[i]]
  k = length(unique(membership))
  cat(sprintf("k=%d: %s\n", k, paste(membership, collapse=" ")))
  
  # Show which data points are in each cluster for clarity
  clusters = unique(membership)
  for (cluster_id in clusters) {
    points_in_cluster = which(membership == cluster_id)
    values_in_cluster = data0$value[points_in_cluster]
    cat(sprintf("     Cluster %d: rows %s (values: %s)\n", 
                cluster_id, 
                paste(points_in_cluster, collapse=","),
                paste(values_in_cluster, collapse=",")))
  }
}
cat("Note: k=2 is skipped because it's not optimal (as shown in the paper)\n")


#### Example 1: Show sequence of membership for simple data
cat("=== Example 1: Membership Sequence ===\n")

data1 = data.frame(value = c(13, 0, 10, 1, 3))
hc1 = hclust(dist(data1), method = "average")

# Get the pruning sequence using paper settings: sum loss, L2 distance
prune_info = get_pruned_seq(hc1, data1, loss_type = "sum", use_squared = TRUE)
membership_seq = prune_info$membership_sequence

# Display the sequence (note: some k values may be skipped)
for (i in seq_along(membership_seq)) {
  membership = membership_seq[[i]]
  k = length(unique(membership))
  cat(sprintf("k=%d: %s\n", k, paste(membership, collapse=" ")))
  
  # Show which data points are in each cluster for clarity
  clusters = unique(membership)
  for (cluster_id in clusters) {
    points_in_cluster = which(membership == cluster_id)
    values_in_cluster = data1$value[points_in_cluster]
    cat(sprintf("     Cluster %d: rows %s (values: %s)\n", 
                cluster_id, 
                paste(points_in_cluster, collapse=","),
                paste(values_in_cluster, collapse=",")))
  }
}


#### Example 2: User specifies k and gets clustering result
cat("=== Example 2: Clustering for Specific k ===\n")
del=3
p=5
n=8
n3=n/3
data2=matrix(rnorm(n*p),n,p)
u2=rep(del,p)
data2[(n3+1):(2*n3),]=data2[(n3+1):(2*n3),]+matrix(u2,n/3,p,byrow=T)
u3=rep(-del,p)
data2[(2*n3+1):n,]=data2[(2*n3+1):n,]+matrix(u3,n/3,p,byrow=T)
data2 = data.frame(value = data2)
print(data2)

hc2 = hclust(dist(data2), method = "average")
k = 3
result2 = k_clus_membership(hc2, data2, k = k, loss_type = "sum", use_squared = TRUE)

cat(sprintf("\nClustering result for k=%d:\n", k))
data2$Cluster = result2$cluster
print(data2)

#### Example 3: Use Gap statistic to find optimal number of clusters and apply clustering
cat("=== Example 3: Gap Statistic for Optimal k Selection ===\n")

# Check if cluster package is available
if (requireNamespace("cluster", quietly = TRUE)) {
  
  data3 = USArrests
  cat("Finding optimal k using Gap statistic...\n")
  
  # Find optimal k using Gap statistic, see cluster::clusGa() for more details about B and method
  gap_result = find_optimal_k_gap(data3, max_k = 12, B = 20, method = "globalSEmax") 
  
  cat(sprintf("Gap statistic suggests optimal k = %d\n", gap_result$optimal_k))
  
  # Apply clustering with optimal k
  hc3 = hclust(dist(data3), method = "average")
  optimal_result = k_clus_membership(hc3, data3, k = gap_result$optimal_k, 
                                   loss_type = "sum", use_squared = TRUE)
  
  cat("\nGap statistic values:\n")
  print(round(gap_result$gap_table[, c("k", "gap", "SE.sim")], 4))
  
  cat(sprintf("\nClustering result with optimal k=%d:\n", gap_result$optimal_k))
  data3$Cluster = optimal_result$cluster
  
} else {
  cat("To run Gap statistic example, install 'cluster' package:\n")
  cat("install.packages('cluster')\n")
}

