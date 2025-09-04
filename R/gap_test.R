# Gap statistic for optimal cluster selection
# Adapted from existing implementations to work with optimal pruning

# Wrapper function for use with clusGap from cluster package
optimal_clustering_wrapper = function(data, k) {
  # This wrapper is designed to work with the clusGap function
  # It returns cluster assignments for k clusters using optimal pruning
  if (k == 1) {
    # For k=1, return a list with cluster assignments (all points in cluster 1)
    return(list(cluster = rep(1, nrow(data))))
  }
  
  hc = hclust(dist(data), method = "average")
  result = k_clus_membership(hc, data, k = k, loss_type = "sum", use_squared = TRUE, quiet = TRUE)
  # Return the result in the expected format for clusGap
  return(list(cluster = result$cluster))
}

# Function to find optimal k using Gap statistic
find_optimal_k_gap = function(data, max_k = min(nrow(data) - 1, 20), B = 50, method) {
  # Load required library
  if (!requireNamespace("cluster", quietly = TRUE)) {
    stop("Package 'cluster' is required for Gap statistic. Please install it with: install.packages('cluster')")
  }
  
  # Limit max_k for computational efficiency
  if (max_k > 20) {
    warning("max_k limited to 20 for computational efficiency")
    max_k = 20
  }
  
  if (max_k > nrow(data)) {
    warning("max_k reduced to sample size")
    max_k = nrow(data)
  }
  
  # Run Gap statistic
  gap_stat = cluster::clusGap(data, FUN = optimal_clustering_wrapper, K.max = max_k, B = B)
  
  # Find optimal k using different methods
  opt_k = cluster::maxSE(f = gap_stat$Tab[, "gap"], SE.f = gap_stat$Tab[, "SE.sim"], method = method)
  
  return(list(
    optimal_k = opt_k,
    gap_stat = gap_stat,
    gap_table = data.frame(gap_stat$Tab, k = 1:max_k)
  ))
}

# Function to plot Gap statistic results
plot_gap_statistic = function(gap_result) {
  # Check if ggplot2 is available
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    # Fall back to base R plotting
    dat = gap_result$gap_table
    plot(dat$k, dat$gap, type = "b", pch = 19, 
         xlab = "Number of clusters", ylab = "Gap statistic",
         main = "Gap Statistic")
    # Add error bars (simplified)
    arrows(dat$k, dat$gap - dat$SE.sim, dat$k, dat$gap + dat$SE.sim,
           length = 0.05, angle = 90, code = 3)
    abline(v = gap_result$optimal_k, col = "red", lty = 2)
    legend("topright", paste("Optimal k =", gap_result$optimal_k), col = "red", lty = 2)
    return(invisible(NULL))
  }
  
  # Use ggplot2 if available
  dat = gap_result$gap_table
  p = ggplot2::ggplot(dat, ggplot2::aes(k, gap)) + 
    ggplot2::geom_line() + 
    ggplot2::geom_point(size = 3) +
    ggplot2::geom_errorbar(ggplot2::aes(ymax = gap + SE.sim, ymin = gap - SE.sim), width = 0.25) +
    ggplot2::geom_vline(xintercept = gap_result$optimal_k, color = "red", linetype = "dashed") +
    ggplot2::labs(x = "Number of clusters", y = "Gap statistic",
                  title = paste("Gap Statistic (Optimal k =", gap_result$optimal_k, ")")) +
    ggplot2::theme_minimal()
  
  return(p)
}
