# get pairwise distance 
pairwise_dist = function(data, use_squared) {
  n = nrow(data)
  if (n <= 1000) {
    if (use_squared) {
      d = as.matrix(dist(data))
      return(d^2)
    } else {
      d = as.matrix(dist(data, method = "manhattan"))
      return(d)
    }
  } else {
    if (use_squared) {
      return(list(data = data, type = "euclidean_squared", n = n))
    } else {
      return(list(data = data, type = "manhattan", n = n))
    }
  }
}

# Compute distance between two points
compute_distance = function(data_or_dist, i, j, use_squared = TRUE) {
  if (is.matrix(data_or_dist)) {
    return(data_or_dist[i, j])
  } else {
    data = data_or_dist$data
    if (data_or_dist$type == "euclidean_squared") {
      diff = data[i, ] - data[j, ]
      return(sum(diff^2))
    } else {
      diff = data[i, ] - data[j, ]
      return(sum(abs(diff)))
    }
  }
}

# Efficient cluster cost computation
compute_cluster_cost = function(cluster_indices, data_or_dist, use_squared = TRUE) {
  n = length(cluster_indices)
  if (n < 2) return(0)
  
  if (is.matrix(data_or_dist)) {
    cluster_dist_matrix = data_or_dist[cluster_indices, cluster_indices]
    cost = sum(cluster_dist_matrix[upper.tri(cluster_dist_matrix)])
  } else {
    cost = 0
    data = data_or_dist$data
    if (data_or_dist$type == "euclidean_squared") {
      for (i in 1:(n-1)) {
        for (j in (i+1):n) {
          idx_i = cluster_indices[i]
          idx_j = cluster_indices[j]
          diff = data[idx_i, ] - data[idx_j, ]
          cost = cost + sum(diff^2)
        }
      }
    } else {
      for (i in 1:(n-1)) {
        for (j in (i+1):n) {
          idx_i = cluster_indices[i]
          idx_j = cluster_indices[j]
          diff = data[idx_i, ] - data[idx_j, ]
          cost = cost + sum(abs(diff))
        }
      }
    }
  }
  return(cost)
}

# Reindex memberships from 1
index_from_one = function(x) {
  uniq = unique(x)
  out = match(x, uniq)
  return(out)
}