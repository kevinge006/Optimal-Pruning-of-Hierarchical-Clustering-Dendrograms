# get pairwise distance into a lower triangular matrix
pairwise_dist = function(data, use_squared) {
  if (use_squared) {
    d = as.matrix(dist(data))  # Euclidean distance
    return(d^2)  # Squared Euclidean distance
  } else {
    d = as.matrix(dist(data, method = "manhattan"))  # L1-norm (Manhattan distance)
    return(d)
  }
}

# Reindex memberships from 1
index_from_one = function(x) {
  uniq = unique(x)
  out = match(x, uniq)
  return(out)
}