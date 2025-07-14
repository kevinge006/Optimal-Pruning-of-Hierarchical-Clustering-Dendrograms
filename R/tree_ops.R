# get all offspring of all internal nodes
get_offspr_info = function(hc) {
  merge_info = hc$merge
  num_merge = dim(merge_info)[1]
  offspr_info = list()
  x_loc_lst = list()
  for (i in 1:num_merge) {
    node1 = merge_info[i, 1]
    node2 = merge_info[i, 2]
    if (node1 < 0) {
      node1 = -node1
      left = match(node1, hc$order)
    } else {
      left = x_loc_lst[[node1]]
      node1 = offspr_info[[node1]]
    }
    if (node2 < 0) {
      node2 = -node2
      right = match(node2, hc$order)
    } else {
      right = x_loc_lst[[node2]]
      node2 = offspr_info[[node2]]
    }
    offspr_info[[i]] = c(node1, node2)
    x_loc = (left + right) / 2
    x_loc_lst[[i]] = x_loc
  }
  return(list(merge_info = merge_info, offspr_info = offspr_info, x_loc_lst = x_loc_lst))
}

# Calculate cost of collapsing a given internal node given current membership
get_added_cost = function(nodes, d, loss_type, membership, use_squared) {
  if (!(loss_type %in% c('average', 'sum'))) {
    stop('loss_type must be average or sum')
  }
  
  # Get unique clusters among the nodes based on current membership
  clusters = unique(membership[nodes])
  if (length(clusters) == 1) {
    return(0)  # Already merged, no additional cost
  }
  
  # Indices of points in the merged cluster
  merged_indices = which(membership %in% clusters)
  if (length(merged_indices) < 2) {
    return(0)  # Fewer than 2 points, no pairwise cost
  }
  
  # Compute total pairwise distance for the merged cluster
  all_pairs = combn(merged_indices, 2)
  cost = 0
  for (p in 1:ncol(all_pairs)) {
    pair = all_pairs[, p]
    i = pair[1]
    j = pair[2]
    dist_ij = d[i, j]
    cost = cost + dist_ij  # d[i, j] is already squared if use_squared = TRUE
  }
  
  if (loss_type == "average") {
    num_pairs = choose(length(merged_indices), 2)
    if (num_pairs > 0) {
      cost = cost / num_pairs  # Average over pairs in the merged cluster
    }
  }
  
  # Subtract the cost of the current subclusters
  current_cost = 0
  for (clus in clusters) {
    clus_indices = which(membership == clus)
    if (length(clus_indices) < 2) next
    clus_pairs = combn(clus_indices, 2)
    clus_cost = 0
    for (p in 1:ncol(clus_pairs)) {
      pair = clus_pairs[, p]
      i = pair[1]
      j = pair[2]
      dist_ij = d[i, j]
      clus_cost = clus_cost + dist_ij  # d[i, j] is already squared if use_squared = TRUE
    }
    if (loss_type == "average") {
      clus_cost = clus_cost / choose(length(clus_indices), 2)
    }
    current_cost = current_cost + clus_cost
  }
  
  added_cost = cost - current_cost
  return(added_cost)
}

# Record initial cost info for all internal nodes (before any cuts)
get_all_costs = function(offspr_info, d, loss_type, membership, use_squared) {
  costs_lst = numeric(length(offspr_info))
  for (i in 1:length(offspr_info)) {
    nodes = offspr_info[[i]]
    cost = get_added_cost(nodes, d, loss_type, membership, use_squared)
    costs_lst[i] = cost
  }
  return(costs_lst)
}