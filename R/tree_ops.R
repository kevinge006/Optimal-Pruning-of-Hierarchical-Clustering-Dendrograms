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
  
  clusters = unique(membership[nodes])
  if (length(clusters) == 1) {
    return(0)
  }
  
  merged_indices = which(membership %in% clusters)
  if (length(merged_indices) < 2) {
    return(0)
  }
  
  cost = compute_cluster_cost(merged_indices, d, use_squared)
  
  if (loss_type == "average") {
    num_pairs = length(merged_indices) * (length(merged_indices) - 1) / 2
    if (num_pairs > 0) {
      cost = cost / num_pairs
    }
  }
  
  current_cost = 0
  for (clus in clusters) {
    clus_indices = which(membership == clus)
    if (length(clus_indices) < 2) next
    
    clus_cost = compute_cluster_cost(clus_indices, d, use_squared)
    
    if (loss_type == "average") {
      n_clus = length(clus_indices)
      clus_cost = clus_cost / (n_clus * (n_clus - 1) / 2)
    }
    current_cost = current_cost + clus_cost
  }
  
  added_cost = cost - current_cost
  return(added_cost)
}

# Record initial cost info for all internal nodes (before any cuts)
get_all_costs = function(offspr_info, d, loss_type, membership, use_squared) {
  costs_lst = numeric(length(offspr_info))
  for (i in seq_along(offspr_info)) {
    nodes = offspr_info[[i]]
    cost = get_added_cost(nodes, d, loss_type, membership, use_squared)
    costs_lst[i] = cost
  }
  return(costs_lst)
}