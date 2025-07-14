# Compare added cost per node of different cut choices
choose_cut = function(offspr_info, d, loss_type, cutted_node_lst, merge_info, membership, use_squared) {
  costs_pn_lst = numeric(length(offspr_info))
  for (i in 1:length(offspr_info)) {
    if (i %in% cutted_node_lst) {
      costs_pn_lst[i] = Inf
      next
    }
    nodes = offspr_info[[i]]
    cost = get_added_cost(nodes, d, loss_type, membership, use_squared)
    if (cost == 0) {
      costs_pn_lst[i] = Inf
      next
    }
    n_springoff = length(unique(membership[nodes]))
    n_node_remain = 1
    cost_pn = cost / (n_springoff - n_node_remain)
    costs_pn_lst[i] = cost_pn
  }
  best_cut = which.min(costs_pn_lst)
  if (is.infinite(costs_pn_lst[best_cut])) {
    return(list(best_cut = NULL, cutted_node_lst = cutted_node_lst))
  }
  stack = c(best_cut)
  while (length(stack) > 0) {
    cur_node = tail(stack, 1)
    cutted_node_lst = union(cutted_node_lst, cur_node)
    stack = stack[-length(stack)]
    left = merge_info[cur_node, 1]
    right = merge_info[cur_node, 2]
    if (left > 0) stack = c(stack, left)
    if (right > 0) stack = c(stack, right)
  }
  return(list(best_cut = best_cut, cutted_node_lst = cutted_node_lst))
}

# Assign new memberships after a cut
get_membership = function(best_cut, offspr_info, membership) {
  clus = offspr_info[[best_cut]]
  old_max = max(membership)
  membership[clus] = as.integer(old_max + 1)
  return(membership)
}

# Produce a sequence of memberships
get_pruned_seq = function(hc, data, loss_type, use_squared) {
  n = nrow(data)
  # Compute the distance matrix
  d = pairwise_dist(data, use_squared)  # This returns a matrix
  merge_info = hc$merge
  offspr_out = get_offspr_info(hc)
  merge_info = offspr_out$merge_info
  offspr_info = offspr_out$offspr_info
  x_loc_lst = offspr_out$x_loc_lst
  membership = 1:n
  msp_lst = list()
  msp_lst[[1]] = membership
  cut_lst = list(0)
  cutted_node_lst = c()
  step = 1
  while (length(cutted_node_lst) == 0 || max(cutted_node_lst) < (n - 1)) {
    step = step + 1
    cut_info = choose_cut(offspr_info, d, loss_type, cutted_node_lst, merge_info, membership, use_squared)
    best_cut = cut_info$best_cut
    if (is.null(best_cut)) break
    cutted_node_lst = cut_info$cutted_node_lst
    membership = get_membership(best_cut, offspr_info, membership)
    msp_lst[[step]] = index_from_one(membership)
    cut_lst[[step - 1]] = best_cut
  }
  return(list(data = data, membership_sequence = msp_lst, merge_steps = cut_lst)) #return x_loc_lst to record plot labels positions
}

# Return membership for k clusters
k_clus_membership = function(hc, data, k, loss_type, use_squared) {
  prune_info = get_pruned_seq(hc, data, loss_type = loss_type, use_squared = use_squared)
  membership_seq = prune_info$membership_sequence
  seq_msp_len = sapply(lapply(membership_seq, unique), length)
  while (!k %in% seq_msp_len) {
    print(paste("There is no subtree with", k, "leaves when using optimal pruning"))
    k = k + 1
    print(paste("k is increased to", k))
  }
  membership = membership_seq[[which(seq_msp_len == k)]]
  return(list(cluster = membership))
}