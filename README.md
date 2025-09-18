# Optimal Pruning of Hierarchical Clustering Dendrograms

## Overview
Given a hierarchical clustering dendrogram, our method finds the optimal clustering for any fixed number of groups k. To select an appropriate value of k, we recommend using the Gap statistic, though other methods can also be applied.

## Citation
If you use this method in your research, please cite:

Ge, Jiacheng & Tibshirani, Robert (2025). Optimal pruning of hierarchical clustering dendrograms. *Communications in Statistics - Theory and Methods*. https://doi.org/10.1080/03610926.2025.2543191

## How to Use
To use our method to obtain the optimal clustering:

- `get_pruned_seq()` returns the sequence of optimal cluster memberships.
- `k_clus_membership(k)` returns the membership vector for exactly k (user-specified) clusters, selected from the optimal sequence.
- `find_optimal_k_gap()` automatically selects the optimal number of clusters using the Gap statistic

**Understanding the output**: The membership vector shows which cluster each data point belongs to. For example, `[1 2 1 2 3]` means points 1 and 3 are in cluster 1, points 2 and 4 are in cluster 2, and point 5 is in cluster 3.

To see how the method works in practice, run `example.R`

## Repository Structure
```
R/
├── tree_ops.R     # Tree utilities for dendrogram structure
├── pruning.R      # Main pruning logic and optimal clustering functions
├── utils.R        # Helper functions for distance computation
├── gap_test.R     # Gap statistic (not this paper's contribution) functionality for optimal k selection

example.R          # Examples demonstrating functionality
README.md          # This file
```

## Dependencies

**Gap Statistic**: Requires `cluster` package (`install.packages("cluster")`)

## Contact
For questions about simulation code, visualization tools, or advanced configuration options, please contact Jiacheng Ge — kevinge1@alumni.stanford.edu

