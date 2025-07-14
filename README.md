📌 Project Title
Optimal Pruning of Hierarchical Clustering Dendrograms


🔍 Overview
Given a hierarchical clustering dendrogram, our method finds the optimal clustering for any fixed number of groups k. To select an appropriate value of k, we recommend using the Gap statistic, though other methods can also be applied.


🚀 How to Use
To use our method to obtain the optimal clustering: Use the functions in pruning.R

- get_pruned_seq() returns the sequence of optimal cluster memberships.
- k_clus_membership(k) returns the membership vector for exactly k clusters, selected from the optimal sequence.

To see how the method works in practice, run the example script.


📁 Repository Structure
R/
├── tree_ops.R        # Tree utilities for dendrogram structure
├── pruning.R         # Main pruning logic and optimal clustering functions
├── utils.R           # Helper functions

example.R             # Example script demonstrating usage
README.md             # This file


📎 Additional Materials
If you're interested in:

- Simulation code comparing our optimal pruning method to horizontal cuts and dynamic programming,
- Code to visualize the pruning process or render pruned dendrograms,
- Code for running the Gap test,

please contact the corresponding author Jiacheng Ge —  kevinge1@alumni.stanford.edu

