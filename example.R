# Load pruning functions
source('R/tree_ops.R')
source('R/pruning.R')
source('R/utils.R')

# example data 1
data = data.frame(value = c(13,0,10,1,3))

# example data 2
del=3
p=5
n=8
n3=n/3
data=matrix(rnorm(n*p),n,p)
u2=rep(del,p)
data[(n3+1):(2*n3),]=data[(n3+1):(2*n3),]+matrix(u2,n/3,p,byrow=T)
u3=rep(-del,p)
data[(2*n3+1):n,]=data[(2*n3+1):n,]+matrix(u3,n/3,p,byrow=T)
data = data.frame(value = data)

# example 3
data = USArrests

#-------------------------------------------------------------------------------

# Create a hierarchical clustering dendrogram
hc = hclust(dist(data), method = "average")

# Run optimal pruning
prune_info = get_pruned_seq(hc, data, loss_type = "sum", use_squared = TRUE)
membership_seq = prune_info$membership_sequence # Sequence of memberships and merge steps

# Get membership for desired k clusters
k = 2
k_result = k_clus_membership(hc, data, k = k, loss_type = "sum", use_squared = TRUE)
print(k_result$cluster)

# Display data with clusters
data$Cluster = k_result$cluster
print(data)
