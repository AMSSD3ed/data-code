# Chapter 2: Cluster Analysis
# K-means Example using fictitious data
# Change to appropriate path for file "fake_data_kmeans.csv".

rm(list = ls())

# Data are in an lower triangular matrix
# kmeans.file <- "C:/AMSSD/Datasets/Chapter2/fake_data_kmeans.csv"
# The data file path needs to be changed to the file's location on your computer

# Alternatively, choose the file "fake_data_kmeans.csv" directly below:
kmeans.file <- file.choose()
kmeans.mat <- read.csv(kmeans.file)
# plot(kmeans.mat)

set.seed(1)
cluster = sample(1:3, 23, replace = T)

par(mfrow = c(1,2))

# Code below reproduces:
# Figure 2.12 (Panel a): Clustering of fictitious data of age and income as in Figure
# 2.1. In each plot, points in the same cluster are represented using the
# same plotting symbol. Panel (a): The individuals are randomly assigned into
# three clusters.
plot(kmeans.mat[cluster == 1,], pch = 1, xlim = c(15, 65), ylim = c(15, 125), xlab = "Age", ylab = "Income", main = "(a)")
points(kmeans.mat[cluster == 2,], pch = 2)
points(kmeans.mat[cluster == 3,], pch = 4)
legend("topleft", legend="total within-cluster dissimilarity  = 28069", bty = "n")

center <- matrix(0, 3, 2)
for(i in 1:3){
  center[i,] = colMeans(kmeans.mat[cluster == i,])
}
within <- rep(0,3)
for(i in 1:3){
  within[i] <- sum((t(kmeans.mat[cluster == i,]) - center[i,])^2)
}
sum(within)

result<- kmeans(kmeans.mat, 3, iter.max = 1000, nstart = 1000)

center <- matrix(0, 3, 2)
for(i in 1:3){
  center[i,] = colMeans(kmeans.mat[result$cluster == i,])
}
within <- rep(0,3)
for(i in 1:3){
  within[i] <- sum((t(kmeans.mat[result$cluster == i,]) - center[i,])^2)
}
sum(within)

# Code below reproduces:
# Figure 2.12 (Panel b): Clustering of fictitious data of age and income as in Figure
# 2.1. In each plot, points in the same cluster are represented using the
# same plotting symbol. Panel (b): The individuals are assigned into three clusters by
# the K-means method.
plot(kmeans.mat[result$cluster == 1,], pch = 2, xlim = c(15, 65), ylim = c(15, 125), xlab = "Age", ylab = "Income", main = "(b)")
points(kmeans.mat[result$cluster == 2,], pch = 1)
points(kmeans.mat[result$cluster == 3,], pch = 4)
legend("topleft", legend="total within-cluster dissimilarity  =  4213", bty = "n")
dev.off()

# K-means (step by step)
# ~~~~~~~~~~~~~~~~~~~~~~

set.seed(1)
cluster = sample(1:3, 23, replace = T)
par(mfrow = c(3,2))

center <- matrix(0, 3, 2)
for(i in 1:3){
  center[i,] = colMeans(kmeans.mat[cluster == i,])
}

within <- rep(0,3)
for(i in 1:3){
  within[i] <- sum((t(kmeans.mat[cluster == i,]) - center[i,])^2)
}
sum(within)

# Code below reproduces:
# Figure 2.13 (Panel a): The first three iterations of the K-means algorithm when
# applied to the fictitious example of age and income. Panels (a), (c), and (e)
# show the calculated centroids in the first three iterations. Panels (b), (d) and
# (f) show the cluster assignment based on the calculated centroids
plot(x=center[1,1],y=center[1,2], pch = 1, xlim = c(15, 65), ylim = c(15, 125), xlab = "Age", ylab = "Income",
     main = "(a) Iter 1.i)")
points(x=center[2,1],y=center[2,2],pch = 2)
points(x=center[3,1],y=center[3,2], pch = 4)
legend("topleft", legend="total within-cluster dissimilarity  = 28069", bty = "n")

dist.matr <- matrix(0, nrow(kmeans.mat), 3)
dist.matr[,1] = rowSums((kmeans.mat - matrix(center[1,],nrow = nrow(kmeans.mat), ncol=2, byrow=T))^2)
dist.matr[,2] = rowSums((kmeans.mat - matrix(center[2,],nrow = nrow(kmeans.mat), ncol=2, byrow=T))^2)
dist.matr[,3] =  rowSums((kmeans.mat - matrix(center[3,],nrow = nrow(kmeans.mat), ncol=2, byrow=T))^2)

clus.res = apply(dist.matr,1,which.min)

# Code below reproduces:
# Figure 2.13 (Panel b): The first three iterations of the K-means algorithm when
# applied to the fictitious example of age and income. Panels (a), (c), and (e)
# show the calculated centroids in the first three iterations. Panels (b), (d) and
# (f) show the cluster assignment based on the calculated centroids
plot(kmeans.mat[clus.res == 1,], pch = 1, xlim = c(15, 65), ylim = c(15, 125), xlab = "Age", ylab = "Income",
     main = "(b) Iter 1.ii)")
points(kmeans.mat[clus.res == 2,], pch = 2)
points(kmeans.mat[clus.res == 3,], pch = 4)

center <- matrix(0, 3, 2)
for(i in 1:3){
  center[i,] = colMeans(kmeans.mat[clus.res == i,])
}
within <- rep(0,3)
for(i in 1:3){
  within[i] <- sum((t(kmeans.mat[clus.res == i,]) - center[i,])^2)
}
sum(within)

# Code below reproduces:
# Figure 2.13 (Panel c): The first three iterations of the K-means algorithm when
# applied to the fictitious example of age and income. Panels (a), (c), and (e)
# show the calculated centroids in the first three iterations. Panels (b), (d) and
# (f) show the cluster assignment based on the calculated centroids
plot(x=center[1,1],y=center[1,2], pch = 1, xlim = c(15, 65), ylim = c(15, 125), xlab = "Age", ylab = "Income",
     main = "(c) Iter 2.i)")
points(x=center[2,1],y=center[2,2],pch = 2)
points(x=center[3,1],y=center[3,2], pch = 4)
legend("topleft", legend="total within-cluster dissimilarity  = 4961", bty = "n")

dist.matr <- matrix(0, nrow(kmeans.mat), 3)
dist.matr[,1] = rowSums((kmeans.mat - matrix(center[1,],nrow = nrow(kmeans.mat), ncol=2, byrow=T))^2)
dist.matr[,2] = rowSums((kmeans.mat - matrix(center[2,],nrow = nrow(kmeans.mat), ncol=2, byrow=T))^2)
dist.matr[,3] =  rowSums((kmeans.mat - matrix(center[3,],nrow = nrow(kmeans.mat), ncol=2, byrow=T))^2)
clus.res = apply(dist.matr,1,which.min)

# Code below reproduces:
# Figure 2.13 (Panel d): The first three iterations of the K-means algorithm when
# applied to the fictitious example of age and income. Panels (a), (c), and (e)
# show the calculated centroids in the first three iterations. Panels (b), (d) and
# (f) show the cluster assignment based on the calculated centroids
plot(kmeans.mat[clus.res == 1,], pch = 1, xlim = c(15, 65), ylim = c(15, 125), xlab = "Age", ylab = "Income", main = "(d) Iter 2.ii)")
points(kmeans.mat[clus.res == 2,], pch = 2)
points(kmeans.mat[clus.res == 3,], pch = 4)

center <- matrix(0, 3, 2)
for(i in 1:3){
  center[i,] = colMeans(kmeans.mat[clus.res == i,])
}
within <- rep(0,3)
for(i in 1:3){
  within[i] <- sum((t(kmeans.mat[clus.res == i,]) - center[i,])^2)
}
sum(within)

# Code below reproduces:
# Figure 2.13 (Panel e): The first three iterations of the K-means algorithm when
# applied to the fictitious example of age and income. Panels (a), (c), and (e)
# show the calculated centroids in the first three iterations. Panels (b), (d) and
# (f) show the cluster assignment based on the calculated centroids
plot(x=center[1,1],y=center[1,2], pch = 1, xlim = c(15, 65), ylim = c(15, 125), xlab = "Age", ylab = "Income",
     main = "(e) Iter 3.i)")
points(x=center[2,1],y=center[2,2],pch = 2)
points(x=center[3,1],y=center[3,2], pch = 4)
legend("topleft", legend="total within-cluster dissimilarity  = 4631", bty = "n")

dist.matr <- matrix(0, nrow(kmeans.mat), 3)
dist.matr[,1] = rowSums((kmeans.mat - matrix(center[1,],nrow = nrow(kmeans.mat), ncol=2, byrow=T))^2)
dist.matr[,2] = rowSums((kmeans.mat - matrix(center[2,],nrow = nrow(kmeans.mat), ncol=2, byrow=T))^2)
dist.matr[,3] =  rowSums((kmeans.mat - matrix(center[3,],nrow = nrow(kmeans.mat), ncol=2, byrow=T))^2)
clus.res = apply(dist.matr,1,which.min)

# Code below reproduces:
# Figure 2.13 (Panel f): The first three iterations of the K-means algorithm when
# applied to the fictitious example of age and income. Panels (a), (c), and (e)
# show the calculated centroids in the first three iterations. Panels (b), (d) and
# (f) show the cluster assignment based on the calculated centroids
plot(kmeans.mat[clus.res == 1,], pch = 1, xlim = c(15, 65), ylim = c(15, 125), xlab = "Age", ylab = "Income",
     main = "(f) Iter 3.ii)")
points(kmeans.mat[clus.res == 2,], pch = 2)
points(kmeans.mat[clus.res == 3,], pch = 4)
dev.off()

# Code below reproduces:
# Figure 2.14: Scree plot of K-means clustering for the fictitious example
# of age and income. The X-axis shows the number of clusters, and the Y-axis
# shows the total within-cluster dissimilarity of the corresponding K-means
solution.
plot(0,0,xlim = c(2,8),ylim = c(400,7000), xlab = "K", ylab = "Total within-cluster dissimilarity")
for(i in 2:8){
  result<- kmeans(kmeans.mat, i, iter.max = 1000, nstart = 1000)
  points(i,sum(result$withinss))
}
