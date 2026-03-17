# Chapter 2: Cluster Analysis
# English Dialects Example
# Change to appropriate path for file "dialect.txt".

rm(list = ls())

# Data are in an lower triangular matrix
# dialect.file <- "C:/AMSSD/Datasets/Chapter2/dialect.txt"
# The data file path needs to be changed to the file's location on your computer

# Alternatively, choose the file "dialect.txt" directly below:
dialect.file <- file.choose()

n <-  max(count.fields(dialect.file))
dialect.mat <- data.matrix(read.table(dialect.file, fill=TRUE, col.names=1:n))
colnames(dialect.mat) <- rownames(dialect.mat) <- paste0("V",1:n)

# Code below reproduces:
# Table 2.9: Similarity Matrix for the English dialect data
dialect.mat

dialect.dist <- as.dist(100 - dialect.mat)

# Nearest neighbour ("single" linkage)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dialect.near <- hclust(dialect.dist, method="single")

# Agglomeration schedule
# Code below reproduces:
# Table 2.10: Nearest Neighbour Agglomeration table, English dialect data
cbind("Stage" = 1:nrow(dialect.near$merge),
      "Number of clusters" = nrow(dialect.near$merge):1,
      "Cluster 1" = dialect.near$merge[, 1],
      "Cluster 2" = dialect.near$merge[, 2],
      "Distance level" = dialect.near$height,
      "Coefficients" = 100-dialect.near$height)

# Code below reproduces:
# Figure 2.7: Dendrogram for nearest neighbour (single linkage) cluster analysis
# for the English dialect data (distance = 100-similarity)
plot(as.dendrogram(dialect.near), horiz=TRUE, xlab = "Distance")

# Farthest neighbour ("complete" linkage, default)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dialect.far <- hclust(dialect.dist)
# Agglomeration schedule
cbind("Stage" = 1:nrow(dialect.far$merge),
      "Number of clusters" = nrow(dialect.far$merge):1,
      "Cluster 1" = dialect.far$merge[, 1],
      "Cluster 2" = dialect.far$merge[, 2],
      "Distance level" = dialect.far$height,
      "Coefficients" = 100-dialect.far$height)

# Code below reproduces:
# Figure 2.9: Dendrogram for farthest neighbour (complete linkage) cluster analysis
# for the English dialect data (distance = 100-similarity)
plot(as.dendrogram(dialect.far), horiz=TRUE, xlab = "Distance")
