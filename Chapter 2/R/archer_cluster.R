# Chapter 2: Cluster Analysis
# Persian Archers Example
# Change to appropriate path for file "archer.txt".

rm(list = ls())

# archer.file <- "~/AMSSD/Datasets/Chapter2/archer.txt"
# the data file path needs to be changed to the file's location on your computer

# Alternatively, choose the file "archer.txt" directly below:
archer.file <- file.choose()

n <-  max(count.fields(archer.file))
archer.mat <- data.matrix(read.table(archer.file, fill=TRUE, col.names=1:n))
colnames(archer.mat) <- 1:n
# Similarity Matrix (Table 2.13)
archer.mat

archer.dist <- as.dist(21 - archer.mat)

# Farthest neighbour ("complete" linkage, default)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
archer.far <- hclust(archer.dist)
# Agglomeration schedule
cbind("Stage" = 1:nrow(archer.far$merge),
      "Number of clusters" = nrow(archer.far$merge):1,
      "Cluster 1" = archer.far$merge[, 1],
      "Cluster 2" = archer.far$merge[, 2],
      "Distance level" = archer.far$height,
      "Coefficients" = 21-archer.far$height)
# Dendrogram (Figure 2.16)
plot(as.dendrogram(archer.far), horiz=TRUE)

# Changing default plot arguments:
archer.den <- as.dendrogram(archer.far)
class(archer.den) # object of class "dendrogram"
plot(archer.den)
nP <- list(col=3:2, cex=c(2.0, 0.75), pch=21:22,
           bg= c("light blue", "pink"),
           lab.cex = 0.75, lab.col = "tomato") # change some colours (from dendrogram)
plot(archer.den, nodePar= nP, edgePar = list(col="gray", lwd=2), horiz = TRUE)

# Other clustering algorithms
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
archer.near <- hclust(archer.dist, method="single")
cbind("Stage" = 1:nrow(archer.near$merge),
      "Number of clusters" = nrow(archer.near$merge):1,
      "Cluster 1" = archer.near$merge[, 1],
      "Cluster 2" = archer.near$merge[, 2],
      "Distance level" = archer.near$height,
      "Coefficient" = 21 - archer.near$height)
plot(archer.near)

archer.ward <- hclust(archer.dist, method="ward.D2")
cbind("Stage" = 1:nrow(archer.ward$merge),
      "Number of clusters" = nrow(archer.ward$merge):1,
      "Cluster 1"=archer.ward$merge[, 1],
      "Cluster 2"=archer.ward$merge[, 2],
      "Distance level" = archer.ward$height,
      "Coefficient"=21 - archer.ward$height)
plot(archer.ward)


