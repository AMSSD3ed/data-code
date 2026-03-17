# Chapter 2: Cluster Analysis
# K-means Example for Attitudes to abortion data
# Change to appropriate path for file "abortion.dat".

rm(list = ls())

# Data are in an lower triangular matrix
# abortion.file <- "C:/AMSSD/Datasets/Chapter2/abortion.dat"
# The data file path needs to be changed to the file's location on your computer

# Alternatively, choose the file "abortion.dat" directly below:
abortion.file <- file.choose()
abortion.mat <- data.matrix(read.table(abortion.file, skip=0))

# Code below reproduces:
# Table 2.14: Attitude to abortion marginal frequencies for four binary variables
apply(abortion.mat,2,table)

library(dplyr)
abortion.comp <- abortion.mat %>% 
 as.data.frame %>% 
 group_by_all() %>%
 summarise(Count = n()) %>% 
 as.matrix

as.matrix(expand.grid(lapply(1:4,function(x) c(0,1))))

set.seed(1)
result<- kmeans(abortion.mat, 2, iter.max = 1000, nstart = 1000)
round(result$centers,2)
dist.matr <- matrix(0, nrow(abortion.comp), 2)
for(i in 1:nrow(abortion.comp)){ 
 dist.matr[i,1] = sqrt( sum((abortion.comp[i,-5] - result$centers[1,])^2) )
 dist.matr[i,2] = sqrt( sum((abortion.comp[i,-5] - result$centers[2,])^2) )
}

# Code below reproduces parts of:
# Table 2.15 (Column K-Means): Response patterns, frequencies and cluster allocations using
# farthest neighbour (complete linkage) clustering and K-means clustering for
# the attitude to abortion data
apply(dist.matr,1,which.min)

