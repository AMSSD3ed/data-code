# Chapter 5: Principal Component Analysis (PCA)
# European Social Mobility  Example 
# Change to appropriate path for file "socmob.txt".

rm(list = ls())

# Social Mobility Dataset
# socmob.file <- "C:/MSSD/Datasets/Chapter5/socmob.txt"
# The data file path needs to be changed to the file's location on your computer

# Alternatively, choose the file "socmob.txt" directly below:
socmob.file <- file.choose()

socmob.mat <- data.matrix(read.table(socmob.file))
rownames(socmob.mat) <- colnames(socmob.mat) <- c("hfo", "wfo", "hfe", "hq", "ho", "wfe", "wq", "fbfe", "fbq", "fbo")

# Table 5.14: Pairwise correlations (x100) between social mobility variables
round(socmob.mat * 100)

socmob.pca <- princomp(covmat=socmob.mat)
# names(socmob.pca)

# Table 5.15: Loadings (a*) for Social Mobility data
socmob.astar <- socmob.pca$loadings* 
                rep(socmob.pca$sdev,each=length(socmob.pca$sdev))
print(round(socmob.astar, d=2), cutoff=0)
