# Chapter 12: Undirected Graphical Models
# Post-traumatic stress disorder symptoms in U.S. military veterans
rm(list = ls())

# Install required package if not already installed:
if(!all(c("igraph", "psychonetrics") %in% installed.packages())){
  install.packages(c("igraph", "psychonetrics")) 
}
library(igraph)
library(psychonetrics)

# PTSD Dataset
# ptsd.file <- "C:/AMSSD/Datasets/Chapter12/PTSDmaleW1corr.txt"
# The data file path needs to be changed to the file's location on your computer

# Alternatively, choose the file "PTSDmaleW1corr.txt" directly below:
ptsd.file <- file.choose()

# Code below reproduces parts of: Table 12.5: Pairwise correlations between PTSD items
ptsd.cor <- read.table(ptsd.file, header=TRUE, skip=0); round(ptsd.cor, 2)

res.regPTSD = qgraph::EBICglasso(as.matrix(ptsd.cor), n = 1136, threshold = TRUE, refit = TRUE)
network.regPTSD = igraph::graph_from_adjacency_matrix(abs(res.regPTSD), mode='undirected', diag=F,weighted=TRUE)
igraph::E(network.regPTSD)$sign = ifelse(res.regPTSD[lower.tri(res.regPTSD)][res.regPTSD[lower.tri(res.regPTSD)] != 0]> 0, 1, -1)

edge_line_types <- ifelse(igraph::E(network.regPTSD)$sign == 1, "solid", "dashed")

# Code below reproduces: Figure 12.5: Undirected graph for PTSD symptoms learned by a graphical LASSO estimator
set.seed(1234)
plot(network.regPTSD, edge.width = igraph::E(network.regPTSD)$weight*15, edge.lty = edge_line_types,
     vertex.size = 14, vertex.color = "white",vertex.shape = "square",  vertex.label.color="black")

