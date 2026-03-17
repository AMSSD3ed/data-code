# Chapter 12: Undirected Graphical Models
# Senate votes in the 108th Congress of U.S.
rm(list = ls())

# Install required package if not already installed:
if(!all(c("igraph", "IsingFit") %in% installed.packages())){
  install.packages(c("igraph", "IsingFit")) 
}
library(igraph)
library(IsingFit)

# Senate Votes Dataset
# votes.file <- "C:/AMSSD/Datasets/Chapter12/voting.txt"
# The data file path needs to be changed to the file's location on your computer

# Alternatively, choose the file "voting.txt" directly below:
votes.file <- file.choose()
votes.dat = read.table(votes.file, header = T, skip = 0)


#Selecting the graph using a LASSO-based estimator (Ravikumar et al., 2010)
res = IsingFit::IsingFit(as.matrix(votes.dat), family = "binomial")

# Re-fit the model using the pseudo-likelihood
adj.matr = (res$weiadj !=0)

# Auxiliary function: Pseudo-likelihood
pseudolik <- function(data, K){
  N = nrow(data)
  J = ncol(data)
  K.diag = diag(K)
  K.off = K
  diag(K.off) = 0
  matr1 = matrix(1,N, J) %*% diag(K.diag)
  matr2 = data %*% K.off
  matr = matr1 + matr2
  logden = log(1+exp(matr))
  lognum = (matr) * data
  sum(lognum - logden)
}

# Auxiliary function: Derivative of pseudo-likelihood
pseudolik.dev <- function(data, K){
  N = nrow(data)
  J = ncol(data)
  K.diag = diag(K)
  K.off = K
  diag(K.off) = 0
  matr1 = matrix(1,N, J) %*% diag(K.diag)
  matr2 = data %*% K.off
  
  matr = matr1 + matr2
  p.matr = 1/(1+exp(-matr))
  dev.diag = colSums(data - p.matr)
  dev.off = t(data) %*% (data - p.matr) 
  dev.off = dev.off+t(dev.off)
  dev = dev.off
  diag(dev) = dev.diag
  dev
}

# Auxiliary function: Estimate pseud-likelihood
estimate.pseudo <- function(data, adj, par){
  N = nrow(data)
  J = ncol(data)
  
  obj <- function(par){
    K = matrix(0, J, J)
    K[lower.tri(K)][adj[lower.tri(adj)]!=0] = par[-(1:J)]
    K = K+t(K)
    diag(K) = par[1:J]
    - pseudolik(data, K)
  }
  obj.dev <- function(par){
    K = matrix(0, J, J)
    K[lower.tri(K)][adj[lower.tri(adj)]!=0] = par[-(1:J)]
    K = K+t(K)
    diag(K) = par[1:J]
    res = - pseudolik.dev(data, K)
    dev.diag = diag(res)
    dev.off = res[lower.tri(res)][adj[lower.tri(adj)]!=0]
    c(dev.diag, dev.off)
  }
  res = optim(par,fn = obj, gr = obj.dev, method = "L-BFGS-B")
  
  res$par
  K = matrix(0, J, J)
  K[lower.tri(K)][adj[lower.tri(adj)]!=0] = res$par[-(1:J)]
  K = K+t(K) 
  #The intercepts are not reported here
  K 
}

par = c(diag(res$weiadj), res$weiadj[lower.tri(res$weiadj)][adj.matr[lower.tri(adj.matr)]!=0])
pseudofit = estimate.pseudo(as.matrix(votes.dat), adj.matr, par)

sum(pseudofit <0)/2
sum(pseudofit >0)/2

network.senate = igraph::graph_from_adjacency_matrix(abs(pseudofit), mode='undirected', diag=F,weighted=TRUE)

igraph::E(network.senate)$sign = ifelse(pseudofit[lower.tri(pseudofit)][pseudofit[lower.tri(pseudofit)] != 0]> 0, 1, -1)

edge_line_types <- ifelse(igraph::E(network.senate)$sign == 1, "solid", "dashed")
shape.vec <- rep("circle", 100)

# Senate Votes Dataset (Auxiliary data)
# votes.file <- "C:/AMSSD/Datasets/Chapter12/votingsupp.txt"
# The data file path needs to be changed to the file's location on your computer

# Alternatively, choose the file "voting.txt" directly below:
votessupp.file <- file.choose()
votessupp.dat = read.table(votessupp.file, header = T, skip = 0)

shape.vec[votessupp.dat == "D"] = "square"
shape.vec[votessupp.dat == "I"] = "crectangle"

# Code below reproduces: Figure 12.6: Undirected graph for senators of the 108th Congress of the
# U.S. learned by a LASSO-based estimator.
set.seed(117)
plot(network.senate, edge.width = igraph::E(network.senate)$weight*2, edge.lty = edge_line_types, vertex.size = 7,
     vertex.color = "white",vertex.shape = shape.vec,  vertex.label.color="black", vertex.label = 1:100, 
     vertex.label.cex	 =0.7)




