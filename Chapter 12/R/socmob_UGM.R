# Chapter 12: Undirected Graphical Models
# Social mobility in UK example
rm(list = ls())

# Install required package if not already installed:
if(!all(c("igraph", "metaSEM", "numDeriv", "qgraph") %in% installed.packages())){
  install.packages(c("igraph","metaSEM", "numDeriv", "qgraph")) 
}
library(igraph)
library(metaSEM)
library(numDeriv)
library(qgraph)

# Social Mobility Dataset
# socmob.file <- "C:/AMSSD/Datasets/Chapter12/socmob.txt"
# The data file path needs to be changed to the file's location on your computer

# Alternatively, choose the file "socmob.txt" directly below:
socmob.file <- file.choose()
socmob.cor <- data.frame(read.table(socmob.file, skip=0))
S = socmob.cor

prec.matr = solve(S)

#Compute the conditional correlations using the precision matrix 
concorr.matr = diag(1/sqrt(diag(prec.matr))) %*% (-prec.matr) %*% diag(1/sqrt(diag(prec.matr)))
diag(concorr.matr) = 1
#Obtaining the asymptotic covariance matrix for the sample correlation matrix
asyCov.matr = metaSEM::asyCov(as.matrix(S), n = 713, cor.analysis = TRUE)

par <- S[lower.tri(S)]
p = 10
sd.matr <- matrix(0, p, p)

for(i in 1:(p-1)){
  for(j in (i+1):p){
    
    f <- function(par){
      S <- matrix(0, p, p)
      S[lower.tri(S)] = par
      S = S+t(S) + diag(1,p)
      prec = solve(S)
      prec[i,j]
    }
    
    grad.f <- numDeriv::grad(f, par)
    sd.matr[j,i] =  sqrt(t(grad.f) %*% asyCov.matr %*% grad.f)
  }
}

# Code below reproduces (parts of): Table 12.1: Estimated off-diagonal entries
# of the precision matrix for social mobility variables.
prec.vec = prec.matr[lower.tri(prec.matr)]

sd.vec = sd.matr[lower.tri(sd.matr)]
z.stat = prec.vec/sd.vec
p.vec = 2*(1-pnorm(abs(z.stat)))

#Bonferroni correction
which(p.vec <= 0.05/(p*(p-1)/2))

p.ind = rep(0, length(p.vec))
p.ind[p.vec <= 0.05/(p*(p-1)/2)] = 1

graph.matr = matrix(0, p,p)
graph.matr[lower.tri(graph.matr)] = p.ind
graph.matr = graph.matr + t(graph.matr)
network = igraph::graph_from_adjacency_matrix(graph.matr, mode='undirected', diag=F,weighted=TRUE)
set.seed(1234)

# Code below reproduces: Figure 12.2a: An un-weighted undirected graph.
plot(network, edge.width = igraph::E(network)$weight *2, vertex.color = "white",
     vertex.shape = "square",vertex.label.color="black", vertex.size= 30,
     vertex.label= c("HF/O", "WF/O", "H/FE", "H/Q", "H/O", "W/FE", "W/Q", "FB/FE", "FB/Q", "FB/O"))

# Code below reproduces (parts of): Table 12.2: Estimated conditional correlations 
# for social mobility variables
Par.corr = - diag(1/sqrt(diag(prec.matr)))%*% prec.matr %*% diag(1/sqrt(diag(prec.matr)))

Par.corr[graph.matr ==0] = 0
network.w = igraph::graph_from_adjacency_matrix(Par.corr, mode='undirected', diag=F,weighted=TRUE)
set.seed(1234)

# Code below reproduces: Figure 12.2b: An weighted undirected graph.
plot(network.w, edge.width = igraph::E(network.w)$weight *20, vertex.shap = "square",
     vertex.color = "white", vertex.label.color="black", vertex.size= 30,
     vertex.label= c("HF/O", "WF/O", "H/FE", "H/Q", "H/O", "W/FE", "W/Q", "FB/FE", "FB/Q", "FB/O"))

# Regularised estimator

res.reg = qgraph::EBICglasso(as.matrix(S), n = 713, threshold = TRUE, refit = TRUE)
network.reg = graph_from_adjacency_matrix(res.reg, mode='undirected', diag=F,weighted=TRUE)


# Code below reproduces: Figure 12.3: The weighted graph for social mobility variables constructed
# using a graphical LASSO estimator
plot(network.reg, edge.width = igraph::E(network.reg)$weight *20, vertex.shape = "square",
     vertex.color = "white", vertex.label.color="black", vertex.size= 25,
     vertex.label= c("HF/O", "WF/O", "H/FE", "H/Q", "H/O", "W/FE", "W/Q", "FB/FE", "FB/Q", "FB/O"))
