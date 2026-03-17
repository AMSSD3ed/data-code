# Chapter 12: Undirected Graphical Models
# Workplace Industrial Relations (WIRS) Data
rm(list = ls())

# Install required package if not already installed:
if(!all(c("igraph", "psychonetrics") %in% installed.packages())){
  install.packages(c("igraph", "psychonetrics")) 
}
library(igraph)
library(psychonetrics)

# Work Industrial Relations (WIRS) Data
# wirs.file <- "C:/AMSSD/Datasets/Chapter12/wirs.dat"
# The data file path needs to be changed to the file's location on your computer

# Alternatively, choose the file "wirs.dat" directly below:
wirs.file <- file.choose()
wirs.data <- read.table(wirs.file, header=TRUE, skip=3)
colnames(wirs.data) = c("y1", "y2", "y3","y4", "y5", "y6")

model = psychonetrics::Ising(wirs.data)
res = psychonetrics::runmodel(model)
res |> parameters()

# Code below reproduces: Table 12.3: Estimated off-diagonal entries of the precision matrix 
# for WIRS variables
adjmatr = psychonetrics::getmatrix(res, "omega", threshold = T, alpha = 0.05, adjust = "bonferroni")
round(adjmatr,2)

res |> fit()

network = igraph::graph_from_adjacency_matrix(abs(adjmatr), mode='undirected', diag=F,weighted=TRUE)
igraph::E(network)$sign = ifelse(adjmatr[lower.tri(adjmatr)][adjmatr[lower.tri(adjmatr)] != 0]> 0, 1, -1)
edge_line_types <- ifelse(igraph::E(network)$sign == 1, "solid", "dashed")

set.seed(3)
# Code below reproduces: Figure 12.4: The weighted graph for WIRS variables, constructed via
# hypothesis testing.
plot(network, edge.width=igraph::E(network)$weight*5, edge.lty = edge_line_types,
     vertex.color = "white",vertex.shape = "square",
     vertex.size= 25,
     vertex.label= c(expression(y[1]), expression(y[2]), expression(y[3]),expression(y[4]),
                     expression(y[5]), expression(y[6])))

omega = adjmatr!=0
model2 = psychonetrics::Ising(wirs.data, omega)
res2 = psychonetrics::runmodel(model2)
res2  |>  fit()


