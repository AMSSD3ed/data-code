/*  
Chapter 2: Cluster Analysis
Educational Variables Example
Change appropriate path for file "educ.txt"
*/ 

*Read text file with 9x9 correlation matrix (full) into 9 variables
infile v1-v9 using "C:\AMSSD\Datasets\Chapter2\educ.txt", clear
	
*Convert to symmetric matrix
mkmat v1-v9, matrix(S)
mata: st_replacematrix("S",makesymmetric(st_matrix("S")))

*Compute dissimilarity matrix (1 - S)
matrix A=J(9,9,1)
matrix D=A-S

*Input variable labels for use in dendrogram
set obs 9
input str10 lab 
	"par-64"
	"teach-64"
	"inter-64"
	"att-64"
	"score-64"
	"sch-68"
	"par-68"
	"inter-68"
	"score-68"

*Cluster analysis using nearest neighbour (single linkage)
clustermat singlelinkage D, shape(full) name(slink) add

*Code below reproduces: Figure 2.20
cluster dendrogram slink, ///
	title("Nearest neighbour cluster analysis of education data") ///
	labels(lab) scale(0.8)


