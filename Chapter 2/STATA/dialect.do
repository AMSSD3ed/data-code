/*  
Chapter 2: Cluster Analysis
English Dialects Example
Change appropriate path for file "dialect.txt"
*/ 

*Read text file with 25x25 similarity matrix (lower triangular) into 25 variables
infix v1 1-3 v2 4-6 v3 7-9 v4 10-12 v5 13-15 v6 16-18 v7 19-21 v8 22-24 ///
	v9 25-27 v10 28-30 v11 31-33 v12 34-36 v13 37-39 v14 40-42 ///
	v15 43-45 v16 46-48 v17 49-51 v18 52-54 v19 55-57 v20 58-60 ///
	v21 61-63 v22 64-66 v23 67-69 v24 70-72 v25 73-75 ///
	using "C:\AMSSD\Datasets\Chapter2\dialect.txt", clear
	
*Convert to symmetric matrix
mkmat v1-v25, matrix(S)
mata: st_replacematrix("S",makesymmetric(st_matrix("S")))

* Code below reproduces:
* Table 2.9: Similarity Matrix for the English dialect data
matrix list S

*Compute dissimilarity matrix (100 - S)
matrix A=J(25,25,100)
matrix D=A-S

*Cluster analysis using nearest neighbour (single linkage)
clustermat singlelinkage D, shape(full) name(slink) clear
* Code below reproduces:
* Figure 2.7: Dendrogram for nearest neighbour (single linkage) cluster analysis
* for the English dialect data (distance = 100-similarity)
cluster dendrogram slink, title("Nearest neighbour cluster analysis of English dialects data")

*Cluster analysis using furthest neighbour (complete linkage)
clustermat completelinkage D, shape(full) name(clink) clear
* Code below reproduces:
* Figure 2.9: Dendrogram for farthest neighbour (complete linkage) cluster analysis
* for the English dialect data (distance = 100-similarity)
cluster dendrogram clink, title("Furthest neighbour cluster analysis of English dialects data")
