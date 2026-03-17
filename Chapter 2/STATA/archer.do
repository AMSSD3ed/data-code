/*  
Chapter 2: Cluster Analysis
Persian Archer Example
Change appropriate path for file "archer.txt"
*/ 

*Read text file with 24x24 similarity matrix (lower triangular) into 24 variables
infix v1 2-7 v2 9-14 v3 16-21 v4 23-28 v5 30-35 v6 37-42 v7 44-49 v8 51-56 ///
	v9 58-63 v10 65-70 v11 72-77 v12 79-84 v13 86-91 v14 93-98 v15 100-105 v16 107-112 ///
	v17 114-119 v18 121-126 v19 128-133 v20 135-140 v21 142-147 v22 149-154 ///
	v23 156-161 v24 163-168	using "C:\AMSSD\Datasets\Chapter2\archer.txt", clear
	
*Convert to symmetric matrix
mkmat v1-v24, matrix(S)
mata: st_replacematrix("S",makesymmetric(st_matrix("S")))

*Compute dissimilarity matrix (21 - S)
matrix A=J(24,24,21)
matrix D=A-S

*Code below reproduces: Figure 2.16
*Cluster analysis using furthest neighbour (complete linkage)
clustermat completelinkage D, shape(full) name(clink) clear
cluster dendrogram clink, title("Furthest neighbour cluster analysis of Persian Archers data")

