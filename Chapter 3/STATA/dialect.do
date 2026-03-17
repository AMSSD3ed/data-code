/*  
Chapter 3: Multidimensional Scaling (MDS)
English Dialects Example (Section 3.7)
Change appropriate path for file "dialect.txt"
*/ 

*Read text file with 25x25 similarity matrix (lower triangular) into 25 variables
infix v1 1-3 v2 4-6 v3 7-9 v4 10-12 v5 13-15 v6 16-18 v7 19-21 v8 22-24 ///
	v9 25-27 v10 28-30 v11 31-33 v12 34-36 v13 37-39 v14 40-42 ///
	v15 43-45 v16 46-48 v17 49-51 v18 52-54 v19 55-57 v20 58-60 ///
	v21 61-63 v22 64-66 v23 67-69 v24 70-72 v25 73-75 ///
	using "C:\AMSSD\Datasets\Chapter3\dialect.txt", clear
	
*Convert to symmetric matrix
mkmat v1-v25, matrix(S)
mata: st_replacematrix("S",makesymmetric(st_matrix("S")))

*Rescale so that similarities lie in [0,1] with 1 on diagonal
matrix S=S/100

*Non-metric (ordinal) MDS of similarity matrix
mdsmat S, s2d(st) loss(stress) trans(monotonic) config
* Code above reproduces: Figures 3.16 and 3.17 (rotated)