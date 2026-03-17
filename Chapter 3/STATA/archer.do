/*  
Chapter 3: Multidimensional Scaling (MDS)
Persian Archers Example (Section 3.7)
Change appropriate path for file "archer.txt"
*/ 

*Read text file with 24x24 similarity matrix (lower triangular) into 24 variables
infix v1 2-7 v2 9-14 v3 16-21 v4 23-28 v5 30-35 v6 37-42 v7 44-49 v8 51-56 ///
	v9 58-63 v10 65-70 v11 72-77 v12 79-84 v13 86-91 v14 93-98 v15 100-105 v16 107-112 ///
	v17 114-119 v18 121-126 v19 128-133 v20 135-140 v21 142-147 v22 149-154 ///
	v23 156-161 v24 163-168	using "C:\AMSSD\Datasets\Chapter3\archer.txt", clear
	
*Convert to symmetric matrix
mkmat v1-v24, matrix(S)
mata: st_replacematrix("S",makesymmetric(st_matrix("S")))

*Rescale so that similarities lie in [0,1] with 1 on diagonal
matrix S=S/21

*Non-metric (ordinal) MDS of similarity matrix
mdsmat S, s2d(st) loss(stress) trans(monotonic) config
* Code above reproduces: Figures 3.14 and 3.15 (rotated).