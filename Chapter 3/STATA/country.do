/*  
Chapter 3: Multidimensional Scaling (MDS)
Countries Example (Section 3.2)
Change appropriate path for file "country.txt"
*/ 

*Read text file with 12x12 similarity matrix (lower triangular) into 12 variables
infix v1 1-4 v2 7-10 v3 14-17 v4 21-24 v5 28-31 v6 35-38 ///
	v7 42-45 v8 49-52 v9 56-59 v10 63-66 v11 70-73 v12 77 ///
	using "C:\AMSSD\Datasets\Chapter3\country.txt", clear

*Convert to symmetric matrix
mkmat v1-v12, matrix(S)
mata: st_replacematrix("S",makesymmetric(st_matrix("S")))

*Rescale so that similarities lie in [0,1] with 1 on diagonal
matrix S=S/9

*Name matrix rows (for display on MDS plot)
matrix rownames S = Brazil Congo Cuba Egypt France India Israel Japan ///
	China Russia USA Yugo

*Non-metric (ordinal) MDS of similarity matrix
mdsmat S, s2d(st) loss(stress) trans(monotonic) config
* Code above reproduces: Unrotated Figure 3.3