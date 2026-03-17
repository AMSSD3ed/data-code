/*  
Chapter 3: Multidimensional Scaling (MDS)
Dimensions of Colour Vision Example (Section 3.6)
Change appropriate path for file "colour.txt"
*/ 

*Read text file with 14x14 similarity matrix (full) into 14 variables
infile c1-c14 using "C:\AMSSD\Datasets\Chapter3\colour.txt", clear

*Convert to symmetric matrix
mkmat c1-c14, matrix(S)
mata: st_replacematrix("S",makesymmetric(st_matrix("S")))

*Non-metric (ordinal) MDS of similarity matrix
mdsmat S, s2d(st) loss(stress) trans(monotonic) config
* Code above reproduces: Figure 3.6