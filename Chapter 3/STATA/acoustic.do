/*  
Chapter 3: Multidimensional Scaling (MDS)
Acoustic Confusion of Letters of the Alphabet Example (Section 3.7)
Change appropriate path for file "acoustic.txt"
*/ 

*Read text file with 26x26 similarity matrix (full) into 26 variables

infile w g c q p t b d e u v h f s x l j k m n a o i r y z using "C:\AMSSD\Datasets\Chapter3\acoustic.txt", clear
	
*Convert to symmetric matrix
mkmat w-z, matrix(S)
mata: st_replacematrix("S",makesymmetric(st_matrix("S")))

*Rescale so that similarities lie in [0,1] with 1 on diagonal
matrix S=S/999

*Name matrix rows (for display on MDS plot)
matrix rownames S = w g c q p t b d e u v h f s x l j k m n a o i r y z

*Non-metric (ordinal) MDS of similarity matrix
mdsmat S, s2d(st) loss(stress) trans(monotonic) config
* Code above reproduces: Figure 3.20 (reflected)