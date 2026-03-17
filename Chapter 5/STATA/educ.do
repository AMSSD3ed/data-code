* Chapter 5: Principal Component Analysis (PCA)
* Educational Circumstances Example 
* Change to appropriate path for file "educ.txt".

*Read text file with 9x9 correlation matrix (full) into 9 variables
infile x1-x9 using "C:\AMSSD\Datasets\Chapter5\educ.txt", clear
	
*Convert to matrix
mkmat x1-x9, matrix(C)

*Display correlation matrix
matrix list C

*Name matrix rows and columns
matrix rownames C = x1 x2 x3 x4 x5 x6 x7 x8 x9
matrix colnames C = x1 x2 x3 x4 x5 x6 x7 x8 x9

/*
PCA using factor command (with principal factor method)
Loadings are rescaled coefficients a*
*/
pca x1-x9, comp(3)

*Equivalently as factor analysis using 'principal factor' method
*Components loadings are rescaled by default

*Extract first 3 components by setting minimum eigenvalue to just below 3rd eigenvalue
factormat C, n(398) pcf mineigen(0.95)

screeplot

* Figure 5.10: Pairwise loading plots for PCs 1 and 2
* Digure 5.11: Pairwise loading plots for PCs 1 and 3
loadingplot, factors(3) combined xlabel(-1(0.5)1) ylabel(-1(0.5)1) xline(0) yline(0)





