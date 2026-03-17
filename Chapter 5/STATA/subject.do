* Chapter 5: Principal Component Analysis (PCA)
* Subject Marks Example 
* Change to appropriate path for file "subject.txt".

*Read text file with 6x6 correlation matrix (lower triangular) into 6 variables
infix v1 1-4 v2 6-9 v3 11-14 v4 16-19 v5 21-24 v6 26-29 using "C:\AMSSD\Datasets\Chapter5\subject.txt", clear
	
*Convert to symmetric matrix 
mkmat v1-v6, matrix(C)
mata: st_replacematrix("C",makesymmetric(st_matrix("C")))

*Name matrix rows and columns
matrix rownames C = gaelic english history arithmetic algebra geometry
matrix colnames C = gaelic english history arithmetic algebra geometry

* Table 5.2: Correlation matrix
matrix list C

/*
pca command
Loadings are raw coefficients a, not rescaled a*
*/
pcamat C, n(220)
* Table 5.3: Variance explained by each PC

* Figure 5.4: Scree plot
screeplot

* Table 5.4: Component loadings (rescaled coefficients a*)
estat loadings, cnorm(eigen)

* Table 5.8: Component score coefficients
estat loadings, cnorm(inveigen)

/*
factor command (with principal factor method)
Loadings are rescaled coefficients a*
*/

*Equivalently as factor analysis using 'principal factor' method
*Components loadings are rescaled by default
factormat C, n(220) pcf

* Figure 5.5: Loading plot
loadingplot, xlabel(-1(0.5)1) ylabel(-1(0.5)1) xline(0) yline(0)




