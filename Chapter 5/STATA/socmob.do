* Chapter 5: Principal Component Analysis (PCA)
* Social Mobility in the UK Example 
* Change to appropriate path for file "socmob.txt".

*Read text file with 10x10 correlation matrix into 10 variables
infile x1-x10 using "C:\AMSSD\Datasets\Chapter5\socmob.txt", clear
	
*Convert to symmetric matrix 
mkmat x1-x10, matrix(C)

*Table 5.14: Correlation matrix
matrix list C

*Name matrix rows and columns
matrix rownames C = HF_0 WF_0 H_FE H_Q H_O W_FE W_Q FB_FE FB_Q FB_O
matrix colnames C = HF_0 WF_0 H_FE H_Q H_O W_FE W_Q FB_FE FB_Q FB_O

*Extract first 6 components
pcamat C, n(713) comp(6)

* Table 5.15: Component loadings rescaled coefficients a*
estat loadings, cnorm(eigen) 


