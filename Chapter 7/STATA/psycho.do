* Chapter 7: Factor Analysis
* Psychomotor Test Example

/* To read the correlation matrix from a file instead please consult the examples in STATA from chapter 5.*/
* The code below reproduces the following output:
* Table 7.14: Pairwise correlation between test anxiety items (x100).
matrix A=(1.00,0.75, 1.00,0.73, 0.85, 1.00,0.66, 0.85, 0.85, 1.00,0.64, 0.84, 0.83, 0.90, 1.00,0.57, 0.79, 0.79, 0.88, 0.90, 1.00,0.63, 0.77, 0.81, 0.86, 0.87, 0.85, 1.00,0.59, 0.79, 0.79, 0.85, 0.86, 0.86, 0.90, 1.00,0.28, 0.30, 0.30, 0.26, 0.22, 0.23, 0.23, 0.24, 1.00,0.51, 0.46, 0.45, 0.40, 0.37, 0.34, 0.36, 0.34, 0.63, 1.00,0.49, 0.40, 0.39, 0.36, 0.36, 0.29, 0.33, 0.30, 0.32, 0.54,  1.00,0.40, 0.45, 0.44, 0.44, 0.42, 0.39, 0.39, 0.36, 0.08, 0.22,  0.22, 1.00,0.08, 0.22, 0.27, 0.30, 0.30, 0.27, 0.33, 0.27, 0.09, 0.05, -0.05, 0.20, 1.00, 0.25, 0.32, 0.31, 0.28, 0.34, 0.37, 0.30, 0.32, 0.12, 0.24,  0.12, 0.20, 0.30,1.00)

/*Perform first a PCA on the correlation matrix*/
* The code below reproduces the following output:
* Table 7.15: Communalities from fitting a 2-factor mode, Psychomotor Test data [Note: the output reports the Uniqueness, which is 1-Communalities].
factormat A, n(197) shape(lower) pcf names(x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14)

/* Produce the Scree plot*/
* The code below reproduces the following output:
* Figure 7.4: Scree plot of eigenvalue vs. number of component from PCA, Psychomotor Test data.
screeplot 

/*Fit 2-factor model (Maximum Likelihood estimation).*/ 
factormat A, n(197) shape(lower) factors(2) ml names(x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14)          

/*Scatterplot of factor loadings*/
* The code below reproduces the following output:
* Figure 7.5: Plot of loadings from a 2-factor model, Psychomotor Test data.
loadingplot