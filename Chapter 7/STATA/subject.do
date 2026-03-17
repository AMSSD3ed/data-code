* Chapter 7: Factor Analysis
* Subject Marks Data Example 

/* To read the correlation matrix from a file instead please consult the examples in STATA from Chapter 5.*/
matrix A=(1.00,0.44, 1.00,0.41, 0.35, 1.00,0.29, 0.35, 0.16, 1.00,0.33, 0.32, 0.19, 0.59, 1.00,0.25, 0.33, 0.18, 0.47, 0.46, 1.00)

/*Fit a 2-factor model (Maximum Likelihood estimation). Obtain unrotated solution
The code below produces the following outputs:
Table 7.2: Estimated factor loadings from a 2-factor model, Subjects marks data
*/
factormat A, n(220) shape(lower) factors(2) ml names(Gaelic English History Arithmetic Algebra Geometry)

/*Fit 2-factor model
Obtain unrotated solution
This code reproduces the following outputs:
Table 7.2: Estimated factor loadings from a 2-factor model, Subjects marks data
Table 7.4: Communalities from a linear 2-factor model, Subjects marks data [Note: the output reports the Uniqueness, which is 1-Communalities].*/

/*Scatterplot of factor loadings
This code reproduces the following outputs:
Figure 7.2: Plot of unrotated factor loadings, Subjects marks data */
loadingplot

/* Reproduced correlation and Residuals
This code reproduces the following outputs:
Table 7.5: Reproduced correlations and communalities, and discrepancies between observed and fitted correlation matrix. */
estat residuals, fit

/*Perform an orthogonal rotation: Varimax rotation*/
rotate, varimax factors(2)

/*Perform an oblique rotation: Oblimin rotation*/
rotate, oblimin

/* Correlation among factors (under oblique rotation the factors are allowed to be correlated)*/
estat common

/* Correlations between variables and common factors*/
estat structure

/*Scatterplot of rotated factor loadings*/
loadingplot
