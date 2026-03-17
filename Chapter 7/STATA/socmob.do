* Chapter 7: Factor Analysis
* Social Mobility Data Example

/* To read the correlation matrix from a file instead please consult the examples in STATA from Chapter 5.*/
matrix A=(1.000, 0.372,  1.000,0.231,  0.225,  1.000,0.100,  0.134,  0.534,  1.000,0.431,  0.375,  0.354,  0.235,  1.000,0.171,  0.149,  0.276,  0.234,  0.196,  1.000,0.128,  0.096,  0.281,  0.380,  0.138,  0.473,  1.000,0.175,  0.184,  0.318,  0.310,  0.231,  0.260,  0.208,  1.000,  0.077,  0.095,  0.247,  0.348,  0.109,  0.115,  0.191,  0.500,  1.000,  0.293,  0.283,  0.287,  0.215,  0.444,  0.189,  0.161,  0.438,  0.331,  1.000)

/*Fit a 3-factor model (Maximum Likelihood estimation).*/
* This code produces: 
* Table 7.16: Loading matrix giving the unrotated loadings from a 3-factor model, Social Mobility Data.
factormat A, n(713) shape(lower) factors(3) ml names(x1 x2 x3 x4 x5 x6 x7 x8 x9 x10)

/*Perform an orthogonal rotation: Varimax rotation*/
* This code produces: 
* Table 7.17a: Loading matrix giving the VARIMAX loadings from a 3-factor model, Social Mobility Data.
rotate, varimax factors(3)

/*Perform an oblique rotation: Oblimin rotation*/
* This code produces: 
* Table 7.17b: Loading matrix giving the OBLIMIN loadings from a 3-factor model, Social Mobility Data.
rotate, oblimin oblique

/* Correlation among factors (under oblique rotation the factors are allowed to be correlated)*/
estat common

/*Fit a 4-factor model (Maximum Likelihood estimation).*/
*A 4-factor model gives better fit. Inspect the Chi-Squared test results. 
factormat A, n(713) shape(lower) factors(4) ml names(x1 x2 x3 x4 x5 x6 x7 x8 x9 x10)
