* Chapter 5: Principal Component Analysis (PCA)
* European Employment Data Example 
* Change to appropriate path for file "eurojob.txt".

*Read raw data
infile str10 country x1 x2 x3 x4 x5 x6 x7 x8 x9 using "C:\AMSSD\Datasets\Chapter5\eurojob.txt", clear

label var x1 "Agriculture"
label var x2 "Mining"
label var x3 "Manufacture"
label var x4 "Power"
label var x5 "Construction"
label var x6 "Service"
label var x7 "Finance"
label var x8 "Social & personal"
label var x9 "Transport"
	
*Display correlation matrix (Table 5.5)
corr x1-x9

/*
pca command
Loadings are raw coefficients a, not rescaled a*
*/

pca x1-x9

*Scree plot (Fig 5.6)
screeplot

*Extract first 3 components (Table 5.6)
pca x1-x9, comp(3)

*Component loadings rescaled coefficients a*, (Table 5.7)
estat loadings, cnorm(eigen) 

*Component score coefficients (Table 5.9)
estat loadings, cnorm(inveigen)

/*
factor command (with principal factor method)
Loadings are rescaled coefficients a*
*/

*Equivalently as factor analysis using 'principal factor' method
*Components loadings are rescaled by default
factor x1-x9, pcf factor(3)

*Loading plot (Fig 5.7)
loadingplot, xlabel(-1(0.5)1) ylabel(-1(0.5)1) xline(0) yline(0)




