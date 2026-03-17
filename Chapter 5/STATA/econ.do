* Chapter 5: Principal Component Analysis (PCA)
* Economic and Demographic Data for 25 Countries Example 
* Change to appropriate path for file "econ.txt".

*Read raw data
infile str11 country popinc life imr fertility gdp using "C:\AMSSD\Datasets\Chapter5\econ.txt", clear

label var popinc "Population increase"
label var life "Life expectancy"
label var imr "Infant mortality rate"
label var fertility "Fertility rate"
label var gdp "Gross domestic product"
	
*Display correlation matrix 
corr popinc-gdp

/*
pca command
Loadings are raw coefficients a, not rescaled a*
*/
pca popinc-gdp
* Table 5.10: Variance explained by each PC, Economic and Demographic Data

* Table 5.11: Component loadings (rescaled coefficients a*)
estat loadings, cnorm(eigen) 

* Figure 5.9: Plot of scores on first 2 PCs
scoreplot, mlabel(country)







