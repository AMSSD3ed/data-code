* Chapter 13: Multilevel Models
* Hedonism in different countries Data Example
* Change to appropriate path for file "hedonism.txt".

*Read text file with data on 20 EU countries
infile country ind hedscore age female income eduyrs using "C:\AMSSD\Datasets\Chapter13\hedonism.txt", clear

*Sort by country
sort country

*Recode numeric missing value codes to system missing
replace age=. if age==999
replace female=. if female==9
replace income=. if income==99
replace eduyrs=. if eduyrs==99

* Variance components model, i.e. with no predictors (Section 13.3)
* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

* Note that the level 2 variance estimate differs slightly from that given in the book
* Estimates in the book obtained using restricted iterative least squares (RIGLS) in MLwiN
* Can obtain very similar estimates in Stata using the reml (restricted maximum likelihood)
* option in place of the default mle (maximum likelihood estimation)
* RIGLS and REML are often used when the level 2 sample size is small

* Code below reproduces parts of:
* Table 13.1: MLM with country effects fitted to hedonism data
mixed hedscore || country: 

* Random intercept model with age (Section 13.4)
* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* Code below reproduces parts of:
* Table 13.2: Random Intercept Model with country and age
mixed hedscore age || country: 

* Random slope model with age (Section 13.5)
* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* Code below reproduces parts of:
* Table 13.3: Random Intercept and Slope Model with country and age effects
mixed hedscore age || country: age, cov(unstructured) 

* Code below reproduces:
* Figure 13.4: Predicted country lines from a random slope model
predict yhat, fitted
sort country age
twoway (line yhat age, connect(ascending)),
	    ytitle(Predicted Hedonism Score) ///
		xtitle(Age (years) centred at 46)

* Code below reproduces parts of:
* Figure 13.5: Estimated intercept and slope residuals for the relationship between
predict uhat*,  reffects relevel(country)
twoway scatter uhat1 uhat2, ytitle("u0j") xtitle("u1j")

* Within and Between Country Relationships between Hedonism and Income:
* Contextual Effects (Section 13.6)
* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

* Random intercept model with income
* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* Code below reproduces parts of:
* Table 13.4a: Random Intercept models with different specifications of the relationship
* between hedonism and income
mixed hedscore income || country: 

*Compute country-level mean income and add as predictor
* Code below reproduces parts of:
* Table 13.4b: Random Intercept models with different specifications of the relationship
* between hedonism and income
by country: egen incav=mean(income)
mixed hedscore income incav || country: 

* Code below reproduces parts of:
* Table 13.4c: Random Intercept models with different specifications of the relationship
* between hedonism and income
gen incdiff=income-incav
mixed hedscore incdiff incav || country: 

