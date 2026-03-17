* Chapter 13: Multilevel Models
* Science test scores in Hungary
* Change to appropriate path for file "hungary.txt".

* Read text file with multivariate data on students in schools
infile school student es_core biol_core phys_core biol_r es_core_st biol_core_st phys_core_st biol_r_st female using "C:\AMSSD\Datasets\Chapter13\hungary.txt", clear

* Drop raw scores as only standardised scores will be used in analysis
drop es_core-biol_r	

* Recode numeric missing values to system missing
replace biol_r_st=. if biol_r_st==99
	
	
*****************************************************************************************
* Simple multivariate model fitted to standardised scores, ignoring schools (Section 13.7)
*****************************************************************************************
	
* First reshape into long form with scores in a single vector

rename es_core_st y1
rename biol_core_st y2
rename phys_core_st y3
rename biol_r_st y4

reshape long y, i(student) j(resp)

*Set up dummy variables to indicate response
tabulate resp, gen(r)

* Code below reproduces parts of:
* Table 13.6: Multivariate estimates of pairwise correlations (and SEs) between
* student's science test scores, ignoring differences between schools
mixed y r1-r4, nocons || student: , nocons residuals(unstructured, t(resp))

*Adding gender effects 

gen r1fem = r1*female
gen r2fem = r2*female
gen r3fem = r3*female
gen r4fem = r4*female

* Code below reproduces parts of:
* Table 13.7: Estimates from a multivariate model with gender effects for 
* correlations between student's science test scores,
* ignoring differences between schools
mixed y r1-r4 r1fem-r4fem, nocons || student: , nocons residuals(unstructured, t(resp))


**********************************************************************************
* Multilevel multivariate model fitted to standardised scores, with school effects
* (Section 13.7)
**********************************************************************************

* Code below reproduces parts of:
* Table 13.8: Estimates from a multivariate model with gender effects
mixed y r1-r4 r1fem-r4fem, nocons || school: r1-r4, nocons cov(unstructured) ///
	|| student: , nocons residuals(unstructured, t(resp))

	
********************************************
*Multilevel factor analysis (Section 13.8)
********************************************

*Return to original dataset in wide form

drop r1-r4fem
reshape wide y, i(student) j(resp)
sort school student

* Single-level factor analysis (constraining factor variance to 1)
* Note that estimates differ from those in the book because we use MLE here rather than MCMC

* Code below reproduces parts of:
* Table 13.10: Estimates from single-level factor model
sem (F1 -> y1-y4), var(F1@1)

*Multilevel factor analysis (constraining variance of factors at each level to 1)

* NOTE: The following specification allows for response-specific school
* random effects in addition to the school-level factor (as presented in the book).
* However, estimation TAKES LONG ('refining starting values' stage after 1 hour).
* Run at your own convenience

* Code below reproduces parts of:
* Table 13.11: Estimates from multilevel-level factor model
/*
gsem (F1 F2[school] U1[school] -> y1) ///
	(F1 F2[school] U2[school] -> y2) ///
	(F1 F2[school] U3[school] -> y3) ///
	(F1 F2[school] U4[school] -> y4), var(F1@1) var(F2[school]@1)
*/
	
*Simpler model without school-student partitioning of uniquenesses
gsem (F1 F2[school] -> y1-y4), var(F1@1) var(F2[school]@1)
