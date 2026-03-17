/*Chapter 8: Factor Analysis for Binary data
WIRS: Workplace Industrial Relations Survey Example
*/

/* To fit factor analysis models for binary data in STATA you need to install GLLAMM
The first time you run gllamm you need to install the package using the commands:*/
ssc describe gllamm
ssc install gllamm, replace

/* Details on the options provided after each command can be found on the GLLAMM manual
 Import the data into STATA (the data are in txt format)*/
infile i1 i2 i3 i4 i5 i6 wt2 using "C:\AMSSD\Datasets\Chapter8\wirs.txt", clear

/* Data must be in long form, with all y_ij for persons j and items i in one variable. 
The data also require person and item identifiers and item indicator (or dummy) variables. Those are done with the commands below:*/
gen pattern=_n
reshape long i, i(pattern) j(item)
rename i score
list in 1/8, clean
tab item, gen(d)

/* One-factor model to six binary items. Identifying the scale of the latent variable by fixing one loading equal to 1.*/
eq loading: d1-d6
gllamm score d1-d6, i(pattern) eqs(loading) link(logit) family(binom) weight(wt) nip(15) nocons adapt trace

/*Identifying the scale of the latent variable by setting its variance equal to 1.
The constraint command below fixes the variance of the latent variable to 1 and estimates the factor loadings for all items*/
constraint def 1 [pat1_1]d1 = 1
gllamm score d1-d6, i(pattern) eqs(loading) link(logit) family(binom) weight(wt) nip(15) nocons constr(1) frload(1) adapt trace

/*Fitting the one-factor model to the five binary items, excluding item 1. Identifying the scale of the latent variable by fixing one loading equal to 1.*/
eq loading: d2-d6
gllamm score d2-d6, i(pattern) eqs(loading) link(logit) family(binom) weight(wt) nip(15) nocons adapt trace

/*Fit the same model but allow the latent variable to have variance 1 and mean 0.*/
* Code below reproduces:
* Table 8.21: Estimated difficulty and discrimination parameters with SEs
* for the one-factor model with item 1 omitted, WIRS data
constraint def 1 [pat1_1]d2 = 1
gllamm score d2-d6, i(pattern) eqs(loading) link(logit) family(binom) weight(wt) nip(15) nocons constr(1) frload(1) adapt trace

/* 2. USE THE COMMAND GSEM AVAILABLE IN STATA13
Read the data first */
infile y1 y2 y3 y4 y5 y6 using "C:\Users\CARDENCA\LSE Statistics Dropbox\Camilo Cárdenas\AMSSD\Datasets\Chapter8\wirs.dat", clear

/*Fitting the one-factor logit model*/
gsem (WiR -> y1-y6, logit), var(WiR@1) intmethod(ghermite) startgrid()
 
/* Fitting a two-factor model */
* Code below reproduces:
* Table 8.19: Estimated difficulty and discrimination parameters with SEs 
* and standardized loadings for the two-factor model, WIRS data
gsem (WiR -> y1-y6, logit) (WiR2 -> y1-y6, logit), ///
var(WiR@1 WiR2@1) cov(WiR*WiR2@0) ///
intmethod(ghermite) startgrid()

/*After estimation, we can obatin factor scores (posterior mean of the latent variable given the vector of responses)*/
predict wir1, latent(WiR)
predict wir2, latent(WiR2)
tab wir1

/*After estimation, we can obtain the predicted probabilities of a correct answer by typing*/
predict pr*, pr

* One-factor model (omitting item 1)
* Code below reproduces:
* Table 8.21: Estimated difficulty and discrimination parameters with SEs 
* and standardized loadings for the one-factor model with item 1 omitted, WIRS data
gsem (WiR -> y2-y6, logit), var(WiR@1) intmethod(ghermite) startgrid()
