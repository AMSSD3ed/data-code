/*Chapter 8: Factor Analysis for Binary data
Contemporary Sexual Attitudes Example (1990 British Social Attitudes Survey)
*/

/* 1. USE GLLAMM
To fit factor analysis models for binary data in STATA you need to install GLLAMM
The first time you run gllamm you need to install the package using the commands:*/
ssc describe gllamm
ssc install gllamm, replace

/* Details on the options provided after each command can be found on the GLLAMM manual
Import the data into STATA (the data are in txt format)*/
infile i1 i2 i3 i4 i5 i6 i7 i8 i9 i10 using "C:\AMSSD\Datasets\Chapter8\sexualat.txt", clear

* We list the first 6 rows (observations)
list in 1/6, clean

/* Data must be in long form, with all y_ij for persons j and items i in one variable. 
The data also require person and item identifiers and item indicator (or dummy) variables. Those are done with the commands below:*/
gen wt2=1
gen pattern=_n
reshape long i, i(pattern) j(item)
rename i score
tab item, gen(d)

/*Fitting the one-factor model to ten binary items. Identifying the scale of the latent variable by fixing one loading equal to 1.*/
eq loading: d1-d10
gllamm score d1-d10, i(pattern) eqs(loading) link(logit) family(binom) weight(wt) nip(10) nocons adapt trace

/*Identifying the scale of the latent variable by setting its variance equal to 1.
The constraint command below fixes the variance of the latent variable to 1 and estimates the factor loadings for all ten items*/
constraint def 1 [pat1_1]d1 = 1
gllamm score d1-d10, i(pattern) eqs(loading) link(logit) family(binom) weight(wt) nip(10) nocons constr(1) frload(1) trace adapt


/* 2. USE THE COMMAND GSEM AVAILABLE IN STATA13

Read the data first */
infile y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 using "C:\Users\CARDENCA\LSE Statistics Dropbox\Camilo Cárdenas\AMSSD\Datasets\Chapter8\sexualat.txt", clear

/*Fitting the one-factor model*/
* Code below reproduces:
* Table 8.9: Estimated difficulty and discrimination parameters with SEs 
* and standardized loadings for the one-factor model, Sexual Attitudes data
gsem (SexAt -> y1-y10, logit), var(SexAt@1) intmethod(mvaghermite) ///
 intpoints(75) startgrid()
 
/* Fitting a two-factor model */
* Code below reproduces:
* Table 8.12: Estimated difficulty and discrimination parameters with SEs 
* and standardized loadings for the two-factor model, Sexual Attitudes data
gsem (SexAt -> y1-y10, logit) (SexAt2 -> y1-y10, logit), ///
var(SexAt@1 SexAt2@1) cov(SexAt*SexAt2@0) ///
intmethod(ghermite) intpoints(75)

/*After estimation, we can obatin factor scores (posterior mean of the latent variable given the vector of responses)*/
predict attitude1, latent(SexAt)
predict attitude2, latent(SexAt2)

/*After estimation, we can obtain the predicted probabilities of a correct answer by typing*/
predict pr*, pr

/*To draw the item characteristic curves for all five questions type*/
twoway line pr1 pr2 pr3 pr4 pr5 pr6 pr7 pr8 pr9 pr10 attitude, sort xlabel(-1.5(.5)1.5)

