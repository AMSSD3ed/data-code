/*Chapter 8: Factor Analysis for Binary data
LSAT: The Law School Admission Test (LSAT) - Section IV Example
*/

/*  1. USE GLLAMM
To fit factor analysis models for binary data in STATA you need to install GLLAMM
The first time you run gllamm you need to install the package using the commands:*/
ssc describe gllamm
ssc install gllamm, replace

/*Details on the options provided after each command can be found on the GLLAMM manual
Import the data into STATA (the data are in txt format)
The path in the command below needs to be changed to the one that your file is located*/
infile i1 i2 i3 i4 i5 using "C:\AMSSD\Datasets\Chapter8\LSAT.txt", clear

* Code below reproduces parts of:
* Table 8.13: Proportions of positive responses for observed items, LSAT data
sum 

/*print the first 6 rows*/
list in 1/6, clean
gen wt2=1

/* Data must be in long form, with all y_ij for persons j and items i in one variable. 
The data also require person and item identifiers and item indicator (or dummy) variables. Those are done with the commands below:*/
gen pattern=_n
reshape long i, i(pattern) j(item)
rename i score
list in 1/8, clean
tab item, gen(d)

/*Fitting the one-factor model to four binary items. Identifying the scale of the latent variable by fixing one loading equal to 1.*/
eq loading: d1-d5
gllamm score d1-d5, i(pattern) eqs(loading) link(logit) family(binom) weight(wt) nip(15) nocons adapt trace

/*Identifying the scale of the latent variable by setting its variance equal to 1.
The constraint command below fixes the variance of the latent variable to 1 and estimates the factor loadings for all items*/
* The code below reproduces parts the following output:
* Table 8.14: Estimated difficulty and discrimination parameters with SEs for the one-factor model, LSAT data
constraint def 1 [pat1_1]d1 = 1
gllamm score d1-d5, i(pattern) eqs(loading) link(logit) family(binom) weight(wt) nip(15) nocons constr(1) frload(1) adapt trace

/*Scoring: posterior means and their corresponding estimated standard deviations*/
* The code below reproduces parts of the following output:
* Table 8.15: Factor scores in increasing order, LSAT data
gllapred fscores, u 

/* 2. USE THE COMMAND GSEM AVAILABLE IN STATA13
The gsem command cannot read the weight variable. The data are now presented per individual */

infile y1 y2 y3 y4 y5 using "C:\AMSSD\Datasets\Chapter8\lsat.txt", clear
summarize
gsem (LawAbil -> y1-y5), logit var(LawAbil@1) 

/*The command estat ic reports the Akaike and Bayesian information criteria*/
estat ic

/*After estimation, we can obatin factor scores (posterior mean of the latent variable given the vector of responses)*/
predict ability, latent(LawAbil)

/*After estimation, we can obtain the predicted probabilities of a correct answer by typing*/
predict pr*, pr

/*To draw the item–characteristic curves for all five questions type*/
twoway line pr1 pr2 pr3 pr4 pr5 ability, sort xlabel(-1.5(.5)1.5)
