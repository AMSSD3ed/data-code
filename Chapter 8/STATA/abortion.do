/*Chapter 8: Factor Analysis for Binary data
Attitude towards to Abortion Example (1986 British Social Attitudes Survey)
*/

/* 1. USE GLAMM
To fit factor analysis models for binary data in STATA you need to install GLLAMM
The first time you run gllamm you need to install the package using the commands:*/
ssc describe gllamm
ssc install gllamm, replace

/* Details on the options provided after each command can be found on the GLLAMM manual
Import the data into STATA (the data are in txt format)
The path in the command below needs to be changed to the one that your file is located*/
infile i1 i2 i3 i4 wt2 using "C:\AMSSD\Datasets\Chapter8\abortion.txt", clear

* Table 8.1: Frequencies of response patterns, Attitudes towards abortion example
* We list first 14 rows (note that patterns 1010 and 1001 have no responses)
list in 1/14, clean

/* Data must be in long form, with all y_ij for persons j and items i in one variable. 
The data also require person and item identifiers and item indicator (or dummy) variables. Those are done with the commands below:*/
gen pattern=_n
reshape long i, i(pattern) j(item)
rename i score
list in 1/8, clean
tab item, gen(d)

/*Fitting the one-factor model to four binary items. Identifying the scale of the latent variable by fixing one loading equal to 1.*/
eq loading: d1-d4
gllamm score d1-d4, i(pattern) eqs(loading) link(logit) family(binom) weight(wt) nip(15) nocons adapt trace

/*Identifying the scale of the latent variable by setting its variance equal to 1.
The constraint command below fixes the variance of the latent variable to 1 and estimates the factor loadings for all items*/
* The code below reproduces parts the following output:
* Table 8.3: Parameter estimates and SEs for the one-factor model, Attitude to Abortion data
constraint def 1 [pat1_1]d1 = 1
gllamm score d1-d4, i(pattern) eqs(loading) link(logit) family(binom) weight(wt) nip(15) nocons constr(1) frload(1) adapt trace

/*Scoring: posterior means and their corresponding estimated standard deviations*/
* The code below reproduces parts of the following output:
* Table 8.6: Factor scores listed increasing order, Attitude to Abortion data
gllapred fscores, u
/*End of Analysis with GLAMM*/


