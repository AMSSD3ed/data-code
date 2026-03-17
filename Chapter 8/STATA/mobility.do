* Chapter 8: Factor Analysis for Binary Data
* Women's Mobility Data Example (Bangladesh Fertility Survey 1989)
* Change to appropriate path for file "mobility.dat".

/* USE THE COMMAND GSEM AVAILABLE IN STATA13
Reading the data */
infile y1 y2 y3 y4 y5 y6 y7 y8 using "C:\AMSSD\Datasets\Chapter8\mobility.dat", clear
summarize

/*Fitting one-factor model to all six items*/
gsem (Mobility-> y1-y8, logit), var(Mobility@1)

/*Fitting a two-factor model */
* Code below reproduces:
* Table 8.24: Estimated difficulty and discrimination parameters with SEs 
* and standardized loadings for the two-factor model, Women's Mobility data
gsem (MobAtt1 -> y1-y8) (MobAtt2 ->y1-y8), logit var(MobAtt1@1 MobAtt2@1) ///
cov(MobAtt1*MobAtt2@0) intmethod(ghermite) intpoints(15) startgrid()