* Chapter 9: Factor Analysis for Binary Data
* Attitudes to Science and Technology Example (1992 Eurobarometer Survey from GB)
* Change to appropriate path for file "scien7i.dat".

/* USING THE COMMAND GSEM AVAILABLE IN STATA13
Reading the data, change the path below according to where you store your data file */
infile y1 y2 y3 y4 y5 y6 y7 using "C:\AMSSD\Datasets\Chapter9\scien7i.dat",clear
summarize

* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* One-factor model (ordinal): Attitudes to Science & Technology Example (4 items)
* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

/*Fitting one-factor model for ordinal items 4 items*/
* Code below reproduces parts of:
* Table 9.1: Estimated factor loadings with SEs
* for the one-factor model for ordinal data, Science & Technology Data (4 items) 
gsem (ScienceAtt-> y1 y3 y4 y7, ologit), var(ScienceAtt@1)

/*After estimation, we can obtain factor scores (posterior mean of the latent variable given the vector of responses)*/
predict attitude, latent(ScienceAtt)

* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* Two-factor model (ordinal): Attitudes to Science & Technology Example (7 items)
* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

/*Fitting a two-factor model */
gsem (ScienceAtt1 ScienceAtt2 -> y1-y7, ologit), var(ScienceAtt1@1 ScienceAtt2@1) /// 
cov(ScienceAtt1*ScienceAtt2@0) intmethod(ghermite)

/*The solution obtained for the two-factor model is subject to rotation and cannot be directly compared with the one provided in our book.
To allow for a comparison, we fix the loading of item 2 of the first factor equal to the corresponding value obtained from GENLAT.
The two solutions now look very close*/

* Code below reproduces parts of:
* Table 9.9: Estimated intercepts and factor loadings (with SEs)
* for the two-factor model for ordinal data, Science & Technology Data (7 items) 
* (Up to column permutation and sign)
gsem (ScienceAtt1 -> y1 y2@0.09 y3-y7) (ScienceAtt2 ->y1-y7), ologit ///
var(ScienceAtt1@1 ScienceAtt2@1) cov(ScienceAtt1*ScienceAtt2@0) ///
intmethod(ghermite)
