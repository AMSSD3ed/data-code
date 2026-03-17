* Chapter 9: Factor Analysis for Binary Data
* Attitudes to Environment Example
* Change to appropriate path for file "environ.dat".

/*USING THE COMMAND GSEM AVAILABLE IN STATA13
Reading the data, change the path according to where you store the data file */
infile y1 y2 y3 y4 y5 y6 using "C:\AMSSD\Datasets\Chapter9\environ.dat",clear
summarize 

* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* One-factor model (ordinal): Attitudes to Environment Example
* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

/*Fitting one-factor nominal model to all six items*/
* Code below reproduces parts of:
* Table 9.5: Estimated factor loadings with SEs and standardized loadings
* for the one-factor model for ordinal data, Environment Data
gsem (EnvironAtt-> y1-y6), ologit var(EnvironAtt@1)

* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* One-factor model (nominal): Attitudes to Environment Example
* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

/*Fitting one-factor nominal model to all six items*/
* Code below reproduces parts of:
* Table 9.13: Estimated difficulty and discrimination parameters
* for the one-factor model for nominal data, Environment Data
gsem (EnvironAtt-> y1-y6), mlogit var(EnvironAtt@1)
