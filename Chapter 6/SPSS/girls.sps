* Chapter 6: Regression Analysis.
* GCSE scores Example.
* Change to appropriate path for file "girls.sav", and "boys.sav" for the example in Section 6.14.

GET FILE = 'C:\AMSSD\Datasets\Chapter6\girls.sav'.
* For example in Section 6.14 use the dataset in "boys.sav". Un-comment the lines below:.
* GET FILE = 'C:\AMSSD\Datasets\Chapter6\boys.sav'.

COMPUTE SPEED = WORDS/100.

* Simple linear regression of GCSE on SPEED (Section 6.2).
* Table 6.1: ANOVA for simple linear regression of GCSE on SPEED.
REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT GCSE
  /METHOD=ENTER SPEED .

* Figure 6.2: Scatterplot of GCSE English score (GCSE) against writing speed (SPEED) with LS regression line (add regression line using Chart Editor).
GRAPH
  /SCATTERPLOT(BIVAR) = SPEED WITH GCSE
  /MISSING=LISTWISE .

*Multiple regression of GCSE on SPEED and CATsVS (Section 6.8).
*Table 6.2: Summary of regerssion of GCSE on SPEED and CATsVS.
*Table 6.3: ANOVA for multiple linear regression (MLR) of GCSE on SPEED and CATsVS. 
REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT GCSE
  /METHOD=ENTER SPEED CATsVS.

* Multiple regression of GCSE on SPEED, CATsVS, CATsNVS and JOINS (Section 6.10).
* Table 6.4: Summary of MLR of GCSE on 4 and 3 variables.
* Regression with 4 covariate:.
REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT GCSE
  /METHOD=ENTER SPEED CATsVS CATsNVS JOINS.

* Regression with 3 covariates: (un-comment and run).
 * REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT GCSE
  /METHOD=ENTER SPEED CATsVS CATsNVS.

* Regression with 3 covariates: GCSE for Boys example in Section 6.14 (un-comment and run).
 * REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT GCSE
  /METHOD=ENTER SPEED CATsVS JOINS.

*MLR of GCSE on SPEED, JOINS and interaction SPEEDxJOINS (Section 6.11).
* Table 6.5: Summary of regression of GCSE on SPEED and JOINS with interaction.
COMPUTE SP_JOIN = SPEED*JOINS.
REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT GCSE
  /METHOD=ENTER SPEED JOINS SP_JOIN.

* Logit regression of GCSE on SPEED and CATsVS (Example in Section 6.14).
COMPUTE GCSEBIN = (GCSE >= 8).
LOGISTIC REGRESSION VARIABLES GCSEBIN WITH SPEED, CATsVS
/PRINT SUMMARY.

