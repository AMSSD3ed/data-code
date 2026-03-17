/* Chapter 4: Correspondence Analysis.
/* Attitudes to Abortion Example. 
/* Change to appropriate path for file "abortion.sav".

GET FILE  = 'C:\AMSSD\Datasets\Chapter4\abortion.sav'.

*Produces 2-dimensional solution for crosstabulation of attitude by education.

WEIGHT BY count.

CORRESPONDENCE
  TABLE = educ(1 3)  BY attitude(1 3)
  /DIMENSIONS = 2
  /MEASURE = CHISQ
  /STANDARDIZE = RCMEAN
  /NORMALIZATION = SYMMETRICAL
  /PRINT = TABLE RPOINTS CPOINTS RPROFILES CPROFILES
  /PLOT = NDIM(1,MAX) BIPLOT(20) RPOINTS(20) CPOINTS(20) .
* Code above reproduces: Figures 4.2 and 4.3

*Creates interactive education/religion variable.

IF (educ=1 AND religion=1) edrel=1.
IF (educ=2 AND religion=1) edrel=2.
IF (educ=3 AND religion=1) edrel=3.
IF (educ=1 AND religion=2) edrel=4.
IF (educ=2 AND religion=2) edrel=5.
IF (educ=3 AND religion=2) edrel=6.
IF (educ=1 AND religion=3) edrel=7.
IF (educ=2 AND religion=3) edrel=8.
IF (educ=3 AND religion=3) edrel=9.
VALUE LABELS edrel 1 '<=8 NP' 2 '9-12 NP' 3 '>=13 NP'
  4 '<=8 SP' 5 '9-12 SP' 6 '>=13 SP'
  7 '<=8 C' 8 '9-12 C' 9 '>=13 C'.

* Section 4.8: CORA of Multi-way tables
*Produces 2-dimensional solution for cross-tabulation of attitude by education/religion.
CORRESPONDENCE
  TABLE = edrel(1 9)  BY attitude(1 3)
  /DIMENSIONS = 2
  /MEASURE = CHISQ
  /STANDARDIZE = RCMEAN
  /NORMALIZATION = SYMMETRICAL
  /PRINT = TABLE RPOINTS CPOINTS
  /PLOT = NDIM(1,MAX) BIPLOT(20) .
* Code above reproduces: Figure 4.8

