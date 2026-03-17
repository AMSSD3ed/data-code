* Chapter 5: Principal Component Analysis (PCA)
* Social Mobility in the UK Example 
* Change to appropriate path for file "socmob.txt".

MATRIX DATA FILE = 'C:\AMSSD\Datasets\Chapter5\socmob.txt'
/VARIABLES = x1 TO x10 /FORMAT = FULL N = 713 /CONTENTS = CORR.

*Obtain first 6 components.

FACTOR
  /MATRIX IN(COR=*)
  /MISSING LISTWISE
  /ANALYSIS x1 x2 x3 x4 x5 x6 x7 x8 x9 x10
  /PRINT INITIAL CORRELATION  EXTRACTION
  /PLOT EIGEN
  /CRITERIA FACTORS(6) ITERATE(25)
  /EXTRACTION PC
  /ROTATION NOROTATE
  /METHOD=CORRELATION .
* Code above reproduces: Table 5.14 and Table 5.15
