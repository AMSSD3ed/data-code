* Chapter 5: Principal Component Analysis (PCA)
* Educational Circumstances Example 
* Change to appropriate path for file "educ.txt".

MATRIX DATA FILE = 'C:\AMSSD\Datasets\Chapter5\educ.txt'
VARIABLES = x1 TO x9 /FORMAT = FULL N = 398  /CONTENTS = CORR

*Obtain first 3 components.

FACTOR
  /MATRIX IN (COR=*)
  /MISSING LISTWISE
 /ANALYSIS x1 TO x9
  /PRINT INITIAL EXTRACTION
  /PLOT EIGEN ROTATION(1,2)(1,3)
  /CRITERIA FACTORS(3) ITERATE(25)
  /EXTRACTION PC
  /ROTATION NOROTATE
  /METHOD=CORRELATION .
*Code above reproduces: Figures 5.10 and 5.11
