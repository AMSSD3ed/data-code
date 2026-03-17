* Chapter 5: Principal Component Analysis (PCA)
* Television viewing in the UK Example 
* Change to appropriate path for file "tv.txt".

MATRIX DATA FILE = 'C:\AMSSD\Datasets\Chapter5\tv.txt'
 /VARIABLES = box thiswk today sport gndstnd lineup match panor rugby hrs24
 /FORMAT = FULL N = 7000 /CONTENTS = CORR

*Obtain first 2 components and plot of their loadings.

FACTOR
 /MATRIX IN(COR = *)
 /MISSING LISTWISE
  /PRINT INITIAL CORRELATION EXTRACTION
  /PLOT EIGEN ROTATION
  /CRITERIA FACTORS(2) ITERATE(25)
  /EXTRACTION PC
  /ROTATION NOROTATE
  /METHOD=CORRELATION .
* Code above reproduces:
* Table 5.16: Pairwise correlation between liking to watch ten TV programmes.
* Figure 5.12: Scree plot of eigenvalue vs. number of PC, TV viewing data
* Figure 5.13: Plot of factor loadings for first 2 PC, TV viewing data
