* Chapter 7: Factor Analysis.
* Psychomotor Test Example.
* Change to appropriate path for file "psycho.txt".

MATRIX DATA FILE = 'C:\AMSSD\Datasets\Chapter7\psycho.txt'
/VARIABLES = x1 TO x14
/FORMAT = LOWER N = 197 /CONTENTS = CORR.

*2-factor model - unrotated solution. Code below produces:.
* Table 7.14: Pairwise correlation between test anxiety items (x100).
* Table 7.15: Communalities from fitting a 2-factor mode, Psychomotor Test data.
* Figure 7.4: Scree plot of eigenvalue vs. number of component from PCA, Psychomotor Test data.
* Figure 7.5: Plot of loadings from a 2-factor model, Psychomotor Test data.

FACTOR
/MATRIX IN (COR = *)
/MISSING LISTWISE 
/ANALYSIS x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14
/PRINT CORRELATION EXTRACTION 
/PLOT EIGEN ROTATION
/CRITERIA FACTORS(2)  ITERATE(25)
/EXTRACTION ML
/ROTATION NOROTATE
/METHOD=CORRELATION.

