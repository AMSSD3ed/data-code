* Chapter 7: Factor Analysis.
* Test Anxiety Inventory Example.
* Change to appropriate path for file "anxiety.txt".

MATRIX DATA FILE = 'C:\AMSSD\Datasets\Chapter7\anxiety.txt'
/VARIABLES = x1 TO x20 /FORMAT = LOWER N = 335 /CONTENTS = CORR.

*2-Factor unrotated solution. Code below produces:.
* Table 7.9: Pairwise correlation between test anxiety items.
* Table 7.10: Variance explained by each princupal component, test anxiety items.
* Table 7.11a: Factor loadings for the unrotated solution
* Figure 7.3: Plot of unrotated factor loadings.
FACTOR
/MATRIX IN (COR = *)
/MISSING LISTWISE
/ANALYSIS x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20
/PRINT INITIAL CORRELATION REPR EXTRACTION ROTATION
/PLOT ROTATION
/CRITERIA FACTORS(2) ITERATE(25)
/EXTRACTION ML
/ROTATION NOROTATE.


*2-factor OBLIMIN rotated solution. Code below produces:.
* Table 7.11b: Factor loadings for the OBLIMIN rotated solution (Pattern Matrix).
* Table 7.12: Structure matrix giving correlations between test anxiety items and the two ROTATED factors (via OBLIMIN). 
FACTOR
/MATRIX IN (COR = *)
/MISSING LISTWISE
/ANALYSIS x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20
/PRINT INITIAL CORRELATION  EXTRACTION ROTATION
/CRITERIA FACTORS(2) ITERATE(25) DELTA(0)
/EXTRACTION ML
/ROTATION OBLIMIN.


