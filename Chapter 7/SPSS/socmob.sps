* Chapter 7: Factor Analysis.
* Social Mobility Data Example.
* Change to appropriate path for file "socmob.txt".

MATRIX DATA FILE ='C:\AMSSD\Datasets\Chapter7\socmob.txt'
/VARIABLES = x1 TO x10 /FORMAT = FULL /CONTENTS = CORR /N = 713.

*3-factor unrotated solution.
* This code produces: 
* Table 7.16: Loading matrix giving the unrotated loadings from a 3-factor model, Social Mobility Data.
FACTOR
/MATRIX IN (COR = *)
/MISSING LISTWISE /ANALYSIS x1 x2 x3 x4 x5  x6 x7 x8 x9 x10
/PRINT EXTRACTION
/CRITERIA FACTORS(3) ITERATE(25)
/EXTRACTION ML
/ROTATION NOROTATE
/METHOD=CORRELATION .

*3-factor VARIMAX rotated solution.
* This code produces: 
* Table 7.17a: Loading matrix giving the VARIMAX loadings from a 3-factor model, Social Mobility Data.
FACTOR
/MATRIX IN (COR = *)
/MISSING LISTWISE /ANALYSIS x1 x2 x3 x4 x5 x6 x7 x8 x9 x10
/PRINT ROTATION
/CRITERIA FACTORS(3) ITERATE(25)
/EXTRACTION ML
/ROTATION VARIMAX
/METHOD=CORRELATION .

*3-factor OBLIMIN rotated solution.
* This code produces: 
* Table 7.17b: Loading matrix giving the OBLIMIN loadings from a 3-factor model, Social Mobility Data.
FACTOR
/MATRIX IN (COR = *)
/MISSING LISTWISE /ANALYSIS x1 x2 x3 x4 x5 x6 x7 x8 x9 x10
/PRINT ROTATION
/CRITERIA FACTORS(3) ITERATE(25)
/EXTRACTION ML
/ROTATION OBLIMIN
/METHOD=CORRELATION .

*4-factor unrotated solution.
*A 4-factor model gives better fit. Inspect the Chi-Squared test results. 
FACTOR
/MATRIX IN (COR = *)
/MISSING LISTWISE /ANALYSIS x1 x2 x3 x4 x5  x6 x7 x8 x9 x10
/PRINT INITIAL EXTRACTION
/CRITERIA FACTORS(4) ITERATE(25)
/EXTRACTION ML
/ROTATION NOROTATE
/METHOD=CORRELATION .





