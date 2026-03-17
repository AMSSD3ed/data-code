* Chapter 7: Factor Analysis.
* Subject Marks Example.
* Change to appropriate path for file "subject.txt".

MATRIX DATA FILE = 'C:\AMSSD\Datasets\Chapter7\subject.txt'
/VARIABLES = gaelic english history arith algebra geo
/FORMAT = LOWER N = 220 /CONTENTS = CORR. 

*Fit 2-factor model
Obtain unrotated solution
The code below produces the following outputs:
Table 7.2: Estimated factor loadings from a 2-factor model, Subjects marks data
Table 7.4: Communalities from a linear 2-factor model, Subjects marks data
Table 7.5: Reproduced correlations and communalities, and discrepancies between observed and fitted correlation matrix
Figure 7.2: Plot of unrotated factor loadings, Subjects marks data
Table 7.8: Coefficients for calculating factor scores (regression method), Subjects marks data.
FACTOR
/MATRIX IN (COR=*)
/MISSING LISTWISE
/ANALYSIS gaelic english history arith algebra geo
/PRINT EXTRACTION FSCORE REPR
/PLOT ROTATION
/CRITERIA FACTORS(2) ITERATE(25)
/EXTRACTION ML
/ROTATION NOROTATE
/METHOD=CORRELATION .

*Obtain OBLIMIN rotated solution.

FACTOR
/MATRIX IN (COR=*)
/MISSING LISTWISE
/ANALYSIS gaelic english history arith algebra geo
/PRINT EXTRACTION FSCORE REPR
/PLOT ROTATION
/CRITERIA FACTORS(2) ITERATE(25)
/EXTRACTION ML
/ROTATION OBLIMIN
/METHOD=CORRELATION .


