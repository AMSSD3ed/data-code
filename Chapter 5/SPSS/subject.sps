* Chapter 5: Principal Component Analysis (PCA)
* Subject Marks Example 
* Change to appropriate path for file "subject.txt".

MATRIX DATA FILE = 'C:\AMSSD\Datasets\Chapter5\subject.txt'
/VARIABLES = gaelic english history arith algebra geo
/FORMAT = LOWER N = 220  /CONTENTS = CORR.

*Obtain all 6 principal components.

FACTOR
 /MATRIX IN(COR=*)
 /MISSING LISTWISE
 /ANALYSIS gaelic english history arith algebra geo
 /PRINT CORRELATION INITIAL EXTRACTION FSCORE
 /PLOT EIGEN ROTATION(1,2)
 /CRITERIA FACTORS(6) ITERATE(25)
 /EXTRACTION PC
 /ROTATION NOROTATE
 /METHOD=CORRELATION .
* Code above reproduces:
* Table 5.2: Pairwise correlation coefficients between subject marks
* Table 5.3: Variance explained by each Principal Component, Subject marks data
* Figure 5.4: Scree plot showing eigenvalue by number of PC, Subject marks data
* Figure 5.5: Plot of loadings for the first 2 components, Subject marks data

FACTOR
 /MATRIX IN(COR=*)
 /MISSING LISTWISE
 /ANALYSIS gaelic english history arith algebra geo
  /PRINT EXTRACTION
  /CRITERIA FACTORS(2) ITERATE(25)
  /EXTRACTION PC
  /ROTATION NOROTATE
  /METHOD=CORRELATION .
* Code above reproduces: Table 5.4: Loadings for the first two PC, Subject marks data
