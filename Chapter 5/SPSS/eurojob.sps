* Chapter 5: Principal Component Analysis (PCA)
* European Employment Data Example 
* Change to appropriate path for file "eurojob.sav".

get file='C:\AMSSD\Datasets\Chapter5\eurojob.sav'.

*Obtain first 3 components from PCA of correlation matrix.

FACTOR
  /VARIABLES agric mining manu power constr service finance social trans
 /MISSING LISTWISE 
 /ANALYSIS agric mining manu power constr service finance
  social trans
  /PRINT INITIAL CORRELATION EXTRACTION FSCORE
  /PLOT EIGEN
  /CRITERIA FACTORS(3) ITERATE(25)
  /EXTRACTION PC
  /ROTATION NOROTATE
  /METHOD=CORRELATION .
* Code above reproduces: Table 5.5, Table 5.6, Table 5.7, Figure 5.6


*Obtain first 2 components and plot of their loadings.
*Component scores are saved as variables in the data file.

FACTOR
  /VARIABLES agric mining manu power constr service finance social trans
 /MISSING LISTWISE 
 /ANALYSIS agric mining manu power constr service finance
  social trans
  /PRINT INITIAL CORRELATION EXTRACTION FSCORE
  /PLOT EIGEN ROTATION
  /CRITERIA FACTORS(2) ITERATE(25)
  /EXTRACTION PC
  /ROTATION NOROTATE
  /SAVE REG(ALL)
  /METHOD=CORRELATION .
* Code above reproduces: Figure 5.7, Table 5.9
