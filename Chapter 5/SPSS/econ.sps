* Chapter 5: Principal Component Analysis (PCA)
* Economic and Demographic Data Example 
* Change to appropriate path for file "econ.sav".

GET FILE = 'C:\AMSSD\Datasets\Chapter5\econ.sav'.

*Obtain all 5 components from a PCA.

FACTOR
  /VARIABLES increase life imr tfr gdp 
  /MISSING LISTWISE
  /ANALYSIS increase life imr tfr gdp
  /PRINT INITIAL CORRELATION EXTRACTION
  /PLOT EIGEN
  /CRITERIA FACTORS(5) ITERATE(25)
  /EXTRACTION PC
  /ROTATION NOROTATE
  /METHOD=CORRELATION .
*Code above reproduces: Tables 5.10 and 5.11

*Obtain first 2 components and a plot of their loadings.
*Compute standardized component scores and store as variables in the data file.

FACTOR
  /VARIABLES increase life imr tfr gdp 
  /MISSING LISTWISE
  /ANALYSIS increase life imr tfr gdp
  /PRINT INITIAL CORRELATION EXTRACTION FSCORE
  /PLOT EIGEN ROTATION
  /CRITERIA FACTORS(2) ITERATE(25)
  /EXTRACTION PC
  /ROTATION NOROTATE
  /SAVE REG(ALL)
  /METHOD=CORRELATION .
*Code above reproduces: Figure 5.9
