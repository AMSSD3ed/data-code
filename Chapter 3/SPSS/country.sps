/*
Chapter 3: Multidimensional Scaling (MDS)
Countries Example 
Change to appropriate path for file "country.txt" and temporary files "temp.sav" */ 

MATRIX DATA FILE = 'C:\AMSSD\Datasets\Chapter3\country.txt'
/VARIABLES = x1 TO x12 /FORMAT = LOWER /CONTENTS = PROX.

/* 2-dimensional ordinal (non-metrical) scaling.
2-dimensional co-ordinates for each country are stored in temp.sav.
See README_Ch3_AoMSSD3.pdf for countries corresponding to X1, ...., X12.*/

/* Change to appropriate path for file "country.txt" and temporary files "temp.sav"
Code below reproduces: Figure 3.3 */ 
PROXSCAL
  VARIABLES=x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12
  /SHAPE=LOWER
  /INITIAL=TORGERSON
  /TRANSFORMATION=ORDINAL (KEEPTIES)
  /PROXIMITIES=SIMILARITIES
  /ACCELERATION=NONE
  /CRITERIA=DIMENSIONS(2,2) MAXITER(100) DIFFSTRESS(.0001) MINSTRESS(.0001)
  /PRINT=COMMON STRESS
  /OUTFILE=COMMON( 'C:\AMSSD\Chapter 3\SPSS\temp_country.sav' )
  /PLOT=COMMON .


