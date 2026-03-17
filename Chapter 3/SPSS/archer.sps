* Chapter 3: Multidimensional Scaling (MDS).
/* Persian Archers Example. 
/* Change to appropriate path for file "archer.txt" and temporary files "temp.sav".

MATRIX DATA FILE = 'C:\AMSSD\Datasets\Chapter3\archer.txt'
/VARIABLES = x1 TO x24 /FORMAT = LOWER /CONTENT = PROX.

*2-dimensional ordinal (non-metric) scaling.
*2-dimensional co-ordinates for each archer saved in temp.sav.

PROXSCAL
  VARIABLES=x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18
  x19 x20 x21 x22 x23 x24
  /SHAPE=BOTH
  /INITIAL=TORGERSON
  /TRANSFORMATION=ORDINAL (KEEPTIES)
  /PROXIMITIES=SIMILARITIES
  /ACCELERATION=NONE
  /CRITERIA=DIMENSIONS(1,4) MAXITER(100) DIFFSTRESS(.0001) MINSTRESS(.0001)
  /PRINT=COMMON STRESS
  /OUTFILE=COMMON( 'C:\AMSSD\Chapter 3\SPSS\temp.sav' )
  /PLOT=COMMON STRESS.
* Code above reproduces: Figure 3.13 for stress scree-plot
* Code above reproduces: Figures 3.14 and 3.15 for dimension = 2
