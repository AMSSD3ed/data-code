* Chapter 3: Multidimensional Scaling (MDS)
/* English Dialects Example. 
/* Change to appropriate path for file "dialect.txt" and temporary files "temp.sav", "dialect_d.sav", and "dialect_dhat.sav".


MATRIX DATA FILE = 'C:\AMSSD\Datasets\Chapter3\dialect.txt'
/VARIABLES = x1 TO x25 /FORMAT = LOWER /CONTENTS = PROX.

*2-dimensional ordinal (non-metric) scaling.
*2-dimensional co-ordinates for each village are saved in temp.sav.
*distances between villages in the 2-dimensional configuration (d's) are saved in. 
*dialect_d.sav.
*'smoothed' distances (d-hat's) are saved in dialect_dhat.sav.

PROXSCAL
  VARIABLES=x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18
  x19 x20 x21 x22 x23 x24 x25
  /SHAPE=LOWER
  /INITIAL=TORGERSON
  /TRANSFORMATION=ORDINAL (KEEPTIES)
  /PROXIMITIES=SIMILARITIES
  /ACCELERATION=NONE
  /CRITERIA=DIMENSIONS(2,2) MAXITER(100) DIFFSTRESS(.0001) MINSTRESS(.0001)
  /PRINT=COMMON STRESS
  /OUTFILE=COMMON( 'C:AMSSD\Chapter 3\SPSS\temp.sav' )
  DISTANCES( 'C:\AMSSD\Chapter 3\SPSS\dialect_d.sav' )
  TRANSFORMATION( 'C:\AMSSD\Chapter 3\SPSS\dialect_dhat.sav' )
  /PLOT=COMMON .
* Code above reproduces: Figures 3.16 and 3.17 (rotated).
