* Chapter 3: Multidimensional Scaling (MDS).
/* Persian Archers Example. 
/* Change to appropriate path for file "acoustic.txt" and temporary files "temp.sav".

MATRIX DATA FILE = 'C:\AMSSD\Datasets\Chapter3\acoustic.txt'
/VARIABLES =w g c q p t b d e u v h f s x l j k m n a o i r y z
/FORMAT = FULL  /CONTENTS = PROX.

*2-dimensional ordinal (non-metric) scaling.
*2-dimensional co-ordinates for each letter saved in temp.sav.

PROXSCAL
  VARIABLES=w g c q p t b d e u v h f s x l j k m n a o i r y z
  /SHAPE=LOWER
  /INITIAL=TORGERSON
  /TRANSFORMATION=ORDINAL (KEEPTIES)
  /PROXIMITIES=SIMILARITIES
  /ACCELERATION=NONE
  /CRITERIA=DIMENSIONS(2,2) MAXITER(100) DIFFSTRESS(.0001) MINSTRESS(.0001)
  /PRINT=COMMON STRESS
  /OUTFILE=COMMON( 'C:\AMSSD\Chapter 3\SPSS\temp.sav' )
  /PLOT=COMMON .
* Code above reproduces: Figure 3.20

