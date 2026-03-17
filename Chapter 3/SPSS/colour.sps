/* Chapter 3: Multidimensional Scaling (MDS)
Colours Example. 
Change to appropriate path for file "colour.txt" and temporary files "temp.sav", "colour_d.sav", and "colour_dhat.sav". */

MATRIX DATA FILE = 'C:\AMSSD\Datasets\Chapter3\colour.txt'
/VARIABLES =colour1 TO colour14 /FORMAT = FULL /CONTENTS = PROX.

/*2-dimensional ordinal (non-metric) scaling.
2-dimensional co-ordinates for each colour saved in temp.sav.
distances between points in configuration (d's) saved in colour_d.sav.
'smoothed' distances (d-hat's) saved in colour_dhat.sav.*/

/* Code below reproduces: Figure 3.6 */
PROXSCAL
  VARIABLES=colour1 colour2 colour3 colour4 colour5 colour6 colour7 colour8 colour9
  colour10 colour11 colour12 colour13 colour14
  /SHAPE=LOWER
  /INITIAL=TORGERSON
  /TRANSFORMATION=ORDINAL (KEEPTIES)
  /PROXIMITIES=SIMILARITIES
  /ACCELERATION=NONE
  /CRITERIA=DIMENSIONS(2,2) MAXITER(100) DIFFSTRESS(.0001) MINSTRESS(.0001)
  /PRINT=COMMON STRESS
  /OUTFILE=COMMON( 'C:\AMSSD\Chapter 3\SPSS\temp.sav' )
  DISTANCES( 'C:\AMSSD\Chapter 3\SPSS\colour_d.sav' )
  TRANSFORMATI( 'C:\AMSSD\Chapter 3\SPSS\colour_dhat.sav' )
  /PLOT=COMMON .

*1-dimensional ordinal (non-metric) scaling.
/* Code below reproduces: Figure 3.10 */
PROXSCAL
  VARIABLES=colour1 colour2 colour3 colour4 colour5 colour6 colour7 colour8 colour9
  colour10 colour11 colour12 colour13 colour14
  /SHAPE=LOWER
  /INITIAL=TORGERSON
  /TRANSFORMATION=ORDINAL (KEEPTIES)
  /PROXIMITIES=SIMILARITIES
  /ACCELERATION=NONE
  /CRITERIA=DIMENSIONS(1,1) MAXITER(100) DIFFSTRESS(.0001) MINSTRESS(.0001)
  /PRINT=COMMON STRESS
  /OUTFILE=COMMON( 'C:\AMSSD\Chapter 3\SPSS\temp.sav')
  /PLOT=COMMON .
