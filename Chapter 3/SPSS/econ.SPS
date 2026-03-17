/* Chapter 3: Multidimensional Scaling (MDS) */.
/* Economic and Demographic Indicators (1990) Example */. 
/* Change to appropriate path for data file "econ.sav" and temporary files "econmat.sav"*/. 

GET FILE = 'C:\AMSSD\Datasets\Chapter3\econ.sav'.

*The first stage is to calculate the distance matrix.  The following commands
*compute pairwise Euclidean distances between countries, based on standardized
*variables.  The distance matrix is saved as econmat.sav.

PROXIMITIES increase life imr tfr gdp   /PRINT NONE  
 /MATRIX OUT('C:\AMSSD\Chapter 3\SPSS\econmat.sav' )
  /MEASURE=EUCLID
  /STANDARDIZE=VARIABLE Z
  /VIEW=CASE .

*1-dimensional ratio scaling.
PROXSCAL
  /MATRIX=IN( 'C:\AMSSD\Chapter 3\SPSS\econmat.sav' )
  /INITIAL=TORGERSON
  /TRANSFORMATION=RATIO
  /ACCELERATION=NONE
  /CRITERIA=DIMENSIONS(1,1) MAXITER(100) DIFFSTRESS(.0001) MINSTRESS(.0001)
  /PRINT=COMMON STRESS
  /PLOT=COMMON.

/* Code below reproduces: Figure 3.12*/.
*2-dimensional ratio scaling.
PROXSCAL
  /MATRIX=IN( 'C:\AMSSD\Chapter 3\SPSS\econmat.sav' )
  /INITIAL=TORGERSON
  /TRANSFORMATION=RATIO
  /ACCELERATION=NONE
  /CRITERIA=DIMENSIONS(2,2) MAXITER(100) DIFFSTRESS(.0001) MINSTRESS(.0001)
  /PRINT=COMMON STRESS
  /PLOT=COMMON .
