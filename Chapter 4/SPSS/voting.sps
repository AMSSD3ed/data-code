* Encoding: UTF-8.
/* Chapter 4: Correspondence Analysis
/* British General Election Study 1992 Example. 
/* Change to appropriate path for file "voting.sav".

GET FILE = 'C:\AMSSD\Datasets\Chapter4\voting.sav'.

WEIGHT BY count.

*Produces 2-dimensional solution.

CORRESPONDENCE
  TABLE = reason(1 3)  BY party(1 5)
  /DIMENSIONS = 2
  /MEASURE = CHISQ
  /STANDARDIZE = RCMEAN
  /NORMALIZATION = SYMMETRICAL
  /PRINT = TABLE RPOINTS CPOINTS
  /PLOT = NDIM(1,MAX) BIPLOT(20) .
* Code above reproduces: Figure 4.4
