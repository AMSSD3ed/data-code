* Encoding: UTF-8.
/* Chapter 4: Correspondence Analysis
/* Leisure Activities Example. 
/* Change to appropriate path for file "leisure.sav".

GET FILE = 'C:\AMSSD\Datasets\Chapter4\leisure.sav'.

WEIGHT  BY count .

*Produces 2-dimensional solution.

CORRESPONDENCE
  TABLE = occup(1 6)  BY activity(1 10)
  /DIMENSIONS = 2
  /MEASURE = CHISQ
  /STANDARDIZE = RCMEAN
  /NORMALIZATION = SYMMETRICAL
  /PRINT = TABLE RPOINTS CPOINTS
  /PLOT = NDIM(1,MAX) BIPLOT(20) .
* Code above reproduces: Figure 4.5: Biplot for cross-tabulation of leisure activities, Survey Level of Living 1995, Norway.
