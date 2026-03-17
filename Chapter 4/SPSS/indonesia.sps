* Encoding: UTF-8.
/* Chapter 4: Correspondence Analysis
/* Contraceptive method choice in Indonesia Example. 
/* Change to appropriate path for file "indonesia.sav".

GET FILE = 'C:\AMSSD\Datasets\Chapter4\indonesia.sav'.

WEIGHT  BY count .

*Produces 2-dimensional solution.

CORRESPONDENCE
  TABLE = method(1 8)  BY agegroup(1 7)
  /DIMENSIONS = 2
  /MEASURE = CHISQ
  /STANDARDIZE = RCMEAN
  /NORMALIZATION = SYMMETRICAL
  /PRINT = TABLE RPOINTS CPOINTS
  /PLOT = NDIM(1,MAX) BIPLOT(20) .
* Code above reproduces: Tables 4.21 to 4.23.
