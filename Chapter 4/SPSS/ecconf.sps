* Encoding: UTF-8.
/* Chapter 4: Correspondence Analysis
/* Confidence in purchasing from European Community Countries Example. 
/* Change to appropriate path for file "ecconf.sav".

GET FILE = 'C:\AMSSD\Datasets\Chapter4\ecconf.sav'.

WEIGHT  BY count .

*Produces 2-dimensional solution.

CORRESPONDENCE
  TABLE = country(1 15)  BY pattern(1 8)
  /DIMENSIONS = 2
  /MEASURE = CHISQ
  /STANDARDIZE = RCMEAN
  /NORMALIZATION = SYMMETRICAL
  /PRINT = TABLE RPOINTS CPOINTS
  /PLOT = NDIM(1,MAX) BIPLOT(20) .
* Code above reproduces: Tables 4.11 to 4.16, Figure 4.7

