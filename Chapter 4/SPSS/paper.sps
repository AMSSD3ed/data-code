* Encoding: UTF-8.
/* Chapter 4: Correspondence Analysis
/* Newspaper Readership in the UK Example. 
/* Change to appropriate path for file "paper.sav".

GET FILE = 'C:\AMSSD\Datasets\Chapter4\paper.sav'.

WEIGHT  BY count .

*Produces 2-dimensional solution.

CORRESPONDENCE
  TABLE = occup(1 8)  BY paper(1 9)
  /DIMENSIONS = 2
  /MEASURE = CHISQ
  /STANDARDIZE = RCMEAN
  /NORMALIZATION = SYMMETRICAL
  /PRINT = TABLE RPOINTS CPOINTS
  /PLOT = NDIM(1,MAX) BIPLOT(20) .
* Code above reproduces: Tables 4.18 to 4.20, and Figure 4.9
