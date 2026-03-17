* Chapter 6: Regression Analysis.
* Hedonism in different countries Example.
* Change to appropriate path for file "hedonism3.sav".

GET FILE = 'C:\AMSSD\Datasets\Chapter6\hedonism3.sav'.

COMPUTE D1 = (COUNTRY = "A").
COMPUTE D2 = (COUNTRY = "B").

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT HEDSCORE
  /METHOD=ENTER D1 D2.
