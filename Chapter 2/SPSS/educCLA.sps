* Encoding: UTF-8.
* Chapter 2: Cluster Analysis
/* Educational Circumstances Example */
/* Replace with appropriate path for file "educ.txt" */.

MATRIX DATA FILE = 'C:\AMSSD\Datasets\Chapter2\educ.txt'
/VARIABLES = x1 TO x9 /FORMAT = FULL /CONTENTS = PROX.

VALUE LABELS ROWTYPE_ 'PROX' 'SIMILARITY'.
CLUSTER
/MATRIX IN (*)
/METHOD SINGLE
/PLOT DENDROGRAM VICICLE.

/* Code below reproduces: Figure 2.20. */ 
VALUE LABELS ROWTYPE_ 'PROX' 'SIMILARITY'.
CLUSTER
/MATRIX IN (*)
/METHOD COMPLETE
/PLOT DENDROGRAM VICICLE.
