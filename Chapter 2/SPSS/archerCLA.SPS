* Encoding: UTF-8.
/* Persian Archers example */
/* Replace with appropriate path for file "archer.txt" */

MATRIX DATA FILE = 'C:\AMSSD\Datasets\Chapter2\ARCHER.TXT'
/VARIABLES = A1 TO A24 /FORMAT = LOWER /CONTENTS=PROX.
VALUE LABELS ROWTYPE_ 'PROX = 'SIMILARITY'.

/* Code below reproduces: Figure 2.13*/
CLUSTER
/MATRIX IN(*)
/METHOD COMPLETE
/PRINT SCHEDULE
/PLOT DENDROGRAM.

