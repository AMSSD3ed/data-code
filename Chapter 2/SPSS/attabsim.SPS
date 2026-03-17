* Encoding: UTF-8.
* Chapter 2: Cluster Analysis.
* Attitude to Abortion Example.
* Replace with appropriate path for file "attabsim.txt".

MATRIX DATA FILE = 'C:\AMSSD\Datasets\Chapter2\attabsim.txt'
/VARIABLES = P1 TO P16 /FORMAT= FULL  /CONTENTS = PROX.
VALUE LABELS ROWTYPE_ 'PROX' 'SIMILARITY'. 

/* Code below reproduces: Figures 2.18 and 2.19. */
CLUSTER
/MATRIX IN (*)
/METHOD COMPLETE
/PRINT SCHEDULE
/PLOT DENDROGRAM.
