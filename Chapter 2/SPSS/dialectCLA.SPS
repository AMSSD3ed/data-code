* Encoding: UTF-8.
* Chapter 2: Cluster Analysis.
* English Dialects Example.
* Replace with appropriate path for file "dialaect.txt".

MATRIX DATA FILE = 'C:\AMSSD\Datasets\Chapter2\dialect.txt'
/VARIABLES V1 TO V25 /FORMAT = LOWER /CONTENTS = PROX.
VALUE LABELS ROWTYPE_ 'PROX' 'SIMILARITY'.

* Code below reproduces:.
* Table 2.9: Similarity Matrix for the English dialect data.
* Table 2.10: Nearest Neighbour Agglomeration table, English dialect data.
* Figure 2.7: Dendrogram for nearest neighbour (single linkage) cluster analysis.
* for the English dialect data (distance = 100-similarity).
* Figure 2.8: Icicle plot for nearest neighbour clustering, English dialect data.
CLUSTER
/MATRIX IN (*)
/PRINT SCHEDULE DISTANCE    
/METHOD SINGLE
/PLOT DENDROGRAM VICICLE.

* Code below reproduces:.
* Figure 2.9: Dendrogram for farthest neighbour (complete linkage) cluster analysis.
* for the English dialect data (distance = 100-similarity).
CLUSTER
/MATRIX IN (*)
/METHOD COMPLETE
/PLOT DENDROGRAM.

