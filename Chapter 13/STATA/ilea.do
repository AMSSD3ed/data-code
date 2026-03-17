* Chapter 13: Multilevel Models
* Inner London Education Authority (ILEA) Example
* Change to appropriate path for file "ilea.txt".

*Read text file with data on 114 London secondary schools
infile student school exam16 vrband11 female schgend schdenom using "C:\AMSSD\Datasets\Chapter13\ilea.txt", clear

*Sort by school
sort school

*Dummies for verbal reasoning band
tabulate vrband11, gen(vr)

*Random intercept model with verbal reasoning band dummies
* Code below reproduces parts of:
* Table 13.13: Random Intercept Model fitted to the ILEA data
mixed exam16 vr2 vr3 || school:
