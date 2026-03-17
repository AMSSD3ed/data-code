/*  
Chapter 3: Multidimensional Scaling (MDS)
Economic and Demographic Indicators - 1997 Example (Section 3.7)
Change appropriate path for file "econ.txt"
*/ 

*Read text file with data for 25 countries
infile str11 country growth life imr tfr gdp using "C:\AMSSD\Datasets\Chapter3\econ.txt", clear

*1-dimensional classicial metric MDS (Euclidean distances are default)
*Ratio MDS not available
mds growth-gdp, id(country) std method(classical) dimension(1) config

*List stress
estat stress

*save coordinates and sort by them
predict d1
sort d1
list country d1

*coordinates are roughly half those in Table 3.6 (for ratio MDS)
replace d1=d1/2
list country d1

*2-dimensional classical metric MDS 
mds growth-gdp, id(country) std method(classical) dimension(2) config
* Code above reproduces: Figure 3.12

estat stress
