* Chapter 6: Regression Analysis
* Hedonism in different countries Example 
* Change to appropriate path for file "hedonism3.txt".

*Read text file with data on hedonism in 3 countries
*Countries are Austria (A), Belgium (B) and Czech Republic (C)
infile str1 country age female income edyrs hedscore using "C:\AMSSD\Datasets\Chapter6\hedonism3.txt", clear

*Recode numeric missing value codes to system missing
replace age=. if age==999
replace female=. if female==9
replace income=. if income==99
replace edyrs=. if edyrs==99

*Compute dummy variables for Austria (d1) and Belgium (d2)
gen d1 = (country=="A")
gen d2 = (country=="B")

*Fit linear regression with country dummies
reg hedscore d1 d2