* Chapter 6: Regression Analysis
* GCSE English Scores Data Example 
* Change to appropriate path for file "girls.txt".

*Read text file with data on girls' test scores
infile gcse words joins catsvs catsnvs using "C:\AMSSD\Datasets\Chapter6\girls.txt", clear

* For example on Section 6.14 (GCSE scores for boys), un-comment below:
* infile gcse words joins catsvs catsnvs using "C:\AMSSD\Datasets\Chapter6\boys.txt", clear

sum

*Divide writing speed (words) by 100 to obtain speed in words per 10th of a minute
gen speed=words/100

*Simple linear regression of GCSE on writing speed (Section 6.2)
* Table 6.1: ANOVA for simple linear regression of GCSE on SPEED.
reg gcse speed

*Scatter plot of GCSE vs speed with regression line 
* Figure 6.2: Scatterplot of GCSE English score (GCSE) vs writing speed (SPEED)
twoway (scatter gcse speed) (lfit gcse speed)

*Multiple regression of GCSE on speed and catsvs (Section 6.8)
*Table 6.2: Summary of regression of GCSE on SPEED and CATsVS.
*Table 6.3: ANOVA for multiple linear regression (MLR) of GCSE on SPEED and CATsVS. 
reg gcse speed catsvs

*Multiple regression of GCSE on SPEED, CATsVS, CATsNVS and joins (Section 6.10)
*Table 6.4: Summary of MLR of GCSE on 4 and 3 variables
reg gcse speed catsvs joins
reg gcse speed catsvs catsnvs joins

*Multiple regression of GCSE on SPEED, joins and interaction speedXjoins (Section 6.11)
*Table 6.5: Summary of regression of GCSE on SPEED and JOINS with interaction
gen speedXjoin=speed*joins
reg gcse speed joins speedXjoin

*Logistic regression (Section 6.14)
*Create binary gcse variable (=1 if A or A*, i.e. gcse>=8)
gen gcsebin=(gcse>=8)
logit gcsebin speed catsvs

*Path analysis (Section 6.15)
sem (gcse <- speed catsvs) (speed <- catsvs)
