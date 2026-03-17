# Chapter 6: Regression Analysis
# GCSE Scores for Girls Example
# Change to appropriate path for file "girls.txt".

rm(list = ls())

# GCSE data for girls
# gcsegirls.file <- "C:/AMSSD/Datasets/Chapter6/girls.txt"
# The data file path needs to be changed to the file's location on your computer

# Alternatively, choose the file "girls.txt" directly below:
gcsegirls.file <- file.choose()

# GCSE data for boys
# For example on Section 6.14 (GCSE scores for boys), un-comment below:
# gcseboys.file <- "C:/AMSSD/Datasets/Chapter6/boys.txt"
# And repeat analysis with dataset:
# gcseboys.dat <- read.table(gcseboys.file)

gcsegirls.dat <- read.table(gcsegirls.file)
colnames(gcsegirls.dat) <- c("GCSE", "WORDS", "JOINS", "CATsVS","CATsNVS")
summary(gcsegirls.dat)
# Create the variable SPEED
gcsegirls.dat$SPEED <- gcsegirls.dat$WORDS/100

# Simple linear regression of GCSE on writing speed (Section 6.2)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

mod1 <- glm(GCSE ~ SPEED, data = gcsegirls.dat, family = gaussian)
summary(mod1) # display results
confint(mod1) # 95% CI for the coefficients

# Code below reproduces:
# Table 6.1: ANOVA for simple linear regression of GCSE on SPEED.
anova(mod1)

# Code below reproduces:
# Figure 6.2: scatter-plot of GCSE English score (GCSE) vs writing speed (SPEED)
attach(gcsegirls.dat)
plot(SPEED, GCSE, ylab = "GCSE score", xlab = "Writing speed (w/6s)")
abline(mod1)
detach(gcsegirls.dat)

# Multiple regression of GCSE on SPEED and CATsVS (Section 6.8)
# Code below reproduces:
# Table 6.2: Summary of regression of GCSE on SPEED and CATsVS.
mod2 <- glm(GCSE ~ SPEED + CATsVS, data = gcsegirls.dat, family = gaussian)
summary(mod2)

# Code below reproduces:
# Table 6.3: ANOVA for multiple linear regression (MLR) of GCSE on SPEED and CATsVS. 
anova(mod2)

# Multiple regression of GCSE on SPEED, CATsVS, CATsNVS and joins (Section 6.10)
# Code below reproduces:
# Table 6.4: Summary of MLR of GCSE on 4 and 3 variables
mod3a <- glm(GCSE ~ SPEED + CATsVS + JOINS, data = gcsegirls.dat, family = gaussian)
summary(mod3a)
mod3b <- glm(GCSE ~ SPEED + CATsVS + CATsNVS + JOINS, data = gcsegirls.dat, family = gaussian)
summary(mod3b)

# Multiple regression of GCSE on SPEED, joins and interaction speedXjoins (Section 6.11)
# Code below reproduces:
# Table 6.5: Summary of regression of GCSE on SPEED and JOINS with interaction
mod4 <- glm(GCSE ~ SPEED*JOINS, data = gcsegirls.dat, family = gaussian)
summary(mod4)

# Prediction, test error, cross-validation,
# and regularised estimation (Section 6.12)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

set.seed(1)

# Assign each observation in the sample to a fold
fold.vec <- rep(1:5, times = 22)[sample(1:110,110)]

# Cross-validation (CV) folds
which(fold.vec==1) #Fold 1
which(fold.vec==2) #Fold 2
which(fold.vec==3) #Fold 3
which(fold.vec==4) #Fold 4
which(fold.vec==5) #Fold 5

# Models to test
model1 <- GCSE~ WORDS
model2 <- GCSE~ WORDS + CATsVS
model3 <- GCSE~ WORDS + CATsVS + JOINS
model4 <- GCSE~ WORDS + CATsVS + CATsNVS + JOINS
model5 <- GCSE~ WORDS + JOINS + WORDS * JOINS

# Matrix to store MSE
MSE.matr <- matrix(0, 5, 5)

for(k in 1:5){
	train <- gcsegirls.dat[fold.vec!=k,]
	test <- gcsegirls.dat[fold.vec==k,]
	res1 <- lm(model1, data = train)
	res2 <- lm(model2, data = train)
	res3 <- lm(model3, data = train)
	res4 <- lm(model4, data = train)
	res5 <- lm(model5, data = train)
	
	pred1 = predict(res1,newdata = test)
	pred2 = predict(res2,newdata = test)
	pred3 = predict(res3,newdata = test)
	pred4 = predict(res4,newdata = test)
	pred5 = predict(res5,newdata = test)
	
	MSE.matr[1,k] = mean((pred1 - test$GCSE)^2)
	MSE.matr[2,k] = mean((pred2 - test$GCSE)^2)
	MSE.matr[3,k] = mean((pred3 - test$GCSE)^2)
	MSE.matr[4,k] = mean((pred4 - test$GCSE)^2)
	MSE.matr[5,k] = mean((pred5 - test$GCSE)^2)
}

# Code below reproduces:
# Table 6.6: Result of five-fold cross-validation, GCSE for girls data
data.frame(Model = paste0("Model ",1:5),
					 MSE_CV = round(rowMeans(MSE.matr),2))

# Logistic regression (Section 6.14)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Create binary GCSE variable (=1 if A or A*, i.e. gcse>=8)
gcsegirls.dat$GCSEbin <- ifelse(gcsegirls.dat$GCSE >= 8, 1, 0)
mod5 <- glm(GCSEbin ~ SPEED + CATsVS, data = gcsegirls.dat, family = binomial)
summary(mod5)

# Code below reproduces:
# Table 6.7: Fitted probabilities of an A or A*
newData <- expand.grid(SPEED = c(1,2,3), CATsVS = seq(2,8, by = 2))
newData$fitted <- predict(mod5, newdata = newData, type = "response")
round(xtabs(fitted ~ SPEED + CATsVS, newData), digits = 2)

# Path analysis (Section 6.15)
# The coefficients in Figure 6.8 can be obtained from two separate regressions:
mod6a <- glm(GCSE~SPEED+CATsVS, data = gcsegirls.dat, family=gaussian)
summary(mod6a)
mod6b <- glm(SPEED~CATsVS, data = gcsegirls.dat, family=gaussian)
summary(mod6b)
