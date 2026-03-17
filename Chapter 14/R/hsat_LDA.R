# Chapter 14: Longitudinal Data Analysis
# Health satisfaction in Germany Example (German Socio-Economic Panel)

rm(list = ls())

# Install required packages if not already installed:
if(!all(c("lme4","nlme","performance") %in% installed.packages())){
	install.packages(c("lme4","nlme","performance"))
} 
library(lme4)
library(nlme)
library(performance)

# Health Satisfaction in Germany Data
# hsatDE.file <- "C:/AMSSD/Datasets/Chapter14/healthsat.csv"
# The data file path needs to be changed to the file's location on your computer

# Alternatively, choose the file "healthsat.csv" directly below:
hsatDE.file <- file.choose()
hsatDE.mat <- read.csv(file = hsatDE.file)

# Number of observations
nrow(hsatDE.mat)
# Number of individuals
length(unique(hsatDE.mat$id))

# Calculate quadratic age term for modelling
hsatDE.mat$age55sq <- hsatDE.mat$age55^2

# Latent Growth Curve Model (LGCM) with Random intercept
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
hsatDE.ri <- lme4::lmer(hsat ~ age55 + (1 | id), data=hsatDE.mat, REML=FALSE)
summary(hsatDE.ri)
performance::icc(hsatDE.ri)

# Latent Growth Curve Model (LGCM) with Random slope
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
hsatDE.rs <- lme4::lmer(hsat ~ age55 + (1 + age55 | id), data=hsatDE.mat, REML=FALSE)
summary(hsatDE.rs)
anova(hsatDE.rs, hsatDE.ri)

# LGCM with AR(1) residuals
# ~~~~~~~~~~~~~~~~~~~~~~~~~

# First refit RS model with independent residuals (for comparison)
# anova() function requires models to be fitted in same package
hsatDE.rs1 <- nlme::lme(hsat ~ age55, 
												random = ~ 1 + age55 | id, 
												data = hsatDE.mat, method="ML")
summary(hsatDE.rs1)

# Extend to AR(1) model
hsatDE.rsar1 <- nlme::lme(hsat ~ age55, 
										random = ~ 1 + age55 | id, 
										correlation=nlme::corAR1(form =~ 1 | id),
										data = hsatDE.mat, method="ML")
summary(hsatDE.rsar1)
anova(hsatDE.rsar1, hsatDE.rs1)

# rho = 0.071, but significant (p=0.006 from LR test)

# Next add AR(1) residuals to a Random Intercept model
# First re-fit Random Intercept model with independent residuals (for comparison)
# anova() function requires models to be fitted in same package
hsatDE.ri <- nlme::lme(hsat ~ age55, 
											 random = ~ 1 | id, 
											 data = hsatDE.mat, method="ML")
summary(hsatDE.ri)

# Random Intercept model with AR(1) residuals
hsatDE.riar1 <- nlme::lme(hsat ~ age55, 
													random = ~ 1 | id, 
													correlation = nlme::corAR1(form =~ 1 | id),
													data = hsatDE.mat, method="ML")
summary(hsatDE.riar1)
anova(hsatDE.riar1, hsatDE.ri)

# rho increases to 0.163
