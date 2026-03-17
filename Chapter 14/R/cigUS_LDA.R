# Chapter 14: Longitudinal Data Analysis
# Cigarette Smoking in the US Example (State level data for 1985-1995, 48 states)
# Data from Baum (2006) "An introduction to modern econometrics using Stata"

rm(list = ls())

# Install required packages if not already installed:
if(!all(c("lme4","dplyr","haven","ggplot2","lattice", "flexmix") %in% installed.packages())){
	install.packages(c("lme4","dplyr","haven","ggplot2","lattice", "flexmix"))
} 
library(lme4)
library(haven)
library(ggplot2)
library(lattice)
library(flexmix)

# Cigarette Smoking in the US Example
# smokeUS.file <- "C:/AMSSD/Datasets/Chapter14/cigUS.dta"
# The data file path needs to be changed to the file's location on your computer

# Alternatively, choose the file "cigUS.dta" directly below:
smokeUS.file <- file.choose()
smokeUS.mat <- haven::read_dta(file = smokeUS.file)

# Variables are: 
# - packpc - number of packs per capita (assume per year)
# - avgprs - average price in fiscal year, including sales taxes (assume cents)

# Input data (long format)
head(smokeUS.mat)
summary(smokeUS.mat)
sd(smokeUS.mat$packpc)

# Latent Growth Curve Model (LGCM) analysis
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# New time variable, centred at 1985 (first year)
smokeUS.mat$t <- smokeUS.mat$year-1985

# Code below reproduces:
# Table 14.9: Results from likelihood ratio tests in building a growth curve
# model of cigarette consumption in US states

# M1: Random intercept model
smokeUS.ri <- lme4::lmer(packpc ~ t + (1 | stateid), data = smokeUS.mat, REML = FALSE)
summary(smokeUS.ri)

# M2: Random slope model
smokeUS.rs <- lme4::lmer(packpc ~ t + (1 + t | stateid), data = smokeUS.mat, REML = FALSE)
summary(smokeUS.rs)

# M3: Random slope model with quadratic in year
smokeUS.mat$tsq <- smokeUS.mat$t^2
smokeUS.rsq1 <- lme4::lmer(packpc ~ t + tsq + (1 + t | stateid), data = smokeUS.mat, REML = FALSE)
summary(smokeUS.rsq1)

# M4: Add random slope on tsq
smokeUS.rsq2 <- lme4::lmer(packpc ~ t + tsq + (1 + t + tsq | stateid), data = smokeUS.mat, REML = FALSE,
               control = lme4::lmerControl(optimizer ="Nelder_Mead"))
summary(smokeUS.rsq2)

# Code below reproduces:
# Table 14.9: Results from likelihood ratio tests in building a growth curve
# model of cigarette consumption in US states
anova(smokeUS.ri, smokeUS.rs, smokeUS.rsq2,smokeUS.rsq1)

# Implied between-state variance for each year
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# First extract estimates of random part parameters
sigma.rsq2 <- as.data.frame(lme4::VarCorr(smokeUS.rsq2),comp="Variance")
sigma.rsq2
sig2u0 <- sigma.rsq2$vcov[1]
sig2u1 <- sigma.rsq2$vcov[2]
sig2u2 <- sigma.rsq2$vcov[3]
sigu01 <- sigma.rsq2$vcov[4]
sigu02 <- sigma.rsq2$vcov[5]
sigu12 <- sigma.rsq2$vcov[6]
sig2e <- sigma.rsq2$vcov[7]

# Calculate estimated between-state variance
year <- c(0:10)
ind.var <- sig2u0 + sig2u1*year^2 + sig2u2*year^4 +
  2*sigu01*year + 2*sigu02*year^2 + 2*sigu12*year^3
print(cbind(year, ind.var), digits=3, row.names=FALSE)

# Code below reproduces:
# Figure 14.7: Estimated between-state variance in number of packs of
# cigarettes per capita from quadratic random slope model (M4)
lattice::xyplot(ind.var ~ year, type=c("l","p"), col="black",
								scales=list(year=seq(0,10,1),tick.number=11,cex=0.7),
								xlab=list("Year",cex=0.8),
								ylab=list("Between-state variance",cex=0.8))


# Latent Class Growth Analysis (LCGA)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Set seed to ensure same class labels each time
set.seed(2304)

# 2-3 class quadratic models
smokeUS.lcgaq <- flexmix::stepFlexmix( packpc ~ t + tsq | stateid, data = smokeUS.mat, 
																			 k=2:3, nrep=20, 
																			 model = flexmix::FLXMRglmfix(family="gaussian", varFix=TRUE),
																			 control = list(iter.max = 500, minprior = 0))
smokeUS.lcgaq

smokeUS.lcgaq2 <- flexmix::getModel(smokeUS.lcgaq, which=1)
smokeUS.lcgaq3 <- flexmix::getModel(smokeUS.lcgaq, which=2)
summary(smokeUS.lcgaq2)
summary(smokeUS.lcgaq3)

# Parameter estimates for 2-class model (Section 14.9, page 460)
parameters(smokeUS.lcgaq2)
