# Chapter 11: Confirmatory Factor Analysis and Structural Equation Models
# Ability Example (Scholastic Aptitude Test Data)

rm(list = ls())

# Install 'lavaan' package if not already installed:
if(!all(c("lavaan", "lavaanPlot") %in% installed.packages())) install.packages(c("lavaan", "lavaanPlot"))
library(lavaan)
library(lavaanPlot)

#Reading the covariance matrix
lower <- '
135.97 
89.21 130.94
94.15 96.42 147.69
94.36 98.54 112.11 137.10
127.47 96.03 104.27 100.72 211.26
97.90 83.74 88.67 91.27 107.89 129.98 
65.43 77.44 71.12 77.25 69.21 68.31 85.03
112.23 113.82 137.43 135.58 115.87 116.16 94.84 214.35
88.54 93.03 106.11 120.17 94.96 91.01 76.31 141.02 135.09 
100.70 79.67 85.93 87.23 128.99 99.87 68.39 110.71 88.00 154.27'

#Assigning names to the variables
ability.cov <- 
  lavaan::getCov(lower, names = c("math9","sci9","read9","scatv9","scatq9",
                                  "math7","sci7","read7","scatv7","scatq7"))

# CFA for items measured at the 7th grade
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ability.model.cfa7 <- '
  # latent variables
    quant7  =~ scatq7 + math7 + sci7
    verbal7 =~ scatv7 + sci7 + read7
'
ability.cfa1 <- lavaan::sem(ability.model.cfa7, 
                            sample.cov = ability.cov, 
                            sample.nobs = 383, estimator="ML")
summary(ability.cfa1)
# Chi-Squared statistic:
ability.cfa1@test$standard$stat
# P-value
ability.cfa1@test$standard$pvalue
# Correlation matrix of latent variables
cov2cor(ability.cfa1@Model@GLIST$psi)

# CFA for items measured at the 9th grade
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ability.model.cfa9 <- '
  # latent variables
    quant9  =~ scatq9 + math9 + sci9
    verbal9 =~ scatv9 + sci9 + read9
'
ability.cfa2 <- lavaan::sem(ability.model.cfa9, 
                            sample.cov = ability.cov, 
                            sample.nobs = 383, estimator="ML")

# Chi-Squared statistic:
ability.cfa2@test$standard$stat
# P-value
ability.cfa2@test$standard$pvalue
# Correlation matrix of latent variables
cov2cor(ability.cfa2@Model@GLIST$psi)


# SEM for joint data (7th and 9th grades) in Figure 11.6
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ability.model.sem1 <- '
  # latent variables
    quant7  =~ scatq7 + math7 + sci7
    quant9  =~ scatq9 + math9 + sci9
    verbal7 =~ scatv7 + sci7 + read7
    verbal9 =~ scatv9 + sci9 + read9

  # regressions
    quant9 ~ quant7
    verbal9 ~ verbal7
'

#Fitting the model that appears in Figure 11.6
ability.sem1 <- lavaan::sem(ability.model.sem1, 
                           sample.cov = ability.cov,
                           orthogonal.y = T,
                           orthogonal.x = T,
                           sample.cov.rescale = F,
                           sample.nobs = 383, estimator="ML")
summary(ability.sem1, standardized = TRUE)

# Code below reproduces parts of:
# Figure 11.6: Path diagram, SEM, unstandardized solution, Ability data
lavaanPlot::lavaanPlot(model = ability.sem1,
                       node_options = list(shape = "box", fontname = "Helvetica"),
                       edge_options = list(color = "grey"),
                       stand = FALSE, coefs = TRUE, covs = TRUE)

# Code below reproduces parts of:
# Table 11.5: Standardized Residuals for the SEM of Figure 11.6, Ability Data
ord <- c("math9", "sci9", "read9", "scatv9", "scatq9", "math7", "sci7", "read7", "scatv7", "scatq7")
stres1 <- resid(ability.sem1, type = "standardized")$cov[ord,ord]
stres1[upper.tri(stres1, diag = T)] <- NA;
print(stres1[ord,ord] , na.print = "" , quote = FALSE, digits = 2)

# Computing modification indices
mi <- lavaan::modindices(ability.sem1)
mi[mi$op == "~~",]
mi[mi$op == "=~",]
mi[mi$op == "~",]

# SEM for joint data (7th and 9th grades) in Figure 11.7
# This model allows for correlated residuals between OV 
# and regression path between LV quant9 ~~ verbal9 (as suggested by MI)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Defining the model (measurement and structural) given in Figure 11.7
ability.model.sem3 <- '
  # latent variables
    quant7  =~ scatq7 + math7 + sci7
    quant9  =~ scatq9 + math9 + sci9
    verbal7 =~ scatv7 + sci7 + read7
    verbal9 =~ scatv9 + sci9 + read9

  # regressions
    quant9 ~ quant7
    verbal9 ~ verbal7 + quant9

  # correlated residuals
     math9 ~~ math7
     sci9 ~~ sci7
     read9 ~~ read7
     scatv9 ~~ scatv7
     scatq9 ~~ scatq7
'

#Fitting the model that appears in Figure 11.7
ability.sem3 <- lavaan::sem(ability.model.sem3, 
                            sample.cov = ability.cov,
                            orthogonal.y = T,
                            orthogonal.x = T,
                            sample.cov.rescale = F,
                            sample.nobs = 383, estimator="ML")
summary(ability.sem3, standardized = T,fit.measures = TRUE)
min(resid(ability.sem3, type="standardized")$cov)
max(resid(ability.sem3, type="standardized")$cov)

# List of Estimated Model parameters (page 353)
ability.sem3@Model@GLIST

# Code below reproduces parts of:
# Figure 11.7: Path diagram for unrestricted model, SEM, unstandardized solution, Ability data
lavaanPlot::lavaanPlot(model = ability.sem3,
                       node_options = list(shape = "box", fontname = "Helvetica"),
                       edge_options = list(color = "grey"),
                       stand = FALSE, coefs = TRUE, covs = TRUE)
lavaan::lavTestLRT(ability.sem3,ability.sem1)
fitMeasures(ability.sem3)
summary(ability.sem3, standardized = TRUE)
