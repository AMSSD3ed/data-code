# Chapter 11: Confirmatory Factor Analysis and Structural Equation Models
# Subject Marks Data Example 

rm(list = ls())

# Install 'lavaan' package if not already installed:
if(!all(c("lavaan", "lavaanPlot") %in% installed.packages())) install.packages(c("lavaan", "lavaanPlot"))
library(lavaan)
library(lavaanPlot)

#Reading the covariance matrix
lower <- '
1.00,
0.44, 1.00,
0.41, 0.35, 1.00,
0.29, 0.35, 0.16, 1.00,
0.33, 0.32, 0.19, 0.59, 1.00,
0.25, 0.33, 0.18, 0.47, 0.46, 1.00'

#Assigning names to the variables
subject.cov <- 
  lavaan::lav_getcov(lower, names = c("Gaelic","English","History","Arithmetic","Algebra","Geometry"))

# CFA for items measured at the 7th grade
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
subject.model.cfa <- '
  # latent variables
    human =~ Gaelic + English + History
    maths =~ Arithmetic + Algebra + Geometry
'

# CFA with standardized LVs
subject.cfa1 <- lavaan::sem(subject.model.cfa, 
                            sample.cov = subject.cov, 
                            sample.cov.rescale = FALSE,
                            correlation = F,
                            sample.nobs = 220, estimator="ML",
                            std.lv = T)

# Code below reproduces:
# Table 11.1: Factor Loadings with SEs for the two-factor CFA model,
# Subject marks data
lavaan::parameterEstimates(subject.cfa1, zstat = F, pvalue = F, ci = F, rsquare = T)

# Code below reproduces parts of:
# Figure 11.3: Path diagram, CFA, unstandardized solution
# Subject marks data
lavaanPlot::lavaanPlot(model = subject.cfa1,
                       node_options = list(shape = "box", fontname = "Helvetica"),
                       edge_options = list(color = "grey"),
                       stand = FALSE, coefs = TRUE, covs = TRUE)

# CFA with unstandardized LVs
subject.cfa2 <- lavaan::sem(subject.model.cfa, 
                            sample.cov = subject.cov, 
                            sample.cov.rescale = FALSE,
                            sample.nobs = 220, estimator="ML",
                            std.lv = F)

# Code below reproduces parts of:
# Figure 11.4: Path diagram, CFA, unstandardized solution
# using Gaelic and Arithmetic as reference OVs
# Subject marks data
lavaanPlot::lavaanPlot(model = subject.cfa2,
                       node_options = list(shape = "box", fontname = "Helvetica"),
                       edge_options = list(color = "grey"),
                       stand = FALSE, coefs = TRUE, covs = TRUE)

lavaan::parameterEstimates(subject.cfa2, zstat = F, pvalue = F, ci = F)

# Code below reproduces parts of:
# Table 11.2: Reproduced correlations for CFA, and discrepancies between observed
# and reproduced correlations (bottom),
# Subject marks data
round(rbind(fitted(subject.cfa1)$cov, NA,
            resid(subject.cfa1, type = "raw")$cov), 2)

# Code below reproduces parts of:
# Table 11.3: Standardized residuals for two-factor CFA,
# Subject marks data
round(resid(subject.cfa1, type = "standardized")$cov,2)

# Code below reproduces parts of:
# Table 11.4: Fit indices for the two-factor CFA,
# Subject marks data
summary(subject.cfa1, fit.measures = T, estimates = F)
