# Chapter 9: Factor Analysis for Ordinal Data
# Attitudes to Science and Technology Example (1992 Eurobarometer Survey from GB)
# Change to appropriate path for file "scien7i.dat".

rm(list = ls())

# Install 'ltm' package if not already installed:
if(!all(c("ltm", "mirt") %in% installed.packages())) install.packages(c("ltm", "mirt"))
library(ltm)
library(mirt)

# Dataset comes with the 'ltm' package:
# science.mat7 = Science

# Attitudes to Science & Technology Data Example (1-factor)
# science.file <- "C:/AMSSD/Datasets/Chapter9/scien4i.dat"
# The data file path needs to be changed to the file's location on your computer

# Alternatively, choose the file "scien7i.dat" directly below:
science.file <- file.choose()
science7.mat <- data.matrix(read.table(science.file, skip=5))

colnames(science7.mat) <- c("Comfort", "Environment", "Work", "Future",
                            "Technology", "Industry", "Benefit")

science4.mat  <- science7.mat[,c("Comfort", "Work", "Future","Benefit")]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# One-factor model: Attitudes to Science & Technology Example (4 items)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

science4.grm1 <- ltm::grm(science4.mat, IRT.param = FALSE, Hessian = T,
                          control = list(GHk = 21, iter.qN = 100))

# Code below reproduces parts of:
# Table 9.1: Estimated factor loadings with SEs and standardized loadings
# for the one-factor model for ordinal data, Science & Technology Data (4 items)
SE <- sqrt(diag(ltm::vcov.grm(science4.grm1)))
AL <- coef(science4.grm1)[,"beta"]
round(cbind("alpha1" = AL, "se" = SE[seq(from = 4, to = 16, by = 4)],
            "stalpha1" = AL/sqrt(1+AL^2)), d=2) # Equation (9.7)

# Code below reproduces parts of:
# Table 9.2: Sum of Chi-squared residuals for pairs of items from the second-order margins,
# one-factor model for ordinal data, Science & Technology Data (4 items)
science4.2way <- ltm::margins(science4.grm1, type = "two-way"); science4.2way

# Code below reproduces parts of:
# Table 9.3: Chi-squared residuals for the two-way margins of items Comfort and Work,
# one-factor model for ordinal data, Science & Technology Data (4 items)
round(science4.2way$margins[[1]]$Resid,2)

# Code below reproduces parts of:
# Table 9.4: Chi-squared residuals (>3) for pairs of items and categories for the
# one-factor model for ordinal data, Science & Technology Data (4 items)
combs <- t(combn(4,2));
out <- NULL
for(i in 1:length(science4.2way$margins)){
  tmp1 <- which(science4.2way$margins[[i]]$Resid > 3.5,arr.ind = T)
  namest <- c("Item1","Item2","Cat1", "Cat2","Resid")
  if(nrow(tmp1) == 0) next
  for(j in 1:nrow(tmp1)){
    tmp2 <- c(combs[i,], tmp1[j,], science4.2way$margins[[i]]$Resid[tmp1])
    names(tmp2) <- namest
    out <- rbind(out,tmp2)
  }; rm(tmp2)
}; rm(tmp1, combs)
rownames(out) <- NULL
round(out,2)

# One-factor model, 4 items with mirt
science4.grm1_mirt <- mirt::mirt(data = science4.mat, model = 1, itemtype = "graded",
                                 SE = T)
coef(science4.grm1_mirt,printSE = T)
# Given the sparsity of the data it is not feasible to carry out global tests:
# M2(science4.grm1_mirt) # Causes error!

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# One-factor model: Attitudes to Science & Technology Example (7 items)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

science7.grm1_ltm <- ltm::grm(science7.mat, IRT.param = TRUE, Hessian = T,
                          control = list(GHk = 41, iter.qN = 1000))

# Code below reproduces parts of:
# Table 9.7: Sum of Chi-squared residuals for pairs of items from the second-order margins,
# one-factor model for ordinal data, Science & Technology Data (7 items)
science7.2way <- ltm::margins(science7.grm1_ltm, type = "two-way"); science7.2way
# summary(science7.grm1_ltm)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# One-factor model: mirt (7 items)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
science7.grm1 <- mirt::mirt(data = science7.mat, model = 1, itemtype = "graded",
                           SE = T)
summary(science7.grm1)
coef(science7.grm1,printSE = T)
residuals(science7.grm1, digits = 2)
# Given the sparsity of the data it is not feasible to carry out global tests:
# M2(science7.grm1) # Causes error!

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Two-factor model: Attitudes to Science & Technology Example (7 items)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# For the 2-factor model for ordinal data we need the "mirt" package.
# Install 'mirt' package if not already installed:

# Two-factor model:
science7.grm2 <- mirt::mirt(data = science7.mat, model = 2, itemtype = "graded",
                            SE = T)

# Code below reproduces parts of:
# Table 9.8: Sum of Chi-squared residuals for pairs of items from the second-order margins,
# two-factor model for ordinal data, Science & Technology Data (7 items)
# (Lower triangular matrix for residuals),
residuals(science7.grm2, digits = 2)

# Code below reproduces parts of:
# Table 9.9: Estimated intercepts and factor loadings (with SEs)
# for the two-factor model for ordinal data, Science & Technology Data (7 items)
# (Up to column permutation and sign)
coef(science7.grm2,printSE = T)

# We obtained the first solution and then rotate it with varimax (Table 9.10)
# (see: ?mirt::mirt.model to check how to impose model restrictions)

science7.grm2_var <- mirt::mirt(science7.mat, model = 2, itemtype="graded",
                                exploratory = TRUE, rotate = "varimax",
                                technical = list(NCYCLES = 1000), SE = T,
                                verbose = T)

# Code below reproduces parts of:
# Table 9.10: Rotated unstandardised factor loadings for the two-factor
# model, Varimax rotation, science and technology data
summary(science7.grm2_var)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# One-factor model: Underlying Variable Approach (UVA)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# For polychoric correlations we need the "psych" package
# Install 'ltm' package if not already installed:
if(!all(c("psych") %in% installed.packages())) install.packages(c("psych"))
library(psych)

# Code below reproduces parts of:
# Table 9.11: Correlation matrices for Science & Technology Data (4 items)
science4.polychor <- psych::polychoric(science4.mat)$rho
round(cbind(cor(science4.mat), science4.polychor),2)

# Code below reproduces parts of:
# Table 9.12: UV approach, estimated factor loadings with SEs and Chi-Squared
# measures of fit, Science & Technology Data (4 items)
science4.famle <- psych::fa(r=science4.mat, factors=1,cor = "poly",
                            n.obs=nrow(science4.mat), fm = "mle")
science4.fawls <- psych::fa(r=science4.mat, factors=1,cor = "poly",
                            n.obs=nrow(science4.mat), fm = "wls")

round(rbind(cbind(science4.famle$loadings, science4.fawls$loadings),
            cbind(science4.famle$STATISTIC, science4.fawls$STATISTIC),
            cbind(science4.famle$PVAL, science4.fawls$PVAL)), 2)
