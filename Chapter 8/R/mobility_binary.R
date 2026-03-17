# Chapter 8: Factor Analysis for Binary Data
# Women's Mobility Data Example (Bangladesh Fertility Survey 1989)
# Change to appropriate path for file "mobility.dat".

rm(list = ls())

# Install 'ltm','mirt' and 'dplyr' packages if not already installed:
if(!all(c("ltm","mirt","dplyr") %in% installed.packages())) install.packages(c("ltm","dplyr","mirt"))
library(ltm)
library(mirt)

# Dataset comes with the 'ltm' package:
# mobility.mat = Mobility

# Women's Mobility Data Example (1-factor)
# mobility.file <- "C:/AMSSD/Datasets/Chapter8/mobility.dat"
# The data file path needs to be changed to the file's location on your computer

# Alternatively, choose the file "mobility.dat" directly below:
mobility.file <- file.choose()
mobility.mat <- data.matrix(read.table(mobility.file, skip=5))

colnames(mobility.mat) <- c("Item 1", "Item 2", "Item 3", "Item 4",
                            "Item 5", "Item 6", "Item 7", "Item 8")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# One-factor model: Women's Mobility Example
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Using ltm
# ~~~~~~~~~
mobility.ltm1 <- ltm::ltm(mobility.mat ~ z1, IRT.param = FALSE,
                          control = list(GHk = 48, iter.qN = 100))
summary(mobility.ltm1)
# Code below reproduces parts of:
# Table 8.22a: Chi-squared residuals (> 3) for the second-order margins, one-factor model, Women's Mobility example.
mobility.2way <- ltm::margins(mobility.ltm1, type = "two-way", nprint = 15, rule = 3); mobility.2way

# Code below reproduces parts of:
# Table 8.22b: Chi-squared residuals for the third-order margins, response (1,1,1) to items (i,j,k), one-factor model, Women's Mobility example.
mobility.3way <- ltm::margins(mobility.ltm1, type = "three-way", nprint = 15, rule = 3); mobility.3way

# Using mirt
# ~~~~~~~~~~
mobility.mirt1 <- mirt::mirt(mobility.mat, model = 1, itemtype = "2PL", SE = T, verbose = F)
summary(mobility.mirt1)
M2(mobility.mirt1)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Goodness-of-fit (GoF, Section 8.4)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Pearson Chi-Squared GoF test statistic (X^2), assuming large sample size:
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
p = ncol(mobility.mat) # Number of items
q = 1 # Number of latent variables
dof = 2^p - p*(q+1) - 1 # Degrees of freedom

# Observed frequencies:
mobility.obs <- residuals(mobility.ltm1)[,"Obs"]
# Expected frequencies:
mobility.exp <- residuals(mobility.ltm1)[,"Exp"]

# Identifying missing patterns:
obs.patterns = mobility.ltm1$patterns$X; colnames(obs.patterns) = colnames(mobility.mat)
all.patterns = expand.grid(lapply(1:p,function(x) c(0,1))); names(all.patterns) = colnames(obs.patterns)
mis.patterns = dplyr::anti_join(all.patterns,as.data.frame(obs.patterns))

# Fixing observed frequencies:
mobility.obs <- c(mobility.obs,rep(0,nrow(mis.patterns)))

# Expected frequencies for missing patterns:
mobility.fit2 <- fitted(mobility.ltm1, resp.patterns = mis.patterns, type = "expected")
mobility.exp <- c(mobility.exp,mobility.fit2[,"Exp"]); rm(mobility.fit2)

# Pearson Chi-Squared test statistic (equation 8.6 in Book):
mobility.chisq <- sum((mobility.obs-mobility.exp)^2/mobility.exp); mobility.chisq
# P-value for Chi-squared test:
1-pchisq(mobility.chisq, dof) #0.01

# Pooling for observed frequencies < 5:
mobility.obs.pool <- c(mobility.obs[mobility.obs >= 5], sum(mobility.obs[mobility.obs < 5]))
mobility.exp.pool <- c(mobility.exp[mobility.obs >= 5], sum(mobility.exp[mobility.obs < 5]))
# Pearson Chi-squared test statistic (pooled):
mobility.chisq.pool <- sum((mobility.obs.pool-mobility.exp.pool)^2/mobility.exp.pool); mobility.chisq.pool
dof2 = length(mobility.obs.pool) - p*(q+1) - 1
1-pchisq(mobility.chisq.pool, dof2) #0.01

# Log-likelihood ratio GoF test statistic (G^2, Equation 8.5) for pooled data
mobility.g2.pool <- 2*sum(mobility.obs.pool*log(mobility.obs.pool/mobility.exp.pool)); mobility.g2.pool
1-pchisq(mobility.g2.pool, dof2) #0.01

# Proportion of G^2 explained (Section 8.4.iii)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

i1 <- mobility.mat[,"Item 1"]
i2 <- mobility.mat[,"Item 2"]
i3 <- mobility.mat[,"Item 3"]
i4 <- mobility.mat[,"Item 4"]
i5 <- mobility.mat[,"Item 5"]
i6 <- mobility.mat[,"Item 6"]
i7 <- mobility.mat[,"Item 7"]
i8 <- mobility.mat[,"Item 8"]

mobility.table <- table(i1,i2,i3,i4,i5,i6,i7,i8)
mobility.indep <- stats::loglin(mobility.table, eps = 0.001,
                            margin=list(c(1),c(2),c(3),c(4),c(5),c(6),c(7),c(8)))

# G^2 (loglikelihood ratio test statistic) for the model of independence
mobility.indep$lrt
# Pearson Chi-square (X^2) for the model of independence
mobility.indep$pearson
# Percentage of G^2 explained:
(mobility.indep$lrt - mobility.g2.pool)/mobility.indep$lrt * 100

# ~~~~~~~~~~~~~~~~
# Two-factor model
# ~~~~~~~~~~~~~~~~

# Using ltm
# ~~~~~~~~~
mobility.ltm2 <- ltm::ltm(mobility.mat ~ z1 + z2, IRT.param = FALSE,
                          control = list(GHk = 15, iter.qN = 100)) #, constraint = t(c(2,3,0))
summary(mobility.ltm2)

# Code below reproduces parts of:
# Table 8.23a: Chi-squared residuals (> 3) for the second-order margins, two-factor model, Women's Mobility example.
mobility.2way2 <- ltm::margins(mobility.ltm2, type = "two-way", nprint = 8, rule = 3); mobility.2way2

# Code below reproduces parts of:
# Table 8.23b: Chi-squared residuals (> 3) for the third-order margins, response (1,1,1) to items (i,j,k), two-factor model, Women's Mobility example.
mobility.3way2 <- ltm::margins(mobility.ltm2, type = "three-way", nprint = 5, rule = 3); mobility.3way2

# Code below reproduces:
# Table 8.24: Estimated difficulty and discrimination parameters with SEs 
# and standardized loadings for the two-factor model, Women's Mobility data
round(coef(mobility.ltm2, standardized = TRUE, prob = TRUE), d=2)

# Using mirt
# ~~~~~~~~~
# mobility.mirt2 <- mirt::mirt(mobility.mat, model = 2, technical = list(NCYCLES = 1000), itemtype = "2PL", SE = T, verbose = T)
mobility.mirt2 <- mirt::mirt(mobility.mat, model = 2, exploratory = TRUE,
                             rotate = "oblimin", technical = list(NCYCLES = 1000),
                             itemtype = "2PL", SE = T, verbose = T)
M2(mobility.mirt2)

# Code below reproduces:
# Table 8.25: Rotated unstandardised factor loadings for the two-factor
# model, oblimin rotation, women’s mobility data
summary(mobility.mirt2)

# Pearson Chi-Squared GoF test statistic (X^2):
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
p = ncol(mobility.mat) # Number of items
q = 2 # Number of latent variables
dof = 2^p - p*(q+1) - 1 # Degrees of freedom

# Observed frequencies:
mobility.obs <- residuals(mobility.ltm2)[,"Obs"]
# Expected frequencies:
mobility.exp <- residuals(mobility.ltm2)[,"Exp"]

# Identifying missing patterns:
obs.patterns = mobility.ltm2$patterns$X; colnames(obs.patterns) = colnames(mobility.mat)
all.patterns = expand.grid(lapply(1:p,function(x) c(0,1))); names(all.patterns) = colnames(obs.patterns)
mis.patterns = dplyr::anti_join(all.patterns,as.data.frame(obs.patterns))

# Fixing observed frequencies:
mobility.obs <- c(mobility.obs,rep(0,nrow(mis.patterns)))

# Expected frequencies for missing patterns:
mobility.fit2 <- fitted(mobility.ltm2, resp.patterns = mis.patterns, type = "expected")
mobility.exp <- c(mobility.exp,mobility.fit2[,"Exp"]); rm(mobility.fit2)

# Pearson Chi-Squared test statistic (equation 8.6 in Book):
mobility.chisq <- sum((mobility.obs-mobility.exp)^2/mobility.exp); mobility.chisq
# P-value for Chi-squared test:
1-pchisq(mobility.chisq, dof) #0.01

# Pooling for observed frequencies < 5:
mobility.obs.pool <- c(mobility.obs[mobility.obs >= 5], sum(mobility.obs[mobility.obs < 5]))
mobility.exp.pool <- c(mobility.exp[mobility.obs >= 5], sum(mobility.exp[mobility.obs < 5]))
# Pearson Chi-squared test statistic (pooled):
mobility.chisq.pool <- sum((mobility.obs.pool-mobility.exp.pool)^2/mobility.exp.pool); mobility.chisq.pool
dof2 = 33
1-pchisq(mobility.chisq.pool, dof2) #0.01

# Log-likelihood ratio GoF test statistic (G^2, Equation 8.5) for pooled data
mobility.g2.pool <- 2*sum(mobility.obs.pool*log(mobility.obs.pool/mobility.exp.pool)); mobility.g2.pool
1-pchisq(mobility.g2.pool, dof2) #0.01

# Percentage of G^2 explained by 2-factor model:
(mobility.indep$lrt - mobility.g2.pool)/mobility.indep$lrt * 100
