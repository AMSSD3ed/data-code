# Chapter 8: Factor Analysis for Binary Data
# LSAT: The Law School Admission Test (LSAT) - Section IV Example
# Change to appropriate path for file "LSAT.txt".

rm(list = ls())

# Install 'ltm', 'mirt' and 'dplyr' packages if not already installed:
if(!all(c("ltm","mirt","dplyr") %in% installed.packages())) install.packages(c("ltm","dplyr","mirt"))
library(ltm)
library(mirt)

# Dataset with 1000 observations (in Book) comes with the 'ltm' package:
# lsat.mat = LSAT

# LSAT: The Law School Admission Test (LSAT) - Section IV Example (1-factor)
# lsat.file <- "C:/AMSSD/Datasets/Chapter8/LSAT.txt"
# The data file path needs to be changed to the file's location on your computer

# Alternatively, choose the file "LSAT.txt" directly below:
lsat.file <- file.choose()
lsat.mat <- data.matrix(read.table(lsat.file, skip=0))

colnames(lsat.mat) <- c("Item 1", "Item 2", "Item 3", "Item 4", "Item 5")

# Code below reproduces:
# Table 8.13: Proportions of positive and negative responses for observed items, LSAT data
round(cbind("Response 1" = colMeans(lsat.mat),
            "Response 0" = 1-colMeans(lsat.mat)),2)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
# One-factor Model: LSAT Data:
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Using ltm:
# **********
lsat.ltm1 <- ltm::ltm(lsat.mat ~ z1, IRT.param = FALSE,
                      control = list(GHk = 48, iter.qN = 100))

# Using mirt:
# ***********
lsat.mirt1 <- mirt::mirt(lsat.mat, model = 1, itemtype = "2PL", SE = T, verbose = F)

# Code below reproduces:
# Table 8.14: Estimated difficulty and discrimination parameters with SEs
# and standardized loadings for the one-factor model, LSAT data
# Using ltm:
# **********
round(coef(lsat.ltm1, standardized = TRUE, prob = TRUE), d=2)
summary(lsat.ltm1) # SEs

# Using mirt:
# **********
# Useful function to reproduce output from "ltm" using "mirt"
coefmirt <- function(mirtobj, digits = 2){
  tmpc <- coef(mirtobj,printSE = T)
  tmpc <- t(sapply(tmpc[-length(tmpc)],FUN=function(x){c(x[,"d"], x[,"a1"])}))
  colnames(tmpc) <- c("a0","SE.a0","a1","SE.a1")
  rownn <- rownames(tmpc)
  tmpc <- as.data.frame(tmpc)
  tmpc$std.a1 <- tmpc$a1/sqrt(tmpc$a1^2 + 1)
  tmpc$`P(x=1|z=0)` <- plogis(tmpc$a0)
  tmpc <- sapply(tmpc, round, digits = digits)
  rownames(tmpc) <- rownn
  tmpc
}
coefmirt(lsat.mirt1)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Goodness-of-fit (GoF, Section 8.4)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Pearson Chi-Squared GoF test statistic (X^2), assuming large sample size:
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Using ltm:
# ~~~~~~~~~~~

p = ncol(lsat.mat) # Number of items
q = 1 # Number of latent variables
dof = 2^p - p*(q+1) - 1 # Degrees of freedom

# Observed frequencies:
lsat.obs <- residuals(lsat.ltm1)[,"Obs"]
# Expected frequencies:
lsat.exp <- residuals(lsat.ltm1)[,"Exp"]

# Before including non-observed patterns:
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Pearson Chi-Squared test statistic (equation 8.6 in Book):
lsat.chisq <- sum((lsat.obs-lsat.exp)^2/lsat.exp); lsat.chisq
# P-value for Chi-squared test:
1-pchisq(lsat.chisq, dof) #0.01

# Log-likelihood ratio GoF test statistic (G^2, Equation 8.5) for pooled data
# (note that log(0) is not defined, hence the use of pooled data):
lsat.g2 <- 2*sum(lsat.obs*log(lsat.obs/lsat.exp)); lsat.g2
1-pchisq(lsat.g2, dof) #0.01

# After including non-observed patterns:
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Identifying missing patterns:
obs.patterns = lsat.ltm1$patterns$X; colnames(obs.patterns) = colnames(lsat.mat)
all.patterns = expand.grid(lapply(1:p,function(x) c(0,1))); names(all.patterns) = colnames(obs.patterns)
mis.patterns = dplyr::anti_join(all.patterns,as.data.frame(obs.patterns))

# Fixing observed frequencies:
lsat.obsf <- c(lsat.obs,0,0)

# Expected frequencies for missing patterns:
lsat.fit2 <- fitted(lsat.ltm1, resp.patterns = mis.patterns, type = "expected")
lsat.expf <- c(lsat.exp,lsat.fit2[,"Exp"]); rm(lsat.fit2)

# Pearson Chi-Squared test statistic (equation 8.6 in Book):
lsat.chisq <- sum((lsat.obsf-lsat.expf)^2/lsat.expf); lsat.chisq
# P-value for Chi-squared test:
1-pchisq(lsat.chisq, dof) #0.01

# Pooling for observed frequencies < 5:
lsat.obs.pool <- c(lsat.obs[lsat.exp >= 5], sum(lsat.obs[lsat.exp < 5]))
lsat.exp.pool <- c(lsat.exp[lsat.exp >= 5], sum(lsat.exp[lsat.exp < 5]))
# Pearson Chi-squared test statistic (pooled):
lsat.chisq.pool <- sum((lsat.obs.pool-lsat.exp.pool)^2/lsat.exp.pool); lsat.chisq.pool
dof2 = length(lsat.obs.pool) - p*(q+1) - 1 #
1-pchisq(lsat.chisq.pool, dof2) #0.01

# Log-likelihood ratio GoF test statistic (G^2, Equation 8.5) for pooled data
# (note that log(0) is not defined, hence the use of pooled data):
lsat.g2 <- 2*sum(lsat.obs.pool*log(lsat.obs.pool/lsat.exp.pool)); lsat.g2
1-pchisq(lsat.g2, dof2) #0.01

# Using mirt:
# ~~~~~~~~~~~
extract.mirt(lsat.mirt1, what = "df")
extract.mirt(lsat.mirt1, what = "G2")

# Factor scores:
# Code below reproduces:
# Table 8.15: Factor scores listed increasing order, LSAT data

# Using ltm:
# ~~~~~~~~~~
lsat.scores.eap <- ltm::factor.scores(lsat.ltm1, method = "EAP")
lsat.scores.com <- ltm::factor.scores(lsat.ltm1, method = "Com")

lsat.scores <- round(cbind("Obs.Freq" = lsat.scores.eap$score.dat$Obs[order(lsat.scores.eap$score.dat$z1)],
                           "Exp.Freq" = lsat.scores.eap$score.dat$Exp[order(lsat.scores.eap$score.dat$z1)],
                           "E(f|x)" = lsat.scores.eap$score.dat$z1[order(lsat.scores.eap$score.dat$z1)],
                           "SE(f|x)" = lsat.scores.eap$score.dat$se.z1[order(lsat.scores.eap$score.dat$z1)],
                           "Comp.Score (X1)" = lsat.scores.com$score.dat$z1[order(lsat.scores.eap$score.dat$z1)],
                           "Tot.Score" = rowSums(lsat.ltm1$patterns$X[order(lsat.scores.eap$score.dat$z1),]),
                           lsat.ltm1$patterns$X[order(lsat.scores.eap$score.dat$z1),]), 2)
lsat.scores

# Using mirt:
# ~~~~~~~~~~
mirt::fscores(lsat.mirt1, full.scores = F, method = "EAP")
