# Chapter 8: Factor Analysis for Binary Data
# WIRS: Workplace Industrial Relations Data Example
# Change to appropriate path for file "wirs.dat".

rm(list = ls())

# Install 'ltm', 'mirt', and 'dplyr' packages if not already installed:
if(!all(c("ltm","mirt","dplyr") %in% installed.packages())) install.packages(c("ltm","dplyr","mirt"))
library(ltm)
library(mirt)

# Dataset comes with the 'ltm' package:
# wirs.mat = WIRS

# WIRS: Workplace Industrial Relations Data Example (1-factor)
# wirs.file <- "C:/AMSSD/Datasets/Chapter8/wirs.dat"
# The data file path needs to be changed to the file's location on your computer

# Alternatively, choose the file "wirs.dat" directly below:
wirs.file <- file.choose()
wirs.mat <- data.matrix(read.table(wirs.file, skip=0))

colnames(wirs.mat) <- c("Item 1", "Item 2", "Item 3", "Item 4", "Item 5", "Item 6")

# Code below reproduces:
# Table 8.16: Proportions of positive and negative responses for observed items, WIRS data
round(cbind("Response 1" = colMeans(wirs.mat),
            "Response 0" = 1-colMeans(wirs.mat)),2)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# One-factor model: WIRS Example (6 items)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Using ltm:
# **********
wirs.ltm1 <- ltm::ltm(wirs.mat ~ z1, IRT.param = FALSE,
                      control = list(GHk = 48, iter.qN = 100))
round(coef(wirs.ltm1, standardized = TRUE, prob = TRUE), d=2)

# Using mirt:
# **********
wirs.mirt1 <- mirt::mirt(wirs.mat, model = 1, itemtype = "2PL", SE = T, verbose = F)

# Code below reproduces parts of:
# Table 8.17a: Chi-squared residuals (> 3) for the second-order margins, one-factor model, WIRS example.
# Table 8.17b: Chi-squared residuals for the third-order margins, response (1,1,1) to items (i,j,k), one-factor model, WIRS example.
# Using ltm:
# **********
wirs.2way <- ltm::margins(wirs.ltm1, type = "two-way", nprint = 4, rule = 3); wirs.2way
wirs.3way <- ltm::margins(wirs.ltm1, type = "three-way", nprint = 8, rule = 3); wirs.3way

# Using mirt:
# **********
# NOTE: mirt does not produce tables for the two- and three-order margins 
# Only observed and expected frequencies (and standardized residuals) for the full patterns
# residuals(wirs.mirt1, type = "exp")
# Bivariate X^2 residuals per item (lower triangular)
# upper diagonal are the standardized residuals in the form of signed Cramers V coefficients
# residuals(wirs.mirt1, type = "LD")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Goodness-of-fit (GoF, Section 8.4)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Pearson Chi-Squared GoF test statistic (X^2), assuming large sample size:
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
p = ncol(wirs.mat) # Number of items
q = 1 # Number of latent variables
dof = 2^p - p*(q+1) - 1 # Degrees of freedom

# Observed frequencies:
wirs.obs <- residuals(wirs.ltm1)[,"Obs"]
# Expected frequencies:
wirs.exp <- residuals(wirs.ltm1)[,"Exp"]

# Pearson Chi-Squared test statistic (equation 8.6 in Book):
wirs.chisq <- sum((wirs.obs-wirs.exp)^2/wirs.exp); wirs.chisq
# P-value for Chi-squared test:
1-pchisq(wirs.chisq, dof) #0.01

# Log-likelihood ratio GoF test statistic (G^2, Equation 8.5)
wirs.g2 <- 2*sum(wirs.obs*log(wirs.obs/wirs.exp)); wirs.g2
1-pchisq(wirs.g2, dof) #0.01

# Identifying missing patterns:
obs.patterns = wirs.ltm1$patterns$X; colnames(obs.patterns) = colnames(wirs.mat)
all.patterns = expand.grid(lapply(1:p,function(x) c(0,1))); names(all.patterns) = colnames(obs.patterns)
mis.patterns = dplyr::anti_join(all.patterns,as.data.frame(obs.patterns))

# Fixing observed frequencies:
wirs.obsf <- c(wirs.obs,rep(0,nrow(mis.patterns)))

# Expected frequencies for missing patterns:
wirs.fit2 <- fitted(wirs.ltm1, resp.patterns = mis.patterns, type = "expected")
wirs.expf <- c(wirs.exp,wirs.fit2[,"Exp"]); rm(wirs.fit2)

# Pearson Chi-Squared test statistic (equation 8.6 in Book):
wirs.chisq <- sum((wirs.obsf-wirs.expf)^2/wirs.expf); wirs.chisq
# P-value for Chi-squared test:
1-pchisq(wirs.chisq, dof) #0.01

# Pooling for expected frequencies < 5:
wirs.obs.pool <- c(wirs.obs[wirs.exp >= 5], sum(wirs.obs[wirs.exp < 5]))
wirs.exp.pool <- c(wirs.exp[wirs.exp >= 5], sum(wirs.exp[wirs.exp < 5]))
# Pearson Chi-squared test statistic (pooled):
wirs.chisq.pool <- sum((wirs.obs.pool-wirs.exp.pool)^2/wirs.exp.pool); wirs.chisq.pool
dof2 = 32 # 23 # length(wirs.obs.pool) - p*(q+1) - 1 # = 23
1-pchisq(wirs.chisq.pool, dof2) #0.01

# Log-likelihood ratio GoF test statistic (G^2, Equation 8.5) for pooled data
wirs.g2.pool <- 2*sum(wirs.obs.pool*log(wirs.obs.pool/wirs.exp.pool)); wirs.g2.pool
1-pchisq(wirs.g2.pool, dof2) #0.01

# Proportion of G^2 explained (Section 8.4.iii)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

i1 <- wirs.mat[,"Item 1"]
i2 <- wirs.mat[,"Item 2"]
i3 <- wirs.mat[,"Item 3"]
i4 <- wirs.mat[,"Item 4"]
i5 <- wirs.mat[,"Item 5"]
i6 <- wirs.mat[,"Item 6"]

wirs.table <- table(i1,i2,i3,i4,i5,i6)
wirs.indep <- stats::loglin(wirs.table, eps = 0.001,
                            margin=list(c(1),c(2),c(3),c(4),c(5),c(6)), print = F)

# G^2 (loglikelihood ratio test statistic) for the model of independence
wirs.indep$lrt
# Pearson Chi-square (X^2) for the model of independence
wirs.indep$pearson
# Percentage of G^2 explained:
(wirs.indep$lrt - wirs.g2.pool)/wirs.indep$lrt * 100

# Using mirt:
# ~~~~~~~~~~~
extract.mirt(wirs.mirt1, what = "df")
extract.mirt(wirs.mirt1, what = "G2")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Two-factor model (6 items, unstable)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Using ltm:
# **********
wirs.ltm2 <- ltm::ltm(wirs.mat ~ z1 + z2, IRT.param = FALSE,
                      control = list(GHk = 15, iter.qN = 100))
# Using mirt:
# ***********
wirs.mirt2 <- mirt::mirt(wirs.mat, model = 2, itemtype = "2PL", SE = T, verbose = F)
# Useful function to reproduce output from "ltm" using "mirt"
coefmirt2 <- function(mirtobj, digits = 2){
  tmpc <- coef(mirtobj,printSE = T)
  tmpc <- t(sapply(tmpc[-length(tmpc)],FUN=function(x){c(x[,"d"], x[,paste0("a",1:2)])}))
  colnames(tmpc) <- c("a0","SE.a0","a1","SE.a1","a2","SE.a2")
  rownn <- rownames(tmpc)
  tmpc <- as.data.frame(tmpc)
  tmpc$std.a1 <- tmpc$a1/sqrt(tmpc$a1^2 + 1)
  tmpc$std.a2 <- tmpc$a2/sqrt(tmpc$a2^2 + 1)
  tmpc$`P(x=1|z=0)` <- plogis(tmpc$a0)
  tmpc <- sapply(tmpc, round, digits = digits)
  rownames(tmpc) <- rownn
  tmpc
}

# Code below reproduces:
# Table 8.19: Estimated difficulty and discrimination parameters with SEs 
# and standardized loadings for the two-factor model, WIRS data
# Using ltm:
# **********
round(coef(wirs.ltm2, standardized = TRUE, prob = TRUE), d=2)
# NOTE: ltm does not impose identification restrictions on the factor loading
# matrix, and thus is subject to rotational indeterminacy.
# We try an orthogonal (Varimax) rotation on estimated coefficients using "ltm".
# The rotated solution is closer to the one in the Book (up to sign & column permutation)
coef.ltm2 <- GPArotation::Varimax(wirs.ltm2$coefficients[,-1], maxit = 1e3)
coef.ltm2$loadings[,"z2"] <- -coef.ltm2$loadings[,"z2"]
coef.ltm2

# Using mirt:
# ***********
coefmirt2(wirs.mirt2,3)
# NOTE: mirt imposes zero-restrictions on the upper diagonal of the factor loading
# matrix. To obtain a more meaningful solution, we try orthogonal (Varimax) rotation
# on the estimated coefficients using "mirt".
# The rotated solution is closer to the one in the Book (up to sign & column permutation).
coef.mirt2 <- GPArotation::Varimax(coefmirt2(wirs.mirt2,3)[,c("a1","a2")], maxit = 1e3)
coef.mirt2$loadings <- cbind("a1" = coef.mirt2$loadings[,"a1"], a2 = -coef.mirt2$loadings[,"a2"])
coef.mirt2

# Code below reproduces parts of:
# Table 8.18: Chi-squared residuals (> 3) for the third-order margins, response (1,1,1) to items (i,j,k), two-factor model, WIRS example.
# Using ltm:
# **********
wirs.3way2 <- ltm::margins(wirs.ltm2, type = "three-way", nprint = 5, rule = 3); wirs.3way2

# Using mirt:
# **********
# NOTE: mirt does not produce tables for the two- and three-order margins 
# Only observed and expected frequencies (and standardized residuals) for the full patterns
# residuals(wirs.mirt2, type = "exp")
# Bivariate X^2 residuals per item (lower triangular)
# upper diagonal are the standardized residuals in the form of signed Cramers V coefficients
# residuals(wirs.mirt2, type = "LD")

# Pearson Chi-Squared GoF test statistic (X^2):
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
p = ncol(wirs.mat) # Number of items
q = 2 # Number of latent variables
dof = 2^p - p*(q+1) - 1 # Degrees of freedom

# Observed frequencies:
wirs.obs <- residuals(wirs.ltm2)[,"Obs"]
# Expected frequencies:
wirs.exp <- residuals(wirs.ltm2)[,"Exp"]

# Pearson Chi-Squared test statistic (equation 8.6 in Book):
wirs.chisq <- sum((wirs.obs-wirs.exp)^2/wirs.exp); wirs.chisq
# P-value for Chi-squared test:
1-pchisq(wirs.chisq, dof) #0.01

# Log-likelihood ratio GoF test statistic (G^2, Equation 8.5) for pooled data
wirs.g2 <- 2*sum(wirs.obs*log(wirs.obs/wirs.exp)); wirs.g2
1-pchisq(wirs.g2, dof) #0.01

# Identifying missing patterns:
obs.patterns = wirs.ltm2$patterns$X; colnames(obs.patterns) = colnames(wirs.mat)
all.patterns = expand.grid(lapply(1:p,function(x) c(0,1))); names(all.patterns) = colnames(obs.patterns)
mis.patterns = dplyr::anti_join(all.patterns,as.data.frame(obs.patterns))

# Fixing observed frequencies:
wirs.obsf <- c(wirs.obs,rep(0,nrow(mis.patterns)))

# Expected frequencies for missing patterns:
wirs.fit2 <- fitted(wirs.ltm2, resp.patterns = mis.patterns, type = "expected")
wirs.expf <- c(wirs.exp,wirs.fit2[,"Exp"]); rm(wirs.fit2)

# Pearson Chi-Squared test statistic (equation 8.6 in Book) for complete patterns:
wirs.chisq <- sum((wirs.obsf-wirs.expf)^2/wirs.expf); wirs.chisq
# P-value for Chi-squared test:
1-pchisq(wirs.chisq, dof) #0.01

# Pooling for expected frequencies < 5 (but not including expected frequencies for non-observed patterns):
wirs.obs.pool <- c(wirs.obs[wirs.exp >= 5], sum(wirs.obs[wirs.exp < 5]))
wirs.exp.pool <- c(wirs.exp[wirs.exp >= 5], sum(wirs.exp[wirs.exp < 5]))
# Pearson Chi-squared test statistic (pooled):
wirs.chisq.pool <- sum((wirs.obs.pool-wirs.exp.pool)^2/wirs.exp.pool); wirs.chisq.pool
dof2 = 24 # !!! CHECK length(wirs.obs.pool) - p*(q+1) - 1 # = 23 
1-pchisq(wirs.chisq.pool, dof2) #0.01

# Log-likelihood ratio GoF test statistic (G^2, Equation 8.5) for pooled data
wirs.g2.pool <- 2*sum(wirs.obs.pool*log(wirs.obs.pool/wirs.exp.pool)); wirs.g2.pool
1-pchisq(wirs.g2.pool, dof2) #0.01

# Percentage of G^2 explained by 2-factor model:
(wirs.indep$lrt - wirs.g2.pool)/wirs.indep$lrt * 100

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Re-analysis with 5 items (exclude item 1)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Using ltm:
# **********
wirs.ltm1s <- ltm::ltm(wirs.mat[,-1] ~ z1, IRT.param = FALSE,
                       control = list(GHk = 48, iter.qN = 100))

# Using mirt:
# **********
wirs.mirt1s <- mirt::mirt(wirs.mat[,-1], model = 1, itemtype = "2PL", SE = T, verbose = F)
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

# Code below reproduces:
# Table 8.21: Estimated difficulty and discrimination parameters with SEs 
# and standardized loadings for the one-factor model with item 1 omitted, WIRS data
# Using ltm:
# **********
round(coef(wirs.ltm1s, standardized = TRUE, prob = TRUE), d=2)
# Using mirt:
# **********
coefmirt(wirs.mirt1s)

# Code below reproduces parts of:
# Table 8.20a: Chi-squared residuals (> 3) for the second-order margins,
# one-factor model (item 1 omitted), WIRS example.
wirs.2way <- ltm::margins(wirs.ltm1s, type = "two-way", nprint = 3, rule = 3); wirs.2way

# Code below reproduces parts of:
# Table 8.20b: Chi-squared residuals for the third-order margins, response (1,1,1) to items (i,j,k),
# one-factor model (item 1 omitted), WIRS example.
wirs.3way <- ltm::margins(wirs.ltm1s, type = "three-way", nprint = 3, rule = 3); wirs.3way


# Pearson Chi-Squared GoF test statistic (X^2), assuming large sample size:
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
p = ncol(wirs.mat[,-1]) # Number of items
q = 1 # Number of latent variables
dof = 2^p - p*(q+1) - 1 # Degrees of freedom

# Observed frequencies:
wirs.obs <- residuals(wirs.ltm1s)[,"Obs"]
# Expected frequencies:
wirs.exp <- residuals(wirs.ltm1s)[,"Exp"]

# Pearson Chi-Squared test statistic (equation 8.6 in Book):
wirs.chisq <- sum((wirs.obs-wirs.exp)^2/wirs.exp); wirs.chisq
# P-value for Chi-squared test:
1-pchisq(wirs.chisq, dof) #0.01

# Pooling for expected frequencies < 5:
wirs.obs.pool <- c(wirs.obs[wirs.exp >= 5], sum(wirs.obs[wirs.exp < 5]))
wirs.exp.pool <- c(wirs.exp[wirs.exp >= 5], sum(wirs.exp[wirs.exp < 5]))
# Pearson Chi-squared test statistic (pooled):
wirs.chisq.pool <- sum((wirs.obs.pool-wirs.exp.pool)^2/wirs.exp.pool); wirs.chisq.pool
dof2 = 17 # (2^p - length(wirs.obs[wirs.obs < 5]) + 1) - p*(q+1)
1-pchisq(wirs.chisq.pool, dof2) #0.01

# Log-likelihood ratio GoF test statistic (G^2, Equation 8.5) for pooled data
wirs.g2.pool <- 2*sum(wirs.obs.pool*log(wirs.obs.pool/wirs.exp.pool)); wirs.g2.pool
1-pchisq(wirs.g2.pool, dof2) #0.01

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Two-factor model (5 items, omitting item 1)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Model with all 6 items: Unstable solution
wirs.ltm2s <- ltm::ltm(wirs.mat[,-1] ~ z1 + z2, IRT.param = FALSE,
                       control = list(GHk = 15, iter.qN = 100))

p = ncol(wirs.mat[,-1]) # Number of items
q = 2 # Number of latent variables
dof = 2^p - p*(q+1) - 1 # Degrees of freedom

# Observed frequencies:
wirs.obs <- residuals(wirs.ltm2s)[,"Obs"]
# Expected frequencies:
wirs.exp <- residuals(wirs.ltm2s)[,"Exp"]

# Pearson Chi-Squared test statistic (equation 8.6 in Book):
wirs.chisq <- sum((wirs.obs-wirs.exp)^2/wirs.exp); wirs.chisq
# P-value for Chi-squared test:
1-pchisq(wirs.chisq, dof) #0.01

# Pooling for expected frequencies < 5:
wirs.obs.pool <- c(wirs.obs[wirs.exp >= 5], sum(wirs.obs[wirs.exp < 5]))
wirs.exp.pool <- c(wirs.exp[wirs.exp >= 5], sum(wirs.exp[wirs.exp < 5]))
# Pearson Chi-squared test statistic (pooled):
wirs.chisq.pool <- sum((wirs.obs.pool-wirs.exp.pool)^2/wirs.exp.pool); wirs.chisq.pool
dof2 = 13
1-pchisq(wirs.chisq.pool, dof2) #0.01

# Log-likelihood ratio GoF test statistic (G^2, Equation 8.5) for pooled data
wirs.g2.pool <- 2*sum(wirs.obs.pool*log(wirs.obs.pool/wirs.exp.pool)); wirs.g2.pool
1-pchisq(wirs.g2.pool, dof2) #0.01

wirs.2way <- ltm::margins(wirs.ltm2s, type = "two-way", nprint = 3, rule = 3); wirs.2way
wirs.3way <- ltm::margins(wirs.ltm2s, type = "three-way", nprint = 3, rule = 3); wirs.3way