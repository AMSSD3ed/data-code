# Chapter 8: Factor Analysis for Binary Data
# Attitude towards to Abortion Example (1986 British Social Attitudes Survey)
# Change to appropriate path for file "abortion.dat".

rm(list = ls())

# Install 'ltm' and 'dplyr' packages if not already installed:
if(!all(c("ltm","dplyr") %in% installed.packages())) install.packages(c("ltm","dplyr"))
library(ltm)

# Dataset with 379 observations (in Book) comes with the 'ltm' package:
# abortion.mat = Abortion

# Attitude to Abortion Example (1-factor)
# abortion.file <- "C:/AMSSD/Datasets/Chapter8/abortion.dat"
# The data file path needs to be changed to the file's location on your computer

# Alternatively, choose the file "abortion.dat" directly below:
abortion.file <- file.choose()
abortion.mat <- data.matrix(read.table(abortion.file, skip=0))

colnames(abortion.mat) <- c("WomanDecide", "CoupleDecide",
                            "NotMarried", "CannotAfford")

# Using ltm
abortion.ltm1 <- ltm::ltm(abortion.mat ~ z1, IRT.param = FALSE,
                          control = list(GHk = 48, iter.qN = 100))
summary(abortion.ltm1)

# Code below reproduces:
# Table 8.1: Frequencies of response patterns, Attitudes towards abortion example
cbind(abortion.ltm1$patterns$X[order(abortion.ltm1$patterns$obs,decreasing = T),],
      "Obs" = abortion.ltm1$patterns$obs[order(abortion.ltm1$patterns$obs,decreasing = T)])

# Code below reproduces:
# Table 8.2: Cross-tabulation of items 1 and 2, Attitudes towards abortion example
table(abortion.mat[,1],abortion.mat[,2])

# Code below reproduces:
# Table 8.3: Parameter estimates and SEs for the one-factor model, Attitude to Abortion data
# SEs obtained from: summary(abortion.ltm1)
round(coef(abortion.ltm1, standardized = TRUE, prob = TRUE), d=2)

# Code below reproduces:
# Table 8.4: Chi-squared residuals for the second order margins, one-factor model, Attitude towards abortion example.
abortion.2way <- ltm::margins(abortion.ltm1, type = "two-way", nprint = 6, rule = 4); abortion.2way

# Code below reproduces parts of:
# Table 8.5: Chi-squared residuals for the third order margins, response (1,1,1) to items (i,j,k), one-factor model, Attitude towards abortion example.
abortion.3way <- ltm::margins(abortion.ltm1, type = "three-way", nprint = 4, rule = 4); abortion.3way
# round(abortion.3way[[1]][,,8],2)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Goodness-of-fit (GoF, Section 8.4)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Pearson Chi-Squared GoF test statistic (X^2), assuming large sample size:
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
p = ncol(abortion.mat) # Number of items
q = 1 # Number of latent variables
dof = 2^p - p*(q+1) - 1 # Degrees of freedom

# Observed frequencies:
abortion.obs <- residuals(abortion.ltm1)[,"Obs"]
# Expected frequencies:
abortion.exp <- residuals(abortion.ltm1)[,"Exp"]

# Identifying missing patterns:
obs.patterns = abortion.ltm1$patterns$X; colnames(obs.patterns) = colnames(abortion.mat)
all.patterns = expand.grid(lapply(1:p,function(x) c(0,1))); names(all.patterns) = colnames(obs.patterns)
mis.patterns = dplyr::anti_join(all.patterns,as.data.frame(obs.patterns))

# Note: patterns c(1,0,0,1) and c(1,0,1,0) were *NOT* observed:
# Fix observed frequencies for missing patterns:
abortion.obs <- c(abortion.obs,rep(0,nrow(mis.patterns)))

# Expected frequencies for missing patterns:
abortion.fit2 <- fitted(abortion.ltm1, resp.patterns = mis.patterns, type = "expected")
abortion.exp <- c(abortion.exp,abortion.fit2[,"Exp"])

# Pearson Chi-Squared test statistic (equation 8.6 in Book):
abortion.chisq <- sum((abortion.obs-abortion.exp)^2/abortion.exp); abortion.chisq
# P-value for Chi-squared test:
1-pchisq(abortion.chisq, dof) #0.01

# Pooling for expected frequencies < 5:
abortion.obs.pool <- c(abortion.obs[abortion.exp >= 5], sum(abortion.obs[abortion.exp < 5]))
abortion.exp.pool <- c(abortion.exp[abortion.exp >= 5], sum(abortion.exp[abortion.exp < 5]))
# Pearson Chi-squared test statistic (pooled):
abortion.chisq.pool <- sum((abortion.obs.pool-abortion.exp.pool)^2/abortion.exp.pool); abortion.chisq.pool
# Pooled degrees of freedom
dof2 = 3 # (2^p - 5 + 1) - p*(q+1) - 1
1-pchisq(abortion.chisq.pool, dof2) #0.01

# Log-likelihood ratio GoF test statistic (G^2, Equation 8.5) for pooled data
# (note that log(0) is not defined, hence the use of pooled data):
abortion.g2 <- 2*sum(abortion.obs.pool*log(abortion.obs.pool/abortion.exp.pool)); abortion.g2
1-pchisq(abortion.g2, dof2) #0.01

# Proportion of G^2 explained (Section 8.4.iii)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
wd <- abortion.mat[,"WomanDecide"]
cd <- abortion.mat[,"CoupleDecide"]
nm <- abortion.mat[,"NotMarried"]
cna <- abortion.mat[,"CannotAfford"]

abortion.table <- table(wd,cd,nm,cna)
abortion.indep <- stats::loglin(abortion.table, eps = 0.001,
                                margin=list(c(1),c(2),c(3),c(4)),
                                print = F)

# G^2 (loglikelihood ratio test statistic) for the model of independence
abortion.indep$lrt
# Pearson Chi-square (X^2) for the model of independence
abortion.indep$pearson
# Percentage of G^2 explained:
(abortion.indep$lrt - abortion.g2)/abortion.indep$lrt * 100

# Factor scores:
# Code below reproduces:
# Table 8.6: Factor scores listed increasing order, Attitude to Abortion data
abortion.scores.eap <- ltm::factor.scores(abortion.ltm1, method = "EAP")
abortion.scores.com <- ltm::factor.scores(abortion.ltm1, method = "Com")

abortion.scores <- round(cbind("Obs.Freq" = abortion.scores.eap$score.dat$Obs[order(abortion.scores.eap$score.dat$z1)],
                               "Exp.Freq" = abortion.scores.eap$score.dat$Exp[order(abortion.scores.eap$score.dat$z1)],
                               "E(f|x)" = abortion.scores.eap$score.dat$z1[order(abortion.scores.eap$score.dat$z1)],
                               "SE(f|x)" = abortion.scores.eap$score.dat$se.z1[order(abortion.scores.eap$score.dat$z1)],
                               "Comp.Score (X1)" = abortion.scores.com$score.dat$z1[order(abortion.scores.eap$score.dat$z1)],
                               "Tot.Score" = rowSums(abortion.ltm1$patterns$X[order(abortion.scores.eap$score.dat$z1),]),
                               abortion.ltm1$patterns$X[order(abortion.scores.eap$score.dat$z1),]), 2)
abortion.scores

