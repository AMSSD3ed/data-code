# Chapter 8: Factor Analysis for Binary Data
# Contemporary Sexual Attitudes Example (1990 British Social Attitudes Survey)
# Change to appropriate path for file "sexualat.dat".

rm(list = ls())

# Install 'ltm','mirt', and 'dplyr' packages if not already installed:
if(!all(c("ltm","mirt","dplyr") %in% installed.packages())) install.packages(c("ltm","mirt","dplyr"))
library(ltm)
library(mirt)
library(dplyr)

# Sexual Attitudes Example (1-factor)
# sexualat.file <- "C:/AMSSD/Datasets/Chapter8/sexualat.dat"
# The data file path needs to be changed to the file's location on your computer

# Alternatively, choose the file "sexualat.dat" directly below:
sexualat.file <- file.choose()
sexualat.mat <- data.matrix(read.table(sexualat.file, skip=0))

colnames(sexualat.mat) <- c("divorce", "sexdisc", "premar", "exmar", 
                            "gaysex", "gayscho", "gayhied", "gaypubl", 
                            "gayfadop", "gaymadop")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# One-factor model: Sexual Attitudes Example
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Using ltm:
# **********
sexualat.ltm1 <- ltm::ltm(sexualat.mat ~ z1, IRT.param = FALSE,
                          control = list(GHk = 75, iter.qN = 100))

# # Using mirt:
# # **********
# sexualat.mirt1 <- mirt::mirt(sexualat.mat, model = 1, itemtype = "2PL", SE = T)
# summary(sexualat.mirt1)
# coef(sexualat.mirt1, IRTpars=TRUE, printSE=TRUE)
# M2(sexualat.mirt1)

# Code below reproduces:
# Table 8.7: Response frequencies, Sexual attitudes data
# Using ltm:
# **********
sexualat.freq <- cbind(sexualat.ltm1$patterns$X[order(sexualat.ltm1$patterns$obs,decreasing = T),],
                       "Obs" = sexualat.ltm1$patterns$obs[order(sexualat.ltm1$patterns$obs,decreasing = T)])
sexualat.freq[sexualat.freq[,"Obs"] >= 10,]

# Using mirt:
# **********
# sexualat.freq <- cbind(mirt::extract.mirt(sexualat.mirt1, what = c("tabdata")),
                       # freq = unlist(mirt::extract.mirt(sexualat.mirt1, what = c("freq"))))
# sexualat.freq %>% as.data.frame %>% arrange(desc(freq)) %>% filter(freq >= 10)

# Code below reproduces:
# Table 8.8: Proportions giving positive and negative responses to observed items, Sexual Attitudes data
round(cbind("Response 1" = colMeans(sexualat.mat),
            "Response 0" = 1-colMeans(sexualat.mat)),2)

# Code below reproduces:
# Table 8.9: Estimated difficulty and discrimination parameters with SEs 
# and standardized loadings for the one-factor model, Sexual Attitudes data
# Using ltm:
# **********
round(coef(sexualat.ltm1, standardized = TRUE, prob=TRUE), d=2)
summary(sexualat.ltm1) # SEs

# Using mirt:
# **********
# Useful function to reproduce output from "ltm" using "mirt"
# coefmirt <- function(mirtobj, digits = 2){
#   tmpc <- coef(mirtobj,printSE = T)
#   tmpc <- t(sapply(tmpc[-length(tmpc)],FUN=function(x){c(x[,"d"], x[,"a1"])}))
#   colnames(tmpc) <- c("a0","SE.a0","a1","SE.a1")
#   rownn <- rownames(tmpc)
#   tmpc <- as.data.frame(tmpc)
#   tmpc$std.a1 <- tmpc$a1/sqrt(tmpc$a1^2 + 1)
#   tmpc$`P(x=1|z=0)` <- plogis(tmpc$a0)
#   tmpc <- sapply(tmpc, round, digits = digits)
#   rownames(tmpc) <- rownn
#   tmpc
# }
# coefmirt(sexualat.mirt1)

# Code below reproduces:
# Table 8.10: Chi-squared residuals for the second- and third-order margins, one-factor model, Sexual attitudes data.
# Using ltm:
# **********
sexualat.2way <- margins(sexualat.ltm1, type = "two-way", rule = 3, nprint = 10); sexualat.2way
sexualat.3way <- margins(sexualat.ltm1, type = "three-way", rule = 3, nprint = 20); sexualat.3way
# Using mirt:
# **********
# NOTE: mirt does not produce tables for the two- and three-order margins 
# Only observed and expected frequencies (and standardized residuals) for the full patterns
# residuals(sexualat.mirt1, type = "exp")
# Bivariate X^2 residuals per item (lower triangular)
# upper diagonal are the standardized residuals in the form of signed Cramers V coefficients
# residuals(sexualat.mirt1, type = "LD")

# ~~~~~~~~~~~~~~~~~~~~~
# Goodness-of-fit tests
# ~~~~~~~~~~~~~~~~~~~~~

# Pearson Chi-Squared GoF test statistic (X^2), assuming large sample size:
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Using ltm:
# ~~~~~~~~~~
p = ncol(sexualat.mat) # Number of items
q = 1 # Number of latent variables
dof = 2^p - p*(q+1) - 1

# Observed frequencies & pooled observed frequencies:
sexualat.obs <- residuals(sexualat.ltm1)[,"Obs"]
# Expected frequencies & pooled expected frequencies:
sexualat.exp <- residuals(sexualat.ltm1)[,"Exp"]

# Pearson Chi-Squared test statistic (equation 8.6 in Book):
sexualat.chisq <- sum((sexualat.obs-sexualat.exp)^2/sexualat.exp); sexualat.chisq
# P-value for Chi-squared test:
1-pchisq(sexualat.chisq, dof) #0.01

# Log-likelihood ratio GoF test statistic (G^2, Equation 8.5)
sexualat.g2 <- 2*sum(sexualat.obs*log(sexualat.obs/sexualat.exp)); sexualat.g2
1-pchisq(sexualat.g2, dof2) #0.01

# Identifying missing patterns:
obs.patterns = sexualat.ltm1$patterns$X; colnames(obs.patterns) = colnames(sexualat.mat)
all.patterns = expand.grid(lapply(1:p,function(x) c(0,1))); names(all.patterns) = colnames(obs.patterns)
mis.patterns = dplyr::anti_join(all.patterns,as.data.frame(obs.patterns))

# Fixing observed frequencies:
sexualat.obsf <- c(sexualat.obs,rep(0,nrow(mis.patterns)))

# Expected frequencies for missing patterns:
sexualat.fit2 <- fitted(sexualat.ltm1, resp.patterns = mis.patterns, type = "expected")
sexualat.expf <- c(sexualat.exp,sexualat.fit2[,"Exp"])

# Pearson Chi-Squared test statistic for fixed data (equation 8.6 in Book):
sexualat.chisqf <- sum((sexualat.obsf-sexualat.expf)^2/sexualat.expf); sexualat.chisqf
# P-value for Chi-squared test:
1-pchisq(sexualat.chisqf, dof) #0.01

# Pooling for expected frequencies < 5:
sexualat.obs.pool <- c(sexualat.obs[sexualat.exp >= 5], sum(sexualat.obs[sexualat.exp < 5]))
sexualat.exp.pool <- c(sexualat.exp[sexualat.exp >= 5], sum(sexualat.exp[sexualat.exp < 5]))
# Pooled Pearson Chi-Squared test statistic (equation 8.6 in Book):
sexualat.chisq.pool <- sum((sexualat.obs.pool-sexualat.exp.pool)^2/sexualat.exp.pool); sexualat.chisq.pool
# Pooled degrees of freedom
dof2 = length(sexualat.obs.pool) - p*(q+1) - 1 # = 1
# P-value for (pooled) Chi-squared test:
1-pchisq(sexualat.chisq.pool, dof2) #0.01

# Log-likelihood ratio GoF test statistic (G^2, Equation 8.5) for pooled data
sexualat.g2.pool <- 2*sum(sexualat.obs.pool*log(sexualat.obs.pool/sexualat.exp.pool)); sexualat.g2.pool
1-pchisq(sexualat.g2.pool, dof2) #0.01

# Using mirt:
# ~~~~~~~~~~~
extract.mirt(sexualat.mirt1, what = "df")
extract.mirt(sexualat.mirt1, what = "G2")

# Proportion of G^2 explained (Section 8.4.iii)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

i1 <- sexualat.mat[,"divorce"]
i2 <- sexualat.mat[,"sexdisc"]
i3 <- sexualat.mat[,"premar"]
i4 <- sexualat.mat[,"exmar"]
i5 <- sexualat.mat[,"gaysex"]
i6 <- sexualat.mat[,"gayscho"]
i7 <- sexualat.mat[,"gayhied"]
i8 <- sexualat.mat[,"gaypubl"]
i9 <- sexualat.mat[,"gayfadop"]
i10 <- sexualat.mat[,"gaymadop"]

sexualat.table <- table(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10)
sexualat.indep <- stats::loglin(sexualat.table, eps = 0.001,
                                margin=list(c(1),c(2),c(3),c(4),c(5),c(6),c(7),c(8),c(9),c(10)),
                                print = F)

# G^2 test statistic for the model of independence:
sexualat.indep$lrt
# Pearson Chi-square (X^2) for the model of independence
sexualat.indep$pearson
# Percentage of G^2 explained:
(sexualat.indep$lrt - sexualat.g2)/sexualat.indep$lrt * 100

# ~~~~~~~~~~~~~~~~
# Two-factor model
# ~~~~~~~~~~~~~~~~

# Using ltm:
# **********
sexualat.ltm2 <- ltm::ltm(sexualat.mat ~ z1 + z2, IRT.param = FALSE,
                          control = list(GHk = 75, iter.qN = 1000))
summary(sexualat.ltm2)

# Code below reproduces parts of:
# Table 8.11: Chi-squared residuals for the second- and third-order margin for the 
# two-factor model, sexual attitudes data
sexualat.2way2 <- margins(sexualat.ltm2, type = "two-way", rule = 3, nprint = 5); sexualat.2way2
sexualat.3way2 <- margins(sexualat.ltm2, type = "three-way", rule = 3, nprint = 5); sexualat.3way2

# Code below reproduces:
# Table 8.12: Estimated difficulty and discrimination parameters with SEs 
# and standardized loadings for the two-factor model, Sexual Attitudes data
# Using ltm:
# ~~~~~~~~~~
round(coef(sexualat.ltm2, standardized = TRUE, prob = TRUE), d=2)
summary(sexualat.ltm2) # SEs

# GoF test statistic (X^2), (2-factor model):
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Using ltm:
# ~~~~~~~~~~
q = 2 # Number of latent variables
dof = 2^p - p*(q+1) - 1

# Observed frequencies & pooled observed frequencies (2-factor model):
sexualat.obs <- residuals(sexualat.ltm2)[,"Obs"]
# Expected frequencies & pooled expected frequencies (2-factor model):
sexualat.exp <- residuals(sexualat.ltm2)[,"Exp"]

# Log-likelihood ratio GoF test statistic (2-factor model) (G^2, Equation 8.5)
sexualat.g2 <- 2*sum(sexualat.obs*log(sexualat.obs/sexualat.exp)); sexualat.g2

# Proportion of G^2 explained (Section 8.4.iii)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Percentage of G^2 explained (2-factor model):
(sexualat.indep$lrt - sexualat.g2)/sexualat.indep$lrt * 100
