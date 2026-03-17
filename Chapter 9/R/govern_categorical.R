# Chapter 9: Factor Analysis for Ordinal Data
# Government Data Example (1996 British Social Attitudes Survey)
# Change to appropriate path for file "govern.dat".

rm(list = ls())

# Install 'ltm' package if not already installed:
if(!all(c("ltm") %in% installed.packages())) install.packages(c("ltm"))
library(ltm)

# Government Data Example (1-factor)
# govern.file <- "C:/AMSSD/Datasets/Chapter9/govern.dat"
# The data file path needs to be changed to the file's location on your computer

# Alternatively, choose the file "govern.dat" directly below:
govern.file <- file.choose()
govern.mat <- data.matrix(read.table(govern.file, skip=5))

colnames(govern.mat) <- c("JobEvery","PriceCont","Living","Income",
                           "Housing")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# One-factor model (ordinal): Government Data Example
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

govern.ord1 <- ltm::grm(govern.mat, IRT.param = FALSE, Hessian = T,
                         control = list(GHk = 21, iter.qN = 100))

# Code below reproduces parts of:
# Table 9.14: Sum of Chi-squared residuals for pairs of items from the second-order margins,
# one-factor model for ordinal data, Government Data
govern.2way <- ltm::margins(govern.ord1, type = "two-way"); govern.2way

# Code below reproduces parts of:
# Table 9.15: Estimated factor loadings with SEs and standardized loadings
# for the one-factor model for ordinal data, Government Data
SE <- sqrt(diag(ltm::vcov.grm(govern.ord1)))
AL <- coef(govern.ord1)[,"beta"]
round(cbind("alpha1" = AL, "se" = SE[seq(from = 4, to = 20, by = 4)],
            "stalpha1" = AL/sqrt(1+AL^2)), d=2) # Equation (9.7)
