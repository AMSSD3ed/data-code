# Chapter 9: Factor Analysis for Ordinal Data
# Attitudes to Environment Example
# Change to appropriate path for file "environ.dat".

rm(list = ls())

# Install 'ltm' package if not already installed:
if(!all(c("ltm","mirt") %in% installed.packages())) install.packages(c("ltm","mirt"))
library(ltm)

# Dataset comes with the 'ltm' package:
# environ.mat = Environment

# Attitudes to Environment Data Example (1-factor)
# environ.file <- "C:/AMSSD/Datasets/Chapter9/environ.dat"
# The data file path needs to be changed to the file's location on your computer

# Alternatively, choose the file "environ.dat" directly below:
environ.file <- file.choose()
environ.mat <- data.matrix(read.table(environ.file, skip=5))

colnames(environ.mat) <- c("LeadPetrol","RiverSea","RadioWaste","AirPollution",
                           "Chemicals","Nuclear")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# One-factor model (ordinal): Attitudes to Environment Example
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

environ.ord1 <- ltm::grm(environ.mat, IRT.param = FALSE, Hessian = T,
                         control = list(GHk = 21, iter.qN = 100))

# Code below reproduces parts of:
# Table 9.5: Estimated factor loadings with SEs and standardized loadings
# for the one-factor model for ordinal data, Environment Data
SE <- sqrt(diag(ltm::vcov.grm(environ.ord1)))
AL <- coef(environ.ord1)[,"beta"]
round(cbind("alpha1" = AL, "se" = SE[seq(from = 3, to = 18, by = 3)],
            "stalpha1" = AL/sqrt(1+AL^2)), d=2) # Equation (9.7)

# Code below reproduces parts of:
# Table 9.6: Sum of Chi-squared residuals for pairs of items from the second-order margins,
# one-factor model for ordinal data, Attitudes to Environment Data
environ.2way <- ltm::margins(environ.ord1, type = "two-way"); environ.2way

# One-factor model (using mirt)
environ.grm1 <- mirt::mirt(data = environ.mat, model = 1, itemtype = "graded",
                                 SE = T)
coef(environ.grm1,printSE = T)
M2(environ.grm1)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# One-factor model (nominal): Attitudes to Environment Example
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# One-factor model:
environ.nom1 <- mirt::mirt(data = environ.mat, itemtype = "nominal",
                           model = 1, SE = T)

# Code below reproduces parts of:
# Table 9.13: Estimated difficulty and discrimination parameters
# for the one-factor model for nominal data, Environment Data
# Note: ak represents the slope (a1 in Table), d represents category intercept (a0 in Table)
# Coefficient transformation to match output (see, ?mirt for additional context)
a1 <- coef(environ.nom1,printSE = T, simplify = T)$items[,"a1"]
round(cbind(a1*coef(environ.nom1,printSE = T, simplify = T)$items[,c("ak1","ak2")],
            coef(environ.nom1,printSE = T, simplify = T)$items[,c("d1","d2")]),2); #rm(a1)
coef(environ.nom1,printSE = T)

# Code below reproduces parts of:
# Table 9.14: Sum of Chi-squared residuals for pairs of items from the second-order margins,
# one-factor model for nominal data, Attitudes to Environment Data (lower triangular)
residuals(environ.nom1, digits = 2)
