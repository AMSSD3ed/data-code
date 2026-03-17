# Chapter 9: Factor Analysis for Ordinal Data
# Voters Study in Flanders, Belgium Example
# Change to appropriate path for file "voters.dat".

rm(list = ls())

# Install 'ltm' package if not already installed:
if(!all(c("ltm", "mirt") %in% installed.packages())) install.packages(c("ltm", "mirt"))
library(ltm)
library(mirt)

# Attitudes to Environment Data Example (1-factor)
# voters.file <- "C:/AMSSD/Datasets/Chapter9/voters1.dat"
# The data file path needs to be changed to the file's location on your computer

# Alternatively, choose the file "voters1.dat" directly below:
voters.file <- file.choose()
voters.mat <- data.matrix(read.table(voters.file, skip=5))

colnames(voters.mat) <- c("Item 1", "Item 2", "Item 3", "Item 4",
                          "Item 5")

# Code below reproduces parts of:
# Table 9.17: Frequency distribution of observed items, Voters Study in Flanders Data
round(sapply(1:5,function(i) table(voters.mat[,i])/nrow(voters.mat)),2)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# One-factor model (ordinal): Voters' Attitudes to Immigration Example
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

voters.ord1 <- ltm::grm(voters.mat, IRT.param = FALSE, Hessian = T,
                         control = list(GHk = 21, iter.qN = 100))

# Code below reproduces parts of:
# Table 9.18: Sum of Chi-squared residuals for pairs of items from the second-order margins,
# one-factor model for ordinal data, Voters Study in Flanders Data
voters.2way <- ltm::margins(voters.ord1, type = "two-way"); voters.2way

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# One-factor model (nominal): Voters Study in Flanders Example
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# One-factor model:
voters.nom1 <- mirt::mirt(data = voters.mat, itemtype = "nominal",
                           model = 1, SE = T)

# Code below reproduces parts of:
# Table 9.19: Estimated factor loadings (with SEs)
# for the one-factor model for nominal data, Voters Study in Flanders Data

# Coefficient transformation to match output (see, ?mirt for additional context)
a1 <- coef(voters.nom1,printSE = T, simplify = T)$items[,"a1"]
round(a1*coef(voters.nom1,printSE = T, simplify = T)$items[,paste0("ak",1:4)],2); rm(a1)
coef(voters.nom1,printSE = T) # for SEs

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Two-factor model (nominal): Voters Study in Flanders Example
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

voters.nom2_temp <- mirt::mirt(data = voters.mat, itemtype = "nominal",
                               model = 2, SE = T, technical = list(NCYCLES = 2e3))
coef(voters.nom2_temp,simplify = T)$items

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# One-factor model (ordinal, 3 categories): Voters' Attitudes to Immigration Example
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Re-formatting the data to three categories:

voters3.mat <- voters.mat
voters3.mat[voters.mat == 2] = 1
voters3.mat[voters3.mat == 3] = 2
voters3.mat[voters.mat == 5] = 4
voters3.mat[voters3.mat == 4] = 3

# One-factor model (3 categories):
voters3.ord1 <- ltm::grm(voters3.mat, IRT.param = FALSE, Hessian = T,
                        control = list(GHk = 21, iter.qN = 100))

# Code below reproduces parts of:
# Table 9.20: Sum of Chi-squared residuals for pairs of items from the second-order margins,
# one-factor model for ordinal data, Voters Study in Flanders Data (3 categories)
voters3.2way <- ltm::margins(voters3.ord1, type = "two-way"); voters3.2way

# Code below reproduces parts of:
# Table 9.21: Estimated factor loadings with SEs and standardized loadings
# for the one-factor model for ordinal data, Voters Study in Flanders Data (3 categories)
SE <- sqrt(diag(ltm::vcov.grm(voters3.ord1)))
AL <- coef(voters3.ord1)[,"beta"]
round(cbind("alpha1" = AL, "se" = SE[seq(from = 3, to = 15, by = 3)],
            "stalpha1" = AL/sqrt(1+AL^2)), d=2) # Equation (9.7)
