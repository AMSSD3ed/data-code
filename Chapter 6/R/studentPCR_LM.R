# Chapter 6: Regression Analysis
# Predicting Student Achievement Example (Data from two Portuguese Schools)
# Change to appropriate path for file "student-por.csv".

# Principal Component Regression (PCR, Section 6.13)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rm(list = ls())

# Install 'pls' package if not already installed:
if(!all(c("pls") %in% installed.packages())) install.packages(c("pls"))
library(pls)

# Data from Portuguese Schools Example
# pcr.file <- "C:/AMSSD/Datasets/Chapter6/student-por.csv"
# The data file path needs to be changed to the file's location on your computer

# Alternatively, choose the file "student-por.csv" directly below:
pcr.file <- file.choose()
pcr.dat <- read.csv(pcr.file, sep= ";")
pcr.dat <- subset(pcr.dat, select = -c(G1, G2))

# Number of students
n = nrow(pcr.dat)

# Create training & testing datasets
set.seed(1)
train.index <- sample(1:n, 500)
pcr.dat.train = pcr.dat[train.index,]
pcr.dat.test = pcr.dat[-train.index,]

# Principal Component Regression (PCR)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
set.seed(1)
pcr.fit <- pls::pcr(G3~., data = pcr.dat.train, scale = TRUE, validation ="CV")
summary(pcr.fit)
cverr <- (pls::RMSEP(pcr.fit)$val[1,,])^2

# Code below reproduces:
# Figure 6.5: Plot of the cross-validation MSE as a function of the number
# of PCs in principal component regression for student achievement data
plot(0:39, cverr, type = "l", xlab = "Number of PCs", ylab = "Cross-validation MSE")
points(which.min(cverr)-1, min(cverr), pch = 4, cex = 2)
dev.off()

# Predicted values from PCR
pcr.pred = predict(pcr.fit, pcr.dat.test, ncomp = which.min(cverr)-1)
mean((pcr.pred - pcr.dat.test$G3)^2)

reg.fit <- lm(G3~., data = pcr.dat.train)
red.pred <- predict(reg.fit, pcr.dat.test)
mean((red.pred - pcr.dat.test$G3)^2)

