# Chapter 10: Models with Categorical Latent Variables
# Cost of Living Data Example (Section 10.2)
# Gaussian Mixture Model (GMM)
rm(list = ls())

# Install required package if not already installed:
if(!all(c("mclust", "scales") %in% installed.packages())) install.packages(c("mclust","scales"))
library(mclust)

# Cost of Living Dataset
# abortion.file <- "C:/AMSSD/Datasets/Chapter10/cost-of-living.txt"
# The data file path needs to be changed to the file's location on your computer

# Alternatively, choose the file "cost-of-living.txt" directly below:
living.file <- file.choose()
living.data <- data.frame(read.table(living.file, skip=0))

# Code below reproduces:
# Figure 10.1: Scatterplot of the logarithm of the average monthly net
# salary versus the logarithm of the average price per square meter to buy an
# apartment in the city centre; cost-of-living data
plot(living.data[,3], living.data[,4], xlab = "log(net salary)", ylab = "log(housing price)")
dev.off()

# GMM with 2 classes ("VVV": different covariance matrices)
living.gmm2 <- mclust::Mclust(living.data[,-(1:2)], G = 2, modelNames = "VVV")

# Code below reproduces parts of:
# Figure 10.2: Contour plots for estimated Gaussian mixture models.
# Panel (a): model with unconstrained class-specific covariance matrices.
par(mfrow = c(1,2))
plot(living.gmm2, what = "density", xlab = "log(net salary)", ylab = "log(housing price)", main = "(a)")
points(living.data[,3], living.data[,4], xlab = "log(net salary)", ylab = "log(housing price)", col= scales::alpha(1, .2))
points(living.gmm2$parameters$mean[1,1], living.gmm2$parameters$mean[2,1],  pch =8)
points(living.gmm2$parameters$mean[1,2], living.gmm2$parameters$mean[2,2],  pch =8)

# GMM with 2 classes ("EEE": equal covariance matrices)
living.gmm2.E <- mclust::Mclust(living.data[,-(1:2)], G = 2, modelNames = "EEE")

# Code below reproduces parts of:
# Figure 10.2: Contour plots for estimated Gaussian mixture models.
# Panel (b): model with a common covariance matrix; cost-of-living data.
plot(living.gmm2.E, what = "density", xlab = "log(net salary)", ylab = "log(housing price)", main = "(b)")
points(living.data[,3], living.data[,4], xlab = "log(net salary)", ylab = "log(housing price)", col= scales::alpha(1, .2))
points(living.gmm2.E$parameters$mean[1,1], living.gmm2.E$parameters$mean[2,1],  pch =8)
points(living.gmm2.E$parameters$mean[1,2], living.gmm2.E$parameters$mean[2,2],  pch =8)
dev.off()

# Code below reproduces parts of:
# Table 10.1: Estimated parameters of a Gaussian mixture model with unconstrained
# class-specific covariance matrices; cost-of-living data
living.gmm2$parameters

# Code below reproduces parts of:
# Table 10.2: Estimated parameters of a Gaussian mixture model with a
# common class-specific covariance matrix; cost-of-living data
living.gmm2.E$parameters

# GMM with 2 classes ("VVI": diagonal different covariance matrices)
living.gmm2.D <- mclust::Mclust(living.data[,-(1:2)], G = 2, modelNames = "VVI")
# GMM with 2 classes ("EEI": diagonal equal covariance matrices)
living.gmm2.DE <- mclust::Mclust(living.data[,-(1:2)], G = 2, modelNames = "EEI")

# GMM with 3 classes ("VVV": different covariance matrices)
living.gmm3 <- mclust::Mclust(living.data[,-(1:2)], G = 3, modelNames = "VVV")
# GMM with 3 classes ("EEE": equal covariance matrices)
living.gmm3.E <- mclust::Mclust(living.data[,-(1:2)], G = 3, modelNames = "EEE")
# GMM with 3 classes ("VVI": diagonal different covariance matrices)
living.gmm3.D <- mclust::Mclust(living.data[,-(1:2)], G = 3, modelNames = "VVI")
# GMM with 3 classes ("EEI": diagonal equal covariance matrices)
living.gmm3.DE <- mclust::Mclust(living.data[,-(1:2)], G = 3, modelNames = "EEI")

# Computing BIC (by hand):
round(-2 * living.gmm2$loglik + log(888) * living.gmm2$df)
round(-2 * living.gmm2.E$loglik + log(888) * living.gmm2.E$df)
round(-2 * living.gmm2.D$loglik + log(888) * living.gmm2.D$df)
round(-2 * living.gmm2.DE$loglik + log(888) * living.gmm2.DE$df)
round(-2 * living.gmm3$loglik + log(888) * living.gmm3$df)
round(-2 * living.gmm3.E$loglik + log(888) * living.gmm3.E$df)
round(-2 * living.gmm3.D$loglik + log(888) * living.gmm3.D$df)
round(-2 * living.gmm3.DE$loglik + log(888) * living.gmm3.DE$df)

# Model parameters for GMM with 3 classes (class-specific covariance)
living.gmm3$parameters

# Code below reproduces:
# Table 10.3: BIC values for eight Gaussian mixture models with different
# numbers of latent classes and covariance matrix assumptions, cost-of-living data
data.frame(Model = paste0("Model ",1:8),
           BIC = c(
            BIC(living.gmm2), BIC(living.gmm3),
            BIC(living.gmm2.E), BIC(living.gmm3.D),
            BIC(living.gmm3.E), BIC(living.gmm2.D),
            BIC(living.gmm2.DE), BIC(living.gmm3.DE)
           ))

# Code below reproduces:
# Table 10.4: Estimated allocation probabilities for 15 randomly selected
# cities; cost-of-living data
set.seed(1)
data.frame(living.data$data.city, living.data$data.country, round(living.gmm2$z,2))[sample(1:888,20)[1:15],]
