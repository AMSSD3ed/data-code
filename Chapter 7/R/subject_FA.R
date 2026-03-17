# Chapter 7: Factor Analysis
# Subject Marks Example
# Change to appropriate path for file "subject.txt".

rm(list = ls())

# Subject Marks Example
# subject.file <- "C:/AMSSD/Datasets/Chapter7/subject.txt"
# The data file path needs to be changed to the file's location on your computer

# Alternatively, choose the file "subject.txt" directly below:
subject.file <- file.choose()

n <-  max(count.fields(subject.file))
subject.mat <- data.matrix(read.table(subject.file, fill=TRUE, col.names=1:n))
colnames(subject.mat) <- c("Gaelic", "English", "History", 
                           "Arithmetic", "Algebra", "Geometry")

# fill in the upper diagonal to obtain the symmetric matrix
subject.mat[upper.tri(subject.mat)] <- 0
subject.mat <- subject.mat + t(subject.mat) - diag(diag(subject.mat))

# Principal Components Analysis (PCA)
subject.pca <- princomp(covmat=subject.mat)
summary(subject.pca)

## Vertical format
round(cbind("Component"=1:n,
            "Variance"=subject.pca$sdev^2,
            "% Variance"=100*subject.pca$sdev^2/sum(subject.pca$sdev^2),
            "Cumulative % Variance"=
            100*cumsum(subject.pca$sdev^2)/sum(subject.pca$sdev^2)),
      d=2)


# Factor Analysis
# Rotation is VARIMAX by default, specify "none" for no rotation
subject.fa <- factanal(covmat=subject.mat, factors=2, rotation="none", n.obs=220)
# Explore the individuals outputs in "subject.fa" object
names(subject.fa)
# Print results
subject.fa                                                                                                                                                                          

# Table 7.2: Estimated factor loadings from a 2-factor model, Subjects marks data
print(subject.fa$loadings, cutoff = 0.0, digits = 2)

# Table 7.4: Communalities from a linear 2-factor model, Subjects marks data
print(data.frame("Subject" = colnames(subject.mat),
                 "Communalities" = rowSums(subject.fa$loadings^2)), digits = 2)
# Alternative way of computing the communalities:
round(cbind("Communalities"=1-subject.fa$uniquenesses), d=2)

# Figure 7.2: Plot of unrotated factor loadings, Subjects marks data
plot(loadings(subject.fa), xlim=c(-1, 1), ylim=c(-1, 1), 
     pch=16, bty="n", axes=FALSE, main="Unrotated loadings")
axis(1, at=c(-1, 1), pos=0)
axis(2, at=c(-1, 1), pos=0, las=2)
text(loadings(subject.fa), labels=colnames(subject.mat), pos=3, cex=0.75)

# Reproduced correlation
# Table 7.5a: Reproduced correlations and communalities (in diagonal)
subject.rcor <- tcrossprod(loadings(subject.fa)) + diag(subject.fa$uniquenesses)
diag(subject.rcor) <- 1-subject.fa$uniquenesses
colnames(subject.rcor) <- rownames(subject.rcor) <- names(subject.fa$uniquenesses)
round(subject.rcor, d = 2)

# Residual matrix
# Table 7.5b: Discrepancies between observed and fitted correlation matrix
subject.resid <- subject.mat - subject.rcor
rownames(subject.resid) <- colnames(subject.resid)
round(subject.resid, d = 2)

# Table 7.5: Reproduced correlations and communalities, and discrepancies between observed and fitted correlation matrix
round(rbind(subject.rcor, subject.resid), d = 2)

# Factor score coefficients (not provided by factanal, but can be computed)
# E.g., when the factors are orthogonal:
fscore.coef <- solve(subject.fa$correlation) %*% subject.fa$loadings

# Table 7.8: Coefficients for calculating factor scores (regression method), Subjects marks data.
round(fscore.coef, d = 2)

# You can calculate the predicted factor scores for rawdata as
# scale(rawdata, means, sds) %*% fscore.coef
# where the means and sds are from the raw data used for the factor analysis

# If you have raw data, use scores="regression" option in factanal
# to compute factor scores

if(!"GPArotation" %in% installed.packages()) install.packages("GPArotation")

library(GPArotation)

# If FA object exists in memory already:
subject.fa <- update(subject.fa, rotation="oblimin")
subject.fa

# New FA object (re-fit with OBLIMIN rotation):
subject.fa <- factanal(covmat=subject.mat, factors=2, rotation="oblimin", n.obs=220)
subject.fa