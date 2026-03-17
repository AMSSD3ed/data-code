
## Subject Marks
subject.file <- "c:/AMDSS/Rwork/code/chapter7/subject.txt"
# the data file path needs to be changed to the file's location on your computer

n <-  max(count.fields(subject.file))
subject.mat <- data.matrix(read.table(subject.file, fill=TRUE, col.names=1:n))
colnames(subject.mat) <- c("Gaelic", "English", "History", 
                           "Arithmetic", "Algebra", "Geometry")

# fill in the upper diagonal to obtain the symmetric matrix
subject.mat[upper.tri(subject.mat)] <- 0
subject.mat <- subject.mat + t(subject.mat) - diag(diag(subject.mat))

#principal components analysis

subject.pca <- princomp(covmat=subject.mat)
summary(subject.pca)

## Vertical format
round(cbind("Component"=1:n,
            "Variance"=subject.pca$sdev^2,
            "% Variance"=100*subject.pca$sdev^2/sum(subject.pca$sdev^2),
            "Cumulative % Variance"=
            100*cumsum(subject.pca$sdev^2)/sum(subject.pca$sdev^2)),
      d=2)


plot(subject.pca$sdev^2, type="b", pch=16, xlab="Component number", 
     ylab="Eigenvalue", main="Scree plot for the subject scores data")


# factor analysis
# rotation is varimax by default, specify "none" for no rotation
subject.fa <- factanal(covmat=subject.mat, factors=2, rotation="none", n.obs=220)
class(subject.fa)
names(subject.fa)
subject.fa                                                                                                                                                                          

subject.fa$STATISTIC
subject.fa$PVAL
subject.fa$dof
                 
print(subject.fa, cutoff=0.0)


plot(loadings(subject.fa), xlim=c(-1, 1), ylim=c(-1, 1), 
     pch=16, bty="n", axes=FALSE, main="Unrotated loadings")
axis(1, at=c(-1, 1), pos=0)
axis(2, at=c(-1, 1), pos=0, las=2)
text(loadings(subject.fa), labels=colnames(subject.mat), pos=3, cex=0.75)

# two ways to compute communalities
round(cbind("Communalities"=rowSums(loadings(subject.fa)^2)), d=2)
round(cbind("Communalities"=1-subject.fa$uniquenesses), d=2)


## Reproduced correlation
subject.rcor <- tcrossprod(loadings(subject.fa)) + diag(subject.fa$uniquenesses)
round(subject.rcor, d=2)

## Residual matrix
subject.resid <- subject.mat - subject.rcor
subject.resid
rownames(subject.resid) <- colnames(subject.resid)
round(subject.resid, d=2)
round(subject.resid[lower.tri(subject.resid)], d=2)

# Factor score coefficients (not provided by factanal, but can be computed)
# E.g., when the factors are orthogonal:
fscore.coef <- solve(subject.fa$correlation) %*% subject.fa$loadings

# You can calculate the predicted factor scores for rawdata as
# scale(rawdata, means, sds) %*% fscore.coef
# where the means and sds are from the raw data used for the factor analysis

# If you have raw data, use scores="regression" option in factanal
# to compute factor scores

install.packages("GPArotation")

library(GPArotation)
?rotations
subject.fa <- update(subject.fa, rotation="oblimin")
subject.fa

subject.fa <- factanal(covmat=subject.mat, factors=2, rotation="oblimin", n.obs=220)
subject.fa
