# Chapter 5: Principal Component Analysis (PCA)
# European Employment Data Example 
# Change to appropriate path for file "eurojob.sav".

rm(list = ls())
if(!"foreign" %in% installed.packages()) install.packages("foreign")

# eurojob.file <- "C:/MSSD/Datasets/Chapter5/eurojob.sav"
# The data file path needs to be changed to the file's location on your computer

# Alternatively, choose the file "eurojob.sav" directly below:
eurojob.file <- file.choose()

eurojob.data <- foreign::read.spss(eurojob.file, to.data.frame=TRUE)
head(eurojob.data)
eurojob.mat <- data.matrix(subset(eurojob.data, select=-COUNTRY))

eurojob.pca <- princomp(eurojob.mat, cor=TRUE)
class(eurojob.pca)
methods(class=princomp)
summary(eurojob.pca, loadings=TRUE)

# Figure 5.6: Scree plot for European Employment data
plot(eurojob.pca$sdev^2, type="b", pch=16, xlab="Component number", ylab="Eigenvalue",
     main="Scree plot for European Employment data")

# Table 5.6: Variance explained by each component, European Employment Data
round(cbind("Variance"=eurojob.pca$sdev^2,
            "%"=100*eurojob.pca$sdev^2/sum(eurojob.pca$sdev^2),
            "Cumulative %"=100*cumsum(eurojob.pca$sdev^2)/sum(eurojob.pca$sdev^2)), d=2)

# Table 5.7: Loadings (a-star values) for first 3 components, European Employment Data
a.star <- eurojob.pca$loadings*rep(eurojob.pca$sdev, each=length(eurojob.pca$sdev))
print(round(a.star[,1:3], d=2), cutoff=0)

# Figure 5.7: Plot of Loadings for first two components, European Employment Data
plot(a.star[, 1], -1*a.star[, 2], axes=FALSE, xlim=c(-1, 1), ylim=c(-1, 1), xlab="", ylab="")
mtext(expression(a[i1]^"*"), 2, las=1)
mtext(expression(a[i2]^"*"), 3, las=1)
axis(1, at=c(-1, 1), pos=0)
axis(2, at=c(-1, 1), pos=0)
text(a.star[, 1], -1*a.star[, 2], labels=colnames(eurojob.mat), pos=3, cex=0.5)

# Table 5.9: Component score coefficients for first two components, European Employment Data
comp.coef <- eurojob.pca$loadings/rep(eurojob.pca$sdev, each=length(eurojob.pca$sdev))
print(round(comp.coef[,1:2], d=2), cutoff=0)

# Figure 5.8: 26 European countries plotted using standardized principal components scores, European Employment Data
std.scores <- scale(eurojob.mat) %*% (comp.coef[, 1:2] %*% diag(c(1, -1)))
plot(std.scores[, 1], std.scores[, 2], xlab="", ylab="", las=1, xlim=c(-4, 2), ylim=c(-2, 3), cex.axis=0.75)
mtext(expression(tilde(y)[1]), side=1, line=2.1, las=1)
mtext(expression(tilde(y)[2]), side=2, line=2.1, las=1)
text(std.scores[, 1], std.scores[, 2], labels=eurojob.data$COUNTRY, pos=3, cex=0.5)


