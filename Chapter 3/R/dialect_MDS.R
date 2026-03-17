# Chapter 3: Multidimensional Scaling (MDS)
# English Dialects

rm(list = ls())

# dialect.file <- "~/AMSSD/Datasets/dialect.txt"
# the data file path needs to be changed to the file's location on your computer

# Alternatively, choose the file "dialect.txt" directly below:
dialect.file <- file.choose()

n <- max(count.fields(dialect.file))
dialect.mat <- data.matrix(read.table(dialect.file, fill=TRUE, col.names=1:n))
dialect.dist <- as.dist(100 - dialect.mat)

# library(MASS)
sims <- 5
stress.mds <- sapply(1:sims, function(i) MASS::isoMDS(dialect.dist, k=i)$stress)
plot(stress.mds/100, xlab="Number of dimensions", ylab="Stress",
     type="b", pch=16, main="Scree-Plot", ylim=c(0, max(stress.mds/100)))

# MDS for K = 2
dialect.mds <- MASS::isoMDS(dialect.dist, k=2)

# Code below reproduces: Figure 3.16: Two-dimensional representation of the 25 English Villages (map)
plot(dialect.mds$points, xlab="Dimension 1", ylab="Dimension 2",
     main="Derived Configuration (2D MDS)")
text(dialect.mds$points, labels=rownames(dialect.mds$points),
     pos=3, offset=0.3, cex=0.75)
abline(h=0, v=0, col="gray")

# fitted vs raw
dialect.sh <- MASS::Shepard(dialect.dist, dialect.mds$points)
observations <- dialect.sh$x
distances <- dialect.sh$y
disparities <- dialect.sh$yf
plot(observations, disparities, type="l")
points(observations, disparities)
plot(observations, distances)

# Code below reproduces: Figure 3.18: Plot of distances vs. fitted distances (disparities) for English Dialects Data
plot(disparities, distances)

# Last batch of commands require the package "spatstat"
if(!"spatstat" %in% installed.packages()[,"Package"]) install.packages("spatstat")

# spatstat.geom::rotate() allows for easy rotation
# library(spatstat)

p1 <- spatstat.geom::ppp(dialect.mds$points[,1], dialect.mds$points[,2],
                    c(min(dialect.mds$points[,1])-10, max(dialect.mds$points[,1])+10),
                    c(min(dialect.mds$points[,2])-10, max(dialect.mds$points[,2])+10))
p2 <- spatstat.geom::rotate(p1, 3*pi/2)
p3 <- as.data.frame(cbind(-p2$x, p2$y))
plot(p3, xlab="", ylab="", main="Conventional Rotation")
text(p3, labels=rownames(p3), pos=1)
