# Chapter 14: Longitudinal Data Analysis
# Cigarette Smoking in Britain Example (Data from BHPS, 1991-2002, 12 waves)
# Subset of 1112 respondents who were smoking (ncigs > 0) in 1991 and responded at each wave

rm(list = ls())

# Install required packages if not already installed:
if(!all(c("lme4","dplyr","haven","ggplot2","lattice", "flexmix") %in% installed.packages())){
  install.packages(c("lme4","dplyr","haven","ggplot2","lattice", "flexmix"))
} 
library(lme4)
library(haven)
library(ggplot2)
library(lattice)
library(flexmix)

# Cigarette Smoking in Britain Example
# smokeGB.file <- "C:/AMSSD/Datasets/Chapter14/cigBritain.dta"
# The data file path needs to be changed to the file's location on your computer

# Alternatively, choose the file "cigGB.dta" directly below:
smokeGB.file <- file.choose()
smokeGB.mat <- haven::read_dta(file = smokeGB.file)

# Input data (long format)
names(smokeGB.mat)
head(smokeGB.mat[,], 13)

# Latent Growth Curve Models
# (Results not reported in book)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Random intercept model
smokeGB.mri <- lme4::lmer(ncigs ~ wave + (1 | pid), data = smokeGB.mat, REML = FALSE,
                          control = lmerControl(optimizer ="Nelder_Mead"))

# Random slope model
smokeGB.mrs <- lme4::lmer(ncigs ~ wave + (1 + wave | pid), data = smokeGB.mat, REML = FALSE,
                          control = lmerControl(optimizer ="Nelder_Mead"))

anova(smokeGB.mri, smokeGB.mrs)

# Quadratic model (fixed year effect)
# Model with random slope on wavesq does not converge
smokeGB.mat$wavesq <- smokeGB.mat$wave^2
smokeGB.mrsq <- lmer(ncigs ~ wave + wavesq + (1 + wave | pid), data = smokeGB.mat, REML = FALSE,
                     control = lmerControl(optimizer ="Nelder_Mead"))
anova(smokeGB.mrs, smokeGB.mrsq)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Latent Class Growth Analysis (LCGA)
# (Assume residual variance same for each class)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

smokeGB.lcgaq <- flexmix::stepFlexmix( ncigs ~ wave + wavesq | pid, data = smokeGB.mat, 
                                       k=1:6, nrep=20, 
                                       model = flexmix::FLXMRglmfix(family="gaussian", varFix=TRUE),
                                       control = list(iter.max = 500, minprior = 0))

# Code below reproduces parts of:
# Table 14.6(a): Results from LCGA of Smoking Data with quadratic effects
# of calendar time, number of classes C = 1 to 6
smokeGB.lcgaq

# Latent class probabilities for each model (C = 1 to 6)
# Note: Latent classes may be in a different order to those in Table 14.6
# Code below reproduces parts of:
# Table 14.6(b): Results from LCGA of Smoking Data with quadratic effects
# of calendar time, number of classes C = 1 to 6
for (c in 2:6) {
  print(summary(flexmix::getModel(smokeGB.lcgaq, which=c)))
}

# Results for 6-class model
smokeGB.lcgaq6 <- flexmix::getModel(smokeGB.lcgaq, which=6)
summary(smokeGB.lcgaq6) # Smallest class contains 132/12 = 12 individuals

# Results for 5-class model
smokeGB.lcgaq5 <- flexmix::getModel(smokeGB.lcgaq, which=5)
summary(smokeGB.lcgaq5) # Smallest class contains 1092/12 = 91 individuals

# Further analysis of 5-class solution
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Create smokeGB.results.df data frame for the 5-class quadratic model containing:
# ypred1-ypred5: predicted y given class 
# class: predicted class (highest posterior probability | y) 
# p1-p5: predicted posterior probability of class membership | y
pred <- data.frame(flexmix::predict(smokeGB.lcgaq5, smokeGB.mat))
colnames(pred) <- c("ypred1","ypred2","ypred3","ypred4","ypred5")
clust <- data.frame(flexmix::clusters(smokeGB.lcgaq5, data.frame(smokeGB.mat)))
colnames(clust) <- c("class")
pprob <- data.frame(flexmix::posterior(smokeGB.lcgaq5, data.frame(smokeGB.mat)))
colnames(pprob) <- c("p1","p2","p3","p4","p5")
smokeGB.results.df <- cbind(smokeGB.mat,pred,clust,pprob)

# Create individual-level dataset with predicted class and posterior probabilities 
# All constant within individuals
library(dplyr)
smokeGB.results.l2.df <- smokeGB.results.df %>% group_by(pid) %>%
  summarise(pid=first(pid), nwave=n(), class=first(class),
            p1=first(p1), p2=first(p2), p3=first(p3), p4=first(p4),
            p5=first(p5))
smokeGB.results.l2.df <- round(smokeGB.results.l2.df,3)

# Number of individuals per class
table(smokeGB.results.l2.df$class)

# Average predicted probability of class membership in each class
smokeGB.results.classprob <- smokeGB.results.l2.df %>% group_by(class) %>%
  summarise(meanp1=mean(p1), meanp2=mean(p2), meanp3=mean(p3), 
            meanp4=mean(p4), meanp5=mean(p5))
smokeGB.results.classprob

# Select records for an individual (pid==10058257) observed at each occasion
# (as ypred1-ypred5 identical for each individual)
smokeGB.results.ind1.df <- subset(smokeGB.results.df, pid==10058257)

# Code below reproduces:
# Figure 14.5: Predicted smoking trajectories from 5-class quadratic LCGA model
lattice::xyplot(ypred1 + ypred2 + ypred3 + ypred4 + ypred5 ~ wave, data=smokeGB.results.ind1.df, 
                type=c("l","l","l","l","l"), lwd=2,
                lty=c("solid","dashed","dotted","dotdash","longdash"),
                col=c("black","black","black","black","black"),
                xlab="Wave", ylab="Predicted number of cigarettes per day",
                scales=list(x=seq(1,12,1),tick.number=12),
                key=list(
                  columns=2, text=list(c("Class 1", "Class 2", "Class 3", "Class 4", "Class 5")),
                  lines=list(lwd=2, 
                             lty=c("solid","dashed","dotted","dotdash","longdash"),
                             col=c("black","black","black","black","black"))
                )       
)
# Note: classes may be in a different order

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Growth mixture model (GMM)
# (Assume residual variance same for each class)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Random intercept models with 1-4 classes 
# Quadratic (in wave in fixed part) - [it takes time to run]
smokeGB.gmmriq <- flexmix::stepFlexmix( . ~ . | pid, data = smokeGB.mat, 
                                k=1:4, nrep=20, 
                                model = flexmix::FLXMRlmm(ncigs ~ wave + wavesq,
                                                          random = ~ 1,
                                                          varFix=c(Random=FALSE, Residual=TRUE)),
                                control = list(iter.max = 500, minprior = 0))
smokeGB.gmmriq

# Random slope models for 1 class
# Quadratic (in wave in fixed part) - [it takes time to run]
smokeGB.gmmrsq1 <- flexmix::stepFlexmix( . ~ . | pid, data = smokeGB.mat, 
                       k=1:1, nrep=10, 
                       model = flexmix::FLXMRlmm(ncigs ~ wave + wavesq,
                                        random = ~ 1 + wave,
                                        varFix=c(Random=FALSE, Residual=TRUE)),
                       control = list(iter.max = 500, minprior = 0))
summary(smokeGB.gmmrsq1)
parameters(smokeGB.gmmrsq1)
# Small differences in random part parameter estimates from lme4
summary(smokeGB.mrsq)

# Code below reproduces:
# Table 14.7: Results from random slope growth mixture models of smoking
# with quadratic effects of calendar time, number of classes C = 1 to 4

# Random slope models for 2-4 classes
# Quadratic (in wave in fixed part) - [it takes time to run]
smokeGB.gmmrsq2to4 <- flexmix::stepFlexmix( . ~ . | pid, data = smokeGB.mat, 
                                    k=2:4, nrep=10, 
                                    model = flexmix::FLXMRlmm(ncigs ~ wave + wavesq,
                                                              random = ~ 1 + wave,
                                                              varFix=c(Random=FALSE, Residual=TRUE)),
                                    control = list(iter.max = 500, minprior = 0))
smokeGB.gmmrsq2to4 

for (c in 1:3) {
  print(summary(flexmix::getModel(smokeGB.gmmrsq2to4, which=c)))
}

# Extract results from 4-class model
smokeGB.gmmrsq4 <- flexmix::getModel(smokeGB.gmmrsq2to4, which=3)
summary(smokeGB.gmmrsq4)

# Code below reproduces:
# Table 14.8: Parameter estimates from 4-class growth mixture model of
# smoking with quadratic effect of calendar time
parameters(smokeGB.gmmrsq4)

# Create smokeGB.results.df data frame for the 4-class quadratic model containing:
# ypred1-ypred4: predicted y given class 
# class: predicted class (highest posterior probability | y) 
# p1-p4: predicted posterior probability of class membership | y
pred <- data.frame(predict(smokeGB.gmmrsq4, smokeGB.mat))
colnames(pred) <- c("ypred1","ypred2","ypred3","ypred4")
clust <- data.frame(clusters(smokeGB.gmmrsq4, data.frame(smokeGB.mat)))
colnames(clust) <- c("class")
pprob <- data.frame(posterior(smokeGB.gmmrsq4, data.frame(smokeGB.mat)))
colnames(pprob) <- c("p1","p2","p3","p4")
smokeGB.results.df <- cbind(smokeGB.mat,pred,clust,pprob)

# Create individual-level dataset with predicted class and posterior probabilities 
# All constant within individuals
smokeGB.results.l2.df <- smokeGB.results.df %>% group_by(pid) %>%
  summarise(pid=first(pid), nwave=n(), class=first(class),
            p1=first(p1), p2=first(p2), p3=first(p3), p4=first(p4))
smokeGB.results.l2.df <- round(smokeGB.results.l2.df,3)

# Number of individuals per class
table(smokeGB.results.l2.df$class)

# Average predicted probability of class membership in each class
smokeGB.classprob <- smokeGB.results.l2.df %>% group_by(class) %>%
  summarise(meanp1=mean(p1), meanp2=mean(p2), meanp3=mean(p3), 
            meanp4=mean(p4))
smokeGB.classprob

# Plot of predicted mean trajectories by class
# Select records for an individual observed at each occasion
# (ypred1-ypred5 identical for each individual)
smokeGB.results.ind1.df <- subset(smokeGB.results.df, pid==10058257)

# Code below reproduces:
# Figure 14.6: Predicted average smoking trajectories from 4-class
# quadratic random slope GMM
lattice::xyplot(ypred1 + ypred2 + ypred3 + ypred4 ~ wave, data=smokeGB.results.ind1.df, 
                type=c("l","l","l","l"), lwd=2,
                lty=c("solid","dashed","dotted","dotdash"),
                col=c("black","black","black","black"),
                xlab=list(label = "Year", cex = 0.8), 
                ylab=list(label="Predicted number of cigarettes per day", cex=0.8),
                scales=list(x=seq(1,12,1),tick.number=12),
                key=list(
                  space="bottom",
                  columns=2, text=list(c("Class 1 (11%)", "Class 2 (10%)", 
                                         "Class 3 (14%)", "Class 4 (65%)")),
                  cex=0.7,
                  lines=list(lwd=2, 
                             lty=c("solid","dashed","dotted","dotdash"),
                             col=c("black","black","black","black"))
                )       
)

# Results used in book (with data)
# load("cigGB_ResultsChapter14.Rdata")
