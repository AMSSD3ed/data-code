# Chapter 14: Longitudinal Data Analysis
# Physical Health Functioning Example (Whitehall II study, 2005)

rm(list = ls())

# Install 'lme4' and 'dplyr' packages if not already installed:
if(!all(c("lme4","dplyr","haven","ggplot2","lattice") %in% installed.packages())){
  install.packages(c("lme4","dplyr","haven","ggplot2","lattice"))
} 
library(lme4)
library(haven)
library(ggplot2)
library(lattice)

# Physical Health Functioning Example
# phf.file <- "C:/AMSSD/Datasets/Chapter14/phf.dta"
# The data file path needs to be changed to the file's location on your computer

# Alternatively, choose the file "abortion.dat" directly below:
phf.file <- file.choose()
phf.mat <- haven::read_dta(file = phf.file)

# Input data (long format)
names(phf.mat)

# Code below reproduces:
# Table 14.3: An extract of the physical functioning dataset in long form
head(phf.mat[,c("id","occ","age","phf")], 13)

summary(phf.mat)
# Physical functioning trajectories for first 4 individuals (for Figure 14.1)
phf4.mat <- phf.mat[phf.mat$id<=4,]

# Code below reproduces:
# Figure 14.1: Physical functioning trajectories for four individuals
lattice::xyplot(phf ~ age, data = phf4.mat, groups=id, 
       type=c("p","l"), col=c("black","black"),pch=19, cex=0.5,
       scales=list(x=seq(40,70),tick.number=5, cex=0.7),
       xlab=list("Age (years)",cex=0.8),
       ylab=list("Physical functioning",cex=0.8))


# Latent growth curve model (LGCM) for Physical Health Functioning Example
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Code below reproduces parts of:
# Table 14.4: Linear random intercept and random slope growth models for physical health functioning
# Table 14.4 (a): Random intercept
phf.mat$age50 <- phf.mat$age-50
phf.lcm.ri <- lmer(phf ~ age50 + (1 | id), data = phf.mat, REML = FALSE)
summary(phf.lcm.ri)
phf.mat$predri <- fitted(phf.lcm.ri)
# Table 14.4 (b): Random slope
phf.lcm.rs <- lmer(phf ~ age50 + (1 + age50 | id), data = phf.mat, REML = FALSE,
                   control = lmerControl(optimizer ="Nelder_Mead"))
summary(phf.lcm.rs)
phf.mat$predrs <- fitted(phf.lcm.rs)

# Update phf4.mat object
phf4.mat <- phf.mat[phf.mat$id<=4,]

# Code below reproduces:
# Figure 14.2: Fitted physical health trajectories for four individuals from random slope and random intercept models
lattice:: xyplot(predrs + predri + phf ~ age| id, data = phf4.mat, distribute.type=TRUE, 
                 strip=FALSE,
                 type=c("l","l","p"), pch=1, lty=c("solid","dashed"),
                 col=c("black","black","black"), cex=0.5,
                 scales=list(x=seq(40,70),tick.number=5, cex=0.7),
                 xlab=list("Age (years)",cex=0.8),
                 ylab=list("Physical functioning",cex=0.8),
                 key=list(
                   border=TRUE,
                   space="top", columns=1,
                   text=list(c("Predicted (random slope)", "Predicted (random intercept)", 
                               "Observed"), cex=0.7),
                   lines=list(type=c("l","l","p"), pch=1, lty=c("solid","dashed"),
                              col=c("black","black","black"))))

# Code below reproduces:
# Figure 14.3: Estimated between-individual variance in physical health functioning from random slope model
sigma2.df <- as.data.frame(lme4::VarCorr(phf.lcm.rs),comp="Variance")
sigma2u0 <- sigma2.df$vcov[1]
sigma2u1 <- sigma2.df$vcov[2]
sigmau01 <- sigma2.df$vcov[3]
sigma2e <- sigma2.df$vcov[4]
t <- c(55:70)
indvar <- sigma2u0 + 2*sigmau01*(t-50) + sigma2u1*(t-50)^2

lattice::xyplot(indvar ~ t, type=c("l"), col="black",
                scales=list(x=seq(55,70,5), tick.number=4,cex=0.8),
                ylab="Between-individual variance",
                xlab="Age (years)")

# Code below reproduces:
# Figure 14.4: Mean physical health trajectory with 95% plausible values range
beta <- as.matrix(lme4::fixef(phf.lcm.rs))
beta0 <- beta[1,]
beta1 <- beta[2,]
predmeanphf <- beta0 + beta1*(t-50)
plrangelow <- predmeanphf - 1.96*sqrt(indvar)
plrangehi <- predmeanphf + 1.96*sqrt(indvar)

library(ggplot2)
ggplot() +
  geom_line(aes(x=t,y=plrangehi),col="black",linetype="dashed") + 
  geom_line(aes(x=t,y=predmeanphf),col="black") + 
  geom_line(aes(x=t,y=plrangelow),col="black",linetype="dashed") +
  ylab('Predicted physical functioning')+xlab('Age (years)') + 
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        panel.background = element_rect(fill = "white", colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# Functions to compute Intra-individual correlations
var_y_t <- function(ti){
  sigma2u0 + 2*sigmau01*ti + sigma2u1*ti^2 + sigma2e
}

cov_y_t1t2 <- function(t1,t2){
  sigma2u0 + sigmau01*(t1+t2) + sigma2u1*t1*t2
}

cor_y_t1t2 <- function(t1,t2){
  cov_y_t1t2(t1,t2)/sqrt(var_y_t(t1)*var_y_t(t2))
}

# Code below reproduces:
# Table 14.5: Estimates of the within-person correlations implied by the random slope model
# of Table 14.4 for two individuals observed at four different ages (t_ij),
# Physical Health Functioning example

# Table 14.5 (a): Individual # 12
T1 <- outer(unlist(phf.mat[phf.mat$id == 12,"age"])-50,
            unlist(phf.mat[phf.mat$id == 12,"age"])-50,
            FUN = cor_y_t1t2)
diag(T1) <- 1
round(T1,3)

# Table 14.5 (b): Individual # 1466
T2 <- outer(unlist(phf.mat[phf.mat$id == 1466,"age"])-50,
            unlist(phf.mat[phf.mat$id == 1466,"age"])-50,
            FUN = cor_y_t1t2)
diag(T2) <- 1
round(T2,3)

# Non-linear Latent growth curve model (LGCM) for Physical Health Functioning Example
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

phf.lcm.rs <- lmer(phf ~ age50 + (1 + age50 | id), data = phf.mat, REML = FALSE,
                   control = lmerControl(optimizer ="Nelder_Mead"))
summary(phf.lcm.rs)
phf.mat$predrs <- fitted(phf.lcm.rs)