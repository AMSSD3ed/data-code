# Chapter 10: Models with Categorical Latent Variables
# Attitude to Abortion Data Example
# Change to appropriate path for file "abortion.dat".

rm(list = ls())

# Install 'poLCA' package if not already installed:
if(!all(c("poLCA") %in% installed.packages())) install.packages(c("poLCA"))
library(poLCA)

# Attitudes to Abortion Data Example
# abortion.file <- "C:/AMSSD/Datasets/Chapter10/abortion.dat"
# The data file path needs to be changed to the file's location on your computer

# Alternatively, choose the file "abortion.dat" directly below:
abortion.file <- file.choose()
abortion.mat <- data.matrix(read.table(abortion.file, skip=0))

colnames(abortion.mat) <- c("WomanDecide", "CoupleDecide",
                            "NotMarried", "CannotAfford")

# Turn the 0,1 coding to 1,2(,...) coding and
# transforming as data.frame, as required by the poLCA package.
abortion.df <- as.data.frame(abortion.mat + 1)

# Latent Class Analysis (LCA) models
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
form <- cbind(WomanDecide,CoupleDecide,NotMarried,CannotAfford) ~ 1

# These use 10 random starts for the estimation algorithm.
set.seed(1234)
abortion.lca2 <- poLCA::poLCA(form,abortion.df,nclass=2,na.rm=F,nrep=10)

# A convenient function for displaying the measurement and class probabilities
LCA.probs <- function(res, digits = 2){
  probs <- res$probs
  ses <- res$probs.se
  item.p <- NULL
  K <- length(res$P)
  for(i in seq_along(probs)){
    p2.tmp <- t(probs[[i]])[2,]
    se.tmp <- t(ses[[i]])[2,]
    tmp <- c(rbind(p2.tmp,se.tmp))
    item.p <- rbind(item.p,tmp)
  }
  item.p <- round(item.p,digits)
  colnames(item.p) <- c(rbind(paste0("Pr(X=1|C=",1:K,")"),"Std.Error"))
  rownames(item.p) <- colnames(res$y)
  class.p <- round(c(rbind(res$P,res$P.se)),digits)
  names(class.p) <- c(rbind(paste0("Class ",1:K),"Std.Error"))
  list(item.probabilities=item.p,class.probabilities=class.p)
}

# The measurement and class probabilities:
# Code below reproduces:
# Table 10.5: Estimated conditional probabilities and prior probabilities for the
# two-class model, attitude to abortion data.
LCA.probs(abortion.lca2)

# Goodness-of-fit indices:
abortion.lca2$Chisq # X^2:
abortion.lca2$Gsq # G^2
2^ncol(abortion.mat) - 2*(ncol(abortion.mat) + 1) # DoF

# A convenient function for two-way margins for LCA models
twowayMargins <- function(df,mod,digits){
  pair <- combn(ncol(df),2)
  coup <- expand.grid(1:2,1:2)
  out <- NULL
  tmp.form <- v1 ~ v2
  for(i in 1:ncol(pair)){
    tmp1 <- c(with(df, table(get(names(df)[pair[1,i]]),
                             get(names(df)[pair[2,i]]))))
    tmp.form <- update(tmp.form, substitute(v1 ~ v2, env = list(v1 = as.name(names(df)[pair[1,i]]),
                                                        v2 = as.name(names(df)[pair[2,i]]))))
    tmp2 <- poLCA::poLCA.table(formula = tmp.form, lc = mod)
    nam1 <- rownames(tmp2); nam1 <- gsub("1","0",nam1); nam1 <- gsub("2","1",nam1)
    nam2 <- colnames(tmp2); nam2 <- gsub("1","0",nam2); nam2 <- gsub("2","1",nam2)
    tmp2 <- c(tmp2)
    tmp <- data.frame(nam1[coup[,1]], nam2[coup[,2]], tmp1,tmp2,
                      round(tmp1-tmp2,digits),
                      round(((tmp1-tmp2)^2)/tmp2,digits))
    out <- rbind(out,tmp)
  }
  names(out) <- c("Item 1", "Item 2", "Obs", "Exp", "O-E","(O-E)^2/E")
  return(out)
}

# Code below reproduces:
# Table 10.6: Chi-Squared Residuals for the Second order margins for a two-class LCA model
# Attitude to abortion data.
twowayMargins(df = abortion.df,mod = abortion.lca2,digits = 2)

# Estimated posterior probabilities for all observations:
abortion.lca2$posterior
# Predicted Class for each observations:
abortion.lca2$predclass

# Code below reproduces:
# Table 10.7: Estimated posterior probabilities of class membership for a two-class LCA model
# Attitude to abortion data.
abortion.pattern <- t(apply(abortion.df-1,1,as.character))
tmp <- NULL
for(i in 1:ncol(abortion.pattern)){
  tmp <- paste0(tmp,abortion.pattern[,i])
}
abortion.pattern <- data.frame(Pattern = tmp, abortion.lca2$posterior, Alo = abortion.lca2$predclass)
abortion.predcla <- aggregate(. ~ Pattern, abortion.pattern, mean)
abortion.predcla$PrClass1 <- round(abortion.predcla$X1,3); abortion.predcla$X1 <- NULL
abortion.predcla$PrClass2 <- round(abortion.predcla$X2,3); abortion.predcla$X2 <- NULL
abortion.predcla$Class <- round(abortion.predcla$Alo,3); abortion.predcla$Alo <- NULL
abortion.predcla

# Code below reproduces parts of:
# Table 10.14: Observed and Expected frequencies under the LCA model
# Attitude to abortion data.
abortion.pattern <- t(apply(abortion.lca2$predcell[,1:4]-1,1,as.character))
tmp <- NULL
for(i in 1:ncol(abortion.pattern)){
  tmp <- paste0(tmp,abortion.pattern[,i])
}
abortion.pattern <- data.frame(Pattern = tmp, Obs = abortion.lca2$predcell$observed,
                               Exp = abortion.lca2$predcell$expected)
abortion.predcla <- aggregate(. ~ Pattern, abortion.pattern, mean)
