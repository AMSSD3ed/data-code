# Chapter 10: Models with Categorical Latent Variables
# Attitudes to Science and Technology Example (1992 Eurobarometer Survey from GB)
# Change to appropriate path for file "science.dat".

rm(list = ls())

# Attitudes to Science & Technology Data Example (1-factor)
# science.file <- "C:/AMSSD/Datasets/Chapter10/science.dat"
# The data file path needs to be changed to the file's location on your computer

# Alternatively, choose the file "science.dat" directly below:
science.file <- file.choose()
science.mat <- data.matrix(read.table(science.file, skip=3))

colnames(science.mat) <- c("Comfort", "Environment", "Work", "Future",
                           "Technology", "Industry", "Benefit")

# Turn the 0,1 coding to 1,2(,...) coding and
# transforming as data.frame, as required by the poLCA package.
science.df <- as.data.frame(science.mat + 1)

# Latent Class Analysis (LCA) models
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
form <- cbind(Comfort,Environment,Work,Future,Technology,Industry,Benefit) ~ 1

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Two-Class LCA for the Attitudes to Science and Technology data
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# These use 10 random starts for the estimation algorithm.
set.seed(1234)
science.lca2 <- poLCA::poLCA(form,science.df,nclass=2,na.rm=F,nrep=10)

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
# Table 10.8: Estimated conditional probabilities and prior probabilities for the
# two-class model, Attitudes to Science and Technology data.
LCA.probs(science.lca2,3)

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
    nam1 <- rownames(tmp2); nam1 <- gsub(" 1"," 0",nam1); nam1 <- gsub(" 2"," 1",nam1)
    nam2 <- colnames(tmp2); nam2 <- gsub(" 1"," 0",nam2); nam2 <- gsub(" 2"," 1",nam2)
    tmp2 <- c(tmp2)
    tmp <- data.frame(nam1[coup[,1]], nam2[coup[,2]], tmp1,tmp2,
                      round(tmp1-tmp2,digits),
                      round(((tmp1-tmp2)^2)/tmp2,digits))
    out <- rbind(out,tmp)
  }
  names(out) <- c("Item 1", "Item 2", "Obs", "Exp", "O-E","(O-E)^2/E")
  return(out)
}

# Code below reproduces parts of:
# Table 10.9: Chi-Squared Residuals (>3) for the Second order margins for a two-class LCA model
# Attitude Science and Technology data.
science.twoway <- twowayMargins(df = science.df,mod = science.lca2, digits = 2)
science.twoway[science.twoway$`(O-E)^2/E` > 3,]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Goodness-of-fit (Two-Class Model)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Goodness-of-fit indices:
science.lca2$Chisq # X^2:
science.lca2$Gsq # G^2

# Observed frequencies
science.obs <- science.lca2$predcell$observed
# Expected frequencies:
science.exp <- science.lca2$predcell$expected

# Effective number of response patterns (aggregating expected < 5)
science.obs.pool <- c(science.obs[science.exp >= 5], sum(science.obs[science.exp < 5]))
science.exp.pool <- c(science.exp[science.exp >= 5], sum(science.exp[science.exp < 5]))
# Chi-Squared (X^2) Statistic
science.chisq.pool <- sum((science.obs.pool-science.exp.pool)^2/science.exp.pool); science.chisq.pool
# Pooled degrees of freedom
dof = length(science.obs.pool) - 2*(ncol(science.mat)+1)
1-pchisq(science.chisq.pool, dof) #0.01
# G-Squared (G^2) Statistic
science.g2.pool <- 2*sum(science.obs.pool*log(science.obs.pool/science.exp.pool)); science.g2.pool
1-pchisq(science.g2.pool, dof) #0.01

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Three-Class LCA for the Attitudes to Science and Technology data
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

set.seed(4567)
science.lca3 <- poLCA::poLCA(form,science.df,nclass=3,na.rm=F,nrep=10)

# The measurement and class probabilities:
# Code below reproduces:
# Table 10.10: Estimated conditional probabilities and prior probabilities for the
# three-class model, Attitudes to Science and Technology data.
LCA.probs(science.lca3, 3)
