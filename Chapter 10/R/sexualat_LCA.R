# Chapter 10: Models with Categorical Latent Variables
# Contemporary Sexual Attitudes Example (1990 British Social Attitudes Survey)
# Change to appropriate path for file "sexualat.dat".

rm(list = ls())

# Sexual Attitudes LCA Example
# sexualat.file <- "C:/AMSSD/Datasets/Chapter10/sexualat.dat"
# The data file path needs to be changed to the file's location on your computer

# Alternatively, choose the file "sexualat.dat" directly below:
sexualat.file <- file.choose()
sexualat.mat <- data.matrix(read.table(sexualat.file, skip=0))

colnames(sexualat.mat) <- c("divorce", "sexdisc", "premar", "exmar",
                            "gaysex", "gayscho", "gayhied", "gaypubl",
                            "gayfadop", "gaymadop")

# Turn the 0,1 coding to 1,2(,...) coding and
# transforming as data.frame, as required by the poLCA package.
sexualat.df <- as.data.frame(sexualat.mat + 1)

# Latent Class Analysis (LCA) models
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
form <- cbind(divorce,sexdisc,premar,exmar,gaysex,gayscho,
              gayhied,gaypubl,gayfadop,gaymadop) ~ 1

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# LCA for the Sexual Attitudes data
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Two-Class Model
# ~~~~~~~~~~~~~~~
sexualat.lca2 <- poLCA::poLCA(form,sexualat.df,nclass=2,na.rm=F,nrep=10)

# A convenient function for displaying the measurement and class probabilities
LCA.probs <- function(res, digits = 2, SE = T){
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
  if(!SE){
    item.p <- item.p[,"Std.Error" != colnames(item.p)]
    class.p <- class.p["Std.Error" != names(class.p)]
  }
  list(item.probabilities=item.p,class.probabilities=class.p)
}

# Three-Class Model
# ~~~~~~~~~~~~~~~
sexualat.lca3 <- poLCA::poLCA(form,sexualat.df,nclass=3,na.rm=F,nrep=10)

# Four-Class Model
# ~~~~~~~~~~~~~~~
sexualat.lca4 <- poLCA::poLCA(form,sexualat.df,nclass=4,na.rm=F,nrep=10)

# Code below reproduces:
# Table 10.11: AIC and BIC, sexual attitudes data
round(data.frame(Class2 = c(sexualat.lca2$aic, sexualat.lca2$bic),
                 Class3 = c(sexualat.lca3$aic, sexualat.lca3$bic),
                 Class4 = c(sexualat.lca4$aic, sexualat.lca4$bic)),2)

# Code below reproduces:
# Table 10.12: Estimated conditional probabilities and prior probabilities for the
# two/three/four-class model, Sexual Attitudes data.
rbind(cbind(LCA.probs(sexualat.lca2,2,F)$item,
            LCA.probs(sexualat.lca3,2,F)$item,
            LCA.probs(sexualat.lca4,2,F)$item),
      c(LCA.probs(sexualat.lca2,2,F)$class,
        LCA.probs(sexualat.lca3,2,F)$class,
        LCA.probs(sexualat.lca4,2,F)$class))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Goodness-of-fit (Two-Class Model)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Observed frequencies
sexualat.obs <- sexualat.lca2$predcell$observed
# Expected frequencies:
sexualat.exp <- sexualat.lca2$predcell$expected

# Effective number of response patterns (aggregating expected < 5)
sexualat.obs.pool <- c(sexualat.obs[sexualat.exp >= 5], sum(sexualat.obs[sexualat.exp < 5]))
sexualat.exp.pool <- c(sexualat.exp[sexualat.exp >= 5], sum(sexualat.exp[sexualat.exp < 5]))
# Chi-Squared (X^2) Statistic
sexualat.chisq.pool <- sum((sexualat.obs.pool-sexualat.exp.pool)^2/sexualat.exp.pool); sexualat.chisq.pool
# Pooled degrees of freedom
dof = length(sexualat.obs.pool) - 2*(ncol(sexualat.df)+1)
1-pchisq(sexualat.chisq.pool, dof) #0.01
# G-Squared (G^2) / Likelihood-ratio Statistic
sexualat.g2.pool <- 2*sum(sexualat.obs.pool*log(sexualat.obs.pool/sexualat.exp.pool)); sexualat.g2.pool
1-pchisq(sexualat.g2.pool, dof) #0.01

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Goodness-of-fit (Three-Class Model)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Observed frequencies
sexualat.obs <- sexualat.lca3$predcell$observed
# Expected frequencies:
sexualat.exp <- sexualat.lca3$predcell$expected

# Effective number of response patterns (aggregating expected < 5)
sexualat.obs.pool <- c(sexualat.obs[sexualat.exp >= 5], sum(sexualat.obs[sexualat.exp < 5]))
sexualat.exp.pool <- c(sexualat.exp[sexualat.exp >= 5], sum(sexualat.exp[sexualat.exp < 5]))
# Chi-Squared (X^2) Statistic
sexualat.chisq.pool <- sum((sexualat.obs.pool-sexualat.exp.pool)^2/sexualat.exp.pool); sexualat.chisq.pool
# Pooled degrees of freedom
dof = length(sexualat.obs.pool) - 3*(ncol(sexualat.df)+1)
1-pchisq(sexualat.chisq.pool, dof) #0.01
# G-Squared (G^2) Statistic
sexualat.g2.pool <- 2*sum(sexualat.obs.pool*log(sexualat.obs.pool/sexualat.exp.pool)); sexualat.g2.pool
1-pchisq(sexualat.g2.pool, dof) #0.01

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Goodness-of-fit (Four-Class Model)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Observed frequencies
sexualat.obs <- sexualat.lca4$predcell$observed
# Expected frequencies:
sexualat.exp <- sexualat.lca4$predcell$expected

# Effective number of response patterns (aggregating expected < 5)
sexualat.obs.pool <- c(sexualat.obs[sexualat.exp >= 5], sum(sexualat.obs[sexualat.exp < 5]))
sexualat.exp.pool <- c(sexualat.exp[sexualat.exp >= 5], sum(sexualat.exp[sexualat.exp < 5]))
# Chi-Squared (X^2) Statistic
sexualat.chisq.pool <- sum((sexualat.obs.pool-sexualat.exp.pool)^2/sexualat.exp.pool); sexualat.chisq.pool
# Pooled degrees of freedom
dof = length(sexualat.obs.pool) - 4*(ncol(sexualat.df)+1)
# G-Squared (G^2) Statistic
sexualat.g2.pool <- 2*sum(sexualat.obs.pool*log(sexualat.obs.pool/sexualat.exp.pool)); sexualat.g2.pool


