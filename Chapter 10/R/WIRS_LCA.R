# Chapter 10: Models with Categorical Latent Variables
# WIRS: Workplace Industrial Relations Data Example
# Change to appropriate path for file "wirs.dat".

rm(list = ls())

# Install 'poLCA' package if not already installed:
if(!all(c("poLCA") %in% installed.packages())) install.packages(c("poLCA"))
library(poLCA)

# WIRS: Workplace Industrial Relations Data Example (1-factor)
# wirs.file <- "C:/AMSSD/Datasets/Chapter10/wirs.dat"
# The data file path needs to be changed to the file's location on your computer

# Alternatively, choose the file "wirs.dat" directly below:
wirs.file <- file.choose()
wirs.mat <- data.matrix(read.table(wirs.file, skip=0))

colnames(wirs.mat) <- c("Item1", "Item2", "Item3", "Item4", "Item5", "Item6")

# Turn the 0,1 coding to 1,2(,...) coding and
# transforming as data.frame, as required by the poLCA package.
wirs.df <- as.data.frame(wirs.mat + 1)

# Latent Class Analysis (LCA) models
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
form <- cbind(Item1,Item2,Item3,Item4,Item5,Item6) ~ 1

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

# A convenient function for two-way margins for LCA models
twowayMargins <- function(df,mod,digits = 2){
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

threewayMargins <- function(df,mod,digits = 2){
  pair <- combn(length(df),3)
  coup <- expand.grid(1:2,1:2)
  out <- NULL
  tmp.form <- v1 ~ v2
  for(i in 1:ncol(pair)){
    tmp1 <- c(with(df, table(get(names(df)[pair[1,i]]),
                             get(names(df)[pair[2,i]]),
                             get(names(df)[pair[3,i]]))))
    tmp.form <- update(tmp.form, substitute(v1 ~ v2, env = list(v1 = as.name(names(df)[pair[1,i]]),
                                                                v2 = as.name(names(df)[pair[2,i]]))))

    var_name <- names(df)[pair[3,i]]
    condition_list <- setNames(list(1), var_name)
    tmp2a <- poLCA::poLCA.table(formula = tmp.form, lc = mod,
                                condition = condition_list)
    condition_list <- setNames(list(2), var_name)
    tmp2b <- poLCA::poLCA.table(formula = tmp.form, lc = mod,
                                condition = condition_list)
    tmp2 <- c(c(tmp2a),c(tmp2b))
    nam1 <- rownames(tmp2a); nam1 <- gsub(" 1"," 0",nam1); nam1 <- gsub(" 2"," 1",nam1)
    nam2 <- colnames(tmp2a); nam2 <- gsub(" 1"," 0",nam2); nam2 <- gsub(" 2"," 1",nam2)
    nam3 <- paste(var_name, c(0,1))
    tmp <- data.frame(rep(nam1[coup[,1]],2), rep(nam2[coup[,2]],2), rep(nam3,each = 4),tmp1,tmp2,
                      round(tmp1-tmp2,digits),
                      round(((tmp1-tmp2)^2)/tmp2,digits))
    out <- rbind(out,tmp)
  }
  names(out) <- c("Item 1", "Item 2", "Item 3","Obs", "Exp", "O-E","(O-E)^2/E")
  return(out)
}


# Two-Class LCA Model (WIRS Data)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
wirs.lca2 <- poLCA::poLCA(form,wirs.df,nclass=2,na.rm=F,nrep=10)
LCA.probs(wirs.lca2,SE = F)
wirs.lca2$Chisq
wirs.lca2$Gsq
wirs.twoway <- twowayMargins(wirs.df,wirs.lca2)
wirs.twoway[wirs.twoway$`(O-E)^2/E` > 3,]
wirs.threeway <- threewayMargins(wirs.df,wirs.lca2)
wirs.threeway[wirs.threeway$`(O-E)^2/E` > 3,]

# Three-Class LCA Model (WIRS Data)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
wirs.lca3 <- poLCA::poLCA(form,wirs.df,nclass=3,na.rm=F,nrep=10)
wirs.lca3$Chisq
wirs.lca3$Gsq

# Code below reproduces parts of:
# Table 10.14: Estimated conditional probabilities and prior probabilities for the
# three-class model, WIRS Data.
LCA.probs(wirs.lca3,SE = F)
