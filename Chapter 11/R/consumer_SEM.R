# Chapter 11: Confirmatory Factor Analysis and Structural Equation Models
# Green Consumer Data Example

rm(list = ls())

# Install 'lavaan' package if not already installed:
if(!all(c("lavaan", "semPlot") %in% installed.packages())) install.packages(c("lavaan", "semPlot"))
library(lavaan)
library(semPlot)

# Green Consumer Data Example
# consumer.file.3c <- "C:/AMSSD/Datasets/Chapter11/consumer_3cat330.dta"
# The data file path needs to be changed to the file's location on your computer

# Alternatively, choose the file "environ.dat" directly below:
consumer.file.3c <- file.choose()
consumer.data.3c = haven::read_dta(consumer.file.3c)

#perform CFA analysis
consumer.model.cfa <- '
  # latent variables
    ENK1 =~  knen1 + knen2 + knen3 + knen4 + knen5 
    ENK2 =~  knen6 + knen7
    GPUR =~ purc1 + purc2 + purc3 +purc4
    GCON =~ cons1 + cons2 + cons3 + cons4 
    
'
# CFA with standardized LVs

consumer.cfa1 <- lavaan::cfa(consumer.model.cfa, 
                             data = consumer.data.3c,
                             estimator="WLSMV",
                             ordered = T,
)                        

summary(consumer.cfa1, fit.measures = T, estimates = F)
mi <- lavaan::modindices(consumer.cfa1)
mi[mi$mi > 10 & mi$op == "=~",]

# SEM with the 3cat data
consumer.raw.model.sem <- '

  # Measurement Model
    ENKN1 =~ knen1 + knen2 + knen3 + knen4 + knen5 
    ENKN2 =~ knen6 + knen7
    GPUR =~ purc1 + purc2 + purc3 + purc4 
    GCON =~ cons1 + cons2 + cons3 + cons4

  # Structural Regressions
    GPUR ~ ENKN1 + ENKN2+ GCON
    GCON ~ ENKN1 + ENKN2

 '

consumer.raw.sem <- lavaan::sem(consumer.raw.model.sem, 
                                data = consumer.data.3c,
                                estimator="WLSMV",
                                ordered = T,
                                parameterization = "delta")
summary(consumer.raw.sem,standardized=TRUE)
summary(consumer.raw.sem, fit.measures = T, estimates = F)

# Code below reproduces parts of:
# Figure 11.8: Path diagram, structural equation model, standardised solution, green consumer data.
semPlot::semPaths(consumer.raw.sem, what = "std", title = FALSE, layout = "tree")
