# Chapter 11: Confirmatory Factor Analysis and Structural Equation Models
# Test Anxiety Inventory Example

rm(list = ls())

# Install 'lavaan' package if not already installed:
if(!all(c("lavaan") %in% installed.packages())) install.packages(c("lavaan"))
library(lavaan)

#Reading the covariance matrix
lower <- '
1.0000,
0.5101, 1.0000,
0.2399, 0.2961, 1.0000,
0.4226, 0.4068, 0.3685, 1.0000,
0.3072, 0.2321, 0.4038, 0.3468, 1.0000,
0.2857, 0.3357, 0.2713, 0.3415, 0.3384, 1.0000,
0.3944, 0.3925, 0.5351, 0.4253, 0.4889, 0.3916, 1.0000,
0.4290, 0.4831, 0.3781, 0.4141, 0.3030, 0.2567, 0.4512, 1.0000,
0.4332, 0.4406, 0.3398, 0.4232, 0.2221, 0.2735, 0.4099, 0.5357, 1.0000,
0.2586, 0.2838, 0.2578, 0.2959, 0.1902, 0.2456, 0.3240, 0.3263, 0.3568, 1.0000,
0.5026, 0.4720, 0.3289, 0.3941, 0.3389, 0.3468, 0.4483, 0.5869, 0.5288, 0.3900, 1.0000,
0.3939, 0.4094, 0.2785, 0.4233, 0.2571, 0.3277, 0.4287, 0.4763, 0.4246, 0.3326, 0.5295, 1.0000,
0.3964, 0.4320, 0.2931, 0.3513, 0.3144, 0.2769, 0.3789, 0.4330, 0.3711, 0.2884, 0.4714, 0.3181, 1.0000,
0.4082, 0.3668, 0.3458, 0.4859, 0.4268, 0.4180, 0.4776, 0.3433, 0.3573, 0.2675, 0.4473, 0.4130, 0.4103, 1.0000,
0.5159, 0.4884, 0.3984, 0.6024, 0.3366, 0.3618, 0.4855, 0.5654, 0.5191, 0.3907, 0.5491, 0.5204, 0.4421, 0.5180, 1.0000,
0.4688, 0.4827, 0.3818, 0.4358, 0.2724, 0.3096, 0.4122, 0.5343, 0.5487, 0.4276, 0.5980, 0.5342, 0.4014, 0.4358, 0.6510, 1.0000,
0.3978, 0.3051, 0.4320, 0.4598, 0.5458, 0.3681, 0.5083, 0.3571, 0.3531, 0.3152, 0.3826, 0.4116, 0.3069, 0.4052, 0.5232, 0.4542, 1.0000,
0.3560, 0.4606, 0.3516, 0.4078, 0.3051, 0.2700, 0.4141, 0.4745, 0.4788, 0.3513, 0.5415, 0.4279, 0.5024, 0.3908, 0.5202, 0.5087, 0.3483, 1.0000,
0.3879, 0.3397, 0.3791, 0.3637, 0.2610, 0.2279, 0.4425, 0.4513, 0.4287, 0.4173, 0.4511, 0.3790, 0.4566, 0.3747, 0.5470, 0.5410, 0.4615, 0.4835, 1.0000,
0.3803, 0.3256, 0.2935, 0.5295, 0.3005, 0.4050, 0.4480, 0.4367, 0.4118, 0.2963, 0.4519, 0.5119, 0.3527, 0.5034, 0.5599, 0.4908, 0.3964, 0.4256, 0.4604, 1.0000'

#Assigning names to the variables
anxiety.cov <- 
  lavaan::getCov(lower, names = paste0("x",1:20))

# CFA for items measured at the 7th grade
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
anxiety.model.cfa <- '
  # latent variables
    emoti =~ x1 + x2 + x8 + x9 + x10 + x11 + x12 + x13 + x15 + x16 + x18 + x19
    worry =~ x3 + x4 + x5 + x6 + x7 + x14 + x17 + x20
'
anxiety.cfa1 <- lavaan::sem(anxiety.model.cfa, 
                            sample.cov = anxiety.cov, 
                            sample.cov.rescale = FALSE,
                            sample.nobs = 335,
                            std.lv = T,
                            estimator="ML")

# Code below reproduces parts of:
# Table 11.6: Parameter estimates and SEs for the two-factor CFA model,
# Test anxiety inventory items data
lavaan::parameterEstimates(anxiety.cfa1, zstat = F, pvalue = F, ci = F, rsquare = T)

# Code below reproduces parts of:
# Table 11.7: Goodness-of-fit statistics and fit indices for the two-factor CFA model,
# Test anxiety inventory items data
summary(anxiety.cfa1, fit.measures = T, estimates = F)

# Code below reproduces parts of:
# Table 11.8: Pair of variables with largest standardized residuals,
# Test anxiety inventory items data
resCov <- residuals(anxiety.cfa1, type = "standardized")$cov
candidates <- resCov[lower.tri(resCov)][abs(resCov[lower.tri(resCov)]) > 2.5]
entries <- which(Reduce("|", lapply(candidates, function(x) resCov == x)), arr.ind = T)
entries <- as.data.frame(entries); names(entries) <- c("V1","V2")
entries <- dplyr::semi_join(as.data.frame(t(combn(nrow(anxiety.cov),2))),as.data.frame(entries))
data.frame("V1" = rownames(resCov)[entries[,1]],
           "V2" = rownames(resCov)[entries[,2]],
           "Std.Res" = round(resCov[as.matrix(entries)],2))

# Code below reproduces parts of:
# Table 11.9: Modification Indices,
# Test anxiety inventory items data
mi <- lavaan::modindices(anxiety.cfa1)
mi[mi$mi > 10 & mi$op == "=~",]
