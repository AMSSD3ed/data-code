# Chapter 13: Multilevel Models
# Hedonism in different countries Data Example
# Change to appropriate path for file "hedonism.txt".

rm(list = ls())

# Install 'lme4' package if not already installed:
if(!all(c("lme4") %in% installed.packages())) install.packages(c("lme4"))
library(lme4)

# Hedonism dataset
# hedonism.file <- "C:/AMSSD/Datasets/Chapter13/hedonism.txt"
# The data file path needs to be changed to the file's location on your computer

# Alternatively, choose the file "hedonism.txt" directly below:
hedonism.file <- file.choose()
hedonism.mat <- read.table(hedonism.file, header = T); rm(hedonism.file)

# Replace '999' as missing values 'NA'
hedonism.mat$INCOME[hedonism.mat$INCOME == 99] <- NA
hedonism.mat$FEMALE[hedonism.mat$FEMALE == 9] <- NA
hedonism.mat$AGE[hedonism.mat$AGE == 999] <- NA
hedonism.mat$EDUYRS[hedonism.mat$EDUYRS == 99] <- NA

summary(hedonism.mat)

# Single-level model for the mean (null model)
# Hedonism Example
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
hedonism.mod0 <- lm(HEDSCORE ~ 1, data = hedonism.mat)
summary(hedonism.mod0)

# MLM for group means - country effect (Eq. 13.2),
# Hedonism Dataset Example
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
hedonism.mod1 <- lme4::lmer(HEDSCORE ~ (1 | country),
                            data = hedonism.mat, REML = FALSE)

# Code below reproduces parts of:
# Table 13.1: MLM with country effects fitted to hedonism data
summary(hedonism.mod1)

# Reject H0 (i.e., no country differences) in favour of MLM model.
anova(hedonism.mod1,hedonism.mod0)

# Code below reproduces parts of:
# Figure 13.3: Caterpillar plot showing country residuals with 95% CIs,
# Hedonism Dataset Example
library(ggplot2)
dd <- as.data.frame(lme4::ranef(hedonism.mod1))
ggplot(dd, aes(x=grp,y=condval)) +
  geom_point() +
  geom_pointrange(aes(ymin=condval -2*condsd,
                      ymax=condval +2*condsd)) +
  labs(title = "Predicted Country Random Effects",
       x = "country (ranked)",
       y = "Random Effect") + geom_hline(yintercept = 0, linetype = 2, color = "red") +
  theme_minimal()
# Alternatively:
lattice::dotplot(lme4::ranef(hedonism.mod1))

# MLM - Random Intercept Model (Eq. 13.6),
# Hedonism Dataset Example
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
hedonism.mod2 <- lme4::lmer(HEDSCORE ~ AGE + (1 | country),
                            data = hedonism.mat, REML = FALSE)

# Code below reproduces parts of:
# Table 12.2: Random Intercept Model with country and age
summary(hedonism.mod2)

# MLM - Random Intercept and Slope Model (Eq. 13.7),
# Hedonism Dataset Example
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
hedonism.mod3 <- lme4::lmer(HEDSCORE ~ AGE + (AGE| country),
                            data = hedonism.mat, REML = T)
# Code below reproduces parts of:
# Table 13.3: Random Intercept and Slope Model with country and age effects
# fitted to Hedonism data
summary(hedonism.mod3)

# The output above includes the correlation between random effects, but
# the parameter reported in Table 13.3 is the covariance, which can be obtained with:
print(VarCorr(hedonism.mod3), comp = "Variance")

# Reject H0 in favour of Random slope model.
anova(hedonism.mod3,hedonism.mod2)

# Code below reproduces parts of:
# Figure 13.4: Predicted country lines from a random slope model
# Hedonism and Age.
bhat <- coef(hedonism.mod3)$country
plot(1,type="n",
     xlim = range(hedonism.mat$AGE, na.rm = T),
     ylim = c(-2,1),
     xlab = "Age (years) centred at 46",
     ylab = "Predicted hedonism score")
for(i in seq_len(nrow(bhat))){
  abline(a = bhat$`(Intercept)`[i], b = bhat$AGE[i])
}

# Code below reproduces parts of:
# Figure 13.5: Estimated intercept and slope residuals for the relationship between
# Hedonism and Age.
plot(x = ranef(hedonism.mod3)$country$`(Intercept)`, y = ranef(hedonism.mod3)$country$AGE,
     xlim = c(-1,1), ylim = c(-0.01,0.01),
     xlab = expression('u'["0j"]), ylab = expression('u'["1j"]), pch = 17, cex = 1.5)

# Contextual Effects Model (Eq. 13.10 & 13.11),
# Hedonism Dataset Example
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
hedonism.mod4a <- lme4::lmer(HEDSCORE ~ INCOME + (1| country),
                             data = hedonism.mat, REML = T)
# Code below reproduces parts of:
# Table 13.4a: Random Intercept models with different specifications of the relationship
# between hedonism and income
summary(hedonism.mod4a)

# Create income band by country
hedonism.mat$INbyCOU <- with(hedonism.mat, ave(INCOME, country, FUN = function(x) mean(x,na.rm = T)))

hedonism.mod4b <- lme4::lmer(HEDSCORE ~ INCOME + INbyCOU + (1| country),
                             data = hedonism.mat, REML = T)
# Code below reproduces parts of:
# Table 13.4b: Random Intercept models with different specifications of the relationship b
# between hedonism and income
summary(hedonism.mod4b)

hedonism.mod4c <- lme4::lmer(HEDSCORE ~ I(INCOME - INbyCOU) + INbyCOU + (1| country),
                             data = hedonism.mat, REML = T)
# Code below reproduces parts of:
# Table 13.4c: Random Intercept models with different specifications of the relationship b
# between hedonism and income
summary(hedonism.mod4c)
