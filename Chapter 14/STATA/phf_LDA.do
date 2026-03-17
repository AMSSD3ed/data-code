* Chapter 14: Longitudinal Data Analysis
* Physical Health Functioning Example (Whitehall II study, 2005)
* Change to appropriate path for file "phf.dta".

cd "C:\AMSSD\Datasets\Chapter14"

use phf, clear

*Centre age at 50 (close to mean at first occasion)
gen age50 = age-50

* Code below reproduces parts of:
* Table 14.4: Linear random intercept and random slope growth models for physical health functioning
* Table 14.4 (a): Random intercept model
* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

* Random intercepts model with centred age
mixed phf age50 || id:

* Intra-individual correlation implied by model 
estat wcorr

* Store model results (for use in LR test later)
estimates store linri

* Compare with single-level model
mixed phf age50
estimates store linsl
lrtest linri linsl

* Code below reproduces parts of:
* Table 14.4: Linear random intercept and random slope growth models for physical health functioning
* Table 14.4 (b): Random slope model
* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

mixed phf age50 || id: age50, cov(unstruc)

*Store model results (for use in LR test)
estimates store linrs

*LR test comparing random slopes and random intercept
lrtest linrs linri

* Intra-individual correlations implied by model for selected individuals
* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

* Code below reproduces:
* Table 14.5: Estimates of the within-person correlations implied by the random slope model
* of Table 14.4 for two individuals observed at four different ages (t_ij),
* Physical Health Functioning example

*Individual 12 observed at young ages (42 to 51.2)
estat wcorr, at(id = 12) list
*Individual 1466 observed at older ages (61 to 76.1)
estat wcorr, at(id = 1466) list


* Non-linear latent growth curve model:
* Random slope model with quadratic in age
* Analysis reported in the text (no tables/figures)
* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

* Create age-squared
gen age50_2 = age50*age50

* Fit quadratic model (with fixed coefficient on age-squared)
mixed phf age50 age50_2 || id: age50, cov(unstruc)
estimates store quad1 

* Compare with linear Random Slope model
lrtest quad1 linrs

* Fit full quadratic model (with random coeff on age-squared)
mixed phf age50 age50_2 || id: age50 age50_2, cov(unstruc)

* Compared with previous model
estimates store quad2 
lrtest quad2 quad1

