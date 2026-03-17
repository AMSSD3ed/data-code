* Chapter 11: Confirmatory Factor Analysis and Structural Equation Models
* Subject Marks Data Example

* USING THE COMMAND SEM AVAILABLE IN STATA13
* Reading the correlation matrix using the SSD command

clear all
ssd init gaelic english history arithm algebra geometry
ssd set obs 220
ssd set cor  1.00 \  0.44 1.00 \ 0.41 0.35 1.00\ 0.29 0.35 0.16 1.00\ 0.33 0.32 0.19 0.59 1.00\ 0.25 0.33 0.18 0.47 0.46 1.00
ssd status
* save subjects /// In case of saving correlation covariance matrix

/*Fitting a confirmatory factor analysis model*/
sem (Human-> gaelic english history) ///
	(Maths-> arithm algebra geometry), method(ml) 

/*Fitting a confirmatory factor analysis model by scaling the latent variables to have variances 1 instead*/
* Code below reproduces:
* Table 11.1: Factor Loadings with SEs for the two-factor CFA model, Subject marks data
sem (Human-> gaelic english history) ///
	(Maths-> arithm algebra geometry), var(Human@1) var(Maths@1)

/*Residuals*/
* Code below reproduces parts of:
* Table 11.2: Reproduced correlations and raw residuals observed for two-factor CFA,
estat framework, fitted compact
estat residuals

/*standardized residuals follow a normal distribution */
* Code below reproduces:
* Table 11.3: Standardized residuals for two-factor CFA,
estat residuals, standardized

* Code below reproduces:
* Table 11.4: Fit indices for the two-factor CFA,
estat gof, stats(all)