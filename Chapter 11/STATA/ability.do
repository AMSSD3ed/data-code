* Chapter 11: Confirmatory Factor Analysis and Structural Equation Models
* Ability Example (Scholastic Aptitude Test Data)

* USING THE COMMAND SEM AVAILABLE IN STATA13
* Reading the covariance matrix using the SSD command*

clear all
ssd init math9 sci9 read9 scatv9 scatq9 math7 sci7 read7 scatv7 scatq7
ssd set obs 383
ssd set cov  135.97 \ 89.21 130.94\ 94.15 96.42 147.69\94.36 98.54 112.11 137.10\127.47 96.03 104.27 100.72 211.26\97.90 83.74 88.67 91.27 107.89 129.98 ///
             \65.43 77.44 71.12 77.25 69.21 68.31 85.03\112.23 113.82 137.43 135.58 115.87 116.16 94.84 214.35 \88.54 93.03 106.11 120.17 94.96 91.01 76.31 141.02 135.09 ///
			 \100.70 79.67 85.93 87.23 128.99 99.87 68.39 110.71 88.00 154.27

ssd status
* save ability /// In case of saving correlation covariance matrix

/*Fitting the SEM model (Figure 11.6)*/
sem (Quant7->scatq7 math7 sci7) (Verbal7->sci7 read7 scatv7@1) ///
    (Quant9->scatq9 math9 sci9) (Verbal9->sci9 read9 scatv9@1) ///
    (Quant7->Quant9) (Verbal7->Verbal9)

/*The command estat ic reports all available goodness of fit test statistics and fit indices*/
estat gof, stats(all)

/*Residuals*/
estat residuals

/*standardized residuals follow a normal distribution */
* Code below reproduces parts of:
* Table 11.5: Standardized Residuals for the SEM of Figure 11.6, Ability Data
estat residuals, standardized

/*Print modification indices for all parameters*/
estat mindices

/*Fitting the SEM model with correlated error terms (Figure 11.7)*/
sem (Quant7->scatq7 math7 sci7) (Verbal7->sci7 read7 scatv7@1) ///
    (Quant9->scatq9 math9 sci9) (Verbal9->sci9 read9 scatv9@1) ///
    (Quant7->Quant9) (Verbal7->Verbal9) (Quant9->Verbal9), ///
	cov(e.math9*e.math7) cov(e.sci9*e.sci7) cov(e.read9*e.read7) ///
	cov(e.scatv9*e.scatv7) cov(e.scatq9*e.scatq7)

/*The command estat ic reports all available goodness of fit test statistics and fit indices*/
estat gof, stats(all)

/*standardized residuals follow a normal distribution */
estat residuals, standardized
