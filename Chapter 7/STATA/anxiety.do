* Chapter 7: Factor Analysis
* Test Anxiety Inventory Example

/* Read in the correlation matrix. 
To read the correlation matrix from a file please consult the examples in STATA from Chapter 5.*/
* Code below reproduces the following output:
* Table 7.9: Pairwise correlation between test anxiety items.
matrix A=(1.0000, 0.5101, 1.0000, 0.2399, 0.2961, 1.0000, 0.4226, 0.4068, 0.3685, 1.0000, 0.3072, 0.2321, 0.4038, 0.3468, 1.0000, 0.2857, 0.3357, 0.2713, 0.3415,0.3384, 1.0000, 0.3944,0.3925,0.5351,0.4253,  0.4889,0.3916, 1.0000, 0.4290, 0.4831,       0.3781,0.4141,       0.3030, 0.2567,  0.4512, 1.0000, 0.4332, 0.4406,  0.3398, 0.4232, 0.2221, 0.2735, 0.4099,       0.5357,       1.0000, 0.2586,   0.2838,  0.2578,  0.2959, 0.1902,0.2456, 0.3240,  0.3263, 0.3568, 1.0000, 0.5026,0.4720,       0.3289,       0.3941,       0.3389,  0.3468,  0.4483, 0.5869, 0.5288, 0.3900,  1.0000, 0.3939, 0.4094, 0.2785,0.4233,       0.2571,      0.3277,       0.4287,  0.4763,  0.4246, 0.3326, 0.5295,  1.0000,    0.3964, 0.4320, 0.2931, 0.3513,0.3144,      0.2769,       0.3789,       0.4330,  0.3711,  0.2884, 0.4714, 0.3181, 1.0000, 0.4082, 0.3668,  0.3458, 0.4859,0.4268,      0.4180,       0.4776,  0.3433,  0.3573, 0.2675, 0.4473,  0.4130, 0.4103, 1.0000,   0.5159, 0.4884,0.3984,       0.6024,       0.3366,      0.3618,  0.4855,  0.5654, 0.5191, 0.3907, 0.5491, 0.5204,  0.4421,  0.5180,1.0000, 0.4688,   0.4827,       0.3818,       0.4358,  0.2724, 0.3096, 0.4122, 0.5343, 0.5487, 0.4276,  0.5980,  0.5342, 0.4014,0.4358,       0.6510,      1.0000,  0.3978,   0.3051,0.4320, 0.4598, 0.5458, 0.3681,  0.5083,  0.3571, 0.3531, 0.3152,       0.3826,       0.4116,       0.3069,  0.4052, 0.5232, 0.4542, 1.0000, 0.3560,   0.4606, 0.3516,  0.4078, 0.3051,0.2700,       0.4141,       0.4745,  0.4788, 0.3513, 0.5415,  0.4279, 0.5024, 0.3908,  0.5202,  0.5087, 0.3483,1.0000, 0.3879,0.3397,0.3791,0.3637, 0.2610,0.2279,0.4425, 0.4513,0.4287,0.4173,0.4511, 0.3790, 0.4566,0.3747,0.5470,0.5410,0.4615, 0.4835, 1.0000,0.3803,0.3256,0.2935,0.5295,0.3005,0.4050,0.4480,0.4367,0.4118,0.2963,0.4519,0.5119,0.3527,0.5034,0.5599,0.4908,0.3964,0.4256,0.4604,1.0000)

/*Perform first a PCA on the correlation matrix.*/
* Code below reproduces the following output:
* Table 7.10: Variance explained by each principal component, test anxiety items.
factormat A, n(335) shape(lower) pcf names(x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20) 

/*Perform Factor Analysis with Maximum Likelihood estimation. A two-factor model is fitted.*/ 
* Code below reproduces the following output:
* Table 7.11a: Factor loadings for the unrotated solution
factormat A, n(335) shape(lower) factors(2) ml names(x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20)

/*Scatterplot of factor loadings*/
* Code below reproduces the following output:
* Figure 7.3: Plot of unrotated factor loadings.
loadingplot

/*Perform oblique rotation and in particular the Oblimin*/
* Code below reproduces the following output:
* Table 7.11b: Factor loadings for the OBLIMIN rotated solution (Pattern Matrix).
rotate, oblimin oblique

/* Correlation among factors (under oblique rotation the factors are allowed to be correlated)*/
estat common

/* Correlations between variables and common factors*/
* Code below reproduces the following output:
* Table 7.12: Structure matrix giving correlations between test anxiety items and the two ROTATED factors (via OBLIMIN). 
estat structure
