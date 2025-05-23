* B. Data analysis

// Primary analyses using multilevel modeling are conducted in HLM software. 
// Below analyses are auxiliary analyses to check the validity of the models, 
// that are unavailable in HLM software. 

// To set the working directory
cd "C:\Users\DeepThought\OneDrive\Yongwook\a1_Dissertation\0_Analysis"
// cd "C:\Users\silbe\OneDrive\Yongwook\a1_Dissertation\0_Analysis"


********************************************************************************
* B.1. Descriptive statistics 
********************************************************************************

// Sample size: 7,230 young adults from 30 countries

use "data.dta", clear
numlabel, add

// Grouping variables 
global y gcid moral mor_rel mor_for polit pol_reg pol_un
global x gce_hr1-gce_hr3 gce_cul1-gce_cul3 gce_sd1-gce_sd3
global control1 female immg edu1-edu5 class1-class3 lr
global control2 mimmgpp mpat gi hdipp 

** B.1.1. Pooled data 

// Individual-level variables (N1 = 7,230)
tabstat $y $control1 , statistics(n mean sd min max) columns(statistics) format(%9.3f) // Unweighted 
tabstat $y $control1 [aw = gwght], statistics(n mean sd min max) columns(statistics) format(%9.3f) // Weighted

// Country-level variables (N2 = 30)
keep cntry $x $control2 
sort cntry 
quietly by cntry: gen dup = cond(_N == 1, 0, _n)
drop if dup > 1

tabstat $x $control2 , statistics(n mean sd min max) columns(statistics) format(%9.3f) 

// Auxiliary t-test for the indices between sample vs. world mean
ttest gi == 61.06 // World mean of KOF GI in 2020 = 61.06
ttest hdipp == 73.2 // World mean of HDI in 2021 = 0.732 
// For both KOF GI and HDI, the sample mean is significantly higher than the world mean.

** B.1.2. By country 

use "data.dta", clear
numlabel, add

tabstat $x $y $control2 , by(cntry) statistics(mean sd) format(%9.3f) // Unweighted 
tabstat $x $y $control2 [aw = gwght], by(cntry) statistics(mean sd) format(%9.3f) // Weighted

// Weight variables in EVS/WVS Joint 2017
// gwght: base weight (multiplied by design, nonresponse, and post-stratification weights and then standardized)
// wght_eq1000: equilibrated weight based on gwght (sum of each country's wght_eq1000 = 1000)
// pwght: population-size weight (wght_eq1000 / 1000 * population total for each country)


********************************************************************************
* B.2. Correlation between variables  
********************************************************************************

// Type of variables
// Binary: gcid female immg (at level 1)
// Ordinal: moral polit edu_corr class_corr lr (at level 1) gce_hr gce_cul gce_sd (at level 2)
// Continuous (including ratio): mimmgpp mpat gi hdipp (at level 2)  

// Note 1: lr can be treated as a continuous variable since it has a large number of levels.
// Note 2: All ordinal variables should be coded according to their order (e.g., edu_corr and class_corr). 
// Note 3: Since some commands (e.g., ktau and tetrachoric) do not support pw or aw options, 
// correlation analyses cannot be weighted. 

** B.2.1. Level-1 variables 

use "data.dta", clear
numlabel, add

// Binary-binary: Tetrachoric correlation coefficient
tetrachoric gcid female immg, pw stats(rho p) 

// Ordinal-binary: Rank-biserial correlation coefficient
// ssc install somersd // To install the add-on package 
foreach b in gcid female immg {
	somersd `b' moral polit edu class lr 
	}

// Ordinal-ordinal: Kendall's rank correlation coefficient tau-b
ktau moral polit edu_corr class_corr lr, pw stats(taub p)  	
	
** B.2.2. Level-2 variables 

// Removing individual level variations 
keep cntry gce_hr gce_cul gce_sd mimmgpp mpat gi hdipp
sort cntry 
quietly by cntry: gen dup = cond(_N == 1, 0, _n)
drop if dup > 1

// Continuous-continuous: Pearson correlation coefficient r
pwcorr mimmgpp mpat gi hdipp, sig

// Ordinal-ordinal and continuous-ordinal: Kendall's tau-b
ktau gce_hr gce_cul gce_sd mimmgpp mpat gi hdipp, pw stats(taub p)


********************************************************************************
* B.3. Intraclass correlation (ICC)
********************************************************************************

use "data.dta", clear
numlabel, add

// ICC over 0.10 indicates significant clustering effects, 
// which should be addressed by appropriate methods such as multilevel modeling. 

** B.3.1. Unweighted analyses 

// GC identity 
melogit gcid || cntry: // ICC analysis using logistic model
estat icc // ICC = 0.084
meprobit gcid || cntry: // ICC analysis using probit model
estat icc // ICC = 0.103

// Note that logistic or probit regression does not return direct estimation of the residual variance at level 1.
// Therefore, it is unable to calculate ICC without additional assumption.
// The above analyses using logistic and probit models are based on the assumption 
// of underlying continuous latent variable that has residual variance following logistic or normal distribution.

// Moral and political GC attitudes
loneway moral cntry // ICC for moral attitudes = 0.226
loneway polit cntry // ICC for political attitudes = 0.125

** B.3.2. Weighted analyses 

// GC identity 
melogit gcid [pw = gwght] || cntry: // ICC analysis using logistic model
estat icc // ICC = 0.085
meprobit gcid [pw = gwght] || cntry: // ICC analysis using probit model
estat icc // ICC = 0.104

// Moral and political GC attitudes
loneway moral cntry [aw = gwght] // ICC for moral attitudes = 0.199
loneway polit cntry [aw = gwght] // ICC for political attitudes = 0.100

// The analyses results indicate that all outcome variables have ICCs large enough to try HLM (i.e., ICC > 0.10)
// Only ICC for identity variable using logistic model is < 0.10, while ICC using probit is > 0.10.

// Weight variables in EVS/WVS Joint 2017
// gwght: base weight (multiplied by design, nonresponse, and post-stratification weights and then standardized)
// wght_eq1000: equilibrated weight based on gwght (sum of each country's wght_eq1000 = 1000)
// pwght: population-size weight (wght_eq1000 / 1000 * population total for each country)


********************************************************************************
* B.4. Multilevel models for predicting GC
********************************************************************************

// Note: Below codes can replicate the analyses presented in my dissertation, 
// but the actual results were produced using HLM software for computational efficiency.

** B.4.1. HGLM to predict GC identity 

use "data_mi.dta", clear
numlabel, add

// Model with level-1 variables only 
mi estimate, cmdok post: meglm gcid i.female i.immg i.edu i.class lr [pw = gwght] || cntry: i.female i.immg i.edu i.class lr, family(bernoulli) link(logit)  

// Full model  
mi estimate, cmdok post: meglm gcid i.female i.immg i.edu i.class lr i.gce_hr i.gce_cul i.gce_sd mimmgpp mpat gi hdipp [pw = gwght] || cntry: i.female i.immg i.edu i.class lr, family(bernoulli) link(logit)  

** B.4.2. HLM to predict moral and political GC attitudes 

use "data_mi.dta", clear
numlabel, add

// Model with level-1 variables only 
foreach var in moral polit {
	mixed `var' i.female i.immg i.edu i.class lr [pw = gwght] || cntry: i.female i.immg i.edu i.class lr, mle cov(un)
	}

// Full model 
foreach var in moral polit {
	mixed `var' i.female i.immg i.edu i.class lr i.gce_hr i.gce_cul i.gce_sd mimmgpp mpat gi hdipp [pw = gwght] || cntry: i.female i.immg i.edu i.class lr, mle cov(un)
	}


********************************************************************************
* B.5. Residual analyses 
********************************************************************************

cd "C:\Users\DeepThought\OneDrive\Yongwook\a1_Dissertation\0_Analysis\0_HLM result\Residual"
// cd "C:\Users\silbe\OneDrive\Yongwook\a1_Dissertation\0_Analysis\0_HLM result\Residual"

// Structure of the level-1 residual files
// L2ID: country code = cntry
// L1RESID: residual value = observed value of y - fitted value  

// Shapiro-Wilk normality test does not work properly, 
// because the outcome variables are not continuous.  

// Therefore, checking QQ plots provides more information about irregular patterns,
// though there are many "gaps" in values and "spikes" of identical values.  

foreach y in gcid moral polit {
	use "`y'_res1.dta", clear
	tab L2ID, missing // Country entry 
	qnorm L1RESID, saving(`y'_all) // QQ plot for the residuals across all countries  

	levelsof L2ID, local(levels) 
	foreach n of local levels { 
		qnorm L1RESID if L2ID == `n', saving(`y'_`n') // QQ plot
		swilk L1RESID if L2ID == `n' // Shapiro-Wilk normality test
		}
	}

// Thailand (764) may show some violation of normality assumption in residual analysis according to Pichler (2011).
// In detail, Pichler noted that Georgia, Germany, Japan, Jordan, Mali as well as Thailand had to be put in the fixed part (instead of being part of random variation at the country level) 
// since their residuals (relatively large aggregate values in the dependent variables) threaten the assumption of normally distributed residuals at the country level.


********************************************************************************
* Appendices
/*******************************************************************************

** Appendix C: Appropriate correlation coefficient according to the variable types

// Correlation between variables can be appropriately examined 
// using following methods according to their types. 
// See Khamis (2008) and Cohen and Grabois's article (2022) at
// https://medium.com/@vatvenger/choosing-the-appropriate-correlation-coefficient-a167a65203ff

// Combinations of variables included in the model 

// * Continuous-continuous
// Pearson correlation coefficient r
// pwcorr var1 var2 var3, sig obs

// * Continuous-ordinal
// Kendall's rank correlation coefficient tau-b
// ktau var1 var2 var3, pw stats(taub p obs) 

// * Ordinal-ordinal 
// Kendall's rank correlation coefficient tau-b (or polychoric correlation coefficient)
// ktau var1 var2 var3, pw stats(taub p obs)  

// * Ordinal-nominal/binary
// Rank-biserial correlation coefficient (special case of Somers' D)
// ssc install somersd // To install the package 
// somersd binary_var ordinal_var1 ordinal_var2 

// * Binary-binary
// Tetrachoric correlation coefficient (or phi coefficient)
// tetrachoric var1 var2 var3, pw stats(rho p obs)  

// Other combinations that are not found in the model 

// * Continuous-nominal/binary
// Point-biserial correlation coefficient
// esize twosample cont_var, by(binary_var) pbcorr

// * Nominal-nominal (if at least one of the variables hae more than two levels)
// Cramer's V (or Goodman and Kruskal's lambda)
// tab var1 var2, V
// return list // To check p-value

** Appendix C: Another syntax to calculate ICC for moral and polit variables  

// Unweighted analyses 
mixed moral || cntry:, mle
estat icc // ICC = 0.195
mixed polit || cntry:, mle
estat icc // ICC = 0.101
// Significant results (i.e., ICC > 0.10)

// Weighted analyses 
mixed moral [pw = gwght] || cntry:, mle
estat icc // ICC = 0.193
mixed polit [pw = gwght] || cntry:, mle
estat icc // ICC = 0.100
// Still significant results (i.e., ICC > 0.10)

*/
