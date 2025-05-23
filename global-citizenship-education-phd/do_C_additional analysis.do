* C. Additional data analysis after defense

// Primary analyses using multilevel modeling are conducted in HLM software. 
// Below analyses are auxiliary analyses to check the validity of the models, 
// that are unavailable in HLM software. 

// To set the working directory
cd "C:\Users\DeepThought\OneDrive\Yongwook\a1_Dissertation\0_Analysis"
// cd "C:\Users\silbe\OneDrive\Yongwook\a1_Dissertation\0_Analysis"


********************************************************************************
* C.1. Additional variables suggested in the defense
********************************************************************************

** C.1.1. Identification of additional variables  

use "data.dta", clear
numlabel, add

// g063 // Original variable of GC identity 
// x003 // Age 
// size_5c // Urbanicity 

codebook g063 x003 size_5c 

** C.1.2. Recoding variables

// GC identity as originally coded 
tab g063
recode g063 (1 = 3) (2 = 2) (3 = 1) (4 = 0) (-5/-1 = .), gen(gcid_4)
tab gcid_4, missing // Ordinal 
// 0 = not close at all (ref.)
// 1 = not very close
// 2 = close
// 3 = very close

// Age
tab x003
gen age = x003
tab age, missing // Continuous 

// Urbanicity
tab size_5c
recode size_5c (1 = 4) (2 = 3) (3 = 2) (4 = 1) (5 = 0) (-5/-1 = .), gen(urban)
tab urban, missing // Categorical
quietly: tab urban, gen(urban)
// urban1 = under 5000
// urban2 = under 20000
// urban3 = under 100000
// urban4 = under 500000
// urban5 = above 500000

keep cntry gwght gcid gcid_4 moral polit ///  
	female immg edu* class lr gce_hr* gce_cul* gce_sd* mimmgpp mpat gi hdipp age urban*

save "data_defense.dta", replace 

** C.1.3. Descriptive statistics (N1 = 7,230)

use "data_defense.dta", clear
numlabel, add

summarize gcid_4 age urban1-urban5
tabstat gcid_4 age urban1-urban5, statistics(n mean sd min max) columns(statistics) format(%9.3f) // Pooled data
tabstat gcid_4 age urban1-urban5, by(cntry) statistics(mean sd) format(%9.3f) // By country


********************************************************************************
* C.2. Correlation between new variables and outcome variables 
********************************************************************************

use "data_defense.dta", clear
numlabel, add

// Type of variables
// Binary: gcid 
// Ordinal: moral polit 
// Nominal: urban
// Continuous: age  

** C.2.1. Age and global citizenship outcomes 

// gcid:     
// Continuous-nominal/binary
// Point-biserial correlation coefficient
esize twosample age, by(gcid) pbcorr

// moral and polit: 
// Continuous-ordinal
// Kendall's rank correlation coefficient tau-b
ktau age moral polit, pw stats(taub p obs) 

** C.2.2. Urbanicity and global citizenship outcomes 

// gcid:
// Nominal-nominal (if at least one of the variables hae more than two levels)
// Cramer's V (or Goodman and Kruskal's lambda)
tab urban gcid, V
return list // To check p-value

// moral and polit:
// Ordinal-nominal/binary
// Rank-biserial correlation coefficient (special case of Somers' D)
// ssc install somersd // To install the add-on package 
somersd urban moral polit 


********************************************************************************
* C.3. Preliminary OLS to examine the impact of new variables
********************************************************************************

use "data_defense.dta", clear
numlabel, add

** C.3.1. GC identity 

logit gcid i.female i.immg i.edu i.class lr i.gce_hr i.gce_cul i.gce_sd mimmgpp mpat gi hdipp [pw = gwght], vce(cluster cntry)
logit gcid i.female i.immg i.edu i.class lr i.gce_hr i.gce_cul i.gce_sd mimmgpp mpat gi hdipp age i.urban [pw = gwght], vce(cluster cntry)


** C.3.2. Moral and political GC attitudes

reg moral i.female i.immg i.edu i.class lr i.gce_hr i.gce_cul i.gce_sd mimmgpp mpat gi hdipp [pw = gwght], vce(cluster cntry)
reg moral i.female i.immg i.edu i.class lr i.gce_hr i.gce_cul i.gce_sd mimmgpp mpat gi hdipp age i.urban [pw = gwght], vce(cluster cntry)

reg polit i.female i.immg i.edu i.class lr i.gce_hr i.gce_cul i.gce_sd mimmgpp mpat gi hdipp [pw = gwght], vce(cluster cntry)
reg polit i.female i.immg i.edu i.class lr i.gce_hr i.gce_cul i.gce_sd mimmgpp mpat gi hdipp age i.urban [pw = gwght], vce(cluster cntry)


********************************************************************************
* C.4. Ordinal logistic regression for four-point GC identity
********************************************************************************

// The follwoing analyses are based on complete cases, employing casewise deletion.
// N = 4,945 (out of 7,230)

use "data_defense.dta", clear
numlabel, add

** C.4.1. Preliminary analysis

// Descriptive statistics for gcid_4 
summarize ibn.gcid_4 // Pooled data
sort cntry
by cntry: summarize ibn.gcid_4 // By country

// Reference model: logit model to predict GC identity coded in binary
logit gcid i.female i.immg i.edu i.class lr i.gce_hr i.gce_cul i.gce_sd mimmgpp mpat gi hdipp [pw = gwght], vce(cluster cntry)

** C.4.1. Proportional odds model (POM)

// Using ologit built-in command in Stata
ologit gcid_4 i.female i.immg i.edu i.class lr i.gce_hr i.gce_cul i.gce_sd mimmgpp mpat gi hdipp [pw = gwght], vce(cluster cntry)
// However, brant test (for testing the parallel lines assumption) is limited when weights are applied. 
// gologit2 from the add-on package can be an alternative. 

// ssc install gologit2 // To install the add-on package

// Using gologit2
gologit2 gcid_4 i.female i.immg i.edu i.class lr i.gce_hr i.gce_cul i.gce_sd mimmgpp mpat gi hdipp [pw = gwght], vce(cluster cntry) pl lrforce 
// Same results as the above one using ologit
margins gce_hr gce_cul gce_sd, atmeans // Predicted probability by provision of GCE topics 
margins, dydx(gce_hr gce_cul gce_sd) atmeans // Marginal change in predicted probability 

gologit2 gcid_4 i.female i.immg i.edu i.class lr i.gce_hr i.gce_cul i.gce_sd mimmgpp mpat gi hdipp [pw = gwght], vce(cluster cntry) autofit lrf 
// autofit option provides a likelihood ratio test for the parallel lines assumption. 
// i.e., alternative to brant 
// Predictors that do not meet the parallel lines assumption
// gce_cul (p = 0.00000)
// mpat (p = 0.03273)
// gi (p = 0.00784)
// hdipp (p = 0.01432)

** C.4.2. Partial proportional odds model (PPOM)

gologit2 gcid_4 i.female i.immg i.edu i.class lr i.gce_hr i.gce_cul i.gce_sd mimmgpp mpat gi hdipp [pw = gwght], vce(cluster cntry) npl lrforce 
margins gce_hr gce_cul gce_sd, atmeans // Predicted probability by provision of GCE topics 
margins, dydx(gce_hr gce_cul gce_sd) atmeans // Marginal change in predicted probability 

** C.4.3. Predicted probability plot

// Results from POM
foreach topic in hr cul sd {
	quietly ologit gcid_4 i.female i.immg i.edu i.class lr i.gce_hr i.gce_cul i.gce_sd mimmgpp mpat gi hdipp [pw = gwght], vce(cluster cntry) 
	quietly margins gce_`topic', atmeans 
	marginsplot
	graph export "POM_`topic'.png", replace
	}

// Results from PPOM
foreach topic in hr cul sd {
	quietly gologit2 gcid_4 i.female i.immg i.edu i.class lr i.gce_hr i.gce_cul i.gce_sd mimmgpp mpat gi hdipp [pw = gwght], vce(cluster cntry) npl lrforce  
	quietly margins gce_`topic', atmeans 
	marginsplot
	graph export "PPOM_`topic'.png", replace
	}	

// Lines for outcome 0 and 1 (i.e., negative responses) show similar patterns, 
// as do the lines for outcome 2 and 3 (i.e., positive responses).
// However, the movement between these two groups significantly differs.
// This suggests that the outcome variable does not maintain equal distances across all levels 
// but rather clusters with levels 0-1 and 2-3 being closer together.


********************************************************************************
* Appendices
/*******************************************************************************

** Appendix A: Brant test  

// Brant test is the most common method for testing the parallel lines assumption
// in ordinal logistic regression regression. 
// However, it does not work with weights or svyset.
// gologit2 can be an alternative in such occasions.

help spost13 // To install the add-on package
brant, detail
// p < significance level (i.e., 0.05) indicates that the variable does not meet the assumption.

** Appendix B: gologit2 syntax summary

// POM 
gologit2 y x1 x2 [pw = weight], vce(if necessary) pl lrforce 
// Equivalent syntax using ologit:
ologit y x1 x2 [pw = weight], vce(if necessary)

// PPOM
gologit2 y x1 x2 [pw = weight], vce(if necessary) npl lrforce 

// Likelihood ratio test for the parallel lines assumption
gologit2 y x1 x2 [pw = weight], vce(if necessary) autofit lrf 

*/
