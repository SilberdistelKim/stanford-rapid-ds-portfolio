* A. Data preparation (v3.1)

// To set the working directory
cd "C:\Users\DeepThought\OneDrive\Yongwook\a1_Dissertation\0_Analysis"
// cd "C:\Users\silbe\OneDrive\Yongwook\a1_Dissertation\0_Analysis"


********************************************************************************
* A.1. Sample 
********************************************************************************

** A.1.1. EVS/WVS Joint 2017

use "0_Original dataset\EVS_WVS_Joint_Stata_v4_0.dta", clear // Final version (v4.0) released in December 2022 
numlabel, add

// Countries in the sample (30)
// Europe (20): Austria 040, Bulgaria 100, Cyprus 196, Czechia 203, Denmark 208, Estonia 233, Finland 246, Great Britain 826, Greece 300, Italy 380, Latvia 428, Lithuania 440, Russia 643, Netherlands 528, Norway 578, Poland 616, Slovakia 703, Slovenia 705, Spain 724, Sweden 752, Switzerland 756
// Latin America (4): Chile 152, Colombia 170, Guatemala 320, Mexico 484
// Asia (5): Hong Kong SAR 344, Indonesia 360, South Korea 410, Thailand 764
// Oceania (1): New Zealand 554

// Excluded country: Taiwan ROC (158) participated in the both surveys, but they do not have data for the Globalization Index. 
// Level-2 variables cannot be imputed to use HLM software. 

tab cntry 
keep if inlist(cntry, 040, 100, 196, 203, 208, 233, 246, 826, 300, 380) | inlist(cntry, 428, 440, 643, 528, 578, 616, 703, 705, 724, 752) | inlist(cntry, 756, 152, 170, 320, 484, 344, 360, 410, 764, 554) // | inlist(cntry, 158) 

summarize x002 // Year of birth
recode x002 (1937/1989 = .) (1999/2003 = .) (-5/-1 = .), gen(yr) // Keep respondents born in 1990-1998, who were in secondary education (i.e., ISCED 2 to 3) in 2008/2009, and completed upper secondary education at the time of EVS/WVS 2017.
// recode x002 (1937/1989 = .) (-5/-1 = .), gen(yr) // Keep respondents born in 1990-2004, who were in formal education system (i.e., ISCED 1 to 3) in 2008/2009.
drop if yr == . 

tab x003, missing // Age of respondents 
// The age must be between 18 and 32.
// Min: 18 = 2017 (year of survey) - 1998 (year of birth) - 1 (in case birthday didn't pass by the time of survey)
// Max: 32 = 2022 (year of survey) - 1990 (year of birth)
// However, 5 respondents show a discrepancy between the reported age and the age inferred from year of birth.
// No way to confirm whether they misentered their age or year of birth.
drop if x003 == -5 | x003 > 32 // Thus, drop those cases. 

sort cntry
tab cntry, missing
// Sample size: 7,230 young adults from 30 countries
// When Taiwan is included: 7,379 young adults from 31 countries

save "data_base.dta", replace

** A.1.2. ICCS 2009 National Questionnaire

use "0_Original dataset\ICCS2009_NQ.dta", clear

// Checking the difference in country ID (IDCNTRY)
// Britain = 8251 in ICCS 2009 instead of 826 in EVS/WVS Joint 2017

keep if inlist(IDCNTRY, 040, 100, 196, 203, 208, 233, 246, 8251, 300, 380) | inlist(IDCNTRY, 428, 440, 643, 528, 578, 616, 703, 705, 724, 752) | inlist(IDCNTRY, 756, 152, 170, 320, 484, 344, 360, 410, 764, 554) // | inlist(IDCNTRY, 158) 

gen cntry = IDCNTRY
recode cntry (8251 = 826)
sort cntry

** A.1.3. Merging datasets

merge 1:m cntry using "data_base.dta"
tab _merge
drop _merge
save "data_base.dta", replace 


********************************************************************************
* A.2. Outcome variables 
********************************************************************************

use "data_base.dta", clear
numlabel, add

** A.2.1. Identification of outcome variables  

// GC identy
// g063 // Q259. How close you feel: World

// Moral GC attitudes
// g007_35_b // Q62. Trust: People of another religion (B)
// g007_36_b // Q63. Trust: People of another nationality (B)
 
// Political GC attitudes
// e069_18a // Q82. Confidence: Major regional organization
// e069_20 // Q83. Confidence: The United Nations

codebook g063 // Identity
codebook g007_35_b g007_36_b // Moral attitudes
codebook e069_18a e069_20 // Political attitudes 

** A.2.2. Recoding variables 
// Coding scheme: GC = higher values 
// (e.g., 1 = GC; 0 = against GC).  

// GC identity
tab g063 // Q259. How close you feel: World
recode g063 (1 2 = 1) (3 4 = 0) (-5/-1 = .), gen(gcid) // 1 = very close or close 
tab gcid, missing // Binary

recode g063 (1 = 1) (2 3 4 = 0) (-5/-1 = .), gen(gcid_s) // 1 = very close only = strong GC identity 
tab gcid_s, missing // Binary
// Alternative measure of GC identity suggested in Pichler (2011) 

// Moral GC attitudes 
tab g007_35_b // Q62. Trust: People of another religion (B)
recode g007_35_b (1 = 4) (2 = 3) (3 = 2) (4 = 1) (-5/-1 = .), gen(mor_rel) // 4 = completely trust
tab mor_rel, missing // Ordinal

tab g007_36_b // Q63. Trust: People of another nationality (B)
recode g007_36_b (1 = 4) (2 = 3) (3 = 2) (4 = 1) (-5/-1 = .), gen(mor_for) // 4 = completely trust
tab mor_for, missing // Ordinal

// Political GC attitudes 
tab e069_18a // Q82. Confidence: Major regional organization
recode e069_18a (1 = 4) (2 = 3) (3 = 2) (4 = 1) (-5/-1 = .), gen(pol_reg) // 4 = great deal
tab pol_reg, missing // Ordinal

tab e069_20 // Q83. Confidence: The United Nations
recode e069_20 (1 = 4) (2 = 3) (3 = 2) (4 = 1) (-5/-1 = .), gen(pol_un) // 4 = great deal
tab pol_un, missing // Ordinal 

save "data.dta", replace 

** A.2.3. Validity and reliability check

use "data.dta", clear
numlabel, add

// Correlation between variables 
pwcorr gcid mor_* pol_* [aw = pwght] 
// polychoric gcid mor_* pol_* [aw = pwght] 
// Polychoric correlation adjusts Pearson's correlation when variables are ordinal or binary. 
// The results are not very different.

// Factor analysis to check validity 
factor gcid mor_* pol_* [aw = pwght] // Exploratory factor analysis
rotate, varimax
// predict moral political // Create the new variables for moral and political GC attitudes 
// summarize moral political

loadingplot, xline(0) yline(0) aspect(1) // Scatterplots of the loadings
// Factor 1 w/ eigenvalue 1.42 (moral GC attitudes): mor_rel, and mor_for
// Factor 2 w/ eigenvalue 0.79 (political GC attitudes): pol_reg, and pol_un 
// Factor 3 w/ eigenvalue 0.004 (GC identity): gcid (uniqueness = 0.99)

// Cronbach's alpha to check reliability within a factor 
// Rule of thumb: > 0.8 (good), > 0.7 (acceptable)

// Across all countries 
alpha mor_* // 0.88
alpha pol_* // 0.78

// By country 
gen alpha_mor = .
gen alpha_pol = .
levelsof cntry, local(levels) 
foreach var in mor pol {
	foreach n of local levels { 
		capture alpha `var'_* if cntry == `n' // capture suppresses any warning message and fore the loop continue. 
		replace alpha_`var' = r(alpha) if cntry == `n'
		}
	}

sort cntry
by cntry: summarize alpha_*
// For moral attitudes factor, alpha > 0.7 in all countries. 
// For political attitudes factor, alpha > 0.7 in all countries except:
// Colombia (0.62), Finland (0.70), Greece (0.60), Hong Kong (0.54), Norway (0.48), Sweden (0.68), Switzerland (0.68)

// Creating new variables by taking the average 
gen moral = (mor_rel + mor_for) / 2
gen polit = (pol_reg + pol_un) / 2
tab1 moral polit

save "data.dta", replace 

** A.2.4. Descriptive statistics (N1 = 7,230, N2 = 30)

use "data.dta", clear
numlabel, add

// Pooled data
summarize gcid mor* pol* 

// By country
tabstat gcid mor* pol*, by(cntry) statistics(mean sd n)


********************************************************************************
* A.3. Key independent variables
********************************************************************************

** A.3.1. Identification of key independent variables  

use "data.dta", clear
numlabel, add

// Key topics of GCE
// XA2G30A // Q30a. Topic at target grade: Human rights
// XA2G30C // Q30c. Topic at target grade: Understanding different cultures and ethnic groups 
// XA2G30L // Q30l. Topic at target grade: Environment

codebook XA2G30A XA2G30C XA2G30L

** A.3.2. Recoding variables

// Human rights 
tab XA2G30A 
recode XA2G30A (1 = 2) (2 = 1) (3 = 0), gen(gce_hr) // 2 = high emphasis
tab gce_hr, missing // Ordinal
quietly: tab gce_hr, gen(gce_hr)
// gce_hr1 = not provided (ref.)
// gce_hr2 = some emphasis
// gce_hr3 = high emphasis

// Understanding different cultures and ethnic groups 
tab XA2G30C 
recode XA2G30C (1 = 2) (2 = 1) (3 = 0), gen(gce_cul) // 2 = high emphasis
tab gce_cul, missing // Ordinal
quietly: tab gce_cul, gen(gce_cul)
// gce_cul1 = not provided (ref.)
// gce_cul2 = some emphasis
// gce_cul3 = high emphasis

// Environment
tab XA2G30L 
recode XA2G30L (1 = 2) (2 = 1) (3 = 0), gen(gce_sd) // 2 = high emphasis
tab gce_sd, missing // Ordinal
quietly: tab gce_sd, gen(gce_sd)
// gce_sd1 = not provided (ref.)
// gce_sd2 = some emphasis
// gce_sd3 = high emphasis

save "data.dta", replace 

** A.3.3. Descriptive statistics

use "data.dta", clear
numlabel, add

foreach var in gce_hr gce_cul gce_sd {
	tab cntry `var', missing
	}


********************************************************************************
* A.4. Control variables
********************************************************************************

** A.4.1. Identification of control variables 

use "data.dta", clear
numlabel, add

// Control variables at individual level 
// x001 // Gender
// g027a // Immigrant status 
// v001 // Father's immigrant status
// v002 // Mother's immigrant status
// x025a_01 // Educational attainment
// v097ef // Parental occupation 
// e033 // Left–right political spectrum

// Control variables at country level 
// g027a v001 v002 // % of immigrants 
// g006 // Level of patriotism 
// KOF Globalization Index
// Human Development Index (HDI)

codebook x001 g027a x025a_01 v001 v002 v097ef e033 g006 // Variables included in the EVS/WVS Joint 2017

** A.4.2. Recoding individual-level variables

// Gender 
tab x001 
recode x001 (1 = 0) (2 = 1) (-5/-1 = .), gen(female) // 1 = female
tab female, missing // Binary

// Immigrant status 
tab1 g027a v001 v002 
gen immg = 0
replace immg = . if g027a < 0
replace immg = 1 if g027a == 2 | v001 == 0 | v002 == 0 // 1 = 1st- and 2nd-generation immigrant
tab immg, missing // Binary

// Educational attainment
tab x025a_01
recode x025a_01 (0 1 = 0) (2 = 1) (3 = 2) (4 5 = 3) (6 7 8 = 4) (-5/-1 = .), gen(edu_corr) 
tab edu_corr, missing // Categorical to be used in correlation analyses 
recode x025a_01 (3 = 0) (0 1 = 1) (2 = 2) (4 5 = 3) (6 7 8 = 4) (-5/-1 = .), gen(edu) 
tab edu, missing // Categorical
quietly: tab edu, gen(edu)
// edu1 = Upper secondary education (ref.)
// edu2 = Primary education or no education 
// edu3 = Lower secondary education 
// edu4 = Some post-secondary education
// edu5 = University or above 

// Social class (proxy: parental occupation)
tab v097ef
recode v097ef (7 8 9 0 = 0) (3 4 5 6 10 = 1) (1 2 = 2) (-5/-1 11 66 = .), gen(class_corr) 
tab class_corr, missing // Categorical to be used in correlation analyses 
recode v097ef (3 4 5 6 10 = 0) (7 8 9 0 = 1) (1 2 = 2) (-5/-1 11 66 = .), gen(class) 
tab class, missing // Categorical 
quietly: tab class, gen(class) 
// class1 = middle (ref.)
// class2 = working
// class3 = upper 

// Left–right political spectrum
tab e033 
recode e033 (-5/-1 = .), gen(lr) // 1 = far left, 10 = far right    
tab lr, missing // Ordinal

sort cntry
save "data.dta", replace 

** A.4.3. Recoding country-level variables from the EVS/WVS Joint 2017

use "0_Original dataset\EVS_WVS_Joint_Stata_v4_0.dta", clear // Full EVS/WVS Joint dataset 
numlabel, add

// % of immigrants: recoding individuals' immigrant status 
// (same coding as the immigrant status variable above) 
tab1 g027a v001 v002 
gen immg = 0
replace immg = . if g027a < 0
replace immg = 1 if g027a == 2 | v001 == 0 | v002 == 0 // 1 = 1st- and 2nd-generation immigrant
tab immg, missing // Binary

// Level of patriotism: recoding the original variable at individual level
tab g006 
recode g006 (1 = 4) (2 = 3) (3 = 2) (4 = 1) (-5/-1 = .), gen(pat) // 4 = very proud 
tab pat, missing // Ordinal

// Creating new national-level variables by taking weighted means  
gen mimmg = . 
gen mpat = .   
levelsof cntry, local(levels) 
quietly foreach var in immg pat { 
	foreach n of local levels { 
		summarize `var' [w = gwght] if cntry == `n', detail 
		replace m`var' = r(mean) if cntry == `n' 
		}
	}
gen mimmgpp = mimmg * 100 // To change unit to percentage point (min. = 0 to max. = 100)
keep cntry mimmgpp mpat 
sort cntry 
quietly by cntry: gen dup = cond(_N == 1, 0, _n)
drop if dup > 1
list 

// Merging mimmgpp and mpat variables with the master data  
merge 1:m cntry using "data.dta"
tab _merge
tab cntry if _merge == 1, missing // Countries that are not included in the sample 
tab cntry if _merge == 3, missing
drop if _merge == 1 
drop _merge

sort cntry
save "data.dta", replace 

** A.4.4. Recoding country-level variables from external sources

// KOF Globalization Index (KOF GI)
use "data.dta", clear
numlabel, add
sort cntry
// Merging the master data with KOF GI data
merge m:1 cntry using "data_kof.dta"
tab _merge
tab cntry if _merge == 2, missing // New Zealand is an extra country and it is not included in the final sample.
tab cntry if _merge == 3, missing
drop if _merge == 2
drop _merge
// Creating the new variable for KOF GI based on the survey year 
gen gi = .
replace gi = KOFGI2017 if year == 2017
replace gi = KOFGI2018 if year == 2018
replace gi = KOFGI2019 if year == 2019
replace gi = KOFGI2020 if year >= 2020 
// Since 2020 is the most recent year of data collection, 
// data in 2020 is used as the proxy for 2021 and 2022.    

// Human Development Index (HDI)
merge m:1 cntry using "data_hdi.dta"
tab _merge // Perfect match
drop _merge
// Creating the new variable for KOF GI based on the survey year 
gen hdipp = .
replace hdipp = hdipp2017 if year == 2017
replace hdipp = hdipp2018 if year == 2018
replace hdipp = hdipp2019 if year == 2019
replace hdipp = hdipp2020 if year == 2020 
replace hdipp = hdipp2021 if year >= 2021 
// Since 2021 is the most recent year of data collection, 
// data in 2021 is used as the proxy for 2022.    

save "data.dta", replace 

** A.4.5. Descriptive statistics (N1 = 7,230, N2 = 30)

use "data.dta", clear
numlabel, add

// Pooled data
summarize female immg edu1-edu5 class1-class3 lr mimmgpp mpat gi hdipp

// By country
tabstat female immg edu1-edu5 class1-class3 lr, by(cntry) statistics(mean sd n) // Individual-level variables
tabstat mimmgpp mpat gi hdipp, by(cntry) statistics(mean sd n) // Country-level variables


********************************************************************************
* A.5. Mulitple imputation (M = 20)
********************************************************************************

use "data.dta", clear
numlabel, add

** A.5.1. Inspection of missing data 

// Note that missing data are included only in the individual-level variables.
// Country-level variables are complete. 

// Missing pattern
misstable summarize gcid gcid_s mor_rel mor_for pol_reg pol_un female immg edu class lr 
misstable patterns gcid gcid_s mor_rel mor_for pol_reg pol_un female immg edu class lr, frequency bypatterns 

// Little's MCAR test 
// help mcartest // To install the add-on package 
mcartest gcid mor_rel mor_for pol_reg pol_un female immg edu class lr
// Chi-squared (847) = 1353.4 with p-value < 0.001
// Missing data are not MCAR.

** A.5.2. Additional variables to help predict plausible values

// Binary variables (2)
// a124_02 		to add_nrac	// Q19. Neighbours: People of a different race
// a124_06 		to add_nfor	// Q21. Neighbours: Immigrants/foreign workers 

// Ordinal variables (4)
// f120 		to add_abor	// Q184. Justifiable: Abortion
// e037			to add_welf	// Q108. Government responsibility
// x047_wvs7	to add_incw	// Scale of incomes (WVS7)
// x047e_evs5	to add_ince	// Scale of incomes (EVS5)

// Categorical variables (2)
// v004rm	 	to add_edm1-add_edm3 // Highest educational level attained - Respondent´s Mother
// v004rf	 	to add_edf1-add_edf3 // Highest educational level attained - Respondent´s Father 

tab1 a124_02 a124_06 f120 e037 x047_wvs7 x047e_evs5 v004rm v004rf, missing
recode a124_02 a124_06 f120 e037 x047_wvs7 x047e_evs5 v004rm v004rf (-5/-1 = .), gen(add_nrac add_nfor add_abor add_welf add_incw add_ince add_edm add_edf) 
tab1 add_nrac add_nfor add_abor add_welf add_incw add_ince add_edm add_edf, missing
quietly foreach var in add_edm add_edf {
	tab `var', gen(`var') 	
	} // Dummy coding for the categorical variables 
tab1 add_ed*, missing
	
misstable summarize add_nrac add_nfor add_abor add_welf add_incw add_ince add_edm add_edf

** A.5.3. Multiple imputation 

// Setup for MI
xtset, clear // Because of dropping off the year variable which is included in the original dataset (see: https://www.stata.com/statalist/archive/2010-01/msg00044.html)
mi set flong
mi register imputed gcid gcid_s mor_rel mor_for pol_reg pol_un female immg edu2-edu5 class2-class3 lr ///
	add_nrac add_nfor add_abor add_welf add_incw add_ince add_edm2-add_edm3 add_edf2-add_edf3
mi register passive moral polit

// MI using MICE method 
mi impute chained (logit) gcid gcid_s female immg edu2-edu5 class2-class3 add_nrac add_nfor add_edm2-add_edm3 add_edf2-add_edf3 ///
	(ologit) mor_rel mor_for pol_reg pol_un lr add_abor add_welf add_incw add_ince, add(20) augment force rseed(10000)

// Checking missingness in the imputed data 
misstable summarize gcid gcid_s mor_rel mor_for pol_reg pol_un female immg edu2-edu5 class2-class3 lr if _mi_m > 0
// No missing data 

// Redefining passive variables 
mi passive: replace moral = (mor_rel + mor_for) / 2
mi passive: replace polit = (pol_reg + pol_un) / 2
tab _mi_m moral, missing
tab _mi_m polit, missing
// No missing data in the imputed sets

save "data_mi.dta", replace 


********************************************************************************
* A.6. Getting ready to export data to HLM 
********************************************************************************

** A.6.1. Level-1 data

use "data_mi.dta", clear
numlabel, add

sort cntry
rename wght_eq1000 ewght // Because the variable name is too long to be recognized in HLM software
keep cntry _mi_m gwght /// ewght pwght // Include alternative weight variables if necessary. 
	gcid gcid_s moral mor_rel mor_for polit pol_reg pol_un  ///
	female immg edu2-edu5 class2-class3 lr 
	
save "0_HLM dataset\data_lv1.dta", replace 
quietly foreach n of numlist 1/20 {
	use if _mi_m == `n' using "0_HLM dataset\data_lv1.dta"
	save "0_HLM dataset\data`n'.dta", replace
    }

** A.6.2. Level-2 data  

use "data_mi.dta", clear
numlabel, add

sort cntry 
keep if _mi_m == 0
keep cntry gce_hr2-gce_hr3 gce_cul2-gce_cul3 gce_sd2-gce_sd3 ///
	mimmgpp mpat gi hdipp 

save "0_HLM dataset\data_lv2.dta", replace 
	
// End of preparation 
// Move to do_B_analysis or HLM software. 


********************************************************************************
* Appendices
/*******************************************************************************

** Appendix A: Other possible outcome variables 

// a124_02 // Q19. Neighbours: People of a different race
// a124_06 // Q21. Neighbours: Immigrants/foreign workers 
// g052 // Q121. Evaluate the impact of immigrants on the developmen of [your country]
// a035 // Q12. Important child qualities: tolerance and respect for other people 
// b008 // Q111. Protecting environment vs. Economic growth

tab a124_02 // Q19. Neighbours: People of a different race
recode a124_02 (0 = 1) (1 = 0) (-5/-1 = .), gen(mor_nrac) // 1 = okay to have as a neighbor
tab mor_nrac, missing // Binary

tab a124_06 // Q21. Neighbours: Immigrants/foreign workers 
recode a124_06 (0 = 1) (1 = 0) (-5/-1 = .), gen(mor_nfor) // 1 = okay to have as a neighbor
tab mor_nfor, missing // Binary

tab g052 // Q121. Evaluate the impact of immigrants on the developmen of [your country]
recode g052 (-5/-1 = .), gen(mor_immg) // 5 = very good
tab mor_immg, missing // Ordinal

tab a035 // Q12. Important child qualities: tolerance and respect for other people 
recode a035 (-5/-1 = .), gen(mor_tol) // 1 = agree to teach tolerance
tab mor_tol, missing // Binary

tab b008 // Q111. Protecting environment vs. Economic growth
recode b008 (1 = 1) (2 = 0) (-5/-1 3 = .), gen(mor_envn) // 1 = protecting environment
tab mor_envn, missing // Binary

** Appendix B: Other GCE-related variables from ICCS 2009

// CCE
// XA2G12 // Q12. Aims of educational policy re. role of the school system in preparing citizens
// XA2G09 // Q9. Priority given to CCE
// XA2G31A // Q31a. Methods at target grade: Official publication containing the curriculum
// XA2G31B // Q31b. Methods at target grade: Mandated or recommended textbook(s)
// XA2G31C // Q31c. Methods at target grade: Instructional or pedagogic guides
// XA2G37A // Q37a. Pre-service/initial teacher training for CCE
// XA2G41A // Q41a. Student assessment in lower secondary schools in relation to CCE

// National education system
// XA2G01 // Q1. Location of major responsibilities in school education system
// XA2G03C // Q3c. Years of compulsory education

// CCE
tab XA2G09 // Priority of CCE
recode XA2G09 (1 = 2) (2 = 1) (3 4 = 0), gen(cce_pri)
tab cce_pri

tab XA2G31A // Official publication for CCE curriculum  
recode XA2G31A (1 = 1) (2 = 0) (3 = .), gen(cce_cur)
tab cce_cur

tab XA2G31B // Textbooks for CCE  
recode XA2G31B (1 = 1) (2 = 0) (3 = .), gen(cce_txt)
tab cce_txt

tab XA2G31C // Pedagogic guides for CCE  
recode XA2G31C (1 = 1) (2 = 0) (3 = .), gen(cce_pdg)
tab cce_pdg

tab XA2G37A // Pre-service teacher training for CCE
recode XA2G37A  (1 = 1) (2 = 0), gen(cce_trn)
tab cce_trn

tab XA2G41A // Student assessment on CCE
recode XA2G41A  (1 = 1) (2 = 0) (3 = .), gen(cce_ass)
tab cce_ass

// National education system
tab XA2G01 // Centralized education system
gen central = XA2G01 == 1 if !missing(XA2G01) 
tab central

tab XA2G03C // Years of compulsory education
recode XA2G03C (99 = .), gen(compul)
tab compul

** Appendix C: Other control variables used in the prior studies  

// Educational attainment (alternative recoding scheme)
recode x025a_01 (2 3 = 0) (0 1 = 1) (4 5 = 2) (6 7 8 = 3) (-5/-1 = .), gen(edu) 
tab edu, missing // Categorical
quietly: tab edu, gen(edu)
edu1 = Secondary education (ref.)
edu2 = Primary education or no education 
edu3 = Some post-secondary education
edu4 = University or above 

// Patriotism 
tab g006 
recode g006 (1 = 4) (2 = 3) (3 = 2) (4 = 1) (-5/-1 = .), gen(pat) // 4 = very proud 
tab pat, missing // Ordinal

// Religion
tab f025
recode f025 (0 = 0) (1/9 = 1) (-5/-1 = .), gen(rel) // 1 = having a religion
tab rel, missing // Binary
recode f025 (0 = 0) (1 = 1) (2 = 2) (3 = 3) (5 = 4) (4 6/9 = 5) (-5/-1 = .), gen(relsub)
quietly: tab relsub, gen(relsub)
// relsub1 = no religion (ref.)
// relsub2 = Roman Catholic 
// relsub3 = Protestant 
// relsub4 = Orthodox 
// relsub5 = Muslim
// relsub6 = Other

** Appendix D: Merging additional data about subject social class from the WVS7  

use "1c_Additional data\WVS7\WVS_Cross-National_Wave_7_Stata_v5_0.dta", clear
numlabel, add

// Social class (subjective)
tab Q287, missing
recode Q287 (-5/-1 = .), gen(add_scs)
tab add_scs, missing
quietly: tab add_scs, gen(add_scs) // Dummy coding

keep S007 add_scs*
rename S007 uniqid // S007 in WVS7 = uniqid in EVS/WVS Joint 2017 = respondent ID
sort uniqid

merge 1:1 uniqid using "data.dta"
tab _merge
tab cntry if _merge == 1, missing // Countries that are not included in the sample 
tab cntry if _merge == 3, missing
drop if _merge == 1 
drop _merge

sort cntry
save "data.dta", replace 

** Appendix E: Merging GDP data  

// GDP
merge m:1 cntry using "data_gdp.dta"
tab _merge // Perfect match
drop _merge
// Creating the new variable for KOF GI based on the survey year 
gen gdpmil = .
replace gdpmil = gdpmil2017 if year == 2017
replace gdpmil = gdpmil2018 if year == 2018
replace gdpmil = gdpmil2019 if year == 2019
replace gdpmil = gdpmil2020 if year == 2020 
replace gdpmil = gdpmil2021 if year == 2021 
replace gdpmil = gdpmil2022 if year == 2022 

save "data.dta", replace 

*/
