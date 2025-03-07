** Household Data Analysis
* Make sure the data is in the right format
tab gender_respondent
tab gender_respondent, nolabel

* Select the data for Question 1 from the CFP Household Women FINAL Combined no pii_wt dataset (still need more variables)
preserve
keep gender_respondent cfp_aware cfp_important cfp_satisfied cfp_enough cfp_fair age ed inc_annual
save Q1household_stat.dta, replace
restore

* Clean data in Q1.dta
rename gender_respondent gender
drop if cfp_aware ==0
* Check for missing values in key variables
misstable summarize gender cfp_aware cfp_important cfp_satisfied cfp_enough cfp_fair age ed inc_annual

* gender, cfp_aware, age, ed has few missing values, so they're dropped
drop if missing(gender, cfp_aware, age, ed)
misstable summarize gender cfp_aware cfp_important cfp_satisfied cfp_enough cfp_fair age ed inc_annual

* use median imputation for the 112 instances of missing income. 
egen med_inc = median(inc_annual)
replace inc_annual = med_inc if missing(inc_annual)
drop med_inc

* Since the rest of the data (cfp_important cfp_satisfied cfp_enough cfp_fair) has around 40% of missing values, I will not try to use the median to impute them. Also, because I don't need them to be in place all at once, I will not drop them right now; when I proceed to analyze them separately in the following sections, I will drop them respectively.


* Preliminary Analysis
* Understanding the general prception of cfp benefits
sum2docx gender cfp_important cfp_satisfied cfp_enough cfp_fair age ed inc_annual using  C:\Users\陈夏恩\Documents\University\Study_Abroad\UPenn\2025SS\GAFL5320_ID\WorkInProgress\Tables\Q1HouseholdSumStat.docx,replace stats(N mean(%9.4f) sd min(%9.2f) median(%9.2f) max(%9.2f))  title("Table X: Summary Statistics")
 
histogram cfp_important, percent title("Distribution of CFP Importance") lcolor(blue) fcolor(blue%30)
histogram cfp_satisfied, percent title("Distribution of CFP Satisfaction") lcolor(red) fcolor(red%30)
histogram cfp_enough, percent title("Distribution of CFP Adequacy") lcolor(yellow) fcolor(yellow%30)
histogram cfp_fair, percent title("Distribution of CFP Fairness") lcolor(green) fcolor(green%30)

* Descriptive Statistics by Gender
sum cfp_important cfp_satisfied cfp_enough cfp_fair if gender == 0
sum cfp_important cfp_satisfied cfp_enough cfp_fair if gender == 1

* Boxplots to visualize gender differences (doesn't help, might delete it in the end)
graph box cfp_important, over(gender) title("CFP Importance by Gender")
graph box cfp_satisfied, over(gender) title("CFP Satisfaction by Gender")
graph box cfp_enough, over(gender) title("CFP Compensation Adequacy by Gender")
graph box cfp_fair, over(gender) title("CFP Fairness by Gender")

* Chi-square test for gender differences in categorical responses
preserve
keep gender cfp_aware cfp_important cfp_satisfied cfp_enough cfp_fair age ed inc_annual
drop if missing(cfp_important)
tab cfp_important gender, chi2
restore

preserve
keep gender cfp_aware cfp_important cfp_satisfied cfp_enough cfp_fair age ed inc_annual
drop if missing(cfp_satisfied)
tab cfp_satisfied gender, chi2
restore

preserve
keep gender cfp_aware cfp_important cfp_satisfied cfp_enough cfp_fair age ed inc_annual
drop if missing(cfp_enough)
tab cfp_enough gender, chi2
restore

preserve
keep gender cfp_aware cfp_important cfp_satisfied cfp_enough cfp_fair age ed inc_annual
drop if missing(cfp_fair)
tab cfp_fair gender, chi2
restore

* Convert categorical variables to binary, assuming 3+ means agreement
gen cfp_important_binary = (cfp_important >=3) if !missing(cfp_important)
gen cfp_satisfied_binary = (cfp_satisfied >=3) if !missing(cfp_satisfied)
gen cfp_enough_binary = (cfp_enough >=3) if !missing(cfp_enough)
gen cfp_fair_binary = (cfp_fair >=3) if !missing(cfp_fair)

* Logistic regression for each outcome variable (I need to find more control variables, like dependence on forest, distance to forest, marital status, household size, whether they received benefits/amount of benefits --> fairness, redd_steal)
logit cfp_important_binary gender age ed inc_annual
logit cfp_satisfied_binary gender age ed inc_annual
logit cfp_enough_binary gender age ed inc_annual
logit cfp_fair_binary gender age ed inc_annual


* Linear regression (doesn't work very well... might discard it)
reg cfp_important gender age ed inc_annual
reg cfp_enough gender age ed inc_annual
reg cfp_satisfied gender age ed inc_annual
reg cfp_fair gender age ed inc_annual 



** Headperson Data Analysis
* Select the data for Question 1 from the CFP Headperson FINAL Combined no pii dataset
preserve
keep hgender cfp_aware cfp_important cfp_satisfied cfp_enough cfp_fair hage heduc
save Q1Headperson_stat.dta, replace
restore

* Clean data in Q1Headperson.dta
* generate a binary(0,1) variable "gender" for analysis
describe hgender
list hgender in 1/10
decode hgender, gen(hgender_str)
tab hgender hgender_str
recode hgender (1=0) (2=1), gen(gender)
tab hgender gender

rename hage age
rename heduc edu

* Remove respondents who were unaware of CFP
drop if cfp_aware == 0

* Check for missing values in key variables
misstable summarize gender cfp_aware cfp_important cfp_satisfied cfp_enough cfp_fair age edu

* Drop cases with missing values in key demographic variables (gender, age, ed)
drop if missing(gender, age, edu)

* Check missingness after dropping
misstable summarize gender cfp_aware cfp_important cfp_satisfied cfp_enough cfp_fair age ed

* Preliminary Analysis
* Understanding the general prception of cfp benefits
sum2docx gender cfp_important cfp_satisfied cfp_enough cfp_fair age edu  using  C:\Users\陈夏恩\Documents\University\Study_Abroad\UPenn\2025SS\GAFL5320_ID\WorkInProgress\Tables\Q1HeadpersonSumStat.docx,replace stats(N mean(%9.4f) sd min(%9.2f) median(%9.2f) max(%9.2f))  title("Table X: Summary Statistics")