** Q2: Is there a difference in how women and men report adaptation and resilience to climate change and the recent drought in Zambia? **
* see overleaf
preserve
keep drought_impactecon drought_impactfood drought_adapt wdrought wdrought_respond wdrought_result
save Q2.dta, replace
restore

* Clean data in Q2.dta
rename gender_respondent gender

* Check for missing values in key variables
misstable summarize drought_impactecon drought_impactfood drought_adapt wdrought wdrought_respond wdrought_result gender age ed inc_annual

* Drop observations with missing values in key variables
drop if missing(drought_impactecon, drought_impactfood, drought_adapt, wdrought, wdrought_respond, wdrought_result, gender, age, ed, inc_annual)

* Replace missing values in other variables with the median
egen med_drought_impactecon = median(drought_impactecon)
replace drought_impactecon = med_drought_impactecon if missing(drought_impactecon)

egen med_drought_impactfood = median(drought_impactfood)
replace drought_impactfood = med_drought_impactfood if missing(drought_impactfood)

egen med_drought_adapt = median(drought_adapt)
replace drought_adapt = med_drought_adapt if missing(drought_adapt)

egen med_wdrought = median(wdrought)
replace wdrought = med_wdrought if missing(wdrought)

egen med_wdrought_respond = median(wdrought_respond)
replace wdrought_respond = med_wdrought_respond if missing(wdrought_respond)

egen med_wdrought_result = median(wdrought_result)
replace wdrought_result = med_wdrought_result if missing(wdrought_result)

egen med_age = median(age)
replace age = med_age if missing(age)

egen med_ed = median(ed)
replace ed = med_ed if missing(ed)

egen med_inc_annual = median(inc_annual)
replace inc_annual = med_inc_annual if missing(inc_annual)

* Drop temporary median variables
drop med_drought_impactecon med_drought_impactfood med_drought_adapt med_wdrought med_wdrought_respond med_wdrought_result med_age med_ed med_inc_annual

* Check for missing values again
misstable summarize drought_impactecon drought_impactfood drought_adapt wdrought wdrought_respond wdrought_result gender age ed inc_annual

* Save cleaned dataset
save Q2_clean.dta, replace

* Descriptive Statistics by Gender

