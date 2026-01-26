# GAFL5320

This repository contains survey data and R scripts used to analyze gender differences and program outcomes in the Zambia Community Forests Program (CFP) evaluation.

## 📂 Data Files

- **`CFP Headperson FINAL Combined no pii.dta`**  
  → Survey responses from **community headpersons**.

- **`CFP Household Women FINAL Combined no pii_wt.dta`**  
  → Survey responses from **household heads** and **wives of male household heads**.

- **`Q1Headperson.dta`**  
  → Subset used for RQ1 analysis with headperson data.

- **`Q1household_stat.dta`**  
  → Subset used for RQ1 analysis with household data.

## 📄 Variable Dictionaries

- **`cfpheadperson_FINAL_V3.xlsx`**  
- **`cfp_hh_wom_FINAL_V3.xlsx`**  
  → Variable names, corresponding survey questions, answer choices, and other metadata.

## 🧮 R Analysis Scripts
- **`HH+HP_10182025.R`**
    → Updated data cleaning for **headperson survey** and **household and women survey**

- **`Headperson.R`**  
  → Analysis of **headperson survey**:
  - **RQ1**: Gendered perceptions of CFP importance, fairness, satisfaction, and adequacy  
  - **RQ2**: Gendered differences in reported climate stressors, observed environmental change, and adaptive concern

- **`Household.R`**  
  → Analysis of **household and women survey**:
  - **RQ1**: Gender differences in perceived CFP impacts (logistic regressions)
  - **RQ2**: Gender differences in climate resilience and coping/adaptation strategies (logistic & negative binomial models)
  - **RQ3**: Factors predicting **benefit receipt**
  - **RQ4**: Factors predicting **support for REDD+** and trust in governance (OLS + PCA indices + interactions)

## 📝 Support Files

- **`asset.txt`**  
  → A list of variable name roots used to construct the **durable asset PCA index**, referenced in `Household.R`.

## ❌ Deprecated

- **`Q1_do.do`, `Q2.do`**  
  → Former Stata `.do` files used for RQ1 and RQ2 are no longer in use. All analysis has been migrated to R.

---

**Note:** All regression models, PCA indices, and summary statistics are now handled in R. This repository is designed to support reproducible evaluation of CFP impacts with a focus on gender and equity.


---

## How to Use This Repository
1. Download the relevant `.dta` files.
2. Open RStudio and run the `.R` files in order.
3. Use the variable dictionaries (`.xlsx`) for reference while analyzing data.

If you have any questions, feel free to reach out!
