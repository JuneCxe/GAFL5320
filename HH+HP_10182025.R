# --- Load Required Packages ---
library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)
library(janitor)
library(officer)
library(flextable)
library(stargazer)
library(MASS)
library(glue)
library(tidyverse)
library(performance)
library(car)
library(psych)

#=========================#
# IMPLEMENTATION ANALYSIS #
#=========================#

# --- Load Data from Stata .dta file ---
setwd("~/University/Study_Abroad/UPenn/2025SS/GAFL5320_ID/WorkInProgress")
df <- read_dta("CFP Household Women FINAL Combined no pii_wt.dta")
head_df <- read_dta("CFP Headperson FINAL Combined no pii.dta")

# -- Making the education variable more interpretable by grouping it into 3 levels ---
# Group ed into three levels：Low / Middle / High
df <- df %>%
  mutate(
    ed_group = case_when(
      ed >= 1 & ed <= 6  ~ "Low",              # Primary incomplete or lower
      ed >= 7 & ed <= 9 ~ "Middle",           # Completed primary or lower secondary
      ed >= 10 & ed <= 15 ~ "High",            # Senior secondary and above
      TRUE ~ NA_character_                    # Catch missing, 888, 999, etc.
    ),
    ed_group = factor(ed_group, levels = c("Low", "Middle", "High"))
  )

# Group ed_head into three levels：Low / Middle / High
df <- df %>%
  mutate(
    ed_head_group = case_when(
      ed_head >= 1 & ed_head <= 6  ~ "Low",              # Primary incomplete or lower
      ed_head >= 7 & ed_head <= 9 ~ "Middle",           # Completed primary or lower secondary
      ed_head >= 10 & ed_head <= 15 ~ "High",            # Senior secondary and above
      TRUE ~ NA_character_                    # Catch missing, 888, 999, etc.
    ),
    ed_head_group = factor(ed_head_group, levels = c("Low", "Middle", "High"))
  )

# --- Apply ordered factor to annual income ---
# Income is divided into 7 groups that represent increasing annual income as the number increases
# Thus, we apply ordered factor to the annual income
df$inc_annual_f <- factor(
  df$inc_annual,
  levels = 1:7,
  ordered = TRUE
)

# --- Calculate mean dependence on forest (higher value = more dependent) ---
df <- df %>%
  mutate(
    hfconsume_avg = rowMeans(.[ , grep("^hfconsume_", names(.)), drop = FALSE], na.rm = TRUE),
    hfincome_avg  = rowMeans(.[ , grep("^hfincome_", names(.)), drop = FALSE], na.rm = TRUE),
    forminutes_avg = rowMeans(.[ , grep("^forminutes_", names(.)), drop = FALSE], na.rm = TRUE)
  )

# --- Create leader perception index (PCA on complete cases only) ---
# fleader_bribes and fleader_transparent were excluded (why?)

# Define variables
leader_vars <- c(
  "fleader_trust",
  "fleader_protect",
  "fleader_fair",
  "fleader_work"
)

# Count missing variables to determine whether to use complete cases only or impute
na_count <- rowSums(is.na(df[, leader_vars]))
table(na_count)
# Not too many missing, we'll use complete rows only

# Extract and standardize only complete rows
leader_complete_idx <- complete.cases(df[, leader_vars])
leader_scaled <- scale(df[leader_complete_idx, leader_vars])

# Run PCA
leader_pca <- prcomp(leader_scaled, center = TRUE, scale. = TRUE)
leader_index <- as.numeric(scale(leader_pca$x[, 1]))  # use PC1

# Create full-length vector, NA where incomplete, and put in PCA score where complete = True
df$leader_index <- NA
df$leader_index[leader_complete_idx] <- leader_index

# --- Create dummy variables for de facto governance power ---
df <- df %>%
  mutate(
    forestlead_village = ifelse(forest_leaders %in% c(1, 2), 1, 0),
    forestlead_chiefdom = ifelse(forest_leaders %in% c(3, 4), 1, 0),
    forestlead_comm = ifelse(forest_leaders %in% 6:9, 1, 0),
    forestlead_gov = ifelse(forest_leaders %in% c(11, 12), 1, 0),
    forestlead_private = ifelse(forest_leaders == 10, 1, 0),
    forestlead_collective = ifelse(forest_leaders == 13, 1, 0),
    forestlead_lack = ifelse(forest_leaders %in% c(15, 16), 1, 0)
  ) %>%
  mutate(across(starts_with("forestlead_"), ~ ifelse(is.na(forest_leaders) | forest_leaders %in% c(97, 888, 999), NA, .)))


# --- Create dummy variable to indicate whether the participant is involved in forest management organizations ---
df <- df %>%
  mutate(
    involved_hhmem_yn = ifelse(involved_hhmem == 0, 0, 1)
  )

# --- Generating the Livestock PCA Variable ---

# 1. Read list of livestock variable prefixes
livestock_vars <- read_lines("livestock_asset.txt")

# 2. Select all relevant livestock variables
livestock_df <- df |>
  dplyr::select(starts_with(livestock_vars)) |>
  
  # 3. Impute missing values so PCA can be computed
  mutate(
    # (a) For quantity columns: set to 0 where the corresponding _yn == 0
    across(
      !ends_with("_yn"),
      \(x) if_else(
        pick(all_of(glue(cur_column(), "_yn"))) == 0,
        true = 0, false = x
      )
    ),
    
    # (b) For any remaining NAs: impute with column means
    across(
      everything(),
      \(x) if_else(is.na(x), true = mean(x, na.rm = TRUE), false = x)
    )
  )

# 4. Run PCA on standardized variables
livestock_pca <- prcomp(livestock_df, center = TRUE, scale. = TRUE)

# 5. Standardize data manually (to ensure reproducibility)
livestock_scaled <- scale(
  as.matrix(livestock_df),
  scale = livestock_pca$scale,
  center = livestock_pca$center
)

# 6. Compute first principal component (PC1) scores
livestock_scores <- livestock_scaled %*% livestock_pca$rotation[, "PC1"]

# 7. Reverse direction so that higher values = higher wealth
df <- df |>
  mutate(
    livestock_pca = -1 * as.double(livestock_scores),
    
    # 8. Log-transform the index (to reduce skewness)
    log_livestock_pca = log(1 + livestock_pca - min(livestock_pca))
  )

# 9. Inspect PCA loadings and variance explained
summary(livestock_pca)
livestock_pca$rotation[, 1]


# --- Generate Durable Asset PCA Variable ---

# 1. Read list of durable asset prefixes
durable_vars <- read_lines("durable_asset.txt")

# 2. Select variables and impute missing values
durable_df <- df |>
  dplyr::select(starts_with(durable_vars)) |>
  mutate(
    # (a) Set quantity = 0 where ownership (_yn) = 0
    across(
      !ends_with("_yn"),
      \(x) if_else(
        pick(all_of(glue(cur_column(), "_yn"))) == 0,
        true = 0,
        false = x
      )
    ),
    # (b) Replace remaining NAs with column mean
    across(
      everything(),
      \(x) if_else(is.na(x), true = mean(x, na.rm = TRUE), false = x)
    )
  )

# 3. Run PCA
durable_pca <- prcomp(durable_df, center = TRUE, scale. = TRUE)

# 4. Manual standardization (exact match to PCA)
durable_scaled <- scale(
  as.matrix(durable_df),
  center = durable_pca$center,
  scale  = durable_pca$scale
)

# 5. Extract PC1 scores
durable_scores <- durable_scaled %*% durable_pca$rotation[, "PC1"]

# 6. Reverse sign + log-transform
df <- df |>
  mutate(
    durable_pca     = -1 * as.double(durable_scores),
    log_durable_pca = log(1 + durable_pca - min(durable_pca))
  )


# --- Social index ---
# Select only the social capital variables
# Then recode "don't know"/"prefer not to say" responses to NA
social_vars <- c("likelihood_help",      # community willingness to lend money
                 "likelihood_help2",     # collectively work for the community's benefit (yn)
                 "likelihood_help3",     # community willingness to help when unfortunate incidences happen
                 "attend_benefit"       # frequency of joint efforts to ask for benefits
                 )

social_df <- df %>%
  dplyr::select(all_of(social_vars)) %>%
  dplyr::mutate(across(everything(), ~ ifelse(.x %in% c(888, 999), NA, .x)))

# Reverse code 'likelihood_help' so that higher number = more people are willing to help
# Original: 0–4, where 0 = No one, 1 = Everyone，2 = Most people, 3 = Some people, 4 = A few people
# New: 1–5, where 1 = No one, 5 = Everyone
social_df <- social_df %>%
  mutate(
    likelihood_help = case_when(
      likelihood_help == 0 ~ 1,   # No one → 1
      likelihood_help == 1 ~ 5,   # Everyone → 5
      likelihood_help == 2 ~ 4,   # Most people → 4
      likelihood_help == 3 ~ 3,   # Some people → 3
      likelihood_help == 4 ~ 2,   # A few people → 2
      TRUE ~ likelihood_help
    )
  )

# Reverse code attend_benefit so higher number = more social participation
# (1 = Weekly, 4 = Never → becomes 4 = Weekly, 1 = Never)
social_df <- social_df %>%
  mutate(
    attend_benefit = case_when(
      attend_benefit == 1 ~ 4,
      attend_benefit == 2 ~ 3,
      attend_benefit == 3 ~ 2,
      attend_benefit == 4 ~ 1,
      TRUE ~ attend_benefit
    )
  )

# Check for remaining NAs or zero-variance columns
colSums(is.na(social_df))
sapply(social_df, sd, na.rm = TRUE)

# NAs are limited and there are no sign of zero variance, we can proceed
# Standardize the variables using their mean
df <- df %>%
  mutate(
    across(c(likelihood_help, likelihood_help2, likelihood_help3, attend_benefit),
           ~ scale(.x), .names = "{.col}_std")
  ) 

# Check correlation among variables, round by 2
round(
  cor(
    df %>% dplyr::select(ends_with("_std")),
    use = "pairwise.complete.obs"
  ),
  2
)

# Pairwise correlations among the four social capital indicators were weak (|r|max < 0.23), 
# suggesting they capture distinct aspects of social relations.
# Therefore, we use an equal-weight standardized mean index as our main measure of overall social capital

# Take row-wise mean across standardized variables and rescale them to (0,1)
df <- df %>%
  mutate(
    social_capital_mean = rowMeans(across(ends_with("_std")), na.rm = TRUE),
    social_capital_mean_01 = scales::rescale(social_capital_mean, to = c(0, 1))
  )

# ======= Load HH level covariates & outcomes ======== #
hh_df <- df[, c(
  "chief",
  "vilid",
  "gender_respondent",
  "age",
  "age_head",
  "ed_group",
  "ed_head_group",
  "born",
  "born_head",
  "hire",
  "econstatus",
  "hh_size",
  "forminutes_avg",
  "hfconsume_avg",
  "hfincome_avg",
  "crb_yn",
  "leader_index",
  "forest_satisfied",
  "involved_hhmem_yn",
  "inc_annual_f",
  "log_livestock_pca",
  "log_durable_pca",
  "social_capital_mean_01",
  "hmrelate",
  "chiefrelate1",
  "fleader_bribes",
  grep("^forestlead_", names(df), value = TRUE),
  
  # CFP Benefit Outcomes
  "any_ben",
  "cfp_aware",
  "cfp_access",
  "cfp_ben",
  "cfp_satisfied",
  "cfp_fair",
  
  # Forest Governance Outcomes
  "hhinform_revexp",    # should reverse so that 5 = strongly agree
  "hhrev_equal",        # reverse
  "redd_steal",
  "redd_drought",       # reverse

  "crbatt_pos", "crbatt_employ", "crbatt_trust", "crbatt_bribe",   #happy with CRB 
 
  "cfmgatt_pos", "cfmgatt_employ", "cfmgatt_trust", "cfmgatt_bribe",  #happy with CFMG
  
  # Descriptive variables for community service importance
  "comsvc_watsan_imp",
  "comsvc_energy_imp",
  "comsvc_road_imp",
  "comsvc_edu_imp",
  "comsvc_health_imp",
  "comsvc",
  
  # Descriptive variables for REDD+ payments
  "tree_amt_10yr", 
  "tree_amt_12mo",
  "tree_enough",    # needs reverse
  "tree_fair",      # reverse
  "frstserv_ben_1", 
  "frstserv_ben_2"
)]

# --- HH Summary stats cleaning ---
hh_df <- hh_df %>%
  mutate(across(everything(), ~ ifelse(.x %in% c(888, 999), NA, .x)))

summary(hh_df)
# looking at all variables for which NA's are > 1000, it is mainly because they have constraints
# (meaning that certain respondents are not eligible for answering those questions.)
# So no imputation are needed. We'll proceed to the next part.

# ----- HH Summary stats: Community services received -----
# 1) Generate dummies for each service (0/1)
make_dummy <- function(x, code){
  ifelse(is.na(x), NA_integer_, ifelse(grepl(paste0("\\b", code, "\\b"), x), 1L, 0L))
}

hh_df$svc_water  <- make_dummy(hh_df$comsvc, 1)
hh_df$svc_energy <- make_dummy(hh_df$comsvc, 2)
hh_df$svc_road   <- make_dummy(hh_df$comsvc, 3)
hh_df$svc_edu    <- make_dummy(hh_df$comsvc, 4)
hh_df$svc_health <- make_dummy(hh_df$comsvc, 5)
hh_df$svc_other  <- make_dummy(hh_df$comsvc, 97)

## Reverse coding for cfp_fair: 1= strongly disagree, 5=strongly agree
hh_df$cfp_fair_rev <- 6 - hh_df$cfp_fair


# --- HH Benefit Receipt Outcomes ---
# Remove villages not participating in CFP
shh_df <- hh_df %>%
  filter(cfp_aware != 0)

# --- Drop rows with missing values in key demographic variables ---
shh_df <- shh_df %>%
  filter(!is.na(cfp_aware) & !is.na(age) & !is.na(gender_respondent))

# --- Correlation between our key independent variables ---
cor(shh_df[, c(
  "cfp_satisfied",
  "cfp_fair")
], use = "complete.obs")

# --- Convert 5-point Likert scale variables to binary indicators (1 = satisfied) ---
shh_df <- shh_df %>%
  mutate(across(
    c(cfp_satisfied, cfp_fair),
    ~ifelse(. < 3, 1, 0),
    .names = "{.col}_binary"
  ))


# --- HH Forest Governance Outcomes ---
# Reverse coding to make 5=agreeing to positive outcomes of cfp
rev_hh_vars <- c("hhinform_revexp", "hhrev_equal", "redd_drought")

hh_df[paste0(rev_hh_vars, "_rev")] <- lapply(hh_df[rev_hh_vars], function(x) {
  ifelse(is.na(x), NA, 6 - x)
})

# Build composite index for "happy with CRB"
## Reverse coding to make 1= strongly disagree, 5=strongly agree
crb_vars <- c("crbatt_pos", "crbatt_employ", "crbatt_trust", "crbatt_bribe")


hh_df[paste0(crb_vars, "_rev")] <- lapply(hh_df[crb_vars], function(x) {
  ifelse(is.na(x), NA, 6 - x)
})

## Make composite index using 
rev_hh_crb <- paste0(crb_vars, "_rev")
hh_df$crb_att_index <- rowMeans(hh_df[, rev_hh_crb], na.rm = TRUE)

summary(hh_df$crb_att_index)
hist(hh_df$crb_att_index)


# Build composite index for "happy with CFMG"
## Reverse coding to make 1= strongly disagree, 5=strongly agree
cfmg_vars <- c("cfmgatt_pos", "cfmgatt_employ", "cfmgatt_trust", "cfmgatt_bribe")

hh_df[paste0(cfmg_vars, "_rev")] <- lapply(hh_df[cfmg_vars], function(x) {
  ifelse(is.na(x), NA, 6 - x)
})

## Make composite index with means of related variables
rev_hh_cfmg <- paste0(cfmg_vars, "_rev")
hh_df$cfmg_att_index <- rowMeans(hh_df[, rev_hh_cfmg], na.rm = TRUE)

summary(hh_df$cfmg_att_index)
hist(hh_df$cfmg_att_index)

############################################################
# ======== Initial cleaning for the headperson data =======#
############################################################

# Calculate the mean of the dependence on forest
head_df <- head_df %>%
  mutate(
    fhhconsume_avg = rowMeans(.[ , grep("^fhhconsume_", names(.)), drop = FALSE], na.rm = TRUE),
    fhhincome_avg  = rowMeans(.[ , grep("fhhincome_", names(.)), drop = FALSE], na.rm = TRUE),
    fdistance_avg = rowMeans(.[ , grep("^fdistance_", names(.)), drop = FALSE], na.rm = TRUE)
  )

 # --- Make a village level composite forest management variable 
 # 0 = not managed by either, 1 = managed by crb, 2 = managed by cfmg, 3 = hybrid ---
 mgmt_vars <- grep("crb_cfmg_any_", names(head_df), value = TRUE)
 mat <- as.matrix(head_df[, mgmt_vars])
 
 head_df$crb_cfmg_any <- apply(mat, 1, function(x) {
   if (all(is.na(x))) return(NA_integer_)
   if (any(x == 3, na.rm = TRUE)) return(3L)         
   if (any(x == 1, na.rm = TRUE) & any(x == 2, na.rm = TRUE)) return(3L)
   if (any(x == 1, na.rm = TRUE) & !any(x == 2, na.rm = TRUE)) return(1L)
   if (any(x == 2, na.rm = TRUE) & !any(x == 1, na.rm = TRUE)) return(2L)
   return(0L)
 })
 table(head_df$crb_cfmg_any, useNA = "ifany")
 
 # --- Make a village level composite forest monitoring system variable ---
 # 0 = No monitoring system for any forest, 1 = Some forests have a system, some don’t (mixed), 2 = All forests have monitoring systems
 monitor_vars <- grep("^fguard_[0-9]+$", names(head_df), value = TRUE)
 mat2 <- as.matrix(head_df[, monitor_vars])

head_df$fguard <- apply(mat2, 1, function(x) {
  if (all(is.na(x))) return(NA_integer_)
  if (all(x == 0, na.rm = TRUE)) return(0L)
  if (all(x == 1, na.rm = TRUE)) return(2L)
  return(1L)
})
table(head_df$fguard, useNA = "ifany")

# --- Make a village level composite forest restriction variable ---
restrict_vars <- grep("^vforrestrict_[0-9]+$", names(head_df), value = TRUE)
mat_r <- as.matrix(head_df[, restrict_vars])
# 0 = No restrictions for any forest
# 1 = Some forests have restrictions, some don’t (mixed)
# 2 = All forests are restricted
head_df$vforrestrict <- apply(mat_r, 1, function(x) {
  if (all(is.na(x))) return(NA_integer_)      
  if (all(x == 0, na.rm = TRUE)) return(0L)   
  if (all(x == 1, na.rm = TRUE)) return(2L)   
  return(1L)                                  
})

table(head_df$vforrestrict, useNA = "ifany")

# Alternative: dummy variable: 0 = no restriction, 1 = some/all restricted
head_df$vforrestrict_dummy <- apply(mat_r, 1, function(x){
  if (all(is.na(x))) return(NA_integer_)
  if (all(x == 0, na.rm = TRUE)) return(0L)
  return(1L)
})

table(head_df$vforrestrict_dummy, useNA = "ifany")

# --- Convert involved_grp into dummy variables (0/1) ---

# Step 1: Parse select_multiple string
parse_codes <- function(x) {
  # Keep NA
  if (is.na(x)) return(NA) 
  # Splitting the answers
  as.integer(unlist(strsplit(as.character(x), " ")))
}

# Step 2: Parse raw answers
codes_raw <- lapply(head_df$involved_grp, parse_codes)

# Step 3: Handle Special Codes (888/999) and Filter
codes_clean <- lapply(codes_raw, function(x) {
  # If the value is already NA, return NA
  if (length(x) == 1 && is.na(x)) return(NA)
  
  # If the respondent chose "Don't Know" or "Refused", 
  # treat the whole record as NA
  if (any(x %in% c(888, 999))) return(NA)
  
  # Otherwise, keep only the meaningful categories (0, 1, 2, 3)
  valid_codes <- x[x %in% c(0, 1, 2, 3)]
  
  # If they didn't select any of the 0-3 codes but were valid respondents,
  # return an empty integer vector (this results in all 0s in dummies)
  return(valid_codes)
})

# Step 4: Create dummy variables
head_df$inv_crb  <- sapply(codes_clean, function(x) as.integer(1 %in% x))
head_df$inv_vag  <- sapply(codes_clean, function(x) as.integer(2 %in% x))
head_df$inv_cfmg <- sapply(codes_clean, function(x) as.integer(3 %in% x))
head_df$inv_none <- sapply(codes_clean, function(x) as.integer(0 %in% x))

# Step 5: Create "overall involvement" and "number of involvement" indicators
# 1 = involved in CRB/VAG/CFMG
# 0 = not involved
head_df$inv_any <- as.integer(
  head_df$inv_crb + head_df$inv_vag + head_df$inv_cfmg > 0
)

# Sum up the number of forest management orgs the headperson is involved in
head_df$inv_num <- head_df$inv_crb + head_df$inv_vag + head_df$inv_cfmg

# Step 5: Quick summary to check results
# Shows how many respondents selected each option
summary_table <- data.frame(
  inv_crb = sum(head_df$inv_crb, na.rm = TRUE),
  inv_vag = sum(head_df$inv_vag, na.rm = TRUE),
  inv_cfmg = sum(head_df$inv_cfmg, na.rm = TRUE),
  inv_none = sum(head_df$inv_none, na.rm = TRUE),
  inv_any = sum(head_df$inv_any, na.rm = TRUE)
)

print(summary_table)


# --- Decision-maker for forest use and management --- 
# We average scores forest mangement and decision-making power scores for the authority variables
authority_vars <- c("hfladder_headman", "hfladder_chief",
                    "hfladder_crb", "hfladder_dnpw")

head_df$authority_index <- rowMeans(head_df[, authority_vars], na.rm = TRUE)

# --- Making the problem index ---
dev_vars <- c(
  "hproblem_job",
  "hproblem_school",
  "hproblem_road",
  "hproblem_heath",
  "hproblem_electric",
  "hproblem_food",
  "hproblem_water"
)

# Checking if we should use mean or PCA
cor(head_df[, dev_vars], use = "pairwise.complete.obs")
alpha(head_df[, dev_vars])

# All correlations are under 0.5, and Cronbach's alpha = 0.7
# Use mean value as the index is easier to interpret, and PCA would not significantly improve the index
head_df$problem_dev_index <- rowMeans(
  head_df[, dev_vars],
  na.rm = TRUE
)

# Looking at gender and education distribution to decide 
# whether to include them as control variables or not
table(head_df$hgender)

table(head_df$hgender, head_df$heduc)
head_df$edu_bin <- ifelse(head_df$heduc == 1, "No education", "Has education")
table(head_df$hgender, head_df$edu_bin)



# ======== VILLAGE COVARIATEs & OUTCOMES ========#

village_df <- head_df[, c(
  "hgender",
  "heduc",
  "hvsize",
  "htarmac",
  "fdistance_avg",
  "fhhconsume_avg",
  "fhhincome_avg",
  "crb_cfmg_any",
  
  "fguard",             
  "vforrestrict",
  "vforrestrict_dummy",
  
  "helite_encroach",
  "hchief_encroach",
  "hinvestor_encroach",
  "hgovt_enroach",
  
  "inv_num", # governance org involvement summed

  "authority_index",
  # unless using individually: "authority_vars",

  # how much power does the authority have on forest decision making power (1-10)
  "problem_dev_index",
  
  # CFP Benefit Outcomes
  "any_ben",
  "cfp_aware",
  "cfp_access",  # should be careful with this - 1 = lost forest access because of cfp
  "cfp_ben",
  "cfp_satisfied",
  "cfp_fair",
  "cfp_enough",
  "cfp_important",
  
  # Forest Governance Outcomes
  "hhinform_revexp", # need to reverse
  "redd_steal",
  "redd_drought",  # need to reverse
  
  # Happy with CRB
  "crbatt_pos", 
  "crbatt_employ", 
  "crbatt_trust", 
  "crbatt_bribe",
  
  "complain_yn"
)]

# --- Cleaning: 888 (don't know) & 999(prefer not to say) are considered missing ---
village_df <- village_df %>%
  mutate(across(everything(), ~ ifelse(.x %in% c(888, 999), NA, .x)))

## Make dummy for benefits received from CFP: Generate dummies for each benefit (0/1)
make_dummy <- function(x, code){
  ifelse(is.na(x), NA_integer_, ifelse(grepl(paste0("\\b", code, "\\b"), x), 1L, 0L))
}

village_df$cfp_none  <- make_dummy(village_df$cfp_ben, 0)
village_df$cfp_beekeeping  <- make_dummy(village_df$cfp_ben, 1)
village_df$cfp_bcocharcoal  <- make_dummy(village_df$cfp_ben, 2)
village_df$cfp_farm  <- make_dummy(village_df$cfp_ben, 3)
village_df$cfp_otheralt  <- make_dummy(village_df$cfp_ben, 4)
village_df$cfp_water  <- make_dummy(village_df$cfp_ben, 5)
village_df$cfp_energy  <- make_dummy(village_df$cfp_ben, 6)
village_df$cfp_road  <- make_dummy(village_df$cfp_ben, 7)
village_df$cfp_edu  <- make_dummy(village_df$cfp_ben, 8)
village_df$cfp_health  <- make_dummy(village_df$cfp_ben, 9)
village_df$cfp_hhcash  <- make_dummy(village_df$cfp_ben, 10)
village_df$cfp_vilcash  <- make_dummy(village_df$cfp_ben, 11)
village_df$cfp_other  <- make_dummy(village_df$cfp_ben, 97)

# --- HH Benefit Receipt Outcomes ---
# Remove villages not participating in CFP
svil_df <- village_df %>%
  filter(cfp_aware != 0  & !is.na(cfp_aware))

# --- Correlation between our key independent variables ---
cor(svil_df[, c(
  "cfp_satisfied",
  "cfp_fair")
], use = "complete.obs")

# --- Convert 5-point Likert scale variables to binary indicators (1 = satisfied) ---
svil_df <- svil_df %>%
  mutate(across(
    c(cfp_satisfied, cfp_fair),
    ~ifelse(. < 3, 1, 0),
    .names = "{.col}_binary"
  ))

# --- HP Forest Governance Outcomes ---
# Reverse coding to make 5=agreeing to positive outcomes of cfp
rev_hp_vars <- c("hhinform_revexp", "redd_drought")

village_df[paste0(rev_hp_vars, "_rev")] <- lapply(village_df[rev_hp_vars], function(x) {
  ifelse(is.na(x), NA, 6 - x)
})

# Build composite index for "happy with CRB"
## Reverse coding to make 1= strongly disagree, 5=strongly agree
crb_vars <- c("crbatt_pos", "crbatt_employ", "crbatt_trust", "crbatt_bribe")

village_df[paste0(crb_vars, "_rev")] <- lapply(village_df[crb_vars], function(x) {
  ifelse(is.na(x), NA, 6 - x)
})

## Make composite index with means of related variables
rev_hp_crb <- paste0(crb_vars, "_rev")
village_df$crb_att_index <- rowMeans(village_df[, rev_hp_crb], na.rm = TRUE)

summary(village_df$crb_att_index)
hist(village_df$crb_att_index)

# Reverse coding to make 5 = positive sentiments towards cfp
rev_vilcfp_vars <- c("cfp_satisfied", "cfp_fair", "cfp_enough")

village_df[paste0(rev_vilcfp_vars, "_rev")] <- lapply(village_df[rev_vilcfp_vars], function(x) {
  ifelse(is.na(x), NA, 6 - x)
})


# ------ Descriptive Analysis (HH) ------
## Summary statistics of perception on the importance of services provided
# Vector of services of our interest
imp_vars <- c(
  "comsvc_watsan_imp",
  "comsvc_energy_imp",
  "comsvc_road_imp",
  "comsvc_edu_imp",
  "comsvc_health_imp"
)

# Summarize the counts and percentages of the importance ratings
# Each row stands for a gender: 0 = male, 1 = female; 
# Each column stands for an importance rating: 1 = not at all important, 5 = very important
for(v in imp_vars){
  cat("\n\n====", v, "(ONLY respondents with this service) ====\n")
  
  df_sub <- hh_df %>%
    filter(!is.na(.data[[v]]))
  
  tab <- table(df_sub$gender_respondent, df_sub[[v]])
  print(tab)
  print(round(100 * prop.table(tab, 1), 1))
}

# Median IQR
median_iqr <- function(x){
  xn <- suppressWarnings(as.numeric(as.character(x)))  # factor->character->numeric; numeric is also OK
  c(median = median(xn, na.rm=TRUE),
    IQR = IQR(xn, na.rm=TRUE),
    n = sum(!is.na(xn)))
}
t(sapply(hh_df[imp_vars], function(x) median_iqr(x)))


## Summary statistics for cash benefits received from REDD+
summary(hh_df$tree_amt_10yr)
summary(hh_df$tree_amt_12mo)

# Define a summary table template
cash_summary <- function(x){
  c(
    n = sum(!is.na(x)),
    mean = mean(x, na.rm=TRUE),
    sd = sd(x, na.rm=TRUE),
    median = median(x, na.rm=TRUE),
    IQR = IQR(x, na.rm=TRUE),
    min = min(x, na.rm=TRUE),
    max = max(x, na.rm=TRUE)
  )
}

# Look at cash benefit received for the past 10 yrs and 12 months
cash_summary(hh_df$tree_amt_10yr)
cash_summary(hh_df$tree_amt_12mo)

# A lot of people did not receive cash benefits in the 12 months before the survey
# So we look at the statistics for those who did receive cash benefits in the past 12 months
mean(hh_df$tree_amt_12mo > 0, na.rm=TRUE)
cash_summary(hh_df$tree_amt_12mo[hh_df$tree_amt_12mo > 0])

## Perception on cash benefit adequacy and fairness
# Vector of services of our interest
cash_vars <- c(
  "tree_enough",
  "tree_fair"
)

# Summarize the counts and percentages of the importance ratings
# Each row stands for a gender: 0 = male, 1 = female; 
# Each column stands for an importance rating: 1 = very bad, 5 = very good
for(v in cash_vars){
  cat("\n\n====", v, "(ONLY respondents with cash beneits) ====\n")
  
  df_subs <- hh_df %>%
    filter(!is.na(.data[[v]]))
  
  tab <- table(df_subs$gender_respondent, df_subs[[v]])
  print(tab)
  print(round(100 * prop.table(tab, 1), 1))
}

# Median IQR
median_iqr <- function(x){
  xn <- suppressWarnings(as.numeric(as.character(x))) 
  c(median = median(xn, na.rm=TRUE),
    IQR = IQR(xn, na.rm=TRUE),
    n = sum(!is.na(xn)))
}
t(sapply(hh_df[cash_vars], function(x) median_iqr(x)))

## Among those who participate in CFP, what would they receive?
table(shh_df$frstserv_ben_1)
table(shh_df$frstserv_ben_2)

## Summary: Community service received
# Count the times services were mentioned
colSums(hh_df[, c("svc_water","svc_energy","svc_road","svc_edu","svc_health","svc_other")] == 1,
        na.rm = TRUE)

# Count number of services chose on the same row
hh_df$svc_n_selected <- rowSums(hh_df[, c("svc_water","svc_energy","svc_road","svc_edu","svc_health","svc_other")] == 1,
                                na.rm = FALSE)

table(hh_df$svc_n_selected, useNA = "ifany")

## Summary: Perception of CFP fairness
# Count and percentage by gender
tab_g <- table(hh_df$gender_respondent, hh_df$cfp_fair_rev)
tab_g
round(100 * prop.table(tab_g, 1), 1)   # percentage within each gender

# Median and IQR
c(
  n = sum(!is.na(hh_df$cfp_fair_rev)),
  median = median(hh_df$cfp_fair_rev, na.rm = TRUE),
  IQR = IQR(hh_df$cfp_fair_rev, na.rm = TRUE)
)

# ------ Descriptive Analysis (Village) ------
## Ever filed a complaint about a carbon/REDD+ project?
table(village_df$complain_yn)

## Has your community received any benefits or services over the past 10 years from the CRB, VAG, or CFMG?
table(village_df$any_ben)

## Participated in CFP or not?
table(village_df$cfp_aware)

## Lose forest access because of CFP?
table(village_df$cfp_access)

## CFP benefit receipt
cfp_vars <- c(
  "cfp_beekeeping",
  "cfp_bcocharcoal",
  "cfp_farm",
  "cfp_otheralt",
  "cfp_water",
  "cfp_energy",
  "cfp_road",
  "cfp_edu",
  "cfp_health",
  "cfp_hhcash",
  "cfp_vilcash",
  "cfp_other"
)
# CFP coverage
cfp_coverage <- colMeans(village_df[, cfp_vars] == 1, na.rm = TRUE)

# number and percentage of villages that received each benefit
colSums(village_df[, cfp_vars] == 1, na.rm = TRUE)
colMeans(village_df[, cfp_vars] == 1, na.rm = TRUE)

# Number of CFP benefits received by village & distribution
village_df$cfp_n_benefits <- rowSums(
  village_df[, cfp_vars] == 1,
  na.rm = FALSE
)
hist(village_df$cfp_n_benefits)

## Perception on village-level cfp benefit adequacy and fairness
# Vector of services of our interest
cfp_perception_vars <- c(
  "cfp_satisfied_rev",
  "cfp_fair_rev",
  "cfp_enough_rev",
  "cfp_important"
)

# Summarize the counts and percentages of the importance ratings
# Each row stands for a gender: 0 = male, 1 = female; 
# Each column stands for a perception rating: 1 = very bad, 5 = very good
for(v in cfp_perception_vars){
  cat("\n\n====", v, "(ONLY respondents with cfp beneits) ====\n")
  
  df_vil_subs <- village_df %>%
    filter(!is.na(.data[[v]]))
  
  tab <- table(df_vil_subs$hgender, df_vil_subs[[v]])
  print(tab)
  print(round(100 * prop.table(tab, 1), 1))
}

# Median IQR
median_iqr <- function(x){
  xn <- suppressWarnings(as.numeric(as.character(x))) 
  c(median = median(xn, na.rm=TRUE),
    IQR = IQR(xn, na.rm=TRUE),
    n = sum(!is.na(xn)))
}
t(sapply(village_df[cfp_perception_vars], function(x) median_iqr(x)))


