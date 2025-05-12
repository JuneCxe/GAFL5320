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

# --- Load Data from Stata .dta file ---
setwd("~/University/Study_Abroad/UPenn/2025SS/GAFL5320_ID/WorkInProgress")
df <- read_dta("CFP Household Women FINAL Combined no pii_wt.dta")

# Making the education variable more interpretable by grouping it into 3 levels
# Group ed into three levels：Low / Middle / High
df <- df %>%
  mutate(
    ed_group = case_when(
      ed >= 1 & ed <= 7  ~ "Low",              # Primary incomplete or lower
      ed >= 8 & ed <= 11 ~ "Middle",           # Completed primary or lower secondary
      ed >= 12 & ed <= 15 ~ "High",            # Senior secondary and above
      TRUE ~ NA_character_                    # Catch missing, 888, 999, etc.
    ),
    ed_group = factor(ed_group, levels = c("Low", "Middle", "High"))
  )


# --- Generating the asset PCA variable ---
# Find asset variables
asset_vars <- read_lines("asset.txt")

# Build a df for calculation of our PCA index
asset_df <- df |> 
  
  # Find just the variables whose names equal (or begin with) the values
  #  of this character vector
  dplyr::select(starts_with(asset_vars)) |> 
  
  # Impute values for all NAs so we can calculate PCA
  mutate(
    # Impute 0s for quantity cols where binary col equals 0
    across(
      !ends_with("_yn"),
      \(x) if_else(
        pick(all_of(glue(cur_column(), "_yn"))) == 0,
        true = 0, false = x
      )
    ),
    
    # Impute column means where NA
    across(
      everything(),
      \(x) if_else(is.na(x), true = mean(x, na.rm=T), false = x)
    )
  )

# Center & scale the data, then produce a PCA of the 'asset' df
asset_pca <- prcomp(asset_df, center=T, scale.=T)

# Center & scale raw data
asset_scaled <- scale(
  as.matrix(asset_df),
  scale = asset_pca$scale,
  center = asset_pca$center
)

# Calculate durable asset index
asset_scores <- asset_scaled %*% asset_pca$rotation[, "PC1"]

# The scores are flipped, meaning households with more assets have scores that
#  are more negative. We will fix this by multiplying the scores by -1 when we
#  add this new variable back to our df using mutate()

# Apply index scores to data (flipped so more wealth = higher score)
df <- df |> 
  mutate(
    asset_pca = -1 * as.double(asset_scores),
    
    # Calculating a logarithmic version, because wealth is lognormal distributed
    # - Because of this, this variable is probably a better covariate.
    log_asset_pca = log(1 + asset_pca - min(asset_pca))
  )

summary(df$log_asset_pca)
hist(df$log_asset_pca, breaks = 30, main = "Distribution of Log Asset Index")


# --- Create a Majority Tribe Indicator by Chiefdom ---

# Step 1: Find the most common tribe within each chiefdom
majority_table <- df %>%
  group_by(chief, tribe) %>%
  summarise(n = n(), .groups = 'drop') %>%
  group_by(chief) %>%
  slice_max(order_by = n, n = 1, with_ties = FALSE) %>%
  rename(majority_tribe = tribe)

# Step 2: Merge majority tribe info back into main dataset
df <- df %>%
  left_join(majority_table, by = "chief")

# Step 3: Create binary variable: 1 = in majority tribe, 0 = minority
df <- df %>%
  mutate(majority_status = ifelse(tribe == majority_tribe, 1, 0))

#calculate the mean of the dependence on forest
df <- df %>%
  mutate(
    hfconsume_avg = rowMeans(.[ , grep("^hfconsume_", names(.)), drop = FALSE], na.rm = TRUE),
    hfincome_avg  = rowMeans(.[ , grep("^hfincome_", names(.)), drop = FALSE], na.rm = TRUE),
    forminutes_avg = rowMeans(.[ , grep("^forminutes_", names(.)), drop = FALSE], na.rm = TRUE)
  )

# rename gender information
df <- df %>%
  rename(gender = gender_respondent)

# --- Create leader perception index (PCA on complete cases only) ---
# Define variables
leader_vars <- c(
  "fleader_trust",
  "fleader_protect",
  "fleader_fair",
  "fleader_work",
  "fleader_bribes",
  "fleader_transparent"
)

# Extract and standardize only complete rows
leader_complete_idx <- complete.cases(df[, leader_vars])
leader_scaled <- scale(df[leader_complete_idx, leader_vars])

# Run PCA
leader_pca <- prcomp(leader_scaled, center = TRUE, scale. = TRUE)
leader_index <- as.numeric(scale(leader_pca$x[, 1]))  # use PC1

# Create full-length vector, NA where incomplete
df$leader_index <- NA
df$leader_index[leader_complete_idx] <- leader_index


# --- Create climate concern index (PCA on complete cases only) ---
climate_vars <- c(
  "problem_temp",
  "problem_rain",
  "temp_important",
  grep("^villchange_10yr_", names(df), value = TRUE)
)
# Count the observed climate changes
villchange_vars <- grep("^villchange_10yr_[1-7]|97$", names(df), value = TRUE)
# Create a variable to summarize the count of observed climate changes
df$climate_observation_count <- rowSums(df[, villchange_vars], na.rm = TRUE)

climate_vars_final <- c(
  "problem_temp",        # Specific concern - temperature
  "problem_rain",        # Specific concern - rainfall
  "temp_important",      # Belief in the importance of protection
  "climate_observation_count"  # Count of observed climate changes
)

climate_complete_idx <- complete.cases(df[, climate_vars_final])
climate_scaled <- scale(df[climate_complete_idx, climate_vars_final])

climate_pca <- prcomp(climate_scaled, center = TRUE, scale. = TRUE)
climate_index <- as.numeric(scale(climate_pca$x[, 1]))

df$climate_index <- NA
df$climate_index[climate_complete_idx] <- climate_index

# Function to summarize both numeric and categorical variables
summarize_data <- function(df) {
  
  # --- Numeric Variable Summary ---
  # Select only numeric columns from the dataset
  numeric_vars <- df[, sapply(df, is.numeric)]
  
  # Compute summary statistics: N, Mean, SD, Min, Median, Max
  numeric_summary <- data.frame(
    Variable = names(numeric_vars),
    N = sapply(numeric_vars, function(x) sum(!is.na(x))),
    Mean = sapply(numeric_vars, function(x) mean(x, na.rm = TRUE)),
    SD = sapply(numeric_vars, function(x) sd(x, na.rm = TRUE)),
    Min = sapply(numeric_vars, function(x) min(x, na.rm = TRUE)),
    Median = sapply(numeric_vars, function(x) median(x, na.rm = TRUE)),
    Max = sapply(numeric_vars, function(x) max(x, na.rm = TRUE))
  )
  
  # --- Categorical Variable Summary ---
  # Select only factor or character variables
  cat_vars <- df[, sapply(df, function(x) is.factor(x) || is.character(x))]
  
  # Loop over each categorical variable and generate frequency tables
  categorical_summary <- lapply(names(cat_vars), function(var) {
    tab <- tabyl(cat_vars[[var]]) %>%
      adorn_pct_formatting(digits = 1) %>%
      mutate(Variable = var)
    return(tab)
  }) %>% bind_rows()
  
  # Return both numeric and categorical summary tables as a list
  list(
    numeric_summary = numeric_summary,
    categorical_summary = categorical_summary
  )
}

################################################################################
#                                RQ1 Analysis                                  #
################################################################################

# --- Select relevant variables for Question 1 analysis ---

# Select columns
Q1household <- df[, c(
  "gender", 
  "cfp_aware", 
  "cfp_important", 
  "cfp_satisfied",
  "cfp_enough", 
  "cfp_fair", 
  "age", 
  "ed", 
  "ed_group",
  "log_asset_pca",
  "hfconsume_avg",
  "hfincome_avg", 
  "forminutes_avg",
  "majority_status",
  "married",
  "leader_index",
  "hh_size",
  "climate_index",
  "hhinform_revexp",
  "inc_annual"
)]

# --- Remove those unaware of CFP ---
Q1household <- Q1household %>%
  filter(cfp_aware != 0)

# --- Drop rows with missing values in key demographic variables ---
Q1household <- Q1household %>%
  filter(!is.na(gender) & !is.na(cfp_aware) & !is.na(age) & !is.na(ed))

# --- Correlation between our key independent variables ---
cor(Q1household[, c(
  "cfp_satisfied",
  "cfp_important",
  "cfp_enough",
  "cfp_fair")
], use = "complete.obs")

# --- Convert 5-point Likert scale variables to binary indicators (1 = >3) ---
Q1household <- Q1household %>%
  mutate(across(
    c(cfp_important, cfp_satisfied, cfp_enough),
    ~ifelse(. > 3, 1, 0),
    .names = "{.col}_binary"
  ))

# summary stats
Q1results <- summarize_data(Q1household)

# View or export the summary tables
View(Q1results$numeric_summary)
View(Q1results$categorical_summary)


# Print clean summary in console
print(summary_stats, row.names = FALSE, digits = 2)

# --- Visualize distribution of CFP perception variables ---
ggplot(Q1household, aes(x = cfp_important)) +
  geom_histogram(binwidth = 1, fill = "blue", alpha = 0.3) +
  ggtitle("Distribution of CFP Importance")

ggplot(Q1household, aes(x = cfp_satisfied)) +
  geom_histogram(binwidth = 1, fill = "red", alpha = 0.3) +
  ggtitle("Distribution of CFP Satisfaction")

ggplot(Q1household, aes(x = cfp_enough)) +
  geom_histogram(binwidth = 1, fill = "yellow", alpha = 0.3) +
  ggtitle("Distribution of CFP Adequacy")


# --- Grouped descriptive stats by gender (mean values) ---
Q1household %>%
  group_by(gender) %>%
  summarise(across(
    c(cfp_important, cfp_satisfied, cfp_enough),
    ~mean(., na.rm = TRUE)
  ))

# Group by gender and compute mean values for the binary outcome variables
Q1household %>%
  group_by(gender) %>%
  summarise(
    n = n(),
    cfp_important = mean(cfp_important_binary, na.rm = TRUE),
    cfp_satisfied = mean(cfp_satisfied_binary, na.rm = TRUE),
    cfp_enough = mean(cfp_enough_binary, na.rm = TRUE)
  )

# --- Look at correlations between variables ---
cor(Q1household[, c(
  "gender", 
  "hh_size",
  "climate_index",
  "cfp_important_binary",
  "cfp_satisfied_binary",
  "cfp_enough_binary",
  "age", 
  "ed", 
  "log_asset_pca",
  "majority_status", 
  "married",
  "hfconsume_avg",
  "hfincome_avg",
  "forminutes_avg",
  "hhinform_revexp",
  "leader_index")], use = "complete.obs")


# --- Logistic regression models for each CFP outcome ---
lm1 <- lm(
  cfp_important_binary ~ gender + age + ed_group + log_asset_pca + climate_index +
    hfconsume_avg + hfincome_avg + forminutes_avg + majority_status + married +
    leader_index + hhinform_revexp,
  data = Q1household
)

stargazer(lm1, type = "text")

lm2 <- lm(
  cfp_satisfied_binary ~ gender + age + ed_group + log_asset_pca + climate_index +
    hfconsume_avg + hfincome_avg + forminutes_avg + majority_status + married +
    leader_index + hhinform_revexp,
  data = Q1household
)

lm3 <- lm(
  cfp_enough_binary ~ gender + age + ed_group + log_asset_pca + climate_index +
    hfconsume_avg + hfincome_avg + forminutes_avg + majority_status + married +
    leader_index  + hhinform_revexp,
  data = Q1household
)

# --- Display results using stargazer ---
stargazer(lm1, lm2, lm3, type = "html", out="Q1_cfp.html")

################################################################################
#                     RQ2 Analysis (Climate resilience)                        #
################################################################################
Q2household <- df[, c(
  "cfp_aware", 
  "gender",
  "drought",
  "drought_respond",
  grep("^drought_adapt_", names(df), value = TRUE),
  grep("^drought_cope_", names(df), value = TRUE),
  "drought_impactecon",
  "age",
  "ed",
  "ed_group",
  "log_asset_pca",
  "married",
  "hfconsume_avg",
  "hfincome_avg", 
  "forminutes_avg",
  "majority_status",
  "leader_index",
  "climate_index")]

# --- Remove those not participated in CFP ---
Q2household <- Q2household %>%
  filter(cfp_aware != 0)

# --- Drop rows with missing values in key demographic variables ---
Q2household <- Q2household %>%
  filter(!is.na(gender) & !is.na(age) & !is.na(ed))

# --- Convert 5-point Likert scale variables to binary indicators (1 = >3) ---
Q2household <- Q2household %>%
  mutate(across(
    c(drought_respond, drought_impactecon),
    ~ifelse(. > 3, 1, 0),
    .names = "{.col}_binary"
  ))


# --- Logistic regression models for each CFP outcome ---
lm5 <- lm(
  drought_respond_binary ~ gender + age + ed_group + log_asset_pca +
    hfconsume_avg + hfincome_avg + forminutes_avg + majority_status + married +
    leader_index + climate_index,
  data = Q2household
)


lm6 <- lm(
  drought_impactecon_binary ~ gender + age + ed_group + log_asset_pca +
    hfconsume_avg + hfincome_avg + forminutes_avg + majority_status + married +
    leader_index + climate_index,
  data = Q2household
)

car::vif(lm5)
car::vif(lm6)


# --- Create a count of valid adaptation strategies used ---
# Step 1: Get all adaptation variable names
adapt_cols_all <- names(Q2household)[grepl("^drought_adapt_", names(Q2household))]

# Step 2: Remove "other"/invalid categories like _97, _777, _888, _oth
valid_adapt_cols <- adapt_cols_all[!grepl("_(97|777|888|oth)$", adapt_cols_all)]

# Step 3: Create a new variable counting the number of valid strategies used
Q2household$adapt_count <- rowSums(Q2household[ , valid_adapt_cols], na.rm = TRUE)
hist(Q2household$adapt_count)
lm_model_adapt <- lm(adapt_count  ~ gender + age + ed_group + log_asset_pca +
                           hfconsume_avg + hfincome_avg + forminutes_avg + majority_status + married +
                           leader_index + climate_index, data = Q2household)
summary(lm_model_adapt)

# --- Create a count of valid coping strategies used ---
cope_cols <- grep("^drought_cope_\\d+$", names(Q2household), value = TRUE)  # select only numbered ones
cope_cols <- cope_cols[!grepl("_(888|999)$", cope_cols)]  # remove unwanted suffixes

# Compute the row sums
Q2household$cope_count <- rowSums(Q2household[, cope_cols], na.rm = TRUE)
hist(Q2household$cope_count)
# Count Model for Coping Strategies Used
lm_model_cope <- lm(cope_count  ~ gender + age + ed_group + log_asset_pca +
                          hfconsume_avg + hfincome_avg + forminutes_avg + majority_status + married +
                          leader_index + climate_index, 
                        data = Q2household)

summary(lm_model_cope)

# make adapt count and cope count a binary variable, 1 = used the strategy, 0= didn't use the strategy
Q2household$used_any_adapt <- ifelse(Q2household$adapt_count > 0, 1, 0)
Q2household$used_any_cope <- ifelse(Q2household$cope_count > 0, 1, 0)

lm_adapt <- lm(used_any_adapt ~ gender + age + ed_group + log_asset_pca +
                          hfconsume_avg + hfincome_avg + forminutes_avg + majority_status + married +
                          leader_index + climate_index,
                        data = Q2household)

lm_cope <- lm(used_any_cope ~ gender + age + ed_group + log_asset_pca +
                   hfconsume_avg + hfincome_avg + forminutes_avg + majority_status + married +
                   leader_index + climate_index,
                 data = Q2household)

# Define high-cost drought adaptation variables
adapt_high_cost_vars <- Q2household[, c(
  "drought_adapt_1", "drought_adapt_2", "drought_adapt_3",
  "drought_adapt_15", "drought_adapt_17", "drought_adapt_18"
)]

# Define high-effort drought adaptation variables
adapt_high_effort_vars <- Q2household[, c(
  "drought_adapt_1", "drought_adapt_2", "drought_adapt_3", "drought_adapt_4",
  "drought_adapt_5", "drought_adapt_6", "drought_adapt_7", "drought_adapt_8",
  "drought_adapt_9", "drought_adapt_11", "drought_adapt_13",
  "drought_adapt_22", "drought_adapt_23"
)]

# Add binary indicator for high-cost adaptation
Q2household$adapt_high_cost <- as.integer(
  rowSums(adapt_high_cost_vars, na.rm = TRUE) > 0
)

# Add binary indicator for high-effort adaptation
Q2household$adapt_high_effort <- as.integer(
  rowSums(adapt_high_effort_vars, na.rm = TRUE) > 0
)

# Fit the groups into a logistic regression
lm7 <- lm(adapt_high_cost ~ gender + age + ed_group + log_asset_pca +
    hfconsume_avg + hfincome_avg + forminutes_avg + majority_status + married +
    leader_index + climate_index,
  data = Q2household
)


lm8 <- lm(adapt_high_effort ~ gender + age + ed_group + log_asset_pca +
              hfconsume_avg + hfincome_avg + forminutes_avg + majority_status + married +
              leader_index + climate_index,
            data = Q2household
)


# High-cost drought coping strategies
cope_high_cost_vars <- c(
  "drought_cope_4",  # Spend cash savings
  "drought_cope_5",  # Sell assets
  "drought_cope_9",  # Get loan
  "drought_cope_13", # Small business
  "drought_cope_16"  # Received food (likely logistical cost)
)

# High-effort drought coping strategies
cope_high_effort_vars <- c(
  "drought_cope_1",  # Harvest more forest products
  "drought_cope_2",  # Harvest more wild foods
  "drought_cope_3",  # Harvest more ag. products
  "drought_cope_6",  # Extra casual labor
  "drought_cope_13", # Small business
  "drought_cope_15", # Change planting/harvest
  "drought_cope_19"  # Gardening
)

# Create binary indicator for high-cost coping
Q2household$cope_high_cost <- as.integer(
  rowSums(Q2household[, cope_high_cost_vars], na.rm = TRUE) > 0
)

# Create binary indicator for high-effort coping
Q2household$cope_high_effort <- as.integer(
  rowSums(Q2household[, cope_high_effort_vars], na.rm = TRUE) > 0
)

# Fit the groups into a logistic regression
lm9 <- lm(cope_high_cost ~ gender + age + ed_group + log_asset_pca +
              hfconsume_avg + hfincome_avg + forminutes_avg + majority_status + married +
              leader_index + climate_index,
            data = Q2household
)


lm10 <- lm(cope_high_effort ~ gender + age + ed_group + log_asset_pca +
              hfconsume_avg + hfincome_avg + forminutes_avg + majority_status + married +
              leader_index + climate_index,
            data = Q2household
)


# summary
Q2summary_vars <- Q2household[ , c(
  # Binary outcomes
  "drought_respond_binary", "drought_impactecon_binary",
  "used_any_adapt", "used_any_cope",
  "adapt_high_cost", "adapt_high_effort",
  "cope_high_cost", "cope_high_effort",
  "adapt_count", "cope_count", "ed",
  "gender", "age", "married", "majority_status",
  "ed_group", "log_asset_pca", 
  "hfconsume_avg", "hfincome_avg", "forminutes_avg",
  "leader_index", "climate_index"
)]


Q2summary_stat <- summarize_data(Q2summary_vars)

# View summary tables
View(Q2summary_stat$numeric_summary)
View(Q2summary_stat$categorical_summary)

# summarize by gender
Q2household %>%
  group_by(gender) %>%
  summarise(
    n = n(),
    responded_to_drought = mean(drought_respond_binary, na.rm = TRUE),
    econ_impact_from_drought = mean(drought_impactecon_binary, na.rm = TRUE),
    adapt = mean(used_any_adapt, na.rm = TRUE),
    cope = mean(used_any_cope, na.rm = TRUE),
    high_cost_adapt = mean(adapt_high_cost, na.rm = TRUE),
    high_effort_adapt = mean(adapt_high_effort, na.rm = TRUE),
    high_cost_cope = mean(cope_high_cost, na.rm = TRUE),
    high_effort_cope = mean(cope_high_effort, na.rm = TRUE)
  )


# --- Display results using stargazer ---
stargazer(lm5, lm6, type = "html", out="Q2_droughbi.html")
stargazer(lm_model_adapt, lm_model_cope, type = "html", out="Q2_strategy_count.html")
stargazer(lm7, lm8, lm9, lm10, type = "html", out="Q2_cost_effort.html")
stargazer(lm5, lm6, lm_adapt, lm_cope, type = "html", out="Q2_bi_total.html")

################################################################################
#             RQ3: Factors that explain receipt of CFP benefits                #
################################################################################

# look at the correlation between annual income and asset PCA to decide whether to include annual income or not
cor(df$asset_pca, df$inc_annual, use = "complete.obs")

Q3household <- df[, c(
  "gender",
  "inc_annual",
  "cfp_aware",
  grep("^cfp_ben_", names(df), value = TRUE),
  "age",
  "ed",
  "ed_group",
  "log_asset_pca",
    "married",
  "hfconsume_avg",
  "hfincome_avg", 
  "forminutes_avg",
  "majority_status",
  "leader_index",
  "climate_index",
  "forest_num",
  "togetherness",        # U.4 On a scale of 1 to 5, with 1 meaning very distant and 5 meaning very close, how strong is the feeling of togetherness in your village?
  "forest_participate",
  "attend_event",
  "attend_benefit",      # U.6 In the past 12 months, how often have people in this village gotten together to jointly ask local authorities/leaders for something to benefit the community?
  "trust_crb",
  "trust_neigh",
  "trust_cust",
  "trust_dnpw",
  "chief_encroach",
  "investor_encroach",
  grep("^forest_access", names(df), value = TRUE)
)]


cor(df[, c("attend_benefit", "forest_participate", "togetherness")], use = "complete.obs")
# filter the data so that only those who are eligible for benefits are inspected
Q3household <- Q3household %>% filter(cfp_aware == 1)

# Make a new column that indicates the cfp benefit reception: 
# 1 = received any CFP benefit; 0 = did not receive any benefit
Q3household <- Q3household %>%
  mutate(
    cfp_benefit_received = ifelse(cfp_ben_0 == 0 & !is.na(cfp_ben_0), 1, 0)
  )

# average the forest_access scores
Q3household <- Q3household %>%
  mutate(
    forest_access_avg = rowMeans(.[ , grep("^forest_access_", names(.)), drop = FALSE], na.rm = TRUE))

# forest participation originally was 1=always, 5=never, 888 and 999 means don't know or prefer not to say. 
# We change it into a more interpretable format by excluding 888 and 999 and making the bigger number meaning a higher frequency
Q3household <- Q3household %>%
  mutate(forest_participate_clean = ifelse(forest_participate %in% c(888, 999), NA, forest_participate))
Q3household <- Q3household %>%
  mutate(forest_participate_rev = ifelse(is.na(forest_participate_clean), NA, 6 - forest_participate_clean))

# doing the same as above to attend_benefit
Q3household <- Q3household %>%
  mutate(attend_benefit_clean = ifelse(attend_benefit %in% c(888, 999), NA, attend_benefit))
Q3household <- Q3household %>%
  mutate(attend_benefit_rev = ifelse(is.na(attend_benefit_clean), NA, 6 - attend_benefit_clean))

# summary
Q3summary_vars <- Q3household[ , c(
  "cfp_benefit_received",
  "gender", "age", "ed", "ed_group", "log_asset_pca",
  "hfconsume_avg", "hfincome_avg", "forminutes_avg",
  "majority_status", "married",
  "leader_index", "climate_index",
  "forest_num", "forest_access_avg",
  "togetherness", "attend_benefit_rev", "forest_participate_rev",
  "trust_neigh", "trust_crb", "trust_cust", "trust_dnpw"
)]


Q3results <- summarize_data(Q3summary_vars)

# View or export the summary tables
View(Q3results$numeric_summary)
View(Q3results$categorical_summary)

# build the model
lm_a <- lm(cfp_benefit_received ~ gender + age + ed_group + log_asset_pca +
               hfconsume_avg + hfincome_avg + forminutes_avg + majority_status + married +
               leader_index + climate_index + forest_num + forest_access_avg +
               togetherness + attend_benefit_rev + trust_neigh +
               forest_participate_rev + trust_crb + trust_cust + trust_dnpw,
             data = Q3household)

stargazer(lm_a, type = "html", out="Q3_cfp_receipt.html")


################################################################################
#                 RQ4: Factors that explain support for REDD+                  #
################################################################################

Q4household <- df[, c(
  "gender",
  "age",
  "ed",
  "ed_group",
  "married",
  "majority_status",
  "log_asset_pca",
  
  "climate_index",
  
  "cfp_aware", 
  "cfp_important", 
  "cfp_satisfied",
  "cfp_enough", 
  "cfp_fair", 
  "any_ben",
  "cfp_ben_0",
  "cfp_access",

  "forest_leaders",
  "forest_satisfied",
  "forest_meeting",
  "forest_meetingno",
  "forest_participate",

  "leader_index",
  
  "frules_clear",
  "frules_known",
  "fdisadvantage_women",
  "fenforce_rights",

  "fcrb_effective",  # L.20 In your opinion, how effective is the CRB at stopping unauthorized tree cutting or clearing land?
  "satisfy_crb",     # L.21 How satisfied are you with how the CRB is managing the local forest(s)?
  "crbatt_forcon",
  "crbatt_pos",
  "crbatt_employ",
  "crbatt_trust",
  "crbatt_bribe",
  
  "fcfmg_effective",
  "satisfy_cfmg",
  "cfmgatt_forcon",
  "cfmgatt_pos",
  "cfmgatt_employ",
  "cfmgatt_trust",
  "cfmgatt_bribe",
  
  "hhinform_revexp",  #L.34 My household has good information about what the CRB and/or CFMG does with revenue from REDD+/carbon projects.
  "hhrev_equal",      # L.35 Payments from REDD+/carbon projects are distributed equally among households in the community.  
  "redd_steal",       # L.36 REDD+/carbon projects steal oxygen from the trees. 
  "redd_drought",     # L.36 REDD+/carbon projects reduce rain and cause droughts.
  "reallocate_exp",   # N.17 Was the land reallocated because of a carbon/REDD+ project? [not using because of missingness]
  "forest_num",       # N.18 How much total land was reallocated? [not using cuz not significant]
  "togetherness",     # U.4 On a scale of 1 to 5, with 1 meaning very distant and 5 meaning very close, how strong is the feeling of togetherness in your village? [not using cuz not significant]
  "attend_event",     # U.5 How many times in the past 12 months did you participate in a family/village festival or ceremony (wedding, funeral, religious festival, etc.)?
  "attend_benefit",   # U.6 In the past 12 months, how often have people in this village gotten together to jointly ask local authorities/leaders for something to benefit the community?
  "happy",            # U.7 Taking all things together, how happy would you say you are these days? 

  # U.8 On a scale from 1 to 5, with 1 meaning you do not trust and 5 meaning you trust completely, please tell me how much you trust the following people or institutions.
  "trust_neigh",      # CFMG
  "trust_crb",
  "trust_cust",       # Village leaders
  "trust_dnpw",       # ZAWA / DNPW

  "tree_scale",
  "tree_enough",
  "tree_fair",
  "alt_scale",
  "alt_enough",
  "alt_fair",
  
  "elite_encroach", 
  "chief_encroach", 
  "investor_encroach", 
  "govt_enroach",
  
  grep("^tree_pay_", names(df), value = TRUE),  # payment types received from carbon/REDD+ project for not cutting/using trees
  grep("^tree_amt_", names(df), value = TRUE),  # cash payment
  grep("^alt_ben_", names(df), value = TRUE),
  grep("^frstserv_ben_", names(df), value = TRUE),
  grep("^tour_", names(df), value = TRUE),
  grep("^involved_hhmem_", names(df), value = TRUE)
)]

# Robust helper function to compute PC1 while handling NAs and Infs
get_pca1 <- function(df, vars) {
  sub <- df[, vars]
  valid_rows <- complete.cases(sub) & rowSums(is.infinite(as.matrix(sub))) == 0
  sub_clean <- sub[valid_rows, ]
  
  # Remove constant columns
  keep_cols <- apply(sub_clean, 2, function(x) sd(x, na.rm = TRUE) > 0)
  sub_clean <- sub_clean[, keep_cols, drop = FALSE]
  
  # Check if there are at least 2 valid columns left
  if (ncol(sub_clean) < 2 || nrow(sub_clean) < 2) return(rep(NA, nrow(df)))
  
  pca <- prcomp(sub_clean, scale. = TRUE, center = TRUE)
  
  scores <- rep(NA, nrow(df))
  scores[which(valid_rows)] <- pca$x[, 1]
  return(scores)
}

# Reverse-code to make higher values to indicate "better governance/perception"
Q4household$fdisadvantage_women_rev <- 6 - Q4household$fdisadvantage_women
Q4household$redd_steal_rev <- 6 - Q4household$redd_steal
Q4household$redd_drought_rev <- 6 - Q4household$redd_drought


# Filter out the people who does not know about the program
Q4household <- Q4household[Q4household$cfp_aware != 0, ]

# Converting household member involvement in CRB,VAG and CFMG into a binary variable, 1=has family member involved, 0=no/don't know
Q4household$hh_member_involved <- ifelse(
  Q4household$involved_hhmem_1 == 1 | 
    Q4household$involved_hhmem_2 == 1 | 
    Q4household$involved_hhmem_3 == 1, 
  1, 0
)


# Converting tree payment into a binary variable, 1= has received benefits, 0=no/prefer not to say 
Q4household$tree_pay_yn <- ifelse(
  Q4household$tree_pay_1 == 1 | Q4household$tree_pay_2 == 1, 
  1, 0
)

# Converting reception of direct benefits from REDD into a binary variable, 1= has received benefits, 0=no
Q4household$frstserv_benefit_any <- ifelse(
  Q4household$frstserv_ben_1 == 1 | 
    Q4household$frstserv_ben_2 == 1 | 
    Q4household$frstserv_ben_3 == 1,
  1, 0
)


# looking at possible PCA index variables's missingness
forest_participation_vars <- c("forest_meeting","forest_meetingno", "forest_participate")
colSums(is.na(Q4household[forest_participation_vars]))

tree_pay_vars <- c("tree_scale", "tree_enough", "tree_fair")
colSums(is.na(Q4household[tree_pay_vars]))

cfmg_perception_vars <- c("fcfmg_effective", "satisfy_cfmg", "cfmgatt_forcon", "cfmgatt_pos", "cfmgatt_employ", "cfmgatt_trust", "cfmgatt_bribe")
colSums(is.na(Q4household[cfmg_perception_vars]))

cfp_perception_vars <- c("cfp_important", "cfp_satisfied", "cfp_enough", "cfp_fair")
colSums(is.na(Q4household[cfp_perception_vars]))


# Generate PCA indices. 
# cfmg_perception_index and crb_perception_index are for explaining their
# satisfactory levels, instead of being put into the regression (?)
Q4household$cfp_perception_index <- get_pca1(Q4household, c("cfp_important", "cfp_satisfied", "cfp_enough", "cfp_fair"))

Q4household$tree_payment_perception <- get_pca1(Q4household, c("tree_scale", "tree_enough", "tree_fair"))

Q4household$cfmg_perception_index <- get_pca1(Q4household, c("fcfmg_effective", "satisfy_cfmg", 
                                                             "cfmgatt_forcon", "cfmgatt_pos", 
                                                             "cfmgatt_employ", "cfmgatt_trust", 
                                                             "cfmgatt_bribe"))

Q4household$crb_perception_index <- get_pca1(Q4household, c("fcrb_effective", "satisfy_crb", 
                                                            "crbatt_forcon", "crbatt_pos", 
                                                            "crbatt_employ", "crbatt_trust", 
                                                            "crbatt_bribe"))

Q4household$forest_governance_index <- get_pca1(Q4household, c("frules_clear", "frules_known", 
                                                               "fdisadvantage_women_rev", 
                                                               "fenforce_rights"))


potential_model_vars <- c(
  "cfp_perception_index",
  "trust_neigh", "trust_crb", "trust_cust", "trust_dnpw",
  "forest_governance_index", "cfmg_perception_index", "crb_perception_index",
  "hhinform_revexp", "hhrev_equal", "climate_index",
  "redd_steal", "redd_drought",  "gender", "age", "ed", "log_asset_pca",
  "majority_status", "leader_index", "forest_meeting", "hh_member_involved",
  "frstserv_ben_1","elite_encroach", "chief_encroach", "investor_encroach", "govt_enroach"
)

colSums(is.na(Q4household[potential_model_vars]))

corr_matrix <- cor(Q4household[, potential_model_vars], use = "pairwise.complete.obs")
ggcorrplot(corr_matrix, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3,
           colors = c("#B2182B", "white", "#2166AC"),
           title = "Correlation Matrix of Key Variables",
           tl.cex = 9)

# distinguishing trust and perception
cor(Q4household$trust_neigh, Q4household$cfmg_perception_index, use = "complete.obs")

# --- Convert 5-point Likert scale variables to binary indicators to better summarize the variables (1 = >3) ---
Q4household <- Q4household %>%
  mutate(across(
    c(trust_neigh, trust_crb, trust_cust, trust_dnpw,
      hhinform_revexp, hhrev_equal, redd_steal, redd_drought,
      elite_encroach, chief_encroach, investor_encroach, govt_enroach),
    ~ifelse(. > 3, 1, 0),
    .names = "{.col}_binary"
  ))

Q4sum_binary <- Q4household[ , c(
  "trust_neigh_binary", 
  "trust_crb_binary", 
  "trust_cust_binary", 
  "trust_dnpw_binary",
  "hhinform_revexp_binary", 
  "hhrev_equal_binary", 
  "redd_steal_binary", 
  "redd_drought_binary",
  "elite_encroach_binary", 
  "chief_encroach_binary", 
  "investor_encroach_binary", 
  "govt_enroach_binary"
)]


# summary
Q4summary_vars <- Q4household[ , c(
  "cfp_perception_index",
  "trust_neigh", "trust_crb", "trust_cust", "trust_dnpw",
  "forest_governance_index", "cfmg_perception_index", "crb_perception_index",
  "hhinform_revexp", "hhrev_equal", "climate_index",
  "redd_steal", "redd_drought",  "gender", "age", "ed_group", "log_asset_pca",
  "majority_status", "leader_index", "forest_meeting", "hh_member_involved",
  "frstserv_benefit_any","elite_encroach", "chief_encroach", "investor_encroach", "govt_enroach"
)]

Q4sum_binary <- Q4household[ , c(
  "trust_neigh_binary", 
  "trust_crb_binary", 
  "trust_cust_binary", 
  "trust_dnpw_binary",
  "hhinform_revexp_binary", 
  "hhrev_equal_binary", 
  "redd_steal_binary", 
  "redd_drought_binary",
  "elite_encroach_binary", 
  "chief_encroach_binary", 
  "investor_encroach_binary", 
  "govt_enroach_binary"
)]

Q4results <- summarize_data(Q4summary_vars)
Q4bi <- summarize_data(Q4sum_binary)

# View or export the summary tables
View(Q4results$numeric_summary)
View(Q4results$categorical_summary)
View(Q4bi$numeric_summary)

# build a regression model to explain support for REDD+
lm_b <- lm(cfp_perception_index ~ trust_neigh + trust_crb + trust_cust + trust_dnpw +
            forest_governance_index + cfmg_perception_index + crb_perception_index + 
            hhinform_revexp + hhrev_equal + redd_steal + redd_drought + frstserv_ben_1 +
            age + ed_group + log_asset_pca + majority_status + gender +
          leader_index + hh_member_involved + climate_index +
          elite_encroach + chief_encroach + investor_encroach + govt_enroach,
           data = Q4household)
stargazer(lm_b, type = "text")


vif(lm_b)
plot(lm_b)  

# build an interaction model
model_interaction <- lm(
  cfp_perception_index ~ frstserv_benefit_any * hhrev_equal +
    trust_neigh + trust_crb + trust_dnpw + climate_index +
    cfmg_perception_index + crb_perception_index + log_asset_pca + ed_group + gender
  + elite_encroach + chief_encroach + investor_encroach + govt_enroach,
  data = Q4household
)

summary(model_interaction)

library(interactions)

interact_plot(model_interaction, pred = hhrev_equal, modx = frstserv_benefit_any,
              plot.points = TRUE, interval = TRUE,
              x.label = "Perceived fairness of benefit distribution",
              y.label = "Support for REDD+ (cfp_perception_index)",
              modx.labels = c("No benefits received", "Received benefits"),
              main.title = "Interaction: Benefit Receipt × Fairness Perception")

# Support for REDD+ implementing institutions (CFMGs)
model_cfmg <- lm(
  cfmg_perception_index ~ trust_neigh + forest_governance_index + 
    hhinform_revexp + hhrev_equal + redd_steal + redd_drought + frstserv_ben_1 +
    age + ed_group + log_asset_pca + majority_status + gender + climate_index +
    leader_index + hh_member_involved + elite_encroach + chief_encroach + 
    investor_encroach + govt_enroach,
  data = Q4household
)

# Trust in CRBs as co-managers
model_crb <- lm(
  crb_perception_index ~ trust_crb + climate_index + trust_neigh + 
    redd_steal + redd_drought + frstserv_ben_1 + forest_governance_index +
    hhinform_revexp + hhrev_equal + age + ed_group + log_asset_pca + majority_status + gender +
    leader_index + hh_member_involved + elite_encroach + chief_encroach + investor_encroach + govt_enroach,
  data = Q4household
)

# Interact benefit receipt × fairness in Model 1
model_program_interact <- lm(
  cfp_perception_index ~ frstserv_ben_1 * hhrev_equal + trust_neigh + 
    trust_crb + trust_dnpw + climate_index +
    forest_governance_index + hhinform_revexp + age + ed_group + log_asset_pca + 
    redd_steal + redd_drought + majority_status + gender +
    leader_index + hh_member_involved + elite_encroach + chief_encroach + investor_encroach + govt_enroach,
  data = Q4household
)

stargazer(lm_b, model_cfmg, model_crb, model_program_interact, type = "html", out="Q4_total.html")

vif(model_program)
plot(model_program) 


vif(model_cfmg)
plot(model_cfmg) # this shows a bit deviance

vif(model_crb)
plot(model_crb) 

vif(model_program_interact)
plot(model_program_interact) 

library(interactions)
interact_plot(
  model_interaction,
  pred = hhrev_equal,
  modx = frstserv_benefit_any,
  plot.points = TRUE,
  interval = TRUE,
  modx.labels = c("No benefits", "Received benefits"),
  x.label = "Perceived Fairness",
  y.label = "Support for REDD+",
  main.title = "Interaction: Benefit Receipt × Fairness Perception",
  colors = c("#014421", "#66bb6a") 
)
ggsave("RQ4-interaction.png", width = 6, height = 4.5, dpi = 300)
