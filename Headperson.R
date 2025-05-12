################################################################################
#                               Headperson                                     #
################################################################################
library(dplyr)
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
library(ggcorrplot)
library(interactions)

setwd("~/University/Study_Abroad/UPenn/2025SS/GAFL5320_ID/WorkInProgress")
head_df <- read_dta("CFP Headperson FINAL Combined no pii.dta")

head_df <- head_df %>%
  mutate(
    hgender = as.numeric(hgender),
    gender = ifelse(hgender == 1, 0,
                    ifelse(hgender == 2, 1, NA)) 
  )

# Making the education variable more interpretable by grouping it into 3 levels
# Group ed into three levels：Low / Middle / High
head_df <- head_df %>%
  mutate(
    ed_group = case_when(
      heduc >= 1 & heduc <= 7  ~ "Low",              # Primary incomplete or lower
      heduc >= 8 & heduc <= 11 ~ "Middle",           # Completed primary or lower secondary
      heduc >= 12 & heduc <= 15 ~ "High",            # Senior secondary and above
      TRUE ~ NA_character_                    # Catch missing, 888, 999, etc.
    ),
    ed_group = factor(ed_group, levels = c("Low", "Middle", "High"))
  )

# Function to summarize both numeric and categorical variables
summarize_data <- function(head_df) {
  
  # --- Numeric Variable Summary ---
  # Select only numeric columns from the dataset
  numeric_vars <- head_df[, sapply(head_df, is.numeric)]
  
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
  cat_vars <- head_df[, sapply(head_df, function(x) is.factor(x) || is.character(x))]
  
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

# --- Select relevant variables for Question 1--- #
Q1headperson <- head_df[, c(
  "gender", 
  "cfp_aware", 
  "cfp_important", 
  "cfp_satisfied",
  "cfp_enough", 
  "cfp_fair",
  "heduc",
  "hage",
  "ed_group"
)]


# --- Filter out respondents who were not aware of CFP and gender = NA ---
Q1headperson <- Q1headperson %>%
  filter(cfp_aware != 0 & !is.na(gender))

# --- Convert 5-point Likert scale variables to binary indicators (1 = >3) ---
Q1headperson <- Q1headperson %>%
  mutate(across(
    c(cfp_important, cfp_satisfied, cfp_enough),
    ~ifelse(. > 3, 1, 0),
    .names = "{.col}_binary"
  ))

# summary stats
Q1hresults <- summarize_data(Q1headperson)

# View or export the summary tables
View(Q1hresults$numeric_summary)
View(Q1hresults$categorical_summary)


# Create grouped summary statistics
Q1headperson %>%
  group_by(gender) %>%
  summarise(across(
    c(cfp_important, cfp_satisfied, cfp_enough, cfp_fair),
    ~mean(., na.rm = TRUE)
  ))

# Group by gender and compute mean values for the binary outcome variables
Q1headperson %>%
  group_by(gender) %>%
  summarise(
    n = n(),
    cfp_important = mean(cfp_important_binary, na.rm = TRUE),
    cfp_satisfied = mean(cfp_satisfied_binary, na.rm = TRUE),
    cfp_enough = mean(cfp_enough_binary, na.rm = TRUE)
  )

################################################################################
#                                RQ2 Analysis                                  #
################################################################################

# --- Select relevant variables for Question 2--- #
Q2headperson <- head_df[, c(
  "gender", 
  
  # H.6 The following is a list of problems often faced by people in this area. On a scale from 1 to 10, 
  # with 1 being the smallest problem and 10 being the biggest problem, how big of a problem are the following issues to your village?
  "hproblem_temp",
  "hproblem_rain",
  "heduc",
  "hage",

  grep("^hdrought_years_", names(head_df), value = TRUE),   # H.3 What years was the village affected by a drought? 
  grep("^hrain_years_", names(head_df), value = TRUE),
  grep("^hflood_years_", names(head_df), value = TRUE),
  
  # Now I am going to read you a series of statements. Please tell me how much you agree or disagree with the following.
  "htemp_important",       # H.13 It is important that village members take steps to protect themselves from these changes in temperature and rainfall patterns.
  
  grep("^hclimatechng_", names(head_df), value = TRUE))]  # H.1 In the past 10 years, have you observed any of the following changes in your village? 

# --- Count the number of years the headperson saw climate change incidents --- #
# Identify relevant columns for drought, rain, and flood years (excluding _888 and _999, which are 'Don't know' or invalid)
# Identify all climate change indicator columns, excluding the "other" column
drought_cols <- names(Q2headperson)[grepl("^hdrought_years_", names(Q2headperson)) & !grepl("_(888|999)$", names(Q2headperson))]
rain_cols    <- names(Q2headperson)[grepl("^hrain_years_", names(Q2headperson)) & !grepl("_(888|999)$", names(Q2headperson))]
flood_cols   <- names(Q2headperson)[grepl("^hflood_yearrs_", names(Q2headperson)) & !grepl("_(888|999)$", names(Q2headperson))]
climate_cols <- names(Q2headperson)[grepl("^hclimatechng_", names(Q2headperson)) & !grepl("_oth$", names(Q2headperson))]

# Count the number of years each headperson reported experiencing drought, rain variability, or floods
# and how many types of climate change were observed per respondent
Q2headperson <- Q2headperson %>%
  rowwise() %>%
  mutate(
    drought_years_count = sum(c_across(all_of(drought_cols)), na.rm = TRUE),
    rain_years_count    = sum(c_across(all_of(rain_cols)), na.rm = TRUE),
    flood_years_count   = sum(c_across(all_of(flood_cols)), na.rm = TRUE),
    climate_change_count = sum(c_across(all_of(climate_cols)), na.rm = TRUE)
  ) %>%
  ungroup()

# Select only processed (summary) variables for grouping — exclude raw variables
summary_vars <- c("drought_years_count", "rain_years_count", "flood_years_count", 
                  "climate_change_count", "hproblem_temp", "hproblem_rain", "htemp_important")

# Group by gender and calculate the mean for each summary variable
Q2headperson_summary <- Q2headperson %>%
  group_by(gender) %>%
  summarise(across(all_of(summary_vars), ~ mean(.x, na.rm = TRUE), .names = "mean_{.col}"))

print(Q2headperson_summary)

lm0 <- lm(drought_years_count ~ gender + heduc + hage, data = Q2headperson)
lm1 <- lm(rain_years_count ~ gender + heduc + hage, data = Q2headperson)
lm2 <- lm(flood_years_count ~ gender + heduc + hage, data = Q2headperson)
lm3 <- lm(climate_change_count ~ gender + heduc + hage, data = Q2headperson)
lm4 <- lm(hproblem_temp ~ gender + heduc + hage, data = Q2headperson)
lm5 <- lm(hproblem_rain ~ gender + heduc + hage, data = Q2headperson)
lm6 <- lm(htemp_important ~ gender + heduc + hage, data = Q2headperson)
stargazer(lm0,lm1,lm2,lm3,lm4,lm5,lm6, type = "text")
