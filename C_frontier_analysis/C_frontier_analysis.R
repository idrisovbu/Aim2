##----------------------------------------------------------------
##' Title: C_frontier_analysis.R
##'
##' Purpose: TBD
##----------------------------------------------------------------

##----------------------------------------------------------------
## 0. Clear environment and set library paths
##----------------------------------------------------------------
rm(list = ls())

# Set drive paths
if (Sys.info()["sysname"] == 'Linux'){
  j <- "/home/j/"
  h <- paste0("/ihme/homes/",Sys.info()[7],"/")
  l <- '/ihme/limited_use/'
} else if (Sys.info()["sysname"] == 'Darwin'){
  j <- "/Volumes/snfs"
  h <- paste0("/Volumes/",Sys.info()[7],"/")
  l <- '/Volumes/limited_use'
} else {
  j <- "J:/"
  h <- "H:/"
  l <- 'L:/'
}

library('frontier')
# source("/ihme/cc_resources/libraries/current/r/get_outputs.R")
# source("/ihme/cc_resources/libraries/current/r/get_location_metadata.R")
# source("/ihme/cc_resources/libraries/current/r/get_age_metadata.R")

##----------------------------------------------------------------
## 0 Set Interactive Parameters
##
## cause : the cause to process, either "hiv" or "_subs"
##----------------------------------------------------------------
if (interactive()) {
  cause <- "hiv" # "_subs"
} else {
  cause <- as.character(args[1])
}

print(paste0("C_frontier_analysis.R, processing cause: ", cause))

##----------------------------------------------------------------
## 0.0 Functions
##----------------------------------------------------------------
ensure_dir_exists <- function(dir_path) {
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
}

'%nin%' <- Negate('%in%')

##----------------------------------------------------------------
## 0.1 Set directories for DEX estimate data / county estimates
##----------------------------------------------------------------
# Set path for data
date_dex <- "20251123"
fp_dex <- file.path(h, "/aim_outputs/Aim2/B_aggregation/", date_dex, "/compiled_dex_data_2010_2019.parquet")

date_ushd <- "20251123"
fp_ushd <- file.path(h, "/aim_outputs/Aim2/B_aggregation/", date_ushd, "/compiled_ushd_data_2010_2019.parquet")

# Set output directories
date_today <- format(Sys.time(), "%Y%m%d")
dir_output <- file.path(h, "/aim_outputs/Aim2/C_frontier_analysis/", date_today)
ensure_dir_exists(dir_output)

# Set directory for DEX + USHD data (if already created)
date_data_combo <- "20251201"
dir_data_combo <- file.path(h, "/aim_outputs/Aim2/C_frontier_analysis/", date_data_combo)
fp_data_combo <- file.path(dir_data_combo, "compiled_dex_ushd_2010_2019.parquet")
ensure_dir_exists(dir_data_combo)

##----------------------------------------------------------------
## 0.2 Read in data
##----------------------------------------------------------------

# If DEX + USHD data already created, just read in, else create it and save it
read_in_saved_data <- T

if (file.exists(fp_data_combo) & read_in_saved_data) {
  df_dex_ushd <- read_parquet(fp_data_combo)
  
} else { # Perform Step 1, and write to flat file
df_dex <- read_parquet(fp_dex)
df_ushd <- read_parquet(fp_ushd)
  
##----------------------------------------------------------------
## 1. Join DEX to USHD data
##----------------------------------------------------------------

# DEX - Group by summary DEX data to match USHD data - collapse on TOC and Payer
dex_cols_collapse <- c("year_id", "geo", "location_name", "fips",
                       "acause", "cause_name", "age_group_years_start", "age_name", 
                       "sex_id", "sex_name",
                       "state_name", "location_id", "merged_location_id")

df_dex_collapse <- df_dex %>%
  filter(payer == "all") %>% # to remove the other payers and only keep "all" for the county level
  filter(geo == "county") %>% # only filter to county level to match USHD data
  group_by(across(all_of(dex_cols_collapse))) %>%
  summarize(spend_mean = mean(spend_mean),
            spend_lower = mean(spend_lower),
            spend_upper = mean(spend_upper))

# DEX - HIV data
df_dex_hiv <- df_dex_collapse %>%
  filter(acause == "hiv") 

# DEX - _subs data
dex_subs_cols_collapse <- c("year_id", "geo", "location_name", "fips",
                            "age_group_years_start", "age_name", "sex_id", "sex_name", "state_name", 
                            "location_id", "merged_location_id")

df_dex_subs <- df_dex_collapse %>%
  filter(acause != "hiv") %>%
  group_by(across(all_of(dex_subs_cols_collapse))) %>%
  summarize(spend_mean = mean(spend_mean),
            spend_lower = mean(spend_lower),
            spend_upper = mean(spend_upper)) %>%
  mutate(acause = "_subs",
         cause_name = "Substance use disorders")
  
# DEX - Combine HIV & _subs data
df_dex_hivsud <- rbind(df_dex_hiv, df_dex_subs)

# USHD - Rename sex -> sex_id
df_ushd <- df_ushd %>%
  rename(sex_id = sex)

# USHD - Convert to int & numeric
df_ushd$sex_id <- as.integer(df_ushd$sex_id)
df_ushd$year_id <- as.integer(df_ushd$year_id)
df_ushd$pred_mean <- as.numeric(df_ushd$pred_mean)

# Merge DEX & USHD data
df_dex_ushd <- left_join(
  x = df_ushd,
  y = df_dex_hivsud,
  by = c("year_id", "acause", "cause_name", "age_group_years_start", 
         "age_name", "sex_id", "state_name", "location_id", "merged_location_id",
         "cnty_name" = "location_name",
         "fips_ihme" = "fips"
  )
)

unmatched <- anti_join(x = df_ushd,
                       y = df_dex_hivsud,
                       by = c("year_id", "acause", "cause_name", "age_group_years_start", 
                              "age_name", "sex_id", "state_name", "location_id", "merged_location_id",
                              "cnty_name" = "location_name",
                              "fips_ihme" = "fips"
                       )) %>%
  filter(age_group_years_start %nin% c(98, 99)) # remove age groups that don't exist in DEX data

# Filter NA values & 98, 99 age groups
df_dex_ushd <- df_dex_ushd %>% 
  filter(age_name %nin% c(98, 99)) %>% 
  filter(!is.na(spend_mean))

# Save data
write_parquet(df_dex_ushd, fp_data_combo)

}

View(head(df_dex_ushd, 100))

##----------------------------------------------------------------
## 2. Frontier Analysis Model
##
## Formula - USHD MX Ratio is the outcome, DEX spend_mean is the predictor (+ other variables)
##----------------------------------------------------------------

# --- 1. Baseline model: log(MX Ratio) on log(spending mean) -----------------
# This is the simplest Cobb–Douglas frontier in log-log form.
# ineffDecrease = TRUE means inefficiency increases MX Ratio (bad outcome).
# Try flipping to FALSE if you want to see the difference in orientation.

# Simple Stochastic Frontier Example
# Outcome: MX Ratio (pred_mean)
# Predictor: Healthcare spending (spend_mean)

# If we want to use sampled data or not
use_sample_data <- F

if (use_sample_data) {
  # Sample our data so model doesn't take all day
  df_dex_ushd <- df_dex_ushd %>% sample_n(100000)
}

df_dex_ushd_hiv <- df_dex_ushd %>% filter(acause == "hiv")
df_dex_ushd_subs <- df_dex_ushd %>% filter(acause == "_subs")

# Model: HIV - MX Ratio ~ spend_mean
mod_simple_hiv <- sfa(
  log(pred_mean) ~ log(spend_mean),
  data          = df_dex_ushd_hiv,
  ineffDecrease = TRUE  # Increased MX Ratio is "bad", so inefficiency = more MX ratio
)

summary(mod_simple_hiv)

# Model: SUD - MX Ratio ~ spend_mean
mod_simple_subs <- sfa(
  log(pred_mean) ~ log(spend_mean),
  data          = df_dex_ushd_subs,
  ineffDecrease = TRUE  # Increased MX Ratio is "bad", so inefficiency = more MX ratio
)

summary(mod_simple_subs)

# --- 2. Extended model: log(MX Ratio) on log(spending mean) + controls -----------------
##  * Still frontier of log(pred_mean) vs log(spend_mean),
##    BUT now controls for composition (age, sex, time, county).
##  * These covariates shift the frontier (what is “expected” given case-mix),
##    so inefficiency is measured after conditioning on them.
##
##  Notes:
##    - factor(year_id) accounts for secular trends in outcomes/spending.
##    - factor(sex_id) and factor(age_group_years_start) adjust for case-mix.
##    - factor(cnty_name) ~ county fixed effects (optional; remove if too slow).

# HIV
t1 <- Sys.time()
mod_extended_hiv <- frontier::sfa(
  formula = log(pred_mean) ~ log(spend_mean) +
    factor(year_id) +
    factor(sex_id) +
    factor(age_group_years_start), 
  data          = df_dex_ushd_hiv,
  ineffDecrease = TRUE
)
t2 <- Sys.time()
t2 - t1

summary(mod_extended_hiv)

# SUD
mod_extended_subs <- frontier::sfa(
  formula = log(pred_mean) ~ log(spend_mean) +
    factor(year_id) +
    factor(sex_id) +
    factor(age_group_years_start),
  data          = df_dex_ushd_subs,
  ineffDecrease = TRUE
)

summary(mod_extended_subs)

##----------------------------------------------------------------
## 3. Extract efficiencies
##
## Efficiency scores are in [0,1], where 1 = most efficient.
## Because we logged the dependent variable, set logDepVar = TRUE.
## minusU = TRUE gives Farrell-type efficiencies (higher = better).
##----------------------------------------------------------------

# Basic Model Efficiency Scores
df_dex_ushd_hiv$eff_simple <- efficiencies(mod_simple_hiv, 
                               asInData = TRUE,
                               logDepVar = TRUE,
                               minusU = TRUE)
df_dex_ushd_subs$eff_simple <- efficiencies(mod_simple_subs,
                                asInData = TRUE,
                                logDepVar = TRUE,
                                minusU = TRUE)

# Extended Model Efficiency Scores
df_dex_ushd_hiv$eff_extended <- efficiencies(
  mod_extended_hiv,
  asInData   = TRUE,
  logDepVar  = TRUE,
  minusU     = TRUE
)

df_dex_ushd_subs$eff_extended <- efficiencies(
  mod_extended_subs,
  asInData   = TRUE,
  logDepVar  = TRUE,
  minusU     = TRUE
)

##----------------------------------------------------------------
## 4. Save to Parquet Files (TODO - rerun not using sample for final data)
##----------------------------------------------------------------

write_parquet(df_dex_ushd_hiv, file.path(dir_output, "hiv_data_fa_estimates.parquet"))
write_parquet(df_dex_ushd_subs, file.path(dir_output, "sud_data_fa_estimates.parquet"))

##----------------------------------------------------------------
## Results - UNUSED ATM
##----------------------------------------------------------------
# 
# # Create dollar mx ratio column
# df_dex_ushd_hiv$dollar_mx_ratio <- df_dex_ushd_hiv$spend_mean / df_dex_ushd_hiv$pred_mean
# df_dex_ushd_subs$dollar_mx_ratio <- df_dex_ushd_subs$spend_mean / df_dex_ushd_subs$pred_mean
# 
# View(df_dex_ushd_hiv[, c("state_name", "cnty_name", "acause", "cause_name", "sex_id", "age_name",
#                       "pred_mean", "spend_mean", "eff_simple", "dollar_mx_ratio")])
# View(df_dex_ushd_subs[, c("state_name", "cnty_name", "acause", "cause_name", "sex_id", "age_name",
#                       "pred_mean", "spend_mean", "eff_simple", "dollar_mx_ratio")])


##----------------------------------------------------------------
## Plots - TO BE MOVED TO D_FIGURES SCRIPT
## BELOW IS JUST FOR LOOKING AT DATA
##----------------------------------------------------------------

# # Efficiency Score Distribution
# 
# # HIV - Basic
# ggplot(df_dex_ushd_hiv, aes(x = eff_simple)) +
#   geom_histogram(bins = 30, fill = "steelblue", color = "white") +
#   labs(title = "Distribution of County Efficiency Scores",
#        x = "Efficiency Score", y = "Number of Counties")
# 
# # HIV - Extended
# ggplot(df_dex_ushd_hiv, aes(x = eff_extended)) +
#   geom_histogram(bins = 30, fill = "steelblue", color = "white") +
#   labs(title = "Distribution of County Efficiency Scores",
#        x = "Efficiency Score", y = "Number of Counties")
# 
# # SUD - Basic
# ggplot(df_dex_ushd_subs, aes(x = eff_simple)) +
#   geom_histogram(bins = 30, fill = "steelblue", color = "white") +
#   labs(title = "Distribution of County Efficiency Scores",
#        x = "Efficiency Score", y = "Number of Counties")
# 
# # SUD - Extended
# ggplot(df_dex_ushd_subs, aes(x = eff_extended)) +
#   geom_histogram(bins = 30, fill = "steelblue", color = "white") +
#   labs(title = "Distribution of County Efficiency Scores",
#        x = "Efficiency Score", y = "Number of Counties")



