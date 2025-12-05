##----------------------------------------------------------------
##' Title: C_frontier_analysis.R
##'
##' Purpose: TBD
##----------------------------------------------------------------

##----------------------------------------------------------------
## 0. Clear environment and set library paths
##----------------------------------------------------------------
rm(list = ls())
pacman::p_load(data.table, arrow, tidyverse, glue)

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
source("/ihme/cc_resources/libraries/current/r/get_age_metadata.R")

##----------------------------------------------------------------
## 0 Set Interactive Parameters
##
## Input parameters:
## cause : the cause to process, either "hiv" or "_subs"
## model : the type of model to process, either "simple" or "extended"
##----------------------------------------------------------------
if (interactive()) {
  cause <- "hiv" 
  # cause <- "_subs"
  model <- "simple"
  # model <- "extended"
} else {
  args <- commandArgs(trailingOnly = TRUE)
  cause <- as.character(args[1])
  model <- as.character(args[2])
}

print(paste0("C_frontier_analysis.R, processing cause: ", cause))
print(paste0("Model type: ", model))

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

date_ushd <- "20251204"
fp_ushd <- file.path(h, "/aim_outputs/Aim2/B_aggregation/", date_ushd, "/compiled_ushd_data_2010_2019.parquet")

# Set output directories
date_today <- format(Sys.time(), "%Y%m%d")
dir_output <- file.path(h, "/aim_outputs/Aim2/C_frontier_analysis/", date_today)
ensure_dir_exists(dir_output)

# Set directory for DEX + USHD data (if already created)
date_data_combo <- "20251204_backup"
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

##----------------------------------------------------------------
## 1. Filter & group by data
##
## This is to reduce the number of variables going into the model since there were
## too many rows causing the model to take too long to finish.
##
## We are collapsing on sex & age groups to drastically reduce the row count
## by using weighted means based on the population counts, from the USHD data
##----------------------------------------------------------------
# Filter our data to our desired cause
df_dex_ushd_acause <- df_dex_ushd %>% filter(acause == cause)

# # Sample data for testing
# testing <- F
# 
# if (testing) {
#   df_dex_ushd_acause <- df_dex_ushd_acause %>% sample_n(50000)
# }

# Drop age groups under 20, and 85+
df_dex_ushd_acause <- df_dex_ushd_acause %>%
  filter(!age_name %in% c("0 - <1", "1 - <5", "5 - <10", "10 - <15", "15 - <20", "85+"))

# Collapse into 10 year age binds
df_dex_ushd_acause <- df_dex_ushd_acause %>%
  mutate(age_name_10_yr_bin = case_when(
    age_name %in% c("20 - <25", "25 - <30") ~ "20 - <30",
    age_name %in% c("30 - <35", "35 - <40") ~ "30 - <40",
    age_name %in% c("40 - <45", "45 - <50") ~ "40 - <50",
    age_name %in% c("50 - <55", "55 - <60") ~ "50 - <60",
    age_name %in% c("60 - <65", "65 - <70") ~ "60 - <70",
    age_name %in% c("70 - <75", "75 - <80") ~ "70 - <80",
    age_name %in% c("80 - <85") ~ "80 - <85"
  ))

# Perform group by 
df_dex_ushd_acause <- df_dex_ushd_acause %>%
  group_by(state_name, cnty_name, fips_ihme, location_id, acause, year_id, sex_id, age_name_10_yr_bin) %>%
  summarize(
    pred_mean = weighted.mean(pred_mean, pop),
    spend_mean = weighted.mean(spend_mean, pop)
  )

##----------------------------------------------------------------
## 2. Frontier Analysis Model
##
## Formula - USHD MX Ratio is the outcome, DEX spend_mean is the predictor (+ other variables)
##----------------------------------------------------------------
if (model == "simple") {
  # --- Baseline model: log(MX Ratio) on log(spending mean) -----------------
  # This is the simplest Cobb–Douglas frontier in log-log form.
  # ineffDecrease = TRUE means inefficiency increases MX Ratio (bad outcome).
  # Try flipping to FALSE if you want to see the difference in orientation.
  
  # Simple Stochastic Frontier Example
  # Outcome: MX Ratio (pred_mean)
  # Predictor: Healthcare spending (spend_mean)
  
  # Model: MX Ratio ~ spend_mean
  print(paste0("Starting Simple Model @ ", Sys.time()))
  t1 <- Sys.time()
  mod_simple <- sfa(
    log(pred_mean) ~ log(spend_mean),
    data          = df_dex_ushd_acause,
    ineffDecrease = TRUE  # Increased MX Ratio is "bad", so inefficiency = more MX ratio
  )
  t2 <- Sys.time()
  print(paste0("Elapsed time for Simple Model: ", round(t2 - t1, 1), " seconds"))
  
  # Save model object
  model_filename <- paste0("mod_", cause, "_", model, ".rds")
  saveRDS(mod_simple, file.path(dir_output, model_filename))
  print(paste0("Saved ", model, " model @ ", dir_output, "/",model_filename))
  
} else if (model == "extended") {
  # --- Extended model: log(MX Ratio) on log(spending mean) + controls -----------------
  ##  * Still frontier of log(pred_mean) vs log(spend_mean),
  ##    BUT now controls for composition (age, sex, time, county).
  ##  * These covariates shift the frontier (what is “expected” given case-mix),
  ##    so inefficiency is measured after conditioning on them.
  ##
  ##  Notes:
  ##    - factor(sex_id) and factor(age_group_years_start) adjust for case-mix.
  ##    - factor(age_name_10_yr_bin) ~ county fixed effects (optional; remove if too slow).
  ##    - factor(year_id) accounts for secular trends in outcomes/spending.
  print(paste0("Starting Extended Model @ ", Sys.time()))
  t1 <- Sys.time()
  mod_extended <- frontier::sfa(
    formula = log(pred_mean) ~ log(spend_mean) +
      factor(sex_id) + 
      factor(age_name_10_yr_bin) +
      factor(year_id),
    data          = df_dex_ushd_acause,
    ineffDecrease = TRUE
  )
  t2 <- Sys.time()
  print(paste0("Elapsed time for Extended Model: ", round(t2 - t1, 1), " seconds"))
  
  # Save model object
  model_filename <- paste0("mod_", cause, "_", model, ".rds")
  saveRDS(mod_extended, file.path(dir_output, model_filename))
  print(paste0("Saved ", model, " model @ ", dir_output, "/",model_filename))
}

##----------------------------------------------------------------
## 3. Extract efficiencies
##
## Efficiency scores are in [0,1], where 1 = most efficient.
## Because we logged the dependent variable, set logDepVar = TRUE.
## minusU = TRUE gives Farrell-type efficiencies (higher = better).
##----------------------------------------------------------------
if (model == "simple") {
  # Basic Model Efficiency Scores
  df_dex_ushd_acause$eff_simple <- efficiencies(mod_simple, 
                                                asInData   = TRUE,
                                                logDepVar  = TRUE,
                                                minusU     = TRUE)
} else if (model == "extended") {
  # Extended Model Efficiency Scores
  df_dex_ushd_acause$eff_extended <- efficiencies(mod_extended,
                                                  asInData   = TRUE,
                                                  logDepVar  = TRUE,
                                                  minusU     = TRUE)
}

##----------------------------------------------------------------
## 4. Save to Parquet Files
##----------------------------------------------------------------
fn_output <- paste0("fa_estimates_", cause, "_", model, ".parquet")
write_parquet(df_dex_ushd_acause, file.path(dir_output, fn_output))

##----------------------------------------------------------------
## TESTING - SAFE TO DELETE
##----------------------------------------------------------------
# Seeing how many rows exist if we collapse on age & sex. 30780 remain.
# df_test <- df_dex_ushd_acause %>%
#   group_by(state_name, cnty_name, fips_ihme, acause, year_id) %>%
#   summarize(
#     pred_mean = mean(pred_mean),
#     spend_mean= mean(spend_mean)
#   )

# This has age weights for each age group, but not by sex
# df_age <- get_age_metadata(release_id = 16, age_group_set_id = 24)

# right now year*age*sex*county
# desired county*year

# Testing reading in data to make sure it has the columns we expect
# sud_extended <- "fa_estimates__subs_extended.parquet"
# sud_simple <- "fa_estimates__subs_simple.parquet"
# hiv_extended <- "fa_estimates_hiv_extended.parquet"
# hiv_simple <- "fa_estimates_hiv_simple.parquet"
# 
# df_test <- read_parquet(file.path(dir_output, hiv_simple))

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



