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
## 0. Functions
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
dir_output <- "/mnt/share/scratch/users/idrisov/Aim2_Outputs/"
ensure_dir_exists(dir_output)

##----------------------------------------------------------------
## 0.2 Read in data
##----------------------------------------------------------------
df_dex <- read_parquet(fp_dex)
df_ushd <- read_parquet(fp_ushd)

##----------------------------------------------------------------
## 1. Join DEX to USHD data
##----------------------------------------------------------------

# DEX - Group by summary DEX data to match USHD data - collapse on TOC and Payer
df_dex_collapse <- df_dex %>%
  group_by(state_name, location_name, fips_ihme, location_id, merged_location_id,
           acause, cause_name, sex_id, year_id, age_name, age_group_years_start) %>%
  summarize(spend_mean = mean(spend_mean),
            spend_lower = mean(spend_lower),
            spend_upper = mean(spend_upper))

# DEX - HIV data
df_dex_hiv <- df_dex_collapse %>%
  filter(acause == "hiv") 

# DEX - _subs data
df_dex_subs <- df_dex_collapse %>%
  group_by(state_name, location_name, fips_ihme, location_id, merged_location_id,
           sex_id, year_id, age_name, age_group_years_start) %>%
  summarize(spend_mean = mean(spend_mean),
            spend_lower = mean(spend_lower),
            spend_upper = mean(spend_upper)) %>%
  mutate(acause = "_subs",
         cause_name = "Substance use disorders")
  
# DEX - Combine HIV & _subs data
df_dex_collapse <- rbind(df_dex_hiv, df_dex_subs)

# USHD - Rename sex -> sex_id
df_ushd <- df_ushd %>%
  rename(
    sex_id = sex
  )

# USHD - Convert to int & numeric
df_ushd$sex_id <- as.integer(df_ushd$sex_id)
df_ushd$year_id <- as.integer(df_ushd$year_id)
df_ushd$pred_mean <- as.numeric(df_ushd$pred_mean)

# Merge DEX & USHD data
df_dex_ushd <- left_join(
  x = df_ushd,
  y = df_dex_collapse,
  by = c("state_name", "cnty_name" = "location_name", "fips_ihme", "location_id", "merged_location_id", 
         "acause", "cause_name", "sex_id", "year_id", 
         "age_group_years_start", "age_name"
         )
)

# Filter NA values & 98, 99 age groups
df_dex_ushd <- df_dex_ushd %>% 
  filter(age_name %nin% c(98, 99)) %>% 
  filter(!is.na(spend_mean))

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

# Sample our data so model doesn't take all day
df_dex_ushd_sample <- df_dex_ushd %>% sample_n(100000)

df_dex_ushd_sample_hiv <- df_dex_ushd_sample %>% filter(acause == "hiv")
df_dex_ushd_sample_subs <- df_dex_ushd_sample %>% filter(acause == "_subs")

# Model: HIV - MX Ratio ~ spend_mean
mod_simple_hiv <- sfa(
  log(pred_mean) ~ log(spend_mean),
  data          = df_dex_ushd_sample_hiv,
  ineffDecrease = TRUE  # Increased MX Ratio is "bad", so inefficiency = more MX ratio
)

summary(mod_simple_hiv)

# Model: SUD - MX Ratio ~ spend_mean
mod_simple_subs <- sfa(
  log(pred_mean) ~ log(spend_mean),
  data          = df_dex_ushd_sample_subs,
  ineffDecrease = TRUE  # Increased MX Ratio is "bad", so inefficiency = more MX ratio
)

summary(mod_simple_subs)

# --- 2. Add demographics as controls -------------------------------
# These controls shift the frontier itself (not the inefficiency).
# Here we include county name (cnty_name), sex, and age group.

# Model: MX Ratio ~ spend_mean + other variables
m_controls <- sfa(
  log(pred_mean) ~ log(spend_mean) + factor(cnty_name) + factor(sex_id) + factor(age_group_years_start),
  data          = df_dex_ushd,
  ineffDecrease = TRUE
)

summary(m_controls)

##----------------------------------------------------------------
## 3. Extract efficiencies
##
## Efficiency scores are in [0,1], where 1 = most efficient.
## Because we logged the dependent variable, set logDepVar = TRUE.
## minusU = TRUE gives Farrell-type efficiencies (higher = better).
##----------------------------------------------------------------
eff_simple_hiv <- efficiencies(mod_simple_hiv, asInData = TRUE,
                           logDepVar = TRUE, minusU = TRUE)
eff_simple_subs <- efficiencies(mod_simple_subs, asInData = TRUE,
                           logDepVar = TRUE, minusU = TRUE)
# eff_controls <- efficiencies(mod_controls, asInData = TRUE,
#                              logDepVar = TRUE, minusU = TRUE)

# Add to model results to dataframe
df_dex_ushd_sample_hiv$eff_simple <- eff_simple_hiv
df_dex_ushd_sample_subs$eff_simple <- eff_simple_subs
# df_dex_ushd_sample$eff_controls <- eff_controls

##----------------------------------------------------------------
## 4. Results
##----------------------------------------------------------------

# Create dollar mx ratio column
df_dex_ushd_sample_hiv$dollar_mx_ratio <- df_dex_ushd_sample_hiv$spend_mean / df_dex_ushd_sample_hiv$pred_mean
df_dex_ushd_sample_subs$dollar_mx_ratio <- df_dex_ushd_sample_subs$spend_mean / df_dex_ushd_sample_subs$pred_mean

View(df_dex_ushd_sample_hiv[, c("state_name", "cnty_name", "acause", "cause_name", "sex_id", "age_name",
                      "pred_mean", "spend_mean", "eff_simple", "dollar_mx_ratio")])
View(df_dex_ushd_sample_subs[, c("state_name", "cnty_name", "acause", "cause_name", "sex_id", "age_name",
                      "pred_mean", "spend_mean", "eff_simple", "dollar_mx_ratio")])


##----------------------------------------------------------------
## 5. Plots
##----------------------------------------------------------------

# Efficiency Score Distribution
ggplot(df_dex_ushd_sample_hiv, aes(x = eff_simple)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(title = "Distribution of County Efficiency Scores",
       x = "Efficiency Score", y = "Number of Counties")

ggplot(df_dex_ushd_sample_subs, aes(x = eff_simple)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(title = "Distribution of County Efficiency Scores",
       x = "Efficiency Score", y = "Number of Counties")


