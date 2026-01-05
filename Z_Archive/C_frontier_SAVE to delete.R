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

date_pmp <- "20251222"
fp_pmp <- file.path(h, "/aim_outputs/Aim2/A_data_preparation/", date_pmp, "/FA/prev_mort_pop_data.parquet")

# date_ushd <- "20251204"
# fp_ushd <- file.path(h, "/aim_outputs/Aim2/B_aggregation/", date_ushd, "/compiled_ushd_data_2010_2019.parquet")

# Set output directories
date_today <- format(Sys.time(), "%Y%m%d")
dir_output <- file.path(h, "/aim_outputs/Aim2/C_frontier_analysis/", date_today)
ensure_dir_exists(dir_output)

#covariates based on Haley's code
cov_df_path <- "/ihme/resource_tracking/us_value/data/sfa_covars2021_shea.csv"
cov_df <- fread(cov_df_path)

##----------------------------------------------------------------
## 0.2 Read in data
##----------------------------------------------------------------
# DEX data - filter down to state level, payers = all
ds_des <- open_dataset(fp_dex)

df_dex <- ds_des %>%
  filter(geo == "state") %>%
  filter(payer == "all") %>%
  select(c("year_id", "geo", "location_name", "fips", "toc", 
           "acause", "cause_name", "age_group_years_start", "age_name", 
           "sex_id", "spend_mean", "spend_lower", "spend_upper")) %>%
  collect()

# Add location_id to DEX data
df_state_names <- fread("/ihme/dex/us_county/maps/states.csv")

df_state_names <- df_state_names %>%
  select(c("state_name", "location_id"))

df_dex <- left_join(
  x = df_dex,
  y = df_state_names,
  by = c("location_name" = "state_name")
)

# GBD Prevalence, Mortality, Population data
df_pmp <- read_parquet(fp_pmp)

# Modify age group names to match DEX age group names
df_pmp <- df_pmp %>%
  mutate(age_name = case_when(
    age_group_name == "0 - <1"   ~ "0 - <1",
    age_group_name == "1 - <5"   ~ "1 - <5",
    age_group_name == "5 to 9"   ~ "5 - <10",
    age_group_name == "10 to 14" ~ "10 - <15",
    age_group_name == "15 to 19" ~ "15 - <20",
    age_group_name == "20 to 24" ~ "20 - <25",
    age_group_name == "25 to 29" ~ "25 - <30",
    age_group_name == "30 to 34" ~ "30 - <35",
    age_group_name == "35 to 39" ~ "35 - <40",
    age_group_name == "40 to 44" ~ "40 - <45",
    age_group_name == "45 to 49" ~ "45 - <50",
    age_group_name == "50 to 54" ~ "50 - <55",
    age_group_name == "55 to 59" ~ "55 - <60",
    age_group_name == "60 to 64" ~ "60 - <65",
    age_group_name == "65 to 69" ~ "65 - <70",
    age_group_name == "70 to 74" ~ "70 - <75",
    age_group_name == "75 to 79" ~ "75 - <80",
    age_group_name == "80 to 84" ~ "80 - <85",
    age_group_name == "85+"      ~ "85+"
  ))

df_pmp <- df_pmp %>%
  ungroup() %>%
  select(!c("age_group_name"))

##----------------------------------------------------------------
## 1. Collapse & Merge DEX & GBD data
##----------------------------------------------------------------
# Collapse on TOC in DEX data
df_dex <- df_dex %>%
  group_by(year_id, geo, location_name, location_id, fips, acause, cause_name, age_group_years_start, age_name, sex_id,) %>%
  summarize(spend_mean = sum(spend_mean))

# Create "_subs" acause
df_dex_sud <- df_dex %>%
  filter(acause != "hiv")

df_dex_sud <- df_dex_sud %>%
  group_by(year_id, geo, location_name, location_id, fips, age_group_years_start, age_name, sex_id) %>%
  summarize(spend_mean = sum(spend_mean)) %>%
  mutate(
    acause = "_subs",
    cause_name = "Substance use disorders"
    )

# Filter on "hiv" acause"
df_dex_hiv <- df_dex %>% 
  filter(acause == "hiv")

# Rbind back "_subs" & "hiv"
df_dex <- rbind(df_dex_hiv, df_dex_sud)

# Merge DEX & GBD data
df_m <- left_join(
  x = df_pmp,
  y = df_dex,
  by = c("location_id", "sex_id", "year_id", "acause", "cause_name", "location_name", "age_name")
)

##----------------------------------------------------------------
## 2. Create spend_prev_ratio & mort_prev_ratio columns
##----------------------------------------------------------------
df_m <- df_m %>%
  mutate(
    spend_prev_ratio = (spend_mean / prevalence),
    mort_prev_ratio = (mortality / prevalence)
  )

##----------------------------------------------------------------
## 3. Format age weights to match DEX age bins
## 
## To read more about GBD age weights and where the below values come from, read:
## https://scicomp-docs.ihme.washington.edu/db_queries/current/get_age_metadata.html
## under "Age group weight"
##----------------------------------------------------------------
# Pull GBD age weights
df_age <- get_age_metadata(release_id = 16)

# Label the 0 - <1, 1 - <5, & 85+ age groups 
df_age <- df_age %>%
  mutate(
    age_name_group = case_when(
      age_group_id %in% c(2, 3, 388, 389) ~ "0 - <1",
      age_group_id %in% c(238, 34) ~ "1 - <5",
      age_group_id %in% c(31, 32, 235) ~ "85+"
                              )
  )

# Filter out the age groups we won't collapse
df_age_non_collapse <- df_age %>%
  filter(is.na(age_name_group))

# Collapse on 0 - <1, 1 - <5, & 85+ age groups 
df_age <- df_age %>%
  filter(!is.na(age_name_group)) %>%
  group_by(age_name_group) %>%
  summarize(age_group_weight_value = sum(age_group_weight_value))

df_age <- df_age %>%
  mutate(
    age_group_years_start = case_when(
      age_name_group == "0 - <1" ~ 0,
      age_name_group == "1 - <5" ~ 1,
      age_name_group == "85+" ~ 85,
    )
  )

df_age <- df_age %>%
  rename(
    "age_group_name" = "age_name_group"
  )

# Rbind age weights back together
df_age_weights <- rbind(df_age_non_collapse, df_age, fill = TRUE)

# Modify age group names to match DEX age group names
df_age_weights <- df_age_weights %>%
  mutate(age_group_name = case_when(
    age_group_name == "0 - <1"   ~ "0 - <1",
    age_group_name == "1 - <5"   ~ "1 - <5",
    age_group_name == "5 to 9"   ~ "5 - <10",
    age_group_name == "10 to 14" ~ "10 - <15",
    age_group_name == "15 to 19" ~ "15 - <20",
    age_group_name == "20 to 24" ~ "20 - <25",
    age_group_name == "25 to 29" ~ "25 - <30",
    age_group_name == "30 to 34" ~ "30 - <35",
    age_group_name == "35 to 39" ~ "35 - <40",
    age_group_name == "40 to 44" ~ "40 - <45",
    age_group_name == "45 to 49" ~ "45 - <50",
    age_group_name == "50 to 54" ~ "50 - <55",
    age_group_name == "55 to 59" ~ "55 - <60",
    age_group_name == "60 to 64" ~ "60 - <65",
    age_group_name == "65 to 69" ~ "65 - <70",
    age_group_name == "70 to 74" ~ "70 - <75",
    age_group_name == "75 to 79" ~ "75 - <80",
    age_group_name == "80 to 84" ~ "80 - <85",
    age_group_name == "85+"      ~ "85+"
  ))

# Select columns we want
df_age_weights <- df_age_weights %>%
  select(c("age_group_name", "age_group_years_start", "age_group_weight_value"))

##----------------------------------------------------------------
## 4. Apply age-standardization
##----------------------------------------------------------------
# Join age weights to data
df_as <- left_join(
  x = df_m,
  y = df_age_weights,
  by = c("age_name" = "age_group_name", "age_group_years_start")
)

# Create age-standardized ratios based on non-sexed GBD age weights
df_as <- df_as %>%
  group_by(sex_id, cause_id, year_id, location_id, location_name, acause, cause_name) %>%
  summarise(
    as_spend_prev_ratio = sum(spend_prev_ratio * age_group_weight_value, na.rm = TRUE),
    as_mort_prev_ratio  = sum(mort_prev_ratio * age_group_weight_value, na.rm = TRUE),
    .groups = "drop"
  )

##----------------------------------------------------------------
## 5. Frontier Analysis Model
##
## Formula - GBD Mortality / Prevalence Ratio is the outcome, DEX spend_mean / prevalence ratio is the predictor (+ other variables)
##----------------------------------------------------------------
# Loop through our causes, create models for each, extract efficiencies 


##------------------------------------
## FRONTIER ANALYSIS USING frontier PACKAGE
## Robustness-check version that mimics the Haley/HSR code logic
## Applied to your df_as (state x year x sex) for acause in c("hiv","_subs")
##------------------------------------

library(data.table)
library(dplyr)
library(glue)
library(frontier)

##------------------------------------------------------------
## 0) Convert to data.table + define causes
##------------------------------------------------------------
dt <- as.data.table(df_as)

# Keep only HIV + SUD aggregate
dt <- dt[acause %in% c("hiv", "_subs")]

# Optional: drop rows with missing/invalid ratios
dt <- dt[is.finite(as_spend_prev_ratio) &
           is.finite(as_mort_prev_ratio) &
           as_spend_prev_ratio > 0 &
           as_mort_prev_ratio > 0]

##------------------------------------------------------------
## 1) (Optional) Merge in covariates (state-year)
##    cov_df is your "Haley covariates" file:
##    /ihme/resource_tracking/us_value/data/sfa_covars2021_shea.csv
##------------------------------------------------------------
# Make sure cov_df has: location_id, year_id, and covariate columns you want.
# In your script you already read cov_df <- fread(cov_df_path)

setDT(cov_df)

# Example covariate list to mimic published paper’s risk adjustment (edit as needed)
# NOTE: Use the actual column names in cov_df.
covs <- c("adj_exp_pc", "obesity", "age65", "cig_pc_10", "phys_act_10", "edu_yrs")

# Your spending in dt is as_spend_prev_ratio (not spending_adj_pc).
# We'll create a "spending covariate" analogous to their adj_exp_pc.
# Here: adj_exp_pc = standardized spending-per-case measure.
dt[, adj_exp_pc := as_spend_prev_ratio]

# Merge covariates on state-year if you want the same covariate behavior.
# If cov_df has location_name too, this is fine; but location_id/year_id is enough.
dt <- merge(
  dt,
  cov_df[, .(location_id, year_id, obesity, age65, cig_pc_10, phys_act_10, edu_yrs)],
  by = c("location_id", "year_id"),
  all.x = TRUE
)

##------------------------------------------------------------
## 2) Preserve raw spending for direction checks/plotting
##------------------------------------------------------------
dt[, adj_exp_pc_c := adj_exp_pc]  # keep pre-normalized spending

##------------------------------------------------------------
## 3) Standardize covariates (z-score), like the published code
##    (including spending covariate itself)
##------------------------------------------------------------
zscore_inplace <- function(DT, cols) {
  for (cc in cols) {
    if (cc %in% names(DT)) {
      mu <- mean(DT[[cc]], na.rm = TRUE)
      sdv <- sd(DT[[cc]], na.rm = TRUE)
      if (is.finite(sdv) && sdv > 0) {
        set(DT, j = cc, value = (DT[[cc]] - mu) / sdv)
      }
    }
  }
  invisible(DT)
}

# Standardize covariates; note that some may be NA for some rows (handle upstream if needed)
zscore_inplace(dt, unique(c(covs)))

# Sort by spending (important in their code)
setorder(dt, adj_exp_pc)

##------------------------------------------------------------
## 4) Rescale outcome to be positive (Haley code does this)
##    In the robustness script, the outcome is a "demeaned draw"
##    and can be negative. In your case, as_mort_prev_ratio is
##    already positive, so this step isn't strictly necessary,
##    but we do it anyway to follow the same “magnitude control.”
##------------------------------------------------------------
rescale_to_mean_sd <- function(x, target_mean = 0.6, target_sd = 0.1) {
  scaled <- (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
  scaled * target_sd + target_mean
}

# We'll create an analysis outcome column "y"
# (so we don't overwrite as_mort_prev_ratio)
dt[, y := rescale_to_mean_sd(as_mort_prev_ratio, target_mean = 0.6, target_sd = 0.1)]

if (dt[y < 0, .N] > 0) stop("Some output y is negative after rescaling; unexpected.")

##------------------------------------------------------------
## 5) Fit SFA function that mimics the published robustness code
##    - Uses level-scale outcome (y), not log
##    - ineffDecrease = FALSE: inefficiency increases y (worse outcomes)
##    - Creates prediction and "null_prediction" with non-spending covs zeroed out
##    - Computes y_adj (outcome net of covariate effects)
##    - If spending direction is increasing, flatten predicted line (hack)
##    - ineff = max(0, y - prediction), scaled 0–1
##------------------------------------------------------------


fit_frontier_haley_style <- function(DT, acause_name,
                                     covs,
                                     spending_cov = "adj_exp_pc",
                                     return_fig = FALSE) {
  
  df <- copy(DT[acause == acause_name])
  
  # Keep only rows with all required covariates (optional)
  # You can loosen this if you prefer imputation instead.
  needed <- unique(c("y", "location_id", "year_id", "location_name", "acause", spending_cov, "adj_exp_pc_c", covs))
  needed <- needed[needed %in% names(df)]
  df <- df[complete.cases(df[, ..needed])]
  
  if (nrow(df) < 30) stop(glue("Too few rows for {acause_name}: {nrow(df)}"))
  
  # Formula: y ~ covs (covs includes spending cov)
  rhs <- paste(covs, collapse = " + ")
  fml <- as.formula(glue("y ~ {rhs}"))
  
  # Fit SFA
  # ineffDecrease=FALSE: inefficiency increases y
  model <- frontier::sfa(
    formula = fml,
    data = df,
    ineffDecrease = FALSE,
    restartMax = 5,
    restartFactor = 0.01
  )
  
  # Predict at observed points
  df[, prediction := predict(model, asInData = TRUE, logDepVar = FALSE)]
  
  # Null prediction: set all covariates except spending to 0 (like Haley code)
  df_null <- copy(df)
  null_covs <- setdiff(covs, spending_cov)
  for (cc in null_covs) df_null[, (cc) := 0.0]
  
  df_null[, null_prediction := predict(model, newdata = df_null, logDepVar = FALSE)]
  
  # Merge null_prediction back (same row order should match, but merge is safe)
  df <- merge(
    df,
    df_null[, .(location_id, year_id, null_prediction)],
    by = c("location_id", "year_id"),
    all.x = TRUE,
    sort = FALSE
  )
  
  # Covariate effect
  df[, cov_hat := prediction - null_prediction]
  
  # Outcome adjusted for covariate effects
  df[, y_adj := y - cov_hat]
  
  ##------------------------------------------------------------
  ## Can't impose monotone non-increasing frontier in frontier::sfa
  ## So mimic the paper's robustness hack:
  ## If the null_prediction increases with spending, flatten it.
  ##------------------------------------------------------------
  check_dir <- suppressWarnings(cor(df$adj_exp_pc_c, df$null_prediction, use = "complete.obs"))
  
  if (is.finite(check_dir) && check_dir >= 0) {
    
    # Only safe when you have ONE predictor effectively driving null_prediction,
    # but we mimic their logic.
    min_val <- min(df$null_prediction, na.rm = TRUE)
    min_val2 <- min(df$prediction, na.rm = TRUE)
    
    # If model has multiple predictors and prediction != null_prediction,
    # flattening both may be misleading; we keep their behavior but warn.
    if (abs(min_val - min_val2) > 1e-8) {
      warning(glue("{acause_name}: multiple predictors may be active; flattening both predictions anyway (Haley-style)."))
    }
    
    df[, `:=`(prediction = min_val, null_prediction = min_val)]
  }
  
  ##------------------------------------------------------------
  ## Define inefficiency as vertical distance above predicted
  ## (predicted is the 'frontier'/best-practice benchmark here)
  ##------------------------------------------------------------
  df[, ineff_raw := y - prediction]
  df[ineff_raw < 0, ineff_raw := 0]
  
  # Scale 0–1 (like their code)
  rng <- range(df$ineff_raw, na.rm = TRUE)
  if (diff(rng) == 0) {
    df[, ineff := 0]
  } else {
    df[, ineff := (ineff_raw - rng[1]) / (rng[2] - rng[1])]
  }
  
  # Output table similar to the robustness script
  out <- df[, .(
    location_name, location_id, year_id, sex_id, acause, cause_name,
    adj_exp_pc, adj_exp_pc_c,
    mort_prev_ratio_as = as_mort_prev_ratio,
    y_rescaled = y,
    y_adj = y_adj,
    y_adj_hat = null_prediction,
    ineff, ineff_raw
  )]
  
  if (return_fig) {
    # Simple plot: adjusted outcome vs spending with fitted null frontier
    library(ggplot2)
    p <- ggplot(df, aes(x = adj_exp_pc_c)) +
      geom_point(aes(y = y_adj), alpha = 0.35, color = "#2678B2") +
      geom_line(aes(y = null_prediction), color = "orange", linewidth = 1.1) +
      theme_minimal() +
      labs(
        title = glue("Haley-style frontier robustness: {acause_name}"),
        x = "Spending per case (raw scale)",
        y = "Adjusted outcome (y_adj) + fitted line (null_prediction)"
      )
    return(list(model = model, out = out, plot = p))
  }
  
  return(list(model = model, out = out))
}

##------------------------------------------------------------
## 6) Run for HIV and SUD and save outputs
##------------------------------------------------------------
results <- list()

for (cau in c("hiv", "_subs")) {
  message("Running Haley-style frontier for: ", cau)
  
  res <- fit_frontier_haley_style(
    DT = dt,
    acause_name = cau,
    covs = covs,
    spending_cov = "adj_exp_pc",
    return_fig = TRUE
  )
  
  results[[cau]] <- res
  
  # Save model
  saveRDS(res$model, file.path(dir_output, glue("haley_frontier_{cau}_model.rds")))
  
  # Save output table
  fwrite(res$out, file.path(dir_output, glue("haley_frontier_{cau}_ineff.csv")))
  
  # Save plot
  ggsave(
    filename = file.path(dir_output, glue("haley_frontier_{cau}_plot.png")),
    plot = res$plot,
    width = 8, height = 5, dpi = 200
  )
}

# Combined results table (optional)
df_haley <- rbindlist(lapply(results, `[[`, "out"), fill = TRUE)
fwrite(df_haley, file.path(dir_output, "haley_frontier_hiv_sud_combined.csv"))

