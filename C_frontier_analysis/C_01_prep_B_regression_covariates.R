##----------------------------------------------------------------
##' Title: C_01_prep_B_regression_covariates.R
##'
##' Purpose: TBD
##'          
##' Outputs:
##' df_as_processed_rw_gbd.csv
##----------------------------------------------------------------

##----------------------------------------------------------------
## Clear environment and set library paths
##----------------------------------------------------------------
rm(list = ls())
pacman::p_load(data.table, arrow, tidyverse, glue, broom, purrr, readr, lubridate, readxl, e1071)
conflicts_prefer(data.table::year)
conflicts_prefer(dplyr::summarize)
conflicts_prefer(dplyr::filter)

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

source(file.path(h, "/repo/Aim1/aim1_scripts/Z_utilities/deflate.R"))

##----------------------------------------------------------------
## 0. Set Boolean variables
## 
## Notes: "rw" variable is always set to T as of 1/26
##
## 3 unique combos,  rw (T), cdc (F), cdc_gbd_mix (F)
##                   rw (T), cdc (T), cdc_gbd_mix (F)
##                   rw (T), cdc (T), cdc_gbd_mix (T)
##----------------------------------------------------------------
rw <- T # Set TRUE if desire RW + DEX / prevalence counts to be the predictor variable (see "Specify Models" section), FALSE if just spend / prev count ratio as predictor
cdc <- F #  USE GBD AS BASE. Section 9.5 below ALSO merges CDC counts (under semantic names) so both GBD and CDC in oneSet TRUE if you want to use CDC HIV prevalence and mortality in the models instead of GBD prev deaths, FALSE if wanting to use GBD data
cdc_gbd_mix <- F # Set T if want to set the outcome ratio to be GBD mort / CDC prev, F if just regular settings

##----------------------------------------------------------------
## 0.1 Functions
##----------------------------------------------------------------
ensure_dir_exists <- function(dir_path) {
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
}

'%nin%' <- Negate('%in%')

##----------------------------------------------------------------
## 1. Set directories
##----------------------------------------------------------------
# Set fp for age-standardized data
as_date <- "20260516"
fp_as <- file.path(h, '/aim_outputs/Aim2/C_frontier_analysis/', as_date, "df_as.csv")

# Set output directories
date_today <- format(Sys.time(), "%Y%m%d")
dir_output <- file.path(h, "/aim_outputs/Aim2/C_frontier_analysis/", date_today)
ensure_dir_exists(dir_output)

# Covariates data
fp_df_cov <- "/ihme/resource_tracking/us_value/data/sfa_covars2021_shea.csv" # H's
fp_df_race_cov <- "/ihme/resource_tracking/us_value/data/sfa_covars_w_race_fractions.csv" # H's
fp_aca_expansion <- file.path(h, "/aim_outputs/Aim2/R_resources/aca_expansion_formatted.csv") # ACA - from online
fp_rw_t1 <- file.path(h, "/aim_outputs/Aim2/R_resources/ryan_white_data/rw_title1.xls") # Marcus
fp_rw_t2 <- file.path(h, "/aim_outputs/Aim2/R_resources/ryan_white_data/rw_title2.xls") # Marcus
fp_rw_2016_2019 <- file.path(h, "aim_outputs/Aim2/R_resources/ryan_white_data/ryan_white_data_2016-2019.csv") # Official RW site

# FIPS table
fp_fips <- file.path(h, "/aim_outputs/Aim2/R_resources/state_county_city_fips.csv")
fp_cityfips <- file.path(h, "/aim_outputs/Aim2/R_resources/ryan_white_data/t1years.xlsx")

##----------------------------------------------------------------
## 2. Read in data
##----------------------------------------------------------------
df_as <- read.csv(fp_as)
df_cov <- read.csv(fp_df_cov)
df_race_cov <- read.csv(fp_df_race_cov)
df_aca_expansion <- read.csv(fp_aca_expansion)

df_rw_t1 <- read_excel(fp_rw_t1)
df_rw_t2 <- read_excel(fp_rw_t2)
df_rw_cityfips <- read_excel(fp_cityfips)
df_rw_2016_2019 <- read.csv(fp_rw_2016_2019)

df_fips <- read.csv(fp_fips)

##----------------------------------------------------------------
## 3. Format ACA Expansion data
##----------------------------------------------------------------
df_aca_expansion_f <- df_aca_expansion %>%
  mutate(
    imp_year_id = case_when(
      Expansion.Implementation.Date %in% c("N/A", NA) ~ NA_integer_,
      TRUE ~ year(mdy(Expansion.Implementation.Date))
    )
  )

df_aca_expansion_f <- df_aca_expansion_f %>%
  mutate(
    imp_2010 = if_else(!is.na(imp_year_id) & imp_year_id <= 2010, 1L, 0L),
    imp_2011 = if_else(!is.na(imp_year_id) & imp_year_id <= 2011, 1L, 0L),
    imp_2012 = if_else(!is.na(imp_year_id) & imp_year_id <= 2012, 1L, 0L),
    imp_2013 = if_else(!is.na(imp_year_id) & imp_year_id <= 2013, 1L, 0L),
    imp_2014 = if_else(!is.na(imp_year_id) & imp_year_id <= 2014, 1L, 0L),
    imp_2015 = if_else(!is.na(imp_year_id) & imp_year_id <= 2015, 1L, 0L),
    imp_2016 = if_else(!is.na(imp_year_id) & imp_year_id <= 2016, 1L, 0L),
    imp_2017 = if_else(!is.na(imp_year_id) & imp_year_id <= 2017, 1L, 0L),
    imp_2018 = if_else(!is.na(imp_year_id) & imp_year_id <= 2018, 1L, 0L),
    imp_2019 = if_else(!is.na(imp_year_id) & imp_year_id <= 2019, 1L, 0L)
  )

df_aca_expansion_f <- df_aca_expansion_f %>%
  pivot_longer(
    cols = starts_with("imp_20"),
    names_to = "year_id",
    names_prefix = "imp_",
    values_to = "imp"
  ) %>%
  mutate(year_id = as.integer(year_id))

df_aca_expansion_f <- df_aca_expansion_f %>%
  setnames(
    old = c("Location", "imp"),
    new = c("location_name", "aca_implemented_status")
  )

df_aca_expansion_f <- df_aca_expansion_f %>%
  select(c("location_name", "year_id", "aca_implemented_status"))

##----------------------------------------------------------------
## 4. Format Ryan White data
##----------------------------------------------------------------
# Title1 City level Ryan White Data #
# Add padded 0 to cityfip column
df_rw_t1$cityfip <- sprintf("%04d", df_rw_t1$cityfip)

# Cityfip codes
# Remove padded spaces from cityfip
df_rw_cityfips$cityfip <- str_trim(df_rw_cityfips$cityfip)

# Merge w/ Ryan White title1 (city)
df_rw_t1_m <- left_join(
  x = df_rw_t1,
  y = df_rw_cityfips,
  by = "cityfip"
)

# Extract state name
df_rw_t1_m$state_abbr <- substr(df_rw_t1_m$city, nchar(df_rw_t1_m$city) - 1, nchar(df_rw_t1_m$city))

# Create full state names
state_lookup <- setNames(state.name, state.abb)
df_rw_t1_m$state_name <- state_lookup[df_rw_t1_m$state_abbr]
df_rw_t1_m$state_name <- if_else(df_rw_t1_m$state_abbr == "DC", "District of Columbia", df_rw_t1_m$state_name)

# Group by summary for whole state
df_rw_t1_m <- df_rw_t1_m %>%
  group_by(year, state_name) %>%
  summarise(
    rw_title1_funding = sum(title1_funding, na.rm = TRUE)
  )

# Filter on 2010 ~ 2019
df_rw_t1_m <- df_rw_t1_m %>%
  filter(year %in% (2010:2019))

df_rw_t1_m <- df_rw_t1_m %>%
  setnames(
    old = c("year", "state_name"),
    new = c("year_id", "location_name")
  )


# Title2 State level Ryan White Data #
# Format FIPS data
df_fips$State.Name <- str_to_title(tolower(df_fips$State.Name))
df_fips_state <- df_fips %>% select(c("State.Name", "State.Code", "State.FIPS.Code")) %>% unique()

df_rw_t2_m <- left_join(
  x = df_rw_t2,
  y = df_fips_state,
  by = c("statefip" = "State.FIPS.Code")
)

df_rw_t2_m <- df_rw_t2_m %>%
  select(c(year, State.Name, title2_funding_annual)) %>%
  filter(year %in% c(2010:2019))

df_rw_t2_m <- df_rw_t2_m %>%
  setnames(
    old = c("year", "State.Name", "title2_funding_annual"),
    new = c("year_id", "location_name", "rw_title2_funding")
  )


# Merge Title1 and Title2 data #
df_rw_t1_m <- df_rw_t1_m %>%
  ungroup() %>%
  mutate(
    year_id = as.integer(year_id),
    location_name = unname(as.character(location_name)),
    location_name = trimws(location_name)
  )

df_rw_t2_m <- df_rw_t2_m %>%
  mutate(
    year_id = as.integer(year_id),
    location_name = as.character(location_name),
    location_name = trimws(location_name)
  )

# Fix naming issue
df_rw_t2_m$location_name <- if_else(df_rw_t2_m$location_name == "District Of Columbia", "District of Columbia", df_rw_t2_m$location_name)

# Join
df_rw_m <- full_join(
  x = df_rw_t1_m,
  y = df_rw_t2_m,
  by = c("year_id", "location_name")
)

# Combine title1 and title2 grant sums
df_rw_m <- df_rw_m %>%
  mutate(
    rw_funding = rowSums(cbind(rw_title1_funding, rw_title2_funding), na.rm = TRUE)
  )


# 2016 - 2019 Ryan White data
df_rw_2016_2019_f <- df_rw_2016_2019 %>%
  filter(HRSA.Program.Area.Name == "HIV/AIDS") %>%
  filter(Grant.Program.Name %in% c("Ryan White Part A HIV Emergency Relief Grant Program (H89)",
                                   "Ryan White Part B HIV Care Grant Program (X07)",
                                   "Ryan White Part B Supplemental (X08)",
                                   "ADAP Shortfall Relief (X09)"
                                )) %>%
  group_by(Award.Year, State.Name) %>%
  summarise(
    `Financial.Assistance` = sum(Financial.Assistance)
  )

df_rw_2016_2019_f <- df_rw_2016_2019_f %>%
  filter(Award.Year %in% c(2016:2019))

df_rw_2016_2019_f <- df_rw_2016_2019_f %>%
  setnames(
    old = c("Award.Year", "State.Name", "Financial.Assistance"),
    new = c("year_id", "location_name", "ryan_white_grant")
  )

# Join to title1 and title2 summed data
df_rw_total <- full_join(
  x = df_rw_m,
  y = df_rw_2016_2019_f,
  by = c("year_id", "location_name")
)

df_rw_total$delta <- (df_rw_total$rw_funding - df_rw_total$ryan_white_grant)

# View(df_rw_total %>% filter(year_id %in% c(2016:2019))) # Checking deltas, it seems some are matching perfectly against Marcus's data, whereas we are off in some rows but unknow why we are off

# Using <=2018 data from Marcus's dataset, 2019 data will come from the official Ryan White grant data
df_rw_total$ryan_white_funding_final <- if_else(df_rw_total$year_id <= 2018, df_rw_total$rw_funding, df_rw_total$ryan_white_grant)

# Inflation adjust RW data before creating (RW Spend + DEX spend_all) / prevalence → RWspend ratio
df_rw_total <- deflate(
  data = df_rw_total,
  val_columns = "ryan_white_funding_final",
  old_year = "year_id",
  new_year = 2019
)

##----------------------------------------------------------------
## 5. Filter & Merge data
##----------------------------------------------------------------
# Covariate Data
df_cov <- df_cov %>%
  filter(year_id %in% c(2010:2019))

df_race_cov <- df_race_cov %>%
  filter(year_id %in% c(2010:2019))

# Merge covariate data
df_cov <- left_join(
  x = df_cov,
  y = df_race_cov
)

df_cov <- left_join(
  x = df_cov,
  y = df_aca_expansion_f
)

# Adds in Ryan White Data
df_cov <- left_join(
  x = df_cov,
  y = df_rw_total %>% select(c("year_id", "location_name", "ryan_white_funding_final")),
  by = c("year_id", "location_name")
)

if (cdc) {
  # Merge covariate data w/ age-standardized data
  df_as <- left_join(
    x = df_as_cdc,
    y = df_cov,
    by = c("year_id", "location_id", "location_name")
  )
} else {
  # Merge covariate data w/ age-standardized data
  df_as <- left_join(
    x = df_as,
    y = df_cov,
    by = c("year_id", "location_id", "location_name")
  )
}

##----------------------------------------------------------------
## 6. Create "high_prev" variable to use as an interactive term in respective models (HIV models use HIV high_prev, SUD models use SUD high_prev)
##----------------------------------------------------------------
# Create sud_prevalence_counts column
df_sud_prev_count <- df_as %>% 
  filter(acause == "_subs") %>%
  select(c("cause_id", "year_id", "location_id", "location_name", "acause", 
           "cause_name", "prevalence_counts")) %>%
  setnames(old = "prevalence_counts", new = "sud_prevalence_counts")

df_sud_prev_count <- df_sud_prev_count %>%
  mutate(high_sud_prev = ifelse(sud_prevalence_counts >= median(sud_prevalence_counts), 1, 0))

# Create alcohol_prevalence_counts column ####################
df_alcohol_prev_count <- df_as %>% 
  filter(acause == "mental_alcohol") %>%
  select(c("cause_id", "year_id", "location_id", "location_name", "acause", 
           "cause_name", "prevalence_counts")) %>%
  setnames(old = "prevalence_counts", new = "alcohol_prevalence_counts")

df_alcohol_prev_count <- df_alcohol_prev_count %>%
  mutate(high_alcohol_prev = ifelse(alcohol_prevalence_counts >= median(alcohol_prevalence_counts), 1, 0))

# Create opioid_prevalence_counts column
df_opioid_prev_count <- df_as %>% 
  filter(acause == "mental_drug_opioids") %>%
  select(c("cause_id", "year_id", "location_id", "location_name", "acause", 
           "cause_name", "prevalence_counts")) %>%
  setnames(old = "prevalence_counts", new = "opioid_prevalence_counts")

df_opioid_prev_count <- df_opioid_prev_count %>%
  mutate(high_opioid_prev = ifelse(opioid_prevalence_counts >= median(opioid_prevalence_counts), 1, 0))

# Create hiv_prevalence_counts column
if (cdc) { # use CDC hiv prevalence data
  df_hiv_prev_count <- df_as %>% 
    filter(acause == "hiv") %>%
    select(c("cause_id", "year_id", "location_id", "location_name", "acause", 
             "cause_name", "cdc_hiv_prevalence_counts"))
  
  df_hiv_prev_count <- df_hiv_prev_count %>%
    mutate(high_cdc_hiv_prev = ifelse(cdc_hiv_prevalence_counts >= median(cdc_hiv_prevalence_counts), 1, 0)) %>%
    select(!c("cdc_hiv_prevalence_counts"))
} else { # else use GBD hiv prevalence data
  df_hiv_prev_count <- df_as %>% 
    filter(acause == "hiv") %>%
    select(c("cause_id", "year_id", "location_id", "location_name", "acause", 
             "cause_name", "prevalence_counts")) %>%
    setnames(old = "prevalence_counts", new = "hiv_prevalence_counts")
  
  df_hiv_prev_count <- df_hiv_prev_count %>%
    mutate(high_hiv_prev = ifelse(hiv_prevalence_counts >= median(hiv_prevalence_counts), 1, 0))
}

# Join
df_as <- left_join(
  x = df_as,
  y = df_sud_prev_count %>% select(!c("acause", "cause_id", "cause_name")),
  by = c("year_id", "location_id", "location_name")
)

df_as <- left_join(
  x = df_as,
  y = df_hiv_prev_count %>% select(!c("acause", "cause_id", "cause_name")),
  by = c("year_id", "location_id", "location_name")
)

df_as <- left_join(
  x = df_as,
  y = df_alcohol_prev_count %>% select(!c("acause", "cause_id", "cause_name")),
  by = c("year_id", "location_id", "location_name")
)

df_as <- left_join(
  x = df_as,
  y = df_opioid_prev_count %>% select(!c("acause", "cause_id", "cause_name")),
  by = c("year_id", "location_id", "location_name")
)

##----------------------------------------------------------------
## 7. Create Ryan White HIV prevalence ratios
## # HIV - ryan_white_grant / HIV prevalence count
## # HIV - ryan_white_grant + (spend_all from DEX data) / HIV prevalence count
## # ONLY used in HIV models
##----------------------------------------------------------------
# Create ratios
if (cdc) {
  df_as$rw_cdc_hiv_prev_ratio <- df_as$ryan_white_funding_final / df_as$cdc_hiv_prevalence_counts
  df_as$rw_cdc_dex_hiv_prev_ratio <- (df_as$ryan_white_funding_final + df_as$spend_all) / df_as$cdc_hiv_prevalence_counts
} else {
  df_as$rw_hiv_prev_ratio <- df_as$ryan_white_funding_final / df_as$hiv_prevalence_counts
  df_as$rw_dex_hiv_prev_ratio <- (df_as$ryan_white_funding_final + df_as$spend_all) / df_as$hiv_prevalence_counts
}

# Add the custom ratio ONLY used in HIV
# - RW + DEX spend / CDC prev (have this already)
# - GBD mortality / CDC prevalence (created below)
if (cdc_gbd_mix) {
  df_as$as_cdc_mort_prev_ratio <- df_as$mortality_counts / df_as$cdc_hiv_prevalence_counts
}

################SAVINF
##----------------------------------------------------------------
## SECTION TO ADD AT END OF DATA PROCESSING
## (Replace sections 8 and 9 with this)
##----------------------------------------------------------------

##----------------------------------------------------------------
## 8. DO NOT Factor location_id and year_id here
##    Keep them numeric for variance decomposition and panel modeling
##    Only factor inside formulas when needed for FE models
##----------------------------------------------------------------
# REMOVED: df_as$year_id <- as.factor(df_as$year_id)
# REMOVED: df_as$location_id <- as.factor(df_as$location_id)

# Instead, ensure they are numeric and create useful derived variables
df_as <- df_as %>%
  mutate(
    year_id = as.numeric(as.character(year_id)),
    location_id = as.numeric(as.character(location_id)),
    # Centered year for time trends (midpoint of 2010-2019)
    year_centered = year_id - 2014.5
  )

##----------------------------------------------------------------
## 9. Log transformations - EXPLICIT variable naming
##    Don't use grep() - explicitly name what you transform
##----------------------------------------------------------------

# Define variables to log-transform EXPLICITLY
# Outcomes
outcome_vars_to_log <- c(
  
  "as_mort_prev_ratio",
  "as_daly_prev_ratio", 
  "as_yll_prev_ratio"
)

# Predictors  
predictor_vars_to_log <- c(
  "as_spend_prev_ratio",
  "rw_dex_hiv_prev_ratio",
  "rw_hiv_prev_ratio"
)

# CDC versions (if using CDC data)
if (cdc) {
  predictor_vars_to_log <- c(predictor_vars_to_log,
                             "rw_cdc_hiv_prev_ratio",
                             "rw_cdc_dex_hiv_prev_ratio",
                             "as_cdc_spend_prev_ratio")
  outcome_vars_to_log <- c(outcome_vars_to_log, "as_cdc_mort_prev_ratio")
}

# Covariates that need logging (skewed)
covariate_vars_to_log <- c(
  "phys_act_10",
  "sud_prevalence_counts"
)

# Combine all vars to log
all_vars_to_log <- c(outcome_vars_to_log, predictor_vars_to_log, covariate_vars_to_log)

# Filter to only vars that exist in the data
vars_to_log <- all_vars_to_log[all_vars_to_log %in% names(df_as)]

# Create BOTH logged and unlogged versions
# Keep originals for diagnostics, create _log versions for modeling
df_as <- df_as %>%
  mutate(across(
    all_of(vars_to_log),
    list(log = ~log1p(.x)),
    .names = "{.col}_log"
  ))

# Print confirmation of what was logged
cat("\n=== Variables log-transformed (originals preserved, _log versions created) ===\n")
cat(paste(vars_to_log, collapse = "\n"))
cat("\n")

##----------------------------------------------------------------
## 9.5 Per-capita variables, logs, lags, Mundlak — POST-MAY-6-COMMITTEE
##     Adds regression-ready columns used by the per-capita scripts
##     (C_04_HIV_per_capita_models.R, C_05_HIV_per_capita_cascade.R).
##     Old per-case scripts (C_02, C_03) ignore these new columns.
##
##     CAVEAT on Mundlak: _B / _W are computed over the FULL panel
##     (all states, all years 2010-2019). If a future model fits on
##     a sub-sample (e.g., filtered years), recompute on the subset.
##----------------------------------------------------------------

safe_log <- function(x) if_else(x > 0, log(x), NA_real_)

## --- 9.5.1 Merge CDC HIV mortality + prevalence counts (if available) ---
fp_cdc_proc <- file.path(h, "/aim_outputs/Aim2/C_frontier_analysis/", as_date, "df_as_cdc.csv")
if (file.exists(fp_cdc_proc)) {
  df_as_cdc_raw <- read.csv(fp_cdc_proc, stringsAsFactors = FALSE)
  df_cdc_point  <- df_as_cdc_raw %>%
    dplyr::filter(acause == "hiv") %>%
    dplyr::group_by(location_id, year_id) %>%
    dplyr::summarise(
      cdc_hiv_mortality_counts  = median(cdc_hiv_mortality_counts,  na.rm = TRUE),
      cdc_hiv_prevalence_counts = median(cdc_hiv_prevalence_counts, na.rm = TRUE),
      .groups = "drop"
    )
  df_as <- df_as %>% dplyr::left_join(df_cdc_point, by = c("location_id", "year_id"))
  cat("CDC counts merged into df_as: ",
      sum(!is.na(df_as$cdc_hiv_mortality_counts)),
      " rows with cdc_hiv_mortality_counts\n", sep = "")
} else {
  message("df_as_cdc.csv not found at: ", fp_cdc_proc,
          " — cdc_hiv_mortality_counts / cdc_hiv_prevalence_counts will be NA.")
  df_as$cdc_hiv_mortality_counts  <- NA_real_
  df_as$cdc_hiv_prevalence_counts <- NA_real_
}

## --- 9.5.2 Per-capita / per-100k variables ---
df_as <- df_as %>%
  mutate(
    spending_per_capita       = (ryan_white_funding_final + spend_all) / dex_pop,
    daly_per_capita           = daly_counts      / dex_pop,
    mortality_per_capita      = mortality_counts / dex_pop,
    cdc_mortality_per_capita  = cdc_hiv_mortality_counts  / dex_pop,
    incidence_per_100k        = (incidence_counts  / dex_pop) * 1e5,
    prevalence_per_100k       = (prevalence_counts / dex_pop) * 1e5,
    cdc_prevalence_per_100k   = (cdc_hiv_prevalence_counts / dex_pop) * 1e5
  )

## --- 9.5.3 Logs of per-capita + a couple of covariates ---
df_as <- df_as %>%
  mutate(
    log_spending_per_capita       = safe_log(spending_per_capita),
    log_daly_per_capita           = safe_log(daly_per_capita),
    log_mortality_per_capita      = safe_log(mortality_per_capita),
    log_cdc_mortality_per_capita  = safe_log(cdc_mortality_per_capita),
    log_incidence_per_100k        = safe_log(incidence_per_100k),
    log_prevalence_per_100k       = safe_log(prevalence_per_100k),
    log_cdc_prevalence_per_100k   = safe_log(cdc_prevalence_per_100k),
    log_prop_homeless             = safe_log(prop_homeless),
    log_ldi_pc                    = safe_log(ldi_pc),
    year_factor                   = factor(year_id)
  )

## --- 9.5.4 TOC-level per-capita spending (if TOC columns are present) ---
toc_cols <- c("spend_AM","spend_ED","spend_HH","spend_IP","spend_NF","spend_RX")
if (all(toc_cols %in% names(df_as))) {
  df_as <- df_as %>%
    mutate(
      spend_pharma_pc      = spend_RX / dex_pop,
      spend_ambulatory_pc  = spend_AM / dex_pop,
      spend_inpatient_pc   = spend_IP / dex_pop,
      spend_nf_pc          = spend_NF / dex_pop,
      spend_ed_pc          = spend_ED / dex_pop,
      spend_hh_pc          = spend_HH / dex_pop,
      log_spend_pharma_pc      = safe_log(spend_pharma_pc),
      log_spend_ambulatory_pc  = safe_log(spend_ambulatory_pc),
      log_spend_inpatient_pc   = safe_log(spend_inpatient_pc),
      log_spend_nf_pc          = safe_log(spend_nf_pc),
      log_spend_ed_pc          = safe_log(spend_ed_pc),
      log_spend_hh_pc          = safe_log(spend_hh_pc)
    )
} else {
  message("TOC columns not all present — per-TOC per-capita variables skipped.")
}

## --- 9.5.5 Lag variables (sorted by state, year) ---
##     Covers per-capita lags (consumed by C_04) and per-case lags
##     (rw_dex_hiv_prev_ratio_log_l1/_l2, consumed by C_02 lag specs).
df_as <- df_as %>%
  arrange(location_id, year_id) %>%
  group_by(location_id) %>%
  mutate(
    # Per-capita (C_04)
    log_spending_per_capita_l1 = dplyr::lag(log_spending_per_capita, 1),
    log_spending_per_capita_l2 = dplyr::lag(log_spending_per_capita, 2),
    log_prevalence_per_100k_l1 = dplyr::lag(log_prevalence_per_100k, 1),
    log_prevalence_per_100k_l2 = dplyr::lag(log_prevalence_per_100k, 2),
    log_incidence_per_100k_l1  = dplyr::lag(log_incidence_per_100k,  1),
    # Per-case (C_02)
    rw_dex_hiv_prev_ratio_log_l1 = dplyr::lag(rw_dex_hiv_prev_ratio_log, 1),
    rw_dex_hiv_prev_ratio_log_l2 = dplyr::lag(rw_dex_hiv_prev_ratio_log, 2)
  ) %>%
  ungroup()

## --- 9.5.6 Mundlak B/W decomposition over the full panel ---
##     Standard convention: _B = mean(X), _W = X - mean(X). When X is
##     already log-transformed (e.g. log_spending_per_capita), _B is
##     mean-of-log; this is the convention C_04 uses.
##
##     Caveat: _B / _W computed over the FULL panel. If a future model
##     fits on a sub-sample, recompute on the subset.
mundlak_vars <- c(
  # Per-capita exposures (C_04)
  "log_spending_per_capita", "log_incidence_per_100k", "log_prevalence_per_100k",
  # Per-case exposure (C_02)
  "rw_dex_hiv_prev_ratio_log",
  # Shared raw covariates
  "race_prop_BLCK", "race_prop_HISP",
  "incidence_rates", "bmi", "obesity", "prev_diabetes",
  "aca_implemented_status", "edu_yrs",
  "mortality_rates", "prevalence_rates",
  "prop_homeless", "ldi_pc", "unemployment_rate"
)
mundlak_vars <- intersect(mundlak_vars, names(df_as))
for (v in mundlak_vars) {
  df_as <- df_as %>%
    group_by(location_id) %>%
    mutate(
      !!paste0(v, "_B") := mean(.data[[v]], na.rm = TRUE),
      !!paste0(v, "_W") := .data[[v]] - mean(.data[[v]], na.rm = TRUE)
    ) %>%
    ungroup()
}

## --- 9.5.7 C_02 log-of-state-mean derived variables ---
##     For variables that C_02 logs AFTER taking the state mean, compute
##     the log-of-mean version under the SAME names C_02 uses today
##     (log_X_B, log_X_W). Matches C_02's existing convention so its
##     formulas don't need updating.
##
##     log_X_B = log(X_B) = log(mean(X))
##     log_X_W = log(X) - log_X_B
##
##     For log_prop_homeless specifically: both conventions are needed.
##     This block produces the log-of-mean version (consumed by C_02).
##     The mean-of-log version (consumed by C_04) is produced in 9.5.8.
c02_log_vars <- intersect(
  c("incidence_rates", "bmi", "prev_diabetes", "prop_homeless", "ldi_pc"),
  mundlak_vars
)
for (v in c02_log_vars) {
  log_var <- paste0("log_", v)
  log_B   <- paste0("log_", v, "_B")
  log_W   <- paste0("log_", v, "_W")
  # log_X (time-varying) may already exist from section 9.5.3
  if (!log_var %in% names(df_as)) {
    df_as[[log_var]] <- safe_log(df_as[[v]])
  }
  df_as[[log_B]] <- safe_log(df_as[[paste0(v, "_B")]])
  df_as[[log_W]] <- df_as[[log_var]] - df_as[[log_B]]
}

## --- 9.5.8 Mean-of-log Mundlak variants (_MLB / _MLW) for C_04 ---
##     For variables where C_02 claimed the _B / _W slot with log-of-mean
##     (above in 9.5.7), provide the mean-of-log variant under _MLB / _MLW
##     so C_04's per-capita Mundlak specs can use it.
##
##     Currently only log_prop_homeless requires this dual treatment;
##     the per-capita exposures (log_spending_per_capita, etc.) are not
##     used by C_02 and keep their default _B / _W as mean-of-log.
ml_log_vars <- intersect(c("log_prop_homeless"), names(df_as))
for (v in ml_log_vars) {
  df_as <- df_as %>%
    group_by(location_id) %>%
    mutate(
      !!paste0(v, "_MLB") := mean(.data[[v]], na.rm = TRUE),
      !!paste0(v, "_MLW") := .data[[v]] - mean(.data[[v]], na.rm = TRUE)
    ) %>%
    ungroup()
}

## --- 9.5.9 Indicators (high_incidence_q4_B, high_hiv_prev_B[_f]) ---
##     Consumed by C_02's interaction specs and high-incidence quartile
##     interactions. State-level binaries derived from state means.
if ("incidence_rates_B" %in% names(df_as)) {
  q75_inc <- quantile(df_as$incidence_rates_B, 0.75, na.rm = TRUE)
  df_as$high_incidence_q4_B <- as.integer(df_as$incidence_rates_B >= q75_inc)
}
if ("prevalence_rates_B" %in% names(df_as)) {
  med_prev_B <- quantile(df_as$prevalence_rates_B, 0.50, na.rm = TRUE)
  df_as$high_hiv_prev_B <- as.integer(df_as$prevalence_rates_B >= med_prev_B)
} else if ("prevalence_counts" %in% names(df_as)) {
  # Fallback: median split of state-mean HIV prevalence counts
  state_prev_hiv <- df_as %>%
    dplyr::filter(acause == "hiv") %>%
    dplyr::group_by(location_id) %>%
    dplyr::summarise(prev_count_mean = mean(prevalence_counts, na.rm = TRUE),
                     .groups = "drop")
  prev_median <- median(state_prev_hiv$prev_count_mean, na.rm = TRUE)
  state_prev_hiv <- state_prev_hiv %>%
    mutate(high_hiv_prev_B = as.integer(prev_count_mean >= prev_median)) %>%
    select(location_id, high_hiv_prev_B)
  df_as <- df_as %>% dplyr::left_join(state_prev_hiv, by = "location_id")
}
if ("high_hiv_prev_B" %in% names(df_as)) {
  df_as$high_hiv_prev_B_f <- factor(
    df_as$high_hiv_prev_B,
    levels = c(0, 1),
    labels = c("Lower prevalence", "Higher prevalence")
  )
}

## --- 9.5.10 Dose-response variables (per-case spending) ---
##     Consumed by C_02's dose family (quadratic, threshold specs).
if ("rw_dex_hiv_prev_ratio_log" %in% names(df_as)) {
  knot_p75 <- quantile(df_as$rw_dex_hiv_prev_ratio_log, 0.75, na.rm = TRUE)
  df_as <- df_as %>%
    mutate(
      rw_dex_hiv_prev_ratio_log_sq = rw_dex_hiv_prev_ratio_log^2,
      over_p75                     = pmax(0, rw_dex_hiv_prev_ratio_log - knot_p75)
    )
}

## --- 9.5.11 Time variables ---
##     year_factor already created in 9.5.3. Add year_centered (midpoint).
df_as <- df_as %>% mutate(year_centered = year_id - 2014.5)

cat("Section 9.5 staging complete. df_as columns: ", ncol(df_as), "\n", sep = "")

##----------------------------------------------------------------
## 10. Data validation checks before saving
##----------------------------------------------------------------
cat("\n=== DATA VALIDATION CHECKS ===\n")

# Check 1: Verify RW + DEX spending identity (for HIV)
if (!cdc) {
  df_check <- df_as %>%
    filter(acause == "hiv") %>%
    mutate(
      # Reconstruct from ratio
      implied_total = rw_dex_hiv_prev_ratio * hiv_prevalence_counts,
      # Actual components
      actual_total = ryan_white_funding_final + spend_all,
      # Difference
      diff_pct = abs(implied_total - actual_total) / actual_total * 100
    )
  
  cat("\nSpending identity check (RW + DEX):\n")
  cat(sprintf("  Max discrepancy: %.2f%%\n", max(df_check$diff_pct, na.rm = TRUE)))
  cat(sprintf("  Mean discrepancy: %.2f%%\n", mean(df_check$diff_pct, na.rm = TRUE)))
  
  if (max(df_check$diff_pct, na.rm = TRUE) > 1) {
    warning("Spending identity discrepancy > 1% detected!")
  }
}

# Check 2: Missingness summary
cat("\nMissingness by year (HIV only):\n")
missing_summary <- df_as %>%
  filter(acause == "hiv") %>%
  group_by(year_id) %>%
  summarize(
    n_states = n(),
    missing_rw = sum(is.na(ryan_white_funding_final)),
    missing_mort_ratio = sum(is.na(as_mort_prev_ratio)),
    missing_spend_ratio = sum(is.na(rw_dex_hiv_prev_ratio)),
    .groups = "drop"
  )
print(missing_summary)

# Check 3: Extreme values (potential outliers)
cat("\nPotential outliers (|z-score| > 3):\n")
df_outliers <- df_as %>%
  filter(acause == "hiv") %>%
  mutate(
    spend_z = (rw_dex_hiv_prev_ratio - mean(rw_dex_hiv_prev_ratio, na.rm = TRUE)) / 
      sd(rw_dex_hiv_prev_ratio, na.rm = TRUE),
    mort_z = (as_mort_prev_ratio - mean(as_mort_prev_ratio, na.rm = TRUE)) / 
      sd(as_mort_prev_ratio, na.rm = TRUE)
  ) %>%
  filter(abs(spend_z) > 3 | abs(mort_z) > 3) %>%
  select(location_name, year_id, rw_dex_hiv_prev_ratio, as_mort_prev_ratio, spend_z, mort_z)

if (nrow(df_outliers) > 0) {
  print(df_outliers)
} else {
  cat("  No extreme outliers detected.\n")
}

# Check 4: Summary statistics for key variables
cat("\nKey variable summary (HIV):\n")
df_as %>%
  filter(acause == "hiv") %>%
  select(as_mort_prev_ratio, rw_dex_hiv_prev_ratio, hiv_prevalence_counts, 
         mortality_counts, spend_all, ryan_white_funding_final) %>%
  summary() %>%
  print()

##----------------------------------------------------------------
## 11. Save processed data
##----------------------------------------------------------------

# Since we're always using RW + GBD (not CDC), simplify the naming
output_suffix <- "rw_gbd"  # Clear label: Ryan White + GBD data


# Save as CSV
output_filename <- glue("df_as_processed_{output_suffix}.csv")
output_filepath <- file.path(dir_output, output_filename)

write_csv(df_as, output_filepath)
cat(sprintf("\n=== Data saved to: %s ===\n", output_filepath))


##----------------------------------------------------------------
## 13. Print data structure for verification
##----------------------------------------------------------------
cat("\n=== Final data structure (df_as) ===\n")
cat(sprintf("Dimensions: %d rows x %d columns\n", nrow(df_as), ncol(df_as)))
cat("\nKey columns:\n")
str(df_as %>% select(
  year_id, location_id, location_name, year_centered,
  acause, 
  as_mort_prev_ratio, as_mort_prev_ratio_log,
  rw_dex_hiv_prev_ratio, rw_dex_hiv_prev_ratio_log,
  hiv_prevalence_counts, mortality_counts,
  spend_all, ryan_white_funding_final,
  high_hiv_prev, high_sud_prev,
  obesity, cig_pc_10, edu_yrs, phys_act_10
))

cat("\n=== DATA PROCESSING COMPLETE ===\n")
cat("Next step: Run C_regression...scripts for modeling\n")








