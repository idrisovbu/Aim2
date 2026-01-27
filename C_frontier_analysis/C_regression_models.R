##----------------------------------------------------------------
##' Title: C_regression_models.R
##'
##' Purpose: Runs regression models to help explore which models have covariates 
##'          that are statistically significant for the frontier analysis
##----------------------------------------------------------------

##----------------------------------------------------------------
## Clear environment and set library paths
##----------------------------------------------------------------
rm(list = ls())
pacman::p_load(data.table, arrow, tidyverse, glue, broom, purrr, readr, lubridate, readxl, e1071)

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
##----------------------------------------------------------------
rw <- T # Set TRUE if desire RW + DEX / prevalence counts to be the predictor variable (see "Specify Models" section), FALSE if just spend / prev count ratio as predictor
cdc <- T # Set TRUE if you want to use CDC HIV prevalence and mortality in the models instead of GBD prev deaths, FALSE if wanting to use GBD data
cdc_gbd_mix <- T # Set T if want to set the outcome ratio to be GBD mort / CDC prev, F if just regular settings

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
as_date <- "20260121"
fp_as <- file.path(h, '/aim_outputs/Aim2/C_frontier_analysis/', as_date, "df_as.csv")
fp_as_cdc <- file.path(h, '/aim_outputs/Aim2/C_frontier_analysis/', as_date, "df_as_cdc.csv")

# Set output directories
date_today <- format(Sys.time(), "%Y%m%d")
dir_output <- file.path(h, "/aim_outputs/Aim2/C_frontier_analysis/", date_today)
ensure_dir_exists(dir_output)

# Covariates data
fp_df_cov <- "/ihme/resource_tracking/us_value/data/sfa_covars2021_shea.csv"
fp_df_race_cov <- "/ihme/resource_tracking/us_value/data/sfa_covars_w_race_fractions.csv"
fp_aca_expansion <- file.path(h, "/aim_outputs/Aim2/R_resources/aca_expansion_formatted.csv")
fp_rw_t1 <- file.path(h, "/aim_outputs/Aim2/R_resources/ryan_white_data/rw_title1.xls")
fp_rw_t2 <- file.path(h, "/aim_outputs/Aim2/R_resources/ryan_white_data/rw_title2.xls")
fp_rw_2016_2019 <- file.path(h, "aim_outputs/Aim2/R_resources/ryan_white_data/ryan_white_data_2016-2019.csv")

# FIPS table
fp_fips <- file.path(h, "/aim_outputs/Aim2/R_resources/state_county_city_fips.csv")
fp_cityfips <- file.path(h, "/aim_outputs/Aim2/R_resources/ryan_white_data/t1years.xlsx")

##----------------------------------------------------------------
## 2. Read in data
##----------------------------------------------------------------
df_as <- read.csv(fp_as)
df_as_cdc <- read.csv(fp_as_cdc)
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

##----------------------------------------------------------------
## 8. Factor location_id and year_id to add as covariates
##----------------------------------------------------------------
df_as$year_id <- as.factor(df_as$year_id)
df_as$location_id <- as.factor(df_as$location_id)

##----------------------------------------------------------------
## 9. Log data - exploration
##----------------------------------------------------------------
# Outcome and predictor ratios
pred_out_log_cols <- grep("ratio", colnames(df_as), value = TRUE)

# Covariate variables to log
cov_log_cols <- c("phys_act_10", "sud_prevalence_counts")

# Log data
df_as <- df_as %>%
  mutate(across(all_of(c(pred_out_log_cols, cov_log_cols)), log1p))

##----------------------------------------------------------------
## 10. Specify Models
##----------------------------------------------------------------
# HIV
if (rw) {
  if (cdc) {
    model_basic_mort <- "as_cdc_mort_prev_ratio ~ rw_cdc_dex_hiv_prev_ratio"
    model_basic_mort_interactive_hiv <- "as_cdc_mort_prev_ratio ~ rw_cdc_dex_hiv_prev_ratio * high_cdc_hiv_prev"
  } else {
    model_basic_mort <- "as_mort_prev_ratio ~ rw_dex_hiv_prev_ratio"
    model_basic_mort_interactive_hiv <- "as_mort_prev_ratio ~ rw_dex_hiv_prev_ratio * high_hiv_prev"
  }
} else {
  if (cdc) {
    model_basic_mort <- "as_cdc_mort_prev_ratio ~ as_cdc_spend_prev_ratio"
    model_basic_mort_interactive_hiv <- "as_cdc_mort_prev_ratio ~ as_cdc_spend_prev_ratio * high_cdc_hiv_prev"
  } else {
    model_basic_mort <- "as_mort_prev_ratio ~ as_spend_prev_ratio"
    model_basic_mort_interactive_hiv <- "as_mort_prev_ratio ~ as_spend_prev_ratio * high_hiv_prev"
  }
}

# SUD
model_basic_mort_sud <- "as_mort_prev_ratio ~ as_spend_prev_ratio"
model_basic_mort_interactive_sud <- "as_mort_prev_ratio ~ as_spend_prev_ratio * high_sud_prev"

# Covariate combinations
cov_h <- c('obesity', 'age65', 'cig_pc_10', 'phys_act_10', 'edu_yrs', 'as_spend_prev_ratio')

cov_sud <- c("sud_prevalence_counts")

# cov_aca_expansion <- c("aca_implemented_status") # UNUSED 

cov_year_loc <- c("year_id", "location_id")

cov_race <- c("race_prop_BLCK", "race_prop_HISP")

cov_risk <- c(
  "obesity",
  "cig_pc_10",
  "phys_act_10"
)

cov_ses <- c(
  "edu_yrs"
)

cov_density <- c(
  "density_l.150",
  "density_g.1000"
)

# HIV - Mort / prev - Formulas
f0_hiv <- as.formula(model_basic_mort)
f1_hiv <- as.formula(paste(model_basic_mort, "+", paste(cov_sud, collapse = " + ")))
# f2_hiv <- as.formula(paste(model_basic_mort, "+", paste(c(cov_sud, cov_aca_expansion), collapse = " + ")))
f3_hiv <- as.formula(paste(model_basic_mort, "+", paste(c(cov_sud, cov_year_loc), collapse = " + ")))
f4_hiv <- as.formula(paste(model_basic_mort, "+", paste(c(cov_sud, cov_year_loc, cov_race), collapse = " + ")))
f5_hiv <- as.formula(paste(model_basic_mort, "+", paste(c(cov_sud, cov_year_loc, cov_race, cov_risk), collapse = " + ")))
f6_hiv <- as.formula(paste(model_basic_mort, "+", paste(c(cov_sud, cov_year_loc, cov_race, cov_risk, cov_ses), collapse = " + ")))
f7_hiv <- as.formula(paste(model_basic_mort, "+", paste(c(cov_sud, cov_year_loc, cov_race, cov_risk, cov_ses, cov_density), collapse = " + ")))
# f8_hiv <- as.formula(paste(model_basic_mort, "+", paste(c(cov_sud, cov_year_loc, cov_race, cov_risk, cov_ses, cov_density, cov_rw), collapse = " + "))) # Ryan White
f9_hiv <- as.formula(paste(model_basic_mort, "+", paste(c(cov_h), collapse = " + ")))

# HIV - Mort / prev w/ interaction term - Formulas
f0_hiv_interact <- as.formula(model_basic_mort_interactive_hiv)
f1_hiv_interact <- as.formula(paste(model_basic_mort_interactive_hiv, "+", paste(cov_sud, collapse = " + ")))
# f2_hiv_interact <- as.formula(paste(model_basic_mort_interactive_hiv, "+", paste(c(cov_sud, cov_aca_expansion), collapse = " + ")))
f3_hiv_interact <- as.formula(paste(model_basic_mort_interactive_hiv, "+", paste(c(cov_sud, cov_year_loc), collapse = " + ")))
f4_hiv_interact <- as.formula(paste(model_basic_mort_interactive_hiv, "+", paste(c(cov_sud, cov_year_loc, cov_race), collapse = " + ")))
f5_hiv_interact <- as.formula(paste(model_basic_mort_interactive_hiv, "+", paste(c(cov_sud, cov_year_loc, cov_race, cov_risk), collapse = " + ")))
f6_hiv_interact <- as.formula(paste(model_basic_mort_interactive_hiv, "+", paste(c(cov_sud, cov_year_loc, cov_race, cov_risk, cov_ses), collapse = " + ")))
f7_hiv_interact <- as.formula(paste(model_basic_mort_interactive_hiv, "+", paste(c(cov_sud, cov_year_loc, cov_race, cov_risk, cov_ses, cov_density), collapse = " + ")))
# f8_hiv_interact <- as.formula(paste(model_basic_mort_interactive_hiv, "+", paste(c(cov_sud, cov_aca_expansion, cov_year_loc, cov_race, cov_risk, cov_ses, cov_density, cov_rw), collapse = " + "))) # Ryan White
f9_hiv_interact <- as.formula(paste(model_basic_mort_interactive_hiv, "+", paste(c(cov_h), collapse = " + ")))

# SUD - Mort / prev - Formulas
f0_sud <- as.formula(model_basic_mort_sud)
f1_sud <- as.formula(paste(model_basic_mort_sud, "+", paste(cov_risk, collapse = " + ")))
# f2_sud <- as.formula(paste(model_basic_mort_sud, "+", paste(c(cov_risk, cov_aca_expansion), collapse = " + ")))
f3_sud <- as.formula(paste(model_basic_mort_sud, "+", paste(c(cov_risk, cov_year_loc), collapse = " + ")))
f4_sud <- as.formula(paste(model_basic_mort_sud, "+", paste(c(cov_risk, cov_year_loc, cov_race), collapse = " + ")))
f5_sud <- as.formula(paste(model_basic_mort_sud, "+", paste(c(cov_risk, cov_year_loc, cov_race, cov_ses), collapse = " + ")))
f6_sud <- as.formula(paste(model_basic_mort_sud, "+", paste(c(cov_risk, cov_year_loc, cov_race, cov_ses, cov_density), collapse = " + ")))
f7_sud <- as.formula(paste(model_basic_mort_sud, "+", paste(c(cov_h), collapse = " + ")))

# SUD - Mort / prev w/ interaction term - Formulas
f0_sud_interact <- as.formula(model_basic_mort_interactive_sud)
f1_sud_interact <- as.formula(paste(model_basic_mort_interactive_sud, "+", paste(cov_risk, collapse = " + ")))
# f2_sud_interact <- as.formula(paste(model_basic_mort_interactive_sud, "+", paste(c(cov_risk, cov_aca_expansion), collapse = " + ")))
f3_sud_interact <- as.formula(paste(model_basic_mort_interactive_sud, "+", paste(c(cov_risk, cov_year_loc), collapse = " + ")))
f4_sud_interact <- as.formula(paste(model_basic_mort_interactive_sud, "+", paste(c(cov_risk, cov_year_loc, cov_race), collapse = " + ")))
f5_sud_interact <- as.formula(paste(model_basic_mort_interactive_sud, "+", paste(c(cov_risk, cov_year_loc, cov_race, cov_ses), collapse = " + ")))
f6_sud_interact <- as.formula(paste(model_basic_mort_interactive_sud, "+", paste(c(cov_risk, cov_year_loc, cov_race, cov_ses, cov_density), collapse = " + ")))
f7_sud_interact <- as.formula(paste(model_basic_mort_interactive_sud, "+", paste(c(cov_h), collapse = " + ")))

# Formula List
list_formulas_hiv <- list(
  m_mort_unadj                               = f0_hiv,
  m_mort_plus_sud                            = f1_hiv,
  #m_mort_plus_aca_expansion                  = f2_hiv,
  m_mort_plus_year_loc                       = f3_hiv,
  m_mort_plus_race                           = f4_hiv,
  m_mort_plus_obesity_cig_phys_ac            = f5_hiv,
  m_mort_plus_edu                            = f6_hiv,
  m_mort_plus_density                        = f7_hiv,
  #m_mort_plus_rw                             = f8_hiv,
  m_mort_haley                               = f9_hiv,
  m_mort_int_unadj                           = f0_hiv_interact,
  m_mort_int_plus_sud                        = f1_hiv_interact,
  #m_mort_int_plus_aca_expansion              = f2_hiv_interact,
  m_mort_int_plus_year_loc                   = f3_hiv_interact,
  m_mort_int_plus_race                       = f4_hiv_interact,
  m_mort_int_plus_obesity_cig_phys_ac        = f5_hiv_interact,
  m_mort_int_plus_edu                        = f6_hiv_interact,
  m_mort_int_plus_density                    = f7_hiv_interact,
  #m_mort_int_plus_rw                         = f8_hiv_interact,
  m_mort_int_haley                           = f9_hiv_interact
)

list_formulas_sud <- list(
  m_mort_unadj                               = f0_sud,
  m_mort_plus_obesity_cig_phys_ac            = f1_sud,
  #m_mort_plus_aca_expansion                  = f2_sud,
  m_mort_plus_year_loc                       = f3_sud,
  m_mort_plus_race                           = f4_sud,
  m_mort_plus_edu                            = f5_sud,
  m_mort_plus_density                        = f6_sud,
  m_mort_haley                               = f7_sud,
  m_mort_int_unadj                           = f0_sud_interact,
  m_mort_int_plus_obesity_cig_phys_ac        = f1_sud_interact,
  #m_mort_int_plus_aca_expansion              = f2_sud_interact,
  m_mort_int_plus_year_loc                   = f3_sud_interact,
  m_mort_int_plus_race                       = f4_sud_interact,
  m_mort_int_plus_edu                        = f5_sud_interact,
  m_mort_int_plus_density                    = f6_sud_interact,
  m_mort_int_haley                           = f7_sud_interact
)


##----------------------------------------------------------------
## 11. Run Models
##----------------------------------------------------------------
list_models <- list()

# HIV 
df_hiv <- df_as %>% filter(acause == "hiv")

for (nm in names(list_formulas_hiv)) {
  list_models[[paste("hiv", nm, sep = "__")]] <- lm(list_formulas_hiv[[nm]], data = df_hiv)
}

# SUD 
if (cdc) {
  print("Skipping SUD models for CDC data run!")
} else {

  df_sud <- df_as %>% filter(acause == "_subs")
  
  for (nm in names(list_formulas_sud)) {
    list_models[[paste("_subs", nm, sep = "__")]] <- lm(list_formulas_sud[[nm]], data = df_sud)
  }
}

##----------------------------------------------------------------
## 12. Extract coefficients from models
##----------------------------------------------------------------
coef_tbl <- imap_dfr(list_models, ~{
  broom::tidy(.x) %>%
    mutate(model_id = .y,
       signif_stars = case_when(
         p.value < 0.001 ~ "***",
         p.value < 0.01  ~ "**",
         p.value < 0.05  ~ "*",
         TRUE            ~ ""
       ),
       signif_label = case_when(
         signif_stars == "***" ~ "p < 0.001",
         signif_stars == "**"  ~ "p < 0.01",
         signif_stars == "*"   ~ "p < 0.05",
         TRUE                  ~ "not significant"
       )) %>%
    tidyr::separate(model_id, into = c("acause", "model_name"), sep = "__", remove = FALSE)
})

metrics_tbl <- imap_dfr(list_models, ~{
  g <- broom::glance(.x)
  tibble(
    model_id = .y,
    n = g$nobs,
    r2 = g$r.squared,
    adj_r2 = g$adj.r.squared,
    aic = AIC(.x),
    bic = BIC(.x),
    sigma = g$sigma
  ) %>%
    tidyr::separate(model_id, into = c("acause", "model_name"), sep = "__", remove = FALSE)
})

##----------------------------------------------------------------
## 13. Save Outputs
##----------------------------------------------------------------
if (rw & cdc & cdc_gbd_mix) {
  # Model Outputs
  write.csv(coef_tbl, file.path(dir_output, "model_coefficients_rw_cdc_gbd_mix.csv"))
  write.csv(metrics_tbl, file.path(dir_output, "model_metrics_rw_cdc_gbd_mix.csv"))
} else {
  if (rw) {
    if (cdc) {
      # Model Outputs
      write.csv(coef_tbl, file.path(dir_output, "model_coefficients_rw_cdc.csv"))
      write.csv(metrics_tbl, file.path(dir_output, "model_metrics_rw_cdc.csv"))
    } else {
      # Model Outputs
      write.csv(coef_tbl, file.path(dir_output, "model_coefficients_rw.csv"))
      write.csv(metrics_tbl, file.path(dir_output, "model_metrics_rw.csv"))
    }
  } else {
    if (cdc) {
      # Model Outputs
      write.csv(coef_tbl, file.path(dir_output, "model_coefficients_cdc.csv"))
      write.csv(metrics_tbl, file.path(dir_output, "model_metrics_cdc.csv"))
    } else {
      # Model Outputs
      write.csv(coef_tbl, file.path(dir_output, "model_coefficients.csv"))
      write.csv(metrics_tbl, file.path(dir_output, "model_metrics.csv"))
    }
  }
}

if (cdc) {
  # DF w/ all covariates
  write.csv(df_as, file.path(dir_output, "df_as_cdc_covariates.csv"))
} else {
  # DF w/ all covariates
  write.csv(df_as, file.path(dir_output, "df_as_covariates.csv"))
}

##----------------------------------------------------------------
## SKEWNESS EXPLORATION - SAFE TO DELETE
##----------------------------------------------------------------
# skew_summary_table <- function(df) {
#   df %>%
#     select(where(is.numeric)) %>%
#     pivot_longer(
#       cols = everything(),
#       names_to = "variable",
#       values_to = "value"
#     ) %>%
#     group_by(variable) %>%
#     summarise(
#       n        = sum(!is.na(value)),
#       mean     = mean(value, na.rm = TRUE),
#       sd       = sd(value, na.rm = TRUE),
#       min      = min(value, na.rm = TRUE),
#       p25      = quantile(value, 0.25, na.rm = TRUE),
#       median   = median(value, na.rm = TRUE),
#       p75      = quantile(value, 0.75, na.rm = TRUE),
#       max      = max(value, na.rm = TRUE),
#       skewness = skewness(value, na.rm = TRUE),
#       .groups = "drop"
#     )
# }
# 
# skew_tbl <- skew_summary_table(df_as %>% filter(acause == "hiv"))
# 
# skew_tbl <- skew_tbl %>%
#   mutate(
#     log_candidate = case_when(
#       skewness > 1  & min > 0 ~ "yes",
#       skewness > 0.5 & min > 0 ~ "maybe",
#       TRUE ~ "no"
#     )
#   )
# 
# skew_tbl <- skew_tbl %>%
#   mutate(
#     skew_interpretation = case_when(
#       skewness < -1              ~ "strong left skew",
#       skewness >= -1 & skewness < -0.5 ~ "moderate left skew",
#       skewness >= -0.5 & skewness <= 0.5 ~ "approximately symmetric",
#       skewness > 0.5 & skewness <= 1     ~ "mild right skew",
#       skewness > 1 & skewness <= 2       ~ "strong right skew",
#       skewness > 2                       ~ "extreme right skew",
#       TRUE ~ NA_character_
#     )
#   )
# 
# skew_tbl <- skew_tbl %>%
#   mutate(
#     transform_recommendation = case_when(
#       skewness > 1 & min > 0  ~ "log recommended",
#       skewness > 0.5 & min > 0 ~ "log optional",
#       TRUE ~ "no transform"
#     )
#   )
# 
# View(skew_tbl %>% select(c("variable", "skewness", "log_candidate", "skew_interpretation", "transform_recommendation")))
# 
# # YES to logging
# # Outcome and predictor ratio variablkes
# 
# # Potential YES log candidates
# # phys_act_10 - this is number of minutes per week of activity
# # sud_prevalence_counts
# 
# # Prevalence Count
# ggplot(df_as %>% filter(acause == "hiv"), aes(x = cdc_hiv_prevalence_counts)) +
#   geom_histogram(bins = 300, fill = "steelblue", color = "white") +
#   labs(
#     title = "Distribution of CDC HIV Prevalence Counts",
#     x = "CDC HIV Prevalence Counts",
#     y = "Frequency"
#   ) +
#   theme_minimal()
# 
# ggplot(df_as %>% filter(acause == "hiv"), aes(x = prevalence_counts)) +
#   geom_histogram(bins = 300) +
#   labs(
#     title = "Distribution of GBD HIV Prevalence Counts",
#     x = "GBD HIV Prevalence Counts",
#     y = "Number of State–Year Observations"
#   ) +
#   theme_minimal()
# 
# # Mortality Count
# ggplot(df_as %>% filter(acause == "hiv"), aes(x = cdc_hiv_mortality_counts)) +
#   geom_histogram(bins = 300, fill = "steelblue", color = "white") +
#   labs(
#     title = "Distribution of CDC HIV Mortality Counts",
#     x = "CDC HIV Mortality Counts",
#     y = "Frequency"
#   ) +
#   theme_minimal()
# 
# ggplot(df_as %>% filter(acause == "hiv"), aes(x = mortality_counts)) +
#   geom_histogram(bins = 300) +
#   labs(
#     title = "Distribution of GBD HIV Mortality Counts",
#     x = "GBD HIV Mortality Counts",
#     y = "Number of State–Year Observations"
#   ) +
#   theme_minimal()


