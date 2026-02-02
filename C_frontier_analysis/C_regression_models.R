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
##
## 3 unique combos,  rw (T), cdc (F), cdc_gbd_mix (F)
##                   rw (T), cdc (T), cdc_gbd_mix (F)
##                   rw (T), cdc (T), cdc_gbd_mix (T)
##----------------------------------------------------------------
rw <- T # Set TRUE if desire RW + DEX / prevalence counts to be the predictor variable (see "Specify Models" section), FALSE if just spend / prev count ratio as predictor
cdc <- F # Set TRUE if you want to use CDC HIV prevalence and mortality in the models instead of GBD prev deaths, FALSE if wanting to use GBD data
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
as_date <- "20260201"
fp_as <- file.path(h, '/aim_outputs/Aim2/C_frontier_analysis/', as_date, "df_as.csv")
fp_as_cdc <- file.path(h, '/aim_outputs/Aim2/C_frontier_analysis/', as_date, "df_as_cdc.csv")

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
    # ORIGINAL MODELS
    model_basic_mort <- "as_mort_prev_ratio ~ rw_dex_hiv_prev_ratio"
    model_basic_mort_interactive_hiv <- "as_mort_prev_ratio ~ rw_dex_hiv_prev_ratio * high_hiv_prev"
    
    # YLL MODELS
    # model_basic_mort <- "as_yll_prev_ratio ~ rw_dex_hiv_prev_ratio"
    # model_basic_mort_interactive_hiv <- "as_yll_prev_ratio ~ rw_dex_hiv_prev_ratio * high_hiv_prev"
    
    # DALY MODELS
    # model_basic_mort <- "as_daly_prev_ratio ~ rw_dex_hiv_prev_ratio"
    # model_basic_mort_interactive_hiv <- "as_daly_prev_ratio ~ rw_dex_hiv_prev_ratio * high_hiv_prev"
    #YLD MODELS
    #model_basic_mort <- "as_yld_prev_ratio ~ rw_dex_hiv_prev_ratio"
    #model_basic_mort_interactive_hiv <- "as_yld_prev_ratio ~ rw_dex_hiv_prev_ratio * high_hiv_prev"
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
## 11.5 STRATIFIED MODELS: URBAN VS RURAL (HIV MORTALITY ONLY)
## 
## Paste this AFTER section 11 (Run Models) and BEFORE section 12
## 
## Set run_urban = TRUE for urban states, FALSE for rural states
## Run twice, clearing console between runs
##----------------------------------------------------------------
# 
# # SET THIS FLAG: TRUE = urban states, FALSE = rural states
# run_urban <- FALSE  # <-- CHANGE THIS TO FALSE FOR RURAL RUN
# 
# # Create urban/rural indicator (based on high-density population proportion)
# df_hiv <- df_hiv %>%
#   mutate(
#     urban = ifelse(density_g.1000 > median(density_g.1000, na.rm = TRUE), 1, 0)
#   )
# 
# # Check distribution
# cat("\n=== URBAN/RURAL DISTRIBUTION ===\n")
# print(table(df_hiv$urban))
# cat("0 = Rural, 1 = Urban\n")
# 
# # Subset based on flag
# if (run_urban) {
#   df_hiv_subset <- df_hiv %>% filter(urban == 1)
#   subset_label <- "URBAN"
# } else {
#   df_hiv_subset <- df_hiv %>% filter(urban == 0)
#   subset_label <- "RURAL"
# }
# 
# cat("\n=============================================\n")
# cat("RUNNING MODELS FOR:", subset_label, "STATES\n")
# cat("N observations:", nrow(df_hiv_subset), "\n")
# cat("=============================================\n\n")
# 
# # Run HIV models on subset (only the interaction models since those are your main ones)
# list_models_stratified <- list()
# 
# for (nm in names(list_formulas_hiv)) {
#   # Only run models with interaction term (your main models)
#   if (grepl("_int_", nm)) {
#     list_models_stratified[[paste("hiv", nm, sep = "__")]] <- lm(list_formulas_hiv[[nm]], data = df_hiv_subset)
#   }
# }
# 
# # Also run the full model without interaction to see main spending effect
# # This is your m_mort_int_plus_density model but without the interaction
# f_main_no_int <- as.formula(
#   "as_mort_prev_ratio ~ rw_dex_hiv_prev_ratio + 
#    sud_prevalence_counts + year_id + location_id + 
#    race_prop_BLCK + race_prop_HISP + 
#    obesity + cig_pc_10 + phys_act_10 + edu_yrs + 
#    density_l.150 + density_g.1000"
# )
# 
# list_models_stratified[["hiv__m_mort_main_effect_only"]] <- lm(f_main_no_int, data = df_hiv_subset)
# 
# ##----------------------------------------------------------------
# ## Extract coefficients for stratified models
# ##----------------------------------------------------------------
# coef_tbl_stratified <- imap_dfr(list_models_stratified, ~{
#   broom::tidy(.x) %>%
#     mutate(model_id = .y,
#            subset = subset_label,
#            signif_stars = case_when(
#              p.value < 0.001 ~ "***",
#              p.value < 0.01  ~ "**",
#              p.value < 0.05  ~ "*",
#              p.value < 0.1   ~ ".",
#              TRUE            ~ ""
#            )) %>%
#     tidyr::separate(model_id, into = c("acause", "model_name"), sep = "__", remove = FALSE)
# })
# 
# metrics_tbl_stratified <- imap_dfr(list_models_stratified, ~{
#   g <- broom::glance(.x)
#   tibble(
#     model_id = .y,
#     subset = subset_label,
#     n = g$nobs,
#     r2 = g$r.squared,
#     adj_r2 = g$adj.r.squared,
#     aic = AIC(.x),
#     bic = BIC(.x),
#     sigma = g$sigma
#   ) %>%
#     tidyr::separate(model_id, into = c("acause", "model_name"), sep = "__", remove = FALSE)
# })
# 
# ##----------------------------------------------------------------
# ## Print key results
# ##----------------------------------------------------------------
# cat("\n=============================================\n")
# cat("KEY SPENDING COEFFICIENTS -", subset_label, "STATES\n")
# cat("=============================================\n\n")
# 
# # Filter to just spending-related coefficients
# spending_coefs <- coef_tbl_stratified %>%
#   filter(grepl("rw_dex|high_hiv_prev", term)) %>%
#   select(model_name, term, estimate, std.error, p.value, signif_stars) %>%
#   arrange(model_name, term)
# 
# print(spending_coefs, n = 50)
# 
# cat("\n\n=============================================\n")
# cat("MODEL FIT METRICS -", subset_label, "STATES\n")
# cat("=============================================\n\n")
# 
# print(metrics_tbl_stratified %>% select(model_name, n, r2, adj_r2, aic))
# 
# cat("\n\n=============================================\n")
# cat("MAIN EFFECT ONLY MODEL (no interaction) -", subset_label, "\n")
# cat("=============================================\n\n")
# 
# # Print the main effect model summary
# main_effect_coef <- coef_tbl_stratified %>%
#   filter(model_name == "m_mort_main_effect_only") %>%
#   filter(term == "rw_dex_hiv_prev_ratio") %>%
#   select(term, estimate, std.error, p.value, signif_stars)
# 
# print(main_effect_coef)
# 
# cat("\nInterpretation: In", subset_label, "states, a 1-unit increase in log(spending/prevalence)\n")
# cat("is associated with a", round(main_effect_coef$estimate, 6), "change in log(mortality/prevalence)\n")
# cat("p-value:", format(main_effect_coef$p.value, digits = 4), main_effect_coef$signif_stars, "\n")
# 
# ##----------------------------------------------------------------
# ## Save stratified outputs
# ##----------------------------------------------------------------
# write.csv(coef_tbl_stratified, file.path(dir_output, paste0("model_coefficients_rw_", tolower(subset_label), ".csv")), row.names = FALSE)
# write.csv(metrics_tbl_stratified, file.path(dir_output, paste0("model_metrics_rw_", tolower(subset_label), ".csv")), row.names = FALSE)
# 
# cat("\n\nOutputs saved to:", dir_output, "\n")
# cat("  - model_coefficients_rw_", tolower(subset_label), ".csv\n", sep = "")
# cat("  - model_metrics_rw_", tolower(subset_label), ".csv\n", sep = "")
# ##----------------------------------------------------------------
# ## 12. Extract coefficients from models
# ##----------------------------------------------------------------
# coef_tbl <- imap_dfr(list_models, ~{
#   broom::tidy(.x) %>%
#     mutate(model_id = .y,
#        signif_stars = case_when(
#          p.value < 0.001 ~ "***",
#          p.value < 0.01  ~ "**",
#          p.value < 0.05  ~ "*",
#          TRUE            ~ ""
#        ),
#        signif_label = case_when(
#          signif_stars == "***" ~ "p < 0.001",
#          signif_stars == "**"  ~ "p < 0.01",
#          signif_stars == "*"   ~ "p < 0.05",
#          TRUE                  ~ "not significant"
#        )) %>%
#     tidyr::separate(model_id, into = c("acause", "model_name"), sep = "__", remove = FALSE)
# })
# 
# metrics_tbl <- imap_dfr(list_models, ~{
#   g <- broom::glance(.x)
#   tibble(
#     model_id = .y,
#     n = g$nobs,
#     r2 = g$r.squared,
#     adj_r2 = g$adj.r.squared,
#     aic = AIC(.x),
#     bic = BIC(.x),
#     sigma = g$sigma
#   ) %>%
#     tidyr::separate(model_id, into = c("acause", "model_name"), sep = "__", remove = FALSE)
# })
# 
# ##----------------------------------------------------------------
# ## 13. Save Outputs
# ##----------------------------------------------------------------
# if (rw & cdc & cdc_gbd_mix) {
#   # Model Outputs
#   write.csv(coef_tbl, file.path(dir_output, "model_coefficients_rw_cdc_gbd_mix.csv"))
#   write.csv(metrics_tbl, file.path(dir_output, "model_metrics_rw_cdc_gbd_mix.csv"))
# } else {
#   if (rw) {
#     if (cdc) {
#       # Model Outputs
#       write.csv(coef_tbl, file.path(dir_output, "model_coefficients_rw_cdc.csv"))
#       write.csv(metrics_tbl, file.path(dir_output, "model_metrics_rw_cdc.csv"))
#     } else {
#       # Model Outputs
#       write.csv(coef_tbl, file.path(dir_output, "model_coefficients_rw.csv"))
#       write.csv(metrics_tbl, file.path(dir_output, "model_metrics_rw.csv"))
#     }
#   } else {
#     if (cdc) {
#       # Model Outputs
#       write.csv(coef_tbl, file.path(dir_output, "model_coefficients_cdc.csv"))
#       write.csv(metrics_tbl, file.path(dir_output, "model_metrics_cdc.csv"))
#     } else {
#       # Model Outputs
#       write.csv(coef_tbl, file.path(dir_output, "model_coefficients.csv"))
#       write.csv(metrics_tbl, file.path(dir_output, "model_metrics.csv"))
#     }
#   }
# }
# 
# if (cdc) {
#   # DF w/ all covariates
#   write.csv(df_as, file.path(dir_output, "df_as_cdc_covariates.csv"))
# } else {
#   # DF w/ all covariates
#   write.csv(df_as, file.path(dir_output, "df_as_covariates.csv"))
# }
# 
# ##----------------------------------------------------------------
# ## SKEWNESS EXPLORATION - SAFE TO DELETE
# ##----------------------------------------------------------------
# # skew_summary_table <- function(df) {
# #   df %>%
# #     select(where(is.numeric)) %>%
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




##----------------------------------------------------------------
##' Title: ML_exploration.R
##'
##' Purpose: Machine learning exploration to identify key predictors of 
##'          HIV mortality and DALYs, helping uncover confounders and 
##'          potential mechanisms driving outcomes
##----------------------------------------------------------------

##----------------------------------------------------------------
## Load packages
##----------------------------------------------------------------
# pacman::p_load(
#   data.table, tidyverse, glmnet, randomForest, caret, 
#   doParallel, foreach, corrplot, ggplot2, viridis,
#   pdp, iml, DALEX, ranger
# )

##----------------------------------------------------------------
## 1. Prepare the data (assumes df_as is already loaded from your main script)
##    Run this AFTER running your main script through section 8
##----------------------------------------------------------------

# # Filter to HIV only
# df_ml <- df_as %>% filter(acause == "hiv")
# 
# # Define outcome variables
# outcome_mort <- "as_mort_prev_ratio"
# outcome_daly <- "as_daly_prev_ratio"
# outcome_yll <- "as_yll_prev_ratio"
# outcome_yld <- "as_yld_prev_ratio"
# 
# # Define ALL potential predictors (excluding outcomes and identifiers)
# predictors_all <- c(
#   # Spending variables
#   "rw_dex_hiv_prev_ratio",      # Main spending predictor (RW + DEX / prev)
#   "rw_hiv_prev_ratio",          # RW only / prev
#   "as_spend_prev_ratio",        # General spend / prev
#   "spend_all",                  # Total spending (raw)
#   "ryan_white_funding_final",   # RW funding (raw)
#   
#   # Demographics
#   "age65",                      # Proportion 65+
#   "race_prop_BLCK",             # Proportion Black
#   "race_prop_HISP",             # Proportion Hispanic
#   "race_prop_WHT",              # Proportion White
#   "race_prop_API",              # Proportion Asian/Pacific Islander
#   "race_prop_AIAN",             # Proportion American Indian/Alaska Native
#   
#   # Socioeconomic
#   "edu_yrs",                    # Education years
#   "ldi_pc",                     # Income per capita
#   "unemployment_rate",          # Unemployment
#   "prop_homeless",              # Homelessness proportion
#   
#   # Health behaviors / risk factors
#   "obesity",                    # Obesity prevalence
#   "cig_pc_10",                  # Cigarette consumption
#   "phys_act_10",                # Physical activity
#   "prev_smoking",               # Smoking prevalence
#   "bmi",                        # BMI
#   
#   # Health system / geography
#   "density_l.150",              # Low density areas
#   "density_g.1000",             # High density areas
#   "aca_implemented_status",     # ACA expansion
#   "spending_adj_pc",            # Adjusted spending per capita
#   
#   # Comorbidities
#   "sud_prevalence_counts",      # SUD prevalence
#   "prev_diabetes",              # Diabetes prevalence
#   
#   # Clinical indicators
#   "sbp",                        # Systolic blood pressure
#   "cholesterol",                # Cholesterol
#   "hemoglobin",                 # Hemoglobin
#   
#   # HIV-specific
#   "high_hiv_prev",              # High prevalence indicator
#   "hiv_prevalence_counts",      # Raw prevalence counts
#   "prevalence_rates",           # Prevalence rate
#   "incidence_rates",            # Incidence rate
#   
#   # Population
#   "population"
# )
# 
# # Keep only predictors that exist in the data
# predictors_available <- predictors_all[predictors_all %in% colnames(df_ml)]
# cat("Available predictors:", length(predictors_available), "\n")
# print(predictors_available)
# 
# ##----------------------------------------------------------------
# ## 2. Create analysis dataset
# ##----------------------------------------------------------------
# # Select variables and remove NAs
# df_analysis <- df_ml %>%
#   select(all_of(c(outcome_mort, outcome_daly, outcome_yll, outcome_yld, 
#                   predictors_available, "year_id", "location_id", "location_name"))) %>%
#   drop_na()
# 
# cat("\nAnalysis dataset:", nrow(df_analysis), "observations\n")
# 
# # Convert factors back to numeric for ML
# df_analysis$year_num <- as.numeric(as.character(df_analysis$year_id))
# df_analysis$location_num <- as.numeric(df_analysis$location_id)
# 
# # Update predictors to include year
# predictors_final <- c(predictors_available, "year_num")
# 
# ##----------------------------------------------------------------
# ## 3. Correlation Analysis - Quick look at what's related
# ##----------------------------------------------------------------
# # Compute correlations with outcomes
# cor_mort <- cor(df_analysis[, predictors_final], 
#                 df_analysis[[outcome_mort]], 
#                 use = "pairwise.complete.obs")
# cor_daly <- cor(df_analysis[, predictors_final], 
#                 df_analysis[[outcome_daly]], 
#                 use = "pairwise.complete.obs")
# 
# cor_df <- data.frame(
#   variable = rownames(cor_mort),
#   cor_mortality = as.vector(cor_mort),
#   cor_daly = as.vector(cor_daly)
# ) %>%
#   arrange(desc(abs(cor_mortality)))
# 
# cat("\n=== TOP CORRELATIONS WITH MORTALITY ===\n")
# print(head(cor_df, 15))
# 
# cat("\n=== TOP CORRELATIONS WITH DALYs ===\n")
# print(cor_df %>% arrange(desc(abs(cor_daly))) %>% head(15))
# 
# ##----------------------------------------------------------------
# ## 4. LASSO Regression - Variable Selection
# ##----------------------------------------------------------------
# run_lasso <- function(outcome_var, predictors, data, alpha = 1) {
#   # Prepare matrices
#   X <- as.matrix(data[, predictors])
#   y <- data[[outcome_var]]
#   
#   # Standardize predictors
#   X_scaled <- scale(X)
#   
#   # Cross-validated LASSO
#   set.seed(42)
#   cv_fit <- cv.glmnet(X_scaled, y, alpha = alpha, nfolds = 10)
#   
#   # Extract coefficients at lambda.min and lambda.1se
#   coef_min <- coef(cv_fit, s = "lambda.min")
#   coef_1se <- coef(cv_fit, s = "lambda.1se")
#   
#   # Create coefficient dataframe
#   coef_df <- data.frame(
#     variable = rownames(coef_min)[-1],  # Remove intercept
#     coef_lambda_min = as.vector(coef_min)[-1],
#     coef_lambda_1se = as.vector(coef_1se)[-1]
#   ) %>%
#     filter(coef_lambda_min != 0 | coef_lambda_1se != 0) %>%
#     arrange(desc(abs(coef_lambda_min)))
#   
#   return(list(
#     cv_fit = cv_fit,
#     coef_df = coef_df,
#     lambda_min = cv_fit$lambda.min,
#     lambda_1se = cv_fit$lambda.1se
#   ))
# }
# 
# cat("\n\n========================================\n")
# cat("=== LASSO RESULTS FOR MORTALITY ===\n")
# cat("========================================\n")
# lasso_mort <- run_lasso(outcome_mort, predictors_final, df_analysis)
# print(lasso_mort$coef_df)
# 
# cat("\n\n========================================\n")
# cat("=== LASSO RESULTS FOR DALYs ===\n")
# cat("========================================\n")
# lasso_daly <- run_lasso(outcome_daly, predictors_final, df_analysis)
# print(lasso_daly$coef_df)
# 
# ##----------------------------------------------------------------
# ## 5. Elastic Net (alpha = 0.5) - Balance between LASSO and Ridge
# ##----------------------------------------------------------------
# cat("\n\n========================================\n")
# cat("=== ELASTIC NET (alpha=0.5) FOR MORTALITY ===\n")
# cat("========================================\n")
# enet_mort <- run_lasso(outcome_mort, predictors_final, df_analysis, alpha = 0.5)
# print(enet_mort$coef_df)
# 
# ##----------------------------------------------------------------
# ## 6. Random Forest - Variable Importance
# ##----------------------------------------------------------------
# run_rf <- function(outcome_var, predictors, data, ntree = 500) {
#   # Prepare data
#   X <- data[, predictors]
#   y <- data[[outcome_var]]
#   
#   set.seed(42)
#   rf_fit <- randomForest(x = X, y = y, ntree = ntree, importance = TRUE)
#   
#   # Extract importance
#   importance_df <- data.frame(
#     variable = rownames(importance(rf_fit)),
#     pct_inc_mse = importance(rf_fit)[, "%IncMSE"],
#     inc_node_purity = importance(rf_fit)[, "IncNodePurity"]
#   ) %>%
#     arrange(desc(pct_inc_mse))
#   
#   return(list(
#     rf_fit = rf_fit,
#     importance = importance_df,
#     r2 = mean(rf_fit$rsq)
#   ))
# }
# 
# cat("\n\n========================================\n")
# cat("=== RANDOM FOREST FOR MORTALITY ===\n")
# cat("========================================\n")
# rf_mort <- run_rf(outcome_mort, predictors_final, df_analysis)
# cat("\nR-squared:", round(rf_mort$r2, 4), "\n")
# cat("\nTop 15 Important Variables (by %IncMSE):\n")
# print(head(rf_mort$importance, 15))
# 
# cat("\n\n========================================\n")
# cat("=== RANDOM FOREST FOR DALYs ===\n")
# cat("========================================\n")
# rf_daly <- run_rf(outcome_daly, predictors_final, df_analysis)
# cat("\nR-squared:", round(rf_daly$r2, 4), "\n")
# cat("\nTop 15 Important Variables (by %IncMSE):\n")
# print(head(rf_daly$importance, 15))
# 
# ##----------------------------------------------------------------
# ## 7. Summary Table - Consensus across methods
# ##----------------------------------------------------------------
# create_summary <- function(cor_df, lasso_result, rf_result, outcome_name) {
#   
#   # Merge all results
#   summary_df <- cor_df %>%
#     rename(correlation = starts_with("cor_")) %>%
#     left_join(
#       lasso_result$coef_df %>% 
#         select(variable, lasso_coef = coef_lambda_min),
#       by = "variable"
#     ) %>%
#     left_join(
#       rf_result$importance %>%
#         select(variable, rf_importance = pct_inc_mse),
#       by = "variable"
#     ) %>%
#     mutate(
#       # Rank each method
#       rank_cor = rank(-abs(correlation), na.last = "keep"),
#       rank_lasso = ifelse(is.na(lasso_coef), NA, rank(-abs(lasso_coef), na.last = "keep")),
#       rank_rf = rank(-rf_importance, na.last = "keep"),
#       # Average rank (lower is better)
#       avg_rank = rowMeans(cbind(rank_cor, rank_lasso, rank_rf), na.rm = TRUE)
#     ) %>%
#     arrange(avg_rank)
#   
#   return(summary_df)
# }
# 
# cat("\n\n========================================\n")
# cat("=== CONSENSUS RANKING - MORTALITY ===\n")
# cat("========================================\n")
# cor_mort_df <- cor_df %>% select(variable, cor_mortality) %>% rename(correlation = cor_mortality)
# summary_mort <- create_summary(cor_mort_df, lasso_mort, rf_mort, "mortality")
# print(head(summary_mort %>% select(variable, correlation, lasso_coef, rf_importance, avg_rank), 20))
# 
# cat("\n\n========================================\n")
# cat("=== CONSENSUS RANKING - DALYs ===\n")
# cat("========================================\n")
# cor_daly_df <- cor_df %>% select(variable, cor_daly) %>% rename(correlation = cor_daly)
# summary_daly <- create_summary(cor_daly_df, lasso_daly, rf_daly, "daly")
# print(head(summary_daly %>% select(variable, correlation, lasso_coef, rf_importance, avg_rank), 20))
# 
# ##----------------------------------------------------------------
# ## 8. Investigate Spending-Outcome Relationship by Subgroups
# ##----------------------------------------------------------------
# cat("\n\n========================================\n")
# cat("=== SPENDING EFFECT BY SUBGROUPS ===\n")
# cat("========================================\n")
# 
# # By high/low prevalence
# cat("\n--- By HIV Prevalence (High vs Low) ---\n")
# df_analysis %>%
#   group_by(high_hiv_prev) %>%
#   summarise(
#     n = n(),
#     mean_spending = mean(rw_dex_hiv_prev_ratio),
#     mean_mortality = mean(as_mort_prev_ratio),
#     cor_spend_mort = cor(rw_dex_hiv_prev_ratio, as_mort_prev_ratio)
#   ) %>%
#   print()
# 
# # By ACA expansion
# cat("\n--- By ACA Expansion Status ---\n")
# df_analysis %>%
#   group_by(aca_implemented_status) %>%
#   summarise(
#     n = n(),
#     mean_spending = mean(rw_dex_hiv_prev_ratio),
#     mean_mortality = mean(as_mort_prev_ratio),
#     cor_spend_mort = cor(rw_dex_hiv_prev_ratio, as_mort_prev_ratio)
#   ) %>%
#   print()
# 
# # By population density (urban/rural)
# df_analysis$urban <- ifelse(df_analysis$density_g.1000 > median(df_analysis$density_g.1000), "Urban", "Rural")
# cat("\n--- By Urban/Rural ---\n")
# df_analysis %>%
#   group_by(urban) %>%
#   summarise(
#     n = n(),
#     mean_spending = mean(rw_dex_hiv_prev_ratio),
#     mean_mortality = mean(as_mort_prev_ratio),
#     cor_spend_mort = cor(rw_dex_hiv_prev_ratio, as_mort_prev_ratio)
#   ) %>%
#   print()
# 
# # By race composition (high Black population vs others)
# df_analysis$high_black_pop <- ifelse(df_analysis$race_prop_BLCK > median(df_analysis$race_prop_BLCK), "High", "Low")
# cat("\n--- By Black Population Proportion ---\n")
# df_analysis %>%
#   group_by(high_black_pop) %>%
#   summarise(
#     n = n(),
#     mean_spending = mean(rw_dex_hiv_prev_ratio),
#     mean_mortality = mean(as_mort_prev_ratio),
#     cor_spend_mort = cor(rw_dex_hiv_prev_ratio, as_mort_prev_ratio)
#   ) %>%
#   print()
# 
# ##----------------------------------------------------------------
# ## 9. Partial Dependence Analysis - Spending effect controlling for others
# ##----------------------------------------------------------------
# cat("\n\n========================================\n")
# cat("=== PARTIAL DEPENDENCE OF SPENDING ===\n")
# cat("========================================\n")
# 
# # Using the RF model, examine partial dependence of spending
# pd_spend <- partial(rf_mort$rf_fit, 
#                     pred.var = "rw_dex_hiv_prev_ratio",
#                     train = df_analysis[, predictors_final])
# 
# cat("Partial dependence of spending on mortality:\n")
# print(summary(pd_spend))
# 
# # Check for interactions between spending and prevalence
# pd_interact <- partial(rf_mort$rf_fit,
#                        pred.var = c("rw_dex_hiv_prev_ratio", "high_hiv_prev"),
#                        train = df_analysis[, predictors_final])
# 
# cat("\nPartial dependence - Spending x High Prevalence interaction:\n")
# print(head(pd_interact, 20))
# 
# ##----------------------------------------------------------------
# ## 10. Mediation Analysis Setup - What mediates spending effect?
# ##----------------------------------------------------------------
# cat("\n\n========================================\n")
# cat("=== POTENTIAL MEDIATORS OF SPENDING ===\n")
# cat("========================================\n")
# 
# # Variables that might be on the causal pathway from spending to outcomes
# potential_mediators <- c("incidence_rates", "prevalence_rates", 
#                          "sud_prevalence_counts", "prev_diabetes")
# 
# cat("Correlations between spending and potential mediators:\n")
# for (med in potential_mediators) {
#   if (med %in% colnames(df_analysis)) {
#     cor_val <- cor(df_analysis$rw_dex_hiv_prev_ratio, df_analysis[[med]], use = "complete.obs")
#     cat(sprintf("  %s: %.4f\n", med, cor_val))
#   }
# }
# 
# ##----------------------------------------------------------------
# ## 11. Time Trends - Does spending effect vary over time?
# ##----------------------------------------------------------------
# cat("\n\n========================================\n")
# cat("=== SPENDING EFFECT BY YEAR ===\n")
# cat("========================================\n")
# 
# yearly_effects <- df_analysis %>%
#   group_by(year_num) %>%
#   summarise(
#     n = n(),
#     cor_spend_mort = cor(rw_dex_hiv_prev_ratio, as_mort_prev_ratio),
#     cor_spend_daly = cor(rw_dex_hiv_prev_ratio, as_daly_prev_ratio),
#     mean_spend = mean(rw_dex_hiv_prev_ratio),
#     mean_mort = mean(as_mort_prev_ratio)
#   )
# 
# print(yearly_effects)
# 
# ##----------------------------------------------------------------
# ## 12. Create Visualization Outputs
# ##----------------------------------------------------------------
# 
# # Variable importance plot - Mortality
# p_imp_mort <- rf_mort$importance %>%
#   head(15) %>%
#   mutate(variable = fct_reorder(variable, pct_inc_mse)) %>%
#   ggplot(aes(x = pct_inc_mse, y = variable)) +
#   geom_col(fill = "steelblue") +
#   labs(title = "Random Forest Variable Importance - Mortality",
#        x = "% Increase in MSE",
#        y = "") +
#   theme_minimal()
# 
# # Variable importance plot - DALYs
# p_imp_daly <- rf_daly$importance %>%
#   head(15) %>%
#   mutate(variable = fct_reorder(variable, pct_inc_mse)) %>%
#   ggplot(aes(x = pct_inc_mse, y = variable)) +
#   geom_col(fill = "darkred") +
#   labs(title = "Random Forest Variable Importance - DALYs",
#        x = "% Increase in MSE",
#        y = "") +
#   theme_minimal()
# 
# # Save plots
# ggsave("rf_importance_mortality.png", p_imp_mort, width = 10, height = 8)
# ggsave("rf_importance_daly.png", p_imp_daly, width = 10, height = 8)
# 
# cat("\nPlots saved to working directory\n")
# 
# ##----------------------------------------------------------------
# ## 13. Export Results
# ##----------------------------------------------------------------
# 
# # Save summary tables
# write.csv(summary_mort, "ml_summary_mortality.csv", row.names = FALSE)
# write.csv(summary_daly, "ml_summary_daly.csv", row.names = FALSE)
# write.csv(yearly_effects, "spending_effect_by_year.csv", row.names = FALSE)
# 
# cat("\n\n========================================\n")
# cat("=== ANALYSIS COMPLETE ===\n")
# cat("========================================\n")
# cat("\nKey outputs saved:\n")
# cat("  - ml_summary_mortality.csv\n")
# cat("  - ml_summary_daly.csv\n")
# cat("  - spending_effect_by_year.csv\n")
# cat("  - rf_importance_mortality.png\n")
# cat("  - rf_importance_daly.png\n")
# 
# ##----------------------------------------------------------------
# ## 14. INTERPRETATION GUIDANCE
# ##----------------------------------------------------------------
# cat("\n\n========================================\n")
# cat("=== INTERPRETATION GUIDANCE ===\n")
# cat("========================================\n")
# cat("
# 1. LASSO COEFFICIENTS: 
#    - Variables with non-zero coefficients are selected as important
#    - Sign indicates direction of association
#    - Magnitude (on standardized scale) indicates relative importance
# 
# 2. RANDOM FOREST %IncMSE:
#    - Higher values = more important for prediction
#    - Captures non-linear relationships and interactions
#    - Does not indicate direction of effect
# 
# 3. POTENTIAL CONFOUNDERS:
#    - Variables that are:
#      a) Correlated with spending
#      b) Correlated with outcomes
#      c) Have high importance in ML models
#    - Check: prevalence_rates, incidence_rates, race variables, density
# 
# 4. REVERSE CAUSALITY CHECK:
#    - If prevalence/incidence rates are top predictors, this suggests
#      spending follows disease burden (reverse causality)
#    - High-burden states receive more funding AND have worse outcomes
# 
# 5. NEXT STEPS:
#    - Consider instrumental variable approaches
#    - Look at lagged spending effects
#    - Examine within-state changes over time (first differences)
#    - Stratify by state characteristics
# ")
# 
# 
# 
# pacman::p_load(randomForest, dplyr, purrr, tibble)
# 
# run_rf_groupcv <- function(outcome_var, predictors, data,
#                            group_var = "location_id",
#                            ntree = 1000, mtry = NULL, seed = 42) {
#   set.seed(seed)
#   
#   # ensure numeric matrix (RF in randomForest can handle factors, but keep it clean)
#   df <- data %>% dplyr::select(all_of(c(outcome_var, predictors, group_var))) %>% tidyr::drop_na()
#   
#   # create grouped folds: hold out entire states
#   groups <- unique(df[[group_var]])
#   K <- 10
#   folds <- split(groups, sort(rep(1:K, length.out = length(groups))))
#   
#   fold_metrics <- purrr::map_dfr(seq_along(folds), function(k) {
#     test_groups <- folds[[k]]
#     train <- df[df[[group_var]] %in% setdiff(groups, test_groups), ]
#     test  <- df[df[[group_var]] %in% test_groups, ]
#     
#     X_train <- train[, predictors, drop = FALSE]
#     y_train <- train[[outcome_var]]
#     X_test  <- test[, predictors, drop = FALSE]
#     y_test  <- test[[outcome_var]]
#     
#     if (is.null(mtry)) mtry_use <- max(1, floor(sqrt(ncol(X_train)))) else mtry_use <- mtry
#     
#     rf_fit <- randomForest::randomForest(
#       x = X_train, y = y_train,
#       ntree = ntree, mtry = mtry_use,
#       importance = TRUE
#     )
#     
#     pred <- predict(rf_fit, newdata = X_test)
#     
#     tibble(
#       fold = k,
#       rmse = sqrt(mean((y_test - pred)^2)),
#       r2   = 1 - sum((y_test - pred)^2) / sum((y_test - mean(y_test))^2)
#     )
#   })
#   
#   # Fit final model on full data for importance table
#   X_full <- df[, predictors, drop = FALSE]
#   y_full <- df[[outcome_var]]
#   if (is.null(mtry)) mtry_use <- max(1, floor(sqrt(ncol(X_full)))) else mtry_use <- mtry
#   
#   rf_full <- randomForest::randomForest(
#     x = X_full, y = y_full,
#     ntree = ntree, mtry = mtry_use,
#     importance = TRUE
#   )
#   
#   imp <- randomForest::importance(rf_full)
#   imp_df <- tibble(
#     variable = rownames(imp),
#     pct_inc_mse = imp[, "%IncMSE"],
#     inc_node_purity = imp[, "IncNodePurity"]
#   ) %>% arrange(desc(pct_inc_mse))
#   
#   list(
#     rf_fit = rf_full,
#     importance = imp_df,
#     cv_metrics = fold_metrics,
#     cv_summary = fold_metrics %>%
#       summarise(rmse_mean = mean(rmse), rmse_sd = sd(rmse),
#                 r2_mean = mean(r2), r2_sd = sd(r2))
#   )
# }
# 
# ## Run it
# rf_mort <- run_rf_groupcv(outcome_mort, predictors_final, df_analysis, group_var = "location_id")
# rf_daly <- run_rf_groupcv(outcome_daly, predictors_final, df_analysis, group_var = "location_id")
# 
# rf_mort$cv_summary
# head(rf_mort$importance, 15)
# 
# rf_daly$cv_summary
# head(rf_daly$importance, 15)
# 
# 
# 
# ##----------------------------------------------------------------
# ##' Title: ML_informed_regression.R
# ##'
# ##' Purpose: Regression models informed by ML variable selection
# ##'          Incorporates urban/rural interactions and key confounders
# ##'          
# ##' Run this AFTER your main script through section 8 (after df_as is created)
# ##----------------------------------------------------------------
# 
# ##----------------------------------------------------------------
# ## 1. Prepare data (assumes df_as is loaded from main script)
# ##----------------------------------------------------------------
# 
# # Filter to HIV only
# df_reg <- df_as %>% filter(acause == "hiv")
# 
# # Create urban/rural indicator based on high-density population proportion
# df_reg <- df_reg %>%
#   mutate(
#     # Urban = above median high-density population
#     urban = ifelse(density_g.1000 > median(density_g.1000, na.rm = TRUE), 1, 0),
#     urban_factor = factor(urban, levels = c(0, 1), labels = c("Rural", "Urban"))
#   )
# 
# # Verify the split
# cat("Urban/Rural distribution:\n")
# print(table(df_reg$urban_factor))
# 
# ##----------------------------------------------------------------
# ## 2. Define model formulas based on ML findings
# ##----------------------------------------------------------------
# 
# # Key variables identified by LASSO:
# # - rw_dex_hiv_prev_ratio (main predictor - negative effect on mortality)
# # - as_spend_prev_ratio (general spending - positive/endogenous)
# # - density variables (urban/rural)
# # - ldi_pc (income)
# # - obesity, edu_yrs, prev_smoking (health/SES)
# # - incidence_rates (HIV-specific confounder)
# # - cholesterol (time proxy)
# # - high_hiv_prev (interaction term)
# 
# # ============================================================
# # MODEL SET 1: MORTALITY OUTCOME
# # ============================================================
# 
# # Model 1a: Basic with urban interaction
# f_mort_1a <- as.formula(
#   "as_mort_prev_ratio ~ rw_dex_hiv_prev_ratio * urban_factor"
# )
# 
# # Model 1b: Add year and state fixed effects
# f_mort_1b <- as.formula(
#   "as_mort_prev_ratio ~ rw_dex_hiv_prev_ratio * urban_factor + year_id + location_id"
# )
# 
# # Model 1c: Add key confounders from LASSO
# f_mort_1c <- as.formula(
#   "as_mort_prev_ratio ~ rw_dex_hiv_prev_ratio * urban_factor + 
#    year_id + location_id + 
#    ldi_pc + edu_yrs + obesity"
# )
# 
# # Model 1d: Full model with HIV-specific controls
# f_mort_1d <- as.formula(
#   "as_mort_prev_ratio ~ rw_dex_hiv_prev_ratio * urban_factor + 
#    year_id + location_id + 
#    ldi_pc + edu_yrs + obesity + prev_smoking +
#    incidence_rates + high_hiv_prev"
# )
# 
# # Model 1e: Three-way interaction (spending x urban x high_prev)
# f_mort_1e <- as.formula(
#   "as_mort_prev_ratio ~ rw_dex_hiv_prev_ratio * urban_factor * high_hiv_prev + 
#    year_id + location_id + 
#    ldi_pc + edu_yrs + obesity"
# )
# 
# # Model 1f: Urban/Rural interaction + race controls
# f_mort_1f <- as.formula(
#   "as_mort_prev_ratio ~ rw_dex_hiv_prev_ratio * urban_factor + 
#    year_id + location_id + 
#    ldi_pc + edu_yrs + obesity + 
#    race_prop_BLCK + race_prop_HISP"
# )
# 
# # ============================================================
# # MODEL SET 2: DALY OUTCOME
# # ============================================================
# 
# # Model 2a: Basic with urban interaction
# f_daly_1a <- as.formula(
#   "as_daly_prev_ratio ~ rw_dex_hiv_prev_ratio * urban_factor"
# )
# 
# # Model 2b: Add year and state fixed effects
# f_daly_1b <- as.formula(
#   "as_daly_prev_ratio ~ rw_dex_hiv_prev_ratio * urban_factor + year_id + location_id"
# )
# 
# # Model 2c: Add key confounders
# f_daly_1c <- as.formula(
#   "as_daly_prev_ratio ~ rw_dex_hiv_prev_ratio * urban_factor + 
#    year_id + location_id + 
#    ldi_pc + edu_yrs + obesity"
# )
# 
# # Model 2d: Full model
# f_daly_1d <- as.formula(
#   "as_daly_prev_ratio ~ rw_dex_hiv_prev_ratio * urban_factor + 
#    year_id + location_id + 
#    ldi_pc + edu_yrs + obesity + prev_smoking +
#    incidence_rates + high_hiv_prev"
# )
# 
# # ============================================================
# # MODEL SET 3: YLL OUTCOME (Years of Life Lost)
# # ============================================================
# 
# f_yll_1d <- as.formula(
#   "as_yll_prev_ratio ~ rw_dex_hiv_prev_ratio * urban_factor + 
#    year_id + location_id + 
#    ldi_pc + edu_yrs + obesity + prev_smoking +
#    incidence_rates + high_hiv_prev"
# )
# 
# # ============================================================
# # MODEL SET 4: YLD OUTCOME (Years Lived with Disability)
# # ============================================================
# 
# f_yld_1d <- as.formula(
#   "as_yld_prev_ratio ~ rw_dex_hiv_prev_ratio * urban_factor + 
#    year_id + location_id + 
#    ldi_pc + edu_yrs + obesity + prev_smoking +
#    incidence_rates + high_hiv_prev"
# )
# 
# ##----------------------------------------------------------------
# ## 3. Run mortality models
# ##----------------------------------------------------------------
# 
# cat("\n\n================================================================\n")
# cat("MORTALITY MODELS WITH URBAN/RURAL INTERACTION\n")
# cat("================================================================\n")
# 
# # Run models
# m_mort_1a <- lm(f_mort_1a, data = df_reg)
# m_mort_1b <- lm(f_mort_1b, data = df_reg)
# m_mort_1c <- lm(f_mort_1c, data = df_reg)
# m_mort_1d <- lm(f_mort_1d, data = df_reg)
# m_mort_1e <- lm(f_mort_1e, data = df_reg)
# m_mort_1f <- lm(f_mort_1f, data = df_reg)
# 
# # Print key results
# cat("\n--- Model 1a: Basic Urban Interaction ---\n")
# summary(m_mort_1a)
# 
# cat("\n--- Model 1d: Full Model with HIV Controls ---\n")
# summary(m_mort_1d)
# 
# cat("\n--- Model 1e: Three-way Interaction ---\n")
# summary(m_mort_1e)
# 
# ##----------------------------------------------------------------
# ## 4. Run DALY models
# ##----------------------------------------------------------------
# 
# cat("\n\n================================================================\n")
# cat("DALY MODELS WITH URBAN/RURAL INTERACTION\n")
# cat("================================================================\n")
# 
# m_daly_1a <- lm(f_daly_1a, data = df_reg)
# m_daly_1b <- lm(f_daly_1b, data = df_reg)
# m_daly_1c <- lm(f_daly_1c, data = df_reg)
# m_daly_1d <- lm(f_daly_1d, data = df_reg)
# 
# cat("\n--- Model 2a: Basic Urban Interaction ---\n")
# summary(m_daly_1a)
# 
# cat("\n--- Model 2d: Full Model ---\n")
# summary(m_daly_1d)
# 
# ##----------------------------------------------------------------
# ## 5. Run YLL and YLD models
# ##----------------------------------------------------------------
# 
# cat("\n\n================================================================\n")
# cat("YLL AND YLD MODELS\n")
# cat("================================================================\n")
# 
# m_yll_1d <- lm(f_yll_1d, data = df_reg)
# m_yld_1d <- lm(f_yld_1d, data = df_reg)
# 
# cat("\n--- YLL Full Model ---\n")
# summary(m_yll_1d)
# 
# cat("\n--- YLD Full Model ---\n")
# summary(m_yld_1d)
# 
# ##----------------------------------------------------------------
# ## 6. Extract and compare results
# ##----------------------------------------------------------------
# 
# # Function to extract key coefficients
# extract_spending_effects <- function(model, model_name) {
#   coefs <- broom::tidy(model) %>%
#     filter(grepl("rw_dex|urban|Urban", term)) %>%
#     mutate(
#       model = model_name,
#       signif = case_when(
#         p.value < 0.001 ~ "***",
#         p.value < 0.01 ~ "**",
#         p.value < 0.05 ~ "*",
#         p.value < 0.1 ~ ".",
#         TRUE ~ ""
#       )
#     )
#   return(coefs)
# }
# 
# # Compile results
# results_mort <- bind_rows(
#   extract_spending_effects(m_mort_1a, "Mort_Basic"),
#   extract_spending_effects(m_mort_1b, "Mort_FE"),
#   extract_spending_effects(m_mort_1c, "Mort_FE_SES"),
#   extract_spending_effects(m_mort_1d, "Mort_Full"),
#   extract_spending_effects(m_mort_1e, "Mort_3way"),
#   extract_spending_effects(m_mort_1f, "Mort_Race")
# )
# 
# results_daly <- bind_rows(
#   extract_spending_effects(m_daly_1a, "DALY_Basic"),
#   extract_spending_effects(m_daly_1b, "DALY_FE"),
#   extract_spending_effects(m_daly_1c, "DALY_FE_SES"),
#   extract_spending_effects(m_daly_1d, "DALY_Full")
# )
# 
# cat("\n\n================================================================\n")
# cat("SUMMARY: SPENDING EFFECTS ACROSS MORTALITY MODELS\n")
# cat("================================================================\n")
# print(results_mort %>% select(model, term, estimate, std.error, p.value, signif))
# 
# cat("\n\n================================================================\n")
# cat("SUMMARY: SPENDING EFFECTS ACROSS DALY MODELS\n")
# cat("================================================================\n")
# print(results_daly %>% select(model, term, estimate, std.error, p.value, signif))
# 
# ##----------------------------------------------------------------
# ## 7. Model comparison metrics
# ##----------------------------------------------------------------
# 
# get_metrics <- function(model, model_name) {
#   g <- broom::glance(model)
#   tibble(
#     model = model_name,
#     n = g$nobs,
#     r2 = round(g$r.squared, 4),
#     adj_r2 = round(g$adj.r.squared, 4),
#     aic = round(AIC(model), 1),
#     bic = round(BIC(model), 1)
#   )
# }
# 
# metrics_comparison <- bind_rows(
#   get_metrics(m_mort_1a, "Mort_1a_Basic"),
#   get_metrics(m_mort_1b, "Mort_1b_FE"),
#   get_metrics(m_mort_1c, "Mort_1c_FE_SES"),
#   get_metrics(m_mort_1d, "Mort_1d_Full"),
#   get_metrics(m_mort_1e, "Mort_1e_3way"),
#   get_metrics(m_mort_1f, "Mort_1f_Race"),
#   get_metrics(m_daly_1a, "DALY_1a_Basic"),
#   get_metrics(m_daly_1b, "DALY_1b_FE"),
#   get_metrics(m_daly_1c, "DALY_1c_FE_SES"),
#   get_metrics(m_daly_1d, "DALY_1d_Full"),
#   get_metrics(m_yll_1d, "YLL_Full"),
#   get_metrics(m_yld_1d, "YLD_Full")
# )
# 
# cat("\n\n================================================================\n")
# cat("MODEL FIT COMPARISON\n")
# cat("================================================================\n")
# print(metrics_comparison)
# 
# ##----------------------------------------------------------------
# ## 8. Compute marginal effects by urban/rural
# ##----------------------------------------------------------------
# 
# cat("\n\n================================================================\n")
# cat("MARGINAL EFFECTS OF SPENDING BY URBAN/RURAL\n")
# cat("================================================================\n")
# 
# # For the full mortality model
# coef_full <- coef(m_mort_1d)
# 
# # Effect in Rural areas (baseline)
# effect_rural <- coef_full["rw_dex_hiv_prev_ratio"]
# 
# # Effect in Urban areas (baseline + interaction)
# effect_urban <- coef_full["rw_dex_hiv_prev_ratio"] + 
#   coef_full["rw_dex_hiv_prev_ratio:urban_factorUrban"]
# 
# cat("\nFull Mortality Model (1d):\n")
# cat(sprintf("  Spending effect in RURAL areas:  %.6f\n", effect_rural))
# cat(sprintf("  Spending effect in URBAN areas:  %.6f\n", effect_urban))
# cat(sprintf("  Difference (Urban - Rural):      %.6f\n", effect_urban - effect_rural))
# 
# # For DALY model
# coef_daly <- coef(m_daly_1d)
# effect_rural_daly <- coef_daly["rw_dex_hiv_prev_ratio"]
# effect_urban_daly <- coef_daly["rw_dex_hiv_prev_ratio"] + 
#   coef_daly["rw_dex_hiv_prev_ratio:urban_factorUrban"]
# 
# cat("\nFull DALY Model (2d):\n")
# cat(sprintf("  Spending effect in RURAL areas:  %.6f\n", effect_rural_daly))
# cat(sprintf("  Spending effect in URBAN areas:  %.6f\n", effect_urban_daly))
# cat(sprintf("  Difference (Urban - Rural):      %.6f\n", effect_urban_daly - effect_rural_daly))
# 
# ##----------------------------------------------------------------
# ## 9. Stratified models (run separately for urban vs rural)
# ##----------------------------------------------------------------
# 
# cat("\n\n================================================================\n")
# cat("STRATIFIED MODELS: URBAN VS RURAL SEPARATELY\n")
# cat("================================================================\n")
# 
# # Stratified formula (no urban interaction needed)
# f_stratified <- as.formula(
#   "as_mort_prev_ratio ~ rw_dex_hiv_prev_ratio + 
#    year_id + location_id + 
#    ldi_pc + edu_yrs + obesity + prev_smoking +
#    incidence_rates + high_hiv_prev"
# )
# 
# # Rural only
# df_rural <- df_reg %>% filter(urban == 0)
# m_rural <- lm(f_stratified, data = df_rural)
# 
# # Urban only
# df_urban <- df_reg %>% filter(urban == 1)
# m_urban <- lm(f_stratified, data = df_urban)
# 
# cat("\n--- RURAL STATES ONLY ---\n")
# cat("N =", nrow(df_rural), "\n")
# rural_coef <- broom::tidy(m_rural) %>% filter(term == "rw_dex_hiv_prev_ratio")
# print(rural_coef)
# 
# cat("\n--- URBAN STATES ONLY ---\n")
# cat("N =", nrow(df_urban), "\n")
# urban_coef <- broom::tidy(m_urban) %>% filter(term == "rw_dex_hiv_prev_ratio")
# print(urban_coef)
# 
# ##----------------------------------------------------------------
# ## 10. Save outputs
# ##----------------------------------------------------------------
# 
# # Compile all coefficient tables
# coef_mort_full <- broom::tidy(m_mort_1d) %>% mutate(model = "Mortality_Full")
# coef_daly_full <- broom::tidy(m_daly_1d) %>% mutate(model = "DALY_Full")
# coef_yll_full <- broom::tidy(m_yll_1d) %>% mutate(model = "YLL_Full")
# coef_yld_full <- broom::tidy(m_yld_1d) %>% mutate(model = "YLD_Full")
# 
# all_coefs <- bind_rows(coef_mort_full, coef_daly_full, coef_yll_full, coef_yld_full)
# 
# # Save to output directory
# write.csv(all_coefs, file.path(dir_output, "ml_informed_model_coefficients.csv"), row.names = FALSE)
# write.csv(metrics_comparison, file.path(dir_output, "ml_informed_model_metrics.csv"), row.names = FALSE)
# write.csv(results_mort, file.path(dir_output, "spending_effects_mortality.csv"), row.names = FALSE)
# write.csv(results_daly, file.path(dir_output, "spending_effects_daly.csv"), row.names = FALSE)
# 
# cat("\n\n================================================================\n")
# cat("OUTPUTS SAVED TO:", dir_output, "\n")
# cat("================================================================\n")
# cat("  - ml_informed_model_coefficients.csv\n")
# cat("  - ml_informed_model_metrics.csv\n")
# cat("  - spending_effects_mortality.csv\n")
# cat("  - spending_effects_daly.csv\n")
# 
# ##----------------------------------------------------------------
# ## 11. INTERPRETATION GUIDE
# ##----------------------------------------------------------------
# 
# cat("\n\n================================================================\n")
# cat("INTERPRETATION GUIDE\n")
# cat("================================================================\n")
# cat("
# KEY TERMS TO INTERPRET:
# 
# 1. rw_dex_hiv_prev_ratio
#    - Main spending effect (in RURAL areas when interaction included)
#    - Negative coefficient = spending reduces mortality/DALYs (good!)
#    - Positive coefficient = spending associated with higher mortality (endogeneity?)
# 
# 2. urban_factorUrban
#    - Difference in baseline mortality between urban vs rural
#    - Negative = urban states have lower mortality at baseline
# 
# 3. rw_dex_hiv_prev_ratio:urban_factorUrban
#    - THE KEY INTERACTION TERM
#    - Positive = spending is LESS effective in urban areas
#    - Negative = spending is MORE effective in urban areas
#    - To get total urban effect: add main effect + interaction
# 
# 4. high_hiv_prev
#    - Controls for baseline disease burden
#    - Helps address reverse causality
# 
# 5. incidence_rates
#    - Controls for new cases
#    - Important confounder (new cases → more spending AND more deaths)
# 
# EXPECTED PATTERN IF REVERSE CAUSALITY:
# - Rural: positive spending coefficient (more sick → more spending)
# - Urban: negative or null (better infrastructure → spending works)
# 
# EXPECTED PATTERN IF SPENDING WORKS:
# - Both: negative coefficients
# - Interaction: may show urban is more effective (better infrastructure)
# ")
# 
