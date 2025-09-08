##----------------------------------------------------------------
##' Title: B_Frontier_Analysis.R
##'
##' Purpose: TBD
##' Notes: If the USHD functions don't work correctly, or you get the dbload issue, try restarting the R session and loading everything from the top
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

library(lbd.loader, lib.loc = sprintf("/share/geospatial/code/geospatial-libraries/lbd.loader-%s.%s",
                                      R.version$major,
                                      strsplit(R.version$minor, '.', fixed = TRUE)[[1]][[1]]))

library("ushd.dbr", lib.loc = lbd.loader::pkg_loc("ushd.dbr"))
pacman::p_load(dplyr, openxlsx, RMySQL, data.table, ini, DBI,
               tidyr, openxlsx, reticulate, ggpubr, arrow, scales)

library('frontier')
source("/ihme/cc_resources/libraries/current/r/get_outputs.R")
source("/ihme/cc_resources/libraries/current/r/get_location_metadata.R")
source("/ihme/cc_resources/libraries/current/r/get_age_metadata.R")

# Load in packages stored in repo
user_lib <- file.path(h, "/repo/Aim2/Y_Utilities/R_Packages/")
.libPaths(c(user_lib, .libPaths()))
library(tidycensus)

##----------------------------------------------------------------
## 0. Functions
##----------------------------------------------------------------
ensure_dir_exists <- function(dir_path) {
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
}

##----------------------------------------------------------------
## 0.1 Set directories for DEX estimate data / county estimates
##----------------------------------------------------------------
# List out directories
fp_dex_estimates <- file.path(h, "/county_data/")
dirs_dex_estimates <- list.dirs(fp_dex_estimates, recursive = TRUE)[-1]

# # Available columns
# c("year_id", "geo", "location_name", "fips", "payer", "toc", 
#   "acause", "cause_name", "age_group_years_start", "age_name", 
#   "sex_id", "sex_name", 
#   "spend_mean", "spend_lower", "spend_upper", 
#   "spend_per_capita_mean", "spend_per_capita_lower", "spend_per_capita_upper", 
#   "spend_per_bene_mean", "spend_per_bene_lower", "spend_per_bene_upper", 
#   "spend_per_vol_mean", "spend_per_vol_lower", "spend_per_vol_upper", 
#   "vol_per_capita_mean", "vol_per_capita_lower", "vol_per_capita_upper", 
#   "vol_per_bene_mean", "vol_per_bene_lower", "vol_per_bene_upper"
# )

# Set the current date for folder naming
date_today <- format(Sys.time(), "%Y%m%d")

# Set output directories
dir_output <- "/mnt/share/scratch/users/idrisov/Aim2_Outputs/"
dir_output_figures <- file.path(dir_output, "A_Figures/")
dir_output_figures_dated <- file.path(dir_output, "A_Figures/", date_today)
parquet_storage <- file.path(dir_output, "Y_Utilities/parquet_storage/")
dir_dex_temp_mi_data <- c("/mnt/share/dex/us_county/05_analysis/inputs/county_ushd/inc_prev_prediction/v10/data")

ensure_dir_exists(dir_output)
ensure_dir_exists(dir_output_figures)
ensure_dir_exists(dir_output_figures_dated)
ensure_dir_exists(parquet_storage)

##----------------------------------------------------------------
## 0.2 Read in FIPS county names
##----------------------------------------------------------------
# Join with FIPS codes to get county names etc.

# read in county_names (mcnty -> fips)
df_county_names <- fread("/mnt/share/dex/us_county/maps/merged_counties.csv")[, .(mcnty, fips = cnty)]

# read in fips code df
df_fips_lookup <- fips_codes

df_fips_lookup <- df_fips_lookup %>%
  mutate(full_fips_code = paste0(state_code, county_code))

# Fix FIPS codes 
df_county_names <- df_county_names %>%
  mutate(
    fips_chr = as.character(fips),             # step 1: convert to character
    fips_chr = ifelse(nchar(fips_chr) == 4,    # step 2: pad 4-digit strings
                      paste0("0", fips_chr),
                      fips_chr)
  ) 

df_fips <- left_join(x = df_county_names, y = df_fips_lookup, by = c("fips_chr" = "full_fips_code"))

df_fips_abbr <- df_fips %>%
  select("mcnty", "fips", "state", "state_name", "county")

##----------------------------------------------------------------
## 0.4 Read in data
##----------------------------------------------------------------

################## HIV Data

# Read in saved parquet file, or read in CSV files
bool_hiv_parquet <- TRUE

if (bool_hiv_parquet) {
  # Save combined_df_sud_state as parquet file for faster reading, read in if trying to save time
  combined_df_hiv_state <- read_parquet(file.path(parquet_storage, "combined_df_hiv_state_state.parquet"))
  
} else if (!bool_hiv_parquet) {
  # List out HIV data files
  dirs_dex_estimates_hiv <- dirs_dex_estimates[1]
  files_hiv <- list.files(dirs_dex_estimates_hiv, full.names = TRUE)
  
  # Columns of interest
  cols_of_int_1_hiv <- c("year_id", "geo", "location_name", "fips","payer", "toc", 
                         "acause", "cause_name", "age_group_years_start", "age_name", 
                         "sex_id", "sex_name",
                         "spend_mean", "spend_lower", "spend_upper")
  # Read in CSV files
  combined_df_hiv_state <- rbindlist(
    lapply(
      files_hiv,
      function(f) fread(f, select = cols_of_int_1_hiv, showProgress = TRUE)
    ),
    fill = TRUE
  )
  
  # Filter for county, remove NA data, remove spending == 0
  combined_df_hiv_state <- combined_df_hiv_state %>%
    filter(geo == "state") %>%
    filter(!is.na(spend_mean)) %>%
    filter(spend_mean > 0)
  
  # Save combined_df_hiv_state as parquet file for faster reading, read in if trying to save time
  write_parquet(combined_df_hiv_state, file.path(parquet_storage, "combined_df_hiv_state_state.parquet"))
}

################## SUD Data

# Read in saved parquet file, or read in CSV files
bool_sud_parquet <- TRUE

if (bool_sud_parquet) {
  # Save combined_df_sud_state as parquet file for faster reading, read in if trying to save time
  combined_df_sud_state <- read_parquet(file.path(parquet_storage, "combined_df_sud_state.parquet"))
  
} else if (!bool_sud_parquet) {
  # List out SUD data files
  dirs_dex_estimates_sud <- dirs_dex_estimates[2:4]
  files_sud <- list.files(dirs_dex_estimates_sud, full.names = TRUE)
  
  # Columns of interest
  cols_of_int_2_sud <- c("year_id", "geo", "location_name", "fips","payer", "toc", 
                         "acause", "cause_name", "age_group_years_start", "age_name", 
                         "sex_id", "sex_name",
                         "spend_mean", "spend_lower", "spend_upper")
  # Read in CSV files
  combined_df_sud_state <- rbindlist(
    lapply(
      files_sud,
      function(f) fread(f, select = cols_of_int_2_sud, showProgress = FALSE)
    ),
    fill = TRUE
  )
  
  # Filter for county, remove NA data, remove spending == 0
  combined_df_sud_state <- combined_df_sud_state %>%
    filter(geo == "state") %>%
    filter(!is.na(spend_mean)) %>%
    filter(spend_mean > 0)
  
  # Save combined_df_sud_state as parquet file for faster reading, read in if trying to save time
  write_parquet(combined_df_sud_state, file.path(parquet_storage, "combined_df_sud_state.parquet"))
}

##----------------------------------------------------------------
## 0.5 Read in DEX team temp MI data that was created for testing
##----------------------------------------------------------------
# dir_dex_temp_mi_data
# 
# files_dex_temp <- list.files(
#   path = dir_dex_temp_mi_data, 
#   pattern = "hiv|mental_",   # regex OR
#   recursive = TRUE,
#   full.names = TRUE,
#   ignore.case = TRUE          # optional, case-insensitive
# )
# 
# files_dex_temp
# 
# df_dex_temp <- do.call(rbind, lapply(files_dex_temp, read_parquet))
# 
# View(head(df_dex_temp, 50))
# 
# # read in county_names (mcnty -> fips)
# df_county_names <- fread("/mnt/share/dex/us_county/maps/merged_counties.csv")[, .(mcnty, fips = cnty)]
# 
# # read in fips code df
# df_fips_lookup <- fips_codes
# 
# df_fips_lookup <- df_fips_lookup %>%
#   mutate(full_fips_code = paste0(state_code, county_code))
# 
# # Fix FIPS codes 
# df_county_names <- df_county_names %>%
#   mutate(
#     fips_chr = as.character(fips),             # step 1: convert to character
#     fips_chr = ifelse(nchar(fips_chr) == 4,    # step 2: pad 4-digit strings
#                   paste0("0", fips_chr),
#                   fips_chr)
#   ) 
# 
# df_fips <- left_join(x = df_county_names, y = df_fips_lookup, by = c("fips_chr" = "full_fips_code"))

##----------------------------------------------------------------
## X. Pull in USHD Mortaliy Incidence ratio data
## TODO - This data has duplication issues, may not be possible to use as is
##----------------------------------------------------------------
measure_id <- 5 # 5: prevalence, 6: incidence
cause_id <- 298 # _subs example (hiv = 298, _subs = 973)

# Grab the model run id
mx_ratio_model_run <- get_mxratio_model_run(measure_id = measure_id)$mxratio_model_run_id 

# Grab summary data
# Can also filter on years, sex, race, location_id, age_group_id. Must be lists.
# May take a while without further filtering
mx_ratio_summary_data <- get_mxratio_model_summary_data(mxratio_model_run_id = mx_ratio_model_run,
                                                        cause_id = cause_id,
                                                        measure_id = measure_id,
                                                        years = list(2018),
                                                        sex = list(1),
                                                        race = list(1))

View(mx_ratio_summary_data %>% filter(age == 1) %>% filter(area == 84) %>% filter(sex == 1))
View(mx_ratio_summary_data %>% filter(age == 1) %>% filter(area == 85) %>% filter(sex == 1))

mx_ratio_summary_data <- unique(mx_ratio_summary_data)

df_counts <- mx_ratio_summary_data %>%
  group_by(across(-c(pred_mean, pred_lb, pred_median, pred_ub, pred_se))) %>%
  summarise(n = n(), .groups = "drop")

# Try filtering on just mcnty data
df_county_level <- mx_ratio_summary_data %>%
  select(all_of(c("cause_id", "year", "sex", "race", "pred_mean", "level", "area", "age", 
                  "measure_id"))) %>%
  filter(level == "mcnty") %>%
  unique()

df_county_level %>% slice(129269)

298
2010
1
1
0.02756524
mcnty
84
1
5

df_county_level %>% filter(age == 1) %>% filter(area == 84) %>% filter(sex == 1)

# # Check for duplicates
# unique(df_county_level)
# 
# df_county_level %>%
#   group_by(across(everything())) %>%
#   filter(n() > 1) %>%
#   ungroup() %>%
#   View()
# 
# mx_ratio_summary_data %>%
#   group_by(across(everything())) %>%
#   filter(n() > 1) %>%
#   ungroup() %>%
#   View()

# Join with FIPS codes to get county names etc.

# read in county_names (mcnty -> fips)
df_county_names <- fread("/mnt/share/dex/us_county/maps/merged_counties.csv")[, .(mcnty, fips = cnty)]

# read in fips code df
df_fips_lookup <- fips_codes

df_fips_lookup <- df_fips_lookup %>%
  mutate(full_fips_code = paste0(state_code, county_code))

# Fix FIPS codes 
df_county_names <- df_county_names %>%
  mutate(
    fips_chr = as.character(fips),             # step 1: convert to character
    fips_chr = ifelse(nchar(fips_chr) == 4,    # step 2: pad 4-digit strings
                      paste0("0", fips_chr),
                      fips_chr)
  ) 

df_fips <- left_join(x = df_county_names, y = df_fips_lookup, by = c("fips_chr" = "full_fips_code"))

df_fips_abbr <- df_fips %>%
  select("mcnty", "fips", "state", "state_name", "county")

# Merge with data
df_county_level <- left_join(x = df_county_level, y = df_fips_abbr, by = c("area" = "mcnty"),
                             relationship = "many-to-one")


##----------------------------------------------------------------
## 1. Pull DALYs data from get_outputs
##----------------------------------------------------------------

# get location metadata
df_loc_metadata <- get_location_metadata(location_set_id = 35, release_id = 16)

us_state_loc_ids <- df_loc_metadata %>% filter(level == 4) %>% filter(parent_id == 102)
us_state_loc_ids <- us_state_loc_ids$location_id %>% unique()

# cause_id (hiv = 298, _subs = 973)
cause_id_list <- c(298, 973)
year_id_list <- seq(2010, 2019, 1)
measure_id <- 2 # DALYs
metric_id <- 1 # Count (number)

# Pull all-cause (cause 294) results from GBD round 6 (release 6) for the years 2010, 2015
df_outputs <- get_outputs("cause", release_id = 16, cause_id = 298, year_id = 2010, measure_id = 2,
                          metric_id = 1, age_group_id = "most_detailed", sex_id = c(1,2),
                          location_id = us_state_loc_ids)

##----------------------------------------------------------------
## 2. Set Frontier Analysis function
## TODO - Age groups, need to figure out how we can group by summary the get_outputs() age groups
## given that they are more granular at the lower and upper age bounds to make them compatible with
## the DEX data's age groups
##----------------------------------------------------------------

# get age metadata
df_age_metadata <- get_age_metadata(release_id = 16)

# group by summary to collapse on payer, toc, for DEX HIV data
df_hiv_dex <- combined_df_hiv_state %>%
  group_by(year_id, location_name,  
           acause, cause_name, age_group_years_start, age_name, 
           sex_id, sex_name) %>%
  summarize(spend_mean = mean(spend_mean))

df_hiv_dex_2010 <- df_hiv_dex %>%
  filter(year_id == 2010)

# TEMP - filter for common age groups 
df_hiv_dex_2010 <- df_hiv_dex_2010 %>%
  filter(!age_group_years_start %in% c("0", "1", "85"))

df_outputs_age_filter <- df_outputs %>%
  filter(!age_group_name %in% c("Early Neonatal", "Late Neonatal", "2 to 4", "90 to 94", "12 to 23 months", "1-5 months",
                                "6-11 months", "95 plus"))

# Join the DEX HIV data with the DALYs output data

# unique values from each
ages1 <- unique(df_hiv_dex_2010$age_name)
ages2 <- unique(df_outputs_age_filter$age_group_name)

# Build a mapping data.frame
age_map <- tibble(
  age_name = ages1,
  age_group_name = ages2[1:length(ages1)]  # align positionally
)

#View(age_map)

# Join into one df (temp)
df_hiv_model <- left_join(x = df_outputs_age_filter, y = age_map, by = c("age_group_name")) %>%
  left_join(y = df_hiv_dex_2010, 
                          by = c("sex_id", "location_name", "cause_name", "age_name", "year_id", "acause"))

# filter NAs (idk why created)
df_hiv_model <- df_hiv_model %>%
  filter(!is.na(spend_mean))


# Create model
# Model: DALYs ~ spend_mean
m_fe <- sfa(
  log(val) ~ log(spend_mean) + factor(location_name) + factor(sex) + factor(age_group_name),
  data          = df_hiv_model,
  ineffDecrease = FALSE
)

m_fe <- sfa(
  log(val) ~ log(spend_mean),
  data          = df_hiv_model,
  ineffDecrease = FALSE
)


#####

# -------------------------------------------------------------------
# Simple Stochastic Frontier Example
# Outcome: DALYs (val)
# Predictor: Healthcare spending (spend_mean)
# Cross-sectional data (one year)
# -------------------------------------------------------------------

library(frontier)

# --- 1. Baseline model: log(DALYs) on log(spending) -----------------
# This is the simplest Cobb–Douglas frontier in log-log form.
# ineffDecrease = TRUE means inefficiency increases DALYs (bad outcome).
# Try flipping to FALSE if you want to see the difference in orientation.

m_simple <- sfa(
  log(val) ~ log(spend_mean),
  data          = df_hiv_model,
  ineffDecrease = TRUE   # DALYs are "bad", so inefficiency = more DALYs
)

summary(m_simple)


# --- 2. Add demographics as controls -------------------------------
# These controls shift the frontier itself (not the inefficiency).
# Here we include state (location_name), sex, and age group.

m_controls <- sfa(
  log(val) ~ log(spend_mean) +
    factor(location_name) +
    factor(sex) +
    factor(age_group_name),
  data          = df_hiv_model,
  ineffDecrease = TRUE
)

summary(m_controls)


# --- 3. Extract efficiencies ---------------------------------------
# Efficiency scores are in [0,1], where 1 = most efficient.
# Because we logged the dependent variable, set logDepVar = TRUE.
# minusU = TRUE gives Farrell-type efficiencies (higher = better).

eff_simple <- efficiencies(m_simple, asInData = TRUE,
                           logDepVar = TRUE, minusU = TRUE)
eff_controls <- efficiencies(m_controls, asInData = TRUE,
                             logDepVar = TRUE, minusU = TRUE)

# Add to your dataframe
df_hiv_model$eff_simple   <- eff_simple
df_hiv_model$eff_controls <- eff_controls


# --- 4. Inspect results --------------------------------------------
head(df_hiv_model[, c("location_name", "sex", "age_group_name",
                      "val", "spend_mean", "eff_simple", "eff_controls")])

View(df_hiv_model[, c("location_name", "sex", "age_group_name",
                      "val", "spend_mean", "eff_simple", "eff_controls")])

df_hiv_model$dollar_daly_ratio <- df_hiv_model$spend_mean / df_hiv_model$val

View(df_hiv_model[, c("location_name", "sex", "age_group_name",
                      "val", "spend_mean", "eff_simple", "eff_controls", "dollar_daly_ratio")])








