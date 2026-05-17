##----------------------------------------------------------------
##' Title: A_launcher_USHD_data.R
##'
##' Purpose: Pulls USHD mx ratio data
##----------------------------------------------------------------

##----------------------------------------------------------------
## 0. Clear environment and set library paths
##----------------------------------------------------------------
rm(list = ls())
pacman::p_load(dplyr, openxlsx, RMySQL, data.table, ini, DBI, tidyr, readr, purrr, arrow)

# # USHD Packages - Only need to load in if the mxratio_model_run_id needs to be updated, must restart R to work
# library(lbd.loader, lib.loc = sprintf("/share/geospatial/code/geospatial-libraries/lbd.loader-%s.%s", 
#                                       R.version$major, 
#                                       strsplit(R.version$minor, '.', fixed = TRUE)[[1]][[1]]))
# 
# library("ushd.dbr", lib.loc = lbd.loader::pkg_loc("ushd.dbr"))

# DEX Packages
library(lbd.loader, lib.loc = sprintf("/share/geospatial/code/geospatial-libraries/lbd.loader-%s", R.version$major))
if("dex.dbr"%in% (.packages())) detach("package:dex.dbr", unload=TRUE)
library(dex.dbr, lib.loc = lbd.loader::pkg_loc("dex.dbr"))
suppressMessages(devtools::load_all(path = "/ihme/homes/idrisov/repo/dex_us_county/"))

# IHME shared functions
# source("/ihme/cc_resources/libraries/current/r/get_cause_metadata.R")
# cause_id_list <- get_cause_metadata(cause_set_id = 2, release_id = 16, include_all_metadata = TRUE)

##----------------------------------------------------------------
## 0.1 Functions
##----------------------------------------------------------------
# Function to ensure filepath / folders exist
ensure_path <- function(filepath) {
  dirpath <- dirname(filepath)
  if (!dir.exists(dirpath)) {
    dir.create(dirpath, recursive = TRUE, showWarnings = FALSE)
  }
  return(filepath)
}

##----------------------------------------------------------------
## 1. Create Parameter file
##----------------------------------------------------------------
# Location_ids
df_counties <- fread("/snfs1/Project/us_counties/locations/counties/merged_counties.csv")

df_counties <- df_counties %>%
  filter(current == 1)

df_params <- df_counties %>%
  select(mcnty, location_id)

# Cause_ids
cause_ids <- list(298, 973) # hiv = 298, _subs = 973

# Sex ids
sex_ids <- list(1, 2) # male, female

# Measure id
measure_id <- 5 # prevalence

# Year ids
year_ids <- list(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019) # 2010 ~ 2019

# Create full df
df_params$cause_id             <- list(cause_ids)
df_params$sex_ids              <- list(sex_ids)
df_params$measure_id           <- list(measure_id)
df_params$year_ids             <- list(year_ids)
df_params$mxratio_model_run_id <- 1

# Write param list
param_path <- paste0("/ihme/homes/idrisov/aim_outputs/Aim2/R_resources/ushd_parameters.parquet")
ensure_path(param_path)

write_parquet(df_params, param_path)

# Define output and log directory paths
log_dir <- file.path("/ihme/homes/idrisov/aim_outputs/Aim2/A_data_preparation/logs/")

# Create output and log directories
ensure_path(log_dir)

##----------------------------------------------------------------
## 2. Launch Jobs
##----------------------------------------------------------------
fp_runner_script <- "/ihme/homes/idrisov/repo/Aim2/A_data_preparation/A_worker_USHD_data.R"

SUBMIT_ARRAY_JOB(
  name = 'USHD_data',
  script = fp_runner_script, 
  error_dir = log_dir,
  output_dir = log_dir,
  queue = "all.q", # string "all.q" or "long.q"
  memory = "10G", # string "#G"
  threads = "1", # string "#"
  time = "00:5:00", # string "##:##:##"
  n_jobs = nrow(df_params),
  args = param_path,
  test = F
)


