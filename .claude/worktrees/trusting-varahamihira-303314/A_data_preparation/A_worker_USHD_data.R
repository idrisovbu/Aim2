##----------------------------------------------------------------
##' Title: A_worker_USHD_data.R
##' Purpose: Retrieves USHD MX Ratio Summary Data & Population data
##' for a single location_id, saves as parquet file
##' ----------------------------------------------------------------

# These location ids seem to not have mx_ratio data, but do have population data? Unsure why.
# This list was created by examining the difference between the files available in the 20251123 run and the 20251204 run

# Linux command used:
# diff -rq ~/aim_outputs/Aim2/A_data_preparation/20251204/USHD/ ~/aim_outputs/Aim2/A_data_preparation/20251123/USHD/ | grep "Only in" | grep -oP 'loc_id_\K[0-9]+'

# c(
#   1118,1121,1776,1780,2217,2380,2385,2976,2986,3397,
#   3400,3403,3406,3408,3409,3414,3416,3418,3420,3422,
#   3425,3429,3430,3431,3438,3449,3454,3466,3470,3472,
#   3482,3488,3500,3503,3510,3519,3522,3524,3525,35525,
#   35526,35527,35528,35529,43855,43856,43857,44632,44633,
#   4990,4991,4992,4993,4994,60919,60920,642,647,651,
#   652,657,658,659,660,661,662,663,664,665,667,
#   674,678,817,828,831,835,837,838,879,957
# )

##----------------------------------------------------------------
## 0. Clear environment and set library paths
##----------------------------------------------------------------
rm(list = ls())

pacman::p_load(dplyr, openxlsx, RMySQL, data.table, ini, DBI, tidyr, readr, purrr, arrow)
library(lbd.loader, lib.loc = sprintf("/share/geospatial/code/geospatial-libraries/lbd.loader-%s.%s", 
                                      R.version$major, 
                                      strsplit(R.version$minor, '.', fixed = TRUE)[[1]][[1]]))

library("ushd.dbr", lib.loc = lbd.loader::pkg_loc("ushd.dbr"))


##----------------------------------------------------------------
## 0.1 Read in Parameter data
##----------------------------------------------------------------

if (interactive()) {
  fp_parameters <- "/ihme/homes/idrisov/aim_outputs/Aim2/R_resources/ushd_parameters.parquet"
  df_params <- read_parquet(fp_parameters)
  i <- 551
  
} else { 
  args <- commandArgs(trailingOnly = TRUE)
  fp_parameters <- args[1]
  
  array_job_number <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))
  print(paste0("job number:", array_job_number))
  df_params <- read_parquet(fp_parameters)
  i <- array_job_number
}

# Set parameters
cause_ids <- as.list(unlist(df_params$cause_id[[i]]))
sex_ids <- as.list(unlist(df_params$sex_id[[i]]))
measure_id <- df_params$measure_id[i]
year_ids <- as.list(unlist(df_params$year_ids[[i]]))
location_id <- as.list(df_params$location_id[i])
mcnty_id <- as.list(df_params$mcnty[i])
mxratio_model_run_id_number <- df_params$mxratio_model_run_id[i]

##----------------------------------------------------------------
## 0.2 Functions
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
## 0.3 Set output path
##----------------------------------------------------------------
# Set output filename
date_folder <- format(Sys.Date(), "%Y%m%d")
out_dir <- paste0("/ihme/homes/idrisov/aim_outputs/Aim2/A_data_preparation/", date_folder, "/USHD/")
filename_output <- paste0(out_dir, "USHD_county_loc_id_", location_id[[1]], ".parquet")

# Ensure output path exists
ensure_path(filename_output)

##----------------------------------------------------------------
## 1. Retrieve USHD MX ratio summary data per location
##----------------------------------------------------------------
# Grab summary data
mx_ratio_summary_data <- get_mxratio_model_summary_data(mxratio_model_run_id = mxratio_model_run_id_number,
                                                        cause_id = cause_ids,
                                                        measure_id = measure_id,
                                                        years = year_ids,
                                                        sex = sex_ids,
                                                        location_id = location_id,
                                                        race = list(1)
                                                        )

# Filter out ages 98, 99
# Drop "edu" column (edu is all 0 in mx_ratio data)
# Drop "race" column (we pull race = 1 which is all races)
mx_ratio_summary_data <- mx_ratio_summary_data %>%
  filter(!age %in% c(98, 99)) %>%
  select(!c("edu", "race"))

##----------------------------------------------------------------
## 2. Retrieve USHD population level data per location
##
## How to check population data versions / names / ids
# pop_versions <- get_covariate_version("pop_by_age_sex")
# View(pop_versions) 
##----------------------------------------------------------------
# Grab population data
population_data <- get_population_data(population_name = "pop_by_age_sex",
                                        location = mcnty_id,
                                        year = year_ids,
                                        sex = sex_ids,
                                        race = list(1)
                                        )

# Drop "edu" column (edu is all 1 in pop data)
population_data <- population_data %>%
  select(!c("edu", "race", "race_label", "edu_label", "race_set"))

##----------------------------------------------------------------
## 3. Merge mx_ratio & population data into single df
##----------------------------------------------------------------
df_output <- left_join(
  x = mx_ratio_summary_data,
  y = population_data,
  by = c("year", "sex", "age")
) %>%
  select(!c("mcnty", "state"))

##----------------------------------------------------------------
## 4. Save outputs
##----------------------------------------------------------------
write_parquet(df_output, filename_output)

