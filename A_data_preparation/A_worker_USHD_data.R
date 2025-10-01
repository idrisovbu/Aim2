##----------------------------------------------------------------
##' Title: A_worker_USHD_data.R
##' Purpose: Retrieves USHD MX Ratio Summary Data for a single location_id, saves as parquet
##' ----------------------------------------------------------------

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
  i <- 1
  
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
year_ids <- as.list(unlist(df_params$years[[i]]))
location_id <- as.list(df_params$location_id[i])
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
                                                        location_id = location_id)

# Filter on race == 1 (all races)
mx_ratio_summary_data <- mx_ratio_summary_data %>%
  filter(race == 1)

##----------------------------------------------------------------
## 2. Save outputs
##----------------------------------------------------------------
write_parquet(mx_ratio_summary_data, filename_output)

