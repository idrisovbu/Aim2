##----------------------------------------------------------------
##' Title: A1_launcher_data_compiler.R
##'
##' Purpose: Launches jobs to read in all raw Medicaid, saving only the columns we need and assigning variables pertaining to hiv / sud
##----------------------------------------------------------------

##----------------------------------------------------------------
## 0. Clear environment and set library paths
##----------------------------------------------------------------
rm(list = ls())

pacman::p_load(dplyr, openxlsx, RMySQL, data.table, ini, DBI, tidyr, openxlsx)
library(lbd.loader, lib.loc = sprintf("/share/geospatial/code/geospatial-libraries/lbd.loader-%s", R.version$major))
if("dex.dbr"%in% (.packages())) detach("package:dex.dbr", unload=TRUE)
library(dex.dbr, lib.loc = lbd.loader::pkg_loc("dex.dbr"))
suppressMessages(devtools::load_all(path = "/ihme/homes/idrisov/repo/dex_us_county/"))

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

##----------------------------------------------------------------
## 1. Define variables for job / create output folders
##----------------------------------------------------------------
# Function to ensure the logs directory exists
ensure_dir_exists <- function(dir_path) {
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
}

# Define key variables
user <- Sys.info()[["user"]]
script_path <- paste0(h, "/repo/Aim2/A_data_preparation/A2_worker/A2_worker_data_compiler.R")

# Define output and log directory paths
date_folder <- format(Sys.Date(), "%Y%m%d")
log_dir <- file.path("/mnt/share/limited_use/LU_CMS/DEX/hivsud/aim2/A_data_preparation/logs/")

# Resources Directory
res_dir <- file.path("/mnt/share/limited_use/LU_CMS/DEX/hivsud/aim2/R_resources/")

# Create output and log directories
ensure_dir_exists(log_dir)
ensure_dir_exists(res_dir)

##----------------------------------------------------------------
## 2. Create parameters CSV file
##----------------------------------------------------------------
# Dynamically read in directories where data is stored to see what data they have in the first place
# Create a table based off their directory hierarchy (use code to scan folder directories to determine available data)
run_id <- 80
fp_input_data <- paste0("/mnt/share/limited_use/LU_CMS/DEX/01_pipeline/MDCD/run_", run_id, "/PRIMARY_CAUSE/data/")
list_input_data <- list.dirs(fp_input_data, recursive = TRUE)

# filter down to folders we are interested in that contain data
list_input_data_subset <- list()

for (i in 1:length(list_input_data)) {
  if (str_count(list_input_data[i], pattern = "st_resi") == 1) {
    list_input_data_subset[length(list_input_data_subset) + 1] <- list_input_data[i]
  }
}

# unlist to flatten list
list_input_data_subset <- unlist(list_input_data_subset)

# create data frame
df_list_input_data_subset <- data.frame(list_input_data_subset)
colnames(df_list_input_data_subset)[colnames(df_list_input_data_subset) == "list_input_data_subset"] <- "directory"

# add additional identifier columns
df_list_input_data_subset <- df_list_input_data_subset %>% mutate(runid = str_extract(directory, "(?<=run_)\\d+"))
df_list_input_data_subset <- df_list_input_data_subset %>% mutate(year_id = str_extract(directory, "(?<=year_id=)\\d+"))
df_list_input_data_subset <- df_list_input_data_subset %>% mutate(age_group_years_start = str_extract(directory, "(?<=age_group_years_start=)\\d+"))
df_list_input_data_subset <- df_list_input_data_subset %>% mutate(toc = str_extract(directory, "(?<=toc=)[^/]+"))
df_list_input_data_subset <- df_list_input_data_subset %>% mutate(sex_id = str_extract(directory, "(?<=sex_id=)[^/]+"))
df_list_input_data_subset <- df_list_input_data_subset %>% mutate(state = str_extract(directory, "(?<=st_resi=)[^/]+"))

# Convert age_group_years_start to numeric if a character string
df_list_input_data_subset$age_group_years_start <- as.numeric(df_list_input_data_subset$age_group_years_start)

# Save the filtered parameters to CSV save list as .csv for runner script to read in based off job
fp_parameters <- paste0(l, "/LU_CMS/DEX/hivsud/aim2/R_resources/A1_parameters_aims1.csv")
write.csv(df_list_input_data_subset, file = fp_parameters, row.names = FALSE)

##----------------------------------------------------------------
## 3. Submit jobs
##----------------------------------------------------------------
# Submit jobs to the cluster - Launcher Script for Aim 1 study - All Years, Ages, TOC from fp_parameter
jid <- SUBMIT_ARRAY_JOB(
  name = "aim1_f2t_data_compiler",
  script = script_path,
  args = c(fp_parameters), # Path to CSV with parameters
  error_dir = log_dir,
  output_dir = log_dir,
  queue = "all.q",
  n_jobs = length(unique(df_list_input_data_subset$year_id)),
  memory = "250G", 
  threads = 1, 
  time = "4:00:00", 
  user_email = paste0(user, "@uw.edu"),
  archive = FALSE,
  test = F # F = Full Run, T = Test Run (only run the first job in a batch)
)

cat("Submitted", length(unique(df_list_input_data_subset$year_id)), "array tasks.\n")
