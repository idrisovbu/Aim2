##----------------------------------------------------------------
##' Title: B_data_aggregator.R
##'
##' Purpose: Aggregates outputs from A into single parquet files that can be read in for tables / figures
##----------------------------------------------------------------

##----------------------------------------------------------------
## 0. Clear environment and set library paths
##----------------------------------------------------------------
rm(list = ls())

pacman::p_load(arrow, data.table, dplyr)

library(lbd.loader, lib.loc = sprintf("/share/geospatial/code/geospatial-libraries/lbd.loader-%s", R.version$major))
if("dex.dbr"%in% (.packages())) detach("package:dex.dbr", unload=TRUE)
library(dex.dbr, lib.loc = lbd.loader::pkg_loc("dex.dbr"))
suppressMessages(devtools::load_all(path = "/ihme/homes/idrisov/repo/dex_us_county/"))
'%nin%' <- Negate('%in%')
source("/ihme/cc_resources/libraries/current/r/get_location_metadata.R")


# Load in packages stored in repo
user_lib <- file.path("/ihme/homes/idrisov/repo/Aim2/Y_Utilities/R_Packages/")
.libPaths(c(user_lib, .libPaths()))
library(ggpol)
library(tidycensus)

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

# Function to ensure the logs directory exists
ensure_dir_exists <- function(dir_path) {
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
}

##----------------------------------------------------------------
## 0.2 Input / Output Directories
##----------------------------------------------------------------
# Input directory
dex_date_folder <- "20250930"
dex_dir <- paste0("/ihme/homes/idrisov/aim_outputs/Aim2/A_data_preparation/", dex_date_folder, "/")

# Output directory
date_folder <- format(Sys.Date(), "%Y%m%d")
out_dir <- paste0("/ihme/homes/idrisov/aim_outputs/Aim2/B_aggregation/", date_folder, "/")

# Ensure output path exists
ensure_dir_exists(out_dir)

##----------------------------------------------------------------
## 0.3 Create FIPS & location_id code lookup table
##
## This is needed as we need to know the state from which each county belongs to
## USHD - "area" column corresponds to the "mcnty" column when level == "mcnty"
## "In the data, when level == 'mcnty' your data will match the mcnty column here, when its state it will match state, and natl is always 1"
##----------------------------------------------------------------
# Fips & location_id table
fp_map <- paste0(j, "/Project/us_counties/locations/counties/merged_counties.csv")
df_map <- read.csv(fp_map)

##----------------------------------------------------------------
## 1. Compile DEX data
##----------------------------------------------------------------
# Get the list of all CSV files from the input directory
files_list_dex <- list.files(dex_dir, pattern = "\\.parquet$", full.names = TRUE) 

# Create list to store dfs
list_dex_df <- list()

# Loop over each file - takes about ~ 1 minute
for (i in 1:length(files_list_dex)) {
  
  file <- files_list_dex[i]
  
  # message to show what we're reading in
  print(paste0("Data: ", file))
  print(paste0("File ", i, " of ", length(files_list_dex)))
  start <- Sys.time()
  
  # Available columns
  # c("year_id", "geo", "location_name", "fips", "payer", "toc", 
  #   "acause", "cause_name", "age_group_years_start", "age_name", 
  #   "sex_id", "sex_name", "spend_mean", "spend_lower", "spend_upper", 
  #   "spend_per_capita_mean", "spend_per_capita_lower", "spend_per_capita_upper", 
  #   "spend_per_bene_mean", "spend_per_bene_lower", "spend_per_bene_upper", 
  #   "spend_per_vol_mean", "spend_per_vol_lower", "spend_per_vol_upper", 
  #   "vol_per_capita_mean", "vol_per_capita_lower", "vol_per_capita_upper", 
  #   "vol_per_bene_mean", "vol_per_bene_lower", "vol_per_bene_upper"
  # )
  
  # read in dataset
  df <- open_dataset(file) %>%
    filter(geo == "county") %>%
    filter(!is.na(spend_mean)) %>%
    filter(spend_mean > 0) %>%
    select("year_id", "geo", "location_name", "fips", "payer", "toc", 
             "acause", "cause_name", "age_group_years_start", "age_name", 
             "sex_id", "sex_name", "spend_mean", "spend_lower", "spend_upper") %>%
    collect() %>%
    as.data.frame()
  
  # Add dataframe to list
  list_dex_df[[length(list_dex_df) + 1]] <- df
  
  # Print time
  message("Done in: ", Sys.time() - start)
  
  # cleanup
  rm(df)
}

# --- Combine all dfs from df_list into one df ---
df_all <- rbindlist(list_dex_df, use.names = TRUE, fill = TRUE)

##----------------------------------------------------------------
## 1.1 Save DEX data
##----------------------------------------------------------------
fp_dex_output <- paste0(out_dir, "compiled_dex_data_2010_2019.parquet")
write_parquet(df_all, fp_dex_output)

##----------------------------------------------------------------
## 2. Compile USHD MX Ratio Data
##----------------------------------------------------------------


##----------------------------------------------------------------
## 2.1 Save USHD MX Ratio Data
##----------------------------------------------------------------















