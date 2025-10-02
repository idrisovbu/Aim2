##----------------------------------------------------------------
##' Title: B_data_aggregator.R
##'
##' Purpose: Aggregates outputs from A into single parquet files that can be read in for tables / figures
##' 
##' How to run: Modify the "0.2 Input / Output Directories" section's "dex_date_folder" & "ushd_date_folder" variables to the correct dates
##'             and run the entire script, changing any other parts as necessary.
##'             
##'             Saves compiled DEX & USHD worker script outputs as a single parquet file.
##----------------------------------------------------------------

##----------------------------------------------------------------
## 0. Clear environment and set library paths
##----------------------------------------------------------------
rm(list = ls())

pacman::p_load(arrow, data.table, dplyr)
'%nin%' <- Negate('%in%')

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

source("/ihme/cc_resources/libraries/current/r/get_cause_metadata.R")
source("/ihme/cc_resources/libraries/current/r/get_age_metadata.R")

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
dex_dir <- paste0("/ihme/homes/idrisov/aim_outputs/Aim2/A_data_preparation/", dex_date_folder, "/DEX/")

ushd_date_folder <- "20251001"
ushd_dir <- paste0("/ihme/homes/idrisov/aim_outputs/Aim2/A_data_preparation/", ushd_date_folder, "/USHD/")

# Output directory
date_folder <- format(Sys.Date(), "%Y%m%d")
out_dir <- paste0("/ihme/homes/idrisov/aim_outputs/Aim2/B_aggregation/", date_folder, "/")

# Output filepaths
fp_dex_output <- paste0(out_dir, "compiled_dex_data_2010_2019.parquet")
fp_ushd_output <- paste0(out_dir, "compiled_ushd_data_2010_2019.parquet")

# Ensure output path exists
ensure_dir_exists(out_dir)

##----------------------------------------------------------------
## 0.3 Create FIPS & location_id code lookup table
##
## This is needed as we need to know the state from which each county belongs to
## USHD - "area" column corresponds to the "mcnty" column when level == "mcnty"
## "In the data, when level == 'mcnty' your data will match the mcnty column here, when its state it will match state, and natl is always 1"
##----------------------------------------------------------------
# County, State, & location_id table
fp_ushd_mapping <- paste0(j, "/Project/us_counties/locations/counties/merged_counties.csv")
df_ushd_mapping <- read.csv(fp_ushd_mapping)
df_ushd_mapping <- df_ushd_mapping %>% rename(
  state_id = state
)

# FIPS
df_fips_lookup <- fips_codes

df_fips_lookup <- df_fips_lookup %>%
  mutate(full_fips_code = paste0(state_code, county_code))

df_fips_lookup <- df_fips_lookup %>% rename(
  state_abbr = state
)

# Merge FIPS & USHD loc_id table
df_loc_ids <- left_join(x = df_ushd_mapping, y = df_fips_lookup, by = c("state_name", "cnty_name" = "county"))

# Save file for use in C & D
write.csv(df_loc_ids, "/ihme/homes/idrisov/aim_outputs/Aim2/R_resources/county_fips_locs.csv")

##----------------------------------------------------------------
## 0.4 Read in IHME age & cause metadata for cause ids, age_group_names
##----------------------------------------------------------------
cause_id_list <- get_cause_metadata(cause_set_id = 2, release_id = 16, include_all_metadata = TRUE)

df_ages <- data.frame(
  age_name = c("0 - <1", "1 - <5", "5 - <10", "10 - <15", "15 - <20",
               "20 - <25", "25 - <30", "30 - <35", "35 - <40", "40 - <45",
               "45 - <50", "50 - <55", "55 - <60", "60 - <65", "65 - <70",
               "70 - <75", "75 - <80", "80 - <85", "85+", "98", "99"),
  age_group_years_start = c(0, 1, 5, 10, 15,
                            20, 25, 30, 35, 40,
                            45, 50, 55, 60, 65,
                            70, 75, 80, 85, 98, 99)
)
                          

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
             "sex_id", "sex_name",
             "spend_mean", "spend_lower", "spend_upper") %>%
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
df_dex_all <- rbindlist(list_dex_df, use.names = TRUE, fill = TRUE)

# Merge with df_loc_ids to get location_ids & county names
df_dex_all <- left_join(x = df_dex_all, 
                             y = df_loc_ids %>% select(cnty, state_name, location_id, merged_location_id),
                             by = c("fips" = "cnty"))

# Rename location_name -> cnty_name
df_dex_all <- df_dex_all %>%
  rename(
   fips_ihme = fips,
    cnty_name = location_name
  )

# Reorder columns
col_order_dex <- c("state_name", "cnty_name", "fips_ihme", "location_id", "merged_location_id", "geo", 
                   "acause", "cause_name", "sex_id", "sex_name", "year_id",  
                   "age_group_years_start", "age_name", "payer", "toc",
                    "spend_mean", "spend_lower", "spend_upper"
                    )

df_dex_all <- df_dex_all %>% select(all_of(col_order_dex))
  
##----------------------------------------------------------------
## 1.1 Save DEX data
##----------------------------------------------------------------
write_parquet(df_dex_all, fp_dex_output)

##----------------------------------------------------------------
## 2. Compile USHD MX Ratio Data
##----------------------------------------------------------------
# Get the list of all CSV files from the input directory
files_list_ushd <- list.files(ushd_dir, pattern = "\\.parquet$", full.names = TRUE) 

# Create list to store dfs
list_ushd_df <- list()

# Loop over each file - takes about ~ 1 minute
for (i in 1:length(files_list_ushd)) {
  
  file <- files_list_ushd[i]
  
  # message to show what we're reading in
  print(paste0("Data: ", file))
  print(paste0("File ", i, " of ", length(files_list_ushd)))
  start <- Sys.time()
  
  # Available columns
  # c("cause_id", "year", "sex", "race", "edu", "pred_mean", "pred_lb", 
  #   "pred_median", "pred_ub", "pred_se", "level", "area", "age", 
  #   "measure_id")
  
  # read in dataset
  df <- open_dataset(file) %>%
    collect() %>%
    as.data.frame()
  
  # Add dataframe to list
  list_ushd_df[[length(list_ushd_df) + 1]] <- df
  
  # Print time
  message("Done in: ", Sys.time() - start)
  
  # cleanup
  rm(df)
}

# --- Combine all dfs from df_list into one df ---
df_ushd_all <- rbindlist(list_ushd_df, use.names = TRUE, fill = TRUE)

# Convert "area" column into integer
df_ushd_all$area <- as.integer(df_ushd_all$area)

# Merge with df_loc_ids to get location_ids & county names
df_ushd_all <- left_join(x = df_ushd_all,
                         y = df_loc_ids %>% select(mcnty, state_name, cnty_name, location_id, merged_location_id, cnty),
                         by = c("area" = "mcnty"))

# Rename area -> cnty_name
df_ushd_all <- df_ushd_all %>%
  rename(
    fips_ihme = cnty
  )

# Merge to get cause names
df_ushd_all$cause_id <- as.integer(df_ushd_all$cause_id)

df_ushd_all <- left_join(x = df_ushd_all,
                         y = cause_id_list %>% select(cause_id, acause, cause_name),
                         by = "cause_id")

# Rename age -> age_group_years_start
df_ushd_all <- df_ushd_all %>%
  rename(
    age_group_years_start = age
  )

# Merge to get cause names
df_ushd_all <- left_join(x = df_ushd_all,
                         y = df_ages,
                         by = "age_group_years_start")

# Rename year -> year_id
df_ushd_all <- df_ushd_all %>%
  rename(
    year_id = year
  )

# Reorder columns
col_order_ushd <- c("state_name", "cnty_name", "fips_ihme", "location_id", "merged_location_id", "level",
                    "cause_id", "acause", "cause_name", "sex", "year_id",
                    "age_group_years_start", "age_name",  "race", "edu", "area",  "measure_id", 
                    "pred_mean", "pred_lb","pred_median", "pred_ub", "pred_se"
)

df_ushd_all <- df_ushd_all %>% select(all_of(col_order_ushd))

##----------------------------------------------------------------
## 2.1 Save USHD MX Ratio Data
##----------------------------------------------------------------
write_parquet(df_ushd_all, fp_ushd_output)



