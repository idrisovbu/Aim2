##----------------------------------------------------------------
##' Title: A_Summary_Tables.R
##'
##' Purpose: Creates tables for Summary Level data for Aim2
##----------------------------------------------------------------

##----------------------------------------------------------------
## 0. Clear environment and set library paths
##----------------------------------------------------------------
rm(list = ls())

pacman::p_load(dplyr, openxlsx, RMySQL, data.table, ini, DBI, tidyr, openxlsx, reticulate, ggpubr, arrow)
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

# Load in packages stored in repo
user_lib <- file.path(h, "/repo/Aim2/Y_Utilities/R_Packages/")
.libPaths(c(user_lib, .libPaths()))
library(ggpol)
library(tidycensus)

##----------------------------------------------------------------
## 0. Functions
##----------------------------------------------------------------
# Ensure directory exists
ensure_dir_exists <- function(dir_path) {
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
}

# Convert to dollars
convert_to_dollars <- function(df, cols_to_convert) {
  for (col in colnames(df)) {
    if (col %in% cols_to_convert) {
      df[[col]] <- dollar(df[[col]])
    }
  }
  return(df)
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
#   "sex_id", "sex_name", "spend_mean", "spend_lower", "spend_upper", 
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
dir_output_tables <- file.path(dir_output, "A_Tables/")
dir_output_tables_dated <- file.path(dir_output, "A_Tables/", date_today)
parquet_storage <- file.path(dir_output, "Y_Utilities/parquet_storage/")

ensure_dir_exists(dir_output)
ensure_dir_exists(dir_output_tables)
ensure_dir_exists(dir_output_tables_dated)
ensure_dir_exists(parquet_storage)

##----------------------------------------------------------------
## 0.2 Read in data
##----------------------------------------------------------------

################## HIV Data

# Read in saved parquet file, or read in CSV files
bool_hiv_parquet <- TRUE

if (bool_hiv_parquet) {
  # Save combined_df_sud as parquet file for faster reading, read in if trying to save time
  combined_df_hiv <- read_parquet(file.path(parquet_storage, "combined_df_hiv.parquet"))
  
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
  combined_df_hiv <- rbindlist(
    lapply(
      files_hiv,
      function(f) fread(f, select = cols_of_int_1_hiv, showProgress = TRUE)
    ),
    fill = TRUE
  )
  
  # Filter for county, remove "oop" data, remove NA data, remove spending == 0
  combined_df_hiv <- combined_df_hiv %>%
    filter(geo == "county") %>%
    filter(payer != "oop") %>%
    filter(!is.na(spend_mean)) %>%
    filter(spend_mean > 0)
  
  # Save combined_df_hiv as parquet file for faster reading, read in if trying to save time
  write_parquet(combined_df_hiv, file.path(parquet_storage, "combined_df_hiv.parquet"))
}

################## SUD Data

# Read in saved parquet file, or read in CSV files
bool_sud_parquet <- TRUE

if (bool_sud_parquet) {
  # Save combined_df_sud as parquet file for faster reading, read in if trying to save time
  combined_df_sud <- read_parquet(file.path(parquet_storage, "combined_df_sud.parquet"))
  
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
  combined_df_sud <- rbindlist(
    lapply(
      files_sud,
      function(f) fread(f, select = cols_of_int_2_sud, showProgress = FALSE)
    ),
    fill = TRUE
  )
  
  # Filter for county, remove "oop" data, remove NA data, remove spending == 0
  combined_df_sud <- combined_df_sud %>%
    filter(geo == "county") %>%
    filter(payer != "oop") %>%
    filter(!is.na(spend_mean)) %>%
    filter(spend_mean > 0)
  
  # Save combined_df_sud as parquet file for faster reading, read in if trying to save time
  write_parquet(combined_df_sud, file.path(parquet_storage, "combined_df_sud.parquet"))
}

##----------------------------------------------------------------
## 0.3 Create FIPS code lookup table
##
## This is needed as we need to know the state from which each county
## belongs to, and the FIPS codes in the data are missing a left padded "0"
## which needs to be added back in to match on the state_name
##----------------------------------------------------------------
df_fips_lookup <- fips_codes

df_fips_lookup <- df_fips_lookup %>%
  mutate(full_fips_code = paste0(state_code, county_code))

##----------------------------------------------------------------
## 1. Table 1 - HIV
## What are the top 10 most expensive and least expensive counties
## in terms of spending for patients with HIV (all age groups all years all toc combined)?
##
## TODO - needs age standardizing, make ... in all the values to represent a jump to the bottom
##----------------------------------------------------------------

# Fix FIPS codes 
df_t1 <- combined_df_hiv %>%
  mutate(
    fips = as.character(fips),             # step 1: convert to character
    fips = ifelse(nchar(fips) == 4,    # step 2: pad 4-digit strings
                      paste0("0", fips),
                  fips)
  ) 

df_t1 <- left_join(x = df_t1, y = df_fips_lookup, by = c("fips" = "full_fips_code"))

# Group by then calculate spend_mean
df_t1 <- df_t1 %>%
  group_by(state_name, location_name) %>%
  summarize("spend_mean" = mean(spend_mean))

# Get top 10 and bottom 10
df_t1_top <- df_t1 %>% 
  arrange(desc(spend_mean)) %>%
  ungroup() %>%
  mutate(rank = as.character(row_number())) %>%
  head(10)

df_t1_bot <- df_t1 %>% 
  arrange(desc(spend_mean)) %>%
  ungroup() %>%
  mutate(rank = as.character(row_number())) %>%
  tail(10)

# Convert to dollars
df_t1_top <- convert_to_dollars(df_t1_top, "spend_mean")
df_t1_bot <- convert_to_dollars(df_t1_bot, "spend_mean")

# Add ... row to top data
ellipsis_row <- tibble(
  state_name   = "...",
  location_name = "...",
  spend_mean  = "...",
  rank = "..."
)

# Bind it in
df_t1_all <- bind_rows(df_t1_top, ellipsis_row, df_t1_bot)

# Arrange columns to desired output
df_t1_all <- df_t1_all %>%
  select(rank, state_name, location_name, spend_mean) %>%
  setnames(old = c("rank", "state_name", "location_name", "spend_mean"), 
           new = c("Rank", "State", "County", "Estimated Average Spending HIV (USD)"))

# Write to CSV
write.csv(df_t1_all, file.path(dir_output_tables_dated, "T1_HIV_top_bottom_10.csv"), row.names = FALSE)


##----------------------------------------------------------------
## 2. Table 2 - SUD
## What are the top 10 most expensive and least expensive counties
## in terms of spending for patients with SUD (all age groups all years all toc combined)?
##
## TODO - needs age standardizing
##----------------------------------------------------------------

# Fix FIPS codes 
df_t2 <- combined_df_sud %>%
  mutate(
    fips = as.character(fips),             # step 1: convert to character
    fips = ifelse(nchar(fips) == 4,    # step 2: pad 4-digit strings
                  paste0("0", fips),
                  fips)
  ) 

df_t2 <- left_join(x = df_t2, y = df_fips_lookup, by = c("fips" = "full_fips_code"))

# Group by then calculate spend_mean
df_t2 <- df_t2 %>%
  group_by(state_name, location_name) %>%
  summarize("spend_mean" = mean(spend_mean))

# Get top 10 and bottom 10
df_t2_top <- df_t2 %>% 
  arrange(desc(spend_mean)) %>%
  ungroup() %>%
  mutate(rank = as.character(row_number())) %>%
  head(10)

df_t2_bot <- df_t2 %>% 
  arrange(desc(spend_mean)) %>%
  ungroup() %>%
  mutate(rank = as.character(row_number())) %>%
  tail(10)

# Convert to dollars
df_t2_top <- convert_to_dollars(df_t2_top, "spend_mean")
df_t2_bot <- convert_to_dollars(df_t2_bot, "spend_mean")

# Add ... row to top data
ellipsis_row <- tibble(
  state_name   = "...",
  location_name = "...",
  spend_mean  = "...",
  rank = "..."
)

# Bind it in
df_t2_all <- bind_rows(df_t2_top, ellipsis_row, df_t2_bot)

# Arrange columns to desired output
df_t2_all <- df_t2_all %>%
  select(rank, state_name, location_name, spend_mean) %>%
  setnames(old = c("rank", "state_name", "location_name", "spend_mean"), 
           new = c("Rank", "State", "County", "Estimated Average Spending SUD (USD)"))

# Write to CSV
write.csv(df_t2_all, file.path(dir_output_tables_dated, "T2_SUD_top_bottom_10.csv"), row.names = FALSE)













