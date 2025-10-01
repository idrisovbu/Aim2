##----------------------------------------------------------------
##' Title: Aim 2 - Medicaid Testing
##'
##' Purpose: tests out ideas for Aim 2 - Medicaid
##----------------------------------------------------------------

##----------------------------------------------------------------
## 0. Clear environment and set library paths
##----------------------------------------------------------------
rm(list = ls())

pacman::p_load(dplyr, openxlsx, RMySQL, data.table, ini, DBI, tidyr, openxlsx, reticulate)
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
## 1. Create parameters CSV file - Medicaid data
##----------------------------------------------------------------
# Dynamically read in directories where data is stored to see what data they have in the first place
# Create a table based off their directory hierarchy (use code to scan folder directories to determine available data)
run_id <- 77
#fp_input_data <- paste0("/mnt/share/limited_use/LU_CMS/DEX/01_pipeline/MDCD/run_",run_id,"/PRIMARY_CAUSE/")
#fp_input_data <- paste0("/mnt/share/limited_use/LU_CMS/DEX/01_pipeline/MDCD/run_",run_id,"/PRIMARY_CAUSE/data/toc=AM/year=2000/age_group_years_start=30/")
fp_input_data <- paste0("/mnt/share/limited_use/LU_CMS/DEX/01_pipeline/MDCD/run_77/PRIMARY_CAUSE/data/toc=AM/year_id=2000/")
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

# Filter out ages below 60 and above 85
# df_list_input_data_subset <- df_list_input_data_subset %>%
#   filter(
#     age_group_years_start >= 10,
#     age_group_years_start <= 55
#   )

# # Tabular visualiaztion of available TOC per year
# df_list_input_data_subset %>%
#   group_by(year_id) %>%
#   summarise(toc_values = paste(sort(unique(toc)), collapse = ", ")) %>%
#   arrange(year_id)

# Save the filtered parameters to CSV save list as .csv for runner script to read in based off job
#fp_parameters <- paste0(l, "/LU_CMS/DEX/hivsud/aim1/resources_aim1/A1_f2t_parameters_aims1.csv")
#write.csv(df_list_input_data_subset, file = fp_parameters, row.names = FALSE)

# Try reading Medicaid data
i <- 30
row <- df_list_input_data_subset[i, ]
dir <- row$directory

# message to show what we're reading in
print(paste0("Data: ", dir))
start <- Sys.time()

# read in dataset
df <- open_dataset(dir) %>%
  # filter(ENHANCED_FIVE_PERCENT_FLAG == "Y") %>% # Filters on 5% random sample column
  # filter(pri_payer == 1) %>%
  # filter(mc_ind == 0) %>%
  # select(bene_id, encounter_id, acause, primary_cause, race_cd, sex_id, tot_pay_amt, st_resi) %>%
  collect() %>%
  as.data.frame()

# Add metadata based on path
df$toc <- row$toc
df$year_id <- row$year_id 
df$age_group_years_start <- row$age_group_years_start
df$code_system <- row$code_system













