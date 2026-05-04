##----------------------------------------------------------------
##' Title: A_worker_DEX_data.R
##'
##' Purpose: Launches jobs to pull / create DEX County level data
##' 
##' Notes: -Masking estimates with large uncertainty - COMMENTED OUT - MASKING RESULTS IN NA VALUES, SAFE TO USE UNMASKED DATA
##----------------------------------------------------------------

##----------------------------------------------------------------
## 0. Clear environment and set library paths
##----------------------------------------------------------------
rm(list = ls())
pacman::p_load(data.table, arrow, tidyverse, glue, conflicted)
conflicts_prefer(dplyr::summarize)
conflicts_prefer(dplyr::filter)

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
## 1. Arguments / output path
##----------------------------------------------------------------
scaled_version <- 102

# Interactive
if(interactive()) {
  param_template <- data.table(y = 2015,
                               c = "hiv",
                               p = "mdcr")
} else {
  # Non-interactive
  args <- commandArgs(trailingOnly = TRUE)
  message(args)
  param_path <- args[1]
  task_id <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
  param_template <- fread(param_path)[task_id]
}

# old params, kept for reference
# out_dir <- "/snfs1/DATA/Incoming Data/IHME/RT_COUNTY_SPENDING_2010_2019"
# filename <- glue(out_dir, "/FULL_ESTIMATES/IHME_USA_HEALTH_CARE_SPENDING_CAUSE_COUNTY_2010_2019_{toupper(param_template$c)}_{toupper(param_template$p)}_{param_template$y}_Y2025M02D07.CSV")

# Set output filename
date_folder <- format(Sys.Date(), "%Y%m%d")
out_dir <- paste0("/ihme/homes/idrisov/aim_outputs/Aim2/A_data_preparation/", date_folder, "/DEX/")
filename <- glue(out_dir, "/DEX_2010_2019_{toupper(param_template$c)}_{toupper(param_template$p)}_{param_template$y}_Y2025M02D07_draw.parquet")

# Ensure output path exists
ensure_path(filename)

##----------------------------------------------------------------
## 2. Read in county / state / location / cause flat files
##----------------------------------------------------------------
county_names <- fread("/ihme/dex/us_county/maps/merged_counties1.csv")[current == 1, .(fips = cnty,location = as.character(mcnty), location_name = cnty_name)]
state_names <- fread("/ihme/dex/us_county/maps/states.csv")[, .(fips = state, location = abbreviation, location_name = state_name)]
state_names <- rbind(state_names, data.table(fips = 0, location = "USA", location_name = "United States"))
location_names <- rbind(county_names, state_names)
causelist <- fread("/ihme/dex/us_county/maps/causelist_figures_archive.csv")[, .(acause, cause_name)]

##----------------------------------------------------------------
## 3. Read in DEX data
##----------------------------------------------------------------
tocs <- c("AM", "DV", "ED", "HH", "IP", "NF", "RX")
states <- unique(state_names$location)
geos <- c("state", "national")

data_dir <- paste0("/ihme/dex/us_county/04_final/scaled_version_",scaled_version,"/data/")

start <- Sys.time()
dt <- data.table()
for(t in tocs){
  for(s in states){
    for(g in geos){
      print(paste(t, s, g))
      # county level files where payer = oop are saved with different file names (ex. year_2016_neo_lymphoma-0.parquet)
      parent_dir <- paste0(data_dir,"/geo=",g,"/toc=",t, "/state=",s,"/payer=",param_template$p, "/")
      if(param_template$p == "oop" & s %in% c("TX", "GA", "TN", "VA") & g == "county"){
        file = paste0(parent_dir, "year_", param_template$y, "_", param_template$c, "-0.parquet")
      } else {
        file = paste0(parent_dir, "year_", param_template$y, "-0.parquet")
      }
      if(!file.exists(file)){
        next
      }
      tmp <-  arrow::open_dataset(file) %>% 
        filter(acause == param_template$c) %>%
        group_by(location, year_id, acause, age_group_years_start, sex_id,draw) %>%
        summarize(spend = sum(spend),
                  vol = sum(vol)) %>%
        collect() %>%
        setDT()
      tmp[, ':='(toc = t, state = s, payer = param_template$p, geo = g)]
      dt <- rbind(dt, tmp)
    }
  }
}

# Merge with location and acause names
dt_loc_names <- merge(location_names, dt, by = "location", allow.cartesian=TRUE)
dt_cause_names <- merge(dt_loc_names, causelist, by = "acause")

# Set age_name based off of age_group_years_start
dt_final <- copy(dt_cause_names)

dt_final[, ':='(sex_name = ifelse(sex_id == 1, "Male", "Female"), 
                age_name = fcase(
                  age_group_years_start == 0, "0 - <1",
                  age_group_years_start == 1, "1 - <5",
                  age_group_years_start == 5, "5 - <10",
                  age_group_years_start == 10, "10 - <15",
                  age_group_years_start == 15, "15 - <20",
                  age_group_years_start == 20, "20 - <25",
                  age_group_years_start == 25, "25 - <30",
                  age_group_years_start == 30, "30 - <35",
                  age_group_years_start == 35, "35 - <40",
                  age_group_years_start == 40, "40 - <45",
                  age_group_years_start == 45, "45 - <50",
                  age_group_years_start == 50, "50 - <55",
                  age_group_years_start == 55, "55 - <60",
                  age_group_years_start == 60, "60 - <65",
                  age_group_years_start == 65, "65 - <70",
                  age_group_years_start == 70, "70 - <75",
                  age_group_years_start == 75, "75 - <80",
                  age_group_years_start == 80, "80 - <85",
                  age_group_years_start == 85, "85+"))]

dt_final2 <- dt_final[,.(
  year_id,
  geo,
  location_name,
  fips,
  payer,
  toc,
  acause,
  cause_name,
  age_group_years_start,
  age_name,
  sex_id,
  sex_name,
  draw,
  spend
)]

##----------------------------------------------------------------
## 4. Save outputs
##----------------------------------------------------------------
# make sure you're on an archive node (may only apply if we are saving to the archive)
write_parquet(dt_final2, filename)
