##----------------------------------------------------------------
##' Title: A_worker_DEX_data.R
##'
##' Purpose: Launches jobs to pull / create DEX County level data
##----------------------------------------------------------------

##----------------------------------------------------------------
## 0. Clear environment and set library paths
##----------------------------------------------------------------
rm(list = ls())
pacman::p_load(data.table, arrow, tidyverse, glue)

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
  param_template <- data.table(y = 2019,
                               c = "_oral",
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
filename <- glue(out_dir, "/DEX_2010_2019_{toupper(param_template$c)}_{toupper(param_template$p)}_{param_template$y}_Y2025M02D07.parquet")

# Ensure output path exists
ensure_path(filename)

##----------------------------------------------------------------
## 2. Read in county / state / location / cause flat files
##----------------------------------------------------------------
county_names <- fread("/ihme/dex/us_county/maps/merged_counties1.csv")[current == 1, .(fips = cnty,location = as.character(mcnty), location_name = cnty_name)]
state_names <- fread("/ihme/dex/us_county/maps/states.csv")[, .(fips = state, location = abbreviation, location_name = state_name)]
state_names <- rbind(state_names, data.table(fips = 0, location = "USA", location_name = "United States"))
location_names <- rbind(county_names, state_names)
causelist <- fread("/ihme/dex/us_county/maps/causelist_figures.csv")[, .(acause, cause_name)]

##----------------------------------------------------------------
## 3. Read in DEX data
##----------------------------------------------------------------
tocs <- c("AM", "DV", "ED", "HH", "IP", "NF", "RX")
states <- unique(state_names$location)
geos <- c("county", "state", "national")

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
        group_by(location, year_id, acause, age_group_years_start, sex_id) %>%
        summarize(spend_mean = mean(spend),
                  spend_lower = quantile(spend, 0.025),
                  spend_upper = quantile(spend, 0.975),
                  vol_mean = mean(vol),
                  vol_lower = quantile(vol, 0.025),
                  vol_upper = quantile(vol, 0.975)) %>%
        collect() %>%
        setDT()
      tmp[, ':='(toc = t, state = s, payer = param_template$p, geo = g)]
      dt <- rbind(dt, tmp)
    }
  }
}

print(Sys.time()-start)
dt_loc_names <- merge(location_names, dt, by = "location", allow.cartesian=TRUE)
dt_cause_names <- merge(dt_loc_names, causelist, by = "acause")

#----------------------
# Add rate metrics
#---------------------
# - spend per capita
# - spend per vol
# - spend per beneficiary
# - vol per capita
# - vol per beneficiary

# Read in population denominators
benes <- open_dataset("/mnt/share/dex/us_county/03_post_model/pop_denom/18/denoms_true/data/") %>% 
  filter(year_id == param_template$y, toc == "all", type == "total", pri_payer == param_template$p) %>% 
  mutate(denom = ifelse(pri_payer == "oop", pop, denom)) %>%
  select(payer = pri_payer, year_id, denom, pop, sex_id, age_group_years_start, geo, location) %>%
  group_by(year_id, payer, age_group_years_start, sex_id, geo, location) %>%
  summarize(denom = sum(denom),
            pop = sum(pop)) %>%
  collect() %>%
  setDT()

dt_final <- merge(dt_cause_names, benes, by = c("year_id", "payer", "geo", "location", "age_group_years_start", "sex_id"))

# Create per capita and per bene rates (round for cleanliness)
dt_final[, ':='(
  spend_mean = round(spend_mean,2),
  spend_lower = round(spend_lower, 2),
  spend_upper = round(spend_upper, 2),
  # spending per capita
  spend_per_capita_mean = round(spend_mean/pop,2),
  spend_per_capita_lower = round(spend_lower/pop,2),
  spend_per_capita_upper = round(spend_upper/pop,2),
  # spending per encounter
  spend_per_vol_mean = round(spend_mean/vol_mean,2),
  spend_per_vol_lower = round(spend_lower/vol_lower,2),
  spend_per_vol_upper = round(spend_upper/vol_upper,2),
  # spending per beneficiary
  spend_per_bene_mean = round(spend_mean/denom,2),
  spend_per_bene_lower = round(spend_lower/denom,2),
  spend_per_bene_upper = round(spend_upper/denom,2),
  # encounters per capita (per 1,000)
  vol_per_capita_mean = round((vol_mean/pop)*1000,2),
  vol_per_capita_lower = round((vol_lower/pop)*1000,2),
  vol_per_capita_upper = round((vol_upper/pop)*1000,2),
  # encounters per beneficiary (per 1,000)
  vol_per_bene_mean = round((vol_mean/denom)*1000,2),
  vol_per_bene_lower = round((vol_lower/denom)*1000,2),
  vol_per_bene_upper = round((vol_upper/denom)*1000,2)
)]


# Masking estimates with large uncertainty
cols <- c("spend", "spend_per_capita", "spend_per_bene", "spend_per_vol", "vol_per_capita", "vol_per_bene")
for (col in cols) {
  mean_col <- paste0(col, "_mean")
  lower_col <- paste0(col, "_lower")
  upper_col <- paste0(col, "_upper")
  
  # Mask if the uncertainty range > mean
  dt_final[, (c(mean_col, lower_col, upper_col)) := 
              .SD[, lapply(.SD, function(x) ifelse(get(upper_col) - get(lower_col) > get(mean_col), NA_real_, x)), 
                  .SDcols = c(mean_col, lower_col, upper_col)]]
}

# We don't produce per beneficiary estimates for out-of-pocket payers
if(param_template$p =="oop"){
  dt_final[,':='(spend_per_bene_mean = NA,
                spend_per_bene_lower = NA,
                spend_per_bene_upper = NA,
                vol_per_bene_mean = NA,
                vol_per_bene_lower = NA,
                vol_per_bene_upper = NA)]
}
print(Sys.time()-start)

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
  spend_mean,
  spend_lower,
  spend_upper,
  spend_per_capita_mean,
  spend_per_capita_lower,
  spend_per_capita_upper,
  spend_per_bene_mean,
  spend_per_bene_lower,
  spend_per_bene_upper,
  spend_per_vol_mean,
  spend_per_vol_lower,
  spend_per_vol_upper,
  vol_per_capita_mean,
  vol_per_capita_lower,
  vol_per_capita_upper,
  vol_per_bene_mean,
  vol_per_bene_lower,
  vol_per_bene_upper
)]

##----------------------------------------------------------------
## 4. Save outputs
##----------------------------------------------------------------
# make sure you're on an archive node (may only apply if we are saving to the archive)
write_parquet(dt_final2, filename)
