##----------------------------------------------------------------
##' Title: A_GBD_data.R
##'
##' Purpose: Pulls and saves Mortality, Prevalence, DALY, Incidence, YLD, YLL, and Population data needed for C Frontier Analysis
##' Use: Run script from start to finish, data will save to Aim2/A_data_preparation/FA/<current_date>
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

# Source IHME shared functions
source("/ihme/cc_resources/libraries/current/r/get_outputs.R")
source("/ihme/cc_resources/libraries/current/r/get_population.R")
source("/ihme/cc_resources/libraries/current/r/get_location_metadata.R")
source("/ihme/cc_resources/libraries/current/r/get_age_metadata.R")

##----------------------------------------------------------------
## 0.1 Functions
##----------------------------------------------------------------
# Function to ensure filepath / folders exist
ensure_path <- function(filepath) {
  if (!dir.exists(filepath)) {
    dir.create(filepath, recursive = TRUE, showWarnings = FALSE)
  }
  return(filepath)
}

##----------------------------------------------------------------
## 0.2 Set output directory
##----------------------------------------------------------------
date_today <- format(Sys.Date(), "%Y%m%d")
dir_out <- paste0("/ihme/homes/idrisov/aim_outputs/Aim2/A_data_preparation/", date_today, "/GBD/")

ensure_path(dir_out)

##----------------------------------------------------------------
## 1. Set Parameters
##----------------------------------------------------------------
# Release ID, 16 for GBD 2023
rel_id <- 16

# Years
year_ids <- c(2010:2019)

# Cause_ids
cause_ids <- c(298, 973) # hiv = 298, _subs = 973

# Sex_ids
sex_ids <- c(1,2)

# Measure
# 5 = Prevalence, 1 = Deaths, 

# Metric
# 1 = Number, 3 = Rate (per 100,000)

# Location_ids
df_loc <- get_location_metadata(location_set_id = 35,
                                release_id = 16)

df_state_loc_ids <- df_loc %>%
  filter(parent_id == 102) %>%
  select(location_name, location_id)

list_state_loc_ids <- df_state_loc_ids$location_id

# Age Group ids
df_age_groups <- get_age_metadata(release_id = 16)

##----------------------------------------------------------------
## 2. Pull GBD Data

# Prevalence count
# Prevalence rate
# Mortality count
# Mortality rate
# Incidence count
# Incidence rate
# DALY count
# DALY rate
# YLL count
# YLL rate

##----------------------------------------------------------------
# Set arguments
args_get_outputs <- list(topic = "cause", 
                         release_id=rel_id,
                         location_id=list_state_loc_ids, 
                         year_id=year_ids, 
                         age_group_id="all",
                         sex_id=sex_ids, 
                         #measure_id=5, # 5 = Prevalence
                         #metric_id=1, # 1 = Number, # 3 = Rate
                         cause_id=cause_ids, 
                         location_set_id=35)

# Pull all counts (will make rates from collapsed counts later)
df_prevalence_counts <- do.call(get_outputs, c(args_get_outputs, list(measure_id = 5, metric_id = 1)))
df_mortality_counts <- do.call(get_outputs, c(args_get_outputs, list(measure_id = 1, metric_id = 1)))
df_daly_counts <- do.call(get_outputs, c(args_get_outputs, list(measure_id = 2, metric_id = 1)))
df_incidence_counts <- do.call(get_outputs, c(args_get_outputs, list(measure_id = 6, metric_id = 1)))
df_yll_counts <- do.call(get_outputs, c(args_get_outputs, list(measure_id = 4, metric_id = 1)))
df_yld_counts <- do.call(get_outputs, c(args_get_outputs, list(measure_id = 3, metric_id = 1)))

df_list <- list(df_prevalence_counts,
                df_mortality_counts,
                df_daly_counts,
                df_incidence_counts,
                df_yll_counts,
                df_yld_counts
                )

df_list_val_label <- c("prevalence_counts",
                       "mortality_counts",
                       "daly_counts",
                       "incidence_counts",
                       "yll_counts",
                       "yld_counts"
                       )

df_gbd <- data.frame()

# Loop through each df, filter columns, relabel "val" column to respective measure_counts, join together
for (i in 1:length(df_list)) {
  df <- df_list[[i]]
  
  df <- df %>%
    select(c(age_group_id, cause_id, location_id, 
             sex_id, year_id, acause, age_group_name, cause_name, location_name, val))
  
  df <- df %>%
    rename(
      !!df_list_val_label[i] := "val"
    )
  
  if (i == 1) {
    df_gbd <- df
  } else {
    df_gbd <- left_join(
      x = df_gbd,
      y = df,
      by = c("age_group_id", "age_group_name", "cause_id", "acause", "cause_name", "location_id", "location_name", "sex_id", "year_id"),
    )
  }
}


# Pull Population data
df_population <- get_population(release_id = rel_id,
                                age_group_id = "all",
                                location_id = list_state_loc_ids,
                                location_set_id = 35,
                                year_id = year_ids,
                                sex_id = sex_ids
                                )

# Join to rest of GBD data
df_gbd <- left_join(
  x = df_gbd,
  y = df_population %>% select(!("run_id")),
  by = c("age_group_id", "location_id", "year_id", "sex_id")
)


# Check where we have NAs
# View(df_m[!complete.cases(df_m), ]) # Looks like we only have NA data for Early and Late Neonatal for HIV, otherwise all other rows have data

# Collapse on the 0 - <1 and 1 - <5 age groups 
# Age groups in the DEX / USHD data
# c("0 - <1", "1 - <5", "10 - <15", "15 - <20", "20 - <25", "25 - <30", 
#   "30 - <35", "35 - <40", "40 - <45", "45 - <50", "5 - <10", "50 - <55", 
#   "55 - <60", "60 - <65", "65 - <70", "70 - <75", "75 - <80", "80 - <85", 
#   "85+")

# Label the 0 - <1, 1 - <5, & 85+ age groups 
df_age_collapse <- df_gbd %>%
  mutate(age_name_group = case_when(
    age_group_id %in% c(2, 3, 388, 389) ~ "0 - <1",
    age_group_id %in% c(238, 34) ~ "1 - <5",
    age_group_id %in% c(31, 32, 235) ~ "85+"
  ))

df_age_non_collapse <- df_age_collapse %>%
  filter(is.na(age_name_group))

# Collapse on the 0 - <1, 1 - <5, & 85+ age groups 
df_age_collapse <- df_age_collapse %>%
  filter(!is.na(age_name_group)) %>%
  group_by(age_name_group, cause_id, location_id, sex_id, 
             year_id, acause, cause_name, location_name) %>%
  summarize(
    prevalence_counts = sum(prevalence_counts),
    mortality_counts= sum(mortality_counts, na.rm = TRUE),
    daly_counts = sum(daly_counts),
    incidence_counts = sum(incidence_counts, na.rm = TRUE),
    yll_counts = sum(yll_counts, na.rm = TRUE),
    yld_counts = sum(yld_counts, na.rm = TRUE),
    population = sum(population)
  )

df_age_collapse <- df_age_collapse %>%
  rename(
    "age_group_name" = "age_name_group"
  )

# Rowbind back the df age collapse data
df_final <- rbind(df_age_collapse, df_age_non_collapse)

df_final <- df_final %>%
  select(!c("age_name_group", "age_group_id"))

##----------------------------------------------------------------
## 3. Save data
##----------------------------------------------------------------
# Prevalence, Mortality, & Population data
fn_main <- file.path(dir_out, "df_gbd.parquet")

write_parquet(df_final, fn_main)

##----------------------------------------------------------------
## SCRATCH SPACE - SAFE TO DELETE
##----------------------------------------------------------------
# # Testing pulling covariates
# source("/ihme/cc_resources/libraries/current/r/get_covariate_estimates.R")
# 
# 
# 
# df_cov <- get_covariate_estimates(covariate_id = 2604, # HIV ART coverage
#                                    location_id = list_state_loc_ids,
#                                    location_set_id=35,
#                                    year_id =  year_ids,
#                                    release_id = 3,
#                                    sex_id=sex_ids,
#                                    age_group_id="all",
#                                    status = 'best') 









