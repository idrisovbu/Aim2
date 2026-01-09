##----------------------------------------------------------------
##' Title: A_FA_data.R
##'
##' Purpose: Pulls and saves Mortality, Prevalence, and Population data needed for C Frontier Analysis
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
dir_out <- paste0("/ihme/homes/idrisov/aim_outputs/Aim2/A_data_preparation/", date_today, "/FA/")

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
## 2. Pull Data
##----------------------------------------------------------------
# Prevalence
df_prevalence <- get_outputs(topic = "cause", 
                             release_id=rel_id,
                             location_id=list_state_loc_ids, 
                             year_id=year_ids, 
                             age_group_id="all",
                             sex_id=sex_ids, 
                             measure_id=5, # 5 = Prevalence
                             metric_id=1, # 1 = Number
                             cause_id=cause_ids, 
                             location_set_id=35
                             )

df_prevalence <- df_prevalence %>%
  select(c("age_group_id", "cause_id", "location_id", "measure_id", "metric_id", 
           "sex_id", "year_id", "acause", "age_group_name", "cause_name", "location_name", "val"))

df_prevalence <- df_prevalence %>%
  rename(
    "prevalence" = "val"
  )

# Mortality
df_mortality <- get_outputs(topic = "cause", 
                             release_id=rel_id, 
                             location_id=list_state_loc_ids,
                             year_id=year_ids, 
                             age_group_id="all", 
                             sex_id=sex_ids, 
                             measure_id=1, # 1 = Deaths
                             metric_id=1, # 1 = Number
                             cause_id=cause_ids, 
                             location_set_id=35
                             )

df_mortality <- df_mortality %>%
  select(c("age_group_id", "cause_id", "location_id", "measure_id", "metric_id", 
           "sex_id", "year_id", "acause", "age_group_name", "cause_name", "location_name", "val"))

df_mortality <- df_mortality %>%
  rename(
    "mortality" = "val"
  )

# Population
df_population <- get_population(release_id = rel_id,
                                age_group_id = "all",
                                location_id = list_state_loc_ids,
                                location_set_id = 35,
                                year_id = year_ids,
                                sex_id = sex_ids
                                )

# DALYs
df_dalys <- get_outputs(topic = "cause", 
                             release_id=rel_id,
                             location_id=list_state_loc_ids, 
                             year_id=year_ids, 
                             age_group_id="all",
                             sex_id=sex_ids, 
                             measure_id=2, # 2 = DALYs
                             metric_id=1, # 1 = Number
                             cause_id=cause_ids, 
                             location_set_id=35
)

df_dalys <- df_dalys %>%
  select(c("age_group_id", "cause_id", "location_id", "measure_id", "metric_id", 
           "sex_id", "year_id", "acause", "age_group_name", "cause_name", "location_name", "val"))

df_dalys <- df_dalys %>%
  rename(
    "DALYs" = "val"
  )

# Merge
df_m <- left_join(
  x = df_prevalence %>% select(!c("measure_id")),
  y = df_mortality %>% select(!c("measure_id")),
  by = c("age_group_id", "age_group_name", "cause_id", "acause", "cause_name", "location_id", "location_name", "sex_id", "year_id", "metric_id"),
)

df_m <- left_join(
  x = df_m,
  y = df_population %>% select(!("run_id")),
  by = c("age_group_id", "location_id", "year_id", "sex_id")
)

df_m <- left_join(
  x = df_m,
  y = df_dalys %>% select(!c("measure_id")),
  by = c("age_group_id", "age_group_name", "cause_id", "acause", "cause_name", "location_id", "location_name", "sex_id", "year_id", "metric_id"),
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
df_age_collapse <- df_m %>%
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
  group_by(age_name_group, cause_id, location_id, metric_id, sex_id, 
             year_id, acause, cause_name, location_name) %>%
  summarize(
    prevalence = sum(prevalence),
    mortality = sum(mortality, na.rm = TRUE),
    population = sum(population),
    DALYs = sum(DALYs)
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
## 3. Create Population Weights
## TODO - need to decide which year_id we are going to use for this if we want to make our own weights that have sex to keep sex in the FA model
##----------------------------------------------------------------
# df_weights <- df_final %>%
#   group_by(age_group_name, sex_id) %>%
#   summarise(
#     population = sum(population, na.rm = TRUE),
#     .groups = "drop"
#   ) %>%
#   group_by(sex_id) %>%
#   mutate(
#     pop_weight = population / sum(population)
#   ) %>%
#   ungroup()

##----------------------------------------------------------------
## 4. Save data
##----------------------------------------------------------------
# Prevalence, Mortality, & Population data
fn_main <- file.path(dir_out, "prev_mort_pop_data.parquet")

write_parquet(df_final, fn_main)

# # Population weights
# fn_weights <- file.path(dir_out, "age_weights.parquet")
# 
# write_parquet(df_weights, fn_weights)


















