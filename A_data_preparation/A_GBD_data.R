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
conflicts_prefer(dplyr::filter)

# Source IHME shared functions
source("/ihme/cc_resources/libraries/current/r/get_population.R")
source("/ihme/cc_resources/libraries/current/r/get_location_metadata.R")
source("/ihme/cc_resources/libraries/current/r/get_age_metadata.R")
source("/ihme/cc_resources/libraries/current/r/get_cause_metadata.R")
source("/ihme/cc_resources/libraries/current/r/get_machinery_estimates.R")

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
cause_ids <- c(298, 973, 560, 561, 562, 563, 564, 565, 566) # hiv = 298, _subs = 973

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

list_age_group_ids <- df_age_groups$age_group_id

##----------------------------------------------------------------
## 2. Pull GBD Data

# Prevalence count
# Mortality count
# Incidence count
# DALY count
# YLL count
# YLD count
# Population data

##----------------------------------------------------------------
# Set arguments
args_get_machinery_estimates <- list(entity = "cause", 
                                     entity_id = cause_ids,
                                     release_id=rel_id,
                                     context="GBD",
                                     estimates="draws", # PE = Point Estimates, UI = Uncertainty Intervals, draws = draws
                                     #measure_id (specified in call)
                                     #metric_id (specified in call)
                                     location_id=list_state_loc_ids, 
                                     sex_id=sex_ids, 
                                     age_group_id=list_age_group_ids,
                                     year_id=year_ids,
                                     add_names = T)

# Pull all draw counts (will make rates from collapsed counts later)
df_prevalence_counts <- do.call(get_machinery_estimates, c(args_get_machinery_estimates, list(measure_id = 5, metric_id = 1)))
df_mortality_counts <- do.call(get_machinery_estimates, c(args_get_machinery_estimates, list(measure_id = 1, metric_id = 1)))
df_daly_counts <- do.call(get_machinery_estimates, c(args_get_machinery_estimates, list(measure_id = 2, metric_id = 1)))
df_incidence_counts <- do.call(get_machinery_estimates, c(args_get_machinery_estimates, list(measure_id = 6, metric_id = 1)))
df_yll_counts <- do.call(get_machinery_estimates, c(args_get_machinery_estimates, list(measure_id = 4, metric_id = 1)))
df_yld_counts <- do.call(get_machinery_estimates, c(args_get_machinery_estimates, list(measure_id = 3, metric_id = 1)))

df_list <- list(
  prevalence_counts = df_prevalence_counts,
  mortality_counts  = df_mortality_counts,
  daly_counts       = df_daly_counts,
  incidence_counts  = df_incidence_counts,
  yll_counts        = df_yll_counts,
  yld_counts        = df_yld_counts
)

id_cols <- c(
  "age_group_id",
  "cause_id",
  "location_id",
  "sex_id",
  "year_id",
  "age_group_name",
  "cause_name",
  "location_name"
)

# Convert each dataframe to one row per ID x draw,
# with the measure as its own value column
df_list_hybrid <- imap(df_list, function(df, measure_name) {
  df %>%
    select(all_of(id_cols), starts_with("draw_")) %>%
    pivot_longer(
      cols = starts_with("draw_"),
      names_to = "draw",
      values_to = measure_name
    )
})

# Join all measure-specific dataframes together by IDs + draw
df_gbd_hybrid <- reduce(
  df_list_hybrid,
  left_join,
  by = c(id_cols, "draw")
)

# Collapse younger and older age groups into 0 - <1, 1 - <5, and 85+
collapse_ids_0_1  <- c(2, 3, 388, 389)
collapse_ids_1_5  <- c(238, 34)
collapse_ids_85p  <- c(31, 32, 235)

collapse_ids_all <- c(collapse_ids_0_1, collapse_ids_1_5, collapse_ids_85p)

# Keep rows that do NOT need collapsing
df_age_non_collapse <- df_gbd_hybrid %>%
  filter(!age_group_id %in% collapse_ids_all) %>%
  select(!c("age_group_id"))

# Keep only rows that DO need collapsing, then assign labels
df_age_collapse <- df_gbd_hybrid %>%
  filter(age_group_id %in% collapse_ids_all) %>%
  mutate(
    age_group_name = case_when(
      age_group_id %in% collapse_ids_0_1 ~ "0 - <1",
      age_group_id %in% collapse_ids_1_5 ~ "1 - <5",
      age_group_id %in% collapse_ids_85p ~ "85+"
    )
  ) %>%
  group_by(
    age_group_name, cause_id, location_id, sex_id, 
    year_id, cause_name, location_name, draw
  ) %>%
  summarise(
    prevalence_counts = sum(prevalence_counts, na.rm = TRUE),
    mortality_counts  = sum(mortality_counts, na.rm = TRUE),
    daly_counts       = sum(daly_counts, na.rm = TRUE),
    incidence_counts  = sum(incidence_counts, na.rm = TRUE),
    yll_counts        = sum(yll_counts, na.rm = TRUE),
    yld_counts        = sum(yld_counts, na.rm = TRUE),
    .groups = "drop"
  )

df_final <- bind_rows(df_age_non_collapse, df_age_collapse) 


# Population data
df_population <- get_population(release_id = rel_id,
                                age_group_id = "all",
                                location_id = list_state_loc_ids,
                                location_set_id = 35,
                                year_id = year_ids,
                                sex_id = sex_ids
)

standard_age_groups <- c(2, 3, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 30, 31, 32, 34, 235, 238, 388, 389)

# Collapse Population data

# Filter non-collapse pop data
df_pop_non_collapse <- df_population %>%
  filter(!age_group_id %in% collapse_ids_all)

# Filter collapse pop data & label the 0 - <1, 1 - <5, & 85+ age groups 
df_pop_collapse <- df_population %>%
  filter(age_group_id %in% collapse_ids_all) %>%
  mutate(age_group_name = case_when(
    age_group_id %in% c(2, 3, 388, 389) ~ "0 - <1",
    age_group_id %in% c(238, 34) ~ "1 - <5",
    age_group_id %in% c(31, 32, 235) ~ "85+"
  ))

# Collapse on the 0 - <1, 1 - <5, & 85+ age groups 
df_pop_collapse <- df_pop_collapse %>%
  group_by(age_group_name, location_id, sex_id, 
           year_id) %>%
  summarise(
    population = sum(population)
  )

# Rejoin data
df_pop_final <- bind_rows(df_pop_collapse, df_pop_non_collapse) 

##----------------------------------------------------------------
## 3. Save data
##----------------------------------------------------------------
# Prevalence, Mortality, DALY, Incidence, YLL, YLD data
fn_counts <- file.path(dir_out, "df_gbd_counts_draws.parquet")
write_parquet(df_final, fn_counts)

# Population data
fn_pop <- file.path(dir_out, "df_gbd_pop.parquet")
write_parquet(df_pop_final, fn_pop)


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









