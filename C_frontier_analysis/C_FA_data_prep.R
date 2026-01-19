##----------------------------------------------------------------
##' Title: C_FA_data_prep.R
##'
##' Purpose: TBD
##----------------------------------------------------------------

##----------------------------------------------------------------
## Clear environment and set library paths
##----------------------------------------------------------------
rm(list = ls())
pacman::p_load(data.table, arrow, tidyverse, glue)

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

library('frontier')
source("/ihme/cc_resources/libraries/current/r/get_age_metadata.R")

##----------------------------------------------------------------
## 0.0 Functions
##----------------------------------------------------------------
ensure_dir_exists <- function(dir_path) {
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
}

'%nin%' <- Negate('%in%')

##----------------------------------------------------------------
## 0.1 Set directories for DEX estimate data / county estimates
##----------------------------------------------------------------
# Set path for data
date_dex <- "20251123"
fp_dex <- file.path(h, "/aim_outputs/Aim2/B_aggregation/", date_dex, "/compiled_dex_data_2010_2019.parquet")

date_gbd <- "20260113"
fp_gbd <- file.path(h, "/aim_outputs/Aim2/A_data_preparation/", date_gbd, "/FA/df_gbd.parquet")

# date_ushd <- "20251204"
# fp_ushd <- file.path(h, "/aim_outputs/Aim2/B_aggregation/", date_ushd, "/compiled_ushd_data_2010_2019.parquet")

fp_ushd_age_weights <- file.path(j, "Project/us_counties/covariates/census_age_weights.csv")
df_ushd_age_weights <- read.csv(fp_ushd_age_weights)

# Set output directories
date_today <- format(Sys.time(), "%Y%m%d")
dir_output <- file.path(h, "/aim_outputs/Aim2/C_frontier_analysis/", date_today)
ensure_dir_exists(dir_output)

##----------------------------------------------------------------
## 0.2 Read in data
##----------------------------------------------------------------
# DEX data - filter down to state level, payers = all
ds_des <- open_dataset(fp_dex)

df_dex <- ds_des %>%
  filter(geo == "state") %>%
  #filter(payer == "all") %>% # Need different payers
  select(c("year_id", "geo", "location_name", "fips", "toc", "payer",
           "acause", "cause_name", "age_group_years_start", "age_name", 
           "sex_id", "spend_mean", "spend_lower", "spend_upper")) %>%
  collect()

# Add location_id to DEX data
df_state_names <- fread("/ihme/dex/us_county/maps/states.csv")

df_state_names <- df_state_names %>%
  select(c("state_name", "location_id"))

df_dex <- left_join(
  x = df_dex,
  y = df_state_names,
  by = c("location_name" = "state_name")
)

# GBD Prevalence, Mortality, Population data
df_gbd <- read_parquet(fp_gbd)

# Modify age group names to match DEX age group names
df_gbd <- df_gbd %>%
  mutate(age_name = case_when(
    age_group_name == "0 - <1"   ~ "0 - <1",
    age_group_name == "1 - <5"   ~ "1 - <5",
    age_group_name == "5 to 9"   ~ "5 - <10",
    age_group_name == "10 to 14" ~ "10 - <15",
    age_group_name == "15 to 19" ~ "15 - <20",
    age_group_name == "20 to 24" ~ "20 - <25",
    age_group_name == "25 to 29" ~ "25 - <30",
    age_group_name == "30 to 34" ~ "30 - <35",
    age_group_name == "35 to 39" ~ "35 - <40",
    age_group_name == "40 to 44" ~ "40 - <45",
    age_group_name == "45 to 49" ~ "45 - <50",
    age_group_name == "50 to 54" ~ "50 - <55",
    age_group_name == "55 to 59" ~ "55 - <60",
    age_group_name == "60 to 64" ~ "60 - <65",
    age_group_name == "65 to 69" ~ "65 - <70",
    age_group_name == "70 to 74" ~ "70 - <75",
    age_group_name == "75 to 79" ~ "75 - <80",
    age_group_name == "80 to 84" ~ "80 - <85",
    age_group_name == "85+"      ~ "85+"
  ))

df_gbd <- df_gbd %>%
  ungroup() %>%
  select(!c("age_group_name"))

##----------------------------------------------------------------
## 1. Collapse & Merge DEX & GBD data
##----------------------------------------------------------------
# Collapse on TOC in DEX data
df_dex <- df_dex %>%
  group_by(year_id, geo, location_name, location_id, fips, payer, acause, cause_name, age_group_years_start, age_name, sex_id,) %>%
  summarize(spend_mean = sum(spend_mean))

# Create "_subs" acause
df_dex_sud <- df_dex %>%
  filter(acause != "hiv")

df_dex_sud <- df_dex_sud %>%
  group_by(year_id, geo, location_name, location_id, fips, payer, age_group_years_start, age_name, sex_id) %>%
  summarize(spend_mean = sum(spend_mean)) %>%
  mutate(
    acause = "_subs",
    cause_name = "Substance use disorders"
    )

# Filter on "hiv" acause"
df_dex_hiv <- df_dex %>% 
  filter(acause == "hiv")

# Rbind back "_subs" & "hiv"
df_dex <- rbind(df_dex_hiv, df_dex_sud)

# Pivot wider to have columns for all different payer types
df_dex_pivot <- df_dex %>% pivot_wider(
  names_from  = payer,
  values_from = spend_mean,
  names_prefix = "spend_"
)

# Merge DEX & GBD data
df_m <- left_join(
  x = df_gbd,
  y = df_dex_pivot,
  by = c("location_id", "sex_id", "year_id", "acause", "cause_name", "location_name", "age_name")
)

##----------------------------------------------------------------
## 2. Collapse on sex_id
##----------------------------------------------------------------
df_m <- df_m %>%
  group_by(cause_id, year_id, location_id, location_name, acause, cause_name, age_name, age_group_years_start) %>%
  summarise(
    spend_all = sum(spend_all, na.rm = TRUE),
    spend_mdcd = sum(spend_mdcd, na.rm = TRUE),
    spend_mdcr = sum(spend_mdcr, na.rm = TRUE),
    spend_oop = sum(spend_oop, na.rm = TRUE),
    spend_priv = sum(spend_priv, na.rm = TRUE),
    mortality_counts = sum(mortality_counts),
    prevalence_counts = sum(prevalence_counts),
    daly_counts = sum(daly_counts),
    incidence_counts = sum(incidence_counts),
    population = sum(population),
    .groups = "drop"
  )

##----------------------------------------------------------------
## 3. Create spend_prev_ratio & mort_prev_ratio columns
##----------------------------------------------------------------
df_m <- df_m %>%
  mutate(
    spend_prev_ratio = (spend_all / prevalence_counts),
    mort_prev_ratio = (mortality_counts / prevalence_counts),
    daly_prev_ratio = (daly_counts / prevalence_counts)
  )

##----------------------------------------------------------------
## 4. Apply age-standardization
##----------------------------------------------------------------
# Rename "wt" column
df_ushd_age_weights <- df_ushd_age_weights %>%
  setnames(old = c("age", "wt"), new = c("age_group_years_start", "age_group_weight_value"))

# Join age weights to data
df_as <- left_join(
  x = df_m,
  y = df_ushd_age_weights,
  by = c("age_group_years_start")
)

# Create age-standardized ratios based on non-sexed GBD age weights (collapsing age groups here) 
df_as <- df_as %>%
  group_by(cause_id, year_id, location_id, location_name, acause, cause_name) %>%
  summarise(
    as_spend_prev_ratio = sum(spend_prev_ratio * age_group_weight_value, na.rm = TRUE),
    as_mort_prev_ratio  = sum(mort_prev_ratio * age_group_weight_value, na.rm = TRUE),
    as_daly_prev_ratio  = sum(daly_prev_ratio * age_group_weight_value, na.rm = TRUE),
    spend_all = sum(spend_all, na.rm = TRUE),
    spend_mdcd = sum(spend_mdcd, na.rm = TRUE),
    spend_mdcr = sum(spend_mdcr, na.rm = TRUE),
    spend_oop = sum(spend_oop, na.rm = TRUE),
    spend_priv = sum(spend_priv, na.rm = TRUE),
    mortality_counts = sum(mortality_counts),
    prevalence_counts = sum(prevalence_counts),
    daly_counts = sum(daly_counts),
    incidence_counts = sum(incidence_counts),
    population = sum(population),
    .groups = "drop"
  )

##----------------------------------------------------------------
## 5. Add Rate columns
##----------------------------------------------------------------
df_as$mortality_rates <- df_as$mortality_counts / df_as$population
df_as$prevalence_rates <- df_as$prevalence_counts / df_as$population
df_as$daly_rates <- df_as$daly_counts / df_as$population
df_as$incidence_rates <- df_as$incidence_counts / df_as$population

##----------------------------------------------------------------
## 6. Add variance column from mortality and deaths data
## Variance column needed for SFMA package 
##----------------------------------------------------------------
df_as$variance <- (df_as$mortality_counts / (df_as$prevalence_counts^2))

# Write out age-standardized data to today's dated folder in C_frontier_analysis
write.csv(x = df_as, row.names = FALSE, file = file.path(dir_output, "df_as.csv"))

##----------------------------------------------------------------
## 7. Collapse on year_id
##----------------------------------------------------------------
# Collapse on year_id
df_as_no_year <- df_as %>%
  group_by(cause_id, location_id, location_name, acause, cause_name) %>%
  summarise(
    spend_all = sum(spend_all, na.rm = TRUE),
    spend_mdcd = sum(spend_mdcd, na.rm = TRUE),
    spend_mdcr = sum(spend_mdcr, na.rm = TRUE),
    spend_oop = sum(spend_oop, na.rm = TRUE),
    spend_priv = sum(spend_priv, na.rm = TRUE),
    mortality_counts = sum(mortality_counts),
    prevalence_counts = sum(prevalence_counts),
    daly_counts = sum(daly_counts),
    incidence_counts = sum(incidence_counts),
    population = sum(population),
    
    spend_prev_ratio = spend_all / prevalence_counts,
    mort_prev_ratio  = mortality_counts / prevalence_counts,
    daly_prev_ratio  = daly_counts / prevalence_counts,
    .groups = "drop"
  )

df_as_no_year$mortality_rates <- df_as_no_year$mortality_counts / df_as_no_year$population
df_as_no_year$prevalence_rates <- df_as_no_year$prevalence_counts / df_as_no_year$population
df_as_no_year$daly_rates <- df_as_no_year$daly_counts / df_as_no_year$population
df_as_no_year$incidence_rates <- df_as_no_year$incidence_counts / df_as_no_year$population

# Save
write.csv(x = df_as_no_year, row.names = FALSE, file = file.path(dir_output, "df_as_no_year.csv"))
