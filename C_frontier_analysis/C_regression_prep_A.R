##----------------------------------------------------------------
##' Title: C_regression_prep_A.R
##'
##' Purpose: TBD
##' 
##' Outputs: 
##' df_decomp.parquet
##' df_as.csv & .parquet
##' df_as_no_year.csv
##----------------------------------------------------------------

##----------------------------------------------------------------
## Clear environment and set library paths
##----------------------------------------------------------------
rm(list = ls())
pacman::p_load(data.table, arrow, tidyverse, glue, dplyr)
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::summarize)

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
## 0.1 Set directories for input data
##----------------------------------------------------------------
# Set path for data
date_dex <- "20260402"
fp_dex <- file.path(h, "/aim_outputs/Aim2/B_aggregation/", date_dex, "/compiled_dex_data_2010_2019_draws.parquet")

date_gbd <- "20260401"
fp_gbd <- file.path(h, "/aim_outputs/Aim2/A_data_preparation/", date_gbd, "/GBD/df_gbd_counts_draws.parquet")

date_gbd_pop <- "20260402"
fp_gbd_pop <- file.path(h, "/aim_outputs/Aim2/A_data_preparation/", date_gbd_pop, "/GBD/df_gbd_pop.parquet")

fp_ushd_age_weights <- file.path(j, "Project/us_counties/covariates/census_age_weights.csv")
df_ushd_age_weights <- read.csv(fp_ushd_age_weights)

# Set output directories
date_today <- format(Sys.time(), "%Y%m%d")
dir_output <- file.path(h, "/aim_outputs/Aim2/C_frontier_analysis/", date_today)
ensure_dir_exists(dir_output)

##----------------------------------------------------------------
## 0.2 Read in data
##----------------------------------------------------------------
# DEX data - filter down to state level
ds_des <- open_dataset(fp_dex)

df_dex <- ds_des %>%
  filter(geo == "state") %>%
  select(c("year_id", "geo", "location_name", "fips", "toc", "payer",
           "acause", "cause_name", "age_group_years_start", "age_name", 
           "sex_id", "spend", "draw")) %>%
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
age_map <- c(
  "0 - <1"   = "0 - <1",
  "1 - <5"   = "1 - <5",
  "5 to 9"   = "5 - <10",
  "10 to 14" = "10 - <15",
  "15 to 19" = "15 - <20",
  "20 to 24" = "20 - <25",
  "25 to 29" = "25 - <30",
  "30 to 34" = "30 - <35",
  "35 to 39" = "35 - <40",
  "40 to 44" = "40 - <45",
  "45 to 49" = "45 - <50",
  "50 to 54" = "50 - <55",
  "55 to 59" = "55 - <60",
  "60 to 64" = "60 - <65",
  "65 to 69" = "65 - <70",
  "70 to 74" = "70 - <75",
  "75 to 79" = "75 - <80",
  "80 to 84" = "80 - <85",
  "85+"      = "85+"
)

df_gbd <- df_gbd %>%
  mutate(age_name = coalesce(unname(age_map[age_group_name]), age_group_name))

df_gbd <- df_gbd %>%
  ungroup() %>%
  select(!c("age_group_name"))

##----------------------------------------------------------------
## 1. Collapse & Merge DEX & GBD data
##----------------------------------------------------------------
# Collapse on TOC & Payer respectively in DEX data
df_dex_payer <- df_dex %>%
  group_by(year_id, geo, location_name, location_id, fips, payer, acause, cause_name, age_group_years_start, age_name, sex_id, draw) %>%
  summarize(spend = sum(spend))

df_dex_toc <- df_dex %>%
  filter(!payer == "all") %>%
  group_by(year_id, geo, location_name, location_id, fips, toc, acause, cause_name, age_group_years_start, age_name, sex_id, draw) %>%
  summarize(spend = sum(spend))

# Create "_subs" acause
df_dex_sud_payer <- df_dex_payer %>%
  filter(acause != "hiv")

df_dex_sud_toc <- df_dex_toc %>%
  filter(acause != "hiv")

df_dex_sud_payer <- df_dex_sud_payer %>%
  group_by(year_id, geo, location_name, location_id, fips, payer, age_group_years_start, age_name, sex_id, draw) %>%
  summarize(spend = sum(spend)) %>%
  mutate(
    acause = "_subs",
    cause_name = "Substance use disorders"
    )

df_dex_sud_toc <- df_dex_sud_toc %>%
  group_by(year_id, geo, location_name, location_id, fips, toc, age_group_years_start, age_name, sex_id, draw) %>%
  summarize(spend = sum(spend)) %>%
  mutate(
    acause = "_subs",
    cause_name = "Substance use disorders"
  )

# Filter on "hiv" acause" & retain "mental_alcohol" & "mental_drug_opioids" SUD subtype acauses
df_dex_hiv_and_sud_subtypes_payer <- df_dex %>% 
  filter(acause != "mental_drug_agg") %>%
  group_by(year_id, geo, location_name, location_id, fips, payer, acause, cause_name, age_group_years_start, age_name, sex_id, draw) %>%
  summarize(spend = sum(spend))

df_dex_hiv_and_sud_subtypes_toc <- df_dex %>% 
  filter(acause != "mental_drug_agg") %>%
  filter(!payer == "all") %>%
  group_by(year_id, geo, location_name, location_id, fips, toc, acause, cause_name, age_group_years_start, age_name, sex_id, draw) %>%
  summarize(spend = sum(spend))

# Rbind back "_subs" & "hiv"
df_dex_payer_all <- rbind(df_dex_hiv_and_sud_subtypes_payer, df_dex_sud_payer)
df_dex_toc_all <- rbind(df_dex_hiv_and_sud_subtypes_toc, df_dex_sud_toc)

# Pivot wider to have columns for all different payer & toc types
df_dex_payer_pivot <- df_dex_payer_all %>% pivot_wider(
  names_from  = payer,
  values_from = spend,
  names_prefix = "spend_"
)

df_dex_toc_pivot <- df_dex_toc_all %>% pivot_wider(
  names_from  = toc,
  values_from = spend,
  names_prefix = "spend_"
)

# this DF contains all the payer types and TOC types spread out over each spend_X column
# totals are respective to Payer groups and TOC groups (all payer groups summed = spend_all, all toc groups summed = spend_all)
df_dex_pivot <- left_join(
  x = df_dex_payer_pivot,
  y = df_dex_toc_pivot
)

# Merge DEX & GBD data

# Filter gbd data to just draw_0 ~ draw_50 (inclusive)
draws_to_filter <- c("draw_0", "draw_1", "draw_2", "draw_3", "draw_4", "draw_5", "draw_6", "draw_7", "draw_8", "draw_9", "draw_10", "draw_11", "draw_12", "draw_13", "draw_14", "draw_15", "draw_16", "draw_17", "draw_18", "draw_19", "draw_20", "draw_21", "draw_22", "draw_23", "draw_24", "draw_25", "draw_26", "draw_27", "draw_28", "draw_29", "draw_30", "draw_31", "draw_32", "draw_33", "draw_34", "draw_35", "draw_36", "draw_37", "draw_38", "draw_39", "draw_40", "draw_41", "draw_42", "draw_43", "draw_44", "draw_45", "draw_46", "draw_47", "draw_48", "draw_49", "draw_50")

df_gbd <- df_gbd %>%
  filter(draw %in% draws_to_filter)

# Filter to just the causes we're interested in 
gbd_causes_to_include <- c("HIV/AIDS", 
                           "Alcohol use disorders",  
                           "Opioid use disorders",
                           "Substance use disorders")

df_gbd <- df_gbd %>%
  filter(cause_name %in% gbd_causes_to_include)

# Strip the draw column's "draw_" values and retain just the draw # itself e.g. "draw_0" -> 0
df_gbd <- df_gbd %>%
  mutate(
    draw = as.integer(sub("draw_", "", draw))
  )

df_m <- left_join(
  x = df_gbd,
  y = df_dex_pivot,
  by = c("sex_id", "year_id", "cause_name", "location_name", "age_name", "draw")
)

# Filter out age groups under 15 years of age, all GBD data is 0, but should be NA. 
opioid_age_groups_to_filter <- c("0 - <1", "1 - <5", "5 - <10", "10 - <15")

df_m <- df_m %>%
  filter(!(cause_name == "Opioid use disorders" &
             age_name %in% opioid_age_groups_to_filter))

# Drop extra location_id column
df_m <- df_m %>%
  select(!c("location_id.y")) %>%
  rename("location_id" = "location_id.x")

# Read in population data and join to df_m
df_gbd_pop <- read_parquet(fp_gbd_pop)

df_m <- left_join(
  x = df_m,
  y = df_gbd_pop,
  by = c("age_name" = "age_group_name", "year_id", "sex_id", "location_id")
)

# Write out age*sex*year*loc strata file, used for decomp analysis
write_parquet(df_m, file.path(dir_output, "df_decomp_draws.parquet"))

##----------------------------------------------------------------
## 2. Collapse on sex_id
##----------------------------------------------------------------
df_m_collapse <- df_m %>%
  group_by(cause_id, year_id, location_id, location_name, acause, cause_name, age_name, age_group_years_start, draw) %>%
  summarise(
    spend_all = sum(spend_all, na.rm = TRUE),
    spend_mdcd = sum(spend_mdcd, na.rm = TRUE),
    spend_mdcr = sum(spend_mdcr, na.rm = TRUE),
    spend_oop = sum(spend_oop, na.rm = TRUE),
    spend_priv = sum(spend_priv, na.rm = TRUE),
    spend_AM = sum(spend_AM, na.rm = TRUE),
    spend_ED = sum(spend_ED, na.rm = TRUE),
    spend_HH = sum(spend_HH, na.rm = TRUE),
    spend_IP = sum(spend_IP, na.rm = TRUE),
    spend_NF = sum(spend_NF, na.rm = TRUE),
    spend_RX = sum(spend_RX, na.rm = TRUE),
    mortality_counts = sum(mortality_counts),
    prevalence_counts = sum(prevalence_counts),
    daly_counts = sum(daly_counts),
    incidence_counts = sum(incidence_counts),
    yll_counts = sum(yll_counts),
    yld_counts = sum(yld_counts),
    population = sum(population),
    .groups = "drop"
  )

##----------------------------------------------------------------
## 3. Create spend_prev_ratio & mort_prev_ratio columns
##----------------------------------------------------------------
df_m_collapse <- df_m_collapse %>%
  mutate(
    spend_prev_ratio = (spend_all / prevalence_counts),
    mort_prev_ratio = (mortality_counts / prevalence_counts),
    daly_prev_ratio = (daly_counts / prevalence_counts),
    yll_prev_ratio = (yll_counts / prevalence_counts),
    yld_prev_ratio = (yld_counts / prevalence_counts)
  )

##----------------------------------------------------------------
## 4. Apply age-standardization
##----------------------------------------------------------------
# Rename "wt" column
df_ushd_age_weights <- df_ushd_age_weights %>%
  setnames(old = c("age", "wt"), new = c("age_group_years_start", "age_group_weight_value"))

# Join age weights to data
df_as <- left_join(
  x = df_m_collapse,
  y = df_ushd_age_weights,
  by = c("age_group_years_start")
)

# Create age-standardized ratios based on non-sexed GBD age weights (collapsing age groups here) 
df_as <- df_as %>%
  group_by(cause_id, year_id, location_id, location_name, acause, cause_name, draw) %>%
  summarise(
    as_spend_prev_ratio = sum(spend_prev_ratio * age_group_weight_value, na.rm = TRUE),
    as_mort_prev_ratio  = sum(mort_prev_ratio * age_group_weight_value, na.rm = TRUE),
    as_daly_prev_ratio  = sum(daly_prev_ratio * age_group_weight_value, na.rm = TRUE),
    as_yll_prev_ratio  = sum(yll_prev_ratio * age_group_weight_value, na.rm = TRUE),
    as_yld_prev_ratio  = sum(yld_prev_ratio * age_group_weight_value, na.rm = TRUE),
    spend_all = sum(spend_all, na.rm = TRUE),
    spend_mdcd = sum(spend_mdcd, na.rm = TRUE),
    spend_mdcr = sum(spend_mdcr, na.rm = TRUE),
    spend_oop = sum(spend_oop, na.rm = TRUE),
    spend_priv = sum(spend_priv, na.rm = TRUE),
    spend_AM = sum(spend_AM, na.rm = TRUE),
    spend_ED = sum(spend_ED, na.rm = TRUE),
    spend_HH = sum(spend_HH, na.rm = TRUE),
    spend_IP = sum(spend_IP, na.rm = TRUE),
    spend_NF = sum(spend_NF, na.rm = TRUE),
    spend_RX = sum(spend_RX, na.rm = TRUE),
    mortality_counts = sum(mortality_counts),
    prevalence_counts = sum(prevalence_counts),
    daly_counts = sum(daly_counts),
    incidence_counts = sum(incidence_counts),
    yll_counts = sum(yll_counts),
    yld_counts = sum(yld_counts),
    population = sum(population),
    .groups = "drop"
  )

##----------------------------------------------------------------
## 5. Add Rate columns - UNUSED? I don't think we use rates for anything?
##----------------------------------------------------------------
# df_as$mortality_rates <- df_as$mortality_counts / df_as$population
# df_as$prevalence_rates <- df_as$prevalence_counts / df_as$population
# df_as$daly_rates <- df_as$daly_counts / df_as$population
# df_as$incidence_rates <- df_as$incidence_counts / df_as$population
# df_as$yll_rates <- df_as$yll_counts / df_as$population
# df_as$yld_rates <- df_as$yld_counts / df_as$population

##----------------------------------------------------------------
## 6. Add variance column from mortality and deaths data
## Variance column needed for SFMA package 
##----------------------------------------------------------------
df_as$variance <- (df_as$mortality_counts / (df_as$prevalence_counts^2))

# Write out age-standardized data to today's dated folder in C_frontier_analysis
write_parquet(df_as, file.path(dir_output, "df_as_draws.parquet"))
write.csv(x = df_as, row.names = FALSE, file = file.path(dir_output, "df_as_draws.csv"))

##----------------------------------------------------------------
## 7. Collapse on year_id for df_as_no_year data (used in D_tables.R)
##----------------------------------------------------------------
# Collapse on year_id
df_as_no_year <- df_as %>%
  group_by(cause_id, location_id, location_name, acause, cause_name, draw) %>%
  summarise(
    spend_all = sum(spend_all, na.rm = TRUE),
    spend_mdcd = sum(spend_mdcd, na.rm = TRUE),
    spend_mdcr = sum(spend_mdcr, na.rm = TRUE),
    spend_oop = sum(spend_oop, na.rm = TRUE),
    spend_priv = sum(spend_priv, na.rm = TRUE),
    spend_AM = sum(spend_AM, na.rm = TRUE),
    spend_ED = sum(spend_ED, na.rm = TRUE),
    spend_HH = sum(spend_HH, na.rm = TRUE),
    spend_IP = sum(spend_IP, na.rm = TRUE),
    spend_NF = sum(spend_NF, na.rm = TRUE),
    spend_RX = sum(spend_RX, na.rm = TRUE),
    mortality_counts = sum(mortality_counts),
    prevalence_counts = sum(prevalence_counts),
    daly_counts = sum(daly_counts),
    incidence_counts = sum(incidence_counts),
    yll_counts = sum(yll_counts),
    yld_counts = sum(yld_counts),
    population = sum(population),
    spend_prev_ratio = spend_all / prevalence_counts,
    mort_prev_ratio  = mortality_counts / prevalence_counts,
    daly_prev_ratio  = daly_counts / prevalence_counts,
    yll_prev_ratio  = yll_counts / prevalence_counts,
    yld_prev_ratio  = yld_counts / prevalence_counts,
    .groups = "drop"
  )

df_as_no_year$mortality_rates <- df_as_no_year$mortality_counts / df_as_no_year$population
df_as_no_year$prevalence_rates <- df_as_no_year$prevalence_counts / df_as_no_year$population
df_as_no_year$daly_rates <- df_as_no_year$daly_counts / df_as_no_year$population
df_as_no_year$incidence_rates <- df_as_no_year$incidence_counts / df_as_no_year$population
df_as_no_year$yll_rates <- df_as_no_year$yll_counts / df_as_no_year$population
df_as_no_year$yld_rates <- df_as_no_year$yld_counts / df_as_no_year$population

# Save
write.csv(x = df_as_no_year, row.names = FALSE, file = file.path(dir_output, "df_as_no_year_draws.csv"))


##### SCRATCH SPACE
# Example code for collapsing the draw values to get PI and UI

# "prevalence_counts"
# 
# df_test <- head(df_m, 51) # This step basically filters to give us a single strata, with 51 draw values
# 
# df_test_2 <- df_test %>% #this step collapses on the 51 draw values, and takes the mean, and then quantiles to get the UI
#   group_by(cause_id, location_id, sex_id, year_id, cause_name, location_name, age_name, geo, fips, acause, age_group_years_start) %>%
#   summarise(
#     prevalence_counts_pi = mean(prevalence_counts),
#     prevalence_counts_lower = quantile(prevalence_counts, 0.025),
#     prevalence_counts_upper = quantile(prevalence_counts, 0.975),
#   )


