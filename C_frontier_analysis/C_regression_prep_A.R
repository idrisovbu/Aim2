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
pacman::p_load(data.table, arrow, tidyverse, glue, dplyr,conflicted,readxl)
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
date_dex <- "20260514"
fp_dex <- file.path(h, "/aim_outputs/Aim2/B_aggregation/", date_dex, "/compiled_dex_data_2010_2019_draws.parquet")

date_gbd_state <- "20260401"
fp_gbd <- file.path(h, "/aim_outputs/Aim2/A_data_preparation/", date_gbd_state, "/GBD/df_gbd_counts_draws.parquet")

date_gbd_nat <- "20260403"
fp_gbd_nat <- file.path(h, "/aim_outputs/Aim2/A_data_preparation/", date_gbd_nat, "/GBD/df_gbd_counts_national_draws.parquet")

date_gbd_pop <- "20260402"
fp_gbd_pop <- file.path(h, "/aim_outputs/Aim2/A_data_preparation/", date_gbd_pop, "/GBD/df_gbd_pop.parquet")

date_gbd_pop_nat <- "20260403"
fp_gbd_pop_nat <- file.path(h, "/aim_outputs/Aim2/A_data_preparation/", date_gbd_pop_nat, "/GBD/df_gbd_national_pop.parquet")

fp_ushd_age_weights <- file.path(j, "Project/us_counties/covariates/census_age_weights.csv")

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
  filter(!geo == "cnty") %>%
  select(c("year_id", "geo", "location_name", "fips", "toc", "payer",
           "acause", "cause_name", "age_group_years_start", "age_name",
           "sex_id", "spend", "pop", "draw")) %>%   # [POST-MAY-6-COMMITTEE: per-capita switch] pull pop from worker
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

# need to label national USA with location_id = 102
df_dex[geo == "national", location_id := 102]

# GBD Prevalence, Mortality, Population data
df_gbd <- read_parquet(fp_gbd)
df_gbd_nat <- read_parquet(fp_gbd_nat)

# rename United States of America -> United States
df_gbd_nat <- df_gbd_nat %>%
  mutate(location_name = if_else(
    location_name == "United States of America", "United States", location_name))

# rbind state level and national level data
df_gbd <- rbind(df_gbd, df_gbd_nat)

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
# [POST-MAY-6-COMMITTEE: per-capita switch] -------------------------------------
# Extract DEX population denominator BEFORE collapsing — pop does not vary by
# draw, toc, or acause. One pop value per (year, geo, location, age, sex).
# Using payer == "all" as the representative row.
df_dex_pop_lookup <- df_dex %>%
  #filter(payer == "all") %>%
  filter(!is.na(pop)) %>%   # [POST-MAY-6-COMMITTEE: per-capita fix] "all" payer rows have NA pop; keep rows from mdcr/mdcd/oop/priv (same pop value)
  group_by(year_id, geo, location_name, location_id, fips, age_group_years_start, age_name, sex_id) %>%
  summarize(dex_pop = max(pop, na.rm = TRUE), .groups = "drop")
# -------------------------------------------------------------------------------


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

# Join DEX and GBD data
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
df_gbd_nat_pop <- read_parquet(fp_gbd_pop_nat)

df_gbd_pop <- rbind(df_gbd_pop, df_gbd_nat_pop)

df_m_pop <- left_join(
  x = df_m,
  y = df_gbd_pop,
  by = c("age_name" = "age_group_name", "year_id", "sex_id", "location_id")
)

# [POST-MAY-6-COMMITTEE: per-capita switch] -------------------------------------
# Join DEX population denominator to df_m_pop so it propagates through collapses.
df_m_pop <- left_join(
  x = df_m_pop,
  y = df_dex_pop_lookup,
  by = c("year_id", "geo", "location_name", "location_id", "fips", "age_group_years_start", "age_name", "sex_id")
)
# -------------------------------------------------------------------------------

# Write out age*sex*year*loc strata file, used for decomp analysis
write_parquet(df_m_pop, file.path(dir_output, "df_decomp_draws.parquet"))

##----------------------------------------------------------------
## 2. Collapse on sex_id
##----------------------------------------------------------------
df_m_collapse <- df_m_pop %>%
  group_by(cause_id, year_id, geo, location_id, location_name, acause, cause_name, age_name, age_group_years_start, draw) %>%
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
    dex_pop = sum(dex_pop, na.rm = TRUE),  # [POST-MAY-6-COMMITTEE: per-capita switch] sum DEX pop across sexes
    .groups = "drop"
  )

##----------------------------------------------------------------
## 3. Create spend_prev_ratio & mort_prev_ratio columns
##----------------------------------------------------------------
df_m_collapse <- df_m_collapse %>%
  mutate(
    spend_prev_ratio  = (spend_all / prevalence_counts),
    spend_per_capita  = (spend_all / dex_pop),       # [POST-MAY-6-COMMITTEE: per-capita switch] new exposure variable
    mort_prev_ratio   = (mortality_counts / prevalence_counts),
    daly_prev_ratio   = (daly_counts / prevalence_counts),
    yll_prev_ratio    = (yll_counts / prevalence_counts),
    yld_prev_ratio    = (yld_counts / prevalence_counts)
  )

colnames(df_m_pop)
##----------------------------------------------------------------
## 4. Apply age-standardization
##----------------------------------------------------------------
# Read in age weights
df_ushd_age_weights <- read.csv(fp_ushd_age_weights)

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
  group_by(cause_id, year_id, geo, location_id, location_name, acause, cause_name, draw) %>%
  summarise(
    as_spend_prev_ratio  = sum(spend_prev_ratio  * age_group_weight_value, na.rm = TRUE),
    as_spend_per_capita  = sum(spend_per_capita  * age_group_weight_value, na.rm = TRUE),  # [POST-MAY-6-COMMITTEE: per-capita switch] age-standardized per-capita exposure
    as_mort_prev_ratio   = sum(mort_prev_ratio   * age_group_weight_value, na.rm = TRUE),
    as_daly_prev_ratio   = sum(daly_prev_ratio   * age_group_weight_value, na.rm = TRUE),
    as_yll_prev_ratio    = sum(yll_prev_ratio    * age_group_weight_value, na.rm = TRUE),
    as_yld_prev_ratio    = sum(yld_prev_ratio    * age_group_weight_value, na.rm = TRUE),
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
    dex_pop = sum(dex_pop, na.rm = TRUE),  # [POST-MAY-6-COMMITTEE: per-capita switch] retain DEX pop after age-standardization
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
## 5.1 Create treated prevalence & spending per treated case (Opioid use disorders only)
##----------------------------------------------------------------
df_as <- df_as %>%
  mutate(
    census_region = case_when(
      location_name %in% c("Connecticut","Maine","Massachusetts","New Hampshire",
                           "Rhode Island","Vermont","New Jersey","New York",
                           "Pennsylvania") ~ "Northeast",
      location_name %in% c("Illinois","Indiana","Iowa","Kansas","Michigan",
                           "Minnesota","Missouri","Nebraska","North Dakota",
                           "Ohio","South Dakota","Wisconsin") ~ "Midwest",
      location_name %in% c("Alabama","Arkansas","Delaware","District of Columbia",
                           "Florida","Georgia","Kentucky","Louisiana","Maryland",
                           "Mississippi","North Carolina","Oklahoma",
                           "South Carolina","Tennessee","Texas","Virginia",
                           "West Virginia") ~ "South",
      location_name %in% c("Alaska","Arizona","California","Colorado","Hawaii",
                           "Idaho","Montana","Nevada","New Mexico","Oregon",
                           "Utah","Washington","Wyoming") ~ "West"
    ),
    treat_rate = case_when(
      cause_name != "Opioid use disorders" ~ NA_real_,
      census_region == "Midwest"   ~ 0.530,
      census_region == "Northeast" ~ 0.630,
      census_region == "South"     ~ 0.556,
      census_region == "West"      ~ 0.501
    ),
    treated_prev_counts = if_else(cause_name == "Opioid use disorders",
                                  prevalence_counts * treat_rate,
                                  NA_real_),
    spend_tx_prev_ratio = if_else(cause_name == "Opioid use disorders",
                                  spend_all / treated_prev_counts,
                                  NA_real_)
  )


##----------------------------------------------------------------
## 6. Add variance column from mortality and deaths data
## Variance column needed for SFMA package 
##----------------------------------------------------------------
df_as$variance <- (df_as$mortality_counts / (df_as$prevalence_counts^2))

# Write out age-standardized data to today's dated folder in C_frontier_analysis
write_parquet(df_as, file.path(dir_output, "df_as_draws.parquet"))
write.csv(x = df_as, row.names = FALSE, file = file.path(dir_output, "df_as_draws.csv"))

##----------------------------------------------------------------
## 6.1 Collapse draws -> point estimates (median across draws)
##     [POST-MAY-6-COMMITTEE: draws-collapse fix]
##     prep_B.R reads df_as.csv (no draws). When JAWS/draws were added,
##     prep_A started writing df_as_draws.csv only, breaking prep_B.
##     Collapse across draws here with median to restore compatibility.
##     Keep df_as_draws.* for uncertainty quantification downstream.
##----------------------------------------------------------------
df_as_point <- df_as %>%
  group_by(cause_id, year_id, geo, location_id, location_name, acause, cause_name) %>%
  summarise(across(where(is.numeric), \(x) median(x, na.rm = TRUE)),
            .groups = "drop") %>%
  select(-any_of("draw"))   # drop the draw column (now meaningless after collapse)

write_parquet(df_as_point, file.path(dir_output, "df_as.parquet"))
write.csv(x = df_as_point, row.names = FALSE, file = file.path(dir_output, "df_as.csv"))



##----------------------------------------------------------------
## 7. Collapse on year_id for df_as_no_year data (used in D_tables.R)
##----------------------------------------------------------------
# Collapse on year_id
df_as_no_year <- df_as %>%
  group_by(cause_id, location_id, geo, location_name, acause, cause_name, draw) %>%
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

##----------------------------------------------------------------
## 8. Process CDC data
##----------------------------------------------------------------
# Select columns we want
df_cdc <- df_cdc %>%
  select(c("Indicator", "Year", "Geography", "Age Group", "Cases"))

# Rename columns
df_cdc <- df_cdc %>%
  setnames(
    old = c("Indicator", "Year", "Geography", "Age Group", "Cases"),
    new = c("measure", "year_id", "location_name", "age_group", "val")
  )

# Pivot wide
df_cdc_p <- df_cdc %>%
  pivot_wider(
    names_from = measure,
    values_from = val
  )

# Modify age group names to match DEX age group names
df_cdc_p <- df_cdc_p %>%
  mutate(age_name = case_when(
    age_group == "25-34" ~ "25 - <35",
    age_group == "35-44" ~ "35 - <45",
    age_group == "45-54" ~ "45 - <55",
    age_group == "55-64" ~ "55 - <65",
    age_group == "65+" ~ "65+"
  ))

# Rename columns
df_cdc_p <- df_cdc_p %>%
  setnames(
    old = c("HIV deaths", "HIV prevalence"),
    new = c("cdc_hiv_mortality_counts", "cdc_hiv_prevalence_counts")
  )

# Add cause columns
df_cdc_p$acause <- "hiv"
df_cdc_p$cause_name <- "HIV/AIDS"

# Fix naming on certain states
df_cdc_p$location_name <- if_else(df_cdc_p$location_name == "Mississippi^", "Mississippi", df_cdc_p$location_name)
df_cdc_p$location_name <- if_else(df_cdc_p$location_name == "West Virginia^", "West Virginia", df_cdc_p$location_name)

##----------------------------------------------------------------
## 9. Merge CDC data with data pre-age-standardization
##----------------------------------------------------------------
# Filter df_m_collapse (data before age-standardization) down to age groups in CDC data
df_m_cdc <- df_m_collapse %>%
  filter(age_name %in% c("25 - <30",
                         "30 - <35", "35 - <40", "40 - <45", "45 - <50", "50 - <55",
                         "55 - <60", "60 - <65", "65 - <70", "70 - <75", "75 - <80", "80 - <85",
                         "85+"))

# Create age groups to match CDC age groups
df_m_cdc <- df_m_cdc %>%
  mutate(age_name_cdc = case_when(
    age_name == "25 - <30" ~ "25 - <35",
    age_name == "30 - <35" ~ "25 - <35",
    age_name == "35 - <40" ~ "35 - <45",
    age_name == "40 - <45" ~ "35 - <45",
    age_name == "45 - <50" ~ "45 - <55",
    age_name == "50 - <55" ~ "45 - <55",
    age_name == "55 - <60" ~ "55 - <65",
    age_name == "60 - <65" ~ "55 - <65",
    age_name == "65 - <70" ~ "65+",
    age_name == "70 - <75" ~ "65+",
    age_name == "75 - <80" ~ "65+",
    age_name == "80 - <85" ~ "65+",
    age_name == "85+"      ~ "65+"
  ))

# Collapse on sex
df_m_cdc <- df_m_cdc %>%
  select(!c("spend_prev_ratio", "mort_prev_ratio", "daly_prev_ratio", "yll_prev_ratio", "yld_prev_ratio")) %>%
  group_by(cause_id, year_id, location_id, location_name, acause, cause_name, age_name_cdc) %>%
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
    yll_counts = sum(yll_counts),
    yld_counts = sum(yld_counts),
    population = sum(population),
    .groups = "drop"
  )

# Left join w/ CDC data
df_m_cdc <- left_join(
  x = df_m_cdc,
  y = df_cdc_p %>% select(!c("age_group")),
  by = c("year_id", "location_name", "age_name_cdc" = "age_name", "acause", "cause_name")
)

##----------------------------------------------------------------
## 10. Create spend_prev_ratio & mort_prev_ratio columns
##----------------------------------------------------------------
df_m_cdc <- df_m_cdc %>%
  mutate(
    cdc_spend_prev_ratio = (spend_all / cdc_hiv_prevalence_counts),
    cdc_mort_prev_ratio = (cdc_hiv_mortality_counts / cdc_hiv_prevalence_counts),
    spend_prev_ratio = (spend_all / prevalence_counts),
    mort_prev_ratio = (mortality_counts / prevalence_counts),
    daly_prev_ratio = (daly_counts / prevalence_counts),
    yll_prev_ratio = (yll_counts / prevalence_counts),
    yld_prev_ratio = (yld_counts / prevalence_counts)
  )

##----------------------------------------------------------------
## 11. Create age weights for CDC age groups c("25-34", "35-44", "45-54", "55-64", "65+")
##----------------------------------------------------------------
# Label age groups to match CDC groups
df_ushd_age_weights_cdc <- df_ushd_age_weights %>%
  mutate(
    age_group_coarse = case_when(
      age_group_years_start %in% c(25, 30) ~ "25 - <35",
      age_group_years_start %in% c(35, 40) ~ "35 - <45",
      age_group_years_start %in% c(45, 50) ~ "45 - <55",
      age_group_years_start %in% c(55, 60) ~ "55 - <65",
      age_group_years_start >= 65          ~ "65+",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(age_group_coarse))

# Sum by new age group
df_ushd_age_weights_cdc <- df_ushd_age_weights_cdc %>%
  group_by(age_group_coarse) %>%
  summarise(
    age_group_weight = sum(age_group_weight_value),
    .groups = "drop"
  )

# Scale new age groups to sum to 1 based on their weight
df_ushd_age_weights_cdc <- df_ushd_age_weights_cdc %>%
  mutate(
    age_group_weight = age_group_weight / sum(age_group_weight)
  )

##----------------------------------------------------------------
## 12. Merge CDC data w/ modified age weights, age-standardize ratios
##----------------------------------------------------------------
df_m_cdc <- left_join(
  x = df_m_cdc,
  y = df_ushd_age_weights_cdc,
  by = c("age_name_cdc" = "age_group_coarse")
)

# Create age-standardized ratios based on non-sexed GBD age weights (collapsing age groups here)
df_as_cdc <- df_m_cdc %>%
  group_by(cause_id, year_id, location_id, location_name, acause, cause_name) %>%
  summarise(
    as_cdc_spend_prev_ratio = sum(cdc_spend_prev_ratio * age_group_weight, na.rm = TRUE),
    as_cdc_mort_prev_ratio = sum(cdc_mort_prev_ratio * age_group_weight, na.rm = TRUE),
    as_spend_prev_ratio = sum(spend_prev_ratio * age_group_weight, na.rm = TRUE),
    as_mort_prev_ratio  = sum(mort_prev_ratio * age_group_weight, na.rm = TRUE),
    as_daly_prev_ratio  = sum(daly_prev_ratio * age_group_weight, na.rm = TRUE),
    as_yll_prev_ratio  = sum(yll_prev_ratio * age_group_weight, na.rm = TRUE),
    as_yld_prev_ratio  = sum(yld_prev_ratio * age_group_weight, na.rm = TRUE),
    spend_all = sum(spend_all, na.rm = TRUE),
    spend_mdcd = sum(spend_mdcd, na.rm = TRUE),
    spend_mdcr = sum(spend_mdcr, na.rm = TRUE),
    spend_oop = sum(spend_oop, na.rm = TRUE),
    spend_priv = sum(spend_priv, na.rm = TRUE),
    cdc_hiv_mortality_counts = sum(cdc_hiv_mortality_counts, na.rm = TRUE),
    cdc_hiv_prevalence_counts = sum(cdc_hiv_prevalence_counts, na.rm = TRUE),
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
## 13. Add Rate columns to df_as_cdc
##----------------------------------------------------------------
df_as_cdc$mortality_rates <- df_as_cdc$mortality_counts / df_as_cdc$population
df_as_cdc$prevalence_rates <- df_as_cdc$prevalence_counts / df_as_cdc$population
df_as_cdc$daly_rates <- df_as_cdc$daly_counts / df_as_cdc$population
df_as_cdc$incidence_rates <- df_as_cdc$incidence_counts / df_as_cdc$population
df_as_cdc$yll_rates <- df_as_cdc$yll_counts / df_as_cdc$population
df_as_cdc$yld_rates <- df_as_cdc$yld_counts / df_as_cdc$population

##----------------------------------------------------------------
## 14. Add variance column from mortality and deaths data
## Variance column needed for SFMA package
##----------------------------------------------------------------
df_as_cdc$variance <- (df_as_cdc$mortality_counts / (df_as_cdc$prevalence_counts^2))
df_as_cdc$cdc_variance <- (df_as_cdc$cdc_hiv_mortality_counts / (df_as_cdc$cdc_hiv_prevalence_counts^2))

# Write out CDC age-standardized data to today's dated folder in C_frontier_analysis
write.csv(x = df_as_cdc, row.names = FALSE, file = file.path(dir_output, "df_as_cdc.csv"))





summary(df_as_cdc)
summary(df_as)
######

##----------------------------------------------------------------
## CDC HIV robustness check: CDC vs GBD correlations + regression
##----------------------------------------------------------------
pacman::p_load(dplyr, tidyr, broom, clubSandwich, ggplot2, data.table)

# ---- 0. File paths (edit to current pipeline date)
fp_hiv_analysis <- file.path(h, "/aim_outputs/Aim2/C_frontier_analysis/20260424//df_as.csv")
fp_as_cdc       <- file.path(h, "/aim_outputs/Aim2/C_frontier_analysis/20260424//df_as_cdc.csv")

df_hiv_base <- fread(fp_hiv_analysis)
df_as_cdc   <- fread(fp_as_cdc)

##----------------------------------------------------------------
## 1. Merge CDC HIV slice into the main analytic frame
##----------------------------------------------------------------
df_cdc_hiv <- df_as_cdc %>%
  filter(acause == "hiv") %>%
  select(year_id, location_id,
         cdc_hiv_prevalence_counts, cdc_hiv_mortality_counts,
         as_cdc_mort_prev_ratio, as_cdc_spend_prev_ratio)

df_hiv_cdc <- df_hiv_base %>%
  left_join(df_cdc_hiv, by = c("year_id", "location_id"))

##----------------------------------------------------------------
## 2. CDC vs GBD correlations (state-year, unsuppressed rows only)
##----------------------------------------------------------------
df_corr <- df_hiv_cdc %>%
  filter(!is.na(cdc_hiv_prevalence_counts),
         cdc_hiv_prevalence_counts > 0,
         !is.na(cdc_hiv_mortality_counts))

make_cor <- function(x, y, method, label) {
  tibble(measure = label, method = method,
         r = cor(x, y, use = "complete.obs", method = method),
         n = sum(complete.cases(x, y)))
}

cdc_gbd_corr_tbl <- bind_rows(
  make_cor(df_corr$prevalence_counts,   df_corr$cdc_hiv_prevalence_counts, "pearson",  "prevalence_counts"),
  make_cor(df_corr$prevalence_counts,   df_corr$cdc_hiv_prevalence_counts, "spearman", "prevalence_counts"),
  make_cor(df_corr$mortality_counts,    df_corr$cdc_hiv_mortality_counts,  "pearson",  "mortality_counts"),
  make_cor(df_corr$mortality_counts,    df_corr$cdc_hiv_mortality_counts,  "spearman", "mortality_counts"),
  make_cor(df_corr$as_mort_prev_ratio,  df_corr$as_cdc_mort_prev_ratio,    "pearson",  "as_mort_prev_ratio"),
  make_cor(df_corr$as_mort_prev_ratio,  df_corr$as_cdc_mort_prev_ratio,    "spearman", "as_mort_prev_ratio")
)
print(cdc_gbd_corr_tbl)

# Visual sanity checks (45-degree reference line)
p_prev <- ggplot(df_corr, aes(prevalence_counts, cdc_hiv_prevalence_counts)) +
  geom_point(alpha = .5) + geom_abline(slope = 1, linetype = 2) +
  scale_x_log10() + scale_y_log10() +
  labs(x = "GBD prevalence (log)", y = "CDC prevalence (log)",
       title = "CDC vs GBD HIV prevalence (state-year)") + theme_minimal()

p_mort <- ggplot(df_corr, aes(mortality_counts, cdc_hiv_mortality_counts)) +
  geom_point(alpha = .5) + geom_abline(slope = 1, linetype = 2) +
  scale_x_log10() + scale_y_log10() +
  labs(x = "GBD mortality (log)", y = "CDC mortality (log)",
       title = "CDC vs GBD HIV mortality (state-year)") + theme_minimal()
print(p_prev); print(p_mort)

##----------------------------------------------------------------
## 3. Build CDC robustness analytic frame
##    (drops state-years with CDC suppression -> prev == 0)
##----------------------------------------------------------------
df_hiv_cdc_est <- df_hiv_cdc %>%
  filter(cdc_hiv_prevalence_counts > 0,
         as_cdc_mort_prev_ratio    > 0) %>%
  mutate(
    as_cdc_mort_prev_ratio_log    = log1p(as_cdc_mort_prev_ratio),
    rw_dex_cdc_hiv_prev_ratio     = (ryan_white_funding_final + spend_all) / cdc_hiv_prevalence_counts,
    rw_dex_cdc_hiv_prev_ratio_log = log1p(rw_dex_cdc_hiv_prev_ratio)
  )

# Defensive: recreate transformed covariates if not already on the frame
if (!"log_incidence_rates" %in% names(df_hiv_cdc_est))
  df_hiv_cdc_est <- df_hiv_cdc_est %>% mutate(log_incidence_rates = log(incidence_rates))
if (!"log_prop_homeless" %in% names(df_hiv_cdc_est))
  df_hiv_cdc_est <- df_hiv_cdc_est %>% mutate(log_prop_homeless = log(prop_homeless))
if (!"year_factor" %in% names(df_hiv_cdc_est))
  df_hiv_cdc_est <- df_hiv_cdc_est %>% mutate(year_factor = factor(year_id))

##----------------------------------------------------------------
## 4. Robustness regressions (same spec as primary, CDC outcome)
##----------------------------------------------------------------
# R1 - CDC outcome, GBD-based spending predictor (isolates outcome-side effect)
m_cdc_out <- lm(
  as_cdc_mort_prev_ratio_log ~ rw_dex_hiv_prev_ratio_log + year_factor +
    race_prop_BLCK + log_incidence_rates + race_prop_HISP + log_prop_homeless,
  data = df_hiv_cdc_est
)

# R2 - Full CDC analog (CDC in numerator AND denominator of every ratio)
m_cdc_full <- lm(
  as_cdc_mort_prev_ratio_log ~ rw_dex_cdc_hiv_prev_ratio_log + year_factor +
    race_prop_BLCK + log_incidence_rates + race_prop_HISP + log_prop_homeless,
  data = df_hiv_cdc_est
)

# Cluster-robust (CR2 / Satterthwaite) SEs, clustered by state
tidy_cr2 <- function(m, df) {
  vc <- vcovCR(m, cluster = df$location_id, type = "CR2")
  ct <- coef_test(m, vcov = vc, test = "Satterthwaite")
  out <- data.frame(term = rownames(ct), ct, row.names = NULL)
  out$signif_stars <- with(out,
                           ifelse(p_Satt < 0.001, "***",
                                  ifelse(p_Satt < 0.01,  "**",
                                         ifelse(p_Satt < 0.05,  "*", ""))))
  out
}

coef_cdc_out  <- tidy_cr2(m_cdc_out,  df_hiv_cdc_est) %>% mutate(model = "between_yfe__primary_CDCoutcomeOnly")
coef_cdc_full <- tidy_cr2(m_cdc_full, df_hiv_cdc_est) %>% mutate(model = "between_yfe__primary_CDCfull")
coef_robust_cdc <- bind_rows(coef_cdc_out, coef_cdc_full)

cat("\n--- CDC robustness coefficients (non-year-FE terms) ---\n")
print(coef_robust_cdc %>% filter(!grepl("year_factor", term)))

cat(sprintf("\nN state-years in CDC estimation sample: %d (of %d total in df_hiv)\n",
            nrow(df_hiv_cdc_est), nrow(df_hiv_base)))

##----------------------------------------------------------------
## 5. Save outputs
##----------------------------------------------------------------
write.csv(cdc_gbd_corr_tbl, file.path(dir_output, "cdc_gbd_correlations.csv"), row.names = FALSE)
write.csv(coef_robust_cdc,  file.path(dir_output, "regression_results_hiv_cdc_robustness.csv"), row.names = FALSE)



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

###Diagnostics below

# summary(df_as$as_spend_per_capita)
# 
# 
# 
# # 1. Did pop come through from the parquet?
# summary(df_dex$pop)
# sum(is.na(df_dex$pop)) / nrow(df_dex)   # fraction NA
# 
# # 2. Did the lookup table build correctly?
# summary(df_dex_pop_lookup$dex_pop)
# head(df_dex_pop_lookup)
# nrow(df_dex_pop_lookup)
# 
# # 3. Did the join populate dex_pop?
# summary(df_m_pop$dex_pop)
# sum(is.na(df_m_pop$dex_pop)) / nrow(df_m_pop)
# 
# # 4. After sex collapse?
# summary(df_m_collapse$dex_pop)
# summary(df_m_collapse$spend_per_capita)
# 
# # 5. Sanity: prev_ratio path should still work (acts as control)
# summary(df_m_collapse$spend_prev_ratio)   # should be non-zero
# summary(df_as$as_spend_prev_ratio)        # should be non-zero
# 
# 
# 
# df_as %>%
#   group_by(cause_name) %>%
#   summarize(
#     median_pc = median(as_spend_per_capita, na.rm = TRUE),
#     mean_pc   = mean(as_spend_per_capita,   na.rm = TRUE),
#     p95_pc    = quantile(as_spend_per_capita, 0.95, na.rm = TRUE),
#     .groups = "drop"
#   )
# 
# 
# df_as %>%
#   filter(geo == "state",
#          cause_name == "Opioid use disorders",
#          year_id == 2017) %>%
#   group_by(location_name) %>%
#   summarize(median_pc = median(as_spend_per_capita)) %>%
#   arrange(desc(median_pc)) %>%
#   head(10)
