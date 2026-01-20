##----------------------------------------------------------------
##' Title: D_tables.R
##'
##' Purpose: Creates tables for Aim2
##----------------------------------------------------------------

##----------------------------------------------------------------
## Clear environment and set library paths
##----------------------------------------------------------------
rm(list = ls())

pacman::p_load(dplyr, openxlsx, RMySQL, data.table, ini, DBI, tidyr, openxlsx, reticulate, ggpubr, arrow, scales)
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
# Set path for DEX data
date_dex <- "20251123"
fp_dex <- file.path(h, "/aim_outputs/Aim2/B_aggregation/", date_dex, "/compiled_dex_data_2010_2019.parquet")

date_ushd <- "20251123"
fp_ushd <- file.path(h, "/aim_outputs/Aim2/B_aggregation/", date_ushd, "/compiled_ushd_data_2010_2019.parquet")

# Age-standardized State level GBD + Dex data
date_as <- "20260119"
fp_as <- file.path(h, "/aim_outputs/Aim2/C_frontier_analysis/", date_as, "/df_as.csv")

# State level GBD + Dex data - No years
date_as_no_year <- "20260119"
fp_as_no_year <- file.path(h, "/aim_outputs/Aim2/C_frontier_analysis/", date_as_no_year, "/df_as_no_year.csv")

# Set output directories
date_today <- format(Sys.time(), "%Y%m%d")
dir_output <- file.path(h, "/aim_outputs/Aim2/D_tables_figures/", date_today)
ensure_dir_exists(dir_output)

##----------------------------------------------------------------
## 0.2 Read in data
##----------------------------------------------------------------
# DEX data
df_dex <- read_parquet(fp_dex)

# USHD Data
#df_ushd <- read_parquet(fp_ushd)

# Age-standardized State level GBD + Dex data
df_as <- read.csv(fp_as)

# Age-standardized State level GBD + Dex data, years collapsed
df_as_no_year <- read.csv(fp_as_no_year)

##----------------------------------------------------------------
## 1. Table 1 - HIV & SUD DEX data
## What are the top 10 most expensive and least expensive counties
## in terms of spending for patients with HIV (all age groups all years all toc combined)?
##
## TODO - needs age standardizing
##----------------------------------------------------------------
# HIV #
df_t1_hiv <- df_dex %>%
  filter(acause == "hiv") %>%
  filter(geo == "county") %>%
  filter(payer == "all")

# Group by then calculate spend_mean
df_t1_hiv <- df_t1_hiv %>%
  group_by(state_name, location_name) %>%
  summarize("spend_mean" = mean(spend_mean))

# Get top 10 and bottom 10
df_t1_hiv_top <- df_t1_hiv %>% 
  arrange(desc(spend_mean)) %>%
  ungroup() %>%
  mutate(rank = as.character(row_number())) %>%
  head(10)

df_t1_hiv_bot <- df_t1_hiv %>% 
  arrange(desc(spend_mean)) %>%
  ungroup() %>%
  mutate(rank = as.character(row_number())) %>%
  tail(10)

# Convert to dollars
df_t1_hiv_top <- convert_to_dollars(df_t1_hiv_top, "spend_mean")
df_t1_hiv_bot <- convert_to_dollars(df_t1_hiv_bot, "spend_mean")

# Add ... row to top data
ellipsis_row <- tibble(
  state_name   = "...",
  location_name = "...",
  spend_mean  = "...",
  rank = "..."
)

# Bind it in
df_t1_hiv_all <- bind_rows(df_t1_hiv_top, ellipsis_row, df_t1_hiv_bot)

# Arrange columns to desired output
df_t1_hiv_all <- df_t1_hiv_all %>%
  select(rank, state_name, location_name, spend_mean) %>%
  setnames(old = c("rank", "state_name", "location_name", "spend_mean"), 
           new = c("Rank", "State", "County", "Estimated Average Spending HIV (2019 USD)"))

# Write to CSV
write.csv(df_t1_hiv_all, file.path(dir_output, "T1_HIV.csv"), row.names = FALSE)



# SUD #
subs_causes <- c("mental_alcohol", "mental_drug_agg", "mental_drug_opioids")

# Fix FIPS codes 
df_t1_sud <- df_dex %>%
  filter(acause %in% subs_causes) %>%
  filter(geo == "county") %>%
  filter(payer == "all")

# Group by then calculate spend_mean
df_t1_sud <- df_t1_sud %>%
  group_by(state_name, location_name) %>%
  summarize("spend_mean" = mean(spend_mean))

# Get top 10 and bottom 10
df_t1_sud_top <- df_t1_sud %>% 
  arrange(desc(spend_mean)) %>%
  ungroup() %>%
  mutate(rank = as.character(row_number())) %>%
  head(10)

df_t1_sud_bot <- df_t1_sud %>% 
  arrange(desc(spend_mean)) %>%
  ungroup() %>%
  mutate(rank = as.character(row_number())) %>%
  tail(10)

# Convert to dollars
df_t1_sud_top <- convert_to_dollars(df_t1_sud_top, "spend_mean")
df_t1_sud_bot <- convert_to_dollars(df_t1_sud_bot, "spend_mean")

# Add ... row to top data
ellipsis_row <- tibble(
  state_name   = "...",
  location_name = "...",
  spend_mean  = "...",
  rank = "..."
)

# Bind it in
df_t1_sud_all <- bind_rows(df_t1_sud_top, ellipsis_row, df_t1_sud_bot)

# Arrange columns to desired output
df_t1_sud_all <- df_t1_sud_all %>%
  select(rank, state_name, location_name, spend_mean) %>%
  setnames(old = c("rank", "state_name", "location_name", "spend_mean"), 
           new = c("Rank", "State", "County", "Estimated Average Spending SUD (2019 USD)"))

# Write to CSV
write.csv(df_t1_sud_all, file.path(dir_output, "T1_SUD.csv"), row.names = FALSE)

##----------------------------------------------------------------
## 2. Table 2 - HIV & SUD 2010 ~ 2019
# 
# T2 - ALL State level table, HIV & SUD separate tables, states are ordered alphabetically
# 
# Columns: State, Spending per prevalence (total spending for state / total summed prevalence count),
# Total spending (cumulative total state spending), 
# 4 total spending by payer (medicaid, medicare, private, oop), 
# prevalence count, 
# prevalence rate, 
# mortality per case (mortality count / prevalence count),
# incidence count, 
# incidence rate
##----------------------------------------------------------------
acauses <- c("hiv", "_subs")

t2_cols <- c("cause_name", "location_name", "spend_per_prev", "spend_all", 
  "spend_mdcd", "spend_mdcr", "spend_oop", "spend_priv", "prevalence_counts", 
  "prevalence_rates", "mort_prev_ratio", "incidence_counts", "incidence_rates"
)

t2_dol_cols <-  c("spend_per_prev", "spend_all", 
                  "spend_mdcd", "spend_mdcr", "spend_oop", "spend_priv")

for (a in acauses) {
  
  df_tmp <- df_as_no_year %>%
    filter(acause == a)
  
  # Create columns
  df_tmp$spend_per_prev <- df_tmp$spend_all / df_tmp$prevalence_counts
  
  # Select columns
  df_tmp <- df_tmp %>%
    select(all_of(t2_cols))
  
  # Convert to dollars
  df_tmp <- convert_to_dollars(df_tmp, t2_dol_cols)
  
  # Rename columns
  df_tmp <- df_tmp %>%
    rename(
      Cause = cause_name,
      State = location_name,
      `Spending per prevalence` = spend_per_prev,
      `Total spending - All` = spend_all,
      `Total spending - Medicare` = spend_mdcr,
      `Total spending - Medicaid` = spend_mdcd,
      `Total spending - Out of pocket` = spend_oop,
      `Total spending - Private` = spend_priv,
      `Prevalence Count` = prevalence_counts,
      `Prevalence Rate` = prevalence_rates,
      `Mortality per case` = mort_prev_ratio,
      `Incidence Count` = incidence_counts,
      `Incidence Rate` = incidence_rates
    )
  
  # Assign to df_t2_hiv or df_t2_subs
  assign(paste0("df_t2_", gsub("^_", "", a)), df_tmp, envir = .GlobalEnv)
}

# Write to CSV
write.csv(df_t2_hiv, file.path(dir_output, "T2_HIV.csv"), row.names = FALSE)
write.csv(df_t2_subs, file.path(dir_output, "T2_SUD.csv"), row.names = FALSE)

##----------------------------------------------------------------
## 3. Table 3 - HIV & SUD 
# 
# T3 - ALL State level table, HIV & SUD separate tables, order by state name
# 
# Columns: State, Spending (total spending per state), 
# Spending per case (total spending per state / prevalence count), 
# DALYs count (total count by state), 
# DALYs per case (DALYs count / prevalence count), 
# Spending effectiveness (Spending in year 2019 - Spending in year 2010) / (DALYs in 2019 - DALY in 2010)

# - Table 3 - Spending Effectiveness, basically add two columns, one for the numerator of the SE, one for denonimator
# - Numerator name, Change in spending (2019 - 2010) - Should be spending per case
# - Denom name, Change in DALYs (2019 - 2010) - Should be daly per case
##----------------------------------------------------------------
t3_cols <- c("cause_name", "location_name", "spend_all",
             "spend_prev_ratio", 
             "daly_counts",
             "daly_prev_ratio")

t3_dol_cols <-  c("spend_all", "spend_prev_ratio", "spend_eff_num")

# Create Spending Effectiveness Column
df_t3_year <- df_as %>%
  filter(year_id %in% c(2010, 2019))

df_t3_year <- df_t3_year %>%
  select(c("cause_id", "year_id", "location_id", "location_name", "acause", 
           "cause_name", "spend_all", "daly_counts", "prevalence_counts"))

# Pivot wider to have columns for all different payer types
df_t3_year <- df_t3_year %>% pivot_wider(
  names_from  = year_id,
  values_from = c(spend_all, daly_counts, prevalence_counts)
)

# Create deltas
df_t3_year$spend_eff_num <- ((df_t3_year$spend_all_2019 / df_t3_year$prevalence_counts_2019) - (df_t3_year$spend_all_2010 / df_t3_year$prevalence_counts_2010))
df_t3_year$spend_eff_denom <- ((df_t3_year$daly_counts_2019 / df_t3_year$prevalence_counts_2019) - (df_t3_year$daly_counts_2010 / df_t3_year$prevalence_counts_2010))
df_t3_year$spend_eff <- df_t3_year$spend_eff_num / df_t3_year$spend_eff_denom 

# Create table with other columns to join to
df_t3 <- copy(df_as_no_year)

# Select columns
df_t3 <- df_t3 %>%
  select(all_of(t3_cols))

# Join w/ delta table
df_t3 <- left_join(
  x = df_t3,
  y = df_t3_year,
  by = c("cause_name", "location_name")
) %>%
  select(!c("spend_all_2010", "spend_all_2019", "daly_counts_2010", "daly_counts_2019", "acause", "cause_id", "location_id"))

# Convert to dollars
df_t3 <- convert_to_dollars(df_t3, t3_dol_cols)

# Rename columns
df_t3 <- df_t3 %>%
  rename(
    Cause = cause_name,
    State = location_name,
    `Total spending - All` = spend_all,
    `Spending per case` = spend_prev_ratio,
    `DALY Count` = daly_counts,
    `DALYs per case` = daly_prev_ratio,
    `Change in spending per case (2019 - 2010)` = spend_eff_num,
    `Change in DALYs per case (2019 - 2010)` = spend_eff_denom,
    `Spending effectiveness` = spend_eff
  )

# Split by cause
df_t3_hiv <- df_t3 %>% filter(Cause == "HIV/AIDS") %>% select(!c("prevalence_counts_2010", "prevalence_counts_2019"))
df_t3_subs <- df_t3 %>% filter(Cause == "Substance use disorders") %>% select(!c("prevalence_counts_2010", "prevalence_counts_2019"))

# Write to CSV
write.csv(df_t3_hiv, file.path(dir_output, "T3_HIV.csv"), row.names = FALSE)
write.csv(df_t3_subs, file.path(dir_output, "T3_SUD.csv"), row.names = FALSE)



# # SCRATCH SPACE, SAFE TO DELETE
# 
# # national level, all payers
# df_national <- df_dex %>%
#   filter(geo == "national") %>%
#   filter(acause == "hiv") %>%
#   filter(age_group_years_start == 50) %>%
#   filter(sex_id == 1) %>%
#   filter(year_id == 2010) %>%
#   filter(payer == "all")
# 
# # county level, all payers
# df_county <- df_dex %>%
#   filter(geo == "county") %>%
#   filter(acause == "hiv") %>%
#   filter(age_group_years_start == 50) %>%
#   filter(sex_id == 1) %>%
#   filter(year_id == 2010) %>%
#   filter(payer == "all")
# 
# df_county_nat <- df_county %>%
#   group_by(toc) %>%
#   summarize(spend_mean = mean(spend_mean))
# 
# # comparing means
# sum(df_national$spend_mean)
# sum(df_county$spend_mean)




