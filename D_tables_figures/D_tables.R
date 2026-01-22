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
date_dex <- "20260120"
fp_dex <- file.path(h, "/aim_outputs/Aim2/B_aggregation/", date_dex, "/compiled_dex_data_2010_2019.parquet")

date_ushd <- "20251123"
fp_ushd <- file.path(h, "/aim_outputs/Aim2/B_aggregation/", date_ushd, "/compiled_ushd_data_2010_2019.parquet")

# Age-standardized State level GBD + Dex data
date_as <- "20260121"
fp_as <- file.path(h, "/aim_outputs/Aim2/C_frontier_analysis/", date_as, "/df_as.csv")

# State level GBD + Dex data - No years
fp_as_no_year <- file.path(h, "/aim_outputs/Aim2/C_frontier_analysis/", date_as, "/df_as_no_year.csv")

# Age-standardized data w/ covariates
fp_df_cov <- file.path(h, "/aim_outputs/Aim2/C_frontier_analysis/", date_as, "/df_as_cdc_covariates.csv")

# Set output directories
date_today <- format(Sys.time(), "%Y%m%d")
dir_output <- file.path(h, "/aim_outputs/Aim2/D_tables_figures/", date_today)
ensure_dir_exists(dir_output)

##----------------------------------------------------------------
## 0.2 Read in data
##----------------------------------------------------------------
# DEX data
df_dex <- open_dataset(fp_dex)

# USHD Data
#df_ushd <- read_parquet(fp_ushd)

# Age-standardized State level GBD + Dex data
df_as <- read.csv(fp_as)

# Age-standardized State level GBD + Dex data, years collapsed
df_as_no_year <- read.csv(fp_as_no_year)

# Age-standardized data w/ covariates
df_cov <- read.csv(fp_df_cov)

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
  filter(payer == "all") %>%
  collect()

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
  filter(payer == "all") %>%
  collect()

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

##----------------------------------------------------------------
## 4. Table 4 - HIV only (one table by Year, one table by State)
#
# Title: HIV Percent of Total Spending
#
# Break down of percentage and dollar amount per (State or Year) for each payer group, plus RW spending.
#
# Columns: (State or Year), Medicaid %, Medicaid $, ..., RW %, RW $
##----------------------------------------------------------------
# Year #
df_t4_year <- df_cov %>%
  filter(acause == "hiv") %>%
  select(c("cause_id", "year_id", "location_id", "location_name", 
           "acause", "cause_name", 
           "spend_mdcd", "spend_mdcr", "spend_oop", "spend_priv", "ryan_white_funding_final")) %>%
  group_by(cause_id, year_id, acause, cause_name) %>%
  summarise(
    spend_mdcd = sum(spend_mdcd),
    spend_mdcr = sum(spend_mdcr),
    spend_oop = sum(spend_oop),
    spend_priv = sum(spend_priv),
    spend_rw = sum(ryan_white_funding_final),
    total_spend_plus_rw = sum(spend_mdcd, spend_mdcr, spend_oop, spend_priv, ryan_white_funding_final),
    spend_mdcd_percentage = (spend_mdcd / total_spend_plus_rw)*100,
    spend_mdcr_percentage = (spend_mdcr / total_spend_plus_rw)*100,
    spend_oop_percentage = (spend_oop / total_spend_plus_rw)*100,
    spend_priv_percentage = (spend_priv / total_spend_plus_rw)*100,
    spend_rw_percentage = (spend_rw / total_spend_plus_rw)*100,
  )

# Convert to dollars
t4_dol_cols <- c("spend_mdcd", 
                 "spend_mdcr", "spend_oop", "spend_priv", "spend_rw", "total_spend_plus_rw")

df_t4_year <- convert_to_dollars(df_t4_year, t4_dol_cols)

# Drop unnecessary columns
df_t4_year <- df_t4_year %>%
  ungroup() %>%
  select(!c("cause_id", "acause", "cause_name"))

# Reorder columns
t4_year_col_order <- c("year_id", 
                       "spend_mdcr_percentage", "spend_mdcr",
                       "spend_mdcd_percentage", "spend_mdcd", 
                       "spend_oop_percentage", "spend_oop",
                       "spend_priv_percentage", "spend_priv",
                       "spend_rw_percentage", "spend_rw")

df_t4_year <- df_t4_year %>%
  select(all_of(t4_year_col_order))

# Rename columns
df_t4_year <- df_t4_year %>%
  setnames(
    old = t4_year_col_order,
    new = c("Year", 
            "Medicare %", "Medicare (2019 USD)",
            "Medicaid %", "Medicaid (2019 USD)",
            "Out of pocket %", "Out of pocket (2019 USD)",
            "Private %", "Private (2019 USD)", 
            "Ryan White Funding %", "Ryan White Funding (2019 USD)")
  )


# State #
df_t4_state <- df_cov %>%
  filter(acause == "hiv") %>%
  select(c("cause_id", "year_id", "location_id", "location_name", 
           "acause", "cause_name", 
           "spend_mdcd", "spend_mdcr", "spend_oop", "spend_priv", "ryan_white_funding_final")) %>%
  group_by(cause_id, location_id, location_name, acause, cause_name) %>%
  summarise(
    spend_mdcd = sum(spend_mdcd),
    spend_mdcr = sum(spend_mdcr),
    spend_oop = sum(spend_oop),
    spend_priv = sum(spend_priv),
    spend_rw = sum(ryan_white_funding_final),
    total_spend_plus_rw = sum(spend_mdcd, spend_mdcr, spend_oop, spend_priv, ryan_white_funding_final),
    spend_mdcd_percentage = (spend_mdcd / total_spend_plus_rw)*100,
    spend_mdcr_percentage = (spend_mdcr / total_spend_plus_rw)*100,
    spend_oop_percentage = (spend_oop / total_spend_plus_rw)*100,
    spend_priv_percentage = (spend_priv / total_spend_plus_rw)*100,
    spend_rw_percentage = (spend_rw / total_spend_plus_rw)*100,
  )

# Convert to dollars
t4_dol_cols <- c("spend_mdcd", 
                 "spend_mdcr", "spend_oop", "spend_priv", "spend_rw", "total_spend_plus_rw")

df_t4_state <- convert_to_dollars(df_t4_state, t4_dol_cols)

# Drop unnecessary columns
df_t4_state <- df_t4_state %>%
  ungroup() %>%
  select(!c("cause_id", "acause", "cause_name", "location_id"))

# Reorder columns
t4_state_col_order <- c("location_name", 
                       "spend_mdcr_percentage", "spend_mdcr", 
                       "spend_mdcd_percentage", "spend_mdcd", 
                       "spend_oop_percentage", "spend_oop", 
                       "spend_priv_percentage", "spend_priv", 
                       "spend_rw_percentage", "spend_rw")

df_t4_state <- df_t4_state %>%
  select(all_of(t4_state_col_order))

# Rename columns
df_t4_state <- df_t4_state %>%
  setnames(
    old = t4_state_col_order,
    new = c("State", 
            "Medicare %", "Medicare (2019 USD)", 
            "Medicaid %", "Medicaid (2019 USD)", 
            "Out of pocket %", "Out of pocket (2019 USD)", 
            "Private %", "Private (2019 USD)", 
            "Ryan White Funding %", "Ryan White Funding (2019 USD)")
  )

# Write to CSV
write.csv(df_t4_year, file.path(dir_output, "T4_HIV_year.csv"), row.names = FALSE)
write.csv(df_t4_state, file.path(dir_output, "T4_HIV_state.csv"), row.names = FALSE)

##----------------------------------------------------------------
## 5. Table 5 - HIV & SUD
#
# Title: (HIV / SUD) Percent of Total Spending, <65 & 65+
#
# Break down of percentage and dollar amount per (State or Year) for each payer group, by <65 and 65+ age categories
#
# Columns: Year, Age group, Medicaid %, Medicaid $, ..., OOP %, OOP $
#
# Notes: Using national data to aggregate age groups
##----------------------------------------------------------------
# Filter down DEX data to national level
df_t5 <- df_dex %>%
  filter(geo == "national") %>%
  collect()

# Pivot wider
df_t5_p <- df_t5 %>%
  select(c("year_id", "geo", "location_name", "fips", "payer", "toc", 
    "acause", "cause_name", "age_group_years_start", "age_name", 
    "sex_id", "sex_name", "spend_mean")) %>%
  pivot_wider(
    names_from = payer,
    values_from = spend_mean,
    names_prefix = "spend_"
  )

# Checking delta between payer groups sum and payer = all
df_t5_p$payer_total <- rowSums(
  df_t5_p[, c("spend_mdcd", "spend_mdcr", "spend_oop", "spend_priv")],
  na.rm = TRUE
)

df_t5_p$payer_delta <- df_t5_p$spend_all - df_t5_p$payer_total

# Create <65 and 65+ age groups
df_t5_p <- df_t5_p %>%
  mutate(age_name_65 = case_when(
    age_name == "0 - <1" ~ "<65",
    age_name == "1 - <5" ~ "<65",
    age_name == "5 - <10" ~ "<65",
    age_name == "10 - <15" ~ "<65",
    age_name == "15 - <20" ~ "<65",
    age_name == "20 - <25" ~ "<65",
    age_name == "25 - <30" ~ "<65",
    age_name == "30 - <35" ~ "<65",
    age_name == "35 - <40" ~ "<65",
    age_name == "40 - <45" ~ "<65",
    age_name == "45 - <50" ~ "<65",
    age_name == "50 - <55" ~ "<65",
    age_name == "55 - <60" ~ "<65",
    age_name == "60 - <65" ~ "<65",
    age_name == "65 - <70" ~ "65+",
    age_name == "70 - <75" ~ "65+",
    age_name == "75 - <80" ~ "65+",
    age_name == "80 - <85" ~ "65+",
    age_name == "85+"      ~ "65+"
  ))

# Group by summary based on new age grouping
df_t5_p_hiv <- df_t5_p %>%
  filter(acause == "hiv") %>%
  group_by(year_id, cause_name, age_name_65) %>%
  summarise(
    spend_mdcd = sum(spend_mdcd, na.rm = TRUE),
    spend_mdcr = sum(spend_mdcr, na.rm = TRUE),
    spend_oop = sum(spend_oop, na.rm = TRUE),
    spend_priv = sum(spend_priv, na.rm = TRUE),
    spend_total = sum(spend_mdcd, spend_mdcr, spend_oop, spend_priv),
    spend_mdcd_percentage = (spend_mdcd / spend_total)*100,
    spend_mdcr_percentage = (spend_mdcr / spend_total)*100,
    spend_oop_percentage = (spend_oop / spend_total)*100,
    spend_priv_percentage = (spend_priv / spend_total)*100
  )

df_t5_p_sud <- df_t5_p %>%
  filter(acause != "hiv") %>%
  group_by(year_id, age_name_65) %>%
  summarise(
    spend_mdcd = sum(spend_mdcd, na.rm = TRUE),
    spend_mdcr = sum(spend_mdcr, na.rm = TRUE),
    spend_oop = sum(spend_oop, na.rm = TRUE),
    spend_priv = sum(spend_priv, na.rm = TRUE),
    spend_total = sum(spend_mdcd, spend_mdcr, spend_oop, spend_priv),
    spend_mdcd_percentage = (spend_mdcd / spend_total)*100,
    spend_mdcr_percentage = (spend_mdcr / spend_total)*100,
    spend_oop_percentage = (spend_oop / spend_total)*100,
    spend_priv_percentage = (spend_priv / spend_total)*100
  ) %>%
  mutate(cause_name = "Substance use disorder")

# Rowbind back together
df_t5_p_final <- rbind(df_t5_p_hiv, df_t5_p_sud)

# Convert columns to dollars
t5_dol_cols <- c("spend_mdcd", "spend_mdcr", "spend_oop", "spend_priv", "spend_total")
df_t5_p_final <- convert_to_dollars(df_t5_p_final, t5_dol_cols)

# Reorder columns
t5_state_col_order <- c("cause_name", "year_id", "age_name_65", 
                        "spend_mdcr_percentage", "spend_mdcr", 
                        "spend_mdcd_percentage", "spend_mdcd",  
                        "spend_oop_percentage", "spend_oop", 
                        "spend_priv_percentage", "spend_priv")

df_t5_p_final <- df_t5_p_final %>%
  select(all_of(t5_state_col_order))

# Rename columns
df_t5_p_final <- df_t5_p_final %>%
  setnames(
    old = t5_state_col_order,
    new = c("Cause", "Year", "Age Group",
            "Medicare %", "Medicare (2019 USD)", 
            "Medicaid %", "Medicaid (2019 USD)", 
            "Out of pocket %", "Out of pocket (2019 USD)", 
            "Private %", "Private (2019 USD)"))

# Sort
df_t5_p_final <- df_t5_p_final %>%
  ungroup() %>% 
  mutate(
    `Age Group` = factor(`Age Group`, levels = c("<65", "65+"), ordered = TRUE)
  )


df_t5_p_final <- df_t5_p_final %>%
  arrange(Cause, Year, `Age Group`)

# Write to CSV
write.csv(df_t5_p_final %>% filter(Cause == "HIV/AIDS"), file.path(dir_output, "T5_HIV.csv"), row.names = FALSE)
write.csv(df_t5_p_final %>% filter(Cause == "Substance use disorder"), file.path(dir_output, "T5_SUD.csv"), row.names = FALSE)

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




