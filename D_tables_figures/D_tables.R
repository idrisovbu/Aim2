##----------------------------------------------------------------
##' Title: D_tables.R
##'
##' Purpose: Creates tables for Aim2
##----------------------------------------------------------------

##----------------------------------------------------------------
## 0. Clear environment and set library paths
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

# "frontier" package data
date_fa <- "20251204"
fp_fa_hiv_simple <- file.path(h, "/aim_outputs/Aim2/C_frontier_analysis/", date_fa, "fa_estimates_hiv_simple.parquet")
fp_fa_hiv_extended <- file.path(h, "/aim_outputs/Aim2/C_frontier_analysis/", date_fa, "fa_estimates_hiv_extended.parquet")
fp_fa_sud_simple <- file.path(h, "/aim_outputs/Aim2/C_frontier_analysis/", date_fa, "fa_estimates__subs_simple.parquet")
fp_fa_sud_extended <- file.path(h, "/aim_outputs/Aim2/C_frontier_analysis/", date_fa, "fa_estimates__subs_extended.parquet")

# SFMA python package data
date_smfa <- "20260105"
fp_sfma_hiv <- file.path(h, "/aim_outputs/Aim2/C_frontier_analysis/", date_smfa, "hiv_output.csv")
fp_sfma_sud <- file.path(h, "/aim_outputs/Aim2/C_frontier_analysis/", date_smfa, "_subs_output.csv")

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

# Frontier Analysis Data
df_hiv_fa_simple <- read_parquet(fp_fa_hiv_simple)
df_hiv_fa_extended <- read_parquet(fp_fa_hiv_extended)
df_sud_fa_simple <- read_parquet(fp_fa_sud_simple)
df_sud_fa_extended <- read_parquet(fp_fa_sud_extended)

# SFMA python package data
df_sfma_hiv <- read.csv(fp_sfma_hiv)
df_sfma_sud <- read.csv(fp_sfma_sud)

##----------------------------------------------------------------
## 0.3 Join FA simple to extended data together
##----------------------------------------------------------------
# HIV
df_hiv_fa <- left_join(
  x = df_hiv_fa_simple,
  y = df_hiv_fa_extended %>% select(!c("spend_mean", "pred_mean")),
  by = c("state_name", "cnty_name", "fips_ihme", "location_id", "acause", 
         "year_id", "sex_id", "age_name_10_yr_bin")
)

rm(df_hiv_fa_extended)
rm(df_hiv_fa_simple)

# SUD
df_sud_fa <- left_join(
  x = df_sud_fa_simple,
  y = df_sud_fa_extended %>% select(!c("spend_mean", "pred_mean")),
  by = c("state_name", "cnty_name", "fips_ihme", "location_id", "acause", 
         "year_id", "sex_id", "age_name_10_yr_bin")
)

rm(df_sud_fa_extended)
rm(df_sud_fa_simple)

##----------------------------------------------------------------
## 1. Table 1 - HIV DEX
## What are the top 10 most expensive and least expensive counties
## in terms of spending for patients with HIV (all age groups all years all toc combined)?
##
## TODO - needs age standardizing
##----------------------------------------------------------------

df_t1 <- df_dex %>%
  filter(acause == "hiv") %>%
  filter(geo == "county")

# Group by then calculate spend_mean
df_t1 <- df_t1 %>%
  group_by(state_name, location_name) %>%
  summarize("spend_mean" = mean(spend_mean))

# Get top 10 and bottom 10
df_t1_top <- df_t1 %>% 
  arrange(desc(spend_mean)) %>%
  ungroup() %>%
  mutate(rank = as.character(row_number())) %>%
  head(10)

df_t1_bot <- df_t1 %>% 
  arrange(desc(spend_mean)) %>%
  ungroup() %>%
  mutate(rank = as.character(row_number())) %>%
  tail(10)

# Convert to dollars
df_t1_top <- convert_to_dollars(df_t1_top, "spend_mean")
df_t1_bot <- convert_to_dollars(df_t1_bot, "spend_mean")

# Add ... row to top data
ellipsis_row <- tibble(
  state_name   = "...",
  location_name = "...",
  spend_mean  = "...",
  rank = "..."
)

# Bind it in
df_t1_all <- bind_rows(df_t1_top, ellipsis_row, df_t1_bot)

# Arrange columns to desired output
df_t1_all <- df_t1_all %>%
  select(rank, state_name, location_name, spend_mean) %>%
  setnames(old = c("rank", "state_name", "location_name", "spend_mean"), 
           new = c("Rank", "State", "County", "Estimated Average Spending HIV (USD)"))

# Write to CSV
write.csv(df_t1_all, file.path(dir_output, "T1_HIV_top_bottom_10.csv"), row.names = FALSE)

##----------------------------------------------------------------
## 2. Table 2 - SUD DEX
## What are the top 10 most expensive and least expensive counties
## in terms of spending for patients with SUD (all age groups all years all toc combined)?
##
## TODO - needs age standardizing
##----------------------------------------------------------------

subs_causes <- c("mental_alcohol", "mental_drug_agg", "mental_drug_opioids")

# Fix FIPS codes 
df_t2 <- df_dex %>%
  filter(acause %in% subs_causes)

# Filter down to just counties
df_t2 <- df_t2 %>%
  filter(geo == "county")

# Group by then calculate spend_mean
df_t2 <- df_t2 %>%
  group_by(state_name, location_name) %>%
  summarize("spend_mean" = mean(spend_mean))

# Get top 10 and bottom 10
df_t2_top <- df_t2 %>% 
  arrange(desc(spend_mean)) %>%
  ungroup() %>%
  mutate(rank = as.character(row_number())) %>%
  head(10)

df_t2_bot <- df_t2 %>% 
  arrange(desc(spend_mean)) %>%
  ungroup() %>%
  mutate(rank = as.character(row_number())) %>%
  tail(10)

# Convert to dollars
df_t2_top <- convert_to_dollars(df_t2_top, "spend_mean")
df_t2_bot <- convert_to_dollars(df_t2_bot, "spend_mean")

# Add ... row to top data
ellipsis_row <- tibble(
  state_name   = "...",
  location_name = "...",
  spend_mean  = "...",
  rank = "..."
)

# Bind it in
df_t2_all <- bind_rows(df_t2_top, ellipsis_row, df_t2_bot)

# Arrange columns to desired output
df_t2_all <- df_t2_all %>%
  select(rank, state_name, location_name, spend_mean) %>%
  setnames(old = c("rank", "state_name", "location_name", "spend_mean"), 
           new = c("Rank", "State", "County", "Estimated Average Spending SUD (USD)"))

# Write to CSV
write.csv(df_t2_all, file.path(dir_output, "T2_SUD_top_bottom_10.csv"), row.names = FALSE)

##----------------------------------------------------------------
## 3. Table 3 - HIV & SUD FA 
## What are the top 10 most efficient and least efficient counties 
## in terms of spending per case versus deaths per case for patients 
## with HIV for all years, all ages, all sexes? (These could also be faceted by age*sex)
##
## This involves collapsing on year / sex / age group to get a mean efficiency score 
## per each county, then basically sorting the counties by efficiency score 
## and picking top 10 / bottom 10, two tables, one for SUD, one for HIV
##
## TODO -
##----------------------------------------------------------------

t3_cols_to_group <- c("state_name", "cnty_name", "fips_ihme", "location_id")

# SUD --
df_t3_sud <- df_sud_fa %>% 
  group_by(across(all_of(t3_cols_to_group))) %>%
  summarize(
    eff_simple = mean(eff_simple),
    eff_extended = mean(eff_extended)
  )

# Top 10 SUD
df_t3_sud_top_10 <- df_t3_sud %>% 
  arrange(desc(eff_extended)) %>%
  ungroup() %>%
  mutate(rank = as.character(row_number())) %>%
  head(10)

# Bottom 10 SUD
df_t3_sud_bot_10 <- df_t3_sud %>% 
  arrange(desc(eff_extended)) %>%
  ungroup() %>%
  mutate(rank = as.character(row_number())) %>%
  tail(10)

# Add ... row to top data
t3_ellipsis_row <- tibble(
  state_name   = "...",
  cnty_name = "...",
  eff_extended  = "...",
  rank = "..."
)

# Bind it in
df_t3_sud_top_10$eff_extended <- as.character(df_t3_sud_top_10$eff_extended)
df_t3_sud_bot_10$eff_extended <- as.character(df_t3_sud_bot_10$eff_extended)

df_t3_sud_all <- bind_rows(df_t3_sud_top_10, t3_ellipsis_row, df_t3_sud_bot_10)

# Arrange columns to desired output
df_t3_sud_all <- df_t3_sud_all %>%
  select(rank, cnty_name, state_name, eff_extended) %>%
  setnames(old = c("rank", "cnty_name", "state_name", "eff_extended"), 
           new = c("Rank", "County", "State", "Efficiency Score"))

# HIV  --
df_t3_hiv <- df_hiv_fa %>% 
  group_by(across(all_of(t3_cols_to_group))) %>%
  summarize(
    eff_simple = mean(eff_simple),
    eff_extended = mean(eff_extended)
  )

# Top 10 HIV
df_t3_hiv_top_10 <- df_t3_hiv %>% 
  arrange(desc(eff_extended)) %>%
  ungroup() %>%
  mutate(rank = as.character(row_number())) %>%
  head(10)

# Bottom 10 HIV
df_t3_hiv_bot_10 <- df_t3_hiv %>% 
  arrange(desc(eff_extended)) %>%
  ungroup() %>%
  mutate(rank = as.character(row_number())) %>%
  tail(10)

# Add ... row to top data
t3_ellipsis_row <- tibble(
  state_name   = "...",
  cnty_name = "...",
  eff_extended  = "...",
  rank = "..."
)

# Bind it in
df_t3_hiv_top_10$eff_extended <- as.character(df_t3_hiv_top_10$eff_extended)
df_t3_hiv_bot_10$eff_extended <- as.character(df_t3_hiv_bot_10$eff_extended)

df_t3_hiv_all <- bind_rows(df_t3_hiv_top_10, t3_ellipsis_row, df_t3_hiv_bot_10)

# Arrange columns to desired output
df_t3_hiv_all <- df_t3_hiv_all %>%
  select(rank, cnty_name, state_name, eff_extended) %>%
  setnames(old = c("rank", "cnty_name", "state_name", "eff_extended"), 
           new = c("Rank", "County", "State", "Efficiency Score"))

# Write to CSV
write.csv(df_t3_hiv_all, file.path(dir_output, "T3_HIV_fa_top_bottom_10.csv"), row.names = FALSE)
write.csv(df_t3_sud_all, file.path(dir_output, "T3_SUD_fa_top_bottom_10.csv"), row.names = FALSE)

# # which are we missing.. (might just be due to the sampling..)
# df_locs <- read.csv(file.path(h, "aim_outputs/Aim2/R_resources/county_fips_locs.csv"))
#
# df_anti <- anti_join(
#   x = df_t3_sud,
#   y = df_locs,
#   by = c("state_name", "cnty_name", "fips_ihme" = "cnty")
# )
# 
# df_anti <- anti_join(
#   y = df_t3_sud,
#   x = df_locs,
#   by = c("state_name", "cnty_name", "cnty" = "fips_ihme")
# )

##----------------------------------------------------------------
## 4. Table 4 - HIV & SUD FA
## Which counties have shifted the most in an upward and downward trajectory 
## in terms of their efficiency scores over the 10 year period (2010 ~ 2019)? 
## (These could also be faceted by age*sex)
##
## Maybe look at top 10 that have increased in efficiency the most, along with bottom 10,
## collapse on age group and sex, then take delta from 
## 2010 to 2019 (to calculate, take 2019 minus 2010), then rank order, 
## this tells us biggest + and biggest - delta
##
## TODO -
##----------------------------------------------------------------

t4_cols_to_group <- c("state_name", "cnty_name", "fips_ihme", "location_id", "year_id")

# SUD -----------------------------------
df_t4_sud <- df_sud_fa %>% 
  group_by(across(all_of(t4_cols_to_group))) %>%
  summarize(
    eff_simple = mean(eff_simple),
    eff_extended = mean(eff_extended)
  ) %>%
  filter(year_id %in% c(2010, 2019)) %>%
  pivot_wider(
    names_from = year_id,
    values_from = c(eff_simple, eff_extended),
    names_sep = "_"
  ) %>%
  mutate(
    delta_eff_extended = eff_extended_2019 - eff_extended_2010,
    delta_eff_simple   = eff_simple_2019 - eff_simple_2010
  )

# Top 10 SUD
df_t4_sud_top_10 <- df_t4_sud %>% 
  arrange(desc(delta_eff_extended)) %>%
  ungroup() %>%
  mutate(rank = as.character(row_number())) %>%
  head(10)

# Bottom 10 SUD
df_t4_sud_bot_10 <- df_t4_sud %>% 
  arrange(desc(delta_eff_extended)) %>%
  ungroup() %>%
  mutate(rank = as.character(row_number())) %>%
  tail(10)

# Add ... row to top data
t4_ellipsis_row <- tibble(
  state_name   = "...",
  cnty_name = "...",
  delta_eff_extended  = "...",
  rank = "..."
)

# Bind it in
df_t4_sud_top_10$delta_eff_extended <- as.character(df_t4_sud_top_10$delta_eff_extended)
df_t4_sud_bot_10$delta_eff_extended <- as.character(df_t4_sud_bot_10$delta_eff_extended)

df_t4_sud_all <- bind_rows(df_t4_sud_top_10, t4_ellipsis_row, df_t4_sud_bot_10)

# Arrange columns to desired output
df_t4_sud_all <- df_t4_sud_all %>%
  select(rank, cnty_name, state_name, delta_eff_extended) %>%
  setnames(old = c("rank", "cnty_name", "state_name", "delta_eff_extended"), 
           new = c("Rank", "County", "State", "Efficiency Score Delta (2010-2019)"))

# HIV  -----------------------------------
df_t4_hiv <- df_hiv_fa %>% 
  group_by(across(all_of(t4_cols_to_group))) %>%
  summarize(
    eff_simple = mean(eff_simple),
    eff_extended = mean(eff_extended)
  ) %>%
  filter(year_id %in% c(2010, 2019)) %>%
  pivot_wider(
    names_from = year_id,
    values_from = c(eff_simple, eff_extended),
    names_sep = "_"
  ) %>%
  mutate(
    delta_eff_extended = eff_extended_2019 - eff_extended_2010,
    delta_eff_simple   = eff_simple_2019 - eff_simple_2010
  )

# Top 10 HIV
df_t4_hiv_top_10 <- df_t4_hiv %>% 
  arrange(desc(delta_eff_extended)) %>%
  ungroup() %>%
  mutate(rank = as.character(row_number())) %>%
  head(10)

# Bottom 10 HIV
df_t4_hiv_bot_10 <- df_t4_hiv %>% 
  arrange(desc(delta_eff_extended)) %>%
  ungroup() %>%
  mutate(rank = as.character(row_number())) %>%
  tail(10)

# Add ... row to top data
t4_ellipsis_row <- tibble(
  state_name   = "...",
  cnty_name = "...",
  delta_eff_extended  = "...",
  rank = "..."
)

# Bind it in
df_t4_hiv_top_10$delta_eff_extended <- as.character(df_t4_hiv_top_10$delta_eff_extended)
df_t4_hiv_bot_10$delta_eff_extended <- as.character(df_t4_hiv_bot_10$delta_eff_extended)

df_t4_hiv_all <- bind_rows(df_t4_hiv_top_10, t4_ellipsis_row, df_t4_hiv_bot_10)

# Arrange columns to desired output
df_t4_hiv_all <- df_t4_hiv_all %>%
  select(rank, cnty_name, state_name, delta_eff_extended) %>%
  setnames(old = c("rank", "cnty_name", "state_name", "delta_eff_extended"), 
           new = c("Rank", "County", "State", "Efficiency Score Delta (2010-2019)"))

# Write to CSV
write.csv(df_t4_hiv_all, file.path(dir_output, "T4_HIV_fa_delta_top_bottom_10.csv"), row.names = FALSE)
write.csv(df_t4_sud_all, file.path(dir_output, "T4_SUD_fa_delta_top_bottom_10.csv"), row.names = FALSE)


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




