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

##----------------------------------------------------------------
## 1. Table 1 - HIV
## What are the top 10 most expensive and least expensive counties
## in terms of spending for patients with HIV (all age groups all years all toc combined)?
##
## TODO - needs age standardizing
##----------------------------------------------------------------

df_t1 <- df_dex %>%
  filter(acause == "hiv")

# Group by then calculate spend_mean
df_t1 <- df_t1 %>%
  group_by(state_name, cnty_name) %>%
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
  cnty_name = "...",
  spend_mean  = "...",
  rank = "..."
)

# Bind it in
df_t1_all <- bind_rows(df_t1_top, ellipsis_row, df_t1_bot)

# Arrange columns to desired output
df_t1_all <- df_t1_all %>%
  select(rank, state_name, cnty_name, spend_mean) %>%
  setnames(old = c("rank", "state_name", "cnty_name", "spend_mean"), 
           new = c("Rank", "State", "County", "Estimated Average Spending HIV (USD)"))

# Write to CSV
write.csv(df_t1_all, file.path(dir_output, "T1_HIV_top_bottom_10.csv"), row.names = FALSE)


##----------------------------------------------------------------
## 2. Table 2 - SUD
## What are the top 10 most expensive and least expensive counties
## in terms of spending for patients with SUD (all age groups all years all toc combined)?
##
## TODO - needs age standardizing
##----------------------------------------------------------------

subs_causes <- c("mental_alcohol", "mental_drug_agg", "mental_drug_opioids")

# Fix FIPS codes 
df_t2 <- df_dex %>%
  filter(acause %in% subs_causes)

# Group by then calculate spend_mean
df_t2 <- df_t2 %>%
  group_by(state_name, cnty_name) %>%
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
  cnty_name = "...",
  spend_mean  = "...",
  rank = "..."
)

# Bind it in
df_t2_all <- bind_rows(df_t2_top, ellipsis_row, df_t2_bot)

# Arrange columns to desired output
df_t2_all <- df_t2_all %>%
  select(rank, state_name, cnty_name, spend_mean) %>%
  setnames(old = c("rank", "state_name", "cnty_name", "spend_mean"), 
           new = c("Rank", "State", "County", "Estimated Average Spending SUD (USD)"))

# Write to CSV
write.csv(df_t2_all, file.path(dir_output, "T2_SUD_top_bottom_10.csv"), row.names = FALSE)





