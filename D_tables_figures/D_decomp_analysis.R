##----------------------------------------------------------------
##' Title: D_decomp_analysis.R
##'
##' Purpose: TBD
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
## 0.1 Set directories
##----------------------------------------------------------------
# Decomp data at strata we need
date_decomp <- "20260131"
fp_decomp <- file.path(h, "/aim_outputs/Aim2/C_frontier_analysis/", date_decomp, "/df_decomp.csv")

# Set output directories
date_today <- format(Sys.time(), "%Y%m%d")
dir_output <- file.path(h, "/aim_outputs/Aim2/D_tables_figures/", date_today)
ensure_dir_exists(dir_output)

##----------------------------------------------------------------
## 0.2 Read in data
##----------------------------------------------------------------
# DEX data
df_decomp <- read_csv(fp_decomp)

##----------------------------------------------------------------
## 1. Transform data
##----------------------------------------------------------------
# Pivot data
val_cols <- c("prevalence_counts", "mortality_counts", "daly_counts", 
              "incidence_counts", "yll_counts", "yld_counts", "population", 
              "spend_all")

cols_to_drop <- c("spend_mdcd", "spend_mdcr", "spend_oop", "spend_priv")

df_decomp_p <- df_decomp %>%
  filter(year_id %in% c(2010, 2019)) %>%
  select(!all_of(cols_to_drop)) %>%
  pivot_wider(
    names_from = year_id,
    values_from = all_of(val_cols)
  )

# Create columns







