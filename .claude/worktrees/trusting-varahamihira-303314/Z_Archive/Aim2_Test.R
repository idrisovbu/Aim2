##----------------------------------------------------------------
##' Title: Aim2_Test.R
##'
##' Purpose: tests out ideas for Aim2
##----------------------------------------------------------------

##----------------------------------------------------------------
## 0. Clear environment and set library paths
##----------------------------------------------------------------
rm(list = ls())

pacman::p_load(dplyr, openxlsx, RMySQL, data.table, ini, DBI, tidyr, openxlsx, reticulate)
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
## 1. Try reading in mortality data
##----------------------------------------------------------------
fp_input_data <- paste0("/mnt/share/dex/us_county/05_analysis/inputs/county_ushd/mort_pop_agesex/")
input_dirs <- list.dirs(fp_input_data, recursive = TRUE)

input_dirs <- input_dirs[2:11] # Removes the initial directory

# Creates data.table listing out directories
dt_dirs <- data.table(
  dir = input_dirs,
  year_id = as.integer(str_extract(input_dirs, "\\d{4}$"))
)

# Read in one df to see what we're working with
df <- open_dataset(input_dirs[1]) %>%
  collect() %>%
  as.data.frame()

# Reads in all of the datasets and combines into a single df
# Be sure to edit the # read in dataset section to only select columns we're interested in
# --- Initialize output list ---
df_list <- list()

for (i in 1:nrow(dt_dirs)) {
  
  # get directory row that contains our data
  dir <- dt_dirs$dir[i]
  
  # message to show what we're reading in
  print(paste0("Data: ", dir))
  start <- Sys.time()
  
  # read in dataset
  df <- open_dataset(dir) %>%
    # select(bene_id, encounter_id, acause, primary_cause, race_cd, sex_id, tot_pay_amt, st_resi) %>%
    collect() %>%
    as.data.frame()
  
  # Add metadata based on path
  df$year_id <- dt_dirs$year_id[i]
  
  # Add dataframe to list
  df_list[[length(df_list) + 1]] <- df
  
  # Print time
  message("Done in: ", Sys.time() - start)
}

# --- Combine all dfs from df_list into one df ---
df <- rbindlist(df_list, use.names = TRUE, fill = TRUE)

# Available columns
c("mcnty", "sex_id", "age_group_years_start", "race", "edu", 
  "raked", "mortality_rate", "mx_lb", "mx_ub", "mx_se", "model_run_name", 
  "acause", "level", "summary_type", "pop", "deaths")

##----------------------------------------------------------------
## 2. Try reading in DEX estimate data / county estimates
##----------------------------------------------------------------
# List out directories
fp_dex_estimates <- file.path(h, "/county_data/")
dirs_dex_estimates <- list.dirs(fp_dex_estimates, recursive = TRUE)[-1]

# Test read in HIV data
dirs_dex_estimates_hiv <- dirs_dex_estimates[1]
files_hiv <- list.files(dirs_dex_estimates_hiv, full.names = TRUE)

# Read in all csv files (reads in all columns of data)
combined_df <- rbindlist(lapply(files_hiv, fread), fill = TRUE)

combined_df_county <- combined_df %>%
  select(c("location_name", "fips")) 

View(combined_df_county %>%
       distinct(location_name, fips))

# 
# df_county_test <- combined_df_county %>%
#   filter(!is.na(spend_mean))

# Available columns
c("year_id", "geo", "location_name", "fips", "payer", "toc", 
  "acause", "cause_name", "age_group_years_start", "age_name", 
  "sex_id", "sex_name", "spend_mean", "spend_lower", "spend_upper", 
  "spend_per_capita_mean", "spend_per_capita_lower", "spend_per_capita_upper", 
  "spend_per_bene_mean", "spend_per_bene_lower", "spend_per_bene_upper", 
  "spend_per_vol_mean", "spend_per_vol_lower", "spend_per_vol_upper", 
  "vol_per_capita_mean", "vol_per_capita_lower", "vol_per_capita_upper", 
  "vol_per_bene_mean", "vol_per_bene_lower", "vol_per_bene_upper"
)

# 1. What are the differences in spending per beneficiary for patients with HIV 
# for each age group based on different types of insurance (Medicare, Medicaid, Private)? (all years, all counties)
# Repeat for SUD

# Figure 1 - HIV
# Notes: TODO calculate CI correctly, needs some research for this

# Columns of interest
cols_of_int_1_hiv <- c("year_id", "geo", "location_name", "fips","payer", "toc", 
  "acause", "cause_name", "age_group_years_start", "age_name", 
  "sex_id", "sex_name",
  "spend_mean", "spend_lower", "spend_upper")

combined_df_hiv <- rbindlist(
  lapply(
    files_hiv,
    function(f) fread(f, select = cols_of_int_1_hiv, showProgress = FALSE)
  ),
  fill = TRUE
)

combined_df_hiv <- combined_df_hiv %>%
  filter(geo == "county") %>%
  filter(payer != "oop") %>%
  filter(!is.na(spend_mean)) %>%
  filter(spend_mean > 0)

# group by: year_id, payer, cause_name
df_hiv <- combined_df_hiv %>%
  group_by(payer, age_name, sex_name) %>%
  summarize(
    "spend_mean" = mean(spend_mean)
  )

# convert to dollars
for (col in colnames(df_hiv)) {
  if (col == "spend_mean") {
    df_hiv[[col]] <- dollar(df_hiv[[col]])
  }
}

# make plot



##----------------------------------------------------------------
## 3. Try reading in USHD mx ratio data
##----------------------------------------------------------------

library(lbd.loader, lib.loc = sprintf("/share/geospatial/code/geospatial-libraries/lbd.loader-%s.%s", 
                                      R.version$major, 
                                      strsplit(R.version$minor, '.', fixed = TRUE)[[1]][[1]]))

library("ushd.dbr", lib.loc = lbd.loader::pkg_loc("ushd.dbr"))

# Get IHME internal ids
source("/ihme/cc_resources/libraries/current/r/get_ids.R")
source("/ihme/cc_resources/libraries/current/r/get_cause_metadata.R")

measure_ids <- get_ids("measure")

cause_id_list <- get_cause_metadata(cause_set_id = 2, release_id = 16, include_all_metadata = TRUE)

hiv_cause_id <- 298
subs_cause_id <- 973

##########
measure_id <- 5 # 5: prevalence, 6: incidence
cause_id <- 973 # _subs example

# Grab the model run id
mx_ratio_model_run <- get_mxratio_model_run(measure_id = measure_id)$mxratio_model_run_id 

get_mxratio_model_run()

# Grab the draw file metadata
mx_ratio_draw_files <- get_mxratio_model_draws_file(mxratio_model_run_id = mx_ratio_model_run,
                                                    cause_id = cause_id)

# Grab summary data
# Can also filter on years, sex, race, location_id, age_group_id. Must be lists.
# May take a while without further filtering
mx_ratio_summary_data <- get_mxratio_model_summary_data(mxratio_model_run_id = mx_ratio_model_run,
                                                        cause_id = cause_id,
                                                        #measure_id = measure_id,
                                                        years = list(2010),
                                                        sex = list(1))


dbload <- import("dbload", convert = FALSE)  # don’t convert to R
# Where did this module come from on disk?
py_run_string("
import dbload, inspect
print('dbload file:', inspect.getfile(dbload))
print('dbload dir :', dir(dbload))
print('dbload ver :', getattr(dbload, '__version__', 'unknown'))
")

py_run_string("
import dbload
print('has mxratio_model    :', hasattr(dbload, 'mxratio_model'))
print('has mx_ratio_model   :', hasattr(dbload, 'mx_ratio_model'))
")

py_run_string("import sys; print(sys.path)")



##----------------------------------------------------------------
## Aim 2: Summaries by payer and by toc, stratified by county, age, sex
## Inputs expected:
##   combined_df (data.frame or data.table) with columns:
##   year_id, fips (or geo), location_name, payer, toc, acause, cause_name,
##   age_group_years_start, age_name, sex_id, sex_name,
##   spend_per_bene_mean, spend_per_bene_lower, spend_per_bene_upper
##----------------------------------------------------------------

pacman::p_load(data.table, dplyr, stringr, readr)

# Make sure we’re on data.table
combined_dt <- as.data.table(combined_df)

# ---- Helpers ----

# 1) Condition filter that is robust to naming
.which_condition <- function(dt, condition = c("HIV","SUD")) {
  condition <- match.arg(condition)
  if (condition == "HIV") {
    idx <- grepl("hiv", dt$acause, ignore.case = TRUE) |
      grepl("^hiv$", dt$cause_name, ignore.case = TRUE)
  } else {
    # SUD
    idx <- grepl("subs|substance", dt$acause, ignore.case = TRUE) |
      grepl("substance", dt$cause_name, ignore.case = TRUE)
  }
  idx
}

# 2) Tidy payer buckets (keep only Medicare, Medicaid, Private, OOP; drop/other -> NA)
.map_payer <- function(x) {
  x <- tolower(trimws(as.character(x)))
  fcase(
    x %in% c("medicare","mc","medicare advantage","traditional medicare"), "Medicare",
    x %in% c("medicaid","md","chip","medi-cal"),                         "Medicaid",
    x %in% c("private","commercial","employer","marketplace"),           "Private",
    x %in% c("oop","ooh","out_of_pocket","out-of-pocket","self-pay"),    "Out-of-pocket",
    default = NA_character_
  )
}

# 3) Age bands (adjust cutpoints if you want JAMA-style bands)
.age_band <- function(age_start) {
  cut(
    as.numeric(age_start),
    breaks = c(-Inf, 18, 35, 50, 65, Inf),
    labels = c("0–17","18–34","35–49","50–64","65+"),
    right = FALSE
  )
}

# 4) Sex handling: "both" (collapse) or "by_sex"
.apply_sex_option <- function(dt, sex_option = c("both","by_sex")) {
  sex_option <- match.arg(sex_option)
  if (sex_option == "both") {
    # collapse by taking the mean across sexes within the grouping below (weighted collapsing would need bene/pop)
    dt[, sex_name := "Both"]
  } else {
    # keep reported sex_name (expects values like "Male"/"Female")
    if (!"sex_name" %in% names(dt)) dt[, sex_name := fifelse(sex_id == 1, "Male",
                                                             fifelse(sex_id == 2, "Female", "Other/Unknown"))]
  }
  dt
}

# 5) Safe county id
.ensure_county_id <- function(dt) {
  if ("fips" %in% names(dt)) {
    dt[, county_id := as.character(fips)]
  } else if ("geo" %in% names(dt)) {
    dt[, county_id := as.character(geo)]
  } else {
    stop("Need either 'fips' or 'geo' in combined_df to identify counties.")
  }
  dt
}

# ---- Core summarizer ----
# What it does:
#   - filters to condition (HIV or SUD)
#   - buckets payers, age bands
#   - creates two outputs:
#       A) payer × year × age_band (optionally by sex)
#       B) toc × year × age_band (optionally by sex)
#   - summarizes spend_per_bene_mean across counties (median & IQR; also mean)
# Notes:
#   County-level spend per bene is already a rate; across-county summaries use
#   distribution summaries (median/IQR) to reduce undue influence of outliers.
summarize_aim2 <- function(dt,
                           condition = c("HIV","SUD"),
                           sex_option = c("both","by_sex"),
                           out_dir = NULL,
                           tag = NULL) {
  
  condition <- match.arg(condition)
  sex_option <- match.arg(sex_option)
  
  # Filter & prep
  d0 <- copy(dt)[.which_condition(dt, condition)]
  if (nrow(d0) == 0L) stop(sprintf("No rows matched condition '%s'. Check 'acause'/'cause_name'.", condition))
  
  d0 <- .ensure_county_id(d0)
  
  # Keep required cols; drop rows missing the core measures
  need_cols <- c("year_id","county_id","payer","toc",
                 "age_group_years_start","sex_id","sex_name",
                 "spend_per_bene_mean","spend_per_bene_lower","spend_per_bene_upper")
  miss <- setdiff(need_cols, names(d0))
  if (length(miss)) stop("combined_df is missing columns: ", paste(miss, collapse = ", "))
  
  d0 <- d0[!is.na(spend_per_bene_mean)]
  
  # Map payers and age bands
  d0[, payer_bkt := .map_payer(payer)]
  d0[, age_band  := .age_band(age_group_years_start)]
  
  # Apply sex option
  d0 <- .apply_sex_option(d0, sex_option)
  
  # Keep only main payer buckets
  d0 <- d0[ payer_bkt %chin% c("Medicare","Medicaid","Private","Out-of-pocket") ]
  
  # ---- A) By payer ----
  # Summaries across counties (distribution stats to characterize variation)
  by_payer <- d0[
    , .(
      n_counties   = uniqueN(county_id),
      # distribution across counties
      bene_mean_median = median(spend_per_bene_mean, na.rm = TRUE),
      bene_mean_p25    = quantile(spend_per_bene_mean, 0.25, na.rm = TRUE, names = FALSE),
      bene_mean_p75    = quantile(spend_per_bene_mean, 0.75, na.rm = TRUE, names = FALSE),
      # simple mean across counties (report; not used for inference)
      bene_mean_mean   = mean(spend_per_bene_mean, na.rm = TRUE)
    ),
    by = .(condition, year_id, sex_name, age_band, payer_bkt)
  ]
  
  by_payer[, condition := condition]  # tag the condition explicitly
  
  setcolorder(by_payer, c("condition","year_id","sex_name","age_band","payer_bkt",
                          "n_counties","bene_mean_median","bene_mean_p25","bene_mean_p75","bene_mean_mean"))
  
  # ---- B) By type of care (toc) ----
  # Keep toc non-missing & give a short label
  d1 <- d0[!is.na(toc) & nzchar(toc)]
  d1[, toc_label := str_to_title(gsub("_"," ", tolower(trimws(toc))))]
  
  by_toc <- d1[
    , .(
      n_counties   = uniqueN(county_id),
      bene_mean_median = median(spend_per_bene_mean, na.rm = TRUE),
      bene_mean_p25    = quantile(spend_per_bene_mean, 0.25, na.rm = TRUE, names = FALSE),
      bene_mean_p75    = quantile(spend_per_bene_mean, 0.75, na.rm = TRUE, names = FALSE),
      bene_mean_mean   = mean(spend_per_bene_mean, na.rm = TRUE)
    ),
    by = .(condition, year_id, sex_name, age_band, toc_label)
  ]
  
  by_toc[, condition := condition]
  setcolorder(by_toc, c("condition","year_id","sex_name","age_band","toc_label",
                        "n_counties","bene_mean_median","bene_mean_p25","bene_mean_p75","bene_mean_mean"))
  
  # ---- Write to disk (optional) ----
  if (!is.null(out_dir)) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    stamp <- format(Sys.time(), "%Y%m%d")
    prefix <- if (is.null(tag)) condition else paste0(condition, "_", tag)
    fwrite(by_payer, file.path(out_dir, sprintf("%s_Aim2_by_payer_%s.csv", prefix, stamp)))
    fwrite(by_toc,   file.path(out_dir, sprintf("%s_Aim2_by_toc_%s.csv",   prefix, stamp)))
  }
  
  list(by_payer = by_payer[], by_toc = by_toc[])
}

##----------------------------------------------------------------
## Run it for HIV and SUD
##----------------------------------------------------------------

# Choose where to save (or set to NULL to skip writing)
aim2_out_dir <- "/mnt/share/limited_use/LU_CMS/DEX/hivsud/aim2/outputs_summaries"

# 1) HIV, collapse sexes (Both)
hiv_out <- summarize_aim2(
  combined_dt,
  condition = "HIV",
  sex_option = "both",
  out_dir = aim2_out_dir,
  tag = "bothsex"
)

# 2) SUD, split by sex
sud_out <- summarize_aim2(
  combined_dt,
  condition = "SUD",
  sex_option = "by_sex",
  out_dir = aim2_out_dir,
  tag = "bysex"
)

# Quick peeks:
print(head(hiv_out$by_payer[order(year_id, age_band, payer_bkt)]))
print(head(hiv_out$by_toc[order(year_id, age_band, toc_label)]))
print(head(sud_out$by_payer[order(year_id, sex_name, age_band, payer_bkt)]))
print(head(sud_out$by_toc[order(year_id, sex_name, age_band, toc_label)]))

