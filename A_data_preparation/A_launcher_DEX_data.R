##----------------------------------------------------------------
##' Title: A_launcher_DEX_data.R
##'
##' Purpose: Launches jobs to pull / create DEX County level data
##' Use: Modify the "Create Parameter file" section to change which years / causes have data being created
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

##----------------------------------------------------------------
## 0.1 Functions
##----------------------------------------------------------------
# Function to ensure filepath / folders exist
ensure_path <- function(filepath) {
  dirpath <- dirname(filepath)
  if (!dir.exists(dirpath)) {
    dir.create(dirpath, recursive = TRUE, showWarnings = FALSE)
  }
  return(filepath)
}

##----------------------------------------------------------------
## 1. Create Parameter file
##----------------------------------------------------------------
# Years
y <- c(2010:2019)

# Causes
# c <- fread("/ihme/dex/us_county/maps/causelist_figures.csv")[acause != "lri_corona",acause] # This file has a list of all the causes
c <- c("hiv", "mental_alcohol", "mental_drug_agg", "mental_drug_opioids") # These are the causes we are running on only

# Payers
p <- c("mdcr", "mdcd", "priv", "oop", "all")

# Create combination table
combos <- tidyr::crossing(y,c,p) %>% as.data.table()
combos[, task_id := 1:.N]

# Write param list
param_path <- paste0("/ihme/homes/idrisov/aim_outputs/Aim2/R_resources/dex_estimates_parameters.csv")
ensure_path(param_path)

write.csv(combos, param_path, row.names = F)

# Define output and log directory paths
log_dir <- file.path("/ihme/homes/idrisov/aim_outputs/Aim2/A_data_preparation/logs/")

# Create output and log directories
ensure_path(log_dir)

##----------------------------------------------------------------
## 2. Launch Jobs
##----------------------------------------------------------------
fp_runner_script <- "/ihme/homes/idrisov/repo/Aim2/A_data_preparation/A_worker_DEX_data.R"

SUBMIT_ARRAY_JOB(
    name = 'dex_GHDx_data',
    script = fp_runner_script, 
    error_dir = log_dir,
    output_dir = log_dir,
    queue = "all.q", # string "all.q" or "long.q"
    memory = "10G", # string "#G"
    threads = "1", # string "#"
    time = "01:30:00", # string "##:##:##" - takes about 50~ minutes to run all 
    archive = F, # Don't need archive? Maybe only needed if writing to the archive drive?
    throttle = 3000, # Not sure what this does
    n_jobs = nrow(combos),
    hold = NULL, # Not sure what this does
    args = param_path,
    test = F
)


