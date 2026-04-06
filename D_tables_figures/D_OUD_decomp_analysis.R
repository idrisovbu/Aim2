##----------------------------------------------------------------
##' Title: D_OUD_decomp_analysis.R
##'
##' Purpose: Das Gupta 4-Factor Decomposition of OUD Spending (2010-2019)
##'          WITH UNCERTAINTY INTERVALS FROM DRAW-LEVEL DATA
##----------------------------------------------------------------

##----------------------------------------------------------------
## Clear environment and set library paths
##----------------------------------------------------------------
rm(list = ls())

pacman::p_load(dplyr, openxlsx, RMySQL, data.table, ini, DBI, tidyr,
               RColorBrewer, forcats, openxlsx, reticulate, ggpubr, 
               arrow, scales, Rcpp)
library(lbd.loader, lib.loc = sprintf("/share/geospatial/code/geospatial-libraries/lbd.loader-%s", R.version$major))
if("dex.dbr" %in% (.packages())) detach("package:dex.dbr", unload=TRUE)
library(dex.dbr, lib.loc = lbd.loader::pkg_loc("dex.dbr"))
suppressMessages(devtools::load_all(path = "/ihme/homes/idrisov/repo/dex_us_county/"))
conflicts_prefer(dplyr::filter)

# Set drive paths
if (Sys.info()["sysname"] == 'Linux'){
  j <- "/home/j/"
  h <- paste0("/ihme/homes/", Sys.info()[7], "/")
  l <- '/ihme/limited_use/'
} else if (Sys.info()["sysname"] == 'Darwin'){
  j <- "/Volumes/snfs"
  h <- paste0("/Volumes/", Sys.info()[7], "/")
  l <- '/Volumes/limited_use'
} else {
  j <- "J:/"
  h <- "H:/"
  l <- 'L:/'
}

##----------------------------------------------------------------
## 0. Functions
##----------------------------------------------------------------
ensure_dir_exists <- function(dir_path) {
  
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
}

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
date_decomp <- "20260406"
fp_decomp <- file.path(h, "/aim_outputs/Aim2/C_frontier_analysis/", date_decomp, "/df_decomp_draws.parquet")

date_today <- format(Sys.time(), "%Y%m%d")
dir_output <- file.path(h, "/aim_outputs/Aim2/D_tables_figures/", date_today)
ensure_dir_exists(dir_output)

##----------------------------------------------------------------
## 0.2 Read in data
##----------------------------------------------------------------
df_decomp <- read_parquet(fp_decomp)

cat("Data loaded. Dimensions:", nrow(df_decomp), "rows x", ncol(df_decomp), "columns\n")
cat("Unique draws:", length(unique(df_decomp$draw)), "\n")
cat("Unique locations:", length(unique(df_decomp$location_name)), "\n")

##----------------------------------------------------------------
## 1. Transform data
##----------------------------------------------------------------
val_cols <- c("prevalence_counts", "mortality_counts", "daly_counts", 
              "incidence_counts", "yll_counts", "yld_counts", "population", 
              "spend_all")

cols_to_drop <- c("spend_mdcd", "spend_mdcr", "spend_oop", "spend_priv",
                  "spend_AM", "spend_ED", "spend_HH", "spend_IP", "spend_NF", "spend_RX")

# KEY CHANGE: Include 'draw' in the ID columns (not pivoted)
df_decomp_p <- df_decomp %>%
  filter(year_id %in% c(2010, 2019)) %>%
  select(!any_of(cols_to_drop)) %>%
  pivot_wider(
    id_cols = c(cause_id, location_id, sex_id, cause_name, location_name, 
                age_name, geo, fips, acause, age_group_years_start, draw),
    names_from = year_id,
    values_from = all_of(val_cols)
  )

# Filter to OUD only
df_decomp_p <- df_decomp_p %>%
  filter(cause_name == "Opioid use disorders")

cat("\nAfter pivot_wider and OUD filter:", nrow(df_decomp_p), "rows\n")
cat("Expected: ~52 locations × 46 age-sex × 51 draws =", 52 * 46 * 51, "\n")


###############################################################################
# Das Gupta 4-Factor Decomposition (WITH UNCERTAINTY)
# 
# Identity: Spend_cell = Pop_state × PrevRate_state × PrevShare_cell × SpendPerCase_cell
#
# Factors:
#   1. Population (state total)
#   2. Case rate (state prevalence / state population) 
#   3. Age-sex structure (cell prevalence / state prevalence)
#   4. Spending intensity (cell spending / cell prevalence)
###############################################################################

# ============================================================================
# STEP 1: Define decomposition function (Das Gupta 1991)
# ============================================================================

cppFunction('NumericVector dcomp(NumericMatrix X, NumericMatrix Y){
    int m = X.nrow();
    int n = X.ncol();
    int i, j, k;
    IntegerVector q(n);
    int kcopy, size_q;
    NumericVector prod_x_in_y_out(m);
    NumericVector prod_y_in_x_out(m);
    IntegerVector denoms(n+1);
    NumericMatrix result(m, n);
    
    denoms[0] = 0;
    denoms[1] = n;
    for(j=1; j<n; j++){
      denoms[j+1] = denoms[j] * (n - j) / j;
      for(i=0; i<m; i++){ result(i, j) = 0.0; }
    }
    
    for(k=1; k<pow(2, n); k++){
      kcopy = k;
      size_q = 0;
      for(i=0; i<m; i++){
        prod_x_in_y_out[i] = 1.0;
        prod_y_in_x_out[i] = 1.0;
      }
      for(j=0; j<n; j++){
        q[j] = kcopy % 2;
        if(q[j] == 1){
          size_q++;
          kcopy--;
          for(i=0; i<m; i++){
            prod_x_in_y_out[i] = prod_x_in_y_out[i] * X(i, j);
            prod_y_in_x_out[i] = prod_y_in_x_out[i] * Y(i, j);
          }
        } else{
          for(i=0; i<m; i++){
            prod_x_in_y_out[i] = prod_x_in_y_out[i] * Y(i, j);
            prod_y_in_x_out[i] = prod_y_in_x_out[i] * X(i, j);
          }
        }
        kcopy = kcopy/2;
      }
      for(j=0; j<n; j++){
        if(q[j] == 1){
          for(i=0; i<m; i++){
            result(i, j) = result(i, j) + (prod_y_in_x_out[i] - prod_x_in_y_out[i])/denoms[size_q];
          }
        }
      }
    }
    return result;
}')

decompose <- function(dt, factor_names, start_year, end_year){
  dt[, paste0(factor_names, "_effect") := as.data.table(
    dcomp(
      as.matrix(.SD[, paste(factor_names, start_year, sep = "_"), with = FALSE]),
      as.matrix(.SD[, paste(factor_names, end_year, sep = "_"), with = FALSE])
    )
  )]
  return(dt[])
}

# ============================================================================
# STEP 2: Prepare data with CORRECT 4-factor identity (BY DRAW)
# ============================================================================

dt <- as.data.table(df_decomp_p)

cat("\n============ CONSTRUCTING FACTORS (BY DRAW) ============\n")

# ---- State-level totals (BY DRAW) ----
# KEY CHANGE: Include 'draw' in grouping
dt[, pop_state_2010 := sum(population_2010), by = .(draw, cause_id, location_id)]
dt[, pop_state_2019 := sum(population_2019), by = .(draw, cause_id, location_id)]

dt[, prev_state_2010 := sum(prevalence_counts_2010), by = .(draw, cause_id, location_id)]
dt[, prev_state_2019 := sum(prevalence_counts_2019), by = .(draw, cause_id, location_id)]

# ---- FACTOR 1: Population size (state total) ----
dt[, population_2010 := pop_state_2010]
dt[, population_2019 := pop_state_2019]

# ---- FACTOR 2: Case rate (state prevalence rate = cases per person) ----
dt[, case_rate_2010 := fifelse(pop_state_2010 > 0, prev_state_2010 / pop_state_2010, 0)]
dt[, case_rate_2019 := fifelse(pop_state_2019 > 0, prev_state_2019 / pop_state_2019, 0)]

# ---- FACTOR 3: Age-sex structure (share of cases in this cell) ----
dt[, age_sex_frac_2010 := fifelse(prev_state_2010 > 0, prevalence_counts_2010 / prev_state_2010, 0)]
dt[, age_sex_frac_2019 := fifelse(prev_state_2019 > 0, prevalence_counts_2019 / prev_state_2019, 0)]

# ---- FACTOR 4: Spending intensity (spend per case in this cell) ----
dt[, spend_per_case_2010 := fifelse(prevalence_counts_2010 > 0, spend_all_2010 / prevalence_counts_2010, 0)]
dt[, spend_per_case_2019 := fifelse(prevalence_counts_2019 > 0, spend_all_2019 / prevalence_counts_2019, 0)]

# Keep original spending for validation
dt[, spend_2010 := spend_all_2010]
dt[, spend_2019 := spend_all_2019]

# ============================================================================
# STEP 2.5: VALIDATION - Check that identity holds (sample of draws)
# ============================================================================

dt[, spend_hat_2010 := population_2010 * case_rate_2010 * age_sex_frac_2010 * spend_per_case_2010]
dt[, spend_hat_2019 := population_2019 * case_rate_2019 * age_sex_frac_2019 * spend_per_case_2019]

dt[, check_2010 := spend_2010 - spend_hat_2010]
dt[, check_2019 := spend_2019 - spend_hat_2019]

cat("\n============ IDENTITY VALIDATION (across all draws) ============\n")
cat("Residuals should be ~0 (tiny numeric noise only)\n\n")
cat("2010 residuals:\n")
print(summary(dt$check_2010))
cat("\n2019 residuals:\n")
print(summary(dt$check_2019))

# ============================================================================
# STEP 3: Run the decomposition (vectorized across all draws)
# ============================================================================

cat("\n============ RUNNING SPENDING DECOMPOSITION ============\n")
cat("Processing", nrow(dt), "rows (all draws at once)...\n")

factor_names <- c("population", "case_rate", "age_sex_frac", "spend_per_case")
dt <- decompose(dt, factor_names = factor_names, start_year = 2010, end_year = 2019)

# Calculate spending change
dt[, delta_spend := spend_2019 - spend_2010]

cat("Decomposition complete.\n")

# ============================================================================
# STEP 4: Aggregate results (TWO-STAGE)
# ============================================================================

cat("\n============ AGGREGATING RESULTS ============\n")

# ----------------------------------------------------------------------------
# STAGE A: Sum cells → location WITHIN each draw
# ----------------------------------------------------------------------------

by_location_draw <- dt[, .(
  pop_size_effect = sum(population_effect, na.rm = TRUE),
  prevalence_rate_effect = sum(case_rate_effect, na.rm = TRUE),
  case_composition_effect = sum(age_sex_frac_effect, na.rm = TRUE),
  spend_intensity_effect = sum(spend_per_case_effect, na.rm = TRUE),
  delta_spend = sum(delta_spend, na.rm = TRUE),
  spend_2010 = sum(spend_2010, na.rm = TRUE),
  spend_2019 = sum(spend_2019, na.rm = TRUE),
  prev_2010 = sum(prevalence_counts_2010, na.rm = TRUE),
  prev_2019 = sum(prevalence_counts_2019, na.rm = TRUE)
), by = .(draw, location_id, location_name)]

# Validation within each draw
by_location_draw[, sum_effects := pop_size_effect + prevalence_rate_effect + 
                   case_composition_effect + spend_intensity_effect]
by_location_draw[, diff_check := delta_spend - sum_effects]

cat("Stage A complete: ", nrow(by_location_draw), "rows (", 
    length(unique(by_location_draw$location_name)), "locations ×", 
    length(unique(by_location_draw$draw)), "draws)\n")
cat("Max decomposition residual:", max(abs(by_location_draw$diff_check)), "\n")

# ----------------------------------------------------------------------------
# STAGE B: Summarize ACROSS draws (mean + 95% UI)
# ----------------------------------------------------------------------------

by_state <- by_location_draw[, .(
  # Factor effects: mean + UI
  pop_size_effect = mean(pop_size_effect, na.rm = TRUE),
  pop_size_effect_lower = quantile(pop_size_effect, 0.025, na.rm = TRUE),
  pop_size_effect_upper = quantile(pop_size_effect, 0.975, na.rm = TRUE),
  
  prevalence_rate_effect = mean(prevalence_rate_effect, na.rm = TRUE),
  prevalence_rate_effect_lower = quantile(prevalence_rate_effect, 0.025, na.rm = TRUE),
  prevalence_rate_effect_upper = quantile(prevalence_rate_effect, 0.975, na.rm = TRUE),
  
  case_composition_effect = mean(case_composition_effect, na.rm = TRUE),
  case_composition_effect_lower = quantile(case_composition_effect, 0.025, na.rm = TRUE),
  case_composition_effect_upper = quantile(case_composition_effect, 0.975, na.rm = TRUE),
  
  spend_intensity_effect = mean(spend_intensity_effect, na.rm = TRUE),
  spend_intensity_effect_lower = quantile(spend_intensity_effect, 0.025, na.rm = TRUE),
  spend_intensity_effect_upper = quantile(spend_intensity_effect, 0.975, na.rm = TRUE),
  
  # Totals: mean + UI
  delta_spend = mean(delta_spend, na.rm = TRUE),
  delta_spend_lower = quantile(delta_spend, 0.025, na.rm = TRUE),
  delta_spend_upper = quantile(delta_spend, 0.975, na.rm = TRUE),
  
  spend_2010 = mean(spend_2010, na.rm = TRUE),
  spend_2010_lower = quantile(spend_2010, 0.025, na.rm = TRUE),
  spend_2010_upper = quantile(spend_2010, 0.975, na.rm = TRUE),
  
  spend_2019 = mean(spend_2019, na.rm = TRUE),
  spend_2019_lower = quantile(spend_2019, 0.025, na.rm = TRUE),
  spend_2019_upper = quantile(spend_2019, 0.975, na.rm = TRUE),
  
  prev_2010 = mean(prev_2010, na.rm = TRUE),
  prev_2019 = mean(prev_2019, na.rm = TRUE)
  
), by = .(location_id, location_name)]

# Add percentages (based on means)
by_state[, `:=`(
  pct_pop_size = 100 * pop_size_effect / delta_spend,
  pct_prevalence_rate = 100 * prevalence_rate_effect / delta_spend,
  pct_case_composition = 100 * case_composition_effect / delta_spend,
  pct_spend_intensity = 100 * spend_intensity_effect / delta_spend
)]

# Decomposition validation (on means)
by_state[, sum_effects := pop_size_effect + prevalence_rate_effect + 
           case_composition_effect + spend_intensity_effect]
by_state[, diff_check := delta_spend - sum_effects]

cat("Stage B complete:", nrow(by_state), "locations with uncertainty intervals\n")

# ============================================================================
# STEP 5: View results
# ============================================================================

# Extract national row
national <- by_state[location_name == "United States"]

cat("\n=========== NATIONAL DECOMPOSITION RESULTS (2010-2019) ===========\n\n")
cat(sprintf("Total Spending 2010:   $%s  (95%% UI: $%s - $%s)\n", 
            format(round(national$spend_2010), big.mark = ","),
            format(round(national$spend_2010_lower), big.mark = ","),
            format(round(national$spend_2010_upper), big.mark = ",")))
cat(sprintf("Total Spending 2019:   $%s  (95%% UI: $%s - $%s)\n", 
            format(round(national$spend_2019), big.mark = ","),
            format(round(national$spend_2019_lower), big.mark = ","),
            format(round(national$spend_2019_upper), big.mark = ",")))
cat(sprintf("Total Spending Change: $%s  (95%% UI: $%s - $%s)\n\n", 
            format(round(national$delta_spend), big.mark = ","),
            format(round(national$delta_spend_lower), big.mark = ","),
            format(round(national$delta_spend_upper), big.mark = ",")))

cat("Factor Contributions (mean, 95% UI):\n")
cat(sprintf("  1. Population Size:      $%15s  (%6.1f%%)  [%s - %s]\n", 
            format(round(national$pop_size_effect), big.mark = ","), national$pct_pop_size,
            format(round(national$pop_size_effect_lower), big.mark = ","),
            format(round(national$pop_size_effect_upper), big.mark = ",")))
cat(sprintf("  2. Prevalence Rate:      $%15s  (%6.1f%%)  [%s - %s]\n", 
            format(round(national$prevalence_rate_effect), big.mark = ","), national$pct_prevalence_rate,
            format(round(national$prevalence_rate_effect_lower), big.mark = ","),
            format(round(national$prevalence_rate_effect_upper), big.mark = ",")))
cat(sprintf("  3. Case Composition:     $%15s  (%6.1f%%)  [%s - %s]\n", 
            format(round(national$case_composition_effect), big.mark = ","), national$pct_case_composition,
            format(round(national$case_composition_effect_lower), big.mark = ","),
            format(round(national$case_composition_effect_upper), big.mark = ",")))
cat(sprintf("  4. Spending Intensity:   $%15s  (%6.1f%%)  [%s - %s]\n", 
            format(round(national$spend_intensity_effect), big.mark = ","), national$pct_spend_intensity,
            format(round(national$spend_intensity_effect_lower), big.mark = ","),
            format(round(national$spend_intensity_effect_upper), big.mark = ",")))

cat(sprintf("\nDecomposition Validation (sum - actual, should be ~0): %.6f\n", national$diff_check))

cat("\n=========== TOP 10 STATES BY SPENDING CHANGE ===========\n")
print(by_state[location_name != "United States"][order(-delta_spend)][1:10, 
                                                                      .(location_name, delta_spend, pct_pop_size, pct_prevalence_rate, 
                                                                        pct_case_composition, pct_spend_intensity, diff_check)])

# ============================================================================
# STEP 6: Save spending decomposition results
# ============================================================================

write.csv(by_state, file.path(dir_output, "T6_OUD_decomp_by_state.csv"), row.names = FALSE)

cat(sprintf("\nSpending decomposition saved to: %s\n", dir_output))


###############################################################################
# PART A: DALY DECOMPOSITION (parallel to spending decomposition)
###############################################################################

cat("\n\n============ DALY DECOMPOSITION ============\n")

# ----------------------------------------------------------------------------
# A.1: Prepare DALY data with same 4-factor identity (BY DRAW)
# ----------------------------------------------------------------------------

dt_daly <- as.data.table(df_decomp_p)

# ---- State-level totals (BY DRAW) ----
dt_daly[, pop_state_2010 := sum(population_2010), by = .(draw, cause_id, location_id)]
dt_daly[, pop_state_2019 := sum(population_2019), by = .(draw, cause_id, location_id)]

dt_daly[, prev_state_2010 := sum(prevalence_counts_2010), by = .(draw, cause_id, location_id)]
dt_daly[, prev_state_2019 := sum(prevalence_counts_2019), by = .(draw, cause_id, location_id)]

# ---- FACTOR 1: Population size (state total) ----
dt_daly[, population_2010 := pop_state_2010]
dt_daly[, population_2019 := pop_state_2019]

# ---- FACTOR 2: Case rate (state prevalence rate = cases per person) ----
dt_daly[, case_rate_2010 := fifelse(pop_state_2010 > 0, prev_state_2010 / pop_state_2010, 0)]
dt_daly[, case_rate_2019 := fifelse(pop_state_2019 > 0, prev_state_2019 / pop_state_2019, 0)]

# ---- FACTOR 3: Age-sex structure (share of cases in this cell) ----
dt_daly[, age_sex_frac_2010 := fifelse(prev_state_2010 > 0, prevalence_counts_2010 / prev_state_2010, 0)]
dt_daly[, age_sex_frac_2019 := fifelse(prev_state_2019 > 0, prevalence_counts_2019 / prev_state_2019, 0)]

# ---- FACTOR 4: DALYs per case ----
dt_daly[, daly_per_case_2010 := fifelse(prevalence_counts_2010 > 0, daly_counts_2010 / prevalence_counts_2010, 0)]
dt_daly[, daly_per_case_2019 := fifelse(prevalence_counts_2019 > 0, daly_counts_2019 / prevalence_counts_2019, 0)]

# Keep original DALYs for validation
dt_daly[, daly_2010 := daly_counts_2010]
dt_daly[, daly_2019 := daly_counts_2019]

# ----------------------------------------------------------------------------
# A.2: Validate DALY identity
# ----------------------------------------------------------------------------

dt_daly[, daly_hat_2010 := population_2010 * case_rate_2010 * age_sex_frac_2010 * daly_per_case_2010]
dt_daly[, daly_hat_2019 := population_2019 * case_rate_2019 * age_sex_frac_2019 * daly_per_case_2019]

dt_daly[, check_2010 := daly_2010 - daly_hat_2010]
dt_daly[, check_2019 := daly_2019 - daly_hat_2019]

cat("\n============ DALY IDENTITY VALIDATION ============\n")
cat("Residuals should be ~0 (tiny numeric noise only)\n\n")
cat("2010 residuals:\n")
print(summary(dt_daly$check_2010))
cat("\n2019 residuals:\n")
print(summary(dt_daly$check_2019))

# ----------------------------------------------------------------------------
# A.3: Run DALY decomposition
# ----------------------------------------------------------------------------

cat("\nRunning DALY decomposition on", nrow(dt_daly), "rows...\n")

factor_names_daly <- c("population", "case_rate", "age_sex_frac", "daly_per_case")
dt_daly <- decompose(dt_daly, factor_names = factor_names_daly, start_year = 2010, end_year = 2019)

# Calculate DALY change
dt_daly[, delta_daly := daly_2019 - daly_2010]

cat("DALY decomposition complete.\n")

# ----------------------------------------------------------------------------
# A.4: Aggregate DALY decomposition results (TWO-STAGE)
# ----------------------------------------------------------------------------

# STAGE A: Sum cells → location WITHIN each draw
by_location_daly_draw <- dt_daly[, .(
  daly_pop_size_effect = sum(population_effect, na.rm = TRUE),
  daly_prevalence_rate_effect = sum(case_rate_effect, na.rm = TRUE),
  daly_case_composition_effect = sum(age_sex_frac_effect, na.rm = TRUE),
  daly_intensity_effect = sum(daly_per_case_effect, na.rm = TRUE),
  delta_daly = sum(delta_daly, na.rm = TRUE),
  daly_2010 = sum(daly_2010, na.rm = TRUE),
  daly_2019 = sum(daly_2019, na.rm = TRUE)
), by = .(draw, location_id, location_name)]

# Validation within each draw
by_location_daly_draw[, sum_effects := daly_pop_size_effect + daly_prevalence_rate_effect + 
                        daly_case_composition_effect + daly_intensity_effect]
by_location_daly_draw[, diff_check := delta_daly - sum_effects]

cat("DALY Stage A complete. Max residual:", max(abs(by_location_daly_draw$diff_check)), "\n")

# STAGE B: Summarize ACROSS draws
by_state_daly <- by_location_daly_draw[, .(
  daly_pop_size_effect = mean(daly_pop_size_effect, na.rm = TRUE),
  daly_pop_size_effect_lower = quantile(daly_pop_size_effect, 0.025, na.rm = TRUE),
  daly_pop_size_effect_upper = quantile(daly_pop_size_effect, 0.975, na.rm = TRUE),
  
  daly_prevalence_rate_effect = mean(daly_prevalence_rate_effect, na.rm = TRUE),
  daly_prevalence_rate_effect_lower = quantile(daly_prevalence_rate_effect, 0.025, na.rm = TRUE),
  daly_prevalence_rate_effect_upper = quantile(daly_prevalence_rate_effect, 0.975, na.rm = TRUE),
  
  daly_case_composition_effect = mean(daly_case_composition_effect, na.rm = TRUE),
  daly_case_composition_effect_lower = quantile(daly_case_composition_effect, 0.025, na.rm = TRUE),
  daly_case_composition_effect_upper = quantile(daly_case_composition_effect, 0.975, na.rm = TRUE),
  
  daly_intensity_effect = mean(daly_intensity_effect, na.rm = TRUE),
  daly_intensity_effect_lower = quantile(daly_intensity_effect, 0.025, na.rm = TRUE),
  daly_intensity_effect_upper = quantile(daly_intensity_effect, 0.975, na.rm = TRUE),
  
  delta_daly = mean(delta_daly, na.rm = TRUE),
  delta_daly_lower = quantile(delta_daly, 0.025, na.rm = TRUE),
  delta_daly_upper = quantile(delta_daly, 0.975, na.rm = TRUE),
  
  daly_2010 = mean(daly_2010, na.rm = TRUE),
  daly_2010_lower = quantile(daly_2010, 0.025, na.rm = TRUE),
  daly_2010_upper = quantile(daly_2010, 0.975, na.rm = TRUE),
  
  daly_2019 = mean(daly_2019, na.rm = TRUE),
  daly_2019_lower = quantile(daly_2019, 0.025, na.rm = TRUE),
  daly_2019_upper = quantile(daly_2019, 0.975, na.rm = TRUE)
  
), by = .(location_id, location_name)]

# Validation (on means)
by_state_daly[, sum_effects := daly_pop_size_effect + daly_prevalence_rate_effect + 
                daly_case_composition_effect + daly_intensity_effect]
by_state_daly[, diff_check := delta_daly - sum_effects]

# Extract national DALY
national_daly <- by_state_daly[location_name == "United States"]

cat("\n=========== NATIONAL DALY DECOMPOSITION RESULTS (2010-2019) ===========\n\n")
cat(sprintf("Total DALYs 2010:   %s  (95%% UI: %s - %s)\n", 
            format(round(national_daly$daly_2010), big.mark = ","),
            format(round(national_daly$daly_2010_lower), big.mark = ","),
            format(round(national_daly$daly_2010_upper), big.mark = ",")))
cat(sprintf("Total DALYs 2019:   %s  (95%% UI: %s - %s)\n", 
            format(round(national_daly$daly_2019), big.mark = ","),
            format(round(national_daly$daly_2019_lower), big.mark = ","),
            format(round(national_daly$daly_2019_upper), big.mark = ",")))
cat(sprintf("Total DALY Change:  %s  (95%% UI: %s - %s)\n\n", 
            format(round(national_daly$delta_daly), big.mark = ","),
            format(round(national_daly$delta_daly_lower), big.mark = ","),
            format(round(national_daly$delta_daly_upper), big.mark = ",")))

cat("Factor Contributions (mean, 95% UI):\n")
cat(sprintf("  1. Population Size:      %15s  [%s - %s]\n", 
            format(round(national_daly$daly_pop_size_effect), big.mark = ","),
            format(round(national_daly$daly_pop_size_effect_lower), big.mark = ","),
            format(round(national_daly$daly_pop_size_effect_upper), big.mark = ",")))
cat(sprintf("  2. Prevalence Rate:      %15s  [%s - %s]\n", 
            format(round(national_daly$daly_prevalence_rate_effect), big.mark = ","),
            format(round(national_daly$daly_prevalence_rate_effect_lower), big.mark = ","),
            format(round(national_daly$daly_prevalence_rate_effect_upper), big.mark = ",")))
cat(sprintf("  3. Case Composition:     %15s  [%s - %s]\n", 
            format(round(national_daly$daly_case_composition_effect), big.mark = ","),
            format(round(national_daly$daly_case_composition_effect_lower), big.mark = ","),
            format(round(national_daly$daly_case_composition_effect_upper), big.mark = ",")))
cat(sprintf("  4. DALYs per Case:       %15s  [%s - %s]\n", 
            format(round(national_daly$daly_intensity_effect), big.mark = ","),
            format(round(national_daly$daly_intensity_effect_lower), big.mark = ","),
            format(round(national_daly$daly_intensity_effect_upper), big.mark = ",")))


###############################################################################
# PART B: SPENDING EFFECTIVENESS TABLE (WITH CROSSED DRAWS)
###############################################################################

cat("\n\n============ SPENDING EFFECTIVENESS ============\n")

# ----------------------------------------------------------------------------
# B.1: Get draw-level intensity effects for crossing
# ----------------------------------------------------------------------------

# Extract spending intensity by draw
spend_intensity_by_draw <- by_location_draw[, .(draw, location_id, location_name, spend_intensity_effect)]

# Extract DALY intensity by draw (and compute daly_averted = negative)
daly_intensity_by_draw <- by_location_daly_draw[, .(draw, location_id, location_name, 
                                                    daly_intensity_effect,
                                                    daly_averted_effect = -daly_intensity_effect)]

# ----------------------------------------------------------------------------
# B.2: Compute spending effectiveness with CROSSED DRAWS
# ----------------------------------------------------------------------------

# Function to compute crossed-draw statistics for one location
compute_crossed_stats <- function(spend_draws, daly_averted_draws) {
  # Cross all draws: 51 × 51 = 2,601 ratios
  crossed <- expand.grid(spend = spend_draws, daly_averted = daly_averted_draws)
  crossed$ratio <- crossed$spend / crossed$daly_averted
  
  # Handle infinite/NA values
  valid_ratios <- crossed$ratio[is.finite(crossed$ratio)]
  
  if (length(valid_ratios) == 0) {
    return(list(
      median = NA_real_,
      lower = NA_real_,
      upper = NA_real_,
      n_valid = 0
    ))
  }
  
  list(
    median = median(valid_ratios, na.rm = TRUE),
    lower = quantile(valid_ratios, 0.025, na.rm = TRUE),
    upper = quantile(valid_ratios, 0.975, na.rm = TRUE),
    n_valid = length(valid_ratios)
  )
}

# Function to compute category percentages from matched draws
compute_category_pcts <- function(spend_draws, daly_averted_draws) {
  n_draws <- length(spend_draws)
  
  # Determine category for each matched draw
  categories <- rep(NA_integer_, n_draws)
  for (i in seq_len(n_draws)) {
    s <- spend_draws[i]
    d <- daly_averted_draws[i]
    if (s > 0 && d > 0) categories[i] <- 1L
    else if (s < 0 && d > 0) categories[i] <- 2L
    else if (s > 0 && d < 0) categories[i] <- 3L
    else if (s < 0 && d < 0) categories[i] <- 4L
  }
  
  # Count percentages
  pct_1 <- 100 * sum(categories == 1L, na.rm = TRUE) / n_draws
  pct_2 <- 100 * sum(categories == 2L, na.rm = TRUE) / n_draws
  pct_3 <- 100 * sum(categories == 3L, na.rm = TRUE) / n_draws
  pct_4 <- 100 * sum(categories == 4L, na.rm = TRUE) / n_draws
  
  list(pct_1 = pct_1, pct_2 = pct_2, pct_3 = pct_3, pct_4 = pct_4)
}

# Get unique locations
locations <- unique(spend_intensity_by_draw$location_id)

cat("Computing spending effectiveness for", length(locations), "locations using crossed draws...\n")

# Compute for each location
spend_eff_results <- lapply(locations, function(loc_id) {
  loc_name <- spend_intensity_by_draw[location_id == loc_id, unique(location_name)]
  
  # Get draws for this location
  spend_draws <- spend_intensity_by_draw[location_id == loc_id, spend_intensity_effect]
  daly_averted_draws <- daly_intensity_by_draw[location_id == loc_id, daly_averted_effect]
  
  # Crossed-draw statistics for ratio
  ratio_stats <- compute_crossed_stats(spend_draws, daly_averted_draws)
  
  # Category percentages from matched draws
  cat_pcts <- compute_category_pcts(spend_draws, daly_averted_draws)
  
  # Mean effects (for point estimate category)
  mean_spend_intensity <- mean(spend_draws, na.rm = TRUE)
  mean_daly_averted <- mean(daly_averted_draws, na.rm = TRUE)
  
  # Determine point estimate category
  category <- NA_integer_
  if (mean_spend_intensity > 0 && mean_daly_averted > 0) category <- 1L
  else if (mean_spend_intensity < 0 && mean_daly_averted > 0) category <- 2L
  else if (mean_spend_intensity > 0 && mean_daly_averted < 0) category <- 3L
  else if (mean_spend_intensity < 0 && mean_daly_averted < 0) category <- 4L
  
  data.table(
    location_id = loc_id,
    location_name = loc_name,
    spend_intensity_effect = mean_spend_intensity,
    daly_averted_effect = mean_daly_averted,
    spend_effectiveness_median = ratio_stats$median,
    spend_effectiveness_lower = ratio_stats$lower,
    spend_effectiveness_upper = ratio_stats$upper,
    n_valid_ratios = ratio_stats$n_valid,
    category = category,
    category_pct_1 = cat_pcts$pct_1,
    category_pct_2 = cat_pcts$pct_2,
    category_pct_3 = cat_pcts$pct_3,
    category_pct_4 = cat_pcts$pct_4
  )
})

spend_eff_table <- rbindlist(spend_eff_results)

cat("Crossed-draw computation complete.\n")

# ----------------------------------------------------------------------------
# B.3: Add category labels and uncertainty indicators
# ----------------------------------------------------------------------------

spend_eff_table[, category_label := fcase(
  category == 1L, "Category 1: +Spend, +Health",
  category == 2L, "Category 2: Cost-saving",
  category == 3L, "Category 3: Dominated",
  category == 4L, "Category 4: -Both",
  default = "N/A"
)]

# Is category stable (all draws in same category)?
spend_eff_table[, category_stable := fcase(
  category_pct_1 == 100, TRUE,
  category_pct_2 == 100, TRUE,
  category_pct_3 == 100, TRUE,
  category_pct_4 == 100, TRUE,
  default = FALSE
)]

# Create uncertainty text
spend_eff_table[, category_uncertainty := fifelse(
  category_stable,
  NA_character_,
  paste0(
    fifelse(category_pct_1 > 0, paste0(round(category_pct_1), "% Cat 1"), ""),
    fifelse(category_pct_1 > 0 & (category_pct_2 > 0 | category_pct_3 > 0 | category_pct_4 > 0), ", ", ""),
    fifelse(category_pct_2 > 0, paste0(round(category_pct_2), "% Cat 2"), ""),
    fifelse(category_pct_2 > 0 & (category_pct_3 > 0 | category_pct_4 > 0), ", ", ""),
    fifelse(category_pct_3 > 0, paste0(round(category_pct_3), "% Cat 3"), ""),
    fifelse(category_pct_3 > 0 & category_pct_4 > 0, ", ", ""),
    fifelse(category_pct_4 > 0, paste0(round(category_pct_4), "% Cat 4"), "")
  )
)]

# Interpretation column
spend_eff_table[, interpretation := fcase(
  category == 1L, paste0("$", format(round(spend_effectiveness_median), big.mark = ","), " per DALY averted"),
  category == 2L, "Cost-saving (less spending, better health)",
  category == 3L, "Dominated (more spending, worse health)",
  category == 4L, "Excluded from ratio calculation",
  default = "N/A"
)]

# ----------------------------------------------------------------------------
# B.4: Merge with other data for final table
# ----------------------------------------------------------------------------

# Get additional columns from by_state and by_state_daly
final_spend_eff <- merge(
  spend_eff_table,
  by_state[, .(location_id, location_name, 
               spend_2010, spend_2010_lower, spend_2010_upper,
               spend_2019, spend_2019_lower, spend_2019_upper,
               delta_spend, delta_spend_lower, delta_spend_upper,
               spend_intensity_effect_lower, spend_intensity_effect_upper,
               prev_2010, prev_2019)],
  by = c("location_id", "location_name")
)

final_spend_eff <- merge(
  final_spend_eff,
  by_state_daly[, .(location_id, location_name,
                    daly_2010, daly_2010_lower, daly_2010_upper,
                    daly_2019, daly_2019_lower, daly_2019_upper,
                    delta_daly, delta_daly_lower, delta_daly_upper,
                    daly_intensity_effect, daly_intensity_effect_lower, daly_intensity_effect_upper)],
  by = c("location_id", "location_name")
)

# Compute daly_averted effect UI
final_spend_eff[, daly_averted_effect_lower := -daly_intensity_effect_upper]
final_spend_eff[, daly_averted_effect_upper := -daly_intensity_effect_lower]

# Compute per-case metrics
final_spend_eff[, spend_per_case_2010 := spend_2010 / prev_2010]
final_spend_eff[, spend_per_case_2019 := spend_2019 / prev_2019]
final_spend_eff[, daly_per_case_2010 := daly_2010 / prev_2010]
final_spend_eff[, daly_per_case_2019 := daly_2019 / prev_2019]
final_spend_eff[, change_spend_per_case := spend_per_case_2019 - spend_per_case_2010]
final_spend_eff[, change_daly_averted_per_case := daly_per_case_2010 - daly_per_case_2019]

# Simple spending effectiveness (for comparison)
final_spend_eff[, spend_effectiveness_simple := fifelse(
  change_daly_averted_per_case != 0,
  change_spend_per_case / change_daly_averted_per_case,
  NA_real_
)]

# Reorder columns for clarity
setcolorder(final_spend_eff, c(
  "location_id", "location_name",
  "spend_2010", "spend_2010_lower", "spend_2010_upper",
  "spend_2019", "spend_2019_lower", "spend_2019_upper",
  "delta_spend", "delta_spend_lower", "delta_spend_upper",
  "daly_2010", "daly_2010_lower", "daly_2010_upper",
  "daly_2019", "daly_2019_lower", "daly_2019_upper",
  "delta_daly", "delta_daly_lower", "delta_daly_upper",
  "prev_2010", "prev_2019",
  "spend_per_case_2010", "spend_per_case_2019", "change_spend_per_case",
  "daly_per_case_2010", "daly_per_case_2019", "change_daly_averted_per_case",
  "spend_intensity_effect", "spend_intensity_effect_lower", "spend_intensity_effect_upper",
  "daly_averted_effect", "daly_averted_effect_lower", "daly_averted_effect_upper",
  "spend_effectiveness_simple",
  "spend_effectiveness_median", "spend_effectiveness_lower", "spend_effectiveness_upper",
  "n_valid_ratios",
  "category", "category_label",
  "category_pct_1", "category_pct_2", "category_pct_3", "category_pct_4",
  "category_stable", "category_uncertainty",
  "interpretation"
))

# Order: United States first, then states alphabetically
final_spend_eff[, sort_order := fifelse(location_name == "United States", 0, 1)]
final_spend_eff <- final_spend_eff[order(sort_order, location_name)]
final_spend_eff[, sort_order := NULL]

# ----------------------------------------------------------------------------
# B.5: Print summary
# ----------------------------------------------------------------------------

national_se <- final_spend_eff[location_name == "United States"]

cat("\n=========== SPENDING EFFECTIVENESS SUMMARY ===========\n\n")

cat("NATIONAL (United States):\n")
cat(sprintf("  Spend Intensity Effect:        $%s  (95%% UI: $%s - $%s)\n", 
            format(round(national_se$spend_intensity_effect), big.mark = ","),
            format(round(national_se$spend_intensity_effect_lower), big.mark = ","),
            format(round(national_se$spend_intensity_effect_upper), big.mark = ",")))
cat(sprintf("  DALY Averted Effect:           %s DALYs  (95%% UI: %s - %s)\n", 
            format(round(national_se$daly_averted_effect), big.mark = ","),
            format(round(national_se$daly_averted_effect_lower), big.mark = ","),
            format(round(national_se$daly_averted_effect_upper), big.mark = ",")))
cat(sprintf("  Spending Effectiveness:        $%s per DALY averted  (95%% UI: $%s - $%s)\n",
            format(round(national_se$spend_effectiveness_median), big.mark = ","),
            format(round(national_se$spend_effectiveness_lower), big.mark = ","),
            format(round(national_se$spend_effectiveness_upper), big.mark = ",")))
cat(sprintf("  Category:                      %s\n", national_se$category_label))
cat(sprintf("  Category Stable:               %s\n", national_se$category_stable))
if (!national_se$category_stable) {
  cat(sprintf("  Category Uncertainty:          %s\n", national_se$category_uncertainty))
}

cat("\nSTATE-LEVEL CATEGORY DISTRIBUTION:\n")
print(table(final_spend_eff[location_name != "United States", category_label]))

cat("\nSTATES WITH CATEGORY UNCERTAINTY (category_stable = FALSE):\n")
uncertain_states <- final_spend_eff[category_stable == FALSE & location_name != "United States", 
                                    .(location_name, category_label, category_uncertainty)]
if (nrow(uncertain_states) > 0) {
  print(uncertain_states)
} else {
  cat("None - all states have stable category assignments.\n")
}

cat("\nTOP 10 STATES BY SPENDING EFFECTIVENESS (Category 1 only):\n")
cat1_states <- final_spend_eff[category == 1 & location_name != "United States"][
  order(spend_effectiveness_median)][1:min(10, .N)]
if (nrow(cat1_states) > 0) {
  print(cat1_states[, .(location_name, spend_effectiveness_median, 
                        spend_effectiveness_lower, spend_effectiveness_upper, interpretation)])
} else {
  cat("No Category 1 states.\n")
}

# ----------------------------------------------------------------------------
# B.6: Save results
# ----------------------------------------------------------------------------

write.csv(final_spend_eff, file.path(dir_output, "T3_OUD_spending_effectiveness.csv"), row.names = FALSE)
write.csv(by_state_daly, file.path(dir_output, "T8_OUD_decomp_daly.csv"), row.names = FALSE)

cat(sprintf("\n=========== FILES SAVED TO: %s ===========\n", dir_output))
cat("  - T3_OUD_spending_effectiveness.csv (national + all states with UI)\n")
cat("  - T6_OUD_decomp_by_state.csv (spending decomposition with UI)\n")
cat("  - T8_OUD_decomp_daly.csv (DALY decomposition with UI)\n")


###############################################################################
# STEP 7: VISUALIZATIONS
###############################################################################

cat("\n\n============ CREATING VISUALIZATIONS ============\n")

# ----------------------------------------------------------------------------
# 7.1: Prepare data for plotting (PERCENT SPACE)
# ----------------------------------------------------------------------------

# Use by_state directly (includes "United States")
# Reshape to long format for stacked bar
plot_data <- by_state %>%
  select(location_id, location_name, delta_spend, spend_2010,
         pop_size_effect, prevalence_rate_effect, 
         case_composition_effect, spend_intensity_effect) %>%
  pivot_longer(
    cols = c(pop_size_effect, prevalence_rate_effect, 
             case_composition_effect, spend_intensity_effect),
    names_to = "factor",
    values_to = "effect"
  ) %>%
  mutate(
    # Convert to percent of 2010 baseline
    effect_pct = 100 * effect / spend_2010,
    delta_pct  = 100 * delta_spend / spend_2010,
    # Clean factor names for legend
    factor = case_when(
      factor == "pop_size_effect" ~ "Population Size",
      factor == "prevalence_rate_effect" ~ "Prevalence Rate",
      factor == "case_composition_effect" ~ "Case Composition (Age-Sex)",
      factor == "spend_intensity_effect" ~ "Spending Intensity"
    ),
    # Order factors for stacking
    factor = factor(factor, levels = c("Population Size", 
                                       "Prevalence Rate",
                                       "Case Composition (Age-Sex)", 
                                       "Spending Intensity"))
  )

# ----------------------------------------------------------------------------
# 7.2: State-Level Stacked Horizontal Bar Chart (ALL LOCATIONS)
#      United States as first row
# ----------------------------------------------------------------------------

# Create state ordering: United States first, then states by delta_spend
state_order <- by_state %>%
  arrange(delta_spend) %>%
  pull(location_name)

# Move "United States" to the end (so it appears at top after coord_flip)
state_order <- c(setdiff(state_order, "United States"), "United States")

plot_data_all <- plot_data %>%
  mutate(location_name = factor(location_name, levels = state_order))

diamond_data_all <- by_state %>%
  mutate(
    delta_pct = 100 * delta_spend / spend_2010,
    location_name = factor(location_name, levels = state_order)
  )

p_states_all <- ggplot(plot_data_all, aes(x = location_name, y = effect_pct, fill = factor)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", alpha = 0.5) +
  geom_point(data = diamond_data_all,
             aes(x = location_name, y = delta_pct),
             inherit.aes = FALSE, shape = 18, size = 2.5, color = "black") +
  coord_flip() +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(
    title = "OUD Spending Decomposition (2010-2019)",
    subtitle = "United States and all states ordered by total spending change | Diamond = Total % Change from 2010 Baseline",
    x = "",
    y = "Effect as % of 2010 Spending",
    fill = "Factor"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10),
    axis.text.y = element_text(size = 7),
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    panel.grid.major = element_line(color = "grey80", linewidth = 0.3),
    panel.grid.minor = element_line(color = "grey90", linewidth = 0.2)
  ) +
  guides(fill = guide_legend(nrow = 1))

print(p_states_all)
ggsave(file.path(dir_output, "F4_OUD_decomp_all_states_stacked_bar.png"), p_states_all, 
       width = 10, height = 15, dpi = 300)

# ----------------------------------------------------------------------------
# 7.3: National Bar Chart (using "United States" data directly)
# ----------------------------------------------------------------------------

national_plot_data <- by_state %>%
  filter(location_name == "United States") %>%
  select(pop_size_effect, prevalence_rate_effect, 
         case_composition_effect, spend_intensity_effect, spend_2010) %>%
  pivot_longer(
    cols = c(pop_size_effect, prevalence_rate_effect, 
             case_composition_effect, spend_intensity_effect),
    names_to = "factor",
    values_to = "effect"
  ) %>%
  mutate(
    effect_pct = 100 * effect / spend_2010,
    factor = case_when(
      factor == "pop_size_effect" ~ "Population Size",
      factor == "prevalence_rate_effect" ~ "Prevalence Rate",
      factor == "case_composition_effect" ~ "Case Composition (Age-Sex)",
      factor == "spend_intensity_effect" ~ "Spending Intensity"
    ),
    factor = factor(factor, levels = c("Population Size", 
                                       "Prevalence Rate",
                                       "Case Composition (Age-Sex)", 
                                       "Spending Intensity")),
    location_name = "United States"
  )

national_delta_pct <- 100 * national$delta_spend / national$spend_2010

p_national <- ggplot(national_plot_data, aes(x = location_name, y = effect_pct, fill = factor)) +
  geom_bar(stat = "identity", position = "stack", width = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", alpha = 0.5) +
  geom_point(aes(x = "United States", y = national_delta_pct),
             inherit.aes = FALSE, shape = 18, size = 4, color = "black") +
  coord_flip() +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(
    title = "National OUD Spending Decomposition (2010-2019)",
    subtitle = paste0("Total spending change: ", 
                      format(round(national_delta_pct, 1), nsmall = 1), 
                      "% of 2010 baseline | Diamond = Total % Change"),
    x = "",
    y = "Effect as % of 2010 Spending",
    fill = "Factor"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10),
    axis.text.y = element_text(size = 12, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    panel.grid.major = element_line(color = "grey80", linewidth = 0.3),
    panel.grid.minor = element_line(color = "grey90", linewidth = 0.2)
  ) +
  guides(fill = guide_legend(nrow = 1))

print(p_national)
ggsave(file.path(dir_output, "F5_OUD_decomp_national_bar.png"), p_national, 
       width = 10, height = 4, dpi = 300)

# ----------------------------------------------------------------------------
# 7.4: State-Level Stacked Horizontal Bar Chart (Top 15 States)
# ----------------------------------------------------------------------------

top_states <- by_state %>%
  filter(location_name != "United States") %>%
  arrange(desc(abs(delta_spend))) %>%
  head(15) %>%
  pull(location_name)

plot_data_top <- plot_data %>%
  filter(location_name %in% top_states) %>%
  mutate(
    location_name = factor(location_name, 
                           levels = by_state %>% 
                             filter(location_name %in% top_states) %>%
                             arrange(delta_spend) %>% 
                             pull(location_name))
  )

diamond_data_top <- by_state %>%
  filter(location_name %in% top_states) %>%
  mutate(
    delta_pct = 100 * delta_spend / spend_2010,
    location_name = factor(location_name, 
                           levels = by_state %>% 
                             filter(location_name %in% top_states) %>%
                             arrange(delta_spend) %>% 
                             pull(location_name))
  )

p_states <- ggplot(plot_data_top, aes(x = location_name, y = effect_pct, fill = factor)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", alpha = 0.5) +
  geom_point(data = diamond_data_top,
             aes(x = location_name, y = delta_pct),
             inherit.aes = FALSE, shape = 18, size = 3, color = "black") +
  coord_flip() +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(
    title = "OUD Spending Decomposition by State (2010-2019)",
    subtitle = "Top 15 states by absolute spending change | Diamond = Total % Change from 2010 Baseline",
    x = "",
    y = "Effect as % of 2010 Spending",
    fill = "Factor"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    panel.grid.major = element_line(color = "grey80", linewidth = 0.3),
    panel.grid.minor = element_line(color = "grey90", linewidth = 0.2)
  ) +
  guides(fill = guide_legend(nrow = 2))

print(p_states)
ggsave(file.path(dir_output, "F6_OUD_decomp_15_states_stacked_bar.png"), p_states, 
       width = 10, height = 8, dpi = 300)

# ----------------------------------------------------------------------------
# 7.5: DALY Decomposition - All Locations (Percent change since 2010)
# ----------------------------------------------------------------------------

plot_data_daly <- by_state_daly %>%
  mutate(
    delta_pct_from2010 = if_else(daly_2010 > 0, 100 * delta_daly / daly_2010, NA_real_),
    daly_pop_size_effect_pct = if_else(daly_2010 > 0, 100 * daly_pop_size_effect / daly_2010, NA_real_),
    daly_prevalence_rate_effect_pct = if_else(daly_2010 > 0, 100 * daly_prevalence_rate_effect / daly_2010, NA_real_),
    daly_case_composition_effect_pct = if_else(daly_2010 > 0, 100 * daly_case_composition_effect / daly_2010, NA_real_),
    daly_intensity_effect_pct = if_else(daly_2010 > 0, 100 * daly_intensity_effect / daly_2010, NA_real_)
  ) %>%
  select(
    location_id, location_name, delta_pct_from2010,
    daly_pop_size_effect_pct, daly_prevalence_rate_effect_pct,
    daly_case_composition_effect_pct, daly_intensity_effect_pct
  ) %>%
  pivot_longer(
    cols = c(daly_pop_size_effect_pct, daly_prevalence_rate_effect_pct,
             daly_case_composition_effect_pct, daly_intensity_effect_pct),
    names_to = "factor",
    values_to = "effect_pct"
  ) %>%
  mutate(
    factor = case_when(
      factor == "daly_pop_size_effect_pct" ~ "Population Size",
      factor == "daly_prevalence_rate_effect_pct" ~ "Prevalence Rate",
      factor == "daly_case_composition_effect_pct" ~ "Case Composition (Age-Sex)",
      factor == "daly_intensity_effect_pct" ~ "DALY Intensity"
    ),
    factor = factor(factor, levels = c("Population Size", "Prevalence Rate",
                                       "Case Composition (Age-Sex)", "DALY Intensity"))
  )

# Order: United States at top, then by delta_daly
daly_state_order <- by_state_daly %>%
  arrange(delta_daly) %>%
  pull(location_name)
daly_state_order <- c(setdiff(daly_state_order, "United States"), "United States")

plot_data_daly_all <- plot_data_daly %>%
  mutate(location_name = factor(location_name, levels = daly_state_order))

p_daly_states_all <- ggplot(plot_data_daly_all, aes(x = location_name, y = effect_pct, fill = factor)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", alpha = 0.5) +
  geom_point(
    data = plot_data_daly_all %>% distinct(location_name, delta_pct_from2010),
    aes(x = location_name, y = delta_pct_from2010),
    inherit.aes = FALSE, shape = 18, size = 2.5, color = "black"
  ) +
  coord_flip() +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(
    title = "OUD DALY Decomposition (2010-2019)",
    subtitle = "United States and all states | Diamond = Total % Change from 2010 Baseline",
    x = "",
    y = "Effect as % of 2010 DALYs",
    fill = "Factor"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10),
    axis.text.y = element_text(size = 7),
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    panel.grid.major = element_line(color = "grey80", linewidth = 0.3),
    panel.grid.minor = element_line(color = "grey90", linewidth = 0.2)
  ) +
  guides(fill = guide_legend(nrow = 1))

print(p_daly_states_all)
ggsave(file.path(dir_output, "F7_OUD_daly_decomp_all_states_stacked_bar.png"), p_daly_states_all, 
       width = 10, height = 15, dpi = 300)

# ----------------------------------------------------------------------------
# 7.6: Two-panel figure: Panel A Spending + Panel B DALY decomposition
#      Alphabetical state order (A->Z top-down), United States at top
# ----------------------------------------------------------------------------

# State order: United States at end (top after flip), then A->Z from top
state_levels_alpha <- c(rev(sort(unique(by_state$location_name[by_state$location_name != "United States"]))), 
                        "United States")

# Build spending plot data
plot_spend <- by_state %>%
  mutate(
    delta_pct_from2010 = if_else(spend_2010 > 0, 100 * delta_spend / spend_2010, NA_real_),
    pop_size_effect_pct = if_else(spend_2010 > 0, 100 * pop_size_effect / spend_2010, NA_real_),
    prevalence_rate_effect_pct = if_else(spend_2010 > 0, 100 * prevalence_rate_effect / spend_2010, NA_real_),
    case_composition_effect_pct = if_else(spend_2010 > 0, 100 * case_composition_effect / spend_2010, NA_real_),
    spend_intensity_effect_pct = if_else(spend_2010 > 0, 100 * spend_intensity_effect / spend_2010, NA_real_)
  ) %>%
  select(location_name, delta_pct_from2010, pop_size_effect_pct, prevalence_rate_effect_pct,
         case_composition_effect_pct, spend_intensity_effect_pct) %>%
  pivot_longer(
    cols = c(pop_size_effect_pct, prevalence_rate_effect_pct,
             case_composition_effect_pct, spend_intensity_effect_pct),
    names_to = "factor", values_to = "effect_pct"
  ) %>%
  mutate(
    factor = case_when(
      factor == "pop_size_effect_pct" ~ "Population Size",
      factor == "prevalence_rate_effect_pct" ~ "Prevalence Rate",
      factor == "case_composition_effect_pct" ~ "Case Composition (Age-Sex)",
      factor == "spend_intensity_effect_pct" ~ "Spending Intensity"
    ),
    factor = factor(factor, levels = c("Population Size", "Prevalence Rate", 
                                       "Case Composition (Age-Sex)", "Spending Intensity")),
    location_name = factor(location_name, levels = state_levels_alpha)
  )

# Build DALY plot data
plot_daly_panel <- by_state_daly %>%
  mutate(
    delta_pct_from2010 = if_else(daly_2010 > 0, 100 * delta_daly / daly_2010, NA_real_),
    pop_size_effect_pct = if_else(daly_2010 > 0, 100 * daly_pop_size_effect / daly_2010, NA_real_),
    prevalence_rate_effect_pct = if_else(daly_2010 > 0, 100 * daly_prevalence_rate_effect / daly_2010, NA_real_),
    case_composition_effect_pct = if_else(daly_2010 > 0, 100 * daly_case_composition_effect / daly_2010, NA_real_),
    daly_intensity_effect_pct = if_else(daly_2010 > 0, 100 * daly_intensity_effect / daly_2010, NA_real_)
  ) %>%
  select(location_name, delta_pct_from2010, pop_size_effect_pct, prevalence_rate_effect_pct,
         case_composition_effect_pct, daly_intensity_effect_pct) %>%
  pivot_longer(
    cols = c(pop_size_effect_pct, prevalence_rate_effect_pct,
             case_composition_effect_pct, daly_intensity_effect_pct),
    names_to = "factor", values_to = "effect_pct"
  ) %>%
  mutate(
    factor = case_when(
      factor == "pop_size_effect_pct" ~ "Population Size",
      factor == "prevalence_rate_effect_pct" ~ "Prevalence Rate",
      factor == "case_composition_effect_pct" ~ "Case Composition (Age-Sex)",
      factor == "daly_intensity_effect_pct" ~ "DALY Intensity"
    ),
    factor = factor(factor, levels = c("Population Size", "Prevalence Rate", 
                                       "Case Composition (Age-Sex)", "DALY Intensity")),
    location_name = factor(location_name, levels = state_levels_alpha)
  )

# Panel-specific color mappings
factor_colors_spend <- c(
  "Population Size" = "#66C2A5",
  "Prevalence Rate" = "#FC8D62",
  "Case Composition (Age-Sex)" = "#8DA0CB",
  "Spending Intensity" = "#E78AC3"
)

factor_colors_daly <- c(
  "Population Size" = "#66C2A5",
  "Prevalence Rate" = "#FC8D62",
  "Case Composition (Age-Sex)" = "#8DA0CB",
  "DALY Intensity" = "#E78AC3"
)

# Panel A: Spending
pA <- ggplot(plot_spend, aes(x = location_name, y = effect_pct, fill = factor)) +
  geom_col(width = 0.72) +
  geom_point(
    data = plot_spend %>% distinct(location_name, delta_pct_from2010),
    aes(x = location_name, y = delta_pct_from2010),
    inherit.aes = FALSE, shape = 18, size = 1.9, color = "black"
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.4) +
  coord_flip() +
  scale_fill_manual(values = factor_colors_spend) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(
    title = "Panel A. OUD Spending Decomposition (2010-2019)",
    subtitle = "Effects as % of 2010 spending baseline | Diamond = total % change",
    x = NULL, y = "Effect (% of 2010 Spending)", fill = "Spending factors"
  ) +
  theme_bw() +
  theme(
    axis.text.y = element_text(size = 6),
    plot.title = element_text(size = 11, face = "bold"),
    plot.subtitle = element_text(size = 8),
    legend.position = "bottom",
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8)
  )

# Panel B: DALY
pB <- ggplot(plot_daly_panel, aes(x = location_name, y = effect_pct, fill = factor)) +
  geom_col(width = 0.72) +
  geom_point(
    data = plot_daly_panel %>% distinct(location_name, delta_pct_from2010),
    aes(x = location_name, y = delta_pct_from2010),
    inherit.aes = FALSE, shape = 18, size = 1.9, color = "black"
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.4) +
  coord_flip() +
  scale_fill_manual(values = factor_colors_daly) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(
    title = "Panel B. OUD DALY Decomposition (2010-2019)",
    subtitle = "Effects as % of 2010 DALY baseline | Diamond = total % change",
    x = NULL, y = "Effect (% of 2010 DALYs)", fill = "DALY factors"
  ) +
  theme_bw() +
  theme(
    axis.text.y = element_text(size = 6),
    plot.title = element_text(size = 11, face = "bold"),
    plot.subtitle = element_text(size = 8),
    legend.position = "bottom",
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8)
  )

# Combine panels
p_combined <- ggpubr::ggarrange(pA, pB, ncol = 2, nrow = 1, common.legend = FALSE, align = "h")

print(p_combined)
ggsave(file.path(dir_output, "F8_OUD_spend_daly_decomp_two_panel.png"), p_combined, 
       width = 18, height = 15, dpi = 300)

cat("\n=========== VISUALIZATIONS SAVED ===========\n")
cat(sprintf("Files saved to: %s\n", dir_output))
cat("  - F4_OUD_decomp_all_states_stacked_bar.png (all locations incl. US)\n")
cat("  - F5_OUD_decomp_national_bar.png\n")
cat("  - F6_OUD_decomp_15_states_stacked_bar.png\n")
cat("  - F7_OUD_daly_decomp_all_states_stacked_bar.png\n")
cat("  - F8_OUD_spend_daly_decomp_two_panel.png\n")

cat("\n============ SCRIPT COMPLETE ============\n")