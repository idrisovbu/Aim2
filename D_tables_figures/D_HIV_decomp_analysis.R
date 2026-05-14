##----------------------------------------------------------------
##' Title: D_HIV_decomp_analysis.R
##' 
##' ##note for the future this code has Cat level 4 reconciliaiton approach that assumes not cat 4 exists. It works fine for HIV which 
##' ## is the case as no Cat 4, but see code for OUD that handless this better. 
##'
##' Purpose: Das Gupta 4-Factor Decomposition of HIV Spending (2010-2019)
##'          WITH UNCERTAINTY INTERVALS FROM DRAW-LEVEL DATA
##'          USING WEAVER ET AL. (2022) METHODOLOGY
##'
##' 4-factor identity (matches expenditure_decomp_with_cases.R):
##'   Spend_cell = P_t * (P_i / P_t) * (C_ik / P_i) * (Spend_ik / C_ik)
##'   Factors:
##'     1. Population size       (state total P_t)
##'     2. Age-sex pop structure (P_i / P_t)
##'     3. Case rate within age  (C_ik / P_i)
##'     4. Spending intensity    (Spend_ik / C_ik)
##'
##' Percent convention (matches output_decomposition.R):
##'   pct_effect = 100 * effect / start_year_value  (baseline, not delta)
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

# Filter to HIV only
df_decomp_p <- df_decomp_p %>%
  filter(cause_name == "HIV/AIDS")

cat("\nAfter pivot_wider and HIV filter:", nrow(df_decomp_p), "rows\n")
cat("Expected: ~52 locations × 46 age-sex × 51 draws =", 52 * 46 * 51, "\n")



###############################################################################
# Das Gupta 4-Factor Decomposition (WITH UNCERTAINTY)
#
# Identity (Weaver et al. 2022, expenditure_decomp_with_cases.R):
#   Spend_cell = P_t × (P_i / P_t) × (C_ik / P_i) × (Spend_ik / C_ik)
#
# Factors:
#   1. Population size       (state total P_t)
#   2. Age-sex pop structure (P_i / P_t)
#   3. Case rate within age  (C_ik / P_i)
#   4. Spending intensity    (Spend_ik / C_ik)
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
# STEP 2: Prepare data with Weaver 4-factor identity (BY DRAW)
# ============================================================================

dt <- as.data.table(df_decomp_p)

cat("\n============ CONSTRUCTING FACTORS (BY DRAW) ============\n")

# ---- State-level population totals (BY DRAW) ----
dt[, pop_state_2010 := sum(population_2010), by = .(draw, cause_id, location_id)]
dt[, pop_state_2019 := sum(population_2019), by = .(draw, cause_id, location_id)]

# ---- Preserve cell-level population BEFORE overwriting `population_*` ----
dt[, pop_cell_2010 := population_2010]
dt[, pop_cell_2019 := population_2019]

# ---- FACTOR 1: Population size (state total P_t) ----
dt[, population_2010 := pop_state_2010]
dt[, population_2019 := pop_state_2019]

# ---- FACTOR 2: Age-sex POPULATION structure (P_i / P_t) ----
dt[, age_sex_pop_prop_2010 := fifelse(pop_state_2010 > 0, pop_cell_2010 / pop_state_2010, 0)]
dt[, age_sex_pop_prop_2019 := fifelse(pop_state_2019 > 0, pop_cell_2019 / pop_state_2019, 0)]

# ---- FACTOR 3: Case rate WITHIN age-sex (C_ik / P_i) ----
dt[, case_rate_2010 := fifelse(pop_cell_2010 > 0, prevalence_counts_2010 / pop_cell_2010, 0)]
dt[, case_rate_2019 := fifelse(pop_cell_2019 > 0, prevalence_counts_2019 / pop_cell_2019, 0)]

# ---- FACTOR 4: Spending intensity (spend per case in this cell) ----
dt[, spend_per_case_2010 := fifelse(prevalence_counts_2010 > 0, spend_all_2010 / prevalence_counts_2010, 0)]
dt[, spend_per_case_2019 := fifelse(prevalence_counts_2019 > 0, spend_all_2019 / prevalence_counts_2019, 0)]

# Keep original spending for validation
dt[, spend_2010 := spend_all_2010]
dt[, spend_2019 := spend_all_2019]

# ============================================================================
# STEP 2.5: VALIDATION - Check that identity holds
# ============================================================================

dt[, spend_hat_2010 := population_2010 * age_sex_pop_prop_2010 * case_rate_2010 * spend_per_case_2010]
dt[, spend_hat_2019 := population_2019 * age_sex_pop_prop_2019 * case_rate_2019 * spend_per_case_2019]

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

factor_names <- c("population", "age_sex_pop_prop", "case_rate", "spend_per_case")
dt <- decompose(dt, factor_names = factor_names, start_year = 2010, end_year = 2019)

# Calculate spending change
dt[, delta_spend := spend_2019 - spend_2010]

cat("Decomposition complete.\n")

# ============================================================================
# STEP 4: Aggregate results (TWO-STAGE)
# ============================================================================

cat("\n============ AGGREGATING RESULTS ============\n")

# ----------------------------------------------------------------------------
# STAGE A: Sum cells -> location WITHIN each draw
# ----------------------------------------------------------------------------

by_location_draw <- dt[, .(
  pop_size_effect        = sum(population_effect,        na.rm = TRUE),
  age_structure_effect   = sum(age_sex_pop_prop_effect,  na.rm = TRUE),
  prev_rate_effect       = sum(case_rate_effect,         na.rm = TRUE),
  spend_intensity_effect = sum(spend_per_case_effect,    na.rm = TRUE),
  delta_spend            = sum(delta_spend,              na.rm = TRUE),
  spend_2010             = sum(spend_2010,               na.rm = TRUE),
  spend_2019             = sum(spend_2019,               na.rm = TRUE),
  prev_2010              = sum(prevalence_counts_2010,   na.rm = TRUE),
  prev_2019              = sum(prevalence_counts_2019,   na.rm = TRUE)
), by = .(draw, location_id, location_name)]

# Validation within each draw
by_location_draw[, sum_effects := pop_size_effect + age_structure_effect +
                   prev_rate_effect + spend_intensity_effect]
by_location_draw[, diff_check := delta_spend - sum_effects]

# Add per-case metrics at draw level (SPENDING)
by_location_draw[, spend_per_case_2010   := fifelse(prev_2010 > 0, spend_2010 / prev_2010, NA_real_)]
by_location_draw[, spend_per_case_2019   := fifelse(prev_2019 > 0, spend_2019 / prev_2019, NA_real_)]
by_location_draw[, change_spend_per_case := spend_per_case_2019 - spend_per_case_2010]

cat("Stage A complete: ", nrow(by_location_draw), "rows (",
    length(unique(by_location_draw$location_name)), "locations ×",
    length(unique(by_location_draw$draw)), "draws)\n")
cat("Max decomposition residual:", max(abs(by_location_draw$diff_check)), "\n")

# ----------------------------------------------------------------------------
# STAGE B: Summarize ACROSS draws (mean + 95% UI)
# ----------------------------------------------------------------------------

by_state <- by_location_draw[, .(
  # Factor effects: mean + UI (lower/upper immediately after point estimate)
  pop_size_effect         = mean(pop_size_effect,        na.rm = TRUE),
  pop_size_effect_lower   = quantile(pop_size_effect,   0.025, na.rm = TRUE),
  pop_size_effect_upper   = quantile(pop_size_effect,   0.975, na.rm = TRUE),
  
  age_structure_effect         = mean(age_structure_effect,      na.rm = TRUE),
  age_structure_effect_lower   = quantile(age_structure_effect, 0.025, na.rm = TRUE),
  age_structure_effect_upper   = quantile(age_structure_effect, 0.975, na.rm = TRUE),
  
  prev_rate_effect         = mean(prev_rate_effect,      na.rm = TRUE),
  prev_rate_effect_lower   = quantile(prev_rate_effect, 0.025, na.rm = TRUE),
  prev_rate_effect_upper   = quantile(prev_rate_effect, 0.975, na.rm = TRUE),
  
  spend_intensity_effect         = mean(spend_intensity_effect,      na.rm = TRUE),
  spend_intensity_effect_lower   = quantile(spend_intensity_effect, 0.025, na.rm = TRUE),
  spend_intensity_effect_upper   = quantile(spend_intensity_effect, 0.975, na.rm = TRUE),
  
  # Totals: mean + UI
  delta_spend         = mean(delta_spend,      na.rm = TRUE),
  delta_spend_lower   = quantile(delta_spend, 0.025, na.rm = TRUE),
  delta_spend_upper   = quantile(delta_spend, 0.975, na.rm = TRUE),
  
  spend_2010          = mean(spend_2010,       na.rm = TRUE),
  spend_2010_lower    = quantile(spend_2010,  0.025, na.rm = TRUE),
  spend_2010_upper    = quantile(spend_2010,  0.975, na.rm = TRUE),
  
  spend_2019          = mean(spend_2019,       na.rm = TRUE),
  spend_2019_lower    = quantile(spend_2019,  0.025, na.rm = TRUE),
  spend_2019_upper    = quantile(spend_2019,  0.975, na.rm = TRUE),
  
  prev_2010 = mean(prev_2010, na.rm = TRUE),
  prev_2019 = mean(prev_2019, na.rm = TRUE),
  
  # Spend per case with UI
  spend_per_case_2010          = mean(spend_per_case_2010,       na.rm = TRUE),
  spend_per_case_2010_lower    = quantile(spend_per_case_2010,  0.025, na.rm = TRUE),
  spend_per_case_2010_upper    = quantile(spend_per_case_2010,  0.975, na.rm = TRUE),
  
  spend_per_case_2019          = mean(spend_per_case_2019,       na.rm = TRUE),
  spend_per_case_2019_lower    = quantile(spend_per_case_2019,  0.025, na.rm = TRUE),
  spend_per_case_2019_upper    = quantile(spend_per_case_2019,  0.975, na.rm = TRUE),
  
  change_spend_per_case        = mean(change_spend_per_case,      na.rm = TRUE),
  change_spend_per_case_lower  = quantile(change_spend_per_case, 0.025, na.rm = TRUE),
  change_spend_per_case_upper  = quantile(change_spend_per_case, 0.975, na.rm = TRUE)
  
), by = .(location_id, location_name)]

# ---- Percent effects: BASELINE convention (matches Weaver output_decomposition.R)
# Each factor percent = 100 * effect / spend_2010 (the start-year value).
# The four factor percents sum to the total percent change (delta / baseline),
# NOT to 100%. pct_delta_spend reports that total for reference.
by_state[, `:=`(
  pct_pop_size        = 100 * pop_size_effect        / spend_2010,
  pct_age_structure   = 100 * age_structure_effect   / spend_2010,
  pct_prev_rate       = 100 * prev_rate_effect       / spend_2010,
  pct_spend_intensity = 100 * spend_intensity_effect / spend_2010,
  pct_delta_spend     = 100 * delta_spend            / spend_2010
)]

# Decomposition validation (on means)
by_state[, sum_effects := pop_size_effect + age_structure_effect +
           prev_rate_effect + spend_intensity_effect]
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
cat(sprintf("Total Spending Change: $%s  (95%% UI: $%s - $%s)  = %.1f%% of 2010 baseline\n\n",
            format(round(national$delta_spend), big.mark = ","),
            format(round(national$delta_spend_lower), big.mark = ","),
            format(round(national$delta_spend_upper), big.mark = ","),
            national$pct_delta_spend))

cat("Factor Contributions (mean, 95% UI, % of 2010 baseline):\n")
cat(sprintf("  1. Population Size:            $%15s  (%6.1f%%)  [%s - %s]\n",
            format(round(national$pop_size_effect), big.mark = ","), national$pct_pop_size,
            format(round(national$pop_size_effect_lower), big.mark = ","),
            format(round(national$pop_size_effect_upper), big.mark = ",")))
cat(sprintf("  2. Age Structure:              $%15s  (%6.1f%%)  [%s - %s]\n",
            format(round(national$age_structure_effect), big.mark = ","), national$pct_age_structure,
            format(round(national$age_structure_effect_lower), big.mark = ","),
            format(round(national$age_structure_effect_upper), big.mark = ",")))
cat(sprintf("  3. Prevalence Rate (within):   $%15s  (%6.1f%%)  [%s - %s]\n",
            format(round(national$prev_rate_effect), big.mark = ","), national$pct_prev_rate,
            format(round(national$prev_rate_effect_lower), big.mark = ","),
            format(round(national$prev_rate_effect_upper), big.mark = ",")))
cat(sprintf("  4. Spending Intensity:         $%15s  (%6.1f%%)  [%s - %s]\n",
            format(round(national$spend_intensity_effect), big.mark = ","), national$pct_spend_intensity,
            format(round(national$spend_intensity_effect_lower), big.mark = ","),
            format(round(national$spend_intensity_effect_upper), big.mark = ",")))

cat(sprintf("\nDecomposition Validation (sum - actual, should be ~0): %.6f\n", national$diff_check))

cat("\n=========== TOP 10 STATES BY SPENDING CHANGE ===========\n")
print(by_state[location_name != "United States"][order(-delta_spend)][1:10,
                                                                      .(location_name, delta_spend,
                                                                        pct_pop_size, pct_age_structure, pct_prev_rate, pct_spend_intensity,
                                                                        diff_check)])

# ============================================================================
# STEP 6: Save spending decomposition results
# ============================================================================

write.csv(by_state, file.path(dir_output, "T6_HIV_decomp_by_state.csv"), row.names = FALSE)

cat(sprintf("\nSpending decomposition saved to: %s\n", dir_output))


###############################################################################
# PART A: DALY DECOMPOSITION (parallel to spending decomposition)
###############################################################################

cat("\n\n============ DALY DECOMPOSITION ============\n")

# ----------------------------------------------------------------------------
# A.1: Prepare DALY data with same 4-factor identity (BY DRAW)
# ----------------------------------------------------------------------------

dt_daly <- as.data.table(df_decomp_p)

# ---- State-level population totals (BY DRAW) ----
dt_daly[, pop_state_2010 := sum(population_2010), by = .(draw, cause_id, location_id)]
dt_daly[, pop_state_2019 := sum(population_2019), by = .(draw, cause_id, location_id)]

# ---- Preserve cell-level population BEFORE overwriting `population_*` ----
dt_daly[, pop_cell_2010 := population_2010]
dt_daly[, pop_cell_2019 := population_2019]

# ---- FACTOR 1: Population size (state total) ----
dt_daly[, population_2010 := pop_state_2010]
dt_daly[, population_2019 := pop_state_2019]

# ---- FACTOR 2: Age-sex POPULATION structure (P_i / P_t) ----
dt_daly[, age_sex_pop_prop_2010 := fifelse(pop_state_2010 > 0, pop_cell_2010 / pop_state_2010, 0)]
dt_daly[, age_sex_pop_prop_2019 := fifelse(pop_state_2019 > 0, pop_cell_2019 / pop_state_2019, 0)]

# ---- FACTOR 3: Case rate WITHIN age-sex (C_ik / P_i) ----
dt_daly[, case_rate_2010 := fifelse(pop_cell_2010 > 0, prevalence_counts_2010 / pop_cell_2010, 0)]
dt_daly[, case_rate_2019 := fifelse(pop_cell_2019 > 0, prevalence_counts_2019 / pop_cell_2019, 0)]

# ---- FACTOR 4: DALYs per case ----
dt_daly[, daly_per_case_2010 := fifelse(prevalence_counts_2010 > 0, daly_counts_2010 / prevalence_counts_2010, 0)]
dt_daly[, daly_per_case_2019 := fifelse(prevalence_counts_2019 > 0, daly_counts_2019 / prevalence_counts_2019, 0)]

# Keep original DALYs for validation
dt_daly[, daly_2010 := daly_counts_2010]
dt_daly[, daly_2019 := daly_counts_2019]

# ----------------------------------------------------------------------------
# A.2: Validate DALY identity
# ----------------------------------------------------------------------------

dt_daly[, daly_hat_2010 := population_2010 * age_sex_pop_prop_2010 * case_rate_2010 * daly_per_case_2010]
dt_daly[, daly_hat_2019 := population_2019 * age_sex_pop_prop_2019 * case_rate_2019 * daly_per_case_2019]

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

factor_names_daly <- c("population", "age_sex_pop_prop", "case_rate", "daly_per_case")
dt_daly <- decompose(dt_daly, factor_names = factor_names_daly, start_year = 2010, end_year = 2019)

# Calculate DALY change
dt_daly[, delta_daly := daly_2019 - daly_2010]

cat("DALY decomposition complete.\n")

# ----------------------------------------------------------------------------
# A.4: Aggregate DALY decomposition results (TWO-STAGE)
# ----------------------------------------------------------------------------

# STAGE A: Sum cells -> location WITHIN each draw
by_location_daly_draw <- dt_daly[, .(
  daly_pop_size_effect       = sum(population_effect,        na.rm = TRUE),
  daly_age_structure_effect  = sum(age_sex_pop_prop_effect,  na.rm = TRUE),
  daly_prev_rate_effect      = sum(case_rate_effect,         na.rm = TRUE),
  daly_intensity_effect      = sum(daly_per_case_effect,     na.rm = TRUE),
  delta_daly                 = sum(delta_daly,               na.rm = TRUE),
  daly_2010                  = sum(daly_2010,                na.rm = TRUE),
  daly_2019                  = sum(daly_2019,                na.rm = TRUE),
  prev_2010                  = sum(prevalence_counts_2010,   na.rm = TRUE),
  prev_2019                  = sum(prevalence_counts_2019,   na.rm = TRUE)
), by = .(draw, location_id, location_name)]

# Validation within each draw
by_location_daly_draw[, sum_effects := daly_pop_size_effect + daly_age_structure_effect +
                        daly_prev_rate_effect + daly_intensity_effect]
by_location_daly_draw[, diff_check := delta_daly - sum_effects]

cat("DALY Stage A complete. Max residual:", max(abs(by_location_daly_draw$diff_check)), "\n")

# Add per-case metrics at draw level (DALY)
by_location_daly_draw[, daly_per_case_2010            := fifelse(prev_2010 > 0, daly_2010 / prev_2010, NA_real_)]
by_location_daly_draw[, daly_per_case_2019            := fifelse(prev_2019 > 0, daly_2019 / prev_2019, NA_real_)]
by_location_daly_draw[, change_daly_per_case          := daly_per_case_2019 - daly_per_case_2010]
by_location_daly_draw[, change_daly_averted_per_case  := daly_per_case_2010 - daly_per_case_2019]

# STAGE B: Summarize ACROSS draws
by_state_daly <- by_location_daly_draw[, .(
  daly_pop_size_effect         = mean(daly_pop_size_effect,       na.rm = TRUE),
  daly_pop_size_effect_lower   = quantile(daly_pop_size_effect,  0.025, na.rm = TRUE),
  daly_pop_size_effect_upper   = quantile(daly_pop_size_effect,  0.975, na.rm = TRUE),
  
  daly_age_structure_effect         = mean(daly_age_structure_effect,      na.rm = TRUE),
  daly_age_structure_effect_lower   = quantile(daly_age_structure_effect, 0.025, na.rm = TRUE),
  daly_age_structure_effect_upper   = quantile(daly_age_structure_effect, 0.975, na.rm = TRUE),
  
  daly_prev_rate_effect         = mean(daly_prev_rate_effect,      na.rm = TRUE),
  daly_prev_rate_effect_lower   = quantile(daly_prev_rate_effect, 0.025, na.rm = TRUE),
  daly_prev_rate_effect_upper   = quantile(daly_prev_rate_effect, 0.975, na.rm = TRUE),
  
  daly_intensity_effect         = mean(daly_intensity_effect,      na.rm = TRUE),
  daly_intensity_effect_lower   = quantile(daly_intensity_effect, 0.025, na.rm = TRUE),
  daly_intensity_effect_upper   = quantile(daly_intensity_effect, 0.975, na.rm = TRUE),
  
  delta_daly         = mean(delta_daly,      na.rm = TRUE),
  delta_daly_lower   = quantile(delta_daly, 0.025, na.rm = TRUE),
  delta_daly_upper   = quantile(delta_daly, 0.975, na.rm = TRUE),
  
  daly_2010          = mean(daly_2010,       na.rm = TRUE),
  daly_2010_lower    = quantile(daly_2010,  0.025, na.rm = TRUE),
  daly_2010_upper    = quantile(daly_2010,  0.975, na.rm = TRUE),
  
  daly_2019          = mean(daly_2019,       na.rm = TRUE),
  daly_2019_lower    = quantile(daly_2019,  0.025, na.rm = TRUE),
  daly_2019_upper    = quantile(daly_2019,  0.975, na.rm = TRUE),
  
  # DALY per case with UI
  daly_per_case_2010          = mean(daly_per_case_2010,       na.rm = TRUE),
  daly_per_case_2010_lower    = quantile(daly_per_case_2010,  0.025, na.rm = TRUE),
  daly_per_case_2010_upper    = quantile(daly_per_case_2010,  0.975, na.rm = TRUE),
  
  daly_per_case_2019          = mean(daly_per_case_2019,       na.rm = TRUE),
  daly_per_case_2019_lower    = quantile(daly_per_case_2019,  0.025, na.rm = TRUE),
  daly_per_case_2019_upper    = quantile(daly_per_case_2019,  0.975, na.rm = TRUE),
  
  change_daly_per_case        = mean(change_daly_per_case,      na.rm = TRUE),
  change_daly_per_case_lower  = quantile(change_daly_per_case, 0.025, na.rm = TRUE),
  change_daly_per_case_upper  = quantile(change_daly_per_case, 0.975, na.rm = TRUE),
  
  change_daly_averted_per_case        = mean(change_daly_averted_per_case,      na.rm = TRUE),
  change_daly_averted_per_case_lower  = quantile(change_daly_averted_per_case, 0.025, na.rm = TRUE),
  change_daly_averted_per_case_upper  = quantile(change_daly_averted_per_case, 0.975, na.rm = TRUE)
  
), by = .(location_id, location_name)]

# ---- Percent effects: BASELINE convention (% of 2010 DALYs)
by_state_daly[, `:=`(
  daly_pct_pop_size       = 100 * daly_pop_size_effect       / daly_2010,
  daly_pct_age_structure  = 100 * daly_age_structure_effect  / daly_2010,
  daly_pct_prev_rate      = 100 * daly_prev_rate_effect      / daly_2010,
  daly_pct_intensity      = 100 * daly_intensity_effect      / daly_2010,
  daly_pct_delta_daly     = 100 * delta_daly                 / daly_2010
)]

# Validation (on means)
by_state_daly[, sum_effects := daly_pop_size_effect + daly_age_structure_effect +
                daly_prev_rate_effect + daly_intensity_effect]
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
cat(sprintf("Total DALY Change:  %s  (95%% UI: %s - %s)  = %.1f%% of 2010 baseline\n\n",
            format(round(national_daly$delta_daly), big.mark = ","),
            format(round(national_daly$delta_daly_lower), big.mark = ","),
            format(round(national_daly$delta_daly_upper), big.mark = ","),
            national_daly$daly_pct_delta_daly))

cat("Factor Contributions (mean, 95% UI, % of 2010 baseline):\n")
cat(sprintf("  1. Population Size:            %15s  (%6.1f%%)  [%s - %s]\n",
            format(round(national_daly$daly_pop_size_effect), big.mark = ","), national_daly$daly_pct_pop_size,
            format(round(national_daly$daly_pop_size_effect_lower), big.mark = ","),
            format(round(national_daly$daly_pop_size_effect_upper), big.mark = ",")))
cat(sprintf("  2. Age Structure:              %15s  (%6.1f%%)  [%s - %s]\n",
            format(round(national_daly$daly_age_structure_effect), big.mark = ","), national_daly$daly_pct_age_structure,
            format(round(national_daly$daly_age_structure_effect_lower), big.mark = ","),
            format(round(national_daly$daly_age_structure_effect_upper), big.mark = ",")))
cat(sprintf("  3. Prevalence Rate (within):   %15s  (%6.1f%%)  [%s - %s]\n",
            format(round(national_daly$daly_prev_rate_effect), big.mark = ","), national_daly$daly_pct_prev_rate,
            format(round(national_daly$daly_prev_rate_effect_lower), big.mark = ","),
            format(round(national_daly$daly_prev_rate_effect_upper), big.mark = ",")))
cat(sprintf("  4. DALYs per Case:             %15s  (%6.1f%%)  [%s - %s]\n",
            format(round(national_daly$daly_intensity_effect), big.mark = ","), national_daly$daly_pct_intensity,
            format(round(national_daly$daly_intensity_effect_lower), big.mark = ","),
            format(round(national_daly$daly_intensity_effect_upper), big.mark = ",")))


###############################################################################
# PART B: SPENDING EFFECTIVENESS TABLE (WEAVER METHODOLOGY)
#
# Key features of Weaver et al. (2022) approach:
# 1. Cross all draws: 51 × 51 = 2,601 ratios (for independence)
# 2. Rank-based ordering: Category 2 < Category 1 ratios < Category 3
# 3. Exclude Category 4 from ranking
# 4. Report MEDIAN and IQR (25th-75th percentile), not mean/95% UI
# 5. IQR bounds can span categories (e.g., "Category 2 – $12,500")
###############################################################################

cat("\n\n============ SPENDING EFFECTIVENESS (WEAVER METHODOLOGY) ============\n")

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
# B.2: Weaver-style functions for spending effectiveness
# ----------------------------------------------------------------------------

#' Compute spending effectiveness using Weaver methodology
compute_weaver_stats <- function(spend_draws, daly_averted_draws) {
  
  crossed <- expand.grid(
    spend        = spend_draws,
    daly_averted = daly_averted_draws
  )
  
  crossed$category <- with(crossed, ifelse(
    spend > 0 & daly_averted > 0, 1L,
    ifelse(spend < 0 & daly_averted > 0, 2L,
           ifelse(spend > 0 & daly_averted < 0, 3L,
                  ifelse(spend < 0 & daly_averted < 0, 4L, NA_integer_)))))
  
  crossed$ratio <- ifelse(crossed$category == 1L,
                          crossed$spend / crossed$daly_averted,
                          NA_real_)
  
  n_cat1_all <- sum(crossed$category == 1L, na.rm = TRUE)
  n_cat2_all <- sum(crossed$category == 2L, na.rm = TRUE)
  n_cat3_all <- sum(crossed$category == 3L, na.rm = TRUE)
  n_cat4_all <- sum(crossed$category == 4L, na.rm = TRUE)
  n_total_all <- nrow(crossed)
  
  # Mean Cat-1 ratio across all crossed draws (for scatter vs. frontier efficiency)
  mean_cat1_ratio <- if (n_cat1_all > 0) {
    mean(crossed$ratio[crossed$category == 1L], na.rm = TRUE)
  } else {
    NA_real_
  }
  
  # Exclude Category 4 (per Weaver methodology)
  crossed <- crossed[!is.na(crossed$category) & crossed$category != 4L, ]
  
  n_total <- nrow(crossed)
  
  if (n_total == 0) {
    return(list(
      median_value = NA_real_, median_category = NA_integer_,
      q25_value = NA_real_, q25_category = NA_integer_,
      q75_value = NA_real_, q75_category = NA_integer_,
      n_cat1 = n_cat1_all, n_cat2 = n_cat2_all,
      n_cat3 = n_cat3_all, n_cat4 = n_cat4_all,
      n_total = n_total_all,
      pct_cat1 = 100 * n_cat1_all / n_total_all,
      pct_cat2 = 100 * n_cat2_all / n_total_all,
      pct_cat3 = 100 * n_cat3_all / n_total_all,
      pct_cat4 = 100 * n_cat4_all / n_total_all,
      mean_cat1_ratio = mean_cat1_ratio
    ))
  }
  
  # Rank ordering: Cat 2 = -Inf (most favorable), Cat 1 = ratio, Cat 3 = +Inf
  crossed$rank_value <- with(crossed, ifelse(
    category == 2L, -Inf,
    ifelse(category == 1L, ratio, Inf)))
  
  crossed <- crossed[order(crossed$rank_value), ]
  
  median_idx <- ceiling(n_total * 0.50)
  q25_idx    <- ceiling(n_total * 0.25)
  q75_idx    <- ceiling(n_total * 0.75)
  
  median_row <- crossed[median_idx, ]
  q25_row    <- crossed[q25_idx, ]
  q75_row    <- crossed[q75_idx, ]
  
  list(
    median_value    = if (median_row$category == 1L) median_row$ratio else NA_real_,
    median_category = median_row$category,
    q25_value       = if (q25_row$category == 1L) q25_row$ratio else NA_real_,
    q25_category    = q25_row$category,
    q75_value       = if (q75_row$category == 1L) q75_row$ratio else NA_real_,
    q75_category    = q75_row$category,
    n_cat1 = n_cat1_all,
    n_cat2 = n_cat2_all,
    n_cat3 = n_cat3_all,
    n_cat4 = n_cat4_all,
    n_total = n_total_all,
    pct_cat1 = 100 * n_cat1_all / n_total_all,
    pct_cat2 = 100 * n_cat2_all / n_total_all,
    pct_cat3 = 100 * n_cat3_all / n_total_all,
    pct_cat4 = 100 * n_cat4_all / n_total_all,
    mean_cat1_ratio = mean_cat1_ratio  
  )
}

format_weaver_value <- function(val, cat) {
  if (is.na(cat)) {
    return("N/A")
  } else if (cat == 1L && !is.na(val)) {
    return(paste0("$", format(round(val), big.mark = ",")))
  } else if (cat == 2L) {
    return("Category 2")
  } else if (cat == 3L) {
    return("Category 3")
  } else {
    return("N/A")
  }
}

format_weaver_interpretation <- function(median_val, median_cat, q25_val, q25_cat, q75_val, q75_cat) {
  median_str <- format_weaver_value(median_val, median_cat)
  q25_str    <- format_weaver_value(q25_val, q25_cat)
  q75_str    <- format_weaver_value(q75_val, q75_cat)
  paste0(median_str, " (", q25_str, " - ", q75_str, ")")
}

# ----------------------------------------------------------------------------
# B.3: Compute spending effectiveness for each location
# ----------------------------------------------------------------------------

locations <- unique(spend_intensity_by_draw$location_id)

cat("Computing spending effectiveness for", length(locations),
    "locations using Weaver methodology (crossed draws, rank-ordering, IQR)...\n")

spend_eff_results <- lapply(locations, function(loc_id) {
  loc_name <- spend_intensity_by_draw[location_id == loc_id, unique(location_name)]
  
  spend_draws        <- spend_intensity_by_draw[location_id == loc_id, spend_intensity_effect]
  daly_averted_draws <- daly_intensity_by_draw[location_id == loc_id, daly_averted_effect]
  
  ws <- compute_weaver_stats(spend_draws, daly_averted_draws)
  
  mean_spend_intensity <- mean(spend_draws,        na.rm = TRUE)
  mean_daly_averted    <- mean(daly_averted_draws, na.rm = TRUE)
  
  category <- NA_integer_
  if (mean_spend_intensity > 0 && mean_daly_averted > 0) category <- 1L
  else if (mean_spend_intensity < 0 && mean_daly_averted > 0) category <- 2L
  else if (mean_spend_intensity > 0 && mean_daly_averted < 0) category <- 3L
  else if (mean_spend_intensity < 0 && mean_daly_averted < 0) category <- 4L
  
  interpretation <- format_weaver_interpretation(
    ws$median_value, ws$median_category,
    ws$q25_value,    ws$q25_category,
    ws$q75_value,    ws$q75_category
  )
  
  data.table(
    location_id   = loc_id,
    location_name = loc_name,
    
    spend_intensity_effect = mean_spend_intensity,
    daly_averted_effect    = mean_daly_averted,
    
    spend_effectiveness_median           = ws$median_value,
    spend_effectiveness_median_category  = ws$median_category,
    spend_effectiveness_q25              = ws$q25_value,
    spend_effectiveness_q25_category     = ws$q25_category,
    spend_effectiveness_q75              = ws$q75_value,
    spend_effectiveness_q75_category     = ws$q75_category,
    
    n_cat1 = ws$n_cat1, n_cat2 = ws$n_cat2,
    n_cat3 = ws$n_cat3, n_cat4 = ws$n_cat4,
    n_total = ws$n_total,
    pct_cat1 = ws$pct_cat1, pct_cat2 = ws$pct_cat2,
    pct_cat3 = ws$pct_cat3, pct_cat4 = ws$pct_cat4,
    
    mean_cat1_ratio = ws$mean_cat1_ratio, 
    
    category = category,
    interpretation = interpretation
  )
})

spend_eff_table <- rbindlist(spend_eff_results)

cat("Weaver-style computation complete.\n")

# ----------------------------------------------------------------------------
# B.4: Add category labels and stability indicators
# ----------------------------------------------------------------------------

spend_eff_table[, category_label := fcase(
  category == 1L, "Category 1: +Spend, +Health",
  category == 2L, "Category 2: Cost-saving",
  category == 3L, "Category 3: Dominated",
  category == 4L, "Category 4: -Both",
  default = "N/A"
)]

spend_eff_table[, category_stable := (
  spend_effectiveness_median_category == spend_effectiveness_q25_category &
    spend_effectiveness_median_category == spend_effectiveness_q75_category
)]

spend_eff_table[, iqr_category_span := paste0(
  "q25: Cat ", spend_effectiveness_q25_category,
  ", median: Cat ", spend_effectiveness_median_category,
  ", q75: Cat ", spend_effectiveness_q75_category
)]

spend_eff_table[, category_uncertainty := fifelse(
  category_stable,
  NA_character_,
  paste0(
    fifelse(pct_cat1 > 0, paste0(round(pct_cat1), "% Cat 1"), ""),
    fifelse(pct_cat1 > 0 & (pct_cat2 > 0 | pct_cat3 > 0 | pct_cat4 > 0), ", ", ""),
    fifelse(pct_cat2 > 0, paste0(round(pct_cat2), "% Cat 2"), ""),
    fifelse(pct_cat2 > 0 & (pct_cat3 > 0 | pct_cat4 > 0), ", ", ""),
    fifelse(pct_cat3 > 0, paste0(round(pct_cat3), "% Cat 3"), ""),
    fifelse(pct_cat3 > 0 & pct_cat4 > 0, ", ", ""),
    fifelse(pct_cat4 > 0, paste0(round(pct_cat4), "% Cat 4"), "")
  )
)]

# ----------------------------------------------------------------------------
# B.5: Merge with other data for final table
# ----------------------------------------------------------------------------

final_spend_eff <- merge(
  spend_eff_table,
  by_state[, .(location_id, location_name,
               spend_2010, spend_2010_lower, spend_2010_upper,
               spend_2019, spend_2019_lower, spend_2019_upper,
               delta_spend, delta_spend_lower, delta_spend_upper,
               prev_2010, prev_2019,
               spend_per_case_2010, spend_per_case_2010_lower, spend_per_case_2010_upper,
               spend_per_case_2019, spend_per_case_2019_lower, spend_per_case_2019_upper,
               change_spend_per_case, change_spend_per_case_lower, change_spend_per_case_upper,
               spend_intensity_effect_lower, spend_intensity_effect_upper)],
  by = c("location_id", "location_name")
)

final_spend_eff <- merge(
  final_spend_eff,
  by_state_daly[, .(location_id, location_name,
                    daly_2010, daly_2010_lower, daly_2010_upper,
                    daly_2019, daly_2019_lower, daly_2019_upper,
                    delta_daly, delta_daly_lower, delta_daly_upper,
                    daly_per_case_2010, daly_per_case_2010_lower, daly_per_case_2010_upper,
                    daly_per_case_2019, daly_per_case_2019_lower, daly_per_case_2019_upper,
                    change_daly_per_case, change_daly_per_case_lower, change_daly_per_case_upper,
                    change_daly_averted_per_case, change_daly_averted_per_case_lower, change_daly_averted_per_case_upper,
                    daly_intensity_effect, daly_intensity_effect_lower, daly_intensity_effect_upper)],
  by = c("location_id", "location_name")
)

# Compute daly_averted effect UI (flip signs)
final_spend_eff[, daly_averted_effect_lower := -daly_intensity_effect_upper]
final_spend_eff[, daly_averted_effect_upper := -daly_intensity_effect_lower]

# Simple spending effectiveness (for comparison) - ratio of means
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
  
  "spend_per_case_2010", "spend_per_case_2010_lower", "spend_per_case_2010_upper",
  "spend_per_case_2019", "spend_per_case_2019_lower", "spend_per_case_2019_upper",
  "change_spend_per_case", "change_spend_per_case_lower", "change_spend_per_case_upper",
  
  "daly_per_case_2010", "daly_per_case_2010_lower", "daly_per_case_2010_upper",
  "daly_per_case_2019", "daly_per_case_2019_lower", "daly_per_case_2019_upper",
  "change_daly_per_case", "change_daly_per_case_lower", "change_daly_per_case_upper",
  "change_daly_averted_per_case", "change_daly_averted_per_case_lower", "change_daly_averted_per_case_upper",
  
  "spend_intensity_effect", "spend_intensity_effect_lower", "spend_intensity_effect_upper",
  "daly_averted_effect", "daly_averted_effect_lower", "daly_averted_effect_upper",
  
  "spend_effectiveness_simple",
  
  "spend_effectiveness_median", "spend_effectiveness_median_category",
  "spend_effectiveness_q25", "spend_effectiveness_q25_category",
  "spend_effectiveness_q75", "spend_effectiveness_q75_category",
  
  "n_cat1", "n_cat2", "n_cat3", "n_cat4", "n_total",
  "pct_cat1", "pct_cat2", "pct_cat3", "pct_cat4",
  
  "category", "category_label",
  "category_stable", "iqr_category_span", "category_uncertainty",
  
  "interpretation"
))

# Order: United States first, then states alphabetically
final_spend_eff[, sort_order := fifelse(location_name == "United States", 0, 1)]
final_spend_eff <- final_spend_eff[order(sort_order, location_name)]
final_spend_eff[, sort_order := NULL]

# ----------------------------------------------------------------------------
# B.6: Print summary
# ----------------------------------------------------------------------------

national_se <- final_spend_eff[location_name == "United States"]

cat("\n=========== SPENDING EFFECTIVENESS SUMMARY (WEAVER METHODOLOGY) ===========\n\n")

cat("NATIONAL (United States):\n")
cat(sprintf("  Spend Intensity Effect:        $%s  (95%% UI: $%s - $%s)\n",
            format(round(national_se$spend_intensity_effect), big.mark = ","),
            format(round(national_se$spend_intensity_effect_lower), big.mark = ","),
            format(round(national_se$spend_intensity_effect_upper), big.mark = ",")))
cat(sprintf("  DALY Averted Effect:           %s DALYs  (95%% UI: %s - %s)\n",
            format(round(national_se$daly_averted_effect), big.mark = ","),
            format(round(national_se$daly_averted_effect_lower), big.mark = ","),
            format(round(national_se$daly_averted_effect_upper), big.mark = ",")))
cat(sprintf("  Spending Effectiveness:        %s\n", national_se$interpretation))
cat(sprintf("  Category (point estimate):     %s\n", national_se$category_label))
cat(sprintf("  Category Stable (IQR):         %s\n", national_se$category_stable))
cat(sprintf("  IQR Category Span:             %s\n", national_se$iqr_category_span))
if (!national_se$category_stable) {
  cat(sprintf("  Category Uncertainty:          %s\n", national_se$category_uncertainty))
}

cat("\nSTATE-LEVEL CATEGORY DISTRIBUTION (point estimates):\n")
print(table(final_spend_eff[location_name != "United States", category_label]))

cat("\nSTATES WITH UNSTABLE CATEGORY (IQR spans multiple categories):\n")
uncertain_states <- final_spend_eff[category_stable == FALSE & location_name != "United States",
                                    .(location_name, category_label, interpretation, iqr_category_span)]
if (nrow(uncertain_states) > 0) {
  print(uncertain_states)
} else {
  cat("None - all states have stable category assignments within IQR.\n")
}

cat("\nTOP 10 STATES BY SPENDING EFFECTIVENESS (Category 1, median ratio):\n")
cat1_states <- final_spend_eff[category == 1 & location_name != "United States" & !is.na(spend_effectiveness_median)][
  order(spend_effectiveness_median)][1:min(10, .N)]
if (nrow(cat1_states) > 0) {
  print(cat1_states[, .(location_name, interpretation, category_stable)])
} else {
  cat("No Category 1 states with valid ratios.\n")
}

# ----------------------------------------------------------------------------
# B.7: Save results
# ----------------------------------------------------------------------------

write.csv(final_spend_eff, file.path(dir_output, "T3_HIV_spending_effectiveness.csv"), row.names = FALSE)
write.csv(by_state_daly,   file.path(dir_output, "T8_HIV_decomp_daly.csv"),            row.names = FALSE)

cat(sprintf("\n=========== FILES SAVED TO: %s ===========\n", dir_output))
cat("  - T3_HIV_spending_effectiveness.csv (Weaver methodology: median, IQR, category stability)\n")
cat("  - T6_HIV_decomp_by_state.csv (spending decomposition with 95% UI)\n")
cat("  - T8_HIV_decomp_daly.csv (DALY decomposition with 95% UI)\n")


###############################################################################
# STEP 7: VISUALIZATIONS
###############################################################################

cat("\n\n============ CREATING VISUALIZATIONS ============\n")

# Shared factor ordering & colors (Weaver convention)
factor_levels_spend <- c("Population Size",
                         "Age Structure",
                         "Prevalence Rate (within age groups)",
                         "Spending Intensity")
factor_levels_daly  <- c("Population Size",
                         "Age Structure",
                         "Prevalence Rate (within age groups)",
                         "DALY Intensity")

factor_colors_spend <- c(
  "Population Size"                       = "#66C2A5",
  "Age Structure"                         = "#8DA0CB",
  "Prevalence Rate (within age groups)"   = "#FC8D62",
  "Spending Intensity"                    = "#E78AC3"
)

factor_colors_daly <- c(
  "Population Size"                       = "#66C2A5",
  "Age Structure"                         = "#8DA0CB",
  "Prevalence Rate (within age groups)"   = "#FC8D62",
  "DALY Intensity"                        = "#E78AC3"
)

# ----------------------------------------------------------------------------
# 7.1: Prepare spending data for plotting (PERCENT OF 2010 BASELINE)
# ----------------------------------------------------------------------------

plot_data <- by_state %>%
  select(location_id, location_name, delta_spend, spend_2010,
         pop_size_effect, age_structure_effect,
         prev_rate_effect, spend_intensity_effect) %>%
  pivot_longer(
    cols = c(pop_size_effect, age_structure_effect,
             prev_rate_effect, spend_intensity_effect),
    names_to = "factor",
    values_to = "effect"
  ) %>%
  mutate(
    effect_pct = 100 * effect / spend_2010,
    delta_pct  = 100 * delta_spend / spend_2010,
    factor = case_when(
      factor == "pop_size_effect"        ~ "Population Size",
      factor == "age_structure_effect"   ~ "Age Structure",
      factor == "prev_rate_effect"       ~ "Prevalence Rate (within age groups)",
      factor == "spend_intensity_effect" ~ "Spending Intensity"
    ),
    factor = factor(factor, levels = factor_levels_spend)
  )

# ----------------------------------------------------------------------------
# 7.2: State-Level Stacked Horizontal Bar Chart (ALL LOCATIONS)
# ----------------------------------------------------------------------------

state_order <- by_state %>%
  arrange(delta_spend) %>%
  pull(location_name)
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
  scale_fill_manual(values = factor_colors_spend) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(
    title = "HIV/AIDS Spending Decomposition (2010-2019)",
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
ggsave(file.path(dir_output, "F4_HIV_decomp_all_states_stacked_bar.png"), p_states_all,
       width = 10, height = 15, dpi = 300)

# ----------------------------------------------------------------------------
# 7.3: National Bar Chart
# ----------------------------------------------------------------------------

national_plot_data <- by_state %>%
  filter(location_name == "United States") %>%
  select(pop_size_effect, age_structure_effect,
         prev_rate_effect, spend_intensity_effect, spend_2010) %>%
  pivot_longer(
    cols = c(pop_size_effect, age_structure_effect,
             prev_rate_effect, spend_intensity_effect),
    names_to = "factor",
    values_to = "effect"
  ) %>%
  mutate(
    effect_pct = 100 * effect / spend_2010,
    factor = case_when(
      factor == "pop_size_effect"        ~ "Population Size",
      factor == "age_structure_effect"   ~ "Age Structure",
      factor == "prev_rate_effect"       ~ "Prevalence Rate (within age groups)",
      factor == "spend_intensity_effect" ~ "Spending Intensity"
    ),
    factor = factor(factor, levels = factor_levels_spend),
    location_name = "United States"
  )

national_delta_pct <- 100 * national$delta_spend / national$spend_2010

p_national <- ggplot(national_plot_data, aes(x = location_name, y = effect_pct, fill = factor)) +
  geom_bar(stat = "identity", position = "stack", width = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", alpha = 0.5) +
  geom_point(aes(x = "United States", y = national_delta_pct),
             inherit.aes = FALSE, shape = 18, size = 4, color = "black") +
  coord_flip() +
  scale_fill_manual(values = factor_colors_spend) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(
    title = "National HIV/AIDS Spending Decomposition (2010-2019)",
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
ggsave(file.path(dir_output, "F5_HIV_decomp_national_bar.png"), p_national,
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
  scale_fill_manual(values = factor_colors_spend) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(
    title = "HIV/AIDS Spending Decomposition by State (2010-2019)",
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
ggsave(file.path(dir_output, "F6_HIV_decomp_15_states_stacked_bar.png"), p_states,
       width = 10, height = 8, dpi = 300)

# ----------------------------------------------------------------------------
# 7.5: DALY Decomposition - All Locations (% of 2010 baseline)
# ----------------------------------------------------------------------------

plot_data_daly <- by_state_daly %>%
  mutate(
    delta_pct_from2010                = if_else(daly_2010 > 0, 100 * delta_daly               / daly_2010, NA_real_),
    daly_pop_size_effect_pct          = if_else(daly_2010 > 0, 100 * daly_pop_size_effect     / daly_2010, NA_real_),
    daly_age_structure_effect_pct     = if_else(daly_2010 > 0, 100 * daly_age_structure_effect/ daly_2010, NA_real_),
    daly_prev_rate_effect_pct         = if_else(daly_2010 > 0, 100 * daly_prev_rate_effect    / daly_2010, NA_real_),
    daly_intensity_effect_pct         = if_else(daly_2010 > 0, 100 * daly_intensity_effect    / daly_2010, NA_real_)
  ) %>%
  select(
    location_id, location_name, delta_pct_from2010,
    daly_pop_size_effect_pct, daly_age_structure_effect_pct,
    daly_prev_rate_effect_pct, daly_intensity_effect_pct
  ) %>%
  pivot_longer(
    cols = c(daly_pop_size_effect_pct, daly_age_structure_effect_pct,
             daly_prev_rate_effect_pct, daly_intensity_effect_pct),
    names_to = "factor",
    values_to = "effect_pct"
  ) %>%
  mutate(
    factor = case_when(
      factor == "daly_pop_size_effect_pct"       ~ "Population Size",
      factor == "daly_age_structure_effect_pct"  ~ "Age Structure",
      factor == "daly_prev_rate_effect_pct"      ~ "Prevalence Rate (within age groups)",
      factor == "daly_intensity_effect_pct"      ~ "DALY Intensity"
    ),
    factor = factor(factor, levels = factor_levels_daly)
  )

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
  scale_fill_manual(values = factor_colors_daly) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(
    title = "HIV/AIDS DALY Decomposition (2010-2019)",
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
ggsave(file.path(dir_output, "F7_HIV_daly_decomp_all_states_stacked_bar.png"), p_daly_states_all,
       width = 10, height = 15, dpi = 300)

# ----------------------------------------------------------------------------
# 7.6: Two-panel figure: Panel A Spending + Panel B DALY decomposition
#      Alphabetical state order, United States at top
# ----------------------------------------------------------------------------

state_levels_alpha <- c(rev(sort(unique(by_state$location_name[by_state$location_name != "United States"]))),
                        "United States")

# Panel A data: Spending
plot_spend <- by_state %>%
  mutate(
    delta_pct_from2010           = if_else(spend_2010 > 0, 100 * delta_spend            / spend_2010, NA_real_),
    pop_size_effect_pct          = if_else(spend_2010 > 0, 100 * pop_size_effect        / spend_2010, NA_real_),
    age_structure_effect_pct     = if_else(spend_2010 > 0, 100 * age_structure_effect   / spend_2010, NA_real_),
    prev_rate_effect_pct         = if_else(spend_2010 > 0, 100 * prev_rate_effect       / spend_2010, NA_real_),
    spend_intensity_effect_pct   = if_else(spend_2010 > 0, 100 * spend_intensity_effect / spend_2010, NA_real_)
  ) %>%
  select(location_name, delta_pct_from2010, pop_size_effect_pct, age_structure_effect_pct,
         prev_rate_effect_pct, spend_intensity_effect_pct) %>%
  pivot_longer(
    cols = c(pop_size_effect_pct, age_structure_effect_pct,
             prev_rate_effect_pct, spend_intensity_effect_pct),
    names_to = "factor", values_to = "effect_pct"
  ) %>%
  mutate(
    factor = case_when(
      factor == "pop_size_effect_pct"        ~ "Population Size",
      factor == "age_structure_effect_pct"   ~ "Age Structure",
      factor == "prev_rate_effect_pct"       ~ "Prevalence Rate (within age groups)",
      factor == "spend_intensity_effect_pct" ~ "Spending Intensity"
    ),
    factor = factor(factor, levels = factor_levels_spend),
    location_name = factor(location_name, levels = state_levels_alpha)
  )

# Panel B data: DALY
plot_daly_panel <- by_state_daly %>%
  mutate(
    delta_pct_from2010             = if_else(daly_2010 > 0, 100 * delta_daly                / daly_2010, NA_real_),
    pop_size_effect_pct            = if_else(daly_2010 > 0, 100 * daly_pop_size_effect      / daly_2010, NA_real_),
    age_structure_effect_pct       = if_else(daly_2010 > 0, 100 * daly_age_structure_effect / daly_2010, NA_real_),
    prev_rate_effect_pct           = if_else(daly_2010 > 0, 100 * daly_prev_rate_effect     / daly_2010, NA_real_),
    daly_intensity_effect_pct      = if_else(daly_2010 > 0, 100 * daly_intensity_effect     / daly_2010, NA_real_)
  ) %>%
  select(location_name, delta_pct_from2010, pop_size_effect_pct, age_structure_effect_pct,
         prev_rate_effect_pct, daly_intensity_effect_pct) %>%
  pivot_longer(
    cols = c(pop_size_effect_pct, age_structure_effect_pct,
             prev_rate_effect_pct, daly_intensity_effect_pct),
    names_to = "factor", values_to = "effect_pct"
  ) %>%
  mutate(
    factor = case_when(
      factor == "pop_size_effect_pct"        ~ "Population Size",
      factor == "age_structure_effect_pct"   ~ "Age Structure",
      factor == "prev_rate_effect_pct"       ~ "Prevalence Rate (within age groups)",
      factor == "daly_intensity_effect_pct"  ~ "DALY Intensity"
    ),
    factor = factor(factor, levels = factor_levels_daly),
    location_name = factor(location_name, levels = state_levels_alpha)
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
    title = "Panel A. HIV Spending Decomposition (2010-2019)",
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
    title = "Panel B. HIV DALY Decomposition (2010-2019)",
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
ggsave(file.path(dir_output, "F8_HIV_spend_daly_decomp_two_panel.png"), p_combined,
       width = 18, height = 15, dpi = 300)

cat("\n=========== VISUALIZATIONS SAVED ===========\n")
cat(sprintf("Files saved to: %s\n", dir_output))
cat("  - F4_HIV_decomp_all_states_stacked_bar.png (all locations incl. US)\n")
cat("  - F5_HIV_decomp_national_bar.png\n")
cat("  - F6_HIV_decomp_15_states_stacked_bar.png\n")
cat("  - F7_HIV_daly_decomp_all_states_stacked_bar.png\n")
cat("  - F8_HIV_spend_daly_decomp_two_panel.png\n")

cat("\n============ SCRIPT COMPLETE ============\n")