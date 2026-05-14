##----------------------------------------------------------------
##' Title: C_regression_models_analysis.R
##'
##' Purpose: SUD Spending-Outcomes Regression Analysis for Aim 2B
##'          Committee-ready pipeline — theory-driven model set.
##'
##' Pipeline order:
##'   1. Setup & Data Loading
##'   2. Log-Transform Outcome / Exposure
##'   3. Create ALL Analysis Variables
##'   4. Save Analysis Datasets
##'   5. Summary Table (internal Table 1 + skewness diagnostics)
##'   6. Diagnostics (NO regressions yet)
##'   7. Fit Regression Models (7 families)
##'   8. Extract Results (cluster-robust SEs) & Save
##'
##' IMPORTANT — DENOMINATOR BIAS:
##'   Prevalence counts appear in both Y (mort/prev) and X (spend/prev)
##'   denominators.  Continuous prevalence_rates MUST NOT be used as a
##'   covariate — it creates mechanical negative correlation with both
##'   the outcome and the exposure.  Use log_incidence_rates instead.
##'   The ONLY exception is the binary high_sud_prev_B indicator used
##'   in the interaction model, which is safe because it captures a
##'   discrete group contrast rather than continuous denominator overlap.
##'
##'
##' Data processing lives in C_model_data_prep.R.
##' This script consumes the processed output.
##----------------------------------------------------------------

##================================================================
## 1.  SETUP & DATA LOADING
##================================================================
rm(list = ls())

# ---------- paths ----------
if (Sys.info()["sysname"] == "Linux") {
  j <- "/home/j/"
  h <- paste0("/ihme/homes/", Sys.info()[7], "/")
  l <- "/ihme/limited_use/"
} else if (Sys.info()["sysname"] == "Darwin") {
  j <- "/Volumes/snfs"
  h <- paste0("/Volumes/", Sys.info()[7], "/")
  l <- "/Volumes/limited_use"
} else {
  j <- "J:/"
  h <- "H:/"
  l <- "L:/"
}

# ---------- user library (IHME cluster requires local lib) ----------
user_lib <- file.path(h, "R_packages")
if (!dir.exists(user_lib)) dir.create(user_lib, recursive = TRUE)
.libPaths(c(user_lib, .libPaths()))

pacman::p_load(
  data.table, tidyverse, glue, broom, e1071,
  lmtest, sandwich, clubSandwich,
  corrplot, Hmisc
)

# ---------- resolve filter conflict (plotly vs dplyr) ----------
if ("plotly" %in% loadedNamespaces()) {
  filter <- dplyr::filter
}
tryCatch(
  conflicted::conflicts_prefer(dplyr::filter, .quiet = TRUE),
  error = function(e) invisible(NULL)
)

input_date  <- "20260409"
dir_input   <- file.path(h, "aim_outputs/Aim2/C_frontier_analysis", input_date)

output_date <- format(Sys.time(), "%Y%m%d")
dir_output  <- file.path(h, "aim_outputs/Aim2/C_frontier_analysis", output_date, "analysis")
if (!dir.exists(dir_output)) dir.create(dir_output, recursive = TRUE)

# ---------- load ----------
df_as <- read.csv(
  file.path(dir_input, "df_as_processed_rw_gbd.csv"),
  stringsAsFactors = FALSE
)

#unique(df_as$acause)

df_sud <- df_as %>%
  dplyr::filter(acause == "mental_drug_opioids")


# df_sud <- df_as %>%
#   dplyr::filter(acause == "mental_alcohol")


# ==============================================================
# CATEGORY 1/2 FILTERED MODELS (conservative threshold)
#
# Conservative filter: state must have at least `cat12_threshold`
# percent of crossed posterior draws falling in Cat 1 (+spend, +health)
# or Cat 2 (cost-saving) per the Weaver decomposition output (T3).
# Drops states where Cat 3 or Cat 4 mass is non-trivial.
#
# Filter is state-level and propagates across all years for that
# state (state-mean property, applied to YFE panel).
# ==============================================================

# ---- Configuration ----
cat12_threshold <- 90     # require pct_cat1 + pct_cat2 >= this value (try 80, 90, 95)
date_t3         <- "20260426"

fp_t3 <- file.path(h, "aim_outputs/Aim2/D_tables_figures",
                   date_t3, "T3_OUD_spending_effectiveness.csv")
if (!file.exists(fp_t3)) stop("T3 file not found: ", fp_t3)

df_t3 <- read.csv(fp_t3, stringsAsFactors = FALSE)

# ---- State-level inclusion flag ----
df_t3_keep <- df_t3 %>%
  dplyr::filter(location_name != "United States",
                (pct_cat1 + pct_cat2) >= cat12_threshold)

keep_ids     <- df_t3_keep$location_id
df_sud_cat12 <- df_sud %>% dplyr::filter(location_id %in% keep_ids)

cat("\n---- Cat 1/2 filter (threshold = ", cat12_threshold, "%) ----\n", sep = "")
cat("States kept:", length(keep_ids), "of", nrow(df_t3) - 1, "(excluding US)\n")
cat("Panel obs:",   nrow(df_sud_cat12), "of", nrow(df_sud), "\n")

cat("\nKept states (sorted by Cat 1/2 share):\n")
print(df_t3_keep %>%
        dplyr::mutate(pct_cat12 = round(pct_cat1 + pct_cat2, 1)) %>%
        dplyr::select(location_name, pct_cat1, pct_cat2, pct_cat12,
                      spend_effectiveness_median_category, dominant_category) %>%
        dplyr::arrange(desc(pct_cat12)) %>%
        as.data.frame())

cat("\nDropped states:\n")
print(df_t3 %>%
        dplyr::filter(location_name != "United States",
                      !location_id %in% keep_ids) %>%
        dplyr::mutate(pct_cat12 = round(pct_cat1 + pct_cat2, 1)) %>%
        dplyr::select(location_name, pct_cat12, pct_cat3, pct_cat4, dominant_category) %>%
        dplyr::arrange(pct_cat12) %>%
        as.data.frame())

##================================================================
## 2.  LOG-TRANSFORM OUTCOME & EXPOSURE
##================================================================
safe_log <- function(x) if_else(x > 0, log(x), NA_real_)

df_sud <- df_sud %>%
  mutate(
    as_mort_prev_ratio_log  = safe_log(as_mort_prev_ratio),
    as_daly_prev_ratio_log  = safe_log(as_daly_prev_ratio),
    as_yll_prev_ratio_log   = safe_log(as_yll_prev_ratio),
    as_yld_prev_ratio_log   = safe_log(as_yld_prev_ratio),
    as_spend_prev_ratio_log = safe_log(as_spend_prev_ratio),
    spend_tx_prev_ratio_log = safe_log(spend_tx_prev_ratio)
  )


##----------------------------------------------------------------
## TOC spending per prevalence (by care setting)
##----------------------------------------------------------------
df_sud <- df_sud  %>%
  mutate(
    # Per prevalence (crude, consistent with spend_prev_ratio logic)
    spend_AM_prev_ratio = spend_AM / prevalence_counts,
    spend_ED_prev_ratio = spend_ED / prevalence_counts,
    spend_HH_prev_ratio = spend_HH / prevalence_counts,
    spend_IP_prev_ratio = spend_IP / prevalence_counts,
    spend_NF_prev_ratio = spend_NF / prevalence_counts,
    spend_RX_prev_ratio = spend_RX / prevalence_counts
  )

df_sud <- df_sud %>%
  mutate(
    spend_AM_prev_ratio_log = safe_log(spend_AM_prev_ratio),
    spend_ED_prev_ratio_log = safe_log(spend_ED_prev_ratio),
    spend_HH_prev_ratio_log = safe_log(spend_HH_prev_ratio),
    spend_IP_prev_ratio_log = safe_log(spend_IP_prev_ratio),
    spend_NF_prev_ratio_log = safe_log(spend_NF_prev_ratio),
    spend_RX_prev_ratio_log = safe_log(spend_RX_prev_ratio)
  )

##================================================================
## 3.  CREATE ALL ANALYSIS VARIABLES
##================================================================

# ---- 3a. Mundlak Between / Within decomposition ----

make_mundlak_vars <- function(df, vars, group_var = "location_id",
                              midpoint = 2014) {
  state_means <- df %>%
    group_by(across(all_of(group_var))) %>%
    summarise(
      across(all_of(vars), \(x) mean(x, na.rm = TRUE), .names = "{.col}_B"),
      .groups = "drop"
    )
  df <- left_join(df, state_means, by = group_var)
  for (v in vars) {
    df[[paste0(v, "_W")]] <- df[[v]] - df[[paste0(v, "_B")]]
  }
  df$year_centered <- df$year_id - midpoint
  df
}

mundlak_vars_requested <- c(
  "as_spend_prev_ratio_log",
  "race_prop_BLCK",
  "race_prop_HISP",
  "incidence_rates",
  "bmi",
  "obesity",
  "prev_diabetes",
  "aca_implemented_status",
  "edu_yrs",
  "mortality_rates",
  "prevalence_rates",
  "prop_homeless",
  "ldi_pc",
  "unemployment_rate"
)
mundlak_vars <- intersect(mundlak_vars_requested, names(df_sud))

df_sud <- make_mundlak_vars(df_sud, mundlak_vars, "location_id", midpoint = 2014)


# ---- 3b. Log transforms of key covariates ----

df_sud <- df_sud %>%
  mutate(
    log_incidence_rates      = safe_log(incidence_rates),
    log_incidence_rates_B    = safe_log(incidence_rates_B),
    log_bmi_B                = safe_log(bmi_B),
    log_prev_diabetes_B      = safe_log(prev_diabetes_B),
    log_prop_homeless        = safe_log(prop_homeless),
    log_prop_homeless_B      = safe_log(prop_homeless_B),
    log_ldi_pc               = safe_log(ldi_pc),
    log_ldi_pc_B             = safe_log(ldi_pc_B),
    # Within deviations of logged covariates (for Mundlak on log scale)
    log_prop_homeless_W      = log_prop_homeless - log_prop_homeless_B,
    log_incidence_rates_W    = log_incidence_rates - log_incidence_rates_B
  )


# ---- 3c. Indicators ----

# High-prevalence binary: median split of state-mean prevalence rates.
# Used ONLY in the interaction model.  Binary is safe because it captures
# a discrete group contrast, not continuous denominator overlap.
if (!"high_sud_prev_B" %in% names(df_sud)) {
  if ("prevalence_rates_B" %in% names(df_sud)) {
    prev_median_B <- quantile(df_sud$prevalence_rates_B, 0.50, na.rm = TRUE)
    df_sud <- df_sud %>%
      mutate(high_sud_prev_B = as.integer(prevalence_rates_B >= prev_median_B))
  } else {
    state_prev <- df_sud %>%
      group_by(location_id) %>%
      summarise(prev_count_mean = mean(prevalence_counts, na.rm = TRUE),
                .groups = "drop")
    prev_median <- median(state_prev$prev_count_mean, na.rm = TRUE)
    df_sud <- df_sud %>%
      left_join(state_prev, by = "location_id") %>%
      mutate(high_sud_prev_B = as.integer(prev_count_mean >= prev_median)) %>%
      select(-prev_count_mean)
  }
}

df_sud <- df_sud %>%
  mutate(
    high_sud_prev_B_f = factor(
      high_sud_prev_B,
      levels = c(0, 1),
      labels = c("Lower prevalence", "Higher prevalence")
    )
  )


# ---- 3d. Lag variables ----
df_sud <- df_sud %>%
  arrange(location_id, year_id) %>%
  group_by(location_id) %>%
  mutate(
    as_spend_prev_ratio_log_l1 = dplyr::lag(as_spend_prev_ratio_log, 1),
    as_spend_prev_ratio_log_l2 = dplyr::lag(as_spend_prev_ratio_log, 2)
  ) %>%
  ungroup()


# ---- 3e. Dose-response variables ----
knot_p75 <- quantile(df_sud$as_spend_prev_ratio_log, 0.75, na.rm = TRUE)
df_sud <- df_sud %>%
  mutate(
    as_spend_prev_ratio_log_sq = as_spend_prev_ratio_log^2,
    over_p75 = pmax(0, as_spend_prev_ratio_log - knot_p75)
  )


# ---- 3f. First-difference variables ----
df_sud <- df_sud %>%
  arrange(location_id, year_id) %>%
  group_by(location_id) %>%
  mutate(
    d_mort_log      = as_mort_prev_ratio_log  - dplyr::lag(as_mort_prev_ratio_log, 1),
    d_daly_log      = as_daly_prev_ratio_log  - dplyr::lag(as_daly_prev_ratio_log, 1),
    d_spend_log     = as_spend_prev_ratio_log - dplyr::lag(as_spend_prev_ratio_log, 1),
    d_unemployment  = unemployment_rate       - dplyr::lag(unemployment_rate, 1),
    d_log_incidence = log_incidence_rates     - dplyr::lag(log_incidence_rates, 1),
    d_log_homeless  = log_prop_homeless        - dplyr::lag(log_prop_homeless, 1)
  ) %>%
  ungroup()


# ---- 3g. Year as factor (for between_yfe family) ----
df_sud <- df_sud %>%
  mutate(year_factor = factor(year_id))


##================================================================
## 4.  SAVE ANALYSIS DATASETS
##================================================================

# ---- 4a. Full panel dataset ----
write.csv(
  df_sud,
  file.path(dir_output, "df_sud_analysis_panel.csv"),
  row.names = FALSE
)


# ---- 4b. Collapsed state-mean dataset (TRUE between estimator) ----
# Per Joe: collapse BOTH Y and X (and covariates) to one row per state.

id_cols <- c("location_id", "location_name", "year_id", "cause_id")

numeric_cols <- df_sud %>%
  select(where(is.numeric)) %>%
  names()

collapse_cols <- setdiff(numeric_cols, id_cols)

df_between <- df_sud %>%
  group_by(location_id, location_name) %>%
  summarise(
    across(all_of(collapse_cols), \(x) mean(x, na.rm = TRUE)),
    n_years = dplyr::n(),
    .groups = "drop"
  )

# Reconstruct factor indicators on collapsed data
if ("high_sud_prev_B" %in% names(df_between)) {
  df_between <- df_between %>%
    mutate(
      high_sud_prev_B = as.integer(round(high_sud_prev_B)),
      high_sud_prev_B_f = factor(
        high_sud_prev_B,
        levels = c(0, 1),
        labels = c("Lower prevalence", "Higher prevalence")
      )
    )
}

write.csv(
  df_between,
  file.path(dir_output, "df_sud_analysis_between.csv"),
  row.names = FALSE
)


# ---- 4c. Pre-fentanyl collapsed (2010-2014) ----
df_sud_pre2015 <- df_sud %>% dplyr::filter(year_id <= 2014)

df_between_pre <- df_sud_pre2015 %>%
  group_by(location_id, location_name) %>%
  summarise(
    across(all_of(intersect(collapse_cols, names(df_sud_pre2015))),
           \(x) mean(x, na.rm = TRUE)),
    n_years = dplyr::n(),
    .groups = "drop"
  )

write.csv(
  df_between_pre,
  file.path(dir_output, "df_sud_analysis_between_pre2015.csv"),
  row.names = FALSE
)


# ---- 4d. Post-fentanyl collapsed (2015-2019) ----
df_sud_post2015 <- df_sud %>% dplyr::filter(year_id >= 2015)

df_between_post <- df_sud_post2015 %>%
  group_by(location_id, location_name) %>%
  summarise(
    across(all_of(intersect(collapse_cols, names(df_sud_post2015))),
           \(x) mean(x, na.rm = TRUE)),
    n_years = dplyr::n(),
    .groups = "drop"
  )

write.csv(
  df_between_post,
  file.path(dir_output, "df_sud_analysis_between_post2015.csv"),
  row.names = FALSE
)

cat("Panel dataset rows:", nrow(df_sud), "\n")
cat("Collapsed between dataset rows:", nrow(df_between), "\n")
cat("Pre-fentanyl between rows:", nrow(df_between_pre), "\n")
cat("Post-fentanyl between rows:", nrow(df_between_post), "\n")


##================================================================
## 5.  SUMMARY TABLE  (internal Table 1 + skewness diagnostics)
##================================================================

summary_vars <- c(
  # Outcome & exposure (raw + log)
  "as_mort_prev_ratio",    "as_mort_prev_ratio_log",
  "as_daly_prev_ratio",    "as_daly_prev_ratio_log",
  "as_spend_prev_ratio",   "as_spend_prev_ratio_log",
  "as_spend_prev_ratio_log_B","spend_tx_prev_ratio", "spend_tx_prev_ratio_log",
  # Key covariates used in models
  "race_prop_BLCK",  "race_prop_BLCK_B",
  "race_prop_HISP",  "race_prop_HISP_B",
  "incidence_rates", "incidence_rates_B", "log_incidence_rates_B",
  "prop_homeless",   "log_prop_homeless_B",
  "prev_diabetes",   "log_prev_diabetes_B",
  "bmi",             "log_bmi_B",
  "obesity",         "obesity_B",
  "ldi_pc",          "log_ldi_pc",  "log_ldi_pc_B",
  "unemployment_rate", "unemployment_rate_B",
  "edu_yrs",         "edu_yrs_B",
  "aca_implemented_status",
  # Contextual
  "population", "prevalence_counts", "mortality_counts",
  "spend_all", "ryan_white_funding_final"
)

summary_vars <- intersect(summary_vars, names(df_sud))

summary_tbl <- df_sud %>%
  select(all_of(summary_vars)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  group_by(variable) %>%
  summarise(
    n       = sum(!is.na(value)),
    mean    = mean(value, na.rm = TRUE),
    sd      = sd(value, na.rm = TRUE),
    min     = min(value, na.rm = TRUE),
    p25     = quantile(value, 0.25, na.rm = TRUE),
    median  = median(value, na.rm = TRUE),
    p75     = quantile(value, 0.75, na.rm = TRUE),
    max     = max(value, na.rm = TRUE),
    skewness = {
      v <- value[!is.na(value)]
      n_v <- length(v)
      if (n_v < 3) NA_real_
      else (sum((v - mean(v))^3) / n_v) / (sum((v - mean(v))^2) / n_v)^1.5
    },
    .groups = "drop"
  ) %>%
  mutate(
    log_justified = case_when(
      skewness > 1   ~ "yes - strong right skew",
      skewness > 0.5 ~ "maybe - moderate skew",
      skewness < -1  ~ "yes - strong left skew",
      TRUE           ~ "no"
    )
  )

write.csv(summary_tbl,
          file.path(dir_output, "summary_table1_diagnostics.csv"),
          row.names = FALSE)


##================================================================
## 6.  DIAGNOSTICS  (no regressions yet)
##================================================================

# ---- 6a. Full correlation matrix among model-ready variables ----

corr_vars <- c(
  "as_mort_prev_ratio_log",
  "as_spend_prev_ratio_log_B", "as_spend_prev_ratio_log_W",
  "year_centered",
  "race_prop_BLCK_B", "race_prop_HISP_B",
  "log_incidence_rates_B", "incidence_rates_W",
  "log_bmi_B", "bmi_W",
  "obesity_B",
  "log_prev_diabetes_B",
  "log_prop_homeless_B",
  "log_ldi_pc_B",
  "unemployment_rate_B",
  "edu_yrs_B",
  "aca_implemented_status_B",
  "high_sud_prev_B",
  "spend_tx_prev_ratio"
)
corr_vars <- intersect(corr_vars, names(df_sud))

corr_mat <- df_sud %>%
  select(all_of(corr_vars)) %>%
  drop_na() %>%
  cor()

write.csv(as.data.frame(corr_mat),
          file.path(dir_output, "correlation_matrix_model_vars.csv"))


# ---- 6b. Confounder screening (exposure & outcome correlations) ----

screen_confounders <- function(df, exposure, outcome,
                               exclude_vars = character(),
                               r_thresh = 0.20,
                               dir_output = NULL,
                               file_stub  = "confounder_screen") {
  numeric_vars <- df %>% select(where(is.numeric)) %>% colnames()
  exclude_all  <- unique(c(exclude_vars, exposure, outcome))
  candidates   <- setdiff(numeric_vars, exclude_all)
  
  mat <- df %>% select(all_of(c(exposure, outcome, candidates))) %>% as.matrix()
  rc  <- Hmisc::rcorr(mat)
  
  pull_stats <- function(target) {
    r <- rc$r[candidates, target]
    p <- rc$P[candidates, target]
    tibble(
      var              = candidates,
      !!paste0("r_", target)   := as.numeric(r),
      !!paste0("p_", target)   := as.numeric(p),
      !!paste0("dir_", target) := sign(as.numeric(r))
    )
  }
  
  ex_stats  <- pull_stats(exposure)
  out_stats <- pull_stats(outcome)
  
  res <- ex_stats %>%
    left_join(out_stats, by = "var") %>%
    rename(
      r_exp   = !!paste0("r_", exposure),
      p_exp   = !!paste0("p_", exposure),
      dir_exp = !!paste0("dir_", exposure),
      r_out   = !!paste0("r_", outcome),
      p_out   = !!paste0("p_", outcome),
      dir_out = !!paste0("dir_", outcome)
    ) %>%
    mutate(
      pass = (abs(r_exp) >= r_thresh & abs(r_out) >= r_thresh)
    ) %>%
    select(var, r_exp, dir_exp, p_exp, r_out, dir_out, p_out, pass) %>%
    arrange(desc(abs(r_exp) + abs(r_out)))
  
  shortlisted <- res %>% dplyr::filter(pass)
  
  if (!is.null(dir_output)) {
    readr::write_csv(res,         file.path(dir_output, paste0(file_stub, "_ALL.csv")))
    readr::write_csv(shortlisted, file.path(dir_output, paste0(file_stub, "_SHORTLIST.csv")))
  }
  list(all = res, shortlist = shortlisted)
}

exclude_base <- c("cause_id", "year_id", "location_id", "year_centered")
exclude_mechanical <- c(
  "as_spend_prev_ratio", "as_spend_prev_ratio_log",
  "rw_sud_prev_ratio", "rw_sud_prev_ratio_log",
  "ryan_white_funding_final", "spend_all",
  "spend_mdcd", "spend_mdcr", "spend_oop", "spend_priv",
  "as_mort_prev_ratio", "as_mort_prev_ratio_log",
  "mortality_rates", "mortality_counts",
  "prevalence_counts", "sud_prevalence_counts",
  "daly_rates", "yll_rates", "yld_rates",
  "daly_counts", "yll_counts", "yld_counts", "incidence_counts",
  "as_daly_prev_ratio", "as_yll_prev_ratio", "as_yld_prev_ratio",
  "as_daly_prev_ratio_log", "as_yll_prev_ratio_log", "as_yld_prev_ratio_log",
  "population", "variance",
  "prevalence_rates", "high_sud_prev", "sud_prevalence_counts"
)

# Panel-level screening
screen_confounders(
  df           = df_sud,
  exposure     = "as_spend_prev_ratio_log",
  outcome      = "as_mort_prev_ratio_log",
  exclude_vars = c(exclude_base, exclude_mechanical),
  r_thresh     = 0.20,
  dir_output   = dir_output,
  file_stub    = "confounder_screen_PANEL"
)

# State-means screening
df_state_means <- df_sud %>%
  group_by(location_id, location_name) %>%
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)),
            .groups = "drop")

screen_confounders(
  df           = df_state_means,
  exposure     = "as_spend_prev_ratio_log",
  outcome      = "as_mort_prev_ratio_log",
  exclude_vars = c(exclude_base, exclude_mechanical),
  r_thresh     = 0.20,
  dir_output   = dir_output,
  file_stub    = "confounder_screen_STATE"
)


# ---- 6c. Variance decomposition ----

calc_variance_decomp <- function(df_panel, var_name, group_var = "location_id") {
  x <- df_panel[[var_name]]
  total_var <- var(x, na.rm = TRUE)
  
  state_means <- df_panel %>%
    group_by(across(all_of(group_var))) %>%
    summarise(xbar = mean(.data[[var_name]], na.rm = TRUE), .groups = "drop")
  between_var <- var(state_means$xbar, na.rm = TRUE)
  
  df_tmp <- df_panel %>%
    left_join(state_means %>% rename(!!paste0(var_name, "_mean") := xbar),
              by = group_var) %>%
    mutate(deviation = .data[[var_name]] - .data[[paste0(var_name, "_mean")]])
  within_var <- var(df_tmp$deviation, na.rm = TRUE)
  
  tibble(
    variable    = var_name,
    total_var   = total_var,
    between_var = between_var,
    within_var  = within_var,
    between_pct = round(between_var / total_var * 100, 1),
    within_pct  = round(within_var  / total_var * 100, 1)
  )
}

variance_decomp <- bind_rows(
  calc_variance_decomp(df_sud, "as_spend_prev_ratio_log"),
  calc_variance_decomp(df_sud, "as_mort_prev_ratio_log"),
  calc_variance_decomp(df_sud, "race_prop_BLCK"),
  calc_variance_decomp(df_sud, "race_prop_HISP"),
  calc_variance_decomp(df_sud, "incidence_rates"),
  calc_variance_decomp(df_sud, "obesity"),
  calc_variance_decomp(df_sud, "bmi"),
  calc_variance_decomp(df_sud, "prev_diabetes"),
  calc_variance_decomp(df_sud, "edu_yrs"),
  calc_variance_decomp(df_sud, "prop_homeless"),
  calc_variance_decomp(df_sud, "ldi_pc"),
  calc_variance_decomp(df_sud, "unemployment_rate"),
  calc_variance_decomp(df_sud, "aca_implemented_status")
)

write.csv(variance_decomp,
          file.path(dir_output, "variance_decomposition.csv"),
          row.names = FALSE)



##================================================================
## 7.  FIT REGRESSION MODELS  (between_yfe family only)
##================================================================

list_models    <- list()
model_data     <- list()
model_registry <- tibble(
  model_id = character(),
  family   = character(),
  spec     = character(),
  is_final = logical()
)

register_model <- function(family, spec, formula, data,
                           is_final = FALSE) {
  model_id <- paste0("sud__", family, "__", spec)
  fit <- lm(as.formula(formula), data = data)
  list_models[[model_id]] <<- fit
  model_data[[model_id]]  <<- data
  model_registry <<- bind_rows(
    model_registry,
    tibble(model_id = model_id, family = family,
           spec = spec, is_final = is_final)
  )
  invisible(fit)
}


# ==============================================================
# CATEGORY 1/2 FILTERED MODELS  (OUD spending-effectiveness)
#
# Subset to states whose median spending-effectiveness falls in
# Cat 1 (+spend, +health) or Cat 2 (cost-saving) per the Weaver
# decomposition output (T3).  Drops states dominated by Cat 3
# (dominated) or Cat 4 (disinvestment with health loss), which
# run the opposite direction and add noise to the pooled fit.
# ==============================================================

# ==============================================================
# CATEGORY-STRATIFIED & CATEGORY-INTERACTION MODELS
#
# A. INTERACTION: as_spend * dom_cat_f -> per-category spending slope
#    in one model, with pooled SEs.
# B. STRATIFIED: separate regression per dominant category. Cat 3/4
#    typically have very few states; results there are diagnostic only.
# ==============================================================

# ---- Merge dominant_category from T3 onto panel ----
df_cat_lookup <- df_t3 %>%
  dplyr::filter(location_name != "United States") %>%
  dplyr::select(location_id, dominant_category,
                pct_cat1, pct_cat2, pct_cat3, pct_cat4)

df_sud <- df_sud %>%
  dplyr::left_join(df_cat_lookup, by = "location_id") %>%
  dplyr::mutate(
    dom_cat_f = factor(
      dominant_category,
      levels = c(1, 2, 3, 4),
      labels = c("Cat1", "Cat2", "Cat3", "Cat4")
    )
  )

cat("\n---- Dominant category counts (states & panel obs) ----\n")
print(df_sud %>%
        dplyr::distinct(location_id, location_name, dom_cat_f) %>%
        dplyr::count(dom_cat_f, name = "n_states") %>%
        dplyr::left_join(
          df_sud %>% dplyr::count(dom_cat_f, name = "n_obs"),
          by = "dom_cat_f"
        ) %>% as.data.frame())

# ==============================================================
# A. INTERACTION MODELS  (Cat1 = reference)
# ==============================================================

register_model(
  family = "between_yfe", spec = "primary_catinteract_mort",
  formula = "as_mort_prev_ratio_log ~
               as_spend_prev_ratio_log * dom_cat_f +
               year_factor +
               unemployment_rate +
               race_prop_BLCK + log_incidence_rates +
               race_prop_HISP +
               log_prop_homeless",
  data = df_sud, is_final = TRUE
)

register_model(
  family = "between_yfe", spec = "primary_catinteract_daly",
  formula = "as_daly_prev_ratio_log ~
               as_spend_prev_ratio_log * dom_cat_f +
               year_factor +
               unemployment_rate +
               race_prop_BLCK + log_incidence_rates +
               race_prop_HISP +
               log_prop_homeless",
  data = df_sud, is_final = TRUE
)

register_model(
  family = "between_yfe", spec = "bivariate_catinteract_mort",
  formula = "as_mort_prev_ratio_log ~
               as_spend_prev_ratio_log * dom_cat_f + year_factor",
  data = df_sud, is_final = FALSE
)

register_model(
  family = "between_yfe", spec = "bivariate_catinteract_daly",
  formula = "as_daly_prev_ratio_log ~
               as_spend_prev_ratio_log * dom_cat_f + year_factor",
  data = df_sud, is_final = FALSE
)

# ==============================================================
# B. STRATIFIED MODELS  (one fit per category)
# Skip categories with < 3 states.
# ==============================================================

min_states_for_strat <- 3

for (cat_lvl in 1:4) {
  
  n_states_cat <- df_sud %>%
    dplyr::filter(dominant_category == cat_lvl) %>%
    dplyr::distinct(location_id) %>% nrow()
  
  if (n_states_cat < min_states_for_strat) {
    cat(sprintf("Skipping stratified Cat %d (only %d states)\n",
                cat_lvl, n_states_cat))
    next
  }
  
  df_subset <- df_sud %>% dplyr::filter(dominant_category == cat_lvl)
  
  register_model(
    family = "between_yfe",
    spec   = paste0("primary_mort_cat", cat_lvl, "only"),
    formula = "as_mort_prev_ratio_log ~
                 as_spend_prev_ratio_log + year_factor +
                 unemployment_rate +
                 race_prop_BLCK + log_incidence_rates +
                 race_prop_HISP +
                 log_prop_homeless",
    data = df_subset, is_final = FALSE
  )
  
  register_model(
    family = "between_yfe",
    spec   = paste0("primary_daly_cat", cat_lvl, "only"),
    formula = "as_daly_prev_ratio_log ~
                 as_spend_prev_ratio_log + year_factor +
                 unemployment_rate +
                 race_prop_BLCK + log_incidence_rates +
                 race_prop_HISP +
                 log_prop_homeless",
    data = df_subset, is_final = FALSE
  )
  
  register_model(
    family = "between_yfe",
    spec   = paste0("bivariate_mort_cat", cat_lvl, "only"),
    formula = "as_mort_prev_ratio_log ~ as_spend_prev_ratio_log + year_factor",
    data = df_subset, is_final = FALSE
  )
  
  register_model(
    family = "between_yfe",
    spec   = paste0("bivariate_daly_cat", cat_lvl, "only"),
    formula = "as_daly_prev_ratio_log ~ as_spend_prev_ratio_log + year_factor",
    data = df_subset, is_final = FALSE
  )
}

# ==============================================================
# PRIMARY MODELS (mort then daly)
# ==============================================================

register_model(
  family   = "between_yfe",
  spec     = "primary",
  formula  = "as_mort_prev_ratio_log ~
                as_spend_prev_ratio_log + year_factor +
                unemployment_rate +
                race_prop_BLCK + log_incidence_rates +
                race_prop_HISP +
                log_prop_homeless",
  data     = df_sud,
  is_final = TRUE
)

register_model(
  family   = "between_yfe",
  spec     = "primary_daly",
  formula  = "as_daly_prev_ratio_log ~
                as_spend_prev_ratio_log + year_factor +
                unemployment_rate +
                race_prop_BLCK + log_incidence_rates +
                race_prop_HISP +
                log_prop_homeless",
  data     = df_sud,
  is_final = TRUE
)

# ==============================================================
# BIVARIATE MODELS
# ==============================================================

register_model(
  family   = "between_yfe",
  spec     = "bivariate_mort",
  formula  = "as_mort_prev_ratio_log ~ as_spend_prev_ratio_log + year_factor",
  data     = df_sud,
  is_final = FALSE
)

register_model(
  family   = "between_yfe",
  spec     = "bivariate_daly",
  formula  = "as_daly_prev_ratio_log ~ as_spend_prev_ratio_log + year_factor",
  data     = df_sud,
  is_final = FALSE
)

register_model(
  family   = "between_yfe",
  spec     = "bivariate_mort_tx_prev",
  formula  = "as_mort_prev_ratio_log ~ spend_tx_prev_ratio_log + year_factor",
  data     = df_sud,
  is_final = FALSE
)

register_model(
  family   = "between_yfe",
  spec     = "bivariate_daly_tx_prev",
  formula  = "as_daly_prev_ratio_log ~ spend_tx_prev_ratio_log + year_factor",
  data     = df_sud,
  is_final = FALSE
)

# ==============================================================
# TOC MODELS (by type of care)
# ==============================================================

register_model(
  family   = "between_yfe",
  spec     = "primary_by_toc_mort",
  formula  = "as_mort_prev_ratio_log ~
                spend_AM_prev_ratio_log +
                spend_ED_prev_ratio_log +
                spend_HH_prev_ratio_log +
                spend_IP_prev_ratio_log +
                spend_NF_prev_ratio_log +
                spend_RX_prev_ratio_log +
                year_factor +
                unemployment_rate +
                race_prop_BLCK + log_incidence_rates +
                race_prop_HISP +
                log_prop_homeless",
  data     = df_sud,
  is_final = TRUE
)

register_model(
  family   = "between_yfe",
  spec     = "primary_by_toc_daly",
  formula  = "as_daly_prev_ratio_log ~
                spend_AM_prev_ratio_log +
                spend_ED_prev_ratio_log +
                spend_HH_prev_ratio_log +
                spend_IP_prev_ratio_log +
                spend_NF_prev_ratio_log +
                spend_RX_prev_ratio_log +
                year_factor +
                unemployment_rate +
                race_prop_BLCK + log_incidence_rates +
                race_prop_HISP +
                log_prop_homeless",
  data     = df_sud,
  is_final = TRUE
)

# ==============================================================
# TX_PREV MODELS (treatment spending predictor)
# ==============================================================

register_model(
  family   = "between_yfe",
  spec     = "primary_tx_prev",
  formula  = "as_mort_prev_ratio_log ~
                spend_tx_prev_ratio_log + year_factor +
                unemployment_rate +
                race_prop_BLCK + log_incidence_rates +
                race_prop_HISP +
                log_prop_homeless",
  data     = df_sud,
  is_final = TRUE
)

register_model(
  family   = "between_yfe",
  spec     = "primary_tx_prev_daly",
  formula  = "as_daly_prev_ratio_log ~
                spend_tx_prev_ratio_log + year_factor +
                unemployment_rate +
                race_prop_BLCK + log_incidence_rates +
                race_prop_HISP +
                log_prop_homeless",
  data     = df_sud,
  is_final = TRUE
)

# ==============================================================
# INTERACTION MODEL
# ==============================================================

register_model(
  family   = "between_yfe",
  spec     = "interact_highprev",
  formula  = "as_mort_prev_ratio_log ~
                as_spend_prev_ratio_log * high_sud_prev_B_f +
                year_factor +
                unemployment_rate +
                race_prop_BLCK + log_incidence_rates +
                race_prop_HISP +
                log_prop_homeless",
  data     = df_sud,
  is_final = TRUE
)

register_model(
  family   = "between_yfe",
  spec     = "interact_highprev_daly",
  formula  = "as_daly_prev_ratio_log ~
                as_spend_prev_ratio_log * high_sud_prev_B_f +
                year_factor +
                unemployment_rate +
                race_prop_BLCK + log_incidence_rates +
                race_prop_HISP +
                log_prop_homeless",
  data     = df_sud,
  is_final = TRUE
)

# ---- SAFEGUARD: verify no _B terms leaked into between_yfe formulas ----
yfe_ids <- grep("between_yfe", names(list_models), value = TRUE)
for (mid in yfe_ids) {
  formula_str <- deparse(formula(list_models[[mid]]), width.cutoff = 500)
  b_terms <- regmatches(formula_str,
                        gregexpr("[A-Za-z0-9_.]+_B(?!_f)\\b", formula_str, perl = TRUE))[[1]]
  if (length(b_terms) > 0) {
    stop(
      "SAFEGUARD VIOLATION in ", mid, ":\n",
      "  Found _B (state-mean) terms that should be time-varying: ",
      paste(b_terms, collapse = ", "), "\n",
      "  between_yfe must use time-varying predictors (no _B).",
      call. = FALSE
    )
  }
}
cat("SAFEGUARD PASSED: no _B terms found in between_yfe formulas.\n")
##================================================================
## 8.  EXTRACT RESULTS  (cluster-robust SEs) & SAVE
##================================================================

# ---- Cluster-robust extraction helper ----
#
# For between_true and subsample_*_between (~51 obs): HC2 robust SEs.
# For all panel models: cluster-robust SEs on location_id.

extract_clustered <- function(model, model_id, model_data_df) {
  # Determine if cross-sectional (HC2) or panel (cluster-robust)
  is_cross_sectional <- grepl("between_true", model_id) |
    grepl("subsample__.*between", model_id)
  
  if (is_cross_sectional) {
    # Cross-sectional: HC2 heteroskedasticity-robust SEs
    tryCatch({
      vcov_hc <- sandwich::vcovHC(model, type = "HC2")
      ct <- lmtest::coeftest(model, vcov. = vcov_hc)
      tibble(
        term      = rownames(ct),
        estimate  = ct[, 1],
        std.error = ct[, 2],
        statistic = ct[, 3],
        p.value   = ct[, 4],
        model_id  = model_id
      )
    }, error = function(e) {
      broom::tidy(model) %>% mutate(model_id = model_id)
    })
  } else {
    # Panel models: cluster-robust SEs on state
    tryCatch({
      cluster_var <- model_data_df$location_id
      vcov_cr <- vcovCR(model, cluster = cluster_var, type = "CR2")
      ct      <- coef_test(model, vcov = vcov_cr, test = "Satterthwaite")
      tibble(
        term      = rownames(ct),
        estimate  = ct$beta,
        std.error = ct$SE,
        statistic = ct$tstat,
        p.value   = ct$p_Satt,
        model_id  = model_id
      )
    }, error = function(e) {
      broom::tidy(model) %>% mutate(model_id = model_id)
    })
  }
}

# ---- Coefficient table ----
coef_tbl <- map_dfr(names(list_models), function(nm) {
  extract_clustered(list_models[[nm]], nm, model_data[[nm]])
})

coef_tbl <- coef_tbl %>%
  mutate(
    signif_stars = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      p.value < 0.1   ~ ".",
      TRUE            ~ ""
    ),
    signif_label = case_when(
      p.value < 0.001 ~ "p < 0.001",
      p.value < 0.01  ~ "p < 0.01",
      p.value < 0.05  ~ "p < 0.05",
      p.value < 0.1   ~ "p < 0.1",
      TRUE            ~ "not significant"
    )
  )

# Parse model_id into components
coef_tbl <- coef_tbl %>%
  mutate(
    acause = "sud",
    family = str_match(model_id, "^sud__([^_]+(?:_[^_]+)?)__")[, 2],
    model_name = if_else(
      str_detect(model_id, "^sud__"),
      str_replace(model_id, "^sud__[^_]+(?:_[^_]+)?__", ""),
      NA_character_
    )
  )


# ---- Metrics table ----
metrics_tbl <- imap_dfr(list_models, function(model, model_id) {
  g <- broom::glance(model)
  tibble(
    model_id = model_id,
    n        = g$nobs,
    r2       = g$r.squared,
    adj_r2   = g$adj.r.squared,
    aic      = AIC(model),
    bic      = BIC(model),
    sigma    = g$sigma
  )
})

# ---- Single combined table ----
regression_results <- coef_tbl %>%
  left_join(metrics_tbl, by = "model_id") %>%
  select(model_id, acause, family, model_name, term,
         estimate, signif_stars, std.error, statistic, p.value,
         signif_label,
         n, r2, adj_r2, aic, bic, sigma)

write.csv(regression_results,
          file.path(dir_output, "regression_results_sud_combined.csv"),
          row.names = FALSE)

# ---- Separate CSVs (backward compat) ----
write.csv(
  coef_tbl %>% select(model_id, acause, family, model_name, term,
                      estimate, signif_stars, std.error, statistic, p.value,
                      signif_label),
  file.path(dir_output, "regression_results_sud_coefficients.csv"),
  row.names = FALSE
)

write.csv(
  metrics_tbl,
  file.path(dir_output, "regression_results_sud_metrics.csv"),
  row.names = FALSE
)


# ---- Spending coefficient summary (quick-reference) ----
spending_summary <- regression_results %>%
  dplyr::filter(
    grepl("spend_prev_ratio|d_spend", term) &
      !grepl("_sq$|over_p75", term)
  ) %>%
  select(model_id, family, model_name, term, estimate, signif_stars,
         std.error, p.value, signif_label, n, adj_r2) %>%
  mutate(
    estimate  = round(estimate, 5),
    std.error = round(std.error, 5),
    p.value   = round(p.value, 4)
  ) %>%
  arrange(estimate)

write.csv(spending_summary,
          file.path(dir_output, "spending_coefficients_summary.csv"),
          row.names = FALSE)

cat("\n=== SPENDING COEFFICIENT COMPARISON (sorted by estimate) ===\n")
print(as.data.frame(spending_summary), row.names = FALSE)


# ---- Model registry ----
write.csv(model_registry,
          file.path(dir_output, "model_registry.csv"),
          row.names = FALSE)

####Check below may delete if not needed Apr 29th
# ---- Per-category spending slopes from interaction model ----
build_cat_slopes <- function(model_id_prefix) {
  rows <- coef_tbl %>%
    dplyr::filter(model_id == paste0("sud__between_yfe__", model_id_prefix))
  if (nrow(rows) == 0) return(NULL)
  main <- rows %>% dplyr::filter(term == "as_spend_prev_ratio_log") %>%
    dplyr::pull(estimate)
  int_terms <- rows %>%
    dplyr::filter(grepl("^as_spend_prev_ratio_log:dom_cat_f", term))
  tibble::tibble(
    source     = model_id_prefix,
    category   = c("Cat1 (ref)",
                   sub("^as_spend_prev_ratio_log:dom_cat_f", "",
                       int_terms$term)),
    slope      = c(main, main + int_terms$estimate),
    se_main_or_diff = c(rows %>%
                          dplyr::filter(term == "as_spend_prev_ratio_log") %>%
                          dplyr::pull(std.error),
                        int_terms$std.error),
    p_diff_vs_cat1  = c(NA, int_terms$p.value)
  )
}

cat_slopes <- dplyr::bind_rows(
  build_cat_slopes("primary_catinteract_mort"),
  build_cat_slopes("primary_catinteract_daly"),
  build_cat_slopes("bivariate_catinteract_mort"),
  build_cat_slopes("bivariate_catinteract_daly")
)

write.csv(cat_slopes,
          file.path(dir_output, "spending_slopes_by_category.csv"),
          row.names = FALSE)

cat("\n=== SPENDING SLOPE BY CATEGORY (from interaction models) ===\n")
print(as.data.frame(cat_slopes))

##================================================================
## END OF PIPELINE
##================================================================

cat("\n==============================\n")
cat("Pipeline complete.\n")
cat("Models fitted:", length(list_models), "\n")
cat("  between_true:", sum(grepl("between_true", names(list_models))), "\n")
cat("  between_yfe:",  sum(grepl("between_yfe",  names(list_models))), "\n")
cat("  mundlak:",      sum(grepl("mundlak",      names(list_models))), "\n")
cat("  lag:",          sum(grepl("^sud__lag",     names(list_models))), "\n")
cat("  dose:",         sum(grepl("dose",          names(list_models))), "\n")
cat("  first_diff:",   sum(grepl("first_diff",    names(list_models))), "\n")
cat("  subsample:",    sum(grepl("subsample",     names(list_models))), "\n")
cat("Output directory:", dir_output, "\n")
cat("==============================\n")




# ---- 4b_alt. Merge decomposition-based spending intensity into between dataset ----
# ---- 4b_alt. Merge decomposition-based spending intensity into between dataset ----
fp_decomp_state <- "/ihme/homes/idrisov/aim_outputs/Aim2/D_tables_figures/20260316/T6_HIV_decomp_by_state.csv"


df_decomp_state <- read.csv(fp_decomp_state, stringsAsFactors = FALSE) %>%
  dplyr::select(
    location_id, location_name,
    pop_size_effect,
    prevalence_rate_effect,
    case_composition_effect,
    spend_intensity_effect,
    delta_spend
  ) %>%
  mutate(
    spend_intensity_effect_signed_log =
      sign(spend_intensity_effect) * log(abs(spend_intensity_effect) + 1),
    prevalence_rate_effect_signed_log =
      sign(prevalence_rate_effect) * log(abs(prevalence_rate_effect) + 1),
    case_composition_effect_signed_log =
      sign(case_composition_effect) * log(abs(case_composition_effect) + 1),
    pop_size_effect_signed_log =
      sign(pop_size_effect) * log(abs(pop_size_effect) + 1)
  )

df_between <- df_between %>%
  left_join(df_decomp_state, by = c("location_id", "location_name"))

cat("\nMerged decomposition variables into df_between\n")
cat("Rows in df_between:", nrow(df_between), "\n")
cat("Non-missing spend_intensity_effect:", sum(!is.na(df_between$spend_intensity_effect)), "\n")
cat("Non-missing prevalence_rate_effect:", sum(!is.na(df_between$prevalence_rate_effect)), "\n")

print(summary(df_between$spend_intensity_effect))
print(summary(df_between$spend_intensity_effect_signed_log))





cat("\n================ CORRELATIONS ================\n")

vars_check <- df_between %>%
  select(
    as_mort_prev_ratio_log,
    as_spend_prev_ratio_log,
    spend_intensity_effect,
    spend_intensity_effect_signed_log,
    prevalence_rate_effect,
    unemployment_rate,
    log_incidence_rates_B
  )

corr_mat <- cor(vars_check, use = "complete.obs")

print(round(corr_mat, 3))



cat("\n================ SIMPLE REGRESSION (NO COVARIATES) ================\n")

model_simple <- lm(
  as_mort_prev_ratio_log ~ spend_intensity_effect_signed_log,
  data = df_between
)

print(summary(model_simple))


cat("\n================ COMPARISON WITH ORIGINAL SPENDING ================\n")

model_original <- lm(
  as_mort_prev_ratio_log ~ as_spend_prev_ratio_log,
  data = df_between
)

model_decomp <- lm(
  as_mort_prev_ratio_log ~ spend_intensity_effect_signed_log,
  data = df_between
)

cat("\n--- Original spending per case ---\n")
print(summary(model_original)$coefficients)

cat("\n--- Decomposition intensity ---\n")
print(summary(model_decomp)$coefficients)



cat("\n================ DECOMPOSITION-BASED REGRESSION ================\n")


df_between$spend_inten_per_case <- 
  df_between$spend_intensity_effect / df_between$prevalence_counts

df_between$spend_inten_per_case_log <- 
  sign(df_between$spend_inten_per_case) *
  log1p(abs(df_between$spend_inten_per_case))



model_decomp_main <- lm(
  as_mort_prev_ratio_log ~
    spend_inten_per_case_log +
    unemployment_rate +
    race_prop_BLCK +
    #log_incidence_rates_B +
    race_prop_HISP,
  data = df_between
)


# Print full summary
print(summary(model_decomp_main))


cat("\n================ ORIGINAL VS DECOMPOSITION =================\n")

model_original <- lm(
  as_mort_prev_ratio_log ~
    as_spend_prev_ratio_log +
    unemployment_rate +
    race_prop_BLCK +
    log_incidence_rates_B +
    race_prop_HISP,
  data = df_between
)

cat("\n--- ORIGINAL SPENDING ---\n")
print(summary(model_original)$coefficients)

cat("\n--- DECOMPOSITION SPENDING ---\n")
print(summary(model_decomp_main)$coefficients)

