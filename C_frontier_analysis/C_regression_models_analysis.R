##----------------------------------------------------------------
##' Title: C_regression_models_analysis.R
##'
##' Purpose: HIV Spending-Outcomes Regression Analysis for Aim 2B
##'          Committee-ready pipeline — theory-driven model set.
##'
##' Pipeline order:
##'   1. Setup & Data Loading
##'   2. Log-Transform Outcome / Exposure
##'   3. Create ALL Analysis Variables
##'        3a. Mundlak B/W decomposition
##'        3b. Log transforms of key covariates
##'        3c. Indicators (high_incidence_q4_B, high_hiv_prev_B)
##'        3d. Lag variables (t-1, t-2)
##'        3e. Dose-response variables (quadratic, threshold)
##'   4. Save Analysis Dataset
##'   5. Summary Table (internal Table 1 + skewness diagnostics)
##'   6. Diagnostics (NO regressions yet)
##'        6a. Full correlation matrix (model-ready vars)
##'        6b. Confounder screening (panel + state-means)
##'        6c. Variance decomposition (% between vs within)
##'   7. Fit Regression Models (4 families)
##'        A. Between_key  (main paper family, 6 specs)
##'        B. Mundlak      (within-between CRE, 4 specs)
##'        C. Lag           (temporal precedence, 2 specs)
##'        D. Dose-response (non-linearity, 2 specs)
##'   8. Extract Results (cluster-robust SEs) & Save
##'
##' IMPORTANT: NO prevalence / high_prev variables in any specification
##'            EXCEPT the explicit interaction model (between__interact_highprev).
##'            Prevalence appears in both Y and X denominators → bias risk.
##'
##' Data processing (RW, CDC, covariates) lives in C_model_data_prep.R.
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

input_date  <- "20260216"
dir_input   <- file.path(h, "aim_outputs/Aim2/C_frontier_analysis", input_date)

output_date <- format(Sys.time(), "%Y%m%d")
dir_output  <- file.path(h, "aim_outputs/Aim2/C_frontier_analysis", output_date, "analysis")
if (!dir.exists(dir_output)) dir.create(dir_output, recursive = TRUE)

# ---------- load ----------
df_as <- read.csv(
  file.path(dir_input, "df_as_processed_rw_gbd.csv"),
  stringsAsFactors = FALSE
)

df_hiv <- df_as %>%
  filter(acause == "hiv") %>%
  filter(!is.na(as_mort_prev_ratio) & !is.na(rw_dex_hiv_prev_ratio))


##================================================================
## 2.  LOG-TRANSFORM OUTCOME & EXPOSURE
##================================================================
df_hiv <- df_hiv %>%
  mutate(
    as_mort_prev_ratio_log    = log(as_mort_prev_ratio),
    as_daly_prev_ratio_log    = log(as_daly_prev_ratio),
    as_yll_prev_ratio_log     = log(as_yll_prev_ratio),
    as_yld_prev_ratio_log     = if_else(as_yld_prev_ratio > 0,
                                        log(as_yld_prev_ratio), NA_real_),
    as_spend_prev_ratio_log   = log(as_spend_prev_ratio),
    rw_dex_hiv_prev_ratio_log = log(rw_dex_hiv_prev_ratio),
    rw_hiv_prev_ratio_log     = if_else(!is.na(rw_hiv_prev_ratio) & rw_hiv_prev_ratio > 0,
                                        log(rw_hiv_prev_ratio), NA_real_)
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

# Variables needing B/W decomposition — covers all model families.
# Only include columns that actually exist in df_hiv.
mundlak_vars_requested <- c(
  "rw_dex_hiv_prev_ratio_log",
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
mundlak_vars <- intersect(mundlak_vars_requested, names(df_hiv))

df_hiv <- make_mundlak_vars(df_hiv, mundlak_vars, "location_id", midpoint = 2014)


# ---- 3b. Log transforms of key covariates ----
# Incidence is always logged (high skewness).
# Other covariates logged where used in formulas (BMI, prev_diabetes,
# prop_homeless, ldi_pc).  Creating pre-computed columns avoids
# inline log() calls and ensures the analysis dataset is self-contained.

df_hiv <- df_hiv %>%
  mutate(
    log_incidence_rates_B   = log(incidence_rates_B),
    log_bmi_B               = log(bmi_B),
    log_prev_diabetes_B     = log(prev_diabetes_B),
    log_prop_homeless        = log(prop_homeless),
    log_prop_homeless_B      = log(prop_homeless_B),
    log_ldi_pc               = log(ldi_pc),
    log_ldi_pc_B             = log(ldi_pc_B),
    # Within deviations of logged covariates (for Mundlak on log scale)
    log_prop_homeless_W      = log_prop_homeless - log_prop_homeless_B,
    log_incidence_rates      = log(incidence_rates),
    log_incidence_rates_W    = log_incidence_rates - log_incidence_rates_B
  )


# ---- 3c. Indicators ----

# High-incidence binary: top quartile of state-mean incidence.
state_inc_q75 <- quantile(df_hiv$incidence_rates_B, 0.75, na.rm = TRUE)
df_hiv <- df_hiv %>%
  mutate(high_incidence_q4_B = as.integer(incidence_rates_B >= state_inc_q75))

# High-prevalence binary: median split of state-mean prevalence.
# Used ONLY in the interaction model (between__interact_highprev).
# Compute from prevalence_counts state mean (always available) if
# prevalence_rates_B was not created (e.g., raw input lacks prevalence_rates).
if (!"high_hiv_prev_B" %in% names(df_hiv)) {
  if ("prevalence_rates_B" %in% names(df_hiv)) {
    prev_median_B <- quantile(df_hiv$prevalence_rates_B, 0.50, na.rm = TRUE)
    df_hiv <- df_hiv %>%
      mutate(high_hiv_prev_B = as.integer(prevalence_rates_B >= prev_median_B))
  } else {
    # Fallback: use state-mean prevalence counts
    state_prev <- df_hiv %>%
      group_by(location_id) %>%
      summarise(prev_count_mean = mean(prevalence_counts, na.rm = TRUE),
                .groups = "drop")
    prev_median <- median(state_prev$prev_count_mean, na.rm = TRUE)
    df_hiv <- df_hiv %>%
      left_join(state_prev, by = "location_id") %>%
      mutate(high_hiv_prev_B = as.integer(prev_count_mean >= prev_median)) %>%
      select(-prev_count_mean)
  }
}

# Factor version for readable interaction labels
df_hiv <- df_hiv %>%
  mutate(
    high_hiv_prev_B_f = factor(
      high_hiv_prev_B,
      levels = c(0, 1),
      labels = c("Lower prevalence", "Higher prevalence")
    )
  )


# ---- 3d. Lag variables ----
df_hiv <- df_hiv %>%
  arrange(location_id, year_id) %>%
  group_by(location_id) %>%
  mutate(
    rw_dex_hiv_prev_ratio_log_l1 = dplyr::lag(rw_dex_hiv_prev_ratio_log, 1),
    rw_dex_hiv_prev_ratio_log_l2 = dplyr::lag(rw_dex_hiv_prev_ratio_log, 2)
  ) %>%
  ungroup()


# ---- 3e. Dose-response variables ----
knot_p75 <- quantile(df_hiv$rw_dex_hiv_prev_ratio_log, 0.75, na.rm = TRUE)
df_hiv <- df_hiv %>%
  mutate(
    rw_dex_hiv_prev_ratio_log_sq = rw_dex_hiv_prev_ratio_log^2,
    over_p75 = pmax(0, rw_dex_hiv_prev_ratio_log - knot_p75)
  )


##================================================================
## 4.  SAVE ANALYSIS DATASET
##================================================================
write.csv(
  df_hiv,
  file.path(dir_output, "df_hiv_analysis.csv"),
  row.names = FALSE
)


##================================================================
## 5.  SUMMARY TABLE  (internal Table 1 + skewness diagnostics)
##================================================================

summary_vars <- c(
  # Outcome & exposure (raw + log)
  "as_mort_prev_ratio",    "as_mort_prev_ratio_log",
  "rw_dex_hiv_prev_ratio", "rw_dex_hiv_prev_ratio_log",
  "rw_dex_hiv_prev_ratio_log_B",
  # Key covariates used in models
  "race_prop_BLCK",  "race_prop_BLCK_B",
  "race_prop_HISP",
  "incidence_rates", "incidence_rates_B", "log_incidence_rates_B",
  "prop_homeless",   "log_prop_homeless_B",
  "prev_diabetes",   "log_prev_diabetes_B",
  "bmi",             "log_bmi_B",
  "obesity",         "obesity_B",
  "ldi_pc",          "log_ldi_pc",
  "unemployment_rate",
  "edu_yrs",         "edu_yrs_B",
  "aca_implemented_status",
  # Contextual
  "population", "prevalence_counts", "mortality_counts",
  "spend_all", "ryan_white_funding_final"
)

# Keep only columns that actually exist
summary_vars <- intersect(summary_vars, names(df_hiv))

summary_tbl <- df_hiv %>%
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
      skewness > 1   ~ "yes — strong right skew",
      skewness > 0.5 ~ "maybe — moderate skew",
      skewness < -1  ~ "yes — strong left skew",
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
  "rw_dex_hiv_prev_ratio_log_B", "rw_dex_hiv_prev_ratio_log_W",
  "year_centered",
  "race_prop_BLCK_B", "race_prop_HISP",
  "log_incidence_rates_B", "incidence_rates_W",
  "log_bmi_B", "bmi_W",
  "obesity_B",
  "log_prev_diabetes_B",
  "log_prop_homeless_B",
  "log_ldi_pc", "log_ldi_pc_B",
  "unemployment_rate",
  "edu_yrs_B",
  "aca_implemented_status_B",
  "high_incidence_q4_B", "high_hiv_prev_B"
)
corr_vars <- intersect(corr_vars, names(df_hiv))

corr_mat <- df_hiv %>%
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
  
  shortlisted <- res %>% filter(pass)
  
  if (!is.null(dir_output)) {
    readr::write_csv(res,         file.path(dir_output, paste0(file_stub, "_ALL.csv")))
    readr::write_csv(shortlisted, file.path(dir_output, paste0(file_stub, "_SHORTLIST.csv")))
  }
  list(all = res, shortlist = shortlisted)
}

# Exclusion lists (mechanical / denominator overlap)
exclude_base <- c("cause_id", "year_id", "location_id", "year_centered")
exclude_mechanical <- c(
  "rw_dex_hiv_prev_ratio", "rw_dex_hiv_prev_ratio_log",
  "rw_hiv_prev_ratio", "rw_hiv_prev_ratio_log",
  "ryan_white_funding_final", "spend_all",
  "as_spend_prev_ratio", "as_spend_prev_ratio_log",
  "spend_mdcd", "spend_mdcr", "spend_oop", "spend_priv",
  "as_mort_prev_ratio", "as_mort_prev_ratio_log",
  "mortality_rates", "mortality_counts",
  "prevalence_counts", "hiv_prevalence_counts",
  "daly_rates", "yll_rates", "yld_rates",
  "daly_counts", "yll_counts", "yld_counts", "incidence_counts",
  "as_daly_prev_ratio", "as_yll_prev_ratio", "as_yld_prev_ratio",
  "as_daly_prev_ratio_log", "as_yll_prev_ratio_log", "as_yld_prev_ratio_log",
  "population", "variance",
  "prevalence_rates", "high_hiv_prev", "high_sud_prev", "sud_prevalence_counts"
)

# Panel-level screening
screen_confounders(
  df           = df_hiv,
  exposure     = "rw_dex_hiv_prev_ratio_log",
  outcome      = "as_mort_prev_ratio_log",
  exclude_vars = c(exclude_base, exclude_mechanical),
  r_thresh     = 0.20,
  dir_output   = dir_output,
  file_stub    = "confounder_screen_PANEL"
)

# State-means screening
df_state_means <- df_hiv %>%
  group_by(location_id, location_name) %>%
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)),
            .groups = "drop")

screen_confounders(
  df           = df_state_means,
  exposure     = "rw_dex_hiv_prev_ratio_log",
  outcome      = "as_mort_prev_ratio_log",
  exclude_vars = c(exclude_base, exclude_mechanical),
  r_thresh     = 0.20,
  dir_output   = dir_output,
  file_stub    = "confounder_screen_STATE"
)


# ---- 6c. Variance decomposition ----

calc_variance_decomp <- function(df, var_name) {
  var_B <- paste0(var_name, "_B")
  total_var   <- var(df[[var_name]], na.rm = TRUE)
  between_var <- var(df[[var_B]],    na.rm = TRUE)
  within_var  <- total_var - between_var
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
  calc_variance_decomp(df_hiv, "rw_dex_hiv_prev_ratio_log"),
  calc_variance_decomp(df_hiv, "race_prop_BLCK"),
  calc_variance_decomp(df_hiv, "race_prop_HISP"),
  calc_variance_decomp(df_hiv, "incidence_rates"),
  calc_variance_decomp(df_hiv, "obesity"),
  calc_variance_decomp(df_hiv, "bmi"),
  calc_variance_decomp(df_hiv, "prev_diabetes"),
  calc_variance_decomp(df_hiv, "edu_yrs"),
  calc_variance_decomp(df_hiv, "prop_homeless"),
  calc_variance_decomp(df_hiv, "ldi_pc"),
  calc_variance_decomp(df_hiv, "unemployment_rate"),
  calc_variance_decomp(df_hiv, "aca_implemented_status")
)

write.csv(variance_decomp,
          file.path(dir_output, "variance_decomposition.csv"),
          row.names = FALSE)


##================================================================
## 7.  FIT REGRESSION MODELS
##================================================================

# ---- Helper: fit and register a model ----
list_models <- list()
model_registry <- tibble(
  model_id = character(),
  family   = character(),
  spec     = character(),
  is_final = logical()
)

register_model <- function(family, spec, formula, data,
                           is_final = FALSE) {
  model_id <- paste0("hiv__", family, "__", spec)
  fit <- lm(as.formula(formula), data = data)
  list_models[[model_id]] <<- fit
  model_registry <<- bind_rows(
    model_registry,
    tibble(model_id = model_id, family = family,
           spec = spec, is_final = is_final)
  )
  invisible(fit)
}


# ==============================================================
# A)  BETWEEN_KEY FAMILY  (main paper family — 6 specifications)
#
#     Cross-sectional (between-state) models using state-mean
#     exposure (_B).  This is the core scientific contribution.
#
#     Primary model: spending_B + year + race_BLCK_B +
#                    log(incidence_B) + race_HISP
# ==============================================================

# A1. PRIMARY — no health-environment variable.
#     Clean structural + severity adjustment.
register_model(
  family   = "between",
  spec     = "primary",
  formula  = "as_mort_prev_ratio_log ~
                rw_dex_hiv_prev_ratio_log_B + year_centered +
                race_prop_BLCK_B + log_incidence_rates_B + race_prop_HISP",
  data     = df_hiv,
  is_final = TRUE
)

# A2. SECONDARY — adds homelessness (vulnerability marker).
register_model(
  family   = "between",
  spec     = "secondary_homeless",
  formula  = "as_mort_prev_ratio_log ~
                rw_dex_hiv_prev_ratio_log_B + year_centered +
                race_prop_BLCK_B + log_incidence_rates_B + race_prop_HISP +
                log_prop_homeless_B",
  data     = df_hiv,
  is_final = TRUE
)

# A3. ROBUSTNESS — diabetes as health-environment measure.
register_model(
  family   = "between",
  spec     = "robustness_diabetes",
  formula  = "as_mort_prev_ratio_log ~
                rw_dex_hiv_prev_ratio_log_B + year_centered +
                race_prop_BLCK_B + log_incidence_rates_B + race_prop_HISP +
                log_prev_diabetes_B",
  data     = df_hiv,
  is_final = FALSE
)

# A4. INTERACTION — high-prevalence median split.
#     NOTE: This is the ONE model that uses a prevalence-derived
#     indicator.  Justified because the research question is whether
#     the spending-mortality association differs by HIV burden context.
register_model(
  family   = "between",
  spec     = "interact_highprev",
  formula  = "as_mort_prev_ratio_log ~
                rw_dex_hiv_prev_ratio_log_B * high_hiv_prev_B_f +
                year_centered +
                race_prop_BLCK_B + log_incidence_rates_B + race_prop_HISP",
  data     = df_hiv,
  is_final = TRUE
)

# A5. ROBUSTNESS — income (LDI per capita).
register_model(
  family   = "between",
  spec     = "robustness_ldi",
  formula  = "as_mort_prev_ratio_log ~
                rw_dex_hiv_prev_ratio_log_B + year_centered +
                race_prop_BLCK_B + log_incidence_rates_B + race_prop_HISP +
                log_ldi_pc_B",
  data     = df_hiv,
  is_final = FALSE
)

# A6. ROBUSTNESS — unemployment rate.
register_model(
  family   = "between",
  spec     = "robustness_unemployment",
  formula  = "as_mort_prev_ratio_log ~
                rw_dex_hiv_prev_ratio_log_B + year_centered +
                race_prop_BLCK_B + log_incidence_rates_B + race_prop_HISP +
                unemployment_rate",
  data     = df_hiv,
  is_final = FALSE
)


# ==============================================================
# B)  MUNDLAK FAMILY  (within-between CRE — 4 specifications)
#
#     Correlated random effects models decompose spending into:
#       _B = between-state mean (long-run cross-sectional)
#       _W = within-state deviation (temporal/reactive)
#
#     Time-invariant covariates enter as _B only.
#     Time-varying covariates enter as _B and _W.
# ==============================================================

# B1. PRIMARY EQUIVALENT — same confounder set as between__primary,
#     but spending + incidence decomposed into _B / _W.
#     race_prop_BLCK, race_prop_HISP: ~time-invariant → _B only.
register_model(
  family   = "mundlak",
  spec     = "primary_equivalent",
  formula  = "as_mort_prev_ratio_log ~
                rw_dex_hiv_prev_ratio_log_B + rw_dex_hiv_prev_ratio_log_W +
                year_centered +
                race_prop_BLCK_B + log_incidence_rates_B + log_incidence_rates_W +
                race_prop_HISP",
  data     = df_hiv,
  is_final = TRUE
)

# B2. SECONDARY EQUIVALENT — matches between__secondary_homeless.
register_model(
  family   = "mundlak",
  spec     = "secondary_equivalent",
  formula  = "as_mort_prev_ratio_log ~
                rw_dex_hiv_prev_ratio_log_B + rw_dex_hiv_prev_ratio_log_W +
                year_centered +
                race_prop_BLCK_B + log_incidence_rates_B + log_incidence_rates_W +
                race_prop_HISP +
                log_prop_homeless_B + log_prop_homeless_W",
  data     = df_hiv,
  is_final = FALSE
)

# B3. BASELINE — original Mundlak specification with BMI + ACA.
register_model(
  family   = "mundlak",
  spec     = "baseline",
  formula  = "as_mort_prev_ratio_log ~
                rw_dex_hiv_prev_ratio_log_B + rw_dex_hiv_prev_ratio_log_W +
                year_centered +
                race_prop_BLCK_B +
                incidence_rates_B + incidence_rates_W +
                bmi_B + bmi_W +
                aca_implemented_status_B",
  data     = df_hiv,
  is_final = FALSE
)

# B4. ALT HEALTH — swap BMI for diabetes prevalence.
register_model(
  family   = "mundlak",
  spec     = "alt_health",
  formula  = "as_mort_prev_ratio_log ~
                rw_dex_hiv_prev_ratio_log_B + rw_dex_hiv_prev_ratio_log_W +
                year_centered +
                race_prop_BLCK_B +
                incidence_rates_B + incidence_rates_W +
                prev_diabetes_B + prev_diabetes_W +
                aca_implemented_status_B",
  data     = df_hiv,
  is_final = FALSE
)


# ==============================================================
# C)  LAG FAMILY  (temporal precedence — 2 specifications)
#
#     Uses the primary covariate set.  Tests whether spending at
#     t-1 / t-2 predicts mortality at t (helps rule out reverse
#     causation; accounts for programme ramp-up time).
# ==============================================================

# C1. LAG-1 — spending at t-1.
register_model(
  family   = "lag",
  spec     = "l1",
  formula  = "as_mort_prev_ratio_log ~
                rw_dex_hiv_prev_ratio_log_l1 + year_centered +
                race_prop_BLCK_B + log_incidence_rates_B + race_prop_HISP",
  data     = df_hiv,
  is_final = FALSE
)

# C2. DISTRIBUTED LAG — spending at t-1 and t-2.
register_model(
  family   = "lag",
  spec     = "distributed_l1_l2",
  formula  = "as_mort_prev_ratio_log ~
                rw_dex_hiv_prev_ratio_log_l1 +
                rw_dex_hiv_prev_ratio_log_l2 + year_centered +
                race_prop_BLCK_B + log_incidence_rates_B + race_prop_HISP",
  data     = df_hiv,
  is_final = FALSE
)


# ==============================================================
# D)  DOSE-RESPONSE FAMILY  (non-linearity — 2 specifications)
#
#     Tests whether the spending-mortality relationship has
#     diminishing returns or threshold effects.
#     Uses primary covariate set.
# ==============================================================

# D1. QUADRATIC — tests for curvature (diminishing returns).
register_model(
  family   = "dose",
  spec     = "quadratic",
  formula  = "as_mort_prev_ratio_log ~
                rw_dex_hiv_prev_ratio_log + rw_dex_hiv_prev_ratio_log_sq +
                year_centered +
                race_prop_BLCK_B + log_incidence_rates_B + race_prop_HISP",
  data     = df_hiv,
  is_final = FALSE
)

# D2. THRESHOLD — piecewise linear at 75th percentile.
register_model(
  family   = "dose",
  spec     = "threshold_p75",
  formula  = "as_mort_prev_ratio_log ~
                rw_dex_hiv_prev_ratio_log + over_p75 +
                year_centered +
                race_prop_BLCK_B + log_incidence_rates_B + race_prop_HISP",
  data     = df_hiv,
  is_final = FALSE
)


##================================================================
## 8.  EXTRACT RESULTS  (cluster-robust SEs) & SAVE
##================================================================

# ---- Cluster-robust extraction helper ----
extract_clustered <- function(model, model_id, cluster_var) {
  tryCatch({
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

# ---- Coefficient table ----
coef_tbl <- map_dfr(names(list_models), function(nm) {
  extract_clustered(list_models[[nm]], nm, df_hiv$location_id)
})

# Parse model_id → attach signif labels
coef_tbl <- coef_tbl %>%
  mutate(
    signif_label = case_when(
      p.value < 0.001 ~ "p < 0.001",
      p.value < 0.01  ~ "p < 0.01",
      p.value < 0.05  ~ "p < 0.05",
      p.value < 0.1   ~ "p < 0.1",
      TRUE            ~ "not significant"
    )
  )

# ---- Metrics table (for joining) ----
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

# ---- Single combined table: coefficients + metrics per row ----
regression_results <- coef_tbl %>%
  left_join(metrics_tbl, by = "model_id") %>%
  select(model_id, term, estimate, std.error, statistic, p.value,
         signif_label, n, r2, adj_r2, aic, bic, sigma)

write.csv(regression_results,
          file.path(dir_output, "regression_results_hiv_combined.csv"),
          row.names = FALSE)

# ---- Spending coefficient summary (quick-reference) ----
spending_summary <- regression_results %>%
  filter(grepl("rw_dex_hiv_prev_ratio", term)) %>%
  select(model_id, term, estimate, std.error, p.value, signif_label,
         n, adj_r2) %>%
  mutate(
    estimate  = round(estimate, 5),
    std.error = round(std.error, 5),
    p.value   = round(p.value, 4)
  )

write.csv(spending_summary,
          file.path(dir_output, "spending_coefficients_summary.csv"),
          row.names = FALSE)

##================================================================
## END OF PIPELINE
##================================================================

colnames(df_hiv)