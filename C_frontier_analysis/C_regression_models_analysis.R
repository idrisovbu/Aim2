##----------------------------------------------------------------
##' Title: C_regression_models_analysis.R
##'
##' Purpose: HIV Spending-Outcomes Regression Analysis for Aim 2B
##'          Publishable, logic-driven model set — NOT optimised for fit.
##'          Model selection is driven by theory and committee guidance.
##'
##' Structure:
##'   1. Setup & Data Loading
##'   2. Correlation Screening (diagnostic / documentation only)
##'   3. Mundlak Between/Within Variables + Derived Indicators
##'   4. Variance Decomposition
##'   5. Regression Models
##'        A. Mundlak (within–between) — 3 variants
##'        B. Between_race             — 2 variants
##'        C. Between_key_confounders  — 4 variants
##'        D. Between_extended         — 2 variants
##'        E. Lag (temporal)           — 2 models
##'        F. Dose-response            — 2 models
##'   6. Extract Results (cluster-robust SEs) & Save
##'   7. Console Diagnostics
##'
##' Note: Data processing (RW, CDC, covariates) lives in C_model_data_prep.R.
##'       This script consumes the processed output.
##'
##' IMPORTANT: NO prevalence or high-prevalence variables in any
##'            specification (avoids denominator bias — prevalence
##'            already appears in both the outcome and the exposure).
##----------------------------------------------------------------

##================================================================
## 1.  SETUP & DATA LOADING
##================================================================
rm(list = ls())

pacman::p_load(
  data.table,
  tidyverse,
  glue,
  broom,
  lmtest,
  sandwich,
  clubSandwich,
  corrplot,
  Hmisc,
  splines
)

# ---------- paths ----------
if (Sys.info()["sysname"] == "Linux") {
  h <- paste0("/ihme/homes/", Sys.info()[7], "/")
} else if (Sys.info()["sysname"] == "Darwin") {
  h <- paste0("/Volumes/", Sys.info()[7], "/")
} else {
  h <- "H:/"
}

input_date  <- "20260216"
dir_input   <- file.path(h, "aim_outputs/Aim2/C_frontier_analysis", input_date)

output_date <- format(Sys.time(), "%Y%m%d")
dir_output  <- file.path(h, "aim_outputs/Aim2/C_frontier_analysis", output_date, "analysis")
if (!dir.exists(dir_output)) dir.create(dir_output, recursive = TRUE)

# ---------- load ----------
cat("=== 1. LOADING DATA ===\n")
df_as <- read.csv(
  file.path(dir_input, "df_as_processed_rw_gbd.csv"),
  stringsAsFactors = FALSE
)

df_hiv <- df_as %>%
  filter(acause == "hiv") %>%
  filter(!is.na(as_mort_prev_ratio) & !is.na(rw_dex_hiv_prev_ratio))

cat(sprintf("HIV observations: %d (51 states x %d years)\n",
            nrow(df_hiv),
            n_distinct(df_hiv$year_id)))
cat(sprintf("Years: %d - %d\n\n", min(df_hiv$year_id), max(df_hiv$year_id)))

# ---------- log-transform outcome & exposure ----------
# Both sides logged for elasticity interpretation
df_hiv <- df_hiv %>%
  mutate(
    as_mort_prev_ratio_log       = log(as_mort_prev_ratio),
    rw_dex_hiv_prev_ratio_log    = log(rw_dex_hiv_prev_ratio),
    as_yll_prev_ratio_log        = log(as_yll_prev_ratio),
    as_daly_prev_ratio_log       = log(as_daly_prev_ratio),
    as_yld_prev_ratio_log        = if_else(as_yld_prev_ratio > 0,
                                           log(as_yld_prev_ratio), NA_real_),
    as_spend_prev_ratio_log      = log(as_spend_prev_ratio),
    rw_hiv_prev_ratio_log        = if_else(!is.na(rw_hiv_prev_ratio) & rw_hiv_prev_ratio > 0,
                                           log(rw_hiv_prev_ratio), NA_real_)
  )


##================================================================
## 2.  CORRELATION SCREENING  (documentation / confounder plausibility)
##================================================================
cat("=== 2. CORRELATION SCREENING ===\n")

# ---------------------------------------------------------------------------
#' screen_confounders()
#'
#' Computes Pearson correlations of every numeric candidate variable with
#' the exposure and the outcome, flags those that exceed |r| >= r_thresh
#' with BOTH, and writes two CSVs (ALL, SHORTLIST) for the audit trail.
# ---------------------------------------------------------------------------
screen_confounders <- function(df,
                               exposure,
                               outcome,
                               exclude_vars = character(),
                               r_thresh     = 0.20,
                               p_thresh     = NULL,
                               dir_output   = NULL,
                               file_stub    = "confounder_screen") {
  
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
      pass_r = (abs(r_exp) >= r_thresh & abs(r_out) >= r_thresh),
      pass_p = if (is.null(p_thresh)) TRUE else (p_exp <= p_thresh & p_out <= p_thresh),
      pass   = pass_r & pass_p
    ) %>%
    select(var, r_exp, dir_exp, p_exp, r_out, dir_out, p_out, pass) %>%
    arrange(desc(abs(r_exp) + abs(r_out)))
  
  shortlisted <- res %>% filter(pass)
  
  if (!is.null(dir_output)) {
    readr::write_csv(res,         file.path(dir_output, paste0(file_stub, "_ALL.csv")))
    readr::write_csv(shortlisted, file.path(dir_output, paste0(file_stub, "_SHORTLIST.csv")))
    cat(sprintf("  Written: %s_ALL.csv  (%d vars)  |  %s_SHORTLIST.csv  (%d vars)\n",
                file_stub, nrow(res), file_stub, nrow(shortlisted)))
  }
  
  list(all = res, shortlist = shortlisted)
}

# ---------- exclusion lists ----------
exclude_base <- c("cause_id", "year_id", "location_id", "year_centered")

exclude_mechanical <- c(
  # exposure components / close cousins
  "rw_dex_hiv_prev_ratio", "rw_dex_hiv_prev_ratio_log",
  "rw_hiv_prev_ratio", "rw_hiv_prev_ratio_log",
  "ryan_white_funding_final", "spend_all",
  "as_spend_prev_ratio", "as_spend_prev_ratio_log",
  "spend_mdcd", "spend_mdcr", "spend_oop", "spend_priv",
  # outcome components / close cousins
  "as_mort_prev_ratio", "as_mort_prev_ratio_log",
  "mortality_rates", "mortality_counts",
  "prevalence_counts", "hiv_prevalence_counts",
  "daly_rates", "yll_rates", "yld_rates",
  "daly_counts", "yll_counts", "yld_counts",
  "incidence_counts",
  "as_daly_prev_ratio", "as_yll_prev_ratio", "as_yld_prev_ratio",
  "as_daly_prev_ratio_log", "as_yll_prev_ratio_log", "as_yld_prev_ratio_log",
  # structural
  "population", "variance",
  "prevalence_rates",       # mechanically linked to denominator
  "high_hiv_prev",          # EXCLUDED: prevalence-derived, denominator bias
  "high_sud_prev",
  "sud_prevalence_counts"
)

# --- 2a. Panel-level screening ---
out_panel <- screen_confounders(
  df           = df_hiv,
  exposure     = "rw_dex_hiv_prev_ratio_log",
  outcome      = "as_mort_prev_ratio_log",
  exclude_vars = c(exclude_base, exclude_mechanical),
  r_thresh     = 0.20,
  dir_output   = dir_output,
  file_stub    = "confounder_screen_PANEL"
)

cat("\nPanel-level shortlisted confounders:\n")
print(out_panel$shortlist, n = 20)

# --- 2b. State-means screening ---
df_state_full <- df_hiv %>%
  group_by(location_id, location_name) %>%
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)),
            .groups = "drop")

out_state <- screen_confounders(
  df           = df_state_full,
  exposure     = "rw_dex_hiv_prev_ratio_log",
  outcome      = "as_mort_prev_ratio_log",
  exclude_vars = c(exclude_base, exclude_mechanical),
  r_thresh     = 0.20,
  dir_output   = dir_output,
  file_stub    = "confounder_screen_STATE"
)

cat("\nState-level shortlisted confounders:\n")
print(out_state$shortlist, n = 20)
cat("\n")


##================================================================
## 3.  MUNDLAK BETWEEN / WITHIN VARIABLES  +  DERIVED INDICATORS
##================================================================
cat("=== 3. CREATING BETWEEN/WITHIN (MUNDLAK) VARIABLES ===\n")

# ---------------------------------------------------------------------------
#' make_mundlak_vars()
#'
#' For each variable in `vars`, creates:
#'   <var>_B  = state-level mean  (between)
#'   <var>_W  = panel value minus state mean  (within)
#' Also creates year_centered = year_id - midpoint.
# ---------------------------------------------------------------------------
make_mundlak_vars <- function(df, vars, group_var = "location_id", midpoint = 2014) {
  
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

# Variables for which we need _B and _W decomposition.
# This list covers every covariate used across ALL model families.
mundlak_vars <- c(
  "rw_dex_hiv_prev_ratio_log",  # exposure (logged)
  "race_prop_BLCK",              # structural: racial composition
  "incidence_rates",             # severity: epidemic intensity (continuous)
  "bmi",                         # health environment: body-mass index (continuous)
  "obesity",                     # health environment: obesity prevalence (binary-ish, alternative to bmi)
  "prev_diabetes",               # health environment: diabetes prevalence (alternative to bmi/obesity)
  "aca_implemented_status",      # policy: Medicaid expansion
  "edu_yrs",                     # socioeconomic position
  "mortality_rates"              # for robustness check (per-population outcome)
)

df_hiv <- make_mundlak_vars(df_hiv, mundlak_vars, "location_id", midpoint = 2014)

cat("Between (_B) and within (_W) variables created for:\n")
cat(paste(" ", mundlak_vars, collapse = "\n"), "\n")

# ---------- High-incidence binary indicator ----------
# Equal to 1 for states whose mean incidence rate falls in the
# TOP QUARTILE of the state-level incidence distribution.
# Computed from state means so it is time-invariant (avoids
# Nickell bias in short panels).  Used in between_race and
# between_key specifications as an alternative to continuous incidence.

state_inc_q75 <- quantile(df_hiv$incidence_rates_B, 0.75, na.rm = TRUE)

df_hiv <- df_hiv %>%
  mutate(high_incidence_q4_B = as.integer(incidence_rates_B >= state_inc_q75))

cat(sprintf("\nHigh-incidence indicator (top quartile): threshold = %.6f\n",
            state_inc_q75))
cat(sprintf("  States flagged as high-incidence: %d of %d\n",
            n_distinct(df_hiv$location_id[df_hiv$high_incidence_q4_B == 1]),
            n_distinct(df_hiv$location_id)))
cat("\n")


##================================================================
## 4.  VARIANCE DECOMPOSITION
##================================================================
cat("=== 4. VARIANCE DECOMPOSITION ===\n")

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
  calc_variance_decomp(df_hiv, "incidence_rates"),
  calc_variance_decomp(df_hiv, "bmi"),
  calc_variance_decomp(df_hiv, "obesity"),
  calc_variance_decomp(df_hiv, "prev_diabetes"),
  calc_variance_decomp(df_hiv, "edu_yrs")
)

cat("Variance Decomposition (% between vs within state):\n")
print(variance_decomp %>% select(variable, between_pct, within_pct))
cat("\n")

write.csv(variance_decomp,
          file.path(dir_output, "variance_decomposition.csv"),
          row.names = FALSE)


##================================================================
## 5.  REGRESSION MODELS
##================================================================
cat("=== 5. FITTING REGRESSION MODELS ===\n")

# Single accumulator — initialised once, never overwritten
list_models <- list()


# ---------------------------------------------------------------
# A)  MUNDLAK FAMILY  (within–between decomposition)  — 3 variants
#
#     The Mundlak CRE model decomposes spending into:
#       _B  = between-state mean (long-run, cross-sectional association)
#       _W  = within-state deviation (temporal, reactive spending signal)
#
#     race_prop_BLCK enters as _B only (essentially time-invariant
#     over 10 years).  Incidence and health-environment covariates
#     get both _B and _W because they vary meaningfully over time.
#
#     IMPORTANT: NO prevalence or high_prev variables anywhere.
# ---------------------------------------------------------------
cat("--- A) Mundlak (within-between) models [3 variants] ---\n")

# A1: MUNDLAK BASELINE
#     Confounders:
#       - race_prop_BLCK_B: structural — racial composition drives both
#         federal funding allocation and health disparities.
#       - incidence_rates_B/_W: severity — ongoing epidemic intensity
#         beyond prevalence.  Higher incidence → more new diagnoses →
#         short-term mortality pressure AND higher spending.
#       - bmi_B/_W: health environment — continuous measure of population
#         metabolic health; correlates with comorbidity burden.
#       - aca_implemented_status_B: policy — Medicaid expansion is an
#         access channel distinct from RW that independently affects
#         both spending levels and mortality.
list_models[["hiv__mundlak_baseline"]] <- lm(
  as_mort_prev_ratio_log ~
    rw_dex_hiv_prev_ratio_log_B + rw_dex_hiv_prev_ratio_log_W +
    year_centered +
    race_prop_BLCK_B +
    incidence_rates_B + incidence_rates_W +
    bmi_B + bmi_W +
    aca_implemented_status_B,
  data = df_hiv
)

# A2: MUNDLAK ALT HEALTH — swap BMI for diabetes prevalence.
#     Rationale: BMI/obesity showed counter-intuitive signs in earlier
#     models (committee meeting 2/3).  Diabetes prevalence is a more
#     direct comorbidity marker for PLWH (metabolic complications of
#     ART, accelerated aging).  If the spending coefficient is stable
#     across health-environment operationalisations, the finding is
#     robust to this choice.
list_models[["hiv__mundlak_alt_health"]] <- lm(
  as_mort_prev_ratio_log ~
    rw_dex_hiv_prev_ratio_log_B + rw_dex_hiv_prev_ratio_log_W +
    year_centered +
    race_prop_BLCK_B +
    incidence_rates_B + incidence_rates_W +
    prev_diabetes_B + prev_diabetes_W +
    aca_implemented_status_B,
  data = df_hiv
)

# A3: MUNDLAK NO POLICY — drop ACA expansion.
#     Tests whether the spending coefficient is confounded with
#     Medicaid expansion.  If beta_B is stable with and without ACA,
#     spending effects are not just a proxy for expansion.
list_models[["hiv__mundlak_no_policy"]] <- lm(
  as_mort_prev_ratio_log ~
    rw_dex_hiv_prev_ratio_log_B + rw_dex_hiv_prev_ratio_log_W +
    year_centered +
    race_prop_BLCK_B +
    incidence_rates_B + incidence_rates_W +
    bmi_B + bmi_W,
  data = df_hiv
)

cat("  Fitted 3 Mundlak models.\n")

# A4: MUNDLAK MINIMAL — structural-only adjustment (no incidence, no BMI, no ACA)
#     Purpose: show whether the between/within spending story holds
#     under the smallest plausible confounder set.
#     Includes:
#       - Spending_B and Spending_W (Mundlak decomposition)
#       - Year trend
#       - Race composition (between-state structural confounder)
list_models[["hiv__mundlak_minimal"]] <- lm(
  as_mort_prev_ratio_log ~
    rw_dex_hiv_prev_ratio_log_B + rw_dex_hiv_prev_ratio_log_W +
    year_centered +
    race_prop_BLCK_B,
  data = df_hiv
)

cat("  Added A4: hiv__mundlak_minimal (no incidence/BMI/ACA).\n")

# (Optional) A4b: MUNDLAK ULTRA-MINIMAL — spending only + year trend
list_models[["hiv__mundlak_ultra_minimal"]] <- lm(
  as_mort_prev_ratio_log ~
    rw_dex_hiv_prev_ratio_log_B + rw_dex_hiv_prev_ratio_log_W +
    year_centered,
  data = df_hiv
)

cat("  Added A4b: hiv__mundlak_ultra_minimal (spending + year only).\n")



# ---------------------------------------------------------------
# B)  BETWEEN_RACE FAMILY  (simple cross-sectional)  — 2 variants
#
#     The simplest between-state model with race as the primary
#     structural confounder.  Adding one severity variable and one
#     health control tests robustness while keeping the model
#     minimal and interpretable.
# ---------------------------------------------------------------
cat("--- B) Between_race models [2 variants] ---\n")

# B1: BETWEEN_RACE BASELINE
#     race (structural) + continuous incidence (severity) + BMI (health).
list_models[["hiv__between_race_baseline"]] <- lm(
  as_mort_prev_ratio_log ~
    rw_dex_hiv_prev_ratio_log_B + year_centered +
    race_prop_BLCK_B + incidence_rates_B + bmi_B,
  data = df_hiv
)

# B2: BETWEEN_RACE HIGH-INCIDENCE DUMMY
#     Replaces continuous incidence with binary top-quartile indicator.
#     A simpler categorisation that captures the structural difference
#     between "high-burden" and "low-burden" states without forcing a
#     linear incidence–mortality relationship.  May also reduce
#     multicollinearity with spending (federal allocation formulas
#     target high-incidence areas).
list_models[["hiv__between_race_high_inc"]] <- lm(
  as_mort_prev_ratio_log ~
    rw_dex_hiv_prev_ratio_log_B + year_centered +
    race_prop_BLCK_B + high_incidence_q4_B + bmi_B,
  data = df_hiv
)

cat("  Fitted 2 between_race models.\n")


# ---------------------------------------------------------------
# C)  BETWEEN_KEY_CONFOUNDERS FAMILY  — 4 variants
#
#     The "full" between-state confounder set.  Variations swap
#     health-environment and severity measures to test sensitivity
#     of the spending coefficient to alternative operationalisations.
#     This directly addresses the committee concern (2/3 meeting)
#     that obesity/BMI coefficients appeared counter-intuitive.
# ---------------------------------------------------------------
cat("--- C) Between_key_confounders models [4 variants] ---\n")

# C1: ORIGINAL — obesity as health environment.
list_models[["hiv__between_key_original_inc_log"]] <- lm(
  as_mort_prev_ratio_log ~
    rw_dex_hiv_prev_ratio_log_B + year_centered +
    race_prop_BLCK_B + log(incidence_rates_B) + obesity_B + aca_implemented_status_B,
  data = df_hiv
)



# C2: SWAP OBESITY → BMI (continuous).
#     Mean BMI may capture the health-environment gradient more smoothly
#     than binary obesity.  Tests whether the counter-intuitive obesity
#     coefficient is an artefact of operationalisation.
list_models[["hiv__between_key_swap_bmi"]] <- lm(
  as_mort_prev_ratio_log ~
    rw_dex_hiv_prev_ratio_log_B + year_centered +
    race_prop_BLCK_B + log(incidence_rates_B) + log(bmi_B) + aca_implemented_status_B,
  data = df_hiv
)


list_models[["hiv__between_key_hisp"]] <- lm(
  as_mort_prev_ratio_log ~
    rw_dex_hiv_prev_ratio_log_B + year_centered +
    race_prop_BLCK_B + log(incidence_rates_B) + log(bmi_B) + race_prop_HISP +  ldi_pc + cig_pc_10, 
  data = df_hiv
)

list_models[["hiv__between_key_best"]] <- lm(
  as_mort_prev_ratio_log ~
    rw_dex_hiv_prev_ratio_log_B + year_centered +
    race_prop_BLCK_B + log(incidence_rates_B) + log(bmi_B) + race_prop_HISP, 
  data = df_hiv
)


list_models[["hiv__between_key_best_nobmi"]] <- lm(
  as_mort_prev_ratio_log ~
    rw_dex_hiv_prev_ratio_log_B + year_centered +
    race_prop_BLCK_B + log(incidence_rates_B) + race_prop_HISP, 
  data = df_hiv
)

list_models[["hiv__between_key_best_diabetes"]] <- lm(
  as_mort_prev_ratio_log ~
    rw_dex_hiv_prev_ratio_log_B + year_centered +
    race_prop_BLCK_B + log(incidence_rates_B) + log(prev_diabetes_B) + race_prop_HISP, 
  data = df_hiv
)


list_models[["hiv__between_key_best_obesity_B"]] <- lm(
  as_mort_prev_ratio_log ~
    rw_dex_hiv_prev_ratio_log_B + year_centered +
    race_prop_BLCK_B + log(incidence_rates_B) + obesity_B + race_prop_HISP, 
  data = df_hiv
)



list_models[["hiv__between_key_best_ldi_pc"]] <- lm(
  as_mort_prev_ratio_log ~
    rw_dex_hiv_prev_ratio_log_B + year_centered +
    race_prop_BLCK_B + log(incidence_rates_B) + race_prop_HISP + log(ldi_pc), 
  data = df_hiv
)

list_models[["hiv__between_key_best_edu_yrs"]] <- lm(
  as_mort_prev_ratio_log ~
    rw_dex_hiv_prev_ratio_log_B + year_centered +
    race_prop_BLCK_B + log(incidence_rates_B) + race_prop_HISP + edu_yrs, 
  data = df_hiv
)

list_models[["hiv__between_key_best_unemployment_rate"]] <- lm(
  as_mort_prev_ratio_log ~
    rw_dex_hiv_prev_ratio_log_B + year_centered +
    race_prop_BLCK_B + log(incidence_rates_B) + race_prop_HISP + unemployment_rate, 
  data = df_hiv
)

list_models[["hiv__between_key_best_prop_homeless"]] <- lm(
  as_mort_prev_ratio_log ~
    rw_dex_hiv_prev_ratio_log_B + year_centered +
    race_prop_BLCK_B + log(incidence_rates_B) + race_prop_HISP + log(prop_homeless), 
  data = df_hiv
)



# C3: SWAP OBESITY → DIABETES PREVALENCE.
#     Diabetes is a more direct HIV-relevant comorbidity (metabolic
#     side effects of ART, accelerated aging in PLWH).  May provide
#     a more plausible coefficient direction than obesity/BMI.
list_models[["hiv__between_key_swap_diabetes"]] <- lm(
  as_mort_prev_ratio_log ~
    rw_dex_hiv_prev_ratio_log_B + year_centered +
    race_prop_BLCK_B + log(incidence_rates_B) + log(prev_diabetes_B) + aca_implemented_status_B,
  data = df_hiv
)

list_models[["hiv__between_key_swap_db_noaca"]] <- lm(
  as_mort_prev_ratio_log ~
    rw_dex_hiv_prev_ratio_log_B + year_centered +
    race_prop_BLCK_B + log(incidence_rates_B) + log(prev_diabetes_B),
  data = df_hiv
)


# C4: SWAP CONTINUOUS INCIDENCE → HIGH-INCIDENCE BINARY.
#     Tests whether a simpler "high vs low burden" distinction is
#     sufficient to adjust for epidemic severity.  Keeps obesity as
#     health environment (same as C1) to isolate the incidence swap.
list_models[["hiv__between_key_swap_incidence"]] <- lm(
  as_mort_prev_ratio_log ~
    rw_dex_hiv_prev_ratio_log_B + year_centered +
    race_prop_BLCK_B + high_incidence_q4_B + obesity_B + aca_implemented_status_B,
  data = df_hiv
)

cat("  Fitted 4 between_key_confounders models.\n")

# C5: MINIMAL STRUCTURAL BETWEEN MODEL
#     Removes epidemic severity (incidence), health environment
#     (obesity/BMI), and policy (ACA).
#     
#     Rationale:
#     - Avoids mechanical or allocation-based overadjustment
#     - Avoids mediators on spending pathway
#     - Preserves cross-state structural benchmarking logic
#     - Provides clean frontier-style interpretation

list_models[["hiv__between_key_minimal"]] <- lm(
  as_mort_prev_ratio_log ~
    rw_dex_hiv_prev_ratio_log_B + year_centered +
    incidence_rates_B,
  data = df_hiv
)
cat("  Added between_key_minimal model (structural-only adjustment).\n")



list_models[["hiv__between_key_inc_log"]] <- lm(
  as_mort_prev_ratio_log ~
    rw_dex_hiv_prev_ratio_log_B + year_centered + race_prop_BLCK_B +
    log(incidence_rates_B),
  data = df_hiv
)
cat("  Added between_key_minimal model (structural-only adjustment).\n")



list_models[["hiv__between_key_minimal_race"]] <- lm(
  as_mort_prev_ratio_log ~
    rw_dex_hiv_prev_ratio_log_B + year_centered +
    race_prop_BLCK_B,
  data = df_hiv
)
cat("  Added between_key_minimal model (structural-only adjustment).\n")

###
##---------------------------------------------------------------
## C6) HIGH-INCIDENCE INTERACTION MODEL  (Between-state)
##     Goal: test whether the spending–mortality association differs
##           in high-incidence states (top quartile of incidence_B).
##---------------------------------------------------------------

# --- Safety checks (creates high_incidence_q4_B if missing) ---
if (!("high_incidence_q4_B" %in% names(df_hiv))) {
  if (!("incidence_rates_B" %in% names(df_hiv))) {
    stop("C6 error: incidence_rates_B not found. Make sure make_mundlak_vars() ran and includes 'incidence_rates'.")
  }
  state_inc_q75 <- quantile(df_hiv$incidence_rates_B, 0.75, na.rm = TRUE)
  df_hiv <- df_hiv %>% mutate(high_incidence_q4_B = as.integer(incidence_rates_B >= state_inc_q75))
  cat(sprintf("C6: created high_incidence_q4_B (top quartile), threshold = %.6f\n", state_inc_q75))
}

# Optional: ensure it's treated as a factor (nice interpretation vs 0/1)
df_hiv <- df_hiv %>%
  mutate(high_incidence_q4_B_f = factor(high_incidence_q4_B, levels = c(0, 1),
                                        labels = c("Lower incidence (Q1–Q3)", "High incidence (Q4)")))

# --- C6 model: interaction ---
# Interpretation:
#   beta_spend = slope of spending_B in lower-incidence states (Q1–Q3)
#   beta_int   = additional slope in high-incidence states (Q4)
#   slope_high = beta_spend + beta_int
list_models[["hiv__between_highinc_interact"]] <- lm(
  as_mort_prev_ratio_log ~
    rw_dex_hiv_prev_ratio_log_B * high_incidence_q4_B_f +
    year_centered +
    race_prop_BLCK_B +
    bmi_B +
    aca_implemented_status_B,
  data = df_hiv
)

# (Optional) Also include continuous incidence_B as a control.
# Only do this if you think it's NOT overadjusting / collinear with spending.
# list_models[["hiv__between_highinc_interact_plus_incB"]] <- lm(
#   as_mort_prev_ratio_log ~
#     rw_dex_hiv_prev_ratio_log_B * high_incidence_q4_B_f +
#     year_centered +
#     race_prop_BLCK_B +
#     incidence_rates_B +
#     bmi_B +
#     aca_implemented_status_B,
#   data = df_hiv
# )

cat("  Added C6: hiv__between_highinc_interact\n")

# --- (Optional) quick helper: compute implied slopes (OLS point estimates) ---
# This prints the implied spending slope for low-incidence and high-incidence groups.
b <- coef(list_models[["hiv__between_highinc_interact"]])
term_int <- grep("rw_dex_hiv_prev_ratio_log_B:high_incidence_q4_B_f", names(b), value = TRUE)

if ("rw_dex_hiv_prev_ratio_log_B" %in% names(b) && length(term_int) == 1) {
  slope_low  <- unname(b["rw_dex_hiv_prev_ratio_log_B"])
  slope_high <- unname(b["rw_dex_hiv_prev_ratio_log_B"] + b[term_int])
  cat(sprintf("  Implied spending_B slope (low incidence):  %.4f\n", slope_low))
  cat(sprintf("  Implied spending_B slope (high incidence): %.4f\n\n", slope_high))
}



###

# ---------------------------------------------------------------
# D)  BETWEEN_EXTENDED FAMILY  — 2 variants
#
#     The key-confounder set plus education (socioeconomic position).
#     edu_yrs is pre-treatment: state-level educational attainment
#     reflects long-run human capital that predates the HIV epidemic
#     and is NOT caused by HIV spending or mortality (not a mediator
#     or collider).  We test with and without to see if SES confounds
#     the spending effect.
# ---------------------------------------------------------------
cat("--- D) Between_extended models [2 variants] ---\n")

# D1: WITH EDUCATION
list_models[["hiv__between_extended_with_edu"]] <- lm(
  as_mort_prev_ratio_log ~
    rw_dex_hiv_prev_ratio_log_B + year_centered +
    race_prop_BLCK_B + incidence_rates_B + obesity_B +
    aca_implemented_status_B + edu_yrs_B,
  data = df_hiv
)

# D2: WITHOUT EDUCATION
#     Same confounders as between_key_original; included here for
#     direct pairwise comparison within the "extended" family.
list_models[["hiv__between_extended_race_inc"]] <- lm(
  as_mort_prev_ratio_log ~
    rw_dex_hiv_prev_ratio_log_B + year_centered +
    race_prop_BLCK_B + incidence_rates_B,
  data = df_hiv
)

cat("  Fitted 2 between_extended models.\n")


# ---------------------------------------------------------------
# E)  LAG FAMILY  (panel-level, non-_B exposure)  — 2 models
#
#     Spending may take 1-2 years to manifest in mortality changes
#     (ART adherence, care linkage).  Lags test temporality and
#     help rule out reverse causation.
# ---------------------------------------------------------------
cat("--- E) Lag models [2 models] ---\n")

df_hiv <- df_hiv %>%
  arrange(location_id, year_id) %>%
  group_by(location_id) %>%
  mutate(
    rw_dex_hiv_prev_ratio_log_l1 = dplyr::lag(rw_dex_hiv_prev_ratio_log, 1),
    rw_dex_hiv_prev_ratio_log_l2 = dplyr::lag(rw_dex_hiv_prev_ratio_log, 2)
  ) %>%
  ungroup()

n_lag1 <- sum(!is.na(df_hiv$rw_dex_hiv_prev_ratio_log_l1))
n_lag2 <- sum(!is.na(df_hiv$rw_dex_hiv_prev_ratio_log_l2))
cat(sprintf("  Lag 1 available obs: %d  |  Lag 2 available obs: %d\n", n_lag1, n_lag2))

# E1: Single lag (t-1)
list_models[["hiv__lag1_simple"]] <- lm(
  as_mort_prev_ratio_log ~ rw_dex_hiv_prev_ratio_log_l1 +
    year_centered + race_prop_BLCK,
  data = df_hiv
)

# E2: Distributed lag (t-1, t-2) — captures longer programme ramp-up.
list_models[["hiv__distributed_lag_1_2"]] <- lm(
  as_mort_prev_ratio_log ~ rw_dex_hiv_prev_ratio_log_l1 +
    rw_dex_hiv_prev_ratio_log_l2 +
    year_centered + race_prop_BLCK,
  data = df_hiv
)

cat("  Fitted 2 lag models.\n")


# ---------------------------------------------------------------
# F)  DOSE / THRESHOLD FAMILY  (panel-level)  — 2 models
#
#     The spending–mortality relationship may not be linear.
#     Tests for diminishing returns or threshold effects.
# ---------------------------------------------------------------
cat("--- F) Dose-response models [2 models] ---\n")

# F1: Quadratic — tests for curvature (diminishing returns).
list_models[["hiv__dose_quadratic"]] <- lm(
  as_mort_prev_ratio_log ~ rw_dex_hiv_prev_ratio_log +
    I(rw_dex_hiv_prev_ratio_log^2) +
    year_centered + race_prop_BLCK,
  data = df_hiv
)

# F2: Piecewise linear with knot at 75th percentile.
knot_p75 <- quantile(df_hiv$rw_dex_hiv_prev_ratio_log, 0.75, na.rm = TRUE)

df_hiv <- df_hiv %>%
  mutate(over_p75 = pmax(0, rw_dex_hiv_prev_ratio_log - knot_p75))

list_models[["hiv__dose_threshold_p75"]] <- lm(
  as_mort_prev_ratio_log ~ rw_dex_hiv_prev_ratio_log + over_p75 +
    year_centered + race_prop_BLCK,
  data = df_hiv
)

cat("  Fitted 2 dose-response models.\n")

cat(sprintf("\n  >>> TOTAL MODELS: %d <<<\n\n", length(list_models)))


##================================================================
## 6.  EXTRACT RESULTS WITH CLUSTER-ROBUST STANDARD ERRORS
##================================================================
cat("=== 6. EXTRACTING RESULTS (cluster-robust SEs) ===\n")

# ---------------------------------------------------------------------------
#' extract_clustered()
#'
#' Tidy coefficient table using CR2 cluster-robust variance (clubSandwich)
#' with Satterthwaite degrees of freedom.
# ---------------------------------------------------------------------------
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
    warning(sprintf("Cluster-robust SEs failed for %s: %s. Using OLS SEs.",
                    model_id, e$message))
    broom::tidy(model) %>% mutate(model_id = model_id)
  })
}

# --- 6a. Coefficient table ---
coef_tbl <- map_dfr(names(list_models), function(nm) {
  extract_clustered(list_models[[nm]], nm, df_hiv$location_id)
}) %>%
  tidyr::separate(model_id, into = c("acause", "model_name"),
                  sep = "__", remove = FALSE) %>%
  mutate(
    signif_stars = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      p.value < 0.1   ~ "\u2020",
      TRUE            ~ ""
    ),
    signif_label = case_when(
      p.value < 0.001 ~ "p < 0.001",
      p.value < 0.01  ~ "p < 0.01",
      p.value < 0.05  ~ "p < 0.05",
      p.value < 0.1   ~ "p < 0.1",
      TRUE            ~ "not significant"
    )
  ) %>%
  select(model_name, term, signif_label, estimate, std.error,
         statistic, p.value, model_id, acause, signif_stars)

# --- 6b. Model-fit metrics (reference only — NOT for model selection) ---
metrics_tbl <- imap_dfr(list_models, function(model, model_id) {
  g <- broom::glance(model)
  tibble(
    model_id   = model_id,
    model_name = sub("^hiv__", "", model_id),
    n          = g$nobs,
    r2         = g$r.squared,
    adj_r2     = g$adj.r.squared,
    aic        = AIC(model),
    bic        = BIC(model),
    sigma      = g$sigma
  )
})


##================================================================
## 7.  SAVE OUTPUTS
##================================================================
cat("=== 7. SAVING OUTPUTS ===\n")

# Coefficients (main deliverable)
write.csv(coef_tbl,
          file.path(dir_output, paste0("regression_results_hiv_coefficients_", output_date, ".csv")),
          row.names = FALSE)

# Metrics (reference only)
write.csv(metrics_tbl,
          file.path(dir_output, paste0("regression_results_hiv_metrics_", output_date, ".csv")),
          row.names = FALSE)

# Analysis dataset (with all _B, _W, lag, dose, indicator variables)
write.csv(df_hiv,
          file.path(dir_output, "df_hiv_analysis.csv"),
          row.names = FALSE)

cat(sprintf("  Outputs saved to: %s\n\n", dir_output))


##================================================================
## 8.  CONSOLE DIAGNOSTICS
##================================================================
cat("========================================\n")
cat("  DIAGNOSTIC SUMMARY\n")
cat("========================================\n\n")

# --- 8a. Observations per model ---
cat("--- Observations per model ---\n")
for (nm in names(list_models)) {
  cat(sprintf("  %-45s  n = %d\n", nm, nobs(list_models[[nm]])))
}

# --- 8b. ALL spending coefficients across models ---
cat("\n--- Spending coefficient(s) per model ---\n")
spending_coefs <- coef_tbl %>%
  filter(grepl("rw_dex_hiv_prev_ratio", term))

print(
  spending_coefs %>%
    select(model_name, term, estimate, std.error, p.value, signif_stars) %>%
    mutate(
      estimate  = round(estimate, 5),
      std.error = round(std.error, 5),
      p.value   = round(p.value, 4)
    ) %>%
    as.data.frame(),
  row.names = FALSE
)

# --- 8c. Between-state coefficient (_B) comparison ---
cat("\n--- Between-state spending coefficient (_B) comparison ---\n")
cat("    (Direction and significance should be broadly stable\n")
cat("     across specifications if confounding is well-addressed.)\n\n")

between_coefs <- spending_coefs %>%
  filter(grepl("_B$", term)) %>%
  select(model_name, estimate, std.error, p.value, signif_stars) %>%
  mutate(
    estimate  = round(estimate, 5),
    std.error = round(std.error, 5),
    p.value   = round(p.value, 4)
  )

print(as.data.frame(between_coefs), row.names = FALSE)

# --- 8d. Within-state coefficient (_W, Mundlak models only) ---
cat("\n--- Within-state spending coefficient (_W, Mundlak models only) ---\n")
cat("    Positive _W = reactive spending (states spend more when mortality rises).\n")
cat("    Negative _W = effective within-state spending changes.\n\n")

within_coefs <- spending_coefs %>%
  filter(grepl("_W$", term)) %>%
  select(model_name, estimate, std.error, p.value, signif_stars) %>%
  mutate(
    estimate  = round(estimate, 5),
    std.error = round(std.error, 5),
    p.value   = round(p.value, 4)
  )

if (nrow(within_coefs) > 0) {
  print(as.data.frame(within_coefs), row.names = FALSE)
} else {
  cat("  (No within-state spending coefficients found.)\n")
}

# --- 8e. Health-environment coefficient comparison ---
cat("\n--- Health-environment coefficient comparison ---\n")
cat("    (BMI vs obesity vs diabetes: check for counter-intuitive signs.)\n\n")

health_coefs <- coef_tbl %>%
  filter(grepl("bmi_B|obesity_B|prev_diabetes_B", term)) %>%
  select(model_name, term, estimate, std.error, p.value, signif_stars) %>%
  mutate(
    estimate  = round(estimate, 5),
    std.error = round(std.error, 5),
    p.value   = round(p.value, 4)
  )

if (nrow(health_coefs) > 0) {
  print(as.data.frame(health_coefs), row.names = FALSE)
} else {
  cat("  (No health-environment coefficients found.)\n")
}

# --- 8f. Sign-flip warning ---
cat("\n--- Sign stability check ---\n")
main_signs <- spending_coefs %>%
  filter(grepl("_B$|_l1$|^rw_dex_hiv_prev_ratio_log$", term)) %>%
  mutate(sign_dir = sign(estimate))

if (length(unique(main_signs$sign_dir)) > 1) {
  cat("  *** WARNING: spending coefficient SIGN FLIPS across models. ***\n")
  cat("  Models with POSITIVE sign:\n")
  pos <- main_signs %>% filter(sign_dir > 0) %>%
    select(model_name, term, estimate) %>%
    mutate(estimate = round(estimate, 5))
  if (nrow(pos) > 0) print(as.data.frame(pos), row.names = FALSE)
  cat("  Models with NEGATIVE sign:\n")
  neg <- main_signs %>% filter(sign_dir < 0) %>%
    select(model_name, term, estimate) %>%
    mutate(estimate = round(estimate, 5))
  if (nrow(neg) > 0) print(as.data.frame(neg), row.names = FALSE)
  cat("  -> This typically indicates confounding; inspect which adjustments\n")
  cat("     flip the sign (usually race/incidence).\n")
} else if (nrow(main_signs) > 0) {
  cat(sprintf("  Spending coefficient sign is consistently %s across %d models.\n",
              ifelse(main_signs$sign_dir[1] > 0, "POSITIVE", "NEGATIVE"),
              nrow(main_signs)))
}

# --- 8g. Variance decomposition recap ---
cat("\n--- Variance Decomposition ---\n")
print(variance_decomp %>% select(variable, between_pct, within_pct) %>% as.data.frame(),
      row.names = FALSE)

# --- 8h. Fit table (reference ONLY — not for model selection per committee) ---
cat("\n--- Model Fit (reference only — NOT for model selection) ---\n")
print(
  metrics_tbl %>%
    select(model_name, n, adj_r2, aic) %>%
    mutate(adj_r2 = round(adj_r2, 4)) %>%
    as.data.frame(),
  row.names = FALSE
)

cat("\n========================================\n")
cat("  KEY NOTES FOR INTERPRETATION\n")
cat("========================================\n")
cat("1. Outcome and exposure are LOGGED: coefficients are elasticities.\n")
cat("   e.g., beta_B = -0.10 means a 1% higher spending/case across states\n")
cat("   is associated with a 0.10% lower mortality/case.\n")
cat("2. NO prevalence or high-prevalence variables in any model\n")
cat("   (avoids denominator bias since prevalence is in both Y and X).\n")
cat("3. Mundlak models separate between (_B) and within (_W) spending effects.\n")
cat("   _B = long-run cross-sectional association.\n")
cat("   _W = within-state temporal change (positive → reactive spending).\n")
cat("4. Health-environment swaps (BMI vs obesity vs diabetes) test\n")
cat("   sensitivity of the counter-intuitive signs from earlier models.\n")
cat("5. High-incidence binary (top quartile) is an alternative to continuous\n")
cat("   incidence that reduces collinearity with spending.\n")
cat("6. ALL models use cluster-robust SEs (CR2, Satterthwaite df) by state.\n")
cat("7. Model families for publication selection:\n")
cat("     A. Mundlak:           3 variants (baseline, alt-health, no-policy)\n")
cat("     B. Between_race:      2 variants (continuous vs binary incidence)\n")
cat("     C. Between_key:       4 variants (obesity/BMI/diabetes/incidence swaps)\n")
cat("     D. Between_extended:  2 variants (with/without education)\n")
cat("     E. Lag:               2 models  (t-1, distributed t-1/t-2)\n")
cat("     F. Dose-response:     2 models  (quadratic, threshold p75)\n")
cat("========================================\n")