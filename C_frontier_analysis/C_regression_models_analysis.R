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
##'   3. Mundlak Between/Within Variables
##'   4. Variance Decomposition
##'   5. Regression Models (Between-State, Lag, Dose-Response)
##'   6. Extract Results (cluster-robust SEs) & Save
##'
##' Note: Data processing (RW, CDC, covariates) lives in C_model_data_prep.R.
##'       This script consumes the processed output.
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
# Both sides logged for elasticity interpretation (committee agreed)
df_hiv <- df_hiv %>%
  mutate(
    as_mort_prev_ratio_log       = log(as_mort_prev_ratio),
    rw_dex_hiv_prev_ratio_log    = log(rw_dex_hiv_prev_ratio),
    # Keep log versions of secondary outcomes for reference
    as_yll_prev_ratio_log        = log(as_yll_prev_ratio),
    as_daly_prev_ratio_log       = log(as_daly_prev_ratio),
    as_yld_prev_ratio_log        = if_else(as_yld_prev_ratio > 0,
                                           log(as_yld_prev_ratio), NA_real_),
    # Log non-ratio spending if present
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
#' 
#' @param df         Data frame (panel or state-means)
#' @param exposure   Character: exposure column name
#' @param outcome    Character: outcome column name
#' @param exclude_vars Character vector of columns to skip
#' @param r_thresh   Absolute correlation threshold (default 0.20)
#' @param p_thresh   Optional p-value filter (NULL = skip)
#' @param dir_output Path for CSV output (NULL = no write)
#' @param file_stub  Prefix for filenames
#' @return list(all, shortlist)
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

# Mechanical exclusions: components of exposure/outcome ratios, closely
# overlapping numerators/denominators, and derived log columns
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
  # other structural
  "population", "variance",
  "prevalence_rates",   # near-mechanically linked to prevalence_counts/pop
  "high_hiv_prev",      # derived from prevalence counts
  "high_sud_prev",      # SUD-specific
  "sud_prevalence_counts"  # SUD-specific
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
## 3.  MUNDLAK BETWEEN / WITHIN VARIABLES
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
  
  # Between means
  state_means <- df %>%
    group_by(across(all_of(group_var))) %>%
    summarise(
      across(all_of(vars), \(x) mean(x, na.rm = TRUE), .names = "{.col}_B"),
      .groups = "drop"
    )
  
  df <- left_join(df, state_means, by = group_var)
  
  # Within deviations
  
  for (v in vars) {
    df[[paste0(v, "_W")]] <- df[[v]] - df[[paste0(v, "_B")]]
  }
  
  # Centered year
  df$year_centered <- df$year_id - midpoint
  
  df
}

# Variables for which we need between-state means
mundlak_vars <- c(
  "rw_dex_hiv_prev_ratio_log",   # exposure (logged)
  "race_prop_BLCK",               # key confounder: racial composition
  "incidence_rates",              # key confounder: HIV incidence
  "obesity",                      # key confounder: comorbidity proxy (labelled "obesity" in data; mapped to bmi concept)
  "aca_implemented_status",       # key confounder: Medicaid expansion
  "edu_yrs",                      # optional confounder: socioeconomic position
  "mortality_rates"               # for robustness check
)

df_hiv <- make_mundlak_vars(df_hiv, mundlak_vars, "location_id", midpoint = 2014)

cat("Between (_B) and within (_W) variables created for:\n")
cat(paste(" ", mundlak_vars, collapse = "\n"), "\n\n")

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
  calc_variance_decomp(df_hiv, "edu_yrs"),
  calc_variance_decomp(df_hiv, "obesity")
)

cat("Variance Decomposition (% between vs within state):\n")
print(variance_decomp %>% select(variable, between_pct, within_pct))
cat("\n")

write.csv(variance_decomp, file.path(dir_output, "variance_decomposition.csv"), row.names = FALSE)

##================================================================
## 5.  REGRESSION MODELS
##================================================================
cat("=== 5. FITTING REGRESSION MODELS ===\n")

# Single accumulator — never overwritten
list_models <- list()

# ---------------------------------------------------------------
# A)  BETWEEN-STATE FAMILY  (use *_B exposure, logged outcome)
#
#     Logic: ~89% of spending variation is between states.
#     Between-state models leverage this cross-sectional variation.
#     Covariates are state-means (_B) to avoid Nickell-type bias.
# ---------------------------------------------------------------
cat("--- A) Between-State models ---\n")

# M1: Unadjusted — spending mean + secular time trend
list_models[["hiv__between_unadj"]] <- lm(
  as_mort_prev_ratio_log ~ rw_dex_hiv_prev_ratio_log_B + year_centered,
  data = df_hiv
)

# M2: + Race — racial composition is the strongest between-state confounder.
#     States with larger Black populations have both higher spending (more
#     federal RW allocation) and higher mortality (health disparities).
list_models[["hiv__between_race"]] <- lm(
  as_mort_prev_ratio_log ~ rw_dex_hiv_prev_ratio_log_B + year_centered +
    race_prop_BLCK_B,
  data = df_hiv
)

# M3: + HIV incidence — captures ongoing epidemic intensity beyond
#     prevalence.  Higher incidence → more new diagnoses → potentially
#     higher short-term mortality AND higher spending.  Pre-treatment
#     because incidence drives funding allocation formulas.
list_models[["hiv__between_race_incidence"]] <- lm(
  as_mort_prev_ratio_log ~ rw_dex_hiv_prev_ratio_log_B + year_centered +
    race_prop_BLCK_B + incidence_rates_B,
  data = df_hiv
)

# M4: Full key confounders — adds obesity (comorbidity burden) and
#     ACA Medicaid expansion (access channel distinct from RW).
#     Obesity proxies overall chronic-disease burden correlated with
#     both mortality and state health spending.
#     ACA expansion is a policy shock that increased coverage
#     independently of RW and could reduce HIV mortality.
list_models[["hiv__between_key_confounders"]] <- lm(
  as_mort_prev_ratio_log ~ rw_dex_hiv_prev_ratio_log_B + year_centered +
    race_prop_BLCK_B + incidence_rates_B + obesity_B + aca_implemented_status_B,
  data = df_hiv
)

# M5: Mundlak — separates between-state and within-state spending effects.
#     Between (_B) = long-run cross-sectional association.
#     Within  (_W) = within-state temporal change (reactive spending signal).
list_models[["hiv__mundlak_key"]] <- lm(
  as_mort_prev_ratio_log ~ rw_dex_hiv_prev_ratio_log_B +
    rw_dex_hiv_prev_ratio_log_W +
    year_centered +
    race_prop_BLCK_B + incidence_rates_B + obesity_B + aca_implemented_status_B,
  data = df_hiv
)

# M6: Extended — adds education years (socioeconomic position).
#     Justified as pre-treatment: state-level educational attainment
#     reflects long-run human-capital stock that predates HIV epidemic.
#     NOT a mediator of spending → mortality (spending does not change
#     education).  NOT a collider because education is not caused by
#     both spending and mortality simultaneously.
list_models[["hiv__between_extended_edu"]] <- lm(
  as_mort_prev_ratio_log ~ rw_dex_hiv_prev_ratio_log_B + year_centered +
    race_prop_BLCK_B + incidence_rates_B + obesity_B +
    aca_implemented_status_B + edu_yrs_B,
  data = df_hiv
)

cat(sprintf("  Fitted %d between-state models.\n", 6))

#####
# Ensure BETWEEN-state versions of prevalence and high prevalence exist
#####

df_hiv <- df_hiv %>%
  group_by(location_id) %>%
  mutate(
    # Continuous prevalence (state mean)
    prevalence_rates_B = mean(prevalence_rates, na.rm = TRUE),
    
    # Binary high-prevalence (state-level)
    # Using max() ensures once-high-always-high across years
    high_hiv_prev_B = max(high_hiv_prev, na.rm = TRUE)
  ) %>%
  ungroup()


#####
# M7: Between-state model with continuous prevalence (burden control)
#####

list_models[["hiv__between_race_prevalence"]] <- lm(
  as_mort_prev_ratio_log ~ 
    rw_dex_hiv_prev_ratio_log_B + 
    year_centered +
    race_prop_BLCK_B + 
    prevalence_rates_B,
  data = df_hiv
)


#####
# M8: Between-state model with high-prevalence indicator
#####

list_models[["hiv__between_race_highprev"]] <- lm(
  as_mort_prev_ratio_log ~ 
    rw_dex_hiv_prev_ratio_log_B + 
    year_centered +
    race_prop_BLCK_B + 
    high_hiv_prev_B,
  data = df_hiv
)


#####
# M9: Interaction model — does spending work differently in high-prevalence states?
#####

list_models[["hiv__between_highprev_interact"]] <- lm(
  as_mort_prev_ratio_log ~ 
    rw_dex_hiv_prev_ratio_log_B * high_hiv_prev_B +
    year_centered +
    race_prop_BLCK_B,
  data = df_hiv
)


# ---------------------------------------------------------------
# B)  LAG FAMILY  (panel-level, non-_B exposure)
#
#     Logic: Spending may take 1-2 years to manifest in mortality
#     changes (ART adherence, care linkage). Lags test temporality.
# ---------------------------------------------------------------
cat("--- B) Lag models ---\n")

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

# L1: Single lag (t-1) — most common specification in health econ
list_models[["hiv__lag1_simple"]] <- lm(
  as_mort_prev_ratio_log ~ rw_dex_hiv_prev_ratio_log_l1 +
    year_centered + race_prop_BLCK,
  data = df_hiv
)

# L2: Distributed lag (t-1, t-2) — captures longer programme ramp-up.
#     Watch for collinearity between lag1 and lag2.
list_models[["hiv__distributed_lag_1_2"]] <- lm(
  as_mort_prev_ratio_log ~ rw_dex_hiv_prev_ratio_log_l1 +
    rw_dex_hiv_prev_ratio_log_l2 +
    year_centered + race_prop_BLCK,
  data = df_hiv
)

cat(sprintf("  Fitted %d lag models.\n", 2))

# ---------------------------------------------------------------
# C)  DOSE / THRESHOLD FAMILY  (panel-level)
#
#     Logic: The relationship between spending and outcomes may not
#     be linear — diminishing returns or threshold effects are
#     plausible.  These models test non-linearity.
# ---------------------------------------------------------------
cat("--- C) Dose-response models ---\n")

# Quadratic: tests for simple curvature (diminishing returns).
#   If beta_X2 < 0: diminishing returns at higher spending.
list_models[["hiv__dose_quadratic"]] <- lm(
  as_mort_prev_ratio_log ~ rw_dex_hiv_prev_ratio_log +
    I(rw_dex_hiv_prev_ratio_log^2) +
    year_centered + race_prop_BLCK,
  data = df_hiv
)

# Threshold at 75th percentile: piecewise linear.
#   Below p75: slope = beta_X.
#   Above p75: slope = beta_X + beta_over_p75 (allows a kink).
knot_p75 <- quantile(df_hiv$rw_dex_hiv_prev_ratio_log, 0.75, na.rm = TRUE)

df_hiv <- df_hiv %>%
  mutate(over_p75 = pmax(0, rw_dex_hiv_prev_ratio_log - knot_p75))

list_models[["hiv__dose_threshold_p75"]] <- lm(
  as_mort_prev_ratio_log ~ rw_dex_hiv_prev_ratio_log + over_p75 +
    year_centered + race_prop_BLCK,
  data = df_hiv
)

cat(sprintf("  Fitted %d dose-response models.\n", 2))
cat(sprintf("  Total models in list: %d\n\n", length(list_models)))

##================================================================
## 6.  EXTRACT RESULTS WITH CLUSTER-ROBUST STANDARD ERRORS
##================================================================
cat("=== 6. EXTRACTING RESULTS (cluster-robust SEs) ===\n")

# ---------------------------------------------------------------------------
#' extract_clustered()
#'
#' Returns a tidy coefficient table using CR2 cluster-robust variance
#' (clubSandwich) with Satterthwaite degrees of freedom.
#' Falls back to OLS SEs if clustering fails (e.g., insufficient clusters
#' for a particular model).
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
    warning(sprintf("Cluster-robust SEs failed for %s: %s. Using OLS SEs.", model_id, e$message))
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
  # Reorder columns to match required output spec
  select(model_name, term, signif_label, estimate, std.error,
         statistic, p.value, model_id, acause, signif_stars)

# --- 6b. Model-fit metrics (reference only — NOT for model selection) ---
metrics_tbl <- imap_dfr(list_models, function(model, model_id) {
  g <- broom::glance(model)
  tibble(
    model_id  = model_id,
    model_name = sub("^hiv__", "", model_id),
    n         = g$nobs,
    r2        = g$r.squared,
    adj_r2    = g$adj.r.squared,
    aic       = AIC(model),
    bic       = BIC(model),
    sigma     = g$sigma
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

# Analysis dataset
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

# --- 8a. Model N ---
cat("--- Observations per model ---\n")
for (nm in names(list_models)) {
  cat(sprintf("  %-40s  n = %d\n", nm, nobs(list_models[[nm]])))
}

# --- 8b. Spending coefficients across models ---
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

# --- 8c. Sign-flip warning ---
signs <- spending_coefs %>%
  filter(grepl("_B$|_l1$|^rw_dex_hiv_prev_ratio_log$", term)) %>%
  pull(estimate) %>%
  sign()

if (length(unique(signs)) > 1) {
  cat("\n  *** WARNING: spending coefficient SIGN FLIPS across models. ***\n")
  cat("      This typically indicates confounding; inspect race/incidence adjustments.\n")
} else {
  cat(sprintf("\n  Spending coefficient sign is consistently %s across all models.\n",
              ifelse(signs[1] > 0, "POSITIVE", "NEGATIVE")))
}

# --- 8d. Variance decomposition recap ---
cat("\n--- Variance Decomposition ---\n")
print(variance_decomp %>% select(variable, between_pct, within_pct) %>% as.data.frame(),
      row.names = FALSE)

# --- 8e. Fit table (reference only) ---
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
cat("   e.g., beta = -0.10 means a 1% increase in spending/case is\n")
cat("   associated with a 0.10% decrease in mortality/case.\n")
cat("2. Between-state models leverage cross-sectional variation (~89% of total).\n")
cat("3. Mundlak _W coefficient captures reactive within-state spending changes.\n")
cat("4. Lag models test whether spending effects manifest with 1-2 year delay.\n")
cat("5. Dose models test non-linearity (diminishing returns / threshold).\n")
cat("6. ALL models use cluster-robust SEs (CR2, Satterthwaite df) by state.\n")
cat("========================================\n")


