##================================================================
##  Title:     C_04_HIV_per_capita_models.R
##  Folder:    C_frontier_analysis/
##  Purpose:   HIV regression analysis — POST-MAY-6-COMMITTEE per-capita framework.
##
##  Three lenses, all with HIV spending per capita as exposure:
##
##    Lens 1A — BURDEN (DALYs)
##      Outcome:  log(daly_per_capita)
##      Primary burden control: log(incidence_per_100k)
##
##    Lens 1B — BURDEN (Mortality)  [NEW]
##      Outcome:  log(mortality_per_capita)
##      Same covariate structure as Lens 1A.
##      Mortality is the more interpretable outcome for clinical audiences
##      (no disability-weight subjectivity) and serves as a robustness check.
##
##    Lens 2 — PREVENTION
##      Outcome:  log(incidence_per_100k)
##      Primary burden control: log(prevalence_per_100k)_L1
##
##  SIGN INTERPRETATION NOTE
##    A POSITIVE coefficient on log_spending_per_capita in a burden lens
##    indicates MORE spending associated with MORE burden — this is the
##    needs-based-allocation / reverse-causation pattern, NOT a causal
##    spending->burden effect. The Mundlak decomposition (model 1.11 /
##    1B.11) separates this:
##      _B coefficient = cross-sectional allocation (typically large +)
##      _W coefficient = within-state year-to-year (closer to causal)
##    The _W coefficient is the cleanest available identification without
##    instrumental variables and is the manuscript-ready answer to the
##    "spending tracks burden because of allocation" critique.
##
##  Why incidence (not prevalence) is the primary burden control in Lens 1:
##    Prevalence is endogenous to the health system (ART → suppression →
##    affects transmission → affects prevalence). Marcia's denominator-
##    endogeneity argument applies on the RHS too. Incidence is the cleaner
##    proxy — new infection pressure, upstream of the spending decision.
##
##  Why lagged (not contemporaneous) prevalence in Lens 2:
##    Current prevalence in year t includes incident cases from t, t-1, t-2,
##    etc., so it's mechanically downstream of current incidence — bad
##    control. Lagged prevalence is the stock of infection that could
##    transmit and is exogenous to current incidence.
##
##  Mundlak B/W split is the primary identification strategy. Directly
##  responds to Joe Dieleman's May 11 critique that fixed effects average
##  out the cross-sectional signal — Mundlak preserves both.
##
##  Standard errors:
##    Panel models      -> clubSandwich::vcovCR + Satterthwaite (CR2)
##    Cross-sectional   -> sandwich::vcovHC type "HC2"
##
##  Inputs (df_as_processed_rw_gbd.csv must contain):
##    - as_spend_per_capita, dex_pop   (post-committee additions)
##    - spend_all, ryan_white_funding_final
##    - spend_AM, spend_ED, spend_HH, spend_IP, spend_NF, spend_RX  (optional)
##    - daly_counts, mortality_counts, incidence_counts, prevalence_counts
##    - location_id, location_name, year_id, acause
##    - race_prop_BLCK, race_prop_HISP, prop_homeless,
##      ldi_pc, unemployment_rate
##================================================================


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

# ---------- user library ----------
user_lib <- file.path(h, "R_packages")
if (!dir.exists(user_lib)) dir.create(user_lib, recursive = TRUE)
.libPaths(c(user_lib, .libPaths()))

pacman::p_load(
  data.table, tidyverse, glue, broom,
  lmtest, sandwich, clubSandwich,
  Hmisc
)

if ("plotly" %in% loadedNamespaces()) filter <- dplyr::filter
tryCatch(
  conflicted::conflicts_prefer(dplyr::filter, .quiet = TRUE),
  error = function(e) invisible(NULL)
)

# ---------- IO ----------
input_date  <- "20260514"   # <-- EDIT: date of prep_B output
dir_input   <- file.path(h, "aim_outputs/Aim2/C_frontier_analysis", input_date)

output_date <- format(Sys.time(), "%Y%m%d")
dir_output  <- file.path(h, "aim_outputs/Aim2/C_frontier_analysis",
                         output_date, "analysis_per_capita")
if (!dir.exists(dir_output)) dir.create(dir_output, recursive = TRUE)

df_as <- read.csv(
  file.path(dir_input, "df_as_processed_rw_gbd.csv"),
  stringsAsFactors = FALSE
)

# ---------- guardrails ----------
required_cols <- c("as_spend_per_capita", "dex_pop",
                   "spend_all", "ryan_white_funding_final",
                   "daly_counts", "mortality_counts",
                   "incidence_counts", "prevalence_counts",
                   "location_id", "location_name", "year_id", "acause")
missing_cols <- setdiff(required_cols, names(df_as))
if (length(missing_cols) > 0) {
  stop("Missing required columns in df_as_processed_rw_gbd.csv: ",
       paste(missing_cols, collapse = ", "),
       "\nRe-run C_00_prep_A_panel_foundation.R + prep_B.R with the per-capita patches first.")
}

toc_cols <- c("spend_AM", "spend_ED", "spend_HH", "spend_IP", "spend_NF", "spend_RX")
toc_available <- all(toc_cols %in% names(df_as))
if (!toc_available) {
  message("NOTE: not all TOC columns present. By-TOC decomposition will be skipped.")
  message("      Missing: ", paste(setdiff(toc_cols, names(df_as)), collapse = ", "))
}


##================================================================
## 2.  FILTER TO HIV
##================================================================
## All per-capita variables, log transforms, lag variables, and
## Mundlak _B/_W are now built in C_01_prep_B_regression_covariates.R
## (section 9.5). This script just filters to the HIV slice and
## drops rows missing the key analytic variables.

df_hiv <- df_as %>%
  dplyr::filter(acause == "hiv",
                !is.na(log_daly_per_capita),
                !is.na(log_mortality_per_capita),
                !is.na(log_spending_per_capita),
                !is.na(dex_pop), dex_pop > 0)

# Refresh TOC-availability flag against the HIV slice
toc_pc_cols <- c("spend_pharma_pc", "spend_ambulatory_pc",
                 "spend_inpatient_pc", "spend_nf_pc")
toc_available <- all(toc_pc_cols %in% names(df_hiv))
if (!toc_available) {
  message("NOTE: TOC per-capita columns not in df_as. ",
          "primary_by_toc models will be skipped.")
}

cat("\n---- Per-capita variable summaries (HIV slice) ----\n")
print(summary(df_hiv$spending_per_capita))
print(summary(df_hiv$daly_per_capita))
print(summary(df_hiv$mortality_per_capita))
print(summary(df_hiv$incidence_per_100k))
print(summary(df_hiv$prevalence_per_100k))


##================================================================
## 5.  SAVE PANEL + COLLAPSED STATE-MEAN DATASETS
##================================================================

write.csv(df_hiv,
          file.path(dir_output, "df_hiv_per_capita_panel.csv"),
          row.names = FALSE)

id_cols <- c("location_id", "location_name", "year_id", "cause_id")
numeric_cols  <- df_hiv %>% select(where(is.numeric)) %>% names()
collapse_cols <- setdiff(numeric_cols, id_cols)

df_between <- df_hiv %>%
  group_by(location_id, location_name) %>%
  summarise(
    across(all_of(collapse_cols), \(x) mean(x, na.rm = TRUE)),
    n_years = dplyr::n(),
    .groups = "drop"
  )

write.csv(df_between,
          file.path(dir_output, "df_hiv_per_capita_between.csv"),
          row.names = FALSE)

cat("\nPanel rows:    ", nrow(df_hiv), "\n",
    "Between rows: ", nrow(df_between), "\n", sep = "")


##================================================================
## 6.  SUMMARY TABLE 1
##================================================================

summary_vars <- c(
  "spending_per_capita",  "log_spending_per_capita",
  "daly_per_capita",      "log_daly_per_capita",
  "mortality_per_capita", "log_mortality_per_capita",
  "incidence_per_100k",   "log_incidence_per_100k",
  "prevalence_per_100k",  "log_prevalence_per_100k",
  "dex_pop",
  "race_prop_BLCK", "race_prop_HISP",
  "prop_homeless",  "log_prop_homeless",
  "ldi_pc",         "log_ldi_pc",
  "unemployment_rate", "edu_yrs",
  "spend_all", "ryan_white_funding_final",
  "spend_pharma_pc", "spend_ambulatory_pc",
  "spend_inpatient_pc", "spend_nf_pc"
)
summary_vars <- intersect(summary_vars, names(df_hiv))

summary_tbl <- df_hiv %>%
  select(all_of(summary_vars)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  group_by(variable) %>%
  summarise(
    n      = sum(!is.na(value)),
    mean   = mean(value, na.rm = TRUE),
    sd     = sd(value, na.rm = TRUE),
    min    = min(value, na.rm = TRUE),
    p25    = quantile(value, 0.25, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    p75    = quantile(value, 0.75, na.rm = TRUE),
    max    = max(value, na.rm = TRUE),
    .groups = "drop"
  )

write.csv(summary_tbl,
          file.path(dir_output, "summary_table1_per_capita.csv"),
          row.names = FALSE)


##================================================================
## 7.  DIAGNOSTICS — confounder screening + variance decomposition
##================================================================

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
    tibble(var = candidates,
           !!paste0("r_", target)   := as.numeric(r),
           !!paste0("p_", target)   := as.numeric(p),
           !!paste0("dir_", target) := sign(as.numeric(r)))
  }
  ex_stats  <- pull_stats(exposure)
  out_stats <- pull_stats(outcome)
  
  res <- ex_stats %>%
    left_join(out_stats, by = "var") %>%
    rename(r_exp   = !!paste0("r_", exposure),
           p_exp   = !!paste0("p_", exposure),
           dir_exp = !!paste0("dir_", exposure),
           r_out   = !!paste0("r_", outcome),
           p_out   = !!paste0("p_", outcome),
           dir_out = !!paste0("dir_", outcome)) %>%
    mutate(pass = (abs(r_exp) >= r_thresh & abs(r_out) >= r_thresh)) %>%
    select(var, r_exp, dir_exp, p_exp, r_out, dir_out, p_out, pass) %>%
    arrange(desc(abs(r_exp) + abs(r_out)))
  
  shortlisted <- res %>% dplyr::filter(pass)
  
  if (!is.null(dir_output)) {
    readr::write_csv(res,         file.path(dir_output, paste0(file_stub, "_ALL.csv")))
    readr::write_csv(shortlisted, file.path(dir_output, paste0(file_stub, "_SHORTLIST.csv")))
  }
  list(all = res, shortlist = shortlisted)
}

exclude_base <- c("cause_id", "year_id", "location_id")
exclude_mechanical_burden <- c(
  "spending_per_capita", "log_spending_per_capita",
  "spend_all", "spend_mdcd", "spend_mdcr", "spend_oop", "spend_priv",
  "spend_AM", "spend_ED", "spend_HH", "spend_IP", "spend_NF", "spend_RX",
  "spend_pharma_pc", "spend_ambulatory_pc", "spend_inpatient_pc",
  "spend_nf_pc", "spend_ed_pc", "spend_hh_pc",
  "log_spend_pharma_pc", "log_spend_ambulatory_pc",
  "log_spend_inpatient_pc", "log_spend_nf_pc",
  "ryan_white_funding_final",
  "as_spend_per_capita", "as_spend_prev_ratio", "as_spend_prev_ratio_log",
  "rw_dex_hiv_prev_ratio", "rw_dex_hiv_prev_ratio_log",
  "rw_hiv_prev_ratio", "rw_hiv_prev_ratio_log",
  "daly_per_capita", "log_daly_per_capita", "daly_counts",
  "mortality_per_capita", "log_mortality_per_capita", "mortality_counts",
  "as_daly_prev_ratio", "as_daly_prev_ratio_log",
  "yll_counts", "yld_counts", "as_yll_prev_ratio", "as_yld_prev_ratio",
  "mortality_rates",
  "as_mort_prev_ratio", "as_mort_prev_ratio_log",
  "dex_pop", "population"
)

exclude_mechanical_prev <- c(
  exclude_mechanical_burden,
  "incidence_per_100k", "log_incidence_per_100k", "incidence_counts",
  "incidence_rates", "log_incidence_per_100k_l1"
)

screen_confounders(
  df = df_hiv, exposure = "log_spending_per_capita",
  outcome = "log_daly_per_capita",
  exclude_vars = c(exclude_base, exclude_mechanical_burden),
  r_thresh = 0.20, dir_output = dir_output,
  file_stub = "confounder_screen_burden_PANEL"
)
screen_confounders(
  df = df_between, exposure = "log_spending_per_capita",
  outcome = "log_daly_per_capita",
  exclude_vars = c(exclude_base, exclude_mechanical_burden),
  r_thresh = 0.20, dir_output = dir_output,
  file_stub = "confounder_screen_burden_STATE"
)
screen_confounders(
  df = df_hiv, exposure = "log_spending_per_capita",
  outcome = "log_mortality_per_capita",
  exclude_vars = c(exclude_base, exclude_mechanical_burden),
  r_thresh = 0.20, dir_output = dir_output,
  file_stub = "confounder_screen_burden_mort_PANEL"
)
screen_confounders(
  df = df_hiv, exposure = "log_spending_per_capita",
  outcome = "log_incidence_per_100k",
  exclude_vars = c(exclude_base, exclude_mechanical_prev),
  r_thresh = 0.20, dir_output = dir_output,
  file_stub = "confounder_screen_prevention_PANEL"
)
screen_confounders(
  df = df_between, exposure = "log_spending_per_capita",
  outcome = "log_incidence_per_100k",
  exclude_vars = c(exclude_base, exclude_mechanical_prev),
  r_thresh = 0.20, dir_output = dir_output,
  file_stub = "confounder_screen_prevention_STATE"
)


# ---- 7b. Variance decomposition ----
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
  tibble(variable = var_name,
         total_var = total_var, between_var = between_var, within_var = within_var,
         between_pct = round(between_var / total_var * 100, 1),
         within_pct  = round(within_var  / total_var * 100, 1))
}

variance_decomp <- bind_rows(
  calc_variance_decomp(df_hiv, "log_spending_per_capita"),
  calc_variance_decomp(df_hiv, "log_daly_per_capita"),
  calc_variance_decomp(df_hiv, "log_mortality_per_capita"),
  calc_variance_decomp(df_hiv, "log_incidence_per_100k"),
  calc_variance_decomp(df_hiv, "log_prevalence_per_100k"),
  calc_variance_decomp(df_hiv, "race_prop_BLCK"),
  calc_variance_decomp(df_hiv, "race_prop_HISP"),
  calc_variance_decomp(df_hiv, "log_prop_homeless"),
  calc_variance_decomp(df_hiv, "log_ldi_pc"),
  calc_variance_decomp(df_hiv, "unemployment_rate")
)
write.csv(variance_decomp,
          file.path(dir_output, "variance_decomposition_per_capita.csv"),
          row.names = FALSE)


##================================================================
## 8.  FIT REGRESSION MODELS
##================================================================

list_models    <- list()
model_data     <- list()
model_registry <- tibble(model_id = character(), lens = character(),
                         spec = character(), is_final = logical())

register_model <- function(lens, spec, formula, data, is_final = FALSE) {
  model_id <- paste0("hiv__", lens, "__", spec)
  fit <- lm(as.formula(formula), data = data)
  list_models[[model_id]]   <<- fit
  model_data[[model_id]]    <<- data
  model_registry <<- bind_rows(
    model_registry,
    tibble(model_id = model_id, lens = lens, spec = spec, is_final = is_final)
  )
  invisible(fit)
}


# ==============================================================
# LENS 1A — BURDEN (DALYs)
# ==============================================================

register_model(lens = "burden", spec = "bivariate",
               formula = "log_daly_per_capita ~ log_spending_per_capita + year_factor",
               data = df_hiv, is_final = TRUE)

register_model(lens = "burden", spec = "primary",
               formula = "log_daly_per_capita ~ log_spending_per_capita + year_factor +
              race_prop_BLCK + race_prop_HISP +
              log_prop_homeless + log_incidence_per_100k",
               data = df_hiv, is_final = TRUE)

register_model(lens = "burden", spec = "no_burden_control",
               formula = "log_daly_per_capita ~ log_spending_per_capita + year_factor +
              race_prop_BLCK + race_prop_HISP + log_prop_homeless",
               data = df_hiv, is_final = TRUE)

register_model(lens = "burden", spec = "sens_prev_control",
               formula = "log_daly_per_capita ~ log_spending_per_capita + year_factor +
              race_prop_BLCK + race_prop_HISP +
              log_prop_homeless + log_prevalence_per_100k",
               data = df_hiv, is_final = TRUE)

register_model(lens = "burden", spec = "sens_lag_prev",
               formula = "log_daly_per_capita ~ log_spending_per_capita + year_factor +
              race_prop_BLCK + race_prop_HISP +
              log_prop_homeless + log_prevalence_per_100k_l1",
               data = df_hiv, is_final = TRUE)

register_model(lens = "burden", spec = "sens_lag_incidence",
               formula = "log_daly_per_capita ~ log_spending_per_capita + year_factor +
              race_prop_BLCK + race_prop_HISP +
              log_prop_homeless + log_incidence_per_100k_l1",
               data = df_hiv, is_final = TRUE)

register_model(lens = "burden", spec = "lag_spending_l1",
               formula = "log_daly_per_capita ~ log_spending_per_capita_l1 + year_factor +
              race_prop_BLCK + race_prop_HISP +
              log_prop_homeless + log_incidence_per_100k",
               data = df_hiv, is_final = FALSE)

register_model(lens = "burden", spec = "lag_spending_l2",
               formula = "log_daly_per_capita ~ log_spending_per_capita_l2 + year_factor +
              race_prop_BLCK + race_prop_HISP +
              log_prop_homeless + log_incidence_per_100k",
               data = df_hiv, is_final = FALSE)

register_model(lens = "burden", spec = "robustness_ldi",
               formula = "log_daly_per_capita ~ log_spending_per_capita + year_factor +
              race_prop_BLCK + race_prop_HISP +
              log_prop_homeless + log_incidence_per_100k + log_ldi_pc",
               data = df_hiv, is_final = FALSE)

register_model(lens = "burden", spec = "robustness_unemployment",
               formula = "log_daly_per_capita ~ log_spending_per_capita + year_factor +
              race_prop_BLCK + race_prop_HISP +
              log_prop_homeless + log_incidence_per_100k +
              unemployment_rate",
               data = df_hiv, is_final = FALSE)

register_model(lens = "burden", spec = "mundlak",
               formula = "log_daly_per_capita ~
              log_spending_per_capita_B + log_spending_per_capita_W +
              log_incidence_per_100k_B  + log_incidence_per_100k_W +
              race_prop_BLCK_B + race_prop_BLCK_W +
              race_prop_HISP_B + race_prop_HISP_W +
              log_prop_homeless_MLB + log_prop_homeless_MLW +   # [Commit B: mean-of-log Mundlak; prep_B exposes both conventions]
              year_factor",
               data = df_hiv, is_final = TRUE)

register_model(lens = "burden", spec = "between_true",
               formula = "log_daly_per_capita ~ log_spending_per_capita +
              race_prop_BLCK + race_prop_HISP +
              log_prop_homeless + log_incidence_per_100k",
               data = df_between, is_final = TRUE)

if (toc_available) {
  register_model(lens = "burden", spec = "primary_by_toc",
                 formula = "log_daly_per_capita ~
                log_spend_pharma_pc + log_spend_ambulatory_pc +
                log_spend_inpatient_pc + log_spend_nf_pc +
                year_factor + race_prop_BLCK + race_prop_HISP +
                log_prop_homeless + log_incidence_per_100k",
                 data = df_hiv, is_final = FALSE)
}


# ==============================================================
# LENS 1B — BURDEN (Mortality)   [NEW]
#   Outcome:  log(mortality_per_capita)
#   Parallel to Lens 1A. Mortality is more interpretable for clinical
#   audiences (no disability weights) and serves as a robustness check.
#   Sign interpretation is the same: a POSITIVE coefficient on
#   log_spending_per_capita = allocation pattern, not effectiveness.
#   The Mundlak _W coefficient is the cleanest causal read.
# ==============================================================

register_model(lens = "burden_mort", spec = "bivariate",
               formula = "log_mortality_per_capita ~ log_spending_per_capita + year_factor",
               data = df_hiv, is_final = TRUE)

register_model(lens = "burden_mort", spec = "primary",
               formula = "log_mortality_per_capita ~ log_spending_per_capita + year_factor +
              race_prop_BLCK + race_prop_HISP +
              log_prop_homeless + log_incidence_per_100k",
               data = df_hiv, is_final = TRUE)

register_model(lens = "burden_mort", spec = "no_burden_control",
               formula = "log_mortality_per_capita ~ log_spending_per_capita + year_factor +
              race_prop_BLCK + race_prop_HISP + log_prop_homeless",
               data = df_hiv, is_final = TRUE)

register_model(lens = "burden_mort", spec = "sens_prev_control",
               formula = "log_mortality_per_capita ~ log_spending_per_capita + year_factor +
              race_prop_BLCK + race_prop_HISP +
              log_prop_homeless + log_prevalence_per_100k",
               data = df_hiv, is_final = TRUE)

register_model(lens = "burden_mort", spec = "sens_lag_prev",
               formula = "log_mortality_per_capita ~ log_spending_per_capita + year_factor +
              race_prop_BLCK + race_prop_HISP +
              log_prop_homeless + log_prevalence_per_100k_l1",
               data = df_hiv, is_final = TRUE)

register_model(lens = "burden_mort", spec = "sens_lag_incidence",
               formula = "log_mortality_per_capita ~ log_spending_per_capita + year_factor +
              race_prop_BLCK + race_prop_HISP +
              log_prop_homeless + log_incidence_per_100k_l1",
               data = df_hiv, is_final = TRUE)

register_model(lens = "burden_mort", spec = "lag_spending_l1",
               formula = "log_mortality_per_capita ~ log_spending_per_capita_l1 + year_factor +
              race_prop_BLCK + race_prop_HISP +
              log_prop_homeless + log_incidence_per_100k",
               data = df_hiv, is_final = FALSE)

register_model(lens = "burden_mort", spec = "lag_spending_l2",
               formula = "log_mortality_per_capita ~ log_spending_per_capita_l2 + year_factor +
              race_prop_BLCK + race_prop_HISP +
              log_prop_homeless + log_incidence_per_100k",
               data = df_hiv, is_final = FALSE)

register_model(lens = "burden_mort", spec = "robustness_ldi",
               formula = "log_mortality_per_capita ~ log_spending_per_capita + year_factor +
              race_prop_BLCK + race_prop_HISP +
              log_prop_homeless + log_incidence_per_100k + log_ldi_pc",
               data = df_hiv, is_final = FALSE)

register_model(lens = "burden_mort", spec = "robustness_unemployment",
               formula = "log_mortality_per_capita ~ log_spending_per_capita + year_factor +
              race_prop_BLCK + race_prop_HISP +
              log_prop_homeless + log_incidence_per_100k +
              unemployment_rate",
               data = df_hiv, is_final = FALSE)

register_model(lens = "burden_mort", spec = "mundlak",
               formula = "log_mortality_per_capita ~
              log_spending_per_capita_B + log_spending_per_capita_W +
              log_incidence_per_100k_B  + log_incidence_per_100k_W +
              race_prop_BLCK_B + race_prop_BLCK_W +
              race_prop_HISP_B + race_prop_HISP_W +
              log_prop_homeless_MLB + log_prop_homeless_MLW +   # [Commit B: mean-of-log Mundlak; prep_B exposes both conventions]
              year_factor",
               data = df_hiv, is_final = TRUE)

register_model(lens = "burden_mort", spec = "between_true",
               formula = "log_mortality_per_capita ~ log_spending_per_capita +
              race_prop_BLCK + race_prop_HISP +
              log_prop_homeless + log_incidence_per_100k",
               data = df_between, is_final = TRUE)

if (toc_available) {
  register_model(lens = "burden_mort", spec = "primary_by_toc",
                 formula = "log_mortality_per_capita ~
                log_spend_pharma_pc + log_spend_ambulatory_pc +
                log_spend_inpatient_pc + log_spend_nf_pc +
                year_factor + race_prop_BLCK + race_prop_HISP +
                log_prop_homeless + log_incidence_per_100k",
                 data = df_hiv, is_final = FALSE)
}


# ==============================================================
# LENS 2 — PREVENTION
# ==============================================================

register_model(lens = "prevention", spec = "bivariate_inc",
               formula = "log_incidence_per_100k ~ log_spending_per_capita + year_factor",
               data = df_hiv, is_final = TRUE)

register_model(lens = "prevention", spec = "primary_inc",
               formula = "log_incidence_per_100k ~ log_spending_per_capita + year_factor +
              race_prop_BLCK + race_prop_HISP +
              log_prop_homeless + log_prevalence_per_100k_l1",
               data = df_hiv, is_final = TRUE)

register_model(lens = "prevention", spec = "primary_inc_no_burden",
               formula = "log_incidence_per_100k ~ log_spending_per_capita + year_factor +
              race_prop_BLCK + race_prop_HISP + log_prop_homeless",
               data = df_hiv, is_final = TRUE)

register_model(lens = "prevention", spec = "primary_inc_lag2prev",
               formula = "log_incidence_per_100k ~ log_spending_per_capita + year_factor +
              race_prop_BLCK + race_prop_HISP +
              log_prop_homeless + log_prevalence_per_100k_l2",
               data = df_hiv, is_final = TRUE)

register_model(lens = "prevention", spec = "lag_spending_l1_inc",
               formula = "log_incidence_per_100k ~ log_spending_per_capita_l1 + year_factor +
              race_prop_BLCK + race_prop_HISP +
              log_prop_homeless + log_prevalence_per_100k_l1",
               data = df_hiv, is_final = FALSE)

register_model(lens = "prevention", spec = "lag_spending_l2_inc",
               formula = "log_incidence_per_100k ~ log_spending_per_capita_l2 + year_factor +
              race_prop_BLCK + race_prop_HISP +
              log_prop_homeless + log_prevalence_per_100k_l1",
               data = df_hiv, is_final = FALSE)

register_model(lens = "prevention", spec = "mundlak_inc",
               formula = "log_incidence_per_100k ~
              log_spending_per_capita_B + log_spending_per_capita_W +
              log_prevalence_per_100k_B + log_prevalence_per_100k_W +
              race_prop_BLCK_B + race_prop_BLCK_W +
              race_prop_HISP_B + race_prop_HISP_W +
              log_prop_homeless_MLB + log_prop_homeless_MLW +   # [Commit B: mean-of-log Mundlak; prep_B exposes both conventions]
              year_factor",
               data = df_hiv, is_final = TRUE)

if (toc_available) {
  register_model(lens = "prevention", spec = "primary_by_toc",
                 formula = "log_incidence_per_100k ~
                log_spend_pharma_pc + log_spend_ambulatory_pc +
                log_spend_inpatient_pc + log_spend_nf_pc +
                year_factor + race_prop_BLCK + race_prop_HISP +
                log_prop_homeless + log_prevalence_per_100k_l1",
                 data = df_hiv, is_final = FALSE)
}


##================================================================
## 9.  EXTRACT RESULTS WITH ROBUST STANDARD ERRORS
##================================================================

extract_clustered <- function(model, model_id, model_data_df) {
  is_between <- grepl("between_true", model_id)
  
  if (is_between) {
    tryCatch({
      vcov_hc <- vcovHC(model, type = "HC2")
      ct      <- coeftest(model, vcov. = vcov_hc)
      tibble(
        term      = rownames(ct),
        estimate  = ct[, "Estimate"],
        std.error = ct[, "Std. Error"],
        statistic = ct[, "t value"],
        p.value   = ct[, "Pr(>|t|)"],
        model_id  = model_id
      )
    }, error = function(e) broom::tidy(model) %>% mutate(model_id = model_id))
  } else {
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
    }, error = function(e) broom::tidy(model) %>% mutate(model_id = model_id))
  }
}

coef_tbl <- map_dfr(names(list_models), function(nm) {
  extract_clustered(list_models[[nm]], nm, model_data[[nm]])
})

coef_tbl <- coef_tbl %>%
  left_join(model_registry %>% select(model_id, lens, spec),
            by = "model_id") %>%
  mutate(
    acause = "hiv",
    signif_stars = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE            ~ ""
    ),
    signif_label = case_when(
      p.value < 0.001 ~ "p < 0.001",
      p.value < 0.01  ~ "p < 0.01",
      p.value < 0.05  ~ "p < 0.05",
      TRUE            ~ "not significant"
    )
  )

metrics_tbl <- imap_dfr(list_models, function(model, model_id) {
  g <- broom::glance(model)
  tibble(model_id = model_id,
         n = g$nobs, r2 = g$r.squared, adj_r2 = g$adj.r.squared,
         aic = AIC(model), bic = BIC(model), sigma = g$sigma)
})

regression_results <- coef_tbl %>%
  left_join(metrics_tbl, by = "model_id") %>%
  select(model_id, acause, lens, spec, term,
         estimate, std.error, statistic, p.value,
         signif_stars, signif_label,
         n, r2, adj_r2, aic, bic, sigma)

write.csv(regression_results,
          file.path(dir_output, "regression_results_hiv_per_capita_combined.csv"),
          row.names = FALSE)

spending_summary <- regression_results %>%
  dplyr::filter(grepl("log_spending_per_capita|log_spend_pharma_pc|log_spend_ambulatory_pc|log_spend_inpatient_pc|log_spend_nf_pc",
                      term)) %>%
  select(model_id, lens, spec, term, estimate, std.error,
         p.value, signif_label, n, adj_r2) %>%
  mutate(
    estimate  = round(estimate, 5),
    std.error = round(std.error, 5),
    p.value   = round(p.value, 4)
  )

write.csv(spending_summary,
          file.path(dir_output, "spending_coefficients_summary_per_capita.csv"),
          row.names = FALSE)

write.csv(model_registry,
          file.path(dir_output, "model_registry_per_capita.csv"),
          row.names = FALSE)


cat("\n================ DONE ================\n")
cat("Outputs written to:", dir_output, "\n")
cat("Burden (DALY) models:      ", sum(model_registry$lens == "burden"), "\n")
cat("Burden (Mortality) models: ", sum(model_registry$lens == "burden_mort"), "\n")
cat("Prevention models:         ", sum(model_registry$lens == "prevention"), "\n")
cat("By-TOC included:           ", toc_available, "\n\n")

##================================================================
## END OF PIPELINE
##================================================================