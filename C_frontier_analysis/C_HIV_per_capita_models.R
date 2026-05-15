##================================================================
##  Title:     C_HIV_per_capita_models.R
##  Folder:    C_frontier_analysis/
##  Purpose:   HIV regression analysis — POST-MAY-6-COMMITTEE per-capita framework.
##
##             Replaces the per-prevalent-case framework used in
##             C_HIV_regression_models_analysis.R. Implements two lenses:
##
##               Lens 1 — BURDEN
##                 Exposure: log(spending_per_capita)
##                 Outcome:  log(daly_per_capita)
##                 Rationale: same population denominator on both sides —
##                            removes the prevalence-endogeneity issue raised
##                            by Marcia Weaver, Joe Dieleman, Emily Williams.
##
##               Lens 2 — PREVENTION
##                 Exposure: log(spending_per_capita)
##                 Outcome:  log(incidence_per_100k)
##                 Rationale: captures the prevention pathway (transmission
##                            averted) that the per-prevalent-case framework
##                            could not address. Incidence is the outcome
##                            here, not a control — using it as a covariate
##                            in Lens 1 would over-adjust the prevention path.
##
##  Standard errors:
##    Panel models      -> clubSandwich::vcovCR + Satterthwaite (CR2)
##    Cross-sectional   -> sandwich::vcovHC type "HC2"
##
##  Inputs:
##    df_as_processed_rw_gbd.csv  (from C_regression_prep_B.R)
##      Must contain:
##        - as_spend_per_capita  (created post-committee in prep_A.R)
##        - dex_pop              (created post-committee in prep_A.R)
##        - spend_all, ryan_white_funding_final
##        - daly_counts, incidence_counts, prevalence_counts
##        - location_id, location_name, year_id, acause
##        - race_prop_BLCK, race_prop_HISP, prop_homeless,
##          ldi_pc, unemployment_rate, edu_yrs, aca_implemented_status
##
##  Outputs (in dir_output):
##    df_hiv_per_capita_panel.csv
##    df_hiv_per_capita_between.csv
##    summary_table1_per_capita.csv
##    confounder_screen_burden_PANEL.csv  / _STATE.csv
##    confounder_screen_prevention_PANEL.csv / _STATE.csv
##    variance_decomposition_per_capita.csv
##    regression_results_hiv_per_capita_combined.csv
##    spending_coefficients_summary_per_capita.csv
##    model_registry_per_capita.csv
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

# Resolve filter conflicts
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

# ---------- guardrails on required columns ----------
required_cols <- c("as_spend_per_capita", "dex_pop",
                   "spend_all", "ryan_white_funding_final",
                   "daly_counts", "incidence_counts", "prevalence_counts",
                   "location_id", "location_name", "year_id", "acause")
missing_cols <- setdiff(required_cols, names(df_as))
if (length(missing_cols) > 0) {
  stop("Missing required columns in df_as_processed_rw_gbd.csv: ",
       paste(missing_cols, collapse = ", "),
       "\nRe-run C_regression_prep_A.R + prep_B.R with the per-capita patches first.")
}


##================================================================
## 2.  COMPUTE PER-CAPITA / PER-100K VARIABLES
##================================================================
##  - spending_per_capita = (RW + DEX spend) / dex_pop
##  - daly_per_capita     = daly_counts / dex_pop
##  - incidence_per_100k  = incidence_counts / dex_pop * 1e5
##  - prevalence_per_100k = prevalence_counts / dex_pop * 1e5
##
##  All denominators are dex_pop (DEX population — consistent with
##  how DEX itself computes per-capita).
##================================================================
safe_log <- function(x) if_else(x > 0, log(x), NA_real_)

df_hiv <- df_as %>%
  dplyr::filter(acause == "hiv") %>%
  mutate(
    spending_per_capita  = (ryan_white_funding_final + spend_all) / dex_pop,
    daly_per_capita      = daly_counts      / dex_pop,
    incidence_per_100k   = (incidence_counts  / dex_pop) * 1e5,
    prevalence_per_100k  = (prevalence_counts / dex_pop) * 1e5,
    # Logs
    log_spending_per_capita = safe_log(spending_per_capita),
    log_daly_per_capita     = safe_log(daly_per_capita),
    log_incidence_per_100k  = safe_log(incidence_per_100k),
    log_prevalence_per_100k = safe_log(prevalence_per_100k)
  ) %>%
  dplyr::filter(!is.na(log_daly_per_capita),
                !is.na(log_spending_per_capita),
                !is.na(dex_pop), dex_pop > 0)

cat("\n---- Per-capita variable summaries ----\n")
print(summary(df_hiv$spending_per_capita))
print(summary(df_hiv$daly_per_capita))
print(summary(df_hiv$incidence_per_100k))
print(summary(df_hiv$prevalence_per_100k))


##================================================================
## 3.  COVARIATE LOGS, LAGS, AND YEAR FACTOR
##================================================================

df_hiv <- df_hiv %>%
  mutate(
    log_prop_homeless        = safe_log(prop_homeless),
    log_ldi_pc               = safe_log(ldi_pc),
    year_factor              = factor(year_id)
  )

# Lags (sorted by state, year)
df_hiv <- df_hiv %>%
  arrange(location_id, year_id) %>%
  group_by(location_id) %>%
  mutate(
    log_spending_per_capita_l1 = dplyr::lag(log_spending_per_capita, 1),
    log_spending_per_capita_l2 = dplyr::lag(log_spending_per_capita, 2),
    log_prevalence_per_100k_l1 = dplyr::lag(log_prevalence_per_100k, 1)
  ) %>%
  ungroup()


##================================================================
## 4.  SAVE PANEL + COLLAPSED STATE-MEAN DATASETS
##================================================================

# ---- 4a. Panel ----
write.csv(df_hiv,
          file.path(dir_output, "df_hiv_per_capita_panel.csv"),
          row.names = FALSE)

# ---- 4b. Collapsed (one row per state, for cross-sectional between) ----
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
## 5.  SUMMARY TABLE 1 (per-capita variables + covariates)
##================================================================

summary_vars <- c(
  "spending_per_capita",  "log_spending_per_capita",
  "daly_per_capita",      "log_daly_per_capita",
  "incidence_per_100k",   "log_incidence_per_100k",
  "prevalence_per_100k",  "log_prevalence_per_100k",
  "dex_pop",
  "race_prop_BLCK", "race_prop_HISP",
  "prop_homeless",  "log_prop_homeless",
  "ldi_pc",         "log_ldi_pc",
  "unemployment_rate", "edu_yrs",
  "aca_implemented_status",
  "spend_all", "ryan_white_funding_final"
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
## 6.  DIAGNOSTICS — confounder screening + variance decomposition
##================================================================

# ---- 6a. Confounder screening function (re-used) ----
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

# Mechanical exclusions — drop variables that are denominators / numerators
# of the analysis variables (would be tautological)
exclude_base <- c("cause_id", "year_id", "location_id")
exclude_mechanical <- c(
  # All forms of spending
  "spending_per_capita", "log_spending_per_capita",
  "spend_all", "spend_mdcd", "spend_mdcr", "spend_oop", "spend_priv",
  "ryan_white_funding_final",
  "as_spend_per_capita", "as_spend_prev_ratio", "as_spend_prev_ratio_log",
  "rw_dex_hiv_prev_ratio", "rw_dex_hiv_prev_ratio_log",
  "rw_hiv_prev_ratio", "rw_hiv_prev_ratio_log",
  # All forms of DALYs / outcomes that are downstream of spending or share denom
  "daly_per_capita", "log_daly_per_capita",
  "daly_counts", "yll_counts", "yld_counts",
  "as_daly_prev_ratio", "as_daly_prev_ratio_log",
  "as_yll_prev_ratio", "as_yld_prev_ratio",
  "mortality_rates", "mortality_counts",
  "as_mort_prev_ratio", "as_mort_prev_ratio_log",
  # Incidence (Lens 2 outcome — don't put it on the screen of Lens 1)
  "incidence_per_100k", "log_incidence_per_100k", "incidence_counts",
  "incidence_rates",
  # Prevalence (used differently in each lens — exclude from auto-screen)
  "prevalence_per_100k", "log_prevalence_per_100k", "prevalence_counts",
  "prevalence_rates",
  # Population denominators
  "dex_pop", "population"
)

# Burden lens — screen on panel
screen_confounders(
  df           = df_hiv,
  exposure     = "log_spending_per_capita",
  outcome      = "log_daly_per_capita",
  exclude_vars = c(exclude_base, exclude_mechanical),
  r_thresh     = 0.20,
  dir_output   = dir_output,
  file_stub    = "confounder_screen_burden_PANEL"
)

# Burden lens — state-means screen
df_state_means <- df_hiv %>%
  group_by(location_id, location_name) %>%
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)),
            .groups = "drop")

screen_confounders(
  df           = df_state_means,
  exposure     = "log_spending_per_capita",
  outcome      = "log_daly_per_capita",
  exclude_vars = c(exclude_base, exclude_mechanical),
  r_thresh     = 0.20,
  dir_output   = dir_output,
  file_stub    = "confounder_screen_burden_STATE"
)

# Prevention lens — screen on panel.
# For this lens, prevalence is allowed back into the candidate pool;
# we keep incidence excluded because it's the outcome.
exclude_mechanical_prev <- setdiff(exclude_mechanical,
                                   c("prevalence_per_100k",
                                     "log_prevalence_per_100k"))

screen_confounders(
  df           = df_hiv,
  exposure     = "log_spending_per_capita",
  outcome      = "log_incidence_per_100k",
  exclude_vars = c(exclude_base, exclude_mechanical_prev),
  r_thresh     = 0.20,
  dir_output   = dir_output,
  file_stub    = "confounder_screen_prevention_PANEL"
)

screen_confounders(
  df           = df_state_means,
  exposure     = "log_spending_per_capita",
  outcome      = "log_incidence_per_100k",
  exclude_vars = c(exclude_base, exclude_mechanical_prev),
  r_thresh     = 0.20,
  dir_output   = dir_output,
  file_stub    = "confounder_screen_prevention_STATE"
)


# ---- 6b. Variance decomposition ----
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
## 7.  FIT REGRESSION MODELS
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
# LENS 1 — BURDEN (panel with year FE)
#   Outcome:  log(daly_per_capita)
#   Exposure: log(spending_per_capita)
#   Design:   Full panel, year fixed effects, state-clustered SEs (CR2)
# ==============================================================

# 1.1 Bivariate (spending + year FE only)
register_model(
  lens = "burden", spec = "bivariate",
  formula = "log_daly_per_capita ~ log_spending_per_capita + year_factor",
  data = df_hiv, is_final = TRUE
)

# 1.2 Primary (committee-recommended adjusted model)
register_model(
  lens = "burden", spec = "primary",
  formula = "log_daly_per_capita ~ log_spending_per_capita + year_factor +
              race_prop_BLCK + race_prop_HISP +
              log_prop_homeless + log_prevalence_per_100k",
  data = df_hiv, is_final = TRUE
)

# 1.3 No prevalence (drop prevalence covariate — sensitivity)
register_model(
  lens = "burden", spec = "no_prev",
  formula = "log_daly_per_capita ~ log_spending_per_capita + year_factor +
              race_prop_BLCK + race_prop_HISP +
              log_prop_homeless",
  data = df_hiv, is_final = TRUE
)

# 1.4 Lagged prevalence (prevalence at t-1 instead of contemporaneous)
register_model(
  lens = "burden", spec = "lag_prev",
  formula = "log_daly_per_capita ~ log_spending_per_capita + year_factor +
              race_prop_BLCK + race_prop_HISP +
              log_prop_homeless + log_prevalence_per_100k_l1",
  data = df_hiv, is_final = TRUE
)

# 1.5 Lagged spending (1 year)
register_model(
  lens = "burden", spec = "lag_spending_l1",
  formula = "log_daly_per_capita ~ log_spending_per_capita_l1 + year_factor +
              race_prop_BLCK + race_prop_HISP +
              log_prop_homeless + log_prevalence_per_100k",
  data = df_hiv, is_final = FALSE
)

# 1.6 Lagged spending (2 years)
register_model(
  lens = "burden", spec = "lag_spending_l2",
  formula = "log_daly_per_capita ~ log_spending_per_capita_l2 + year_factor +
              race_prop_BLCK + race_prop_HISP +
              log_prop_homeless + log_prevalence_per_100k",
  data = df_hiv, is_final = FALSE
)

# 1.7 Robustness: + log(LDI per capita)
register_model(
  lens = "burden", spec = "robustness_ldi",
  formula = "log_daly_per_capita ~ log_spending_per_capita + year_factor +
              race_prop_BLCK + race_prop_HISP +
              log_prop_homeless + log_prevalence_per_100k +
              log_ldi_pc",
  data = df_hiv, is_final = FALSE
)

# 1.8 Robustness: + unemployment rate
register_model(
  lens = "burden", spec = "robustness_unemployment",
  formula = "log_daly_per_capita ~ log_spending_per_capita + year_factor +
              race_prop_BLCK + race_prop_HISP +
              log_prop_homeless + log_prevalence_per_100k +
              unemployment_rate",
  data = df_hiv, is_final = FALSE
)

# 1.9 Between (cross-sectional state means, HC2 SEs)
register_model(
  lens = "burden", spec = "between_true",
  formula = "log_daly_per_capita ~ log_spending_per_capita +
              race_prop_BLCK + race_prop_HISP +
              log_prop_homeless + log_prevalence_per_100k",
  data = df_between, is_final = TRUE
)


# ==============================================================
# LENS 2 — PREVENTION (panel with year FE)
#   Outcome:  log(incidence_per_100k)
#   Exposure: log(spending_per_capita)
# ==============================================================

# 2.1 Bivariate
register_model(
  lens = "prevention", spec = "bivariate_inc",
  formula = "log_incidence_per_100k ~ log_spending_per_capita + year_factor",
  data = df_hiv, is_final = TRUE
)

# 2.2 Primary
register_model(
  lens = "prevention", spec = "primary_inc",
  formula = "log_incidence_per_100k ~ log_spending_per_capita + year_factor +
              race_prop_BLCK + race_prop_HISP +
              log_prop_homeless",
  data = df_hiv, is_final = TRUE
)

# 2.3 Lagged spending (1 year)
register_model(
  lens = "prevention", spec = "lag_spending_l1_inc",
  formula = "log_incidence_per_100k ~ log_spending_per_capita_l1 + year_factor +
              race_prop_BLCK + race_prop_HISP +
              log_prop_homeless",
  data = df_hiv, is_final = FALSE
)

# 2.4 Lagged spending (2 years)
register_model(
  lens = "prevention", spec = "lag_spending_l2_inc",
  formula = "log_incidence_per_100k ~ log_spending_per_capita_l2 + year_factor +
              race_prop_BLCK + race_prop_HISP +
              log_prop_homeless",
  data = df_hiv, is_final = FALSE
)


##================================================================
## 8.  EXTRACT RESULTS WITH ROBUST STANDARD ERRORS
##================================================================
##  - Cross-sectional between models -> HC2
##  - Panel models                   -> CR2 + Satterthwaite

extract_clustered <- function(model, model_id, model_data_df) {
  is_between <- grepl("between_true", model_id)
  
  if (is_between) {
    # HC2 heteroskedasticity-robust SEs
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
    # CR2 cluster-robust on location_id + Satterthwaite df
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
    ),
    acause = "hiv",
    lens   = str_match(model_id, "^hiv__([^_]+)__")[, 2],
    spec   = str_replace(model_id, "^hiv__[^_]+__", "")
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


# ---- Spending coefficient quick-reference summary ----
spending_summary <- regression_results %>%
  dplyr::filter(grepl("log_spending_per_capita", term)) %>%
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


# ---- Model registry ----
write.csv(model_registry,
          file.path(dir_output, "model_registry_per_capita.csv"),
          row.names = FALSE)


cat("\n================ DONE ================\n")
cat("Outputs written to:", dir_output, "\n")
cat("Burden lens models:    ", sum(model_registry$lens == "burden"), "\n")
cat("Prevention lens models:", sum(model_registry$lens == "prevention"), "\n\n")

##================================================================
## END OF PIPELINE
##================================================================