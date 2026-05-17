##================================================================
##  Title:     C_HIV_per_capita_models.R
##  Folder:    C_frontier_analysis/
##  Purpose:   HIV regression analysis — POST-MAY-6-COMMITTEE per-capita framework.
##
##  Four lenses, all with HIV spending per capita as exposure:
##
##    Lens 1A — BURDEN (DALYs)
##      Outcome:  log(daly_per_capita)
##      Primary burden control: log(incidence_per_100k)
##
##    Lens 1B — BURDEN (GBD Mortality)
##      Outcome:  log(mortality_per_capita)  (GBD: HIV as underlying cause)
##      Same covariate structure as Lens 1A.
##
##    Lens 1C — BURDEN (CDC Mortality)  [NEW]
##      Outcome:  log(cdc_mortality_per_capita)
##                = log(cdc_hiv_mortality_counts / dex_pop)
##      Same covariate structure as Lens 1A/1B.
##      WHAT IT MEASURES (differs from Lens 1B):
##        CDC mortality = all-cause deaths among people living with HIV.
##        GBD mortality = deaths where HIV is the underlying cause.
##        CDC counts > GBD counts (by definition).
##      SCOPE:
##        CDC Atlas data is published for ages 25+ only. Lens 1C is
##        therefore a 25+-restricted analysis — panel size will be
##        slightly smaller than 1A/1B.
##      WHY ADD IT:
##        Clinical audiences care about all-cause mortality in PLWH
##        (a treatment effect of HIV care also reduces non-HIV
##        comorbidity deaths via better engagement). This outcome is
##        a complement to Lens 1B, not a replacement.
##
##    Lens 2 — PREVENTION
##      Outcome:  log(incidence_per_100k)
##      Primary burden control: log(prevalence_per_100k)_L1
##
##  SIGN INTERPRETATION NOTE
##    Positive coefficient on log_spending_per_capita in a burden lens
##    indicates allocation (states with more burden get more funding),
##    NOT a causal harm of spending. The Mundlak _W coefficient is the
##    closest available within-state-causal read.
##
##  Standard errors:
##    Panel models      -> clubSandwich::vcovCR + Satterthwaite (CR2)
##    Cross-sectional   -> sandwich::vcovHC type "HC2"
##
##  Inputs:
##    df_as_processed_rw_gbd.csv   (prep_B output — Lens 1A/1B/2)
##    df_as_cdc.csv                (prep_A output — for Lens 1C only)
##================================================================


##================================================================
## 1.  SETUP & DATA LOADING
##================================================================
rm(list = ls())

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

user_lib <- file.path(h, "R_packages")
if (!dir.exists(user_lib)) dir.create(user_lib, recursive = TRUE)
.libPaths(c(user_lib, .libPaths()))

pacman::p_load(
  data.table, tidyverse, glue, broom,
  lmtest, sandwich, clubSandwich,
  Hmisc
)

if ("plotly" %in% loadedNamespaces()) filter <- dplyr::filter
tryCatch(conflicted::conflicts_prefer(dplyr::filter, .quiet = TRUE),
         error = function(e) invisible(NULL))

# ---------- IO ----------
input_date  <- "20260515"   # <-- EDIT: date of prep_B output (df_as_processed_rw_gbd.csv)
cdc_date    <- input_date   # <-- EDIT if prep_A (df_as_cdc.csv) was run on a different day
dir_input   <- file.path(h, "aim_outputs/Aim2/C_frontier_analysis", input_date)
dir_cdc     <- file.path(h, "aim_outputs/Aim2/C_frontier_analysis", cdc_date)

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
       "\nRe-run prep_A.R + prep_B.R with the per-capita patches first.")
}

toc_cols <- c("spend_AM", "spend_ED", "spend_HH", "spend_IP", "spend_NF", "spend_RX")
toc_available <- all(toc_cols %in% names(df_as))
if (!toc_available) {
  message("NOTE: not all TOC columns present. By-TOC decomposition will be skipped.")
}


##================================================================
## 1.1  LOAD CDC DATA (FOR LENS 1C — CDC MORTALITY)
##================================================================
##  Reads the RAW CDC AtlasPlus export directly from R_resources/CDC_data/.
##  We bypass prep_A's CDC processing block because that block currently
##  references a `df_cdc` object that is never explicitly loaded (a known
##  bug — section 8 of C_regression_prep_A.R), so df_as_cdc.csv is not
##  reliably produced. Reading raw here means Lens 1C works without
##  depending on prep_A's CDC pipeline.
##
##  CDC data scope: 25+ only. We sum cases across CDC age groups
##  (25-34, 35-44, 45-54, 55-64, 65+) within each (Year, Geography)
##  to get state-year totals.
##================================================================

fp_cdc_raw <- file.path(h, "aim_outputs/Aim2/R_resources/CDC_data",
                        "hiv_mort_prev_data.csv")
cdc_available <- file.exists(fp_cdc_raw)

if (cdc_available) {
  # Find the header row dynamically (file has metadata at the top)
  raw_lines <- readLines(fp_cdc_raw, n = 50, encoding = "latin1")
  header_row <- which(grepl("^Indicator,Year,Geography", raw_lines))
  if (length(header_row) == 0) {
    message("Could not locate header row in CDC raw file — disabling Lens 1C")
    cdc_available <- FALSE
  } else {
    df_cdc_raw <- read.csv(fp_cdc_raw, skip = header_row - 1,
                           stringsAsFactors = FALSE,
                           na.strings = c("", "NA", "Data not available"))
    # Columns: Indicator, Year, Geography, FIPS, Age Group, Cases, Rate per 100000
    names(df_cdc_raw)[1:6] <- c("Indicator", "Year", "Geography",
                                "FIPS", "Age_Group", "Cases")

    # Parse Cases (strip commas, coerce numeric); drop suppressed rows
    df_cdc_raw <- df_cdc_raw %>%
      mutate(
        Year  = as.integer(Year),
        Cases = suppressWarnings(as.numeric(gsub(",", "", as.character(Cases))))
      ) %>%
      dplyr::filter(!is.na(Cases))

    # State-name cleanup consistent with existing pipeline pattern
    df_cdc_raw <- df_cdc_raw %>%
      mutate(Geography = case_when(
        Geography == "Mississippi^"   ~ "Mississippi",
        Geography == "West Virginia^" ~ "West Virginia",
        TRUE                          ~ Geography))

    # Map indicator names to clean snake_case
    df_cdc_raw <- df_cdc_raw %>%
      mutate(measure = case_when(
        Indicator == "HIV deaths"     ~ "cdc_hiv_mortality_counts",
        Indicator == "HIV prevalence" ~ "cdc_hiv_prevalence_counts",
        TRUE                          ~ NA_character_
      )) %>%
      dplyr::filter(!is.na(measure))

    # Sum across CDC age groups within each state-year
    df_cdc_point <- df_cdc_raw %>%
      group_by(Geography, Year, measure) %>%
      summarise(total = sum(Cases, na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(names_from = measure, values_from = total) %>%
      rename(location_name = Geography, year_id = Year)

    # Sanity: how much did we load?
    cat("CDC raw data loaded: ", nrow(df_cdc_point), " state-year rows; ",
        "mortality non-NA: ", sum(!is.na(df_cdc_point$cdc_hiv_mortality_counts)),
        ", prevalence non-NA: ", sum(!is.na(df_cdc_point$cdc_hiv_prevalence_counts)),
        "\n", sep = "")
  }
} else {
  message("CDC raw file not found at: ", fp_cdc_raw,
          "\nLens 1C (CDC mortality) will be skipped.")
}


##================================================================
## 2.  COMPUTE PER-CAPITA / PER-100K VARIABLES
##================================================================
safe_log <- function(x) if_else(x > 0, log(x), NA_real_)

df_hiv <- df_as %>%
  dplyr::filter(acause == "hiv") %>%
  mutate(
    spending_per_capita   = (ryan_white_funding_final + spend_all) / dex_pop,
    daly_per_capita       = daly_counts      / dex_pop,
    mortality_per_capita  = mortality_counts / dex_pop,
    incidence_per_100k    = (incidence_counts  / dex_pop) * 1e5,
    prevalence_per_100k   = (prevalence_counts / dex_pop) * 1e5,
    log_spending_per_capita   = safe_log(spending_per_capita),
    log_daly_per_capita       = safe_log(daly_per_capita),
    log_mortality_per_capita  = safe_log(mortality_per_capita),
    log_incidence_per_100k    = safe_log(incidence_per_100k),
    log_prevalence_per_100k   = safe_log(prevalence_per_100k)
  )

# Merge CDC mortality (Lens 1C) by (location_name, year_id)
if (cdc_available) {
  df_hiv <- df_hiv %>%
    left_join(df_cdc_point, by = c("location_name", "year_id")) %>%
    mutate(
      cdc_mortality_per_capita     = cdc_hiv_mortality_counts  / dex_pop,
      cdc_prevalence_per_100k      = (cdc_hiv_prevalence_counts / dex_pop) * 1e5,
      log_cdc_mortality_per_capita = safe_log(cdc_mortality_per_capita),
      log_cdc_prevalence_per_100k  = safe_log(cdc_prevalence_per_100k)
    )
  n_cdc_avail <- sum(!is.na(df_hiv$log_cdc_mortality_per_capita))
  cat("Lens 1C analytic rows (non-NA log_cdc_mortality_per_capita): ",
      n_cdc_avail, " / ", nrow(df_hiv), "\n", sep = "")
}

# TOC-level per-capita spending
if (toc_available) {
  df_hiv <- df_hiv %>%
    mutate(
      spend_pharma_pc      = spend_RX / dex_pop,
      spend_ambulatory_pc  = spend_AM / dex_pop,
      spend_inpatient_pc   = spend_IP / dex_pop,
      spend_nf_pc          = spend_NF / dex_pop,
      log_spend_pharma_pc      = safe_log(spend_pharma_pc),
      log_spend_ambulatory_pc  = safe_log(spend_ambulatory_pc),
      log_spend_inpatient_pc   = safe_log(spend_inpatient_pc),
      log_spend_nf_pc          = safe_log(spend_nf_pc)
    )
}

df_hiv <- df_hiv %>%
  dplyr::filter(!is.na(log_daly_per_capita),
                !is.na(log_mortality_per_capita),
                !is.na(log_spending_per_capita),
                !is.na(dex_pop), dex_pop > 0)

cat("\n---- Per-capita variable summaries ----\n")
print(summary(df_hiv$spending_per_capita))
print(summary(df_hiv$daly_per_capita))
print(summary(df_hiv$mortality_per_capita))
if (cdc_available) print(summary(df_hiv$cdc_mortality_per_capita))
print(summary(df_hiv$incidence_per_100k))
print(summary(df_hiv$prevalence_per_100k))


##================================================================
## 3.  COVARIATE LOGS, LAGS, YEAR FACTOR
##================================================================
df_hiv <- df_hiv %>%
  mutate(
    log_prop_homeless = safe_log(prop_homeless),
    log_ldi_pc        = safe_log(ldi_pc),
    year_factor       = factor(year_id)
  )

df_hiv <- df_hiv %>%
  arrange(location_id, year_id) %>%
  group_by(location_id) %>%
  mutate(
    log_spending_per_capita_l1 = dplyr::lag(log_spending_per_capita, 1),
    log_spending_per_capita_l2 = dplyr::lag(log_spending_per_capita, 2),
    log_prevalence_per_100k_l1 = dplyr::lag(log_prevalence_per_100k, 1),
    log_prevalence_per_100k_l2 = dplyr::lag(log_prevalence_per_100k, 2),
    log_incidence_per_100k_l1  = dplyr::lag(log_incidence_per_100k,  1)
  ) %>%
  ungroup()


##================================================================
## 4.  MUNDLAK B/W DECOMPOSITION
##================================================================
mundlak_vars <- c(
  "log_spending_per_capita", "log_incidence_per_100k",
  "log_prevalence_per_100k", "race_prop_BLCK", "race_prop_HISP",
  "log_prop_homeless", "unemployment_rate"
)
mundlak_vars <- intersect(mundlak_vars, names(df_hiv))

for (v in mundlak_vars) {
  df_hiv <- df_hiv %>%
    group_by(location_id) %>%
    mutate(!!paste0(v, "_B") := mean(.data[[v]], na.rm = TRUE),
           !!paste0(v, "_W") := .data[[v]] - mean(.data[[v]], na.rm = TRUE)) %>%
    ungroup()
}


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
  summarise(across(all_of(collapse_cols), \(x) mean(x, na.rm = TRUE)),
            n_years = dplyr::n(),
            .groups = "drop")

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
  "cdc_mortality_per_capita", "log_cdc_mortality_per_capita",
  "incidence_per_100k",   "log_incidence_per_100k",
  "prevalence_per_100k",  "log_prevalence_per_100k",
  "cdc_prevalence_per_100k", "log_cdc_prevalence_per_100k",
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
  if (!(exposure %in% names(df)) || !(outcome %in% names(df))) {
    message("Skipping ", file_stub, " — missing exposure/outcome column.")
    return(NULL)
  }

  mat <- df %>% select(all_of(c(exposure, outcome, candidates))) %>% as.matrix()
  rc  <- Hmisc::rcorr(mat)

  pull_stats <- function(target) {
    r <- rc$r[candidates, target]; p <- rc$P[candidates, target]
    tibble(var = candidates,
           !!paste0("r_", target)   := as.numeric(r),
           !!paste0("p_", target)   := as.numeric(p),
           !!paste0("dir_", target) := sign(as.numeric(r)))
  }
  ex_stats  <- pull_stats(exposure); out_stats <- pull_stats(outcome)
  res <- ex_stats %>% left_join(out_stats, by = "var") %>%
    rename(r_exp = !!paste0("r_", exposure), p_exp = !!paste0("p_", exposure),
           dir_exp = !!paste0("dir_", exposure),
           r_out = !!paste0("r_", outcome), p_out = !!paste0("p_", outcome),
           dir_out = !!paste0("dir_", outcome)) %>%
    mutate(pass = (abs(r_exp) >= r_thresh & abs(r_out) >= r_thresh)) %>%
    select(var, r_exp, dir_exp, p_exp, r_out, dir_out, p_out, pass) %>%
    arrange(desc(abs(r_exp) + abs(r_out)))

  if (!is.null(dir_output)) {
    readr::write_csv(res, file.path(dir_output, paste0(file_stub, "_ALL.csv")))
    readr::write_csv(res %>% dplyr::filter(pass),
                     file.path(dir_output, paste0(file_stub, "_SHORTLIST.csv")))
  }
  list(all = res)
}

exclude_base <- c("cause_id", "year_id", "location_id")
exclude_mechanical_burden <- c(
  "spending_per_capita", "log_spending_per_capita",
  "spend_all", "spend_mdcd", "spend_mdcr", "spend_oop", "spend_priv",
  "spend_AM", "spend_ED", "spend_HH", "spend_IP", "spend_NF", "spend_RX",
  "spend_pharma_pc", "spend_ambulatory_pc", "spend_inpatient_pc",
  "spend_nf_pc",
  "log_spend_pharma_pc", "log_spend_ambulatory_pc",
  "log_spend_inpatient_pc", "log_spend_nf_pc",
  "ryan_white_funding_final",
  "as_spend_per_capita", "as_spend_prev_ratio", "as_spend_prev_ratio_log",
  "rw_dex_hiv_prev_ratio", "rw_dex_hiv_prev_ratio_log",
  "rw_hiv_prev_ratio", "rw_hiv_prev_ratio_log",
  # outcome side — exclude all burden + cdc outcomes from the candidate pool
  "daly_per_capita", "log_daly_per_capita", "daly_counts",
  "mortality_per_capita", "log_mortality_per_capita", "mortality_counts",
  "cdc_mortality_per_capita", "log_cdc_mortality_per_capita",
  "cdc_hiv_mortality_counts", "cdc_hiv_prevalence_counts",
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

screen_confounders(df_hiv, "log_spending_per_capita", "log_daly_per_capita",
                   c(exclude_base, exclude_mechanical_burden), 0.20,
                   dir_output, "confounder_screen_burden_PANEL")
screen_confounders(df_between, "log_spending_per_capita", "log_daly_per_capita",
                   c(exclude_base, exclude_mechanical_burden), 0.20,
                   dir_output, "confounder_screen_burden_STATE")
screen_confounders(df_hiv, "log_spending_per_capita", "log_mortality_per_capita",
                   c(exclude_base, exclude_mechanical_burden), 0.20,
                   dir_output, "confounder_screen_burden_mort_PANEL")
if (cdc_available) {
  screen_confounders(df_hiv, "log_spending_per_capita",
                     "log_cdc_mortality_per_capita",
                     c(exclude_base, exclude_mechanical_burden), 0.20,
                     dir_output, "confounder_screen_burden_cdc_mort_PANEL")
}
screen_confounders(df_hiv, "log_spending_per_capita", "log_incidence_per_100k",
                   c(exclude_base, exclude_mechanical_prev), 0.20,
                   dir_output, "confounder_screen_prevention_PANEL")
screen_confounders(df_between, "log_spending_per_capita", "log_incidence_per_100k",
                   c(exclude_base, exclude_mechanical_prev), 0.20,
                   dir_output, "confounder_screen_prevention_STATE")


# ---- Variance decomposition ----
calc_variance_decomp <- function(df_panel, var_name, group_var = "location_id") {
  if (!(var_name %in% names(df_panel))) return(NULL)
  x <- df_panel[[var_name]]
  total_var <- var(x, na.rm = TRUE)
  state_means <- df_panel %>% group_by(across(all_of(group_var))) %>%
    summarise(xbar = mean(.data[[var_name]], na.rm = TRUE), .groups = "drop")
  between_var <- var(state_means$xbar, na.rm = TRUE)
  df_tmp <- df_panel %>% left_join(
    state_means %>% rename(!!paste0(var_name, "_mean") := xbar),
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
  calc_variance_decomp(df_hiv, "log_cdc_mortality_per_capita"),
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
  fit <- tryCatch(lm(as.formula(formula), data = data),
                  error = function(e) {
                    warning("Fit failed for ", model_id, ": ", conditionMessage(e))
                    NULL
                  })
  if (is.null(fit)) return(invisible(NULL))
  list_models[[model_id]]  <<- fit
  model_data[[model_id]]   <<- data
  model_registry <<- bind_rows(
    model_registry,
    tibble(model_id = model_id, lens = lens, spec = spec, is_final = is_final)
  )
  invisible(fit)
}

# Helper to build the parallel burden-lens spec set for ANY outcome
register_burden_family <- function(lens, outcome_col) {
  # 1. bivariate
  register_model(lens, "bivariate",
    paste0(outcome_col, " ~ log_spending_per_capita + year_factor"),
    df_hiv, is_final = TRUE)
  # 2. primary (incidence as burden proxy)
  register_model(lens, "primary",
    paste0(outcome_col, " ~ log_spending_per_capita + year_factor +
            race_prop_BLCK + race_prop_HISP +
            log_prop_homeless + log_incidence_per_100k"),
    df_hiv, is_final = TRUE)
  # 3. no_burden_control
  register_model(lens, "no_burden_control",
    paste0(outcome_col, " ~ log_spending_per_capita + year_factor +
            race_prop_BLCK + race_prop_HISP + log_prop_homeless"),
    df_hiv, is_final = TRUE)
  # 4. sens_prev_control
  register_model(lens, "sens_prev_control",
    paste0(outcome_col, " ~ log_spending_per_capita + year_factor +
            race_prop_BLCK + race_prop_HISP +
            log_prop_homeless + log_prevalence_per_100k"),
    df_hiv, is_final = TRUE)
  # 5. sens_lag_prev
  register_model(lens, "sens_lag_prev",
    paste0(outcome_col, " ~ log_spending_per_capita + year_factor +
            race_prop_BLCK + race_prop_HISP +
            log_prop_homeless + log_prevalence_per_100k_l1"),
    df_hiv, is_final = TRUE)
  # 6. sens_lag_incidence
  register_model(lens, "sens_lag_incidence",
    paste0(outcome_col, " ~ log_spending_per_capita + year_factor +
            race_prop_BLCK + race_prop_HISP +
            log_prop_homeless + log_incidence_per_100k_l1"),
    df_hiv, is_final = TRUE)
  # 7. lag_spending_l1
  register_model(lens, "lag_spending_l1",
    paste0(outcome_col, " ~ log_spending_per_capita_l1 + year_factor +
            race_prop_BLCK + race_prop_HISP +
            log_prop_homeless + log_incidence_per_100k"),
    df_hiv, is_final = FALSE)
  # 8. lag_spending_l2
  register_model(lens, "lag_spending_l2",
    paste0(outcome_col, " ~ log_spending_per_capita_l2 + year_factor +
            race_prop_BLCK + race_prop_HISP +
            log_prop_homeless + log_incidence_per_100k"),
    df_hiv, is_final = FALSE)
  # 9. robustness_ldi
  register_model(lens, "robustness_ldi",
    paste0(outcome_col, " ~ log_spending_per_capita + year_factor +
            race_prop_BLCK + race_prop_HISP +
            log_prop_homeless + log_incidence_per_100k + log_ldi_pc"),
    df_hiv, is_final = FALSE)
  # 10. robustness_unemployment
  register_model(lens, "robustness_unemployment",
    paste0(outcome_col, " ~ log_spending_per_capita + year_factor +
            race_prop_BLCK + race_prop_HISP +
            log_prop_homeless + log_incidence_per_100k +
            unemployment_rate"),
    df_hiv, is_final = FALSE)
  # 11. mundlak
  register_model(lens, "mundlak",
    paste0(outcome_col, " ~
            log_spending_per_capita_B + log_spending_per_capita_W +
            log_incidence_per_100k_B  + log_incidence_per_100k_W +
            race_prop_BLCK_B + race_prop_BLCK_W +
            race_prop_HISP_B + race_prop_HISP_W +
            log_prop_homeless_B + log_prop_homeless_W +
            year_factor"),
    df_hiv, is_final = TRUE)
  # 12. between_true (cross-sectional state means)
  register_model(lens, "between_true",
    paste0(outcome_col, " ~ log_spending_per_capita +
            race_prop_BLCK + race_prop_HISP +
            log_prop_homeless + log_incidence_per_100k"),
    df_between, is_final = TRUE)
  # 13. primary_by_toc (conditional on TOC availability)
  if (toc_available) {
    register_model(lens, "primary_by_toc",
      paste0(outcome_col, " ~
              log_spend_pharma_pc + log_spend_ambulatory_pc +
              log_spend_inpatient_pc + log_spend_nf_pc +
              year_factor + race_prop_BLCK + race_prop_HISP +
              log_prop_homeless + log_incidence_per_100k"),
      df_hiv, is_final = FALSE)
  }
}

# Lens 1A — DALYs
register_burden_family("burden",      "log_daly_per_capita")

# Lens 1B — GBD mortality (HIV underlying cause)
register_burden_family("burden_mort", "log_mortality_per_capita")

# Lens 1C — CDC mortality (all-cause among PLWH, 25+)
if (cdc_available) {
  register_burden_family("burden_cdc_mort", "log_cdc_mortality_per_capita")
}

# Lens 2 — Prevention (incidence)
register_model("prevention", "bivariate_inc",
  "log_incidence_per_100k ~ log_spending_per_capita + year_factor",
  df_hiv, is_final = TRUE)
register_model("prevention", "primary_inc",
  "log_incidence_per_100k ~ log_spending_per_capita + year_factor +
    race_prop_BLCK + race_prop_HISP +
    log_prop_homeless + log_prevalence_per_100k_l1",
  df_hiv, is_final = TRUE)
register_model("prevention", "primary_inc_no_burden",
  "log_incidence_per_100k ~ log_spending_per_capita + year_factor +
    race_prop_BLCK + race_prop_HISP + log_prop_homeless",
  df_hiv, is_final = TRUE)
register_model("prevention", "primary_inc_lag2prev",
  "log_incidence_per_100k ~ log_spending_per_capita + year_factor +
    race_prop_BLCK + race_prop_HISP +
    log_prop_homeless + log_prevalence_per_100k_l2",
  df_hiv, is_final = TRUE)
register_model("prevention", "lag_spending_l1_inc",
  "log_incidence_per_100k ~ log_spending_per_capita_l1 + year_factor +
    race_prop_BLCK + race_prop_HISP +
    log_prop_homeless + log_prevalence_per_100k_l1",
  df_hiv, is_final = FALSE)
register_model("prevention", "lag_spending_l2_inc",
  "log_incidence_per_100k ~ log_spending_per_capita_l2 + year_factor +
    race_prop_BLCK + race_prop_HISP +
    log_prop_homeless + log_prevalence_per_100k_l1",
  df_hiv, is_final = FALSE)
register_model("prevention", "mundlak_inc",
  "log_incidence_per_100k ~
    log_spending_per_capita_B + log_spending_per_capita_W +
    log_prevalence_per_100k_B + log_prevalence_per_100k_W +
    race_prop_BLCK_B + race_prop_BLCK_W +
    race_prop_HISP_B + race_prop_HISP_W +
    log_prop_homeless_B + log_prop_homeless_W +
    year_factor",
  df_hiv, is_final = TRUE)
if (toc_available) {
  register_model("prevention", "primary_by_toc",
    "log_incidence_per_100k ~
      log_spend_pharma_pc + log_spend_ambulatory_pc +
      log_spend_inpatient_pc + log_spend_nf_pc +
      year_factor + race_prop_BLCK + race_prop_HISP +
      log_prop_homeless + log_prevalence_per_100k_l1",
    df_hiv, is_final = FALSE)
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
      tibble(term = rownames(ct), estimate = ct[, "Estimate"],
             std.error = ct[, "Std. Error"], statistic = ct[, "t value"],
             p.value = ct[, "Pr(>|t|)"], model_id = model_id)
    }, error = function(e) broom::tidy(model) %>% mutate(model_id = model_id))
  } else {
    tryCatch({
      cluster_var <- model_data_df$location_id
      vcov_cr <- vcovCR(model, cluster = cluster_var, type = "CR2")
      ct      <- coef_test(model, vcov = vcov_cr, test = "Satterthwaite")
      tibble(term = rownames(ct), estimate = ct$beta, std.error = ct$SE,
             statistic = ct$tstat, p.value = ct$p_Satt, model_id = model_id)
    }, error = function(e) broom::tidy(model) %>% mutate(model_id = model_id))
  }
}

coef_tbl <- map_dfr(names(list_models), function(nm) {
  extract_clustered(list_models[[nm]], nm, model_data[[nm]])
})

coef_tbl <- coef_tbl %>%
  left_join(model_registry %>% select(model_id, lens, spec), by = "model_id") %>%
  mutate(acause = "hiv",
         signif_stars = case_when(
           p.value < 0.001 ~ "***", p.value < 0.01 ~ "**",
           p.value < 0.05  ~ "*",   TRUE          ~ ""),
         signif_label = case_when(
           p.value < 0.001 ~ "p < 0.001", p.value < 0.01 ~ "p < 0.01",
           p.value < 0.05  ~ "p < 0.05",  TRUE          ~ "not significant"))

metrics_tbl <- imap_dfr(list_models, function(model, model_id) {
  g <- broom::glance(model)
  tibble(model_id = model_id, n = g$nobs,
         r2 = g$r.squared, adj_r2 = g$adj.r.squared,
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
  filter(grepl("log_spending_per_capita|log_spend_pharma_pc|log_spend_ambulatory_pc|log_spend_inpatient_pc|log_spend_nf_pc",
               term)) %>%
  select(model_id, lens, spec, term, estimate, std.error,
         p.value, signif_label, n, adj_r2) %>%
  mutate(estimate = round(estimate, 5),
         std.error = round(std.error, 5),
         p.value   = round(p.value, 4))

write.csv(spending_summary,
          file.path(dir_output, "spending_coefficients_summary_per_capita.csv"),
          row.names = FALSE)
write.csv(model_registry,
          file.path(dir_output, "model_registry_per_capita.csv"),
          row.names = FALSE)


cat("\n================ DONE ================\n")
cat("Outputs written to:        ", dir_output, "\n")
cat("Burden (DALY) models:      ", sum(model_registry$lens == "burden"), "\n")
cat("Burden (GBD mort) models:  ", sum(model_registry$lens == "burden_mort"), "\n")
cat("Burden (CDC mort) models:  ", sum(model_registry$lens == "burden_cdc_mort"), "\n")
cat("Prevention models:         ", sum(model_registry$lens == "prevention"), "\n")
cat("Total models fit:          ", nrow(model_registry), "\n")
cat("By-TOC included:           ", toc_available, "\n")
cat("CDC mortality lens included: ", cdc_available, "\n\n")

# Quick headline: bivariate + primary spending coefficient across the three burden lenses
cat("---- Headline: spending coefficient (bivariate + primary) by lens ----\n")
print(as.data.frame(
  regression_results %>%
    dplyr::filter(grepl("^burden", lens),
                  spec %in% c("bivariate", "primary"),
                  term == "log_spending_per_capita") %>%
    select(lens, spec, estimate, std.error, p.value, signif_label, n)
))

##================================================================
## END OF PIPELINE
##================================================================
