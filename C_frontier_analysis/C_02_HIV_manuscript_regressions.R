##----------------------------------------------------------------
##  Title:    aim2_regressions_v4.R
##  Purpose:  End-to-end regression pipeline for Aim 2 HIV paper (v4).
##
##  DESIGN CHANGES FROM v3:
##    1. Tercile stratification applied ONLY where X is per-case spending,
##       using the SAME prevalence source on both sides:
##         - per-case GBD  -> GBD-prevalence tercile (2010)
##         - per-case CDC  -> CDC-prevalence tercile (first non-missing
##                            year 2010-2012 per state)
##         - per-capita    -> NO stratification (denominator is population,
##                            not prevalence; Marcia's concern does not apply)
##         - M -> Y        -> pooled only (M is a proportion, not a ratio
##                            of differently-sized epidemics)
##    2. Primary outcome = DALYs per GBD prev case.  Mortality moves to a
##       sensitivity CSV with parallel structure.
##    3. Output is a TIERED BUNDLE OF CSVs (no XLSX).  Each result block
##       is a separate file; long-format columns are standardized so
##       results can be filtered or pivoted downstream.
##    4. Headline summary CSV leads with the burden-lens c-total at n=510
##       (the proper reduced-form estimate, larger than the matched-sample
##       mediation c-total).
##    5. Sample construction note CSV explains where each n comes from.
##    6. PRIMARY mediator switched to the full 90-90-90 cascade product
##       (K x L x V = knowledge of status x linkage to care x viral suppression).
##       K x V is retained as a sensitivity composite alongside K, L, V
##       individually for transparency.
##
##  PRIMARY FRAMEWORK:
##    - Lens:     per-case GBD
##    - Time:     all current year (X(t), M(t), Y(t))
##    - Control:  contemporaneous log(HIV incidence per 100k) + race +
##                log(homeless) + year FE
##    - Mediator: K x L x V composite (knowledge x linkage x viral suppression)
##                = full 90-90-90 cascade product, per UNAIDS framework.
##                K x V retained as a sensitivity composite.
##    - Outcome:  log(DALYs per GBD prev case), year t
##    - Strata:   GBD-prevalence tercile, 2010 baseline
##    - SEs:      clubSandwich CR2 + Satterthwaite (lm); sandwich HC0 (glm)
##    - Bootstrap: state-block, 500 reps, seed 20260520, percentile CI
##
##  Author / runner: Bulat Idrisov  (script by Claude, 2026-05-20)
##----------------------------------------------------------------

##================================================================
## 0. SETUP
##================================================================
rm(list = ls())

## ---- paths -----------------------------------------------------
if (Sys.info()[["sysname"]] == "Linux") {
  h <- paste0("/ihme/homes/", Sys.info()[["user"]], "/")
} else if (Sys.info()[["sysname"]] == "Darwin") {
  h <- paste0("/Volumes/", Sys.info()[["user"]], "/")
} else {
  h <- "H:/"
}

panel_input_path <- file.path(
  h, "aim_outputs/Aim2/C_frontier_analysis",
  "20260517", "analysis_per_capita",
  "df_hiv_cascade_panel.csv"
)
output_date <- format(Sys.time(), "%Y%m%d_%H%M")
dir_output  <- file.path(
  h, "aim_outputs/Aim2/C_frontier_analysis",
  output_date, "analytic_plan_regressions_v4"
)
if (!dir.exists(dir_output)) dir.create(dir_output, recursive = TRUE)

## ---- libraries -------------------------------------------------
user_lib <- file.path(h, "R_packages")
if (!dir.exists(user_lib)) dir.create(user_lib, recursive = TRUE)
.libPaths(c(user_lib, .libPaths()))

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(
  data.table, tidyverse, glue, broom,
  lmtest, sandwich, clubSandwich,
  ggplot2, scales
)
if (!requireNamespace("marginaleffects", quietly = TRUE)) {
  tryCatch(install.packages("marginaleffects"),
           error = function(e) message("marginaleffects unavailable; using manual AME"))
}

## Resolve dplyr <-> data.table name collisions
tryCatch({
  conflicted::conflicts_prefer(
    dplyr::filter, dplyr::first, dplyr::last,
    dplyr::lag,    dplyr::lead, dplyr::between,
    .quiet = TRUE
  )
}, error = function(e) invisible(NULL))
filter  <- dplyr::filter
first   <- dplyr::first
last    <- dplyr::last
lag     <- dplyr::lag
lead    <- dplyr::lead
between <- dplyr::between

## ---- run log ---------------------------------------------------
log_file <- file.path(dir_output, "run_log.txt")
log_msg <- function(...) {
  msg <- paste0(..., collapse = "")
  cat(msg, "\n")
  cat(msg, "\n", file = log_file, append = TRUE)
}
cat(sprintf("Run: %s\nInput: %s\nOutput: %s\nScript: v4 (tiered CSVs, source-matched terciles, DALYs primary, K x L x V mediator)\n\n",
            Sys.time(), panel_input_path, dir_output),
    file = log_file)

##================================================================
## 1. LOAD AND VALIDATE PANEL
##================================================================
if (!file.exists(panel_input_path)) {
  stop("Input panel not found: ", panel_input_path)
}
df <- read.csv(panel_input_path, stringsAsFactors = FALSE)
df <- df %>% dplyr::filter(acause == "hiv")
log_msg("Loaded ", nrow(df), " rows, ", ncol(df), " columns from cascade panel")

required_cols <- c(
  "location_id", "location_name", "year_id", "acause",
  ## outcomes (per-case current year)
  "as_daly_prev_ratio_log", "as_mort_prev_ratio_log",
  ## outcomes (per-capita current year)
  "log_daly_per_capita", "log_mortality_per_capita",
  "log_cdc_mortality_per_capita", "log_cdc_prevalence_per_100k",
  "log_incidence_per_100k",
  ## predictors
  "rw_dex_hiv_prev_ratio_log",
  "log_spending_per_capita",
  ## lagged versions (sensitivities)
  "rw_dex_hiv_prev_ratio_log_l1",
  "log_spending_per_capita_l1",
  "log_incidence_per_100k_l1",
  ## Mundlak B/W
  "rw_dex_hiv_prev_ratio_log_B", "rw_dex_hiv_prev_ratio_log_W",
  ## demographics
  "race_prop_BLCK", "race_prop_HISP",
  "log_prop_homeless",
  "log_ldi_pc", "edu_yrs", "aca_implemented_status",
  "opioid_prevalence_counts",
  ## prevalence
  "log_prevalence_per_100k",
  ## counts (CDC sensitivities)
  "mortality_counts", "hiv_prevalence_counts",
  "cdc_hiv_mortality_counts", "cdc_hiv_prevalence_counts",
  ## cascade
  "cdc_knowledge_status", "cdc_linkage_1mo", "cdc_viral_suppress",
  ## RW pieces
  "ryan_white_funding_final", "spend_all"
)
missing_required <- setdiff(required_cols, names(df))
if (length(missing_required) > 0) {
  stop("Required columns missing from panel: ", paste(missing_required, collapse = ", "))
}

##================================================================
## 2. SOURCE-MATCHED TERCILE ASSIGNMENTS
##================================================================
## Marcia's denominator-consistency rule applied to stratification:
##   the prevalence source used to define terciles must match the
##   prevalence source used in the spending denominator.
##================================================================

## ---- GBD-prevalence tercile (2010 baseline) --------------------
prev_2010_GBD <- df %>%
  dplyr::filter(year_id == 2010) %>%
  select(location_id, log_prevalence_per_100k) %>%
  rename(log_gbd_prev_2010 = log_prevalence_per_100k)

gbd_cuts <- quantile(prev_2010_GBD$log_gbd_prev_2010,
                     probs = c(1/3, 2/3), na.rm = TRUE)
log_msg("GBD tercile cuts (log GBD prev/100k, 2010): low<",
        sprintf("%.3f", gbd_cuts[1]), "  high>", sprintf("%.3f", gbd_cuts[2]))

prev_2010_GBD <- prev_2010_GBD %>%
  mutate(
    prev_tercile_GBD = case_when(
      log_gbd_prev_2010 <= gbd_cuts[1] ~ "Low",
      log_gbd_prev_2010 <= gbd_cuts[2] ~ "Mid",
      TRUE                              ~ "High"
    ),
    prev_tercile_GBD = factor(prev_tercile_GBD, levels = c("Low", "Mid", "High"))
  )

df <- df %>%
  left_join(prev_2010_GBD %>% select(location_id, prev_tercile_GBD,
                                     log_gbd_prev_2010),
            by = "location_id")
stopifnot(all(!is.na(df$prev_tercile_GBD)))
log_msg("GBD tercile counts (state-years): ",
        paste(sprintf("%s=%d",
                      levels(df$prev_tercile_GBD),
                      as.integer(table(df$prev_tercile_GBD))),
              collapse = ", "))

## ---- CDC-prevalence tercile (first available year 2010-2012) ---
## CDC ATLAS prevalence reporting expanded over time; use the earliest
## non-missing year per state from 2010-2012 as the baseline for tercile
## assignment.  States with no non-missing CDC prev in 2010-2012 get
## NA tercile (they will not appear in CDC-stratified analyses).
prev_baseline_CDC <- df %>%
  dplyr::filter(year_id %in% 2010:2012, !is.na(log_cdc_prevalence_per_100k)) %>%
  arrange(location_id, year_id) %>%
  group_by(location_id) %>%
  summarise(
    log_cdc_prev_baseline = first(log_cdc_prevalence_per_100k),
    cdc_prev_baseline_year = first(year_id),
    .groups = "drop"
  )

cdc_cuts <- quantile(prev_baseline_CDC$log_cdc_prev_baseline,
                     probs = c(1/3, 2/3), na.rm = TRUE)
log_msg("CDC tercile cuts (log CDC prev/100k, first avail 2010-2012): low<",
        sprintf("%.3f", cdc_cuts[1]), "  high>", sprintf("%.3f", cdc_cuts[2]))

prev_baseline_CDC <- prev_baseline_CDC %>%
  mutate(
    prev_tercile_CDC = case_when(
      log_cdc_prev_baseline <= cdc_cuts[1] ~ "Low",
      log_cdc_prev_baseline <= cdc_cuts[2] ~ "Mid",
      TRUE                                  ~ "High"
    ),
    prev_tercile_CDC = factor(prev_tercile_CDC, levels = c("Low", "Mid", "High"))
  )

df <- df %>%
  left_join(prev_baseline_CDC %>%
              select(location_id, prev_tercile_CDC,
                     log_cdc_prev_baseline, cdc_prev_baseline_year),
            by = "location_id")

n_cdc_assigned <- df %>% distinct(location_id, prev_tercile_CDC) %>%
  dplyr::filter(!is.na(prev_tercile_CDC)) %>% nrow()
log_msg("CDC tercile counts (state-years, NA dropped): ",
        paste(sprintf("%s=%d",
                      levels(df$prev_tercile_CDC),
                      as.integer(table(df$prev_tercile_CDC))),
              collapse = ", "),
        sprintf("  (states assigned: %d/51)", n_cdc_assigned))

##================================================================
## 3. DERIVED VARIABLES
##================================================================
safe_log <- function(x) ifelse(is.finite(x) & x > 0, log(x), NA_real_)

df <- df %>%
  ## upstream HIV risk-factor proxy
  mutate(log_opioid_prev = safe_log(opioid_prevalence_counts)) %>%
  ## K x L x V composite mediator (PRIMARY — full 90-90-90 cascade product)
  ## K = knowledge of HIV status; L = linkage to care within 1 month;
  ## V = viral suppression. Composite = K * L * V (joint probability of
  ## being diagnosed, linked, and suppressed) when all three are observed
  ## and in [0, 1]. Anchored in UNAIDS 90-90-90 framework (Levi et al. 2016).
  mutate(
    cdc_klv_composite = ifelse(
      !is.na(cdc_knowledge_status) & !is.na(cdc_linkage_1mo) & !is.na(cdc_viral_suppress) &
        cdc_knowledge_status >= 0 & cdc_knowledge_status <= 1 &
        cdc_linkage_1mo      >= 0 & cdc_linkage_1mo      <= 1 &
        cdc_viral_suppress   >= 0 & cdc_viral_suppress   <= 1,
      cdc_knowledge_status * cdc_linkage_1mo * cdc_viral_suppress,
      NA_real_
    )
  ) %>%
  ## K x V composite mediator (SENSITIVITY — two-component composite without L)
  mutate(
    cdc_kv_composite = ifelse(
      !is.na(cdc_knowledge_status) & !is.na(cdc_viral_suppress) &
        cdc_knowledge_status >= 0 & cdc_knowledge_status <= 1 &
        cdc_viral_suppress   >= 0 & cdc_viral_suppress   <= 1,
      cdc_knowledge_status * cdc_viral_suppress,
      NA_real_
    )
  ) %>%
  ## consistent-denominator CDC ratios
  mutate(
    log_cdc_mort_per_gbd_prev = safe_log(
      cdc_hiv_mortality_counts / hiv_prevalence_counts),
    log_spend_per_cdc_prev    = safe_log(
      (spend_all + ryan_white_funding_final) / cdc_hiv_prevalence_counts),
    log_cdc_mort_per_cdc_prev = safe_log(
      cdc_hiv_mortality_counts / cdc_hiv_prevalence_counts),
    log_gbd_mort_per_cdc_prev = safe_log(
      mortality_counts / cdc_hiv_prevalence_counts)
  ) %>%
  ## RW-exclusive per-case spending
  mutate(
    log_spend_excl_rw_per_case = ifelse(
      !is.na(spend_all) & !is.na(hiv_prevalence_counts) &
        spend_all > 0 & hiv_prevalence_counts > 0,
      log(spend_all / hiv_prevalence_counts),
      NA_real_
    )
  )

## Lead variables for temporal-sensitivity specs
df <- df %>%
  arrange(location_id, year_id) %>%
  group_by(location_id) %>%
  mutate(
    log_daly_per_capita_f1         = dplyr::lead(log_daly_per_capita, 1),
    log_mortality_per_capita_f1    = dplyr::lead(log_mortality_per_capita, 1),
    log_cdc_mortality_per_capita_f1 = dplyr::lead(log_cdc_mortality_per_capita, 1),
    as_daly_prev_ratio_log_f1      = dplyr::lead(as_daly_prev_ratio_log, 1),
    as_mort_prev_ratio_log_f1      = dplyr::lead(as_mort_prev_ratio_log, 1),
    log_cdc_mort_per_cdc_prev_f1   = dplyr::lead(log_cdc_mort_per_cdc_prev, 1),
    log_gbd_mort_per_cdc_prev_f1   = dplyr::lead(log_gbd_mort_per_cdc_prev, 1)
  ) %>%
  ungroup()

if (!"year_factor" %in% names(df)) df$year_factor <- factor(df$year_id)

##================================================================
## 4. HELPER FUNCTIONS
##================================================================

## null-coalescing operator (defined early because helpers below use it)
`%||%` <- function(a, b) if (is.null(a)) b else a

## significance label
sig_lab <- function(p) {
  case_when(
    is.na(p)  ~ "NA",
    p < 0.001 ~ "***",
    p < 0.01  ~ "**",
    p < 0.05  ~ "*",
    p < 0.10  ~ "†",
    TRUE      ~ "ns"
  )
}

## empty result row (for filling NA estimates consistently)
empty_row <- function() {
  tibble(
    estimate = NA_real_, std_error = NA_real_,
    statistic = NA_real_, p_value = NA_real_,
    sig_label = NA_character_, n_obs = NA_integer_
  )
}

## cluster-robust SE for lm (CR2 + Satterthwaite; HC0 fallback)
extract_lm_CR2 <- function(fit, cluster_var) {
  tryCatch({
    vc <- vcovCR(fit, cluster = cluster_var, type = "CR2")
    ct <- coef_test(fit, vcov = vc, test = "Satterthwaite")
    tibble(
      term      = rownames(ct),
      estimate  = ct$beta,
      std_error = ct$SE,
      statistic = ct$tstat,
      df        = ct$df,
      p_value   = ct$p_Satt
    )
  }, error = function(e) {
    message("CR2 failed: ", e$message, " -- falling back to HC0")
    vc <- vcovCL(fit, cluster = cluster_var, type = "HC0")
    ct <- coeftest(fit, vcov. = vc)
    tibble(
      term      = rownames(ct),
      estimate  = ct[, "Estimate"],
      std_error = ct[, "Std. Error"],
      statistic = ct[, "t value"],
      df        = NA_real_,
      p_value   = ct[, "Pr(>|t|)"]
    )
  })
}

## cluster-robust SE for GLM (HC0)
extract_glm_CL <- function(fit, cluster_var) {
  vc <- vcovCL(fit, cluster = cluster_var, type = "HC0")
  ct <- coeftest(fit, vcov. = vc)
  tibble(
    term      = rownames(ct),
    estimate  = ct[, "Estimate"],
    std_error = ct[, "Std. Error"],
    statistic = ct[, "z value"],
    p_value   = ct[, "Pr(>|z|)"]
  )
}

## get one coefficient (b, se, p) by term name
get_coef <- function(fit, term, vc) {
  if (!term %in% names(coef(fit))) return(list(b = NA_real_, se = NA_real_, p = NA_real_))
  b  <- coef(fit)[term]
  se <- sqrt(vc[term, term])
  z  <- b / se
  p  <- 2 * pnorm(-abs(z))
  list(b = as.numeric(b), se = as.numeric(se), p = as.numeric(p))
}

## subgroup beta from interaction model via lin combination + delta method
## Tercile column name is configurable (so we can use GBD or CDC strata).
subgroup_betas <- function(fit, xvar, cluster_var, tercile_var) {
  vc <- tryCatch(
    if (inherits(fit, "glm"))
      sandwich::vcovCL(fit, cluster = cluster_var, type = "HC0")
    else
      clubSandwich::vcovCR(fit, cluster = cluster_var, type = "CR2"),
    error = function(e) {
      message("subgroup_betas vcov failed: ", e$message,
              " -- falling back to model vcov")
      vcov(fit)
    }
  )
  cf <- coef(fit)
  if (!(xvar %in% names(cf))) return(NULL)
  
  int_mid_term  <- sprintf("%s:%sMid",  xvar, tercile_var)
  int_high_term <- sprintf("%s:%sHigh", xvar, tercile_var)
  
  b_low  <- cf[xvar]
  se_low <- sqrt(vc[xvar, xvar])
  z_low  <- b_low / se_low
  p_low  <- 2 * pnorm(-abs(z_low))
  out <- tibble(
    tercile  = "Low",
    estimate = as.numeric(b_low),
    std_error = as.numeric(se_low),
    statistic = as.numeric(z_low),
    p_value   = as.numeric(p_low)
  )
  
  if (int_mid_term %in% names(cf)) {
    b_mid <- cf[xvar] + cf[int_mid_term]
    v_mid <- vc[xvar, xvar] + vc[int_mid_term, int_mid_term] +
      2 * vc[xvar, int_mid_term]
    se_mid <- sqrt(v_mid)
    z_mid  <- b_mid / se_mid
    p_mid  <- 2 * pnorm(-abs(z_mid))
    out <- bind_rows(out, tibble(
      tercile = "Mid",
      estimate = as.numeric(b_mid),
      std_error = as.numeric(se_mid),
      statistic = as.numeric(z_mid),
      p_value   = as.numeric(p_mid)
    ))
  }
  if (int_high_term %in% names(cf)) {
    b_high <- cf[xvar] + cf[int_high_term]
    v_high <- vc[xvar, xvar] + vc[int_high_term, int_high_term] +
      2 * vc[xvar, int_high_term]
    se_high <- sqrt(v_high)
    z_high  <- b_high / se_high
    p_high  <- 2 * pnorm(-abs(z_high))
    out <- bind_rows(out, tibble(
      tercile = "High",
      estimate = as.numeric(b_high),
      std_error = as.numeric(se_high),
      statistic = as.numeric(z_high),
      p_value   = as.numeric(p_high)
    ))
  }
  out
}

## manual AME for logit GLM (delta method)
compute_manual_ame <- function(fit, xvar, cluster_var) {
  tryCatch({
    preds  <- predict(fit, type = "response")
    weight <- mean(preds * (1 - preds), na.rm = TRUE)
    beta   <- coef(fit)[xvar]
    vc     <- sandwich::vcovCL(fit, cluster = cluster_var, type = "HC0")
    se_b   <- sqrt(vc[xvar, xvar])
    ame    <- beta * weight
    se_ame <- se_b * weight
    z      <- ame / se_ame
    p      <- 2 * pnorm(-abs(z))
    list(estimate = as.numeric(ame),
         std_error = as.numeric(se_ame),
         p_value  = as.numeric(p))
  }, error = function(e) {
    list(estimate = NA_real_, std_error = NA_real_, p_value = NA_real_)
  })
}

compute_ame <- function(fit, xvar, cluster_var) {
  if (requireNamespace("marginaleffects", quietly = TRUE)) {
    res <- tryCatch({
      vc <- sandwich::vcovCL(fit, cluster = cluster_var, type = "HC0")
      s  <- marginaleffects::avg_slopes(fit, variables = xvar, vcov = vc)
      list(estimate = as.numeric(s$estimate[1]),
           std_error = as.numeric(s$std.error[1]),
           p_value  = as.numeric(s$p.value[1]))
    }, error = function(e) NULL)
    if (!is.null(res)) return(res)
  }
  compute_manual_ame(fit, xvar, cluster_var)
}

## mediation fit: a, b, c', c, indirect with cluster-robust SEs
fit_mediation <- function(d, X, M, Y, covars) {
  if (nrow(d) < 30) return(NULL)
  tryCatch({
    fa <- lm(as.formula(paste(M, "~", X, "+", covars)), data = d)
    fb <- lm(as.formula(paste(Y, "~", M, "+", X, "+", covars)), data = d)
    fc <- lm(as.formula(paste(Y, "~", X, "+", covars)),         data = d)
    
    vc_a <- tryCatch(vcovCR(fa, cluster = d$location_id, type = "CR2"),
                     error = function(e) vcovCL(fa, cluster = d$location_id, type = "HC0"))
    vc_b <- tryCatch(vcovCR(fb, cluster = d$location_id, type = "CR2"),
                     error = function(e) vcovCL(fb, cluster = d$location_id, type = "HC0"))
    vc_c <- tryCatch(vcovCR(fc, cluster = d$location_id, type = "CR2"),
                     error = function(e) vcovCL(fc, cluster = d$location_id, type = "HC0"))
    
    a_info  <- get_coef(fa, X, vc_a)
    b_info  <- get_coef(fb, M, vc_b)
    cp_info <- get_coef(fb, X, vc_b)
    ct_info <- get_coef(fc, X, vc_c)
    
    list(
      a = a_info$b,         a_se = a_info$se,         a_p = a_info$p,
      b = b_info$b,         b_se = b_info$se,         b_p = b_info$p,
      c_prime = cp_info$b,  c_prime_se = cp_info$se,  c_prime_p = cp_info$p,
      c_total = ct_info$b,  c_total_se = ct_info$se,  c_total_p = ct_info$p,
      indirect = a_info$b * b_info$b,
      n_obs = nrow(d)
    )
  }, error = function(e) {
    message("fit_mediation failed: ", e$message)
    NULL
  })
}

## state-block bootstrap of indirect effect
bootstrap_indirect <- function(d, X, M, Y, covars, n_boot = 500, seed = 20260520) {
  set.seed(seed)
  state_ids <- unique(d$location_id)
  ind_vals <- numeric(n_boot)
  n_failed <- 0
  for (i in seq_len(n_boot)) {
    samp <- sample(state_ids, length(state_ids), replace = TRUE)
    d_boot <- bind_rows(lapply(samp, function(s) d[d$location_id == s, , drop = FALSE]))
    d_boot$location_id <- as.integer(factor(
      paste0(d_boot$location_id, "_",
             ave(seq_along(samp), samp, FUN = seq_along)[match(d_boot$location_id, samp)])
    ))
    res <- tryCatch(fit_mediation(d_boot, X, M, Y, covars),
                    error = function(e) { n_failed <<- n_failed + 1; NULL })
    if (is.null(res)) { ind_vals[i] <- NA_real_; next }
    ind_vals[i] <- res$indirect
  }
  ind_vals <- ind_vals[!is.na(ind_vals)]
  if (length(ind_vals) < 20) {
    return(list(lower = NA_real_, upper = NA_real_,
                n_boot_successful = length(ind_vals), n_boot_failed = n_failed))
  }
  list(
    lower = quantile(ind_vals, 0.025, na.rm = TRUE),
    upper = quantile(ind_vals, 0.975, na.rm = TRUE),
    n_boot_successful = length(ind_vals),
    n_boot_failed = n_failed
  )
}

## shared RHS blocks
rhs_demog    <- function() "race_prop_BLCK + race_prop_HISP + log_prop_homeless + year_factor"
rhs_primary  <- function() paste(rhs_demog(), "+ log_incidence_per_100k")
rhs_lagged   <- function() paste(rhs_demog(), "+ log_incidence_per_100k_l1")
rhs_cascade  <- function() paste(rhs_primary(), "+ log_prevalence_per_100k_l1")
rhs_upstream <- function() paste(rhs_demog(),
                                 "+ log_opioid_prev + aca_implemented_status",
                                 "+ edu_yrs + log_ldi_pc")

## Convert a list of fit_mediation results into a long-format tibble
## with one row per path (a, b, c', c, indirect) per stratum.
mediation_to_long <- function(paths_dict, lens, outcome, tercile_source,
                              predictor, mediator) {
  rows <- list()
  for (tercile in names(paths_dict)) {
    res <- paths_dict[[tercile]]
    if (is.null(res)) next
    base <- tibble(
      lens = lens, outcome = outcome,
      predictor = predictor, mediator = mediator,
      tercile_source = tercile_source, tercile = tercile,
      n_obs = res$n_obs
    )
    rows[[paste(tercile, "a", sep = "_")]] <- base %>%
      mutate(path = "a (X->M)",
             estimate = res$a, std_error = res$a_se, p_value = res$a_p,
             sig_label = sig_lab(res$a_p),
             indirect_CI_low = NA_real_, indirect_CI_high = NA_real_)
    rows[[paste(tercile, "b", sep = "_")]] <- base %>%
      mutate(path = "b (M->Y|X)",
             estimate = res$b, std_error = res$b_se, p_value = res$b_p,
             sig_label = sig_lab(res$b_p),
             indirect_CI_low = NA_real_, indirect_CI_high = NA_real_)
    rows[[paste(tercile, "cp", sep = "_")]] <- base %>%
      mutate(path = "c' (X->Y|M, direct)",
             estimate = res$c_prime, std_error = res$c_prime_se,
             p_value = res$c_prime_p, sig_label = sig_lab(res$c_prime_p),
             indirect_CI_low = NA_real_, indirect_CI_high = NA_real_)
    rows[[paste(tercile, "ct", sep = "_")]] <- base %>%
      mutate(path = "c (X->Y, total, matched sample)",
             estimate = res$c_total, std_error = res$c_total_se,
             p_value = res$c_total_p, sig_label = sig_lab(res$c_total_p),
             indirect_CI_low = NA_real_, indirect_CI_high = NA_real_)
    rows[[paste(tercile, "ind", sep = "_")]] <- base %>%
      mutate(path = "indirect (a*b)",
             estimate = res$indirect, std_error = NA_real_,
             p_value = NA_real_, sig_label = NA_character_,
             indirect_CI_low  = res$indirect_lower %||% NA_real_,
             indirect_CI_high = res$indirect_upper %||% NA_real_)
  }
  bind_rows(rows) %>%
    select(lens, outcome, predictor, mediator, tercile_source, tercile,
           path, estimate, std_error, p_value, sig_label,
           indirect_CI_low, indirect_CI_high, n_obs)
}

## Run the full mediation chain (Low/Mid/High/Pooled) for a given
## (X, M, Y, covars, tercile_var) and return a paths_dict.
run_stratified_mediation <- function(df_in, X, M, Y, covars,
                                     tercile_var, n_boot = 500,
                                     seed = 20260520, verbose_label = NULL) {
  out <- list()
  strata <- c("Low", "Mid", "High", "Pooled")
  for (tercile in strata) {
    d_base <- if (tercile == "Pooled") df_in
    else df_in %>% dplyr::filter(.data[[tercile_var]] == tercile)
    rhs_terms <- trimws(strsplit(covars, "\\+")[[1]])
    rhs_terms <- setdiff(rhs_terms, c("", "1"))
    rhs_terms <- rhs_terms[!grepl("year_factor", rhs_terms)]
    rhs_terms <- gsub(" ", "", rhs_terms)
    cols <- unique(c(Y, X, M, rhs_terms, "year_factor", "location_id"))
    d <- d_base[, intersect(cols, names(d_base))] %>% drop_na()
    res <- fit_mediation(d, X, M, Y, covars)
    if (!is.null(res) && n_boot > 0) {
      boot <- bootstrap_indirect(d, X, M, Y, covars,
                                 n_boot = n_boot, seed = seed)
      res$indirect_lower <- boot$lower
      res$indirect_upper <- boot$upper
      res$n_boot_successful <- boot$n_boot_successful
      res$n_boot_failed <- boot$n_boot_failed
    }
    out[[tercile]] <- res
    if (!is.null(res) && !is.null(verbose_label)) {
      log_msg(sprintf("  %s %-6s  n=%d  a=%+.4f (p=%.3f)  b=%+.5f (p=%.3f)  c=%+.5f (p=%.3f)  ab=%+.6f",
                      verbose_label, tercile, res$n_obs,
                      res$a, res$a_p, res$b, res$b_p,
                      res$c_total, res$c_total_p, res$indirect))
    }
  }
  out
}

##================================================================
## 5. PRIMARY MEDIATION  (per-case GBD, DALYs, GBD-tercile strata)
##================================================================
log_msg("\n=== PRIMARY MEDIATION  (per-case GBD, DALYs, GBD strata) ===")

X_GBD <- "rw_dex_hiv_prev_ratio_log"
M_main <- "cdc_klv_composite"   # PRIMARY mediator: K x L x V (full 90-90-90 cascade product)
covars_primary <- "log_incidence_per_100k + log_prop_homeless + race_prop_BLCK + race_prop_HISP + year_factor"

PRIMARY_DALYs <- run_stratified_mediation(
  df, X = X_GBD, M = M_main, Y = "as_daly_prev_ratio_log",
  covars = covars_primary, tercile_var = "prev_tercile_GBD",
  verbose_label = "DALYs"
)

primary_DALYs_long <- mediation_to_long(
  PRIMARY_DALYs, lens = "per_case_GBD",
  outcome = "log(DALYs per GBD prev case)",
  tercile_source = "GBD-prevalence (2010)",
  predictor = X_GBD, mediator = M_main
)

##================================================================
## 6. SECONDARY MEDIATION  (per-case GBD, Mortality, GBD strata)
##================================================================
log_msg("\n=== SECONDARY MEDIATION  (per-case GBD, Mortality, GBD strata) ===")

PRIMARY_Mortality <- run_stratified_mediation(
  df, X = X_GBD, M = M_main, Y = "as_mort_prev_ratio_log",
  covars = covars_primary, tercile_var = "prev_tercile_GBD",
  verbose_label = "Mort "
)

primary_Mortality_long <- mediation_to_long(
  PRIMARY_Mortality, lens = "per_case_GBD",
  outcome = "log(GBD mortality per GBD prev case)",
  tercile_source = "GBD-prevalence (2010)",
  predictor = X_GBD, mediator = M_main
)

##================================================================
## 7. CASCADE X -> M  (per-case GBD, GBD strata)
##================================================================
log_msg("\n=== CASCADE X->M:  per_case_GBD (GBD strata) ===")

cascade_mediators <- list(
  K     = "cdc_knowledge_status",
  L     = "cdc_linkage_1mo",
  V     = "cdc_viral_suppress",
  KxLxV = "cdc_klv_composite",   # PRIMARY composite — full 90-90-90 product
  KxV   = "cdc_kv_composite"      # SENSITIVITY composite — two-component
)

run_cascade_X_to_M <- function(df_in, xvar, predictor_label,
                               tercile_var, tercile_source) {
  rows <- list()
  for (m_label in names(cascade_mediators)) {
    yvar <- cascade_mediators[[m_label]]
    cols_needed <- unique(c(yvar, xvar,
                            "log_incidence_per_100k",
                            "log_prevalence_per_100k_l1",
                            "race_prop_BLCK", "race_prop_HISP",
                            "log_prop_homeless", "year_factor",
                            tercile_var, "location_id"))
    d <- df_in[, intersect(cols_needed, names(df_in))] %>% drop_na()
    if (nrow(d) < 100) {
      log_msg(sprintf("  WARNING %s ~ %s n=%d, skipping",
                      m_label, predictor_label, nrow(d)))
      next
    }
    d$.outcome <- pmin(pmax(d[[yvar]], 1e-6), 1 - 1e-6)
    
    ## Pooled regression (log-odds + AME)
    f_pool <- as.formula(paste0(".outcome ~ ", xvar, " + ", rhs_cascade()))
    fit_pool <- glm(f_pool, data = d, family = quasibinomial(link = "logit"))
    pool_coef <- extract_glm_CL(fit_pool, d$location_id) %>%
      dplyr::filter(term == xvar)
    rows[[paste("pool_logodds", m_label, sep = "_")]] <- tibble(
      lens = predictor_label, mediator = m_label, predictor = xvar,
      tercile_source = tercile_source, tercile = "Pooled",
      scale = "log_odds",
      estimate = pool_coef$estimate, std_error = pool_coef$std_error,
      p_value = pool_coef$p_value, sig_label = sig_lab(pool_coef$p_value),
      n_obs = nrow(d)
    )
    
    ame <- compute_ame(fit_pool, xvar, d$location_id)
    rows[[paste("pool_ame", m_label, sep = "_")]] <- tibble(
      lens = predictor_label, mediator = m_label, predictor = xvar,
      tercile_source = tercile_source, tercile = "Pooled",
      scale = "AME_percentage_points",
      estimate = ame$estimate, std_error = ame$std_error,
      p_value = ame$p_value, sig_label = sig_lab(ame$p_value),
      n_obs = nrow(d)
    )
    
    ## Tercile interaction (subgroup betas via lincomb)
    f_int <- as.formula(paste0(".outcome ~ ", xvar, " * ", tercile_var, " + ", rhs_cascade()))
    fit_int <- glm(f_int, data = d, family = quasibinomial(link = "logit"))
    sg <- subgroup_betas(fit_int, xvar, d$location_id, tercile_var)
    if (!is.null(sg)) {
      for (i in seq_len(nrow(sg))) {
        rows[[paste("sub_logodds", m_label, sg$tercile[i], sep = "_")]] <- tibble(
          lens = predictor_label, mediator = m_label, predictor = xvar,
          tercile_source = tercile_source, tercile = as.character(sg$tercile[i]),
          scale = "log_odds",
          estimate = sg$estimate[i], std_error = sg$std_error[i],
          p_value = sg$p_value[i], sig_label = sig_lab(sg$p_value[i]),
          n_obs = nrow(d)
        )
      }
    }
    log_msg(sprintf("  %-3s ~ %-13s : β=%+.3f (p=%.3f); AME=%+.4f  n=%d",
                    m_label, predictor_label,
                    pool_coef$estimate, pool_coef$p_value,
                    ame$estimate, nrow(d)))
  }
  bind_rows(rows) %>%
    select(lens, mediator, predictor, tercile_source, tercile, scale,
           estimate, std_error, p_value, sig_label, n_obs)
}

cascade_GBD <- run_cascade_X_to_M(
  df, xvar = X_GBD, predictor_label = "per_case_GBD",
  tercile_var = "prev_tercile_GBD",
  tercile_source = "GBD-prevalence (2010)"
)

##================================================================
## 8. CASCADE X -> M  (per-case CDC, CDC strata)
##================================================================
log_msg("\n=== CASCADE X->M:  per_case_CDC (CDC strata) ===")

X_CDC <- "log_spend_per_cdc_prev"

cascade_CDC <- run_cascade_X_to_M(
  df %>% dplyr::filter(!is.na(prev_tercile_CDC)),
  xvar = X_CDC, predictor_label = "per_case_CDC",
  tercile_var = "prev_tercile_CDC",
  tercile_source = "CDC-prevalence (first avail 2010-2012)"
)

##================================================================
## 9. CASCADE X -> M  (per-capita, POOLED ONLY)
##================================================================
log_msg("\n=== CASCADE X->M:  per_capita (pooled only) ===")

X_PC <- "log_spending_per_capita"

run_cascade_pooled_only <- function(df_in, xvar, predictor_label) {
  rows <- list()
  for (m_label in names(cascade_mediators)) {
    yvar <- cascade_mediators[[m_label]]
    cols_needed <- unique(c(yvar, xvar, "log_incidence_per_100k",
                            "log_prevalence_per_100k_l1",
                            "race_prop_BLCK", "race_prop_HISP",
                            "log_prop_homeless", "year_factor",
                            "location_id"))
    d <- df_in[, intersect(cols_needed, names(df_in))] %>% drop_na()
    if (nrow(d) < 100) next
    d$.outcome <- pmin(pmax(d[[yvar]], 1e-6), 1 - 1e-6)
    f_pool <- as.formula(paste0(".outcome ~ ", xvar, " + ", rhs_cascade()))
    fit_pool <- glm(f_pool, data = d, family = quasibinomial(link = "logit"))
    pool_coef <- extract_glm_CL(fit_pool, d$location_id) %>%
      dplyr::filter(term == xvar)
    ame <- compute_ame(fit_pool, xvar, d$location_id)
    rows[[paste("logodds", m_label, sep = "_")]] <- tibble(
      lens = predictor_label, mediator = m_label, predictor = xvar,
      tercile_source = "none (pooled)", tercile = "Pooled",
      scale = "log_odds",
      estimate = pool_coef$estimate, std_error = pool_coef$std_error,
      p_value = pool_coef$p_value, sig_label = sig_lab(pool_coef$p_value),
      n_obs = nrow(d)
    )
    rows[[paste("ame", m_label, sep = "_")]] <- tibble(
      lens = predictor_label, mediator = m_label, predictor = xvar,
      tercile_source = "none (pooled)", tercile = "Pooled",
      scale = "AME_percentage_points",
      estimate = ame$estimate, std_error = ame$std_error,
      p_value = ame$p_value, sig_label = sig_lab(ame$p_value),
      n_obs = nrow(d)
    )
    log_msg(sprintf("  %-3s ~ %-13s : β=%+.3f (p=%.3f); AME=%+.4f  n=%d",
                    m_label, predictor_label,
                    pool_coef$estimate, pool_coef$p_value,
                    ame$estimate, nrow(d)))
  }
  bind_rows(rows) %>%
    select(lens, mediator, predictor, tercile_source, tercile, scale,
           estimate, std_error, p_value, sig_label, n_obs)
}

cascade_PC <- run_cascade_pooled_only(df, xvar = X_PC,
                                      predictor_label = "per_capita")

##================================================================
## 10. CASCADE M -> Y  (pooled only; DALYs primary, Mortality parallel)
##================================================================
log_msg("\n=== CASCADE M->Y (pooled only, both outcomes) ===")

outcomes_M_to_Y <- list(
  DALYs     = "as_daly_prev_ratio_log",
  Mortality = "as_mort_prev_ratio_log"
)

run_M_to_Y <- function(df_in) {
  rows <- list()
  for (cp_label in names(cascade_mediators)) {
    cp <- cascade_mediators[[cp_label]]
    for (y_label in names(outcomes_M_to_Y)) {
      Y <- outcomes_M_to_Y[[y_label]]
      for (spec in c("without_spending", "with_spending")) {
        cols <- c(Y, cp, "log_incidence_per_100k", "race_prop_BLCK",
                  "race_prop_HISP", "log_prop_homeless",
                  "year_factor", "location_id")
        if (spec == "with_spending") cols <- c(cols, X_GBD)
        d <- df_in[, cols] %>% drop_na()
        if (nrow(d) < 50) next
        rhs <- if (spec == "with_spending") {
          paste0(cp, " + ", X_GBD, " + ", rhs_primary())
        } else {
          paste0(cp, " + ", rhs_primary())
        }
        f <- as.formula(paste0(Y, " ~ ", rhs))
        fit <- lm(f, data = d)
        ce <- extract_lm_CR2(fit, d$location_id) %>%
          dplyr::filter(term == cp)
        rows[[paste(cp_label, y_label, spec, sep = "_")]] <- tibble(
          mediator_predictor = cp_label,
          outcome = y_label,
          spec = spec,
          estimate = ce$estimate, std_error = ce$std_error,
          p_value = ce$p_value, sig_label = sig_lab(ce$p_value),
          adj_r2 = summary(fit)$adj.r.squared,
          n_obs = nrow(d)
        )
        log_msg(sprintf("  %-25s -> %-10s (%s)  β=%+.4f (p=%.3f) n=%d",
                        cp_label, y_label, spec,
                        ce$estimate, ce$p_value, nrow(d)))
      }
    }
  }
  bind_rows(rows)
}

M_to_Y_table <- run_M_to_Y(df)

##================================================================
## 11. BURDEN LENS  (DALYs primary, Mortality parallel)
##================================================================
##   Predictor: X_GBD; pooled (no tercile strata at this tier — the
##   point of burden lens is the reduced-form effect of spending on
##   outcome).  Six robustness specs.
##================================================================
log_msg("\n=== BURDEN LENS (pooled; DALYs primary) ===")

burden_specs <- list(
  "Bivariate (no controls)"               = "1",
  "Primary: contemporaneous incidence"    = rhs_primary(),
  "Sensitivity: lagged incidence (t-1)"   = rhs_lagged(),
  "Sensitivity: drop incidence entirely"  = rhs_demog(),
  "Sensitivity: upstream risk-factor set" = rhs_upstream(),
  "Mundlak between/within decomposition"  = "MUNDLAK"
)
burden_outcomes <- list(
  DALYs     = "as_daly_prev_ratio_log",
  Mortality = "as_mort_prev_ratio_log"
)

run_burden_lens <- function(df_in) {
  rows <- list()
  for (spec_label in names(burden_specs)) {
    controls <- burden_specs[[spec_label]]
    for (y_label in names(burden_outcomes)) {
      Y <- burden_outcomes[[y_label]]
      if (controls == "MUNDLAK") {
        cols <- c(Y, "rw_dex_hiv_prev_ratio_log_B",
                  "rw_dex_hiv_prev_ratio_log_W",
                  "log_incidence_per_100k", "race_prop_BLCK",
                  "race_prop_HISP", "log_prop_homeless",
                  "year_factor", "location_id")
        d <- df_in[, cols] %>% drop_na()
        f <- as.formula(paste0(Y, " ~ rw_dex_hiv_prev_ratio_log_B + rw_dex_hiv_prev_ratio_log_W + ",
                               rhs_primary()))
        fit <- lm(f, data = d)
        ce <- extract_lm_CR2(fit, d$location_id)
        bB <- ce %>% dplyr::filter(term == "rw_dex_hiv_prev_ratio_log_B")
        bW <- ce %>% dplyr::filter(term == "rw_dex_hiv_prev_ratio_log_W")
        rows[[paste(spec_label, y_label, "B", sep = "|")]] <- tibble(
          spec_label = spec_label, outcome = y_label,
          coefficient = "between (state mean)",
          estimate = bB$estimate, std_error = bB$std_error,
          p_value = bB$p_value, sig_label = sig_lab(bB$p_value),
          adj_r2 = summary(fit)$adj.r.squared, n_obs = nobs(fit)
        )
        rows[[paste(spec_label, y_label, "W", sep = "|")]] <- tibble(
          spec_label = spec_label, outcome = y_label,
          coefficient = "within (state-mean-deviated)",
          estimate = bW$estimate, std_error = bW$std_error,
          p_value = bW$p_value, sig_label = sig_lab(bW$p_value),
          adj_r2 = summary(fit)$adj.r.squared, n_obs = nobs(fit)
        )
      } else {
        cols_needed <- c(Y, X_GBD, "location_id")
        if (controls != "1") {
          extras <- trimws(strsplit(controls, "\\+")[[1]])
          extras <- setdiff(extras, c("", "1"))
          for (e in extras) {
            if (e == "year_factor") next
            if (!e %in% cols_needed) cols_needed <- c(cols_needed, e)
          }
          if (!"year_factor" %in% cols_needed) cols_needed <- c(cols_needed, "year_factor")
        }
        d <- df_in[, intersect(cols_needed, names(df_in))] %>% drop_na()
        f <- if (controls == "1") as.formula(paste0(Y, " ~ ", X_GBD))
        else as.formula(paste0(Y, " ~ ", X_GBD, " + ", controls))
        fit <- lm(f, data = d)
        ce <- extract_lm_CR2(fit, d$location_id) %>%
          dplyr::filter(term == X_GBD)
        rows[[paste(spec_label, y_label, sep = "|")]] <- tibble(
          spec_label = spec_label, outcome = y_label,
          coefficient = "log(spending/GBD prev case), year t",
          estimate = ce$estimate, std_error = ce$std_error,
          p_value = ce$p_value, sig_label = sig_lab(ce$p_value),
          adj_r2 = summary(fit)$adj.r.squared, n_obs = nobs(fit)
        )
      }
    }
  }
  bind_rows(rows)
}

burden_table_all <- run_burden_lens(df)
burden_DALYs <- burden_table_all %>% dplyr::filter(outcome == "DALYs")
burden_Mortality <- burden_table_all %>% dplyr::filter(outcome == "Mortality")
log_msg(sprintf("  Burden lens DALYs rows: %d", nrow(burden_DALYs)))
log_msg(sprintf("  Burden lens Mortality rows: %d", nrow(burden_Mortality)))


#########
##================================================================
## 11b. BURDEN LENS STRATIFIED  (NEW — added 2026-05-21)
##================================================================
##   Same regression form as §11 burden lens (log Y ~ log SpendPrev +
##   primary covariates + year FE), but fitted by SUBSET on each
##   2010-GBD-prevalence tercile on the FULL 510 panel (NOT cascade-
##   matched).  This is the within-stratum reduced form — the primary
##   inferential quantity the memo Table 1 leads with.
##
##   Distinct from the c-total path of run_stratified_mediation (§5),
##   which fits the same regression form on the cascade-matched 311
##   sample (n = 97 / 97 / 117).  Both are reported side by side in the
##   manuscript / equations reference.
##
##   Drop this block in immediately after the existing §11 block (after
##   the `burden_Mortality` / `log_msg` lines, before §12 sensitivities).
##================================================================
log_msg("\n=== BURDEN LENS STRATIFIED (DALYs + Mortality, by 2010 GBD tercile) ===")

run_burden_lens_stratified <- function(df_in) {
  rows <- list()
  strata <- c("Low", "Mid", "High", "Pooled")
  for (s in strata) {
    d_base <- if (s == "Pooled") df_in
    else df_in %>% dplyr::filter(prev_tercile_GBD == s)
    for (y_label in names(burden_outcomes)) {
      Y <- burden_outcomes[[y_label]]
      cols_needed <- c(Y, X_GBD,
                       "log_incidence_per_100k",
                       "log_prop_homeless",
                       "race_prop_BLCK",
                       "race_prop_HISP",
                       "year_factor",
                       "location_id")
      d <- d_base[, intersect(cols_needed, names(d_base))] %>% drop_na()
      if (nrow(d) < 30) {
        message(sprintf("  Stratum %s / %s: skipped (n=%d < 30).", s, y_label, nrow(d)))
        next
      }
      f <- as.formula(paste0(Y, " ~ ", X_GBD, " + ", rhs_primary()))
      fit <- lm(f, data = d)
      ce <- extract_lm_CR2(fit, d$location_id) %>%
        dplyr::filter(term == X_GBD)
      rows[[paste(s, y_label, sep = "|")]] <- tibble(
        lens           = "burden_stratified",
        outcome        = y_label,
        tercile_source = "GBD-prevalence (2010)",
        tercile        = s,
        coefficient    = "log(spending/GBD prev case), year t",
        estimate       = ce$estimate,
        std_error      = ce$std_error,
        df             = ce$df,
        p_value        = ce$p_value,
        sig_label      = sig_lab(ce$p_value),
        adj_r2         = summary(fit)$adj.r.squared,
        n_obs          = nobs(fit),
        n_states       = length(unique(d$location_id))
      )
      log_msg(sprintf("  %-4s  %-9s  beta=%+.4f  SE=%.4f  p=%.4f  n=%d  states=%d",
                      s, y_label, ce$estimate, ce$std_error, ce$p_value,
                      nobs(fit), length(unique(d$location_id))))
    }
  }
  bind_rows(rows)
}

burden_stratified_all       <- run_burden_lens_stratified(df)
burden_stratified_DALYs     <- burden_stratified_all %>% dplyr::filter(outcome == "DALYs")
burden_stratified_Mortality <- burden_stratified_all %>% dplyr::filter(outcome == "Mortality")

log_msg(sprintf("  Burden lens stratified DALYs rows:     %d", nrow(burden_stratified_DALYs)))
log_msg(sprintf("  Burden lens stratified Mortality rows: %d", nrow(burden_stratified_Mortality)))

## ---- CSV writers (add these lines to your existing writer block in §13) ---
out_dir <- if (exists("dir_output")) dir_output else getwd()
readr::write_csv(burden_stratified_all,
                 file.path(out_dir, "burden_lens_stratified_per_case_GBD.csv"))
log_msg("    burden_lens_stratified_per_case_GBD.csv             — 2 outcomes x 4 strata")


##================================================================
## 11c. BURDEN LENS — INTERACTION MODEL  (NEW — permanent block)
##================================================================
##   Same regression form as §11 / §11b, but uses ONE lm() on the full
##   510 panel with a spending × tercile interaction.  Subgroup slopes
##   (Low / Mid / High) are computed manually from coef() and the
##   cluster-robust covariance via linear combinations.  Pure base R
##   plus clubSandwich for the cluster-robust SE — no custom helpers.
##
##   Drop in immediately after the §11b block, just before §12.
##================================================================
log_msg("\n=== BURDEN LENS INTERACTION (DALYs primary, full panel) ===")

# ---- (1) Fit the interaction model on the full 510 panel -----------------
fit_burden_interaction <- lm(
  as_daly_prev_ratio_log ~                            # log(DALYs per GBD prev case)
    rw_dex_hiv_prev_ratio_log * prev_tercile_GBD +    # log(SpendPrev) × tercile
    log_incidence_per_100k +                          # log(Incidence)
    log_prop_homeless +                               # log(PropHomeless)
    race_prop_BLCK +                                  # PropBlack (linear)
    race_prop_HISP +                                  # PropHispanic (linear)
    year_factor,                                      # year fixed effects
  data = df
)

# ---- (2) Print the raw regression table ----------------------------------
cat("\n=========================================================\n")
cat("  Burden lens — interaction model (DALYs)\n")
cat("=========================================================\n")
cat("n observations :", nobs(fit_burden_interaction), "\n")
cat("Adjusted R^2   :", round(summary(fit_burden_interaction)$adj.r.squared, 4), "\n")
cat("\nFull coefficient table (default lm SEs — replaced below by CR2):\n")
print(round(summary(fit_burden_interaction)$coefficients, 4))

# ---- (3) Subgroup slopes via linear combinations -------------------------
# Cluster-robust covariance (state = location_id; CR2 small-sample correction)
vc <- clubSandwich::vcovCR(fit_burden_interaction,
                           cluster = df$location_id, type = "CR2")
b  <- coef(fit_burden_interaction)

# Coefficient names — Low is the reference level for prev_tercile_GBD
main_x   <- "rw_dex_hiv_prev_ratio_log"
int_mid  <- "rw_dex_hiv_prev_ratio_log:prev_tercile_GBDMid"
int_high <- "rw_dex_hiv_prev_ratio_log:prev_tercile_GBDHigh"

# Subgroup slope = main effect + (if Mid or High) the interaction term
slope_low  <- b[main_x]
slope_mid  <- b[main_x] + b[int_mid]
slope_high <- b[main_x] + b[int_high]

# Standard error of a linear combination A + B:
#   SE = sqrt( Var(A) + Var(B) + 2 * Cov(A, B) )
se_low  <- sqrt( vc[main_x, main_x] )
se_mid  <- sqrt( vc[main_x, main_x] +
                   vc[int_mid, int_mid] +
                   2 * vc[main_x, int_mid] )
se_high <- sqrt( vc[main_x, main_x] +
                   vc[int_high, int_high] +
                   2 * vc[main_x, int_high] )

# t-statistic, residual df, two-sided p-value
deg_f  <- fit_burden_interaction$df.residual
t_low  <- slope_low  / se_low
t_mid  <- slope_mid  / se_mid
t_high <- slope_high / se_high
p_low  <- 2 * pt(abs(t_low),  df = deg_f, lower.tail = FALSE)
p_mid  <- 2 * pt(abs(t_mid),  df = deg_f, lower.tail = FALSE)
p_high <- 2 * pt(abs(t_high), df = deg_f, lower.tail = FALSE)

# ---- (4) Build a tidy subgroup table and print --------------------------
burden_interaction_subgroups <- data.frame(
  lens     = "burden_interaction",
  outcome  = "DALYs",
  tercile  = c("Low", "Mid", "High"),
  estimate = c(slope_low,  slope_mid,  slope_high),
  std_error= c(se_low,     se_mid,     se_high),
  t_stat   = c(t_low,      t_mid,      t_high),
  p_value  = c(p_low,      p_mid,      p_high),
  n_obs    = nobs(fit_burden_interaction),
  stringsAsFactors = FALSE
)

cat("\nSubgroup slopes from the interaction model\n")
cat("(linear combinations, cluster-robust on state, CR2):\n\n")
print(transform(burden_interaction_subgroups,
                estimate = round(estimate, 4),
                std_error = round(std_error, 4),
                t_stat   = round(t_stat,   3),
                p_value  = round(p_value,  4)),
      row.names = FALSE)
cat("\n")

# ---- (5) Write CSV (uses readr directly, no script helper) ---------------
out_dir <- if (exists("dir_output")) dir_output else getwd()
readr::write_csv(burden_interaction_subgroups,
                 file.path(out_dir, "burden_lens_interaction_subgroups_DALYs.csv"))
log_msg(sprintf("    burden_lens_interaction_subgroups_DALYs.csv         — 3 subgroup slopes (Low/Mid/High)"))




##================================================================
## 12. SENSITIVITIES
##================================================================

## ---- 12a. Temporal sensitivities (per-case GBD, pooled) --------
log_msg("\n--- Sensitivity: temporal structure ---")
temporal_specs <- list(
  "All current year (primary)" = list(X = X_GBD, M = M_main,
                                      Y_DALYs = "as_daly_prev_ratio_log",
                                      Y_Mortality = "as_mort_prev_ratio_log"),
  "Lag X only"                  = list(X = "rw_dex_hiv_prev_ratio_log_l1", M = M_main,
                                       Y_DALYs = "as_daly_prev_ratio_log",
                                       Y_Mortality = "as_mort_prev_ratio_log"),
  "Lead Y only"                 = list(X = X_GBD, M = M_main,
                                       Y_DALYs = "as_daly_prev_ratio_log_f1",
                                       Y_Mortality = "as_mort_prev_ratio_log_f1"),
  "Lag X + Lead Y"              = list(X = "rw_dex_hiv_prev_ratio_log_l1", M = M_main,
                                       Y_DALYs = "as_daly_prev_ratio_log_f1",
                                       Y_Mortality = "as_mort_prev_ratio_log_f1")
)
temporal_rows <- list()
for (spec_label in names(temporal_specs)) {
  s <- temporal_specs[[spec_label]]
  for (y_label in c("DALYs", "Mortality")) {
    Y <- if (y_label == "DALYs") s$Y_DALYs else s$Y_Mortality
    cols <- c(Y, s$X, s$M, "log_incidence_per_100k", "log_prop_homeless",
              "race_prop_BLCK", "race_prop_HISP", "year_factor", "location_id")
    d <- df[, cols] %>% drop_na()
    res <- fit_mediation(d, s$X, s$M, Y, covars_primary)
    if (is.null(res)) next
    temporal_rows[[paste(spec_label, y_label, sep = "|")]] <- tibble(
      spec = spec_label, outcome = y_label,
      predictor_X = s$X, outcome_Y = Y,
      a = res$a, a_p = res$a_p, a_sig = sig_lab(res$a_p),
      b = res$b, b_p = res$b_p, b_sig = sig_lab(res$b_p),
      c_prime = res$c_prime, c_prime_p = res$c_prime_p,
      c_prime_sig = sig_lab(res$c_prime_p),
      c_total = res$c_total, c_total_p = res$c_total_p,
      c_total_sig = sig_lab(res$c_total_p),
      indirect = res$indirect, n_obs = res$n_obs
    )
  }
}
temporal_table <- bind_rows(temporal_rows)

## ---- 12b. Per-capita full mediation (pooled, no strata) --------
log_msg("--- Sensitivity: per-capita lens (full mediation, pooled) ---")
covars_percapita <- "log_incidence_per_100k + log_prevalence_per_100k_l1 + log_prop_homeless + race_prop_BLCK + race_prop_HISP + year_factor"
percapita_rows <- list()
for (y_label in c("DALYs", "Mortality", "CDC_Mortality")) {
  Y <- switch(y_label,
              DALYs         = "log_daly_per_capita",
              Mortality     = "log_mortality_per_capita",
              CDC_Mortality = "log_cdc_mortality_per_capita")
  cols <- c(Y, X_PC, M_main, "log_incidence_per_100k",
            "log_prevalence_per_100k_l1", "log_prop_homeless",
            "race_prop_BLCK", "race_prop_HISP", "year_factor", "location_id")
  d <- df[, cols] %>% drop_na()
  res <- fit_mediation(d, X_PC, M_main, Y, covars_percapita)
  if (is.null(res)) next
  percapita_rows[[y_label]] <- tibble(
    outcome = y_label, predictor_X = X_PC, mediator_M = M_main, outcome_Y = Y,
    a = res$a, a_p = res$a_p, a_sig = sig_lab(res$a_p),
    b = res$b, b_p = res$b_p, b_sig = sig_lab(res$b_p),
    c_prime = res$c_prime, c_prime_p = res$c_prime_p,
    c_prime_sig = sig_lab(res$c_prime_p),
    c_total = res$c_total, c_total_p = res$c_total_p,
    c_total_sig = sig_lab(res$c_total_p),
    indirect = res$indirect, n_obs = res$n_obs
  )
}
percapita_med_table <- bind_rows(percapita_rows)

## ---- 12c. Per-case CDC full mediation (CDC strata) ------------
## CDC does not publish DALYs.  Within the CDC framework, the only
## defensible outcome is CDC mortality / CDC prev (denominators match).
## We also report a GBD-mortality-over-CDC-prev variant for comparison.
log_msg("--- Sensitivity: per-case CDC full mediation (CDC strata) ---")
df_cdc <- df %>% dplyr::filter(!is.na(prev_tercile_CDC))
CDC_med <- list()
CDC_med[["CDCmort_CDCprev"]] <- run_stratified_mediation(
  df_cdc, X = X_CDC, M = M_main, Y = "log_cdc_mort_per_cdc_prev",
  covars = covars_primary, tercile_var = "prev_tercile_CDC",
  verbose_label = "CDC-CDCmort"
)
CDC_med[["GBDmort_CDCprev"]] <- run_stratified_mediation(
  df_cdc, X = X_CDC, M = M_main, Y = "log_gbd_mort_per_cdc_prev",
  covars = covars_primary, tercile_var = "prev_tercile_CDC",
  verbose_label = "CDC-GBDmort"
)
percase_CDC_long <- bind_rows(
  mediation_to_long(CDC_med[["CDCmort_CDCprev"]], lens = "per_case_CDC",
                    outcome = "log(CDC mortality per CDC prev case)",
                    tercile_source = "CDC-prevalence (first avail 2010-2012)",
                    predictor = X_CDC, mediator = M_main),
  mediation_to_long(CDC_med[["GBDmort_CDCprev"]], lens = "per_case_CDC",
                    outcome = "log(GBD mortality per CDC prev case)",
                    tercile_source = "CDC-prevalence (first avail 2010-2012)",
                    predictor = X_CDC, mediator = M_main)
)

## ---- 12d. CDC denominator-consistency (S7 + S8) ---------------
log_msg("--- Sensitivity: CDC denominator consistency (S7/S8) ---")
denom_rows <- list()
## S7: within-GBD framework, CDC mortality numerator swap
d_S7 <- df[, c("log_cdc_mort_per_gbd_prev", X_GBD,
               "log_incidence_per_100k", "log_prop_homeless",
               "race_prop_BLCK", "race_prop_HISP", "year_factor",
               "location_id")] %>% drop_na()
fit_S7 <- lm(as.formula(paste0("log_cdc_mort_per_gbd_prev ~ ", X_GBD, " + ", rhs_primary())),
             data = d_S7)
c_S7 <- extract_lm_CR2(fit_S7, d_S7$location_id) %>%
  dplyr::filter(term == X_GBD)
denom_rows[["S7"]] <- tibble(
  spec = "S7: Within-GBD framework, CDC mortality numerator",
  predictor = "log(spending / GBD prev), year t",
  outcome   = "log(CDC mort / GBD prev)",
  estimate = c_S7$estimate, std_error = c_S7$std_error,
  p_value = c_S7$p_value, sig_label = sig_lab(c_S7$p_value),
  n_obs = nobs(fit_S7)
)
for (variant in c("S8a_CDCmort", "S8b_GBDmort")) {
  Y <- if (variant == "S8a_CDCmort") "log_cdc_mort_per_cdc_prev"
  else "log_gbd_mort_per_cdc_prev"
  d_S8 <- df[, c(Y, X_CDC, "log_incidence_per_100k", "log_prop_homeless",
                 "race_prop_BLCK", "race_prop_HISP", "year_factor",
                 "location_id")] %>% drop_na()
  fit_S8 <- lm(as.formula(paste0(Y, " ~ ", X_CDC, " + ", rhs_primary())),
               data = d_S8)
  c_S8 <- extract_lm_CR2(fit_S8, d_S8$location_id) %>%
    dplyr::filter(term == X_CDC)
  denom_rows[[variant]] <- tibble(
    spec = paste0(variant, ": Within-CDC framework"),
    predictor = "log(spending / CDC prev), year t",
    outcome = ifelse(grepl("CDC", variant),
                     "log(CDC mort / CDC prev)",
                     "log(GBD mort / CDC prev)"),
    estimate = c_S8$estimate, std_error = c_S8$std_error,
    p_value = c_S8$p_value, sig_label = sig_lab(c_S8$p_value),
    n_obs = nobs(fit_S8)
  )
}
denom_table <- bind_rows(denom_rows)

## ---- 12e. Leave-one-state-out (DALYs burden lens) --------------
log_msg("--- Sensitivity: leave-one-state-out (DALYs burden lens) ---")
state_ids <- unique(df$location_id)
state_names <- df %>% group_by(location_id) %>%
  summarise(location_name = first(location_name), .groups = "drop")
loo_rows <- list()
for (sid in state_ids) {
  d_loo <- df %>% dplyr::filter(location_id != sid,
                                !is.na(as_daly_prev_ratio_log),
                                !is.na(log_incidence_per_100k))
  fit_loo <- lm(as.formula(paste0("as_daly_prev_ratio_log ~ ", X_GBD, " + ", rhs_primary())),
                data = d_loo)
  ce <- coef(fit_loo)[X_GBD]
  loo_rows[[as.character(sid)]] <- tibble(
    excluded_location_id   = sid,
    excluded_location_name = state_names$location_name[state_names$location_id == sid],
    beta_X = as.numeric(ce),
    n_obs = nobs(fit_loo)
  )
}
loo_table <- bind_rows(loo_rows) %>% arrange(beta_X)
log_msg(sprintf("  LOO beta_X range [%.4f, %.4f], median %.4f",
                min(loo_table$beta_X), max(loo_table$beta_X),
                median(loo_table$beta_X)))

## ---- 12f. Drop 2010-2012 ---------------------------------------
log_msg("--- Sensitivity: drop 2010-2012 ---")
d_drop <- df %>% dplyr::filter(year_id >= 2013,
                               !is.na(as_daly_prev_ratio_log),
                               !is.na(log_incidence_per_100k))
fit_drop <- lm(as.formula(paste0("as_daly_prev_ratio_log ~ ", X_GBD, " + ", rhs_primary())),
               data = d_drop)
drop_row <- extract_lm_CR2(fit_drop, d_drop$location_id) %>%
  dplyr::filter(term == X_GBD)
drop_table <- tibble(
  spec = "Drop 2010-2012", outcome = "DALYs/case",
  estimate = drop_row$estimate, std_error = drop_row$std_error,
  p_value = drop_row$p_value, sig_label = sig_lab(drop_row$p_value),
  n_obs = nobs(fit_drop)
)

## ---- 12g. ACA Medicaid interaction -----------------------------
log_msg("--- Sensitivity: Medicaid expansion (ACA) interaction ---")
d_aca <- df %>% dplyr::filter(!is.na(as_daly_prev_ratio_log),
                              !is.na(log_incidence_per_100k),
                              !is.na(aca_implemented_status))
fit_aca <- lm(as.formula(paste0("as_daly_prev_ratio_log ~ ", X_GBD,
                                " * aca_implemented_status + ", rhs_primary())),
              data = d_aca)
aca_rows <- extract_lm_CR2(fit_aca, d_aca$location_id) %>%
  dplyr::filter(grepl(X_GBD, term, fixed = TRUE) | grepl("aca", term)) %>%
  mutate(spec = "Medicaid expansion (ACA) interaction",
         sig_label = sig_lab(p_value), n_obs = nobs(fit_aca))

## ---- 12h. RW-exclusive spending --------------------------------
log_msg("--- Sensitivity: Ryan-White-exclusive spending ---")
d_rw <- df %>% dplyr::filter(!is.na(as_daly_prev_ratio_log),
                             !is.na(log_spend_excl_rw_per_case),
                             !is.na(log_incidence_per_100k))
fit_rw <- lm(as.formula(paste0("as_daly_prev_ratio_log ~ log_spend_excl_rw_per_case + ",
                               rhs_primary())),
             data = d_rw)
rw_row <- extract_lm_CR2(fit_rw, d_rw$location_id) %>%
  dplyr::filter(term == "log_spend_excl_rw_per_case") %>%
  mutate(spec = "Ryan-White-exclusive spending",
         sig_label = sig_lab(p_value), n_obs = nobs(fit_rw))

policy_table <- bind_rows(
  aca_rows %>% transmute(spec, term, estimate, std_error, p_value,
                         sig_label, n_obs),
  rw_row %>% transmute(spec, term, estimate, std_error, p_value,
                       sig_label, n_obs)
)

##================================================================
## 13. REFERENCE TABLES
##================================================================

reference_tercile_GBD <- df %>%
  distinct(location_id, location_name, log_gbd_prev_2010, prev_tercile_GBD) %>%
  arrange(prev_tercile_GBD, location_name)

reference_tercile_CDC <- df %>%
  distinct(location_id, location_name, log_cdc_prev_baseline,
           cdc_prev_baseline_year, prev_tercile_CDC) %>%
  arrange(prev_tercile_CDC, location_name)

state_means <- df %>%
  group_by(location_id) %>%
  summarise(
    spending_pc_mean   = mean(log_spending_per_capita, na.rm = TRUE),
    daly_pc_mean       = mean(log_daly_per_capita, na.rm = TRUE),
    spending_case_mean = mean(rw_dex_hiv_prev_ratio_log, na.rm = TRUE),
    daly_case_mean     = mean(as_daly_prev_ratio_log, na.rm = TRUE),
    .groups = "drop"
  )
reference_variance_decomp <- tibble(
  variable    = c("log spending per capita", "log DALY per capita",
                  "log spending per GBD prev case", "log DALY per GBD prev case"),
  total_var   = c(var(df$log_spending_per_capita, na.rm = TRUE),
                  var(df$log_daly_per_capita, na.rm = TRUE),
                  var(df$rw_dex_hiv_prev_ratio_log, na.rm = TRUE),
                  var(df$as_daly_prev_ratio_log, na.rm = TRUE)),
  between_var = c(var(state_means$spending_pc_mean, na.rm = TRUE),
                  var(state_means$daly_pc_mean, na.rm = TRUE),
                  var(state_means$spending_case_mean, na.rm = TRUE),
                  var(state_means$daly_case_mean, na.rm = TRUE))
) %>%
  mutate(within_var  = total_var - between_var,
         between_pct = round(between_var / total_var * 100, 1),
         within_pct  = round(within_var  / total_var * 100, 1))

##================================================================
## 14. SAMPLE CONSTRUCTION NOTES
##================================================================
sample_notes <- tibble(
  step = c(
    "Full panel (51 states x 10 years, 2010-2019)",
    "Burden lens primary: DALYs ~ X + contemp incidence + race + homeless + year FE",
    "Burden lens with lagged incidence (drops 2010 due to t-1 lag)",
    "Cascade X -> K (knowledge): lagged-prev control; K otherwise complete",
    "Cascade X -> L (linkage): lagged-prev control + L availability",
    "Cascade X -> V: lagged-prev control + V availability",
    "Cascade X -> K x L x V composite (PRIMARY): lagged-prev + ALL three indicators (L binding)",
    "Cascade X -> K x V composite (SENSITIVITY): lagged-prev + V availability (V binding for KxV)",
    "Mediation chain X -> M -> Y (PRIMARY: K x L x V; GBD framework, matched sample)",
    "Mediation chain X -> M -> Y (SENSITIVITY: K x V; GBD framework, matched sample)",
    "Per-case CDC analyses (states with >= 1 non-missing CDC prev value in 2010-2012)"
  ),
  n_state_years = c(
    nrow(df),
    sum(!is.na(df$as_daly_prev_ratio_log) & !is.na(df$rw_dex_hiv_prev_ratio_log) &
          !is.na(df$log_incidence_per_100k)),
    sum(!is.na(df$as_daly_prev_ratio_log) & !is.na(df$rw_dex_hiv_prev_ratio_log) &
          !is.na(df$log_incidence_per_100k_l1)),
    sum(!is.na(df$cdc_knowledge_status) & !is.na(df$log_prevalence_per_100k_l1) &
          !is.na(df$rw_dex_hiv_prev_ratio_log)),
    sum(!is.na(df$cdc_linkage_1mo) & !is.na(df$log_prevalence_per_100k_l1) &
          !is.na(df$rw_dex_hiv_prev_ratio_log)),
    sum(!is.na(df$cdc_viral_suppress) & !is.na(df$log_prevalence_per_100k_l1) &
          !is.na(df$rw_dex_hiv_prev_ratio_log)),
    sum(!is.na(df$cdc_klv_composite) & !is.na(df$log_prevalence_per_100k_l1) &
          !is.na(df$rw_dex_hiv_prev_ratio_log)),
    sum(!is.na(df$cdc_kv_composite) & !is.na(df$log_prevalence_per_100k_l1) &
          !is.na(df$rw_dex_hiv_prev_ratio_log)),
    sum(!is.na(df$cdc_klv_composite) & !is.na(df$as_daly_prev_ratio_log) &
          !is.na(df$rw_dex_hiv_prev_ratio_log) & !is.na(df$log_incidence_per_100k)),
    sum(!is.na(df$cdc_kv_composite) & !is.na(df$as_daly_prev_ratio_log) &
          !is.na(df$rw_dex_hiv_prev_ratio_log) & !is.na(df$log_incidence_per_100k)),
    sum(!is.na(df$log_spend_per_cdc_prev) & !is.na(df$prev_tercile_CDC))
  ),
  reason_for_drop = c(
    "Baseline",
    "GBD outcome and X complete; primary control set complete",
    "Lagged incidence undefined for 2010 (drops 51 state-years)",
    "Lagged prevalence undefined for 2010; K otherwise complete (510 - 51 = 459)",
    "Lagged prevalence (2010) + L missing in early years (CDC ATLAS expansion)",
    "Lagged prevalence (2010) + V missing in early years (CDC ATLAS expansion)",
    "L is the binding constraint for the K x L x V composite (sparser than V in 2010-2014)",
    "V is the binding constraint for the K x V composite (K is complete)",
    "K + L + V joint availability + DALYs outcome non-missing; matched for c = c' + a*b algebra",
    "K + V joint availability + DALYs outcome non-missing (broader sample, omits L)",
    "All 51 states had non-missing CDC prev in at least one of 2010-2012"
  )
)

##================================================================
## 15. HEADLINE SUMMARY
##================================================================
## Three primary cells, hand-picked from the long tables.
## NOTE: burden lens c-total is at n=510 (the reduced-form estimate),
## while the cascade b-path is at n=347 (matched-sample mediation).
##================================================================
burden_primary_DALYs <- burden_DALYs %>%
  dplyr::filter(spec_label == "Primary: contemporaneous incidence")

primary_b_mortality <- primary_Mortality_long %>%
  dplyr::filter(tercile == "Pooled", path == "b (M->Y|X)")

primary_a_high_DALYs <- primary_DALYs_long %>%
  dplyr::filter(tercile == "High", path == "a (X->M)")

headline_summary <- tibble(
  claim = c(
    "(1) Burden lens reduced form, DALYs/case, full sample",
    "(2) Cascade -> Mortality b-path, matched sample",
    "(3) Spending -> Cascade in High-prev states (a-path)"
  ),
  lens = c("per_case_GBD", "per_case_GBD", "per_case_GBD"),
  outcome = c("log(DALYs per GBD prev case)",
              "log(GBD mortality per GBD prev case)",
              "K x L x V cascade composite (full 90-90-90)"),
  parameter = c("c (total, X->Y)",
                "b (M->Y|X)",
                "a (X->M), High tercile"),
  estimate = c(
    if (nrow(burden_primary_DALYs) > 0) burden_primary_DALYs$estimate[1] else NA_real_,
    if (nrow(primary_b_mortality)   > 0) primary_b_mortality$estimate[1]   else NA_real_,
    if (nrow(primary_a_high_DALYs)  > 0) primary_a_high_DALYs$estimate[1]  else NA_real_
  ),
  std_error = c(
    if (nrow(burden_primary_DALYs) > 0) burden_primary_DALYs$std_error[1] else NA_real_,
    if (nrow(primary_b_mortality)   > 0) primary_b_mortality$std_error[1]   else NA_real_,
    if (nrow(primary_a_high_DALYs)  > 0) primary_a_high_DALYs$std_error[1]  else NA_real_
  ),
  p_value = c(
    if (nrow(burden_primary_DALYs) > 0) burden_primary_DALYs$p_value[1] else NA_real_,
    if (nrow(primary_b_mortality)   > 0) primary_b_mortality$p_value[1]   else NA_real_,
    if (nrow(primary_a_high_DALYs)  > 0) primary_a_high_DALYs$p_value[1]  else NA_real_
  ),
  n_obs = c(
    if (nrow(burden_primary_DALYs) > 0) burden_primary_DALYs$n_obs[1] else NA_integer_,
    if (nrow(primary_b_mortality)   > 0) primary_b_mortality$n_obs[1]   else NA_integer_,
    if (nrow(primary_a_high_DALYs)  > 0) primary_a_high_DALYs$n_obs[1]  else NA_integer_
  )
) %>%
  mutate(sig_label = sig_lab(p_value))

##================================================================
## 16. BIVARIATE SCATTER PNGs
##================================================================
log_msg("\n=== BIVARIATE SCATTERS ===")

## Per-case GBD framework, stratified by GBD tercile
scatter_GBD <- bind_rows(
  df %>% transmute(location_name, year_id,
                   tercile = prev_tercile_GBD,
                   log_x = rw_dex_hiv_prev_ratio_log,
                   log_y = as_daly_prev_ratio_log,
                   outcome = "A. DALYs per GBD prevalent case"),
  df %>% transmute(location_name, year_id,
                   tercile = prev_tercile_GBD,
                   log_x = rw_dex_hiv_prev_ratio_log,
                   log_y = as_mort_prev_ratio_log,
                   outcome = "B. GBD mortality per GBD prevalent case")
) %>% dplyr::filter(!is.na(log_x), !is.na(log_y))

r_labels_GBD <- scatter_GBD %>%
  group_by(tercile, outcome) %>%
  summarise(r = cor(log_x, log_y, use = "complete.obs"),
            n = dplyr::n(), .groups = "drop") %>%
  mutate(label = sprintf("r = %+.2f  n = %d", r, n))

p_GBD <- ggplot(scatter_GBD, aes(x = log_x, y = log_y)) +
  geom_point(alpha = 0.45, size = 0.9, colour = "#4A6FA5") +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
              colour = "navy", fill = "navy", alpha = 0.18) +
  geom_text(data = r_labels_GBD, aes(x = -Inf, y = Inf, label = label),
            hjust = -0.1, vjust = 1.5, size = 3, inherit.aes = FALSE) +
  facet_grid(tercile ~ outcome, scales = "free", switch = "y") +
  labs(
    title = "Bivariate spending-outcome association by GBD 2010-baseline tercile",
    subtitle = "Per-case GBD framework. One point per state-year, 2010-2019.",
    x = "log HIV spending per GBD prevalent case",
    y = "log outcome per GBD prevalent case"
  ) +
  theme_minimal(base_size = 10) +
  theme(strip.placement = "outside",
        strip.text = element_text(face = "bold"),
        panel.spacing = unit(0.5, "lines"))

ggsave(file.path(dir_output, "bivariate_per_case_GBD_stratified.png"),
       p_GBD, width = 10, height = 11, dpi = 220)
log_msg("Wrote bivariate_per_case_GBD_stratified.png")

## Per-case CDC framework, stratified by CDC tercile (sensitivity exhibit)
scatter_CDC <- bind_rows(
  df %>% transmute(location_name, year_id,
                   tercile = prev_tercile_CDC,
                   log_x = log_spend_per_cdc_prev,
                   log_y = log_cdc_mort_per_cdc_prev,
                   outcome = "CDC mortality per CDC prevalent case")
) %>% dplyr::filter(!is.na(log_x), !is.na(log_y), !is.na(tercile))

if (nrow(scatter_CDC) > 0) {
  r_labels_CDC <- scatter_CDC %>%
    group_by(tercile, outcome) %>%
    summarise(r = cor(log_x, log_y, use = "complete.obs"),
              n = dplyr::n(), .groups = "drop") %>%
    mutate(label = sprintf("r = %+.2f  n = %d", r, n))
  p_CDC <- ggplot(scatter_CDC, aes(x = log_x, y = log_y)) +
    geom_point(alpha = 0.45, size = 0.9, colour = "#A56A4A") +
    geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
                colour = "#7A3B1B", fill = "#7A3B1B", alpha = 0.18) +
    geom_text(data = r_labels_CDC, aes(x = -Inf, y = Inf, label = label),
              hjust = -0.1, vjust = 1.5, size = 3, inherit.aes = FALSE) +
    facet_grid(tercile ~ outcome, scales = "free", switch = "y") +
    labs(
      title = "Bivariate spending-outcome association by CDC 2010-2012 baseline tercile",
      subtitle = "Per-case CDC framework. One point per state-year.",
      x = "log HIV spending per CDC prevalent case",
      y = "log CDC mortality per CDC prevalent case"
    ) +
    theme_minimal(base_size = 10) +
    theme(strip.placement = "outside",
          strip.text = element_text(face = "bold"),
          panel.spacing = unit(0.5, "lines"))
  ggsave(file.path(dir_output, "bivariate_per_case_CDC_stratified.png"),
         p_CDC, width = 10, height = 11, dpi = 220)
  log_msg("Wrote bivariate_per_case_CDC_stratified.png")
}

## Per-capita framework, pooled (no stratification)
scatter_PC <- bind_rows(
  df %>% transmute(location_name, year_id,
                   log_x = log_spending_per_capita,
                   log_y = log_daly_per_capita,
                   outcome = "A. DALYs per capita (GBD)"),
  df %>% transmute(location_name, year_id,
                   log_x = log_spending_per_capita,
                   log_y = log_mortality_per_capita,
                   outcome = "B. GBD mortality per capita")
) %>% dplyr::filter(!is.na(log_x), !is.na(log_y))

r_labels_PC <- scatter_PC %>%
  group_by(outcome) %>%
  summarise(r = cor(log_x, log_y, use = "complete.obs"),
            n = dplyr::n(), .groups = "drop") %>%
  mutate(label = sprintf("r = %+.2f  n = %d", r, n))

p_PC <- ggplot(scatter_PC, aes(x = log_x, y = log_y)) +
  geom_point(alpha = 0.45, size = 0.9, colour = "#A55050") +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
              colour = "darkred", fill = "darkred", alpha = 0.18) +
  geom_text(data = r_labels_PC, aes(x = -Inf, y = Inf, label = label),
            hjust = -0.1, vjust = 1.5, size = 3, inherit.aes = FALSE) +
  facet_wrap(~ outcome, scales = "free") +
  labs(
    title = "Bivariate per-capita spending-outcome association (pooled)",
    subtitle = "Per-capita framework. No tercile stratification (denominator is not prevalence).",
    x = "log HIV spending per capita",
    y = "log outcome per capita"
  ) +
  theme_minimal(base_size = 10) +
  theme(strip.text = element_text(face = "bold"))

ggsave(file.path(dir_output, "bivariate_per_capita_pooled.png"),
       p_PC, width = 10, height = 5.5, dpi = 220)
log_msg("Wrote bivariate_per_capita_pooled.png")

##================================================================
## 17. WRITE ALL CSVs
##================================================================
log_msg("\n=== WRITING CSVs ===")

write_csv_safe <- function(obj, fname) {
  fpath <- file.path(dir_output, fname)
  write.csv(obj, fpath, row.names = FALSE, na = "")
  log_msg("  Wrote ", fname, "  (", nrow(obj), " rows)")
}

## --- Headline ----------------------------------------------------
write_csv_safe(headline_summary, "headline_summary.csv")

## --- Primary tier (DALYs) ----------------------------------------
write_csv_safe(primary_DALYs_long, "primary_DALYs_mediation_per_case_GBD.csv")

## --- Cascade X -> M tier -----------------------------------------
write_csv_safe(cascade_GBD, "cascade_X_to_M_per_case_GBD_stratified.csv")
write_csv_safe(cascade_CDC, "cascade_X_to_M_per_case_CDC_stratified.csv")
write_csv_safe(cascade_PC,  "cascade_X_to_M_per_capita_pooled.csv")

## --- Cascade M -> Y tier -----------------------------------------
write_csv_safe(M_to_Y_table, "cascade_M_to_Y_pooled.csv")

## --- Burden lens tier --------------------------------------------
write_csv_safe(burden_DALYs,     "burden_lens_DALYs.csv")
write_csv_safe(burden_Mortality, "burden_lens_Mortality.csv")

## --- Sensitivity tier --------------------------------------------
write_csv_safe(primary_Mortality_long, "sens_mortality_mediation_per_case_GBD.csv")
write_csv_safe(temporal_table,         "sens_temporal_structure.csv")
write_csv_safe(percapita_med_table,    "sens_per_capita_mediation_pooled.csv")
write_csv_safe(percase_CDC_long,       "sens_per_case_CDC_mediation_stratified.csv")
write_csv_safe(denom_table,            "sens_CDC_denom_consistency.csv")
write_csv_safe(loo_table,              "sens_leave_one_state_out.csv")
write_csv_safe(drop_table,             "sens_drop_2010_2012.csv")
write_csv_safe(policy_table,           "sens_policy_strat.csv")

## --- Reference tier ----------------------------------------------
write_csv_safe(reference_tercile_GBD,     "reference_tercile_assignment_GBD.csv")
write_csv_safe(reference_tercile_CDC,     "reference_tercile_assignment_CDC.csv")
write_csv_safe(reference_variance_decomp, "reference_variance_decomp.csv")
write_csv_safe(sample_notes,              "sample_construction_notes.csv")

##================================================================
## 18. END-OF-RUN LOG SUMMARY
##================================================================
log_msg("\n================ RUN COMPLETE ================")
log_msg("Outputs in: ", dir_output)
log_msg("File index:")
log_msg("  HEADLINE")
log_msg("    headline_summary.csv                              — three primary cells")
log_msg("  PRIMARY (DALYs)")
log_msg("    primary_DALYs_mediation_per_case_GBD.csv          — 5 paths x 4 strata")
log_msg("  CASCADE X -> M")
log_msg("    cascade_X_to_M_per_case_GBD_stratified.csv        — 4 mediators x 4 strata")
log_msg("    cascade_X_to_M_per_case_CDC_stratified.csv        — 4 mediators x 4 strata (CDC strata)")
log_msg("    cascade_X_to_M_per_capita_pooled.csv              — 4 mediators, pooled only")
log_msg("  CASCADE M -> Y")
log_msg("    cascade_M_to_Y_pooled.csv                         — 4 mediators x 2 outcomes x 2 specs")
log_msg("  BURDEN LENS")
log_msg("    burden_lens_DALYs.csv                             — 6 specs, pooled (n=510)")
log_msg("    burden_lens_Mortality.csv                         — 6 specs, pooled")
log_msg("  SENSITIVITIES")
log_msg("    sens_mortality_mediation_per_case_GBD.csv")
log_msg("    sens_temporal_structure.csv")
log_msg("    sens_per_capita_mediation_pooled.csv")
log_msg("    sens_per_case_CDC_mediation_stratified.csv")
log_msg("    sens_CDC_denom_consistency.csv")
log_msg("    sens_leave_one_state_out.csv")
log_msg("    sens_drop_2010_2012.csv")
log_msg("    sens_policy_strat.csv")
log_msg("  REFERENCE")
log_msg("    reference_tercile_assignment_GBD.csv")
log_msg("    reference_tercile_assignment_CDC.csv")
log_msg("    reference_variance_decomp.csv")
log_msg("    sample_construction_notes.csv")
log_msg("  PLOTS")
log_msg("    bivariate_per_case_GBD_stratified.png             — primary visual exhibit")
log_msg("    bivariate_per_case_CDC_stratified.png             — CDC sensitivity")
log_msg("    bivariate_per_capita_pooled.png                   — per-capita sensitivity")
log_msg("  LOG")
log_msg("    run_log.txt                                       — diagnostic log")

log_msg("\nPrimary cascade mediator: K x L x V (full 90-90-90 product); K x V reported as sensitivity.")
log_msg("Next step: build memo and committee deck from headline_summary.csv + key files.")

##================================================================
## END OF SCRIPT
##================================================================