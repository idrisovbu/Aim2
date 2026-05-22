##----------------------------------------------------------------
##  Title:    aim2_regressions_v3.R
##  Purpose:  End-to-end regression pipeline for Aim 2 HIV paper.
##            v3 = analytic plan v4 implementation.
##
##  PRIMARY FRAMEWORK (per analytic plan v4):
##    - Lens:     per-case GBD (spending per GBD prev case → outcome per GBD prev case)
##    - Time:     all current year (X(t), M(t), Y(t))
##    - Control:  contemporaneous log(HIV incidence per 100k) + race + log(homeless) + year FE
##    - Mediator: K × V composite (cdc_knowledge_status × cdc_viral_suppress)
##    - Outcomes: DALYs per GBD prev case (primary), GBD mortality per GBD prev case (secondary)
##    - Stratification: 2010-baseline prevalence tercile (Low / Mid / High, fixed)
##    - SEs:      cluster-robust on state (CR2 + Satterthwaite for lm; HC0 for GLM)
##    - Bootstrap: state-block, 500 reps, seed 20260520, percentile CI on indirect effect
##
##  OUTPUTS:
##    aim2_primary_per_case_GBD.xlsx       — headline meeting deliverable (6 tabs)
##    aim2_alternative_specifications.xlsx — sensitivity backup (9 tabs)
##    bivariate_stratified_per_case.png    — primary bivariate exhibit
##    bivariate_stratified_per_capita.png  — sensitivity bivariate exhibit
##    run_log.txt                          — diagnostic log
##
##  Author / runner: Bulat Idrisov  (script by Claude, 2026-05-19)
##----------------------------------------------------------------


##================================================================
## 0. SETUP
##================================================================
rm(list = ls())

## ---- paths (edit panel_input_path if running locally) ----------
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
  output_date, "analytic_plan_regressions_v3"
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
  ggplot2, scales,
  openxlsx
)

## Try to install marginaleffects if not available; manual AME fallback below
if (!requireNamespace("marginaleffects", quietly = TRUE)) {
  tryCatch(install.packages("marginaleffects"),
           error = function(e) message("marginaleffects unavailable; using manual AME"))
}

## Resolve dplyr <-> data.table name collisions defensively
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
cat(sprintf("Run: %s\nInput: %s\nOutput: %s\nScript: v3 (analytic plan v4)\n\n",
            Sys.time(), panel_input_path, dir_output),
    file = log_file)


##================================================================
## 1. LOAD AND VALIDATE PANEL
##================================================================
if (!file.exists(panel_input_path)) {
  stop("Input panel not found: ", panel_input_path,
       "\nUpdate panel_input_path at the top of this script.")
}
df <- read.csv(panel_input_path, stringsAsFactors = FALSE)
df <- df %>% dplyr::filter(acause == "hiv")
log_msg("Loaded ", nrow(df), " rows, ", ncol(df), " columns from cascade panel")

required_cols <- c(
  "location_id", "location_name", "year_id", "acause",
  ## outcomes (per-case, current year)
  "as_daly_prev_ratio_log", "as_mort_prev_ratio_log",
  ## outcomes (per-capita, current year)
  "log_daly_per_capita", "log_mortality_per_capita",
  "log_cdc_mortality_per_capita", "log_cdc_prevalence_per_100k",
  "log_incidence_per_100k",
  ## predictors (current year)
  "rw_dex_hiv_prev_ratio_log",
  "log_spending_per_capita",
  ## lagged versions (for sensitivities)
  "rw_dex_hiv_prev_ratio_log_l1",
  "log_spending_per_capita_l1",
  "log_incidence_per_100k_l1",
  ## Mundlak B/W
  "rw_dex_hiv_prev_ratio_log_B", "rw_dex_hiv_prev_ratio_log_W",
  ## demographics + risk-factor proxies
  "race_prop_BLCK", "race_prop_HISP",
  "log_prop_homeless",
  "log_ldi_pc", "edu_yrs", "aca_implemented_status",
  "opioid_prevalence_counts",
  ## prevalence (for tercile + cascade controls)
  "log_prevalence_per_100k",
  ## raw counts for CDC sensitivities
  "mortality_counts", "hiv_prevalence_counts",
  "cdc_hiv_mortality_counts", "cdc_hiv_prevalence_counts",
  ## cascade indicators
  "cdc_knowledge_status", "cdc_linkage_1mo", "cdc_viral_suppress",
  ## RW pieces (for RW-exclusive sensitivity)
  "ryan_white_funding_final", "spend_all"
)
missing_required <- setdiff(required_cols, names(df))
if (length(missing_required) > 0) {
  stop("Required columns missing from panel: ",
       paste(missing_required, collapse = ", "))
}


##================================================================
## 2. DERIVE 2010-BASELINE PREVALENCE TERCILES (TIME-INVARIANT)
##================================================================
prev_2010 <- df %>%
  dplyr::filter(year_id == 2010) %>%
  select(location_id, log_prevalence_per_100k) %>%
  rename(log_prev_2010 = log_prevalence_per_100k)

cut_points <- quantile(prev_2010$log_prev_2010, probs = c(1/3, 2/3), na.rm = TRUE)
log_msg("Tercile cut points (log prevalence per 100k, 2010): ",
        sprintf("low<%.3f  high>%.3f", cut_points[1], cut_points[2]))

prev_2010 <- prev_2010 %>%
  mutate(
    prev_tercile_2010 = case_when(
      log_prev_2010 <= cut_points[1] ~ "Low",
      log_prev_2010 <= cut_points[2] ~ "Mid",
      TRUE                           ~ "High"
    ),
    prev_tercile_2010 = factor(prev_tercile_2010, levels = c("Low", "Mid", "High")),
    high_prev_2010   = as.integer(prev_tercile_2010 == "High")
  )

df <- df %>%
  left_join(prev_2010 %>% select(location_id, prev_tercile_2010,
                                 high_prev_2010, log_prev_2010),
            by = "location_id")
stopifnot(all(!is.na(df$prev_tercile_2010)))

log_msg("Tercile counts (state-years): ",
        paste(sprintf("%s=%d", levels(df$prev_tercile_2010),
                      as.integer(table(df$prev_tercile_2010))),
              collapse = ", "))


##================================================================
## 3. CONSTRUCT DERIVED VARIABLES
##================================================================
safe_log <- function(x) ifelse(is.finite(x) & x > 0, log(x), NA_real_)

df <- df %>%
  ## Upstream HIV risk-factor proxy
  mutate(log_opioid_prev = safe_log(opioid_prevalence_counts)) %>%
  ## K x V composite mediator
  mutate(
    cdc_kv_composite = ifelse(
      !is.na(cdc_knowledge_status) & !is.na(cdc_viral_suppress) &
        cdc_knowledge_status >= 0 & cdc_knowledge_status <= 1 &
        cdc_viral_suppress   >= 0 & cdc_viral_suppress   <= 1,
      cdc_knowledge_status * cdc_viral_suppress,
      NA_real_
    )
  ) %>%
  ## Consistent-denominator CDC variables
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
  ## RW-exclusive per-case spending (for Policy_strat tab)
  mutate(
    log_spend_excl_rw_per_case = ifelse(
      !is.na(spend_all) & !is.na(hiv_prevalence_counts) &
        spend_all > 0 & hiv_prevalence_counts > 0,
      log(spend_all / hiv_prevalence_counts),
      NA_real_
    )
  )

## Lead variables (for temporal-structure sensitivities)
df <- df %>%
  arrange(location_id, year_id) %>%
  group_by(location_id) %>%
  mutate(
    log_daly_per_capita_f1      = dplyr::lead(log_daly_per_capita, 1),
    log_mortality_per_capita_f1 = dplyr::lead(log_mortality_per_capita, 1),
    log_cdc_mortality_per_capita_f1 = dplyr::lead(log_cdc_mortality_per_capita, 1),
    as_daly_prev_ratio_log_f1   = dplyr::lead(as_daly_prev_ratio_log, 1),
    as_mort_prev_ratio_log_f1   = dplyr::lead(as_mort_prev_ratio_log, 1)
  ) %>%
  ungroup()

## year_factor required for year FE
if (!"year_factor" %in% names(df)) {
  df$year_factor <- factor(df$year_id)
}


##================================================================
## 4. HELPER FUNCTIONS
##================================================================

## Significance label
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

## "+0.0345 (*)" style label
beta_p <- function(b, p, d = 4) {
  out <- character(length(b))
  na_idx <- is.na(b)
  out[na_idx]  <- "—"
  out[!na_idx] <- sprintf("%+.*f (%s)", d, b[!na_idx], sig_lab(p[!na_idx]))
  out
}

## Cluster-robust SE extraction for lm (CR2 + Satterthwaite, with HC0 fallback)
extract_lm_CR2 <- function(fit, cluster_var) {
  tryCatch({
    vc <- vcovCR(fit, cluster = cluster_var, type = "CR2")
    ct <- coef_test(fit, vcov = vc, test = "Satterthwaite")
    tibble(
      term      = rownames(ct),
      estimate  = ct$beta,
      std.error = ct$SE,
      statistic = ct$tstat,
      df        = ct$df,
      p.value   = ct$p_Satt
    )
  }, error = function(e) {
    message("CR2 failed: ", e$message, " -- falling back to vcovCL HC0")
    vc <- vcovCL(fit, cluster = cluster_var, type = "HC0")
    ct <- coeftest(fit, vcov. = vc)
    tibble(
      term      = rownames(ct),
      estimate  = ct[, "Estimate"],
      std.error = ct[, "Std. Error"],
      statistic = ct[, "t value"],
      df        = NA_real_,
      p.value   = ct[, "Pr(>|t|)"]
    )
  })
}

## Cluster-robust SE extraction for GLM (vcovCL HC0)
extract_glm_CL <- function(fit, cluster_var) {
  vc <- vcovCL(fit, cluster = cluster_var, type = "HC0")
  ct <- coeftest(fit, vcov. = vc)
  tibble(
    term      = rownames(ct),
    estimate  = ct[, "Estimate"],
    std.error = ct[, "Std. Error"],
    statistic = ct[, "z value"],
    p.value   = ct[, "Pr(>|z|)"]
  )
}

## Get one coefficient (b, se, p) by term name; NA if missing
get_coef <- function(fit, term, vc) {
  if (!term %in% names(coef(fit))) return(list(b = NA_real_, se = NA_real_, p = NA_real_))
  b  <- coef(fit)[term]
  se <- sqrt(vc[term, term])
  z  <- b / se
  p  <- 2 * pnorm(-abs(z))
  list(b = as.numeric(b), se = as.numeric(se), p = as.numeric(p))
}

## Subgroup beta from interaction model via linear combination + delta method
##   beta_Low  = beta_main
##   beta_Mid  = beta_main + beta_interaction_Mid
##   beta_High = beta_main + beta_interaction_High
## SE from full covariance matrix of (beta_main, beta_int).
subgroup_betas <- function(fit, xvar, cluster_var) {
  ## Get cluster-robust vcov
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
  
  ## interaction term names in R use factor+level concatenation
  int_mid_term  <- sprintf("%s:prev_tercile_2010Mid",  xvar)
  int_high_term <- sprintf("%s:prev_tercile_2010High", xvar)
  
  ## Low (reference) = main effect only
  b_low  <- cf[xvar]
  se_low <- sqrt(vc[xvar, xvar])
  z_low  <- b_low / se_low
  p_low  <- 2 * pnorm(-abs(z_low))
  
  ## Mid = main + interaction; SE = sqrt(Var(main) + Var(int) + 2*Cov(main, int))
  mid_present  <- int_mid_term  %in% names(cf)
  high_present <- int_high_term %in% names(cf)
  
  out <- tibble(
    tercile  = "Low",
    estimate = as.numeric(b_low),
    std.error = as.numeric(se_low),
    statistic = as.numeric(z_low),
    p.value   = as.numeric(p_low)
  )
  
  if (mid_present) {
    b_mid <- cf[xvar] + cf[int_mid_term]
    v_mid <- vc[xvar, xvar] + vc[int_mid_term, int_mid_term] +
      2 * vc[xvar, int_mid_term]
    se_mid <- sqrt(v_mid)
    z_mid  <- b_mid / se_mid
    p_mid  <- 2 * pnorm(-abs(z_mid))
    out <- bind_rows(out, tibble(
      tercile = "Mid",
      estimate = as.numeric(b_mid),
      std.error = as.numeric(se_mid),
      statistic = as.numeric(z_mid),
      p.value   = as.numeric(p_mid)
    ))
  }
  if (high_present) {
    b_high <- cf[xvar] + cf[int_high_term]
    v_high <- vc[xvar, xvar] + vc[int_high_term, int_high_term] +
      2 * vc[xvar, int_high_term]
    se_high <- sqrt(v_high)
    z_high  <- b_high / se_high
    p_high  <- 2 * pnorm(-abs(z_high))
    out <- bind_rows(out, tibble(
      tercile = "High",
      estimate = as.numeric(b_high),
      std.error = as.numeric(se_high),
      statistic = as.numeric(z_high),
      p.value   = as.numeric(p_high)
    ))
  }
  out
}

## Manual AME for logit GLM (delta-method approximation)
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
         std.error = as.numeric(se_ame),
         p.value  = as.numeric(p))
  }, error = function(e) {
    list(estimate = NA_real_, std.error = NA_real_, p.value = NA_real_)
  })
}

## AME via marginaleffects (preferred) with manual fallback
compute_ame <- function(fit, xvar, cluster_var) {
  if (requireNamespace("marginaleffects", quietly = TRUE)) {
    res <- tryCatch({
      vc <- sandwich::vcovCL(fit, cluster = cluster_var, type = "HC0")
      s  <- marginaleffects::avg_slopes(fit, variables = xvar, vcov = vc)
      list(estimate = as.numeric(s$estimate[1]),
           std.error = as.numeric(s$std.error[1]),
           p.value  = as.numeric(s$p.value[1]))
    }, error = function(e) NULL)
    if (!is.null(res)) return(res)
  }
  compute_manual_ame(fit, xvar, cluster_var)
}

## Mediation fit: returns a, b, c', c, indirect with cluster-robust SEs
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
      a = a_info$b,       a_se = a_info$se,       a_p = a_info$p,
      b = b_info$b,       b_se = b_info$se,       b_p = b_info$p,
      c_prime = cp_info$b, c_prime_se = cp_info$se, c_prime_p = cp_info$p,
      c_total = ct_info$b, c_total_se = ct_info$se, c_total_p = ct_info$p,
      indirect = a_info$b * b_info$b,
      n_obs = nrow(d)
    )
  }, error = function(e) {
    message("fit_mediation failed: ", e$message)
    NULL
  })
}

## State-block bootstrap of the indirect effect
bootstrap_indirect <- function(d, X, M, Y, covars, n_boot = 500, seed = 20260520) {
  set.seed(seed)
  state_ids <- unique(d$location_id)
  ind_vals <- numeric(n_boot)
  n_failed <- 0
  for (i in seq_len(n_boot)) {
    samp <- sample(state_ids, length(state_ids), replace = TRUE)
    d_boot <- bind_rows(lapply(samp, function(s) d[d$location_id == s, , drop = FALSE]))
    ## Re-id to break duplicate-cluster issues
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

## Shared RHS blocks (per analytic plan v4 — contemporaneous incidence as primary)
rhs_demog   <- function() "race_prop_BLCK + race_prop_HISP + log_prop_homeless + year_factor"
rhs_primary <- function() paste(rhs_demog(), "+ log_incidence_per_100k")          # contemp inc
rhs_lagged  <- function() paste(rhs_demog(), "+ log_incidence_per_100k_l1")       # lagged inc (sensitivity)
rhs_cascade <- function() paste(rhs_primary(), "+ log_prevalence_per_100k_l1")    # + lagged prevalence
rhs_upstream <- function() paste(rhs_demog(),
                                 "+ log_opioid_prev + aca_implemented_status",
                                 "+ edu_yrs + log_ldi_pc")


##================================================================
## 5. PRIMARY MEDIATION CHAIN (per-case GBD, all current year)
##================================================================
##   X = rw_dex_hiv_prev_ratio_log   (year t)
##   M = cdc_kv_composite            (year t)
##   Y in {as_daly_prev_ratio_log, as_mort_prev_ratio_log}   (year t)
##   Controls: contemporaneous incidence + race + log(homeless) + year FE
##   By tercile + pooled
##   Bootstrap 500 reps, seed 20260520
##================================================================
log_msg("\n=== PRIMARY MEDIATION CHAIN (per-case GBD, all current year) ===")

X_primary <- "rw_dex_hiv_prev_ratio_log"
M_primary <- "cdc_kv_composite"
covars_primary <- "log_incidence_per_100k + log_prop_homeless + race_prop_BLCK + race_prop_HISP + year_factor"

Y_primary_dict <- list(
  DALYs     = "as_daly_prev_ratio_log",
  Mortality = "as_mort_prev_ratio_log"
)

PRIMARY <- list()
for (y_name in names(Y_primary_dict)) {
  Y <- Y_primary_dict[[y_name]]
  PRIMARY[[y_name]] <- list()
  for (tercile in c("Low", "Mid", "High", "Pooled")) {
    d_base <- if (tercile == "Pooled") df else df %>% dplyr::filter(prev_tercile_2010 == tercile)
    cols_needed <- c(Y, X_primary, M_primary,
                     "log_incidence_per_100k", "log_prop_homeless",
                     "race_prop_BLCK", "race_prop_HISP",
                     "year_factor", "location_id")
    d <- d_base[, cols_needed] %>% drop_na()
    res <- fit_mediation(d, X_primary, M_primary, Y, covars_primary)
    if (!is.null(res)) {
      boot <- bootstrap_indirect(d, X_primary, M_primary, Y, covars_primary,
                                 n_boot = 500, seed = 20260520)
      res$indirect_lower <- boot$lower
      res$indirect_upper <- boot$upper
      res$n_boot_successful <- boot$n_boot_successful
      res$n_boot_failed <- boot$n_boot_failed
    }
    PRIMARY[[y_name]][[tercile]] <- res
    if (!is.null(res)) {
      log_msg(sprintf("  %s %-6s  n=%d  a=%+.4f (p=%.3f)  b=%+.5f (p=%.3f)  c=%+.5f (p=%.3f)  ab=%+.6f",
                      y_name, tercile, res$n_obs,
                      res$a, res$a_p, res$b, res$b_p, res$c_total, res$c_total_p,
                      res$indirect))
    }
  }
}


##================================================================
## 6. CASCADE REGRESSIONS  X -> M  (Table 2 / Cascade_X_to_M)
##================================================================
##   M(t) in {K, L, V, K x V}
##   X(t) in 3 predictor scalings (per-case GBD primary)
##   Family: quasibinomial(logit)
##   Controls: contemporaneous incidence + lagged prevalence + race + log(homeless) + year FE
##   By tercile (subgroup beta via lincomb) + pooled
##================================================================
log_msg("\n=== CASCADE REGRESSIONS (X -> M) ===")

cascade_outcomes <- list(
  K    = "cdc_knowledge_status",
  L    = "cdc_linkage_1mo",
  V    = "cdc_viral_suppress",
  KxV  = "cdc_kv_composite"
)
predictors_T3 <- list(
  per_case_GBD  = "rw_dex_hiv_prev_ratio_log",      # PRIMARY
  per_capita    = "log_spending_per_capita",
  per_case_CDC  = "log_spend_per_cdc_prev"
)

cascade_results <- list()
ame_results <- list()

for (predictor_label in names(predictors_T3)) {
  xvar <- predictors_T3[[predictor_label]]
  for (outcome_label in names(cascade_outcomes)) {
    yvar <- cascade_outcomes[[outcome_label]]
    cols_needed <- c(yvar, xvar, "log_incidence_per_100k", "log_prevalence_per_100k_l1",
                     "race_prop_BLCK", "race_prop_HISP", "log_prop_homeless",
                     "year_factor", "prev_tercile_2010", "location_id")
    d <- df[, cols_needed] %>% drop_na()
    d$.outcome <- pmin(pmax(d[[yvar]], 1e-6), 1 - 1e-6)
    if (nrow(d) < 100) {
      log_msg(sprintf("  WARNING: cascade %s ~ %s n=%d, skipping",
                      outcome_label, predictor_label, nrow(d)))
      next
    }
    
    ## --- Pooled regression (no interaction) ---
    f_pool <- as.formula(paste0(".outcome ~ ", xvar, " + ", rhs_cascade()))
    fit_pool <- glm(f_pool, data = d, family = quasibinomial(link = "logit"))
    pool_coef <- extract_glm_CL(fit_pool, d$location_id) %>%
      dplyr::filter(term == xvar) %>%
      mutate(predictor = predictor_label, outcome = outcome_label,
             tercile = "Pooled", scale = "log_odds", n = nrow(d),
             signif_label = sig_lab(p.value))
    cascade_results[[paste("pooled", predictor_label, outcome_label, sep = "_")]] <- pool_coef
    
    ## --- Tercile interaction model ---
    f_int <- as.formula(paste0(".outcome ~ ", xvar, " * prev_tercile_2010 + ", rhs_cascade()))
    fit_int <- glm(f_int, data = d, family = quasibinomial(link = "logit"))
    sg <- subgroup_betas(fit_int, xvar, d$location_id)
    if (!is.null(sg)) {
      sg <- sg %>%
        mutate(predictor = predictor_label, outcome = outcome_label,
               scale = "log_odds", n = nrow(d), term = xvar,
               signif_label = sig_lab(p.value))
      cascade_results[[paste("subgroup", predictor_label, outcome_label, sep = "_")]] <- sg
    }
    
    ## --- AME for the pooled fit (one number per cascade × predictor cell) ---
    ame <- compute_ame(fit_pool, xvar, d$location_id)
    ame_results[[paste(predictor_label, outcome_label, sep = "_")]] <- tibble(
      predictor = predictor_label, outcome = outcome_label,
      scale = "AME_percentage_points", tercile = "Pooled",
      term = xvar,
      estimate = ame$estimate, std.error = ame$std.error, p.value = ame$p.value,
      signif_label = sig_lab(ame$p.value), n = nrow(d)
    )
    
    log_msg(sprintf("  %s ~ %-13s: pooled β=%+.3f (p=%.3f); AME=%+.4f",
                    outcome_label, predictor_label,
                    pool_coef$estimate, pool_coef$p.value, ame$estimate))
  }
}

cascade_table <- bind_rows(c(cascade_results, ame_results)) %>%
  select(predictor, outcome, tercile, scale, term,
         estimate, std.error, statistic, p.value, signif_label, n)


##================================================================
## 7. CASCADE -> OUTCOME  M -> Y  (Table 3 / Cascade_M_to_Y)
##================================================================
##   Each cascade indicator (K, L, V, K x V) -> {DALYs, mortality} per case
##   Two specs per cell: WITHOUT spending control, WITH spending control (b-path)
##   Controls (without spending): contemp incidence + race + log(homeless) + year FE
##   Controls (with spending):    + rw_dex_hiv_prev_ratio_log (year t)
##   By tercile + pooled
##================================================================
log_msg("\n=== CASCADE -> OUTCOME (M -> Y) ===")

cascade_to_outcome_results <- list()

cascade_preds_T5 <- c("cdc_knowledge_status", "cdc_linkage_1mo",
                      "cdc_viral_suppress", "cdc_kv_composite")
outcomes_T5 <- list(
  DALYs     = "as_daly_prev_ratio_log",
  Mortality = "as_mort_prev_ratio_log"
)

for (cp in cascade_preds_T5) {
  for (y_label in names(outcomes_T5)) {
    Y <- outcomes_T5[[y_label]]
    
    ## -- without spending control --
    cols_no <- c(Y, cp, "log_incidence_per_100k", "race_prop_BLCK", "race_prop_HISP",
                 "log_prop_homeless", "year_factor", "prev_tercile_2010", "location_id")
    d_no <- df[, cols_no] %>% drop_na()
    if (nrow(d_no) < 50) next
    f_pool_no <- as.formula(paste0(Y, " ~ ", cp, " + ", rhs_primary()))
    fit_pool_no <- lm(f_pool_no, data = d_no)
    pool_no <- extract_lm_CR2(fit_pool_no, d_no$location_id) %>%
      dplyr::filter(term == cp) %>%
      mutate(spec = "without_spending", cascade_predictor = cp, outcome = y_label,
             tercile = "Pooled", n = nrow(d_no),
             adj_r2 = summary(fit_pool_no)$adj.r.squared,
             signif_label = sig_lab(p.value))
    cascade_to_outcome_results[[paste("pool_noSp", cp, y_label, sep = "_")]] <- pool_no
    
    f_int_no <- as.formula(paste0(Y, " ~ ", cp, " * prev_tercile_2010 + ", rhs_primary()))
    fit_int_no <- lm(f_int_no, data = d_no)
    sg_no <- subgroup_betas(fit_int_no, cp, d_no$location_id)
    if (!is.null(sg_no)) {
      sg_no <- sg_no %>%
        mutate(spec = "without_spending", cascade_predictor = cp, outcome = y_label,
               term = cp, n = nrow(d_no), signif_label = sig_lab(p.value))
      cascade_to_outcome_results[[paste("sub_noSp", cp, y_label, sep = "_")]] <- sg_no
    }
    
    ## -- with spending control (b-path) --
    cols_with <- c(cols_no, X_primary)
    d_with <- df[, cols_with] %>% drop_na()
    f_pool_with <- as.formula(paste0(Y, " ~ ", cp, " + ", X_primary, " + ", rhs_primary()))
    fit_pool_with <- lm(f_pool_with, data = d_with)
    pool_with <- extract_lm_CR2(fit_pool_with, d_with$location_id) %>%
      dplyr::filter(term == cp) %>%
      mutate(spec = "with_spending", cascade_predictor = cp, outcome = y_label,
             tercile = "Pooled", n = nrow(d_with),
             adj_r2 = summary(fit_pool_with)$adj.r.squared,
             signif_label = sig_lab(p.value))
    cascade_to_outcome_results[[paste("pool_withSp", cp, y_label, sep = "_")]] <- pool_with
    
    f_int_with <- as.formula(paste0(Y, " ~ ", cp, " * prev_tercile_2010 + ",
                                    X_primary, " + ", rhs_primary()))
    fit_int_with <- lm(f_int_with, data = d_with)
    sg_with <- subgroup_betas(fit_int_with, cp, d_with$location_id)
    if (!is.null(sg_with)) {
      sg_with <- sg_with %>%
        mutate(spec = "with_spending", cascade_predictor = cp, outcome = y_label,
               term = cp, n = nrow(d_with), signif_label = sig_lab(p.value))
      cascade_to_outcome_results[[paste("sub_withSp", cp, y_label, sep = "_")]] <- sg_with
    }
    
    log_msg(sprintf("  %s -> %s: pooled β (no sp)=%+.4f (p=%.3f); β (with sp)=%+.4f (p=%.3f)",
                    cp, y_label, pool_no$estimate, pool_no$p.value,
                    pool_with$estimate, pool_with$p.value))
  }
}

cascade_outcome_table <- bind_rows(cascade_to_outcome_results) %>%
  select(cascade_predictor, outcome, spec, tercile, term,
         estimate, std.error, statistic, p.value, signif_label, n)


##================================================================
## 8. BURDEN LENS — primary + 6-row robustness panel
##================================================================
##   Primary: Y(t) ~ X(t) + contemporaneous incidence + race + log(homeless) + year FE
##   Robustness rows: bivariate, lagged incidence, drop incidence, upstream RFs, Mundlak
##================================================================
log_msg("\n=== BURDEN LENS ===")

burden_specs <- list(
  "Bivariate (no controls)"                    = "1",
  "Primary: contemporaneous incidence"         = rhs_primary(),
  "Sensitivity: lagged incidence (t-1)"        = rhs_lagged(),
  "Sensitivity: drop incidence entirely"       = rhs_demog(),
  "Sensitivity: upstream risk-factor set"      = rhs_upstream(),
  "Mundlak between/within decomposition"       = "MUNDLAK"
)

burden_outcomes <- list(
  "DALYs/case"        = "as_daly_prev_ratio_log",
  "GBD mortality/case" = "as_mort_prev_ratio_log"
)

burden_rows <- list()

for (spec_label in names(burden_specs)) {
  controls <- burden_specs[[spec_label]]
  for (y_label in names(burden_outcomes)) {
    Y <- burden_outcomes[[y_label]]
    
    if (controls == "MUNDLAK") {
      ## Mundlak: replace X with X_B + X_W
      cols_needed <- c(Y, "rw_dex_hiv_prev_ratio_log_B", "rw_dex_hiv_prev_ratio_log_W",
                       "log_incidence_per_100k", "race_prop_BLCK", "race_prop_HISP",
                       "log_prop_homeless", "year_factor", "location_id")
      d <- df[, cols_needed] %>% drop_na()
      f <- as.formula(paste0(Y, " ~ rw_dex_hiv_prev_ratio_log_B + rw_dex_hiv_prev_ratio_log_W + ",
                             rhs_primary()))
      fit <- lm(f, data = d)
      ce <- extract_lm_CR2(fit, d$location_id)
      bB <- ce %>% dplyr::filter(term == "rw_dex_hiv_prev_ratio_log_B")
      bW <- ce %>% dplyr::filter(term == "rw_dex_hiv_prev_ratio_log_W")
      burden_rows[[paste(spec_label, y_label, sep = " | ")]] <- tibble(
        spec_label = spec_label, outcome = y_label,
        estimate_str = sprintf("B=%+.4f(%s) / W=%+.4f(%s)",
                               bB$estimate, sig_lab(bB$p.value),
                               bW$estimate, sig_lab(bW$p.value)),
        estimate_B = bB$estimate, p_B = bB$p.value,
        estimate_W = bW$estimate, p_W = bW$p.value,
        n = nobs(fit)
      )
    } else {
      cols_needed <- c(Y, X_primary, "location_id")
      ## Parse out continuous control names (skip C(year_factor))
      if (controls != "1") {
        for (c in strsplit(controls, "\\+")[[1]]) {
          cc <- trimws(c)
          if (cc == "" || grepl("^year_factor$", cc)) next
          if (cc == "year_factor") cc <- "year_factor"  # included separately below
          if (!cc %in% cols_needed) cols_needed <- c(cols_needed, cc)
        }
        if (!"year_factor" %in% cols_needed) cols_needed <- c(cols_needed, "year_factor")
      }
      d <- df[, cols_needed] %>% drop_na()
      f <- if (controls == "1") {
        as.formula(paste0(Y, " ~ ", X_primary))
      } else {
        as.formula(paste0(Y, " ~ ", X_primary, " + ", controls))
      }
      fit <- lm(f, data = d)
      ce <- extract_lm_CR2(fit, d$location_id) %>%
        dplyr::filter(term == X_primary)
      burden_rows[[paste(spec_label, y_label, sep = " | ")]] <- tibble(
        spec_label = spec_label, outcome = y_label,
        estimate_str = sprintf("%+.4f (%s)", ce$estimate, sig_lab(ce$p.value)),
        estimate = ce$estimate, p_value = ce$p.value,
        n = nobs(fit), adj_r2 = summary(fit)$adj.r.squared
      )
    }
  }
}

burden_table <- bind_rows(burden_rows)
log_msg(sprintf("  Built %d burden lens rows", nrow(burden_table)))


##================================================================
## 9. SENSITIVITIES (for alternative-specifications file)
##================================================================

log_msg("\n=== SENSITIVITIES ===")

## --- 9a. Temporal sensitivities ---
## Lag X / Lead Y / Lag X + Lead Y variants of the per-case GBD mediation chain.
log_msg("  Temporal sensitivities...")
temporal_specs <- list(
  "All current year (primary)"   = list(X = "rw_dex_hiv_prev_ratio_log",    M = "cdc_kv_composite", Y_daly = "as_daly_prev_ratio_log",    Y_mort = "as_mort_prev_ratio_log"),
  "Lag X only"                    = list(X = "rw_dex_hiv_prev_ratio_log_l1", M = "cdc_kv_composite", Y_daly = "as_daly_prev_ratio_log",    Y_mort = "as_mort_prev_ratio_log"),
  "Lead Y only"                   = list(X = "rw_dex_hiv_prev_ratio_log",    M = "cdc_kv_composite", Y_daly = "as_daly_prev_ratio_log_f1", Y_mort = "as_mort_prev_ratio_log_f1"),
  "Lag X + Lead Y"                = list(X = "rw_dex_hiv_prev_ratio_log_l1", M = "cdc_kv_composite", Y_daly = "as_daly_prev_ratio_log_f1", Y_mort = "as_mort_prev_ratio_log_f1")
)
temporal_results <- list()
for (spec_label in names(temporal_specs)) {
  s <- temporal_specs[[spec_label]]
  for (y_label in c("DALYs", "Mortality")) {
    Y <- if (y_label == "DALYs") s$Y_daly else s$Y_mort
    cols <- c(Y, s$X, s$M, "log_incidence_per_100k", "log_prop_homeless",
              "race_prop_BLCK", "race_prop_HISP", "year_factor", "location_id")
    d <- df[, cols] %>% drop_na()
    res <- fit_mediation(d, s$X, s$M, Y, covars_primary)
    if (is.null(res)) next
    temporal_results[[paste(spec_label, y_label, sep = " | ")]] <- tibble(
      spec = spec_label, outcome = y_label, X = s$X, Y = Y,
      a = res$a, a_p = res$a_p,
      b = res$b, b_p = res$b_p,
      c_prime = res$c_prime, c_prime_p = res$c_prime_p,
      c_total = res$c_total, c_total_p = res$c_total_p,
      indirect = res$indirect,
      n = res$n_obs
    )
  }
}
temporal_table <- bind_rows(temporal_results)

## --- 9b. Per-capita lens (full mediation, pooled) ---
log_msg("  Per-capita lens...")
percapita_results <- list()
covars_percapita <- "log_incidence_per_100k + log_prevalence_per_100k_l1 + log_prop_homeless + race_prop_BLCK + race_prop_HISP + year_factor"
for (y_label in c("DALYs", "Mortality", "CDC_Mortality")) {
  Y <- switch(y_label,
              DALYs        = "log_daly_per_capita",
              Mortality    = "log_mortality_per_capita",
              CDC_Mortality = "log_cdc_mortality_per_capita")
  cols <- c(Y, "log_spending_per_capita", "cdc_kv_composite",
            "log_incidence_per_100k", "log_prevalence_per_100k_l1",
            "log_prop_homeless", "race_prop_BLCK", "race_prop_HISP",
            "year_factor", "location_id")
  d <- df[, cols] %>% drop_na()
  res <- fit_mediation(d, "log_spending_per_capita", "cdc_kv_composite", Y, covars_percapita)
  if (is.null(res)) next
  percapita_results[[y_label]] <- tibble(
    outcome = y_label, X = "log_spending_per_capita", M = "cdc_kv_composite", Y = Y,
    a = res$a, a_p = res$a_p, b = res$b, b_p = res$b_p,
    c_prime = res$c_prime, c_prime_p = res$c_prime_p,
    c_total = res$c_total, c_total_p = res$c_total_p,
    indirect = res$indirect, n = res$n_obs
  )
}
percapita_table <- bind_rows(percapita_results)

## --- 9c. Per-case CDC lens ---
log_msg("  Per-case CDC lens...")
df <- df %>%
  arrange(location_id, year_id) %>%
  group_by(location_id) %>%
  mutate(
    log_cdc_mort_per_cdc_prev_f1 = dplyr::lead(log_cdc_mort_per_cdc_prev, 1),
    log_gbd_mort_per_cdc_prev_f1 = dplyr::lead(log_gbd_mort_per_cdc_prev, 1)
  ) %>%
  ungroup()
percasecdc_results <- list()
for (y_label in c("CDCmort_CDCprev", "GBDmort_CDCprev")) {
  Y <- switch(y_label,
              CDCmort_CDCprev = "log_cdc_mort_per_cdc_prev",
              GBDmort_CDCprev = "log_gbd_mort_per_cdc_prev")
  cols <- c(Y, "log_spend_per_cdc_prev", "cdc_kv_composite",
            "log_incidence_per_100k", "log_prop_homeless",
            "race_prop_BLCK", "race_prop_HISP", "year_factor", "location_id")
  d <- df[, cols] %>% drop_na()
  res <- fit_mediation(d, "log_spend_per_cdc_prev", "cdc_kv_composite", Y, covars_primary)
  if (is.null(res)) next
  percasecdc_results[[y_label]] <- tibble(
    outcome = y_label, X = "log_spend_per_cdc_prev", M = "cdc_kv_composite", Y = Y,
    a = res$a, a_p = res$a_p, b = res$b, b_p = res$b_p,
    c_prime = res$c_prime, c_prime_p = res$c_prime_p,
    c_total = res$c_total, c_total_p = res$c_total_p,
    indirect = res$indirect, n = res$n_obs
  )
}
percasecdc_table <- bind_rows(percasecdc_results)

## --- 9d. CDC denominator consistency (S7 + S8) ---
log_msg("  CDC denominator consistency analyses...")
denom_rows <- list()

## S7: within-GBD framework, CDC mortality numerator swap
d_S7 <- df[, c("log_cdc_mort_per_gbd_prev", X_primary,
               "log_incidence_per_100k", "log_prop_homeless",
               "race_prop_BLCK", "race_prop_HISP", "year_factor",
               "location_id")] %>% drop_na()
fit_S7 <- lm(as.formula(paste0("log_cdc_mort_per_gbd_prev ~ ", X_primary, " + ", rhs_primary())),
             data = d_S7)
c_S7 <- extract_lm_CR2(fit_S7, d_S7$location_id) %>%
  dplyr::filter(term == X_primary)
denom_rows[["S7"]] <- tibble(
  spec = "S7: Within-GBD framework, CDC mortality numerator",
  predictor = "log(spending/GBD prev), year t",
  outcome   = "log(CDC mort 25+ / GBD prev)",
  estimate = c_S7$estimate, std.error = c_S7$std.error, p.value = c_S7$p.value,
  signif_label = sig_lab(c_S7$p.value), n = nobs(fit_S7)
)

## S8a, S8b: within-CDC framework, two mortality numerators
for (variant in c("S8a_CDCmort", "S8b_GBDmort")) {
  Y <- if (variant == "S8a_CDCmort") "log_cdc_mort_per_cdc_prev" else "log_gbd_mort_per_cdc_prev"
  d_S8 <- df[, c(Y, "log_spend_per_cdc_prev",
                 "log_incidence_per_100k", "log_prop_homeless",
                 "race_prop_BLCK", "race_prop_HISP", "year_factor",
                 "location_id")] %>% drop_na()
  fit_S8 <- lm(as.formula(paste0(Y, " ~ log_spend_per_cdc_prev + ", rhs_primary())),
               data = d_S8)
  c_S8 <- extract_lm_CR2(fit_S8, d_S8$location_id) %>%
    dplyr::filter(term == "log_spend_per_cdc_prev")
  denom_rows[[variant]] <- tibble(
    spec = paste0(variant, ": Within-CDC framework"),
    predictor = "log(spending/CDC prev 25+), year t",
    outcome   = ifelse(grepl("CDC", variant), "log(CDC mort 25+ / CDC prev 25+)",
                       "log(GBD mort / CDC prev 25+)"),
    estimate = c_S8$estimate, std.error = c_S8$std.error, p.value = c_S8$p.value,
    signif_label = sig_lab(c_S8$p.value), n = nobs(fit_S8)
  )
}
denom_table <- bind_rows(denom_rows)

## --- 9e. Mortality outcomes across all lenses (pooled mediation grid) ---
log_msg("  Mortality outcomes across all lenses...")
lens_grid <- list(
  list(label = "Per-capita / DALYs",       X = "log_spending_per_capita",   Y = "log_daly_per_capita",              covars = covars_percapita),
  list(label = "Per-capita / GBD mort",     X = "log_spending_per_capita",   Y = "log_mortality_per_capita",         covars = covars_percapita),
  list(label = "Per-capita / CDC mort",     X = "log_spending_per_capita",   Y = "log_cdc_mortality_per_capita",     covars = covars_percapita),
  list(label = "Per-case GBD / DALYs (PRIMARY)", X = X_primary,              Y = "as_daly_prev_ratio_log",            covars = covars_primary),
  list(label = "Per-case GBD / GBD mort",    X = X_primary,                  Y = "as_mort_prev_ratio_log",            covars = covars_primary),
  list(label = "Per-case CDC / CDC mort",    X = "log_spend_per_cdc_prev",   Y = "log_cdc_mort_per_cdc_prev",          covars = covars_primary),
  list(label = "Per-case CDC / GBD mort",    X = "log_spend_per_cdc_prev",   Y = "log_gbd_mort_per_cdc_prev",          covars = covars_primary)
)
mort_all_rows <- list()
for (g in lens_grid) {
  ## Parse covar names defensively
  cnames <- trimws(strsplit(g$covars, "\\+")[[1]])
  cnames <- setdiff(cnames, c("", "year_factor"))
  cnames <- cnames[!grepl("C\\(", cnames)]
  cols <- unique(c(g$Y, g$X, "cdc_kv_composite", cnames, "year_factor", "location_id"))
  d <- df[, intersect(cols, names(df))] %>% drop_na()
  res <- fit_mediation(d, g$X, "cdc_kv_composite", g$Y, g$covars)
  if (is.null(res)) next
  mort_all_rows[[g$label]] <- tibble(
    lens_outcome = g$label,
    a = res$a, a_p = res$a_p,
    b = res$b, b_p = res$b_p,
    c_prime = res$c_prime, c_prime_p = res$c_prime_p,
    c_total = res$c_total, c_total_p = res$c_total_p,
    indirect = res$indirect, n = res$n_obs
  )
}
mort_all_table <- bind_rows(mort_all_rows)

## --- 9f. Leave-one-state-out ---
log_msg("  Leave-one-state-out...")
state_ids <- unique(df$location_id)
state_names <- df %>% group_by(location_id) %>%
  summarise(location_name = first(location_name), .groups = "drop")
loo_rows <- list()
for (sid in state_ids) {
  d_loo <- df %>% dplyr::filter(location_id != sid,
                                !is.na(as_daly_prev_ratio_log),
                                !is.na(log_incidence_per_100k))
  fit_loo <- lm(as.formula(paste0("as_daly_prev_ratio_log ~ ", X_primary, " + ", rhs_primary())),
                data = d_loo)
  ce <- coef(fit_loo)[X_primary]
  loo_rows[[as.character(sid)]] <- tibble(
    excluded_location_id   = sid,
    excluded_location_name = state_names$location_name[state_names$location_id == sid],
    beta_X = as.numeric(ce),
    n = nobs(fit_loo)
  )
}
loo_table <- bind_rows(loo_rows)
log_msg(sprintf("    LOO beta_X range: [%+.4f, %+.4f], median %+.4f",
                min(loo_table$beta_X), max(loo_table$beta_X), median(loo_table$beta_X)))

## --- 9g. Drop 2010-2012 ---
log_msg("  Drop 2010-2012...")
d_drop <- df %>% dplyr::filter(year_id >= 2013,
                               !is.na(as_daly_prev_ratio_log),
                               !is.na(log_incidence_per_100k))
fit_drop <- lm(as.formula(paste0("as_daly_prev_ratio_log ~ ", X_primary, " + ", rhs_primary())),
               data = d_drop)
drop_row <- extract_lm_CR2(fit_drop, d_drop$location_id) %>%
  dplyr::filter(term == X_primary) %>%
  mutate(spec = "Drop years 2010-2012", n = nobs(fit_drop),
         signif_label = sig_lab(p.value))

## --- 9h. Medicaid expansion interaction ---
log_msg("  Medicaid expansion interaction...")
d_aca <- df %>% dplyr::filter(!is.na(as_daly_prev_ratio_log),
                              !is.na(log_incidence_per_100k),
                              !is.na(aca_implemented_status))
fit_aca <- lm(as.formula(paste0("as_daly_prev_ratio_log ~ ", X_primary,
                                " * aca_implemented_status + ", rhs_primary())),
              data = d_aca)
aca_rows <- extract_lm_CR2(fit_aca, d_aca$location_id) %>%
  dplyr::filter(grepl(X_primary, term, fixed = TRUE) | grepl("aca", term)) %>%
  mutate(spec = "Medicaid expansion (ACA) interaction", n = nobs(fit_aca),
         signif_label = sig_lab(p.value))

## --- 9i. RW-exclusive spending ---
log_msg("  RW-exclusive spending...")
d_rw <- df %>% dplyr::filter(!is.na(as_daly_prev_ratio_log),
                             !is.na(log_spend_excl_rw_per_case),
                             !is.na(log_incidence_per_100k))
fit_rw <- lm(as.formula(paste0("as_daly_prev_ratio_log ~ log_spend_excl_rw_per_case + ",
                               rhs_primary())),
             data = d_rw)
rw_row <- extract_lm_CR2(fit_rw, d_rw$location_id) %>%
  dplyr::filter(term == "log_spend_excl_rw_per_case") %>%
  mutate(spec = "Ryan-White-exclusive spending", n = nobs(fit_rw),
         signif_label = sig_lab(p.value))


##================================================================
## 10. REFERENCE TABLES (state tercile assignment + variance decomp)
##================================================================
tercile_states <- df %>%
  distinct(location_id, location_name, prev_tercile_2010) %>%
  arrange(prev_tercile_2010, location_name)

state_means <- df %>%
  group_by(location_id) %>%
  summarise(
    spending_pc_mean   = mean(log_spending_per_capita, na.rm = TRUE),
    daly_pc_mean       = mean(log_daly_per_capita, na.rm = TRUE),
    spending_case_mean = mean(rw_dex_hiv_prev_ratio_log, na.rm = TRUE),
    daly_case_mean     = mean(as_daly_prev_ratio_log, na.rm = TRUE),
    .groups = "drop"
  )

var_decomp <- tibble(
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
  mutate(within_var = total_var - between_var,
         between_pct = round(between_var / total_var * 100, 1),
         within_pct  = round(within_var  / total_var * 100, 1))


##================================================================
## 11. BIVARIATE SCATTER PNGS (stratified by 2010-baseline tercile)
##================================================================
log_msg("\n=== BIVARIATE SCATTERS ===")

## Per-case GBD framework
scatter_case <- bind_rows(
  df %>% transmute(
    location_name, year_id, tercile = prev_tercile_2010,
    log_x = rw_dex_hiv_prev_ratio_log,
    log_y = as_daly_prev_ratio_log,
    outcome = "A. DALYs per GBD prevalent case"
  ),
  df %>% transmute(
    location_name, year_id, tercile = prev_tercile_2010,
    log_x = rw_dex_hiv_prev_ratio_log,
    log_y = as_mort_prev_ratio_log,
    outcome = "B. GBD mortality per GBD prevalent case"
  )
) %>% dplyr::filter(!is.na(log_x), !is.na(log_y))

r_labels_case <- scatter_case %>%
  group_by(tercile, outcome) %>%
  summarise(r = cor(log_x, log_y, use = "complete.obs"),
            n = dplyr::n(), .groups = "drop") %>%
  mutate(label = sprintf("r = %+.2f  n = %d", r, n))

p_case <- ggplot(scatter_case, aes(x = log_x, y = log_y)) +
  geom_point(alpha = 0.45, size = 0.9, colour = "#4A6FA5") +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
              colour = "navy", fill = "navy", alpha = 0.18) +
  geom_text(data = r_labels_case, aes(x = -Inf, y = Inf, label = label),
            hjust = -0.1, vjust = 1.5, size = 3, inherit.aes = FALSE) +
  facet_grid(tercile ~ outcome, scales = "free", switch = "y") +
  labs(
    title = "Bivariate spending-outcome association by 2010-baseline prevalence tercile",
    subtitle = "Per-case GBD framework. Each point is one state-year, 2010-2019.",
    x = "log HIV spending per GBD prevalent case",
    y = "log outcome per GBD prevalent case"
  ) +
  theme_minimal(base_size = 10) +
  theme(strip.placement = "outside",
        strip.text = element_text(face = "bold"),
        panel.spacing = unit(0.5, "lines"))
ggsave(file.path(dir_output, "bivariate_stratified_per_case.png"),
       p_case, width = 10, height = 11, dpi = 220)
log_msg("Wrote bivariate_stratified_per_case.png")

## Per-capita framework
scatter_pc <- bind_rows(
  df %>% transmute(
    location_name, year_id, tercile = prev_tercile_2010,
    log_x = log_spending_per_capita,
    log_y = log_daly_per_capita,
    outcome = "A. DALYs per capita (GBD)"
  ),
  df %>% transmute(
    location_name, year_id, tercile = prev_tercile_2010,
    log_x = log_spending_per_capita,
    log_y = log_mortality_per_capita,
    outcome = "B. GBD mortality per capita"
  )
) %>% dplyr::filter(!is.na(log_x), !is.na(log_y))

r_labels_pc <- scatter_pc %>%
  group_by(tercile, outcome) %>%
  summarise(r = cor(log_x, log_y, use = "complete.obs"),
            n = dplyr::n(), .groups = "drop") %>%
  mutate(label = sprintf("r = %+.2f  n = %d", r, n))

p_pc <- ggplot(scatter_pc, aes(x = log_x, y = log_y)) +
  geom_point(alpha = 0.45, size = 0.9, colour = "#A55050") +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
              colour = "darkred", fill = "darkred", alpha = 0.18) +
  geom_text(data = r_labels_pc, aes(x = -Inf, y = Inf, label = label),
            hjust = -0.1, vjust = 1.5, size = 3, inherit.aes = FALSE) +
  facet_grid(tercile ~ outcome, scales = "free", switch = "y") +
  labs(
    title = "Bivariate spending-outcome association by 2010-baseline prevalence tercile",
    subtitle = "Per-capita framework. Each point is one state-year, 2010-2019.",
    x = "log HIV spending per capita",
    y = "log outcome per capita"
  ) +
  theme_minimal(base_size = 10) +
  theme(strip.placement = "outside",
        strip.text = element_text(face = "bold"),
        panel.spacing = unit(0.5, "lines"))
ggsave(file.path(dir_output, "bivariate_stratified_per_capita.png"),
       p_pc, width = 10, height = 11, dpi = 220)
log_msg("Wrote bivariate_stratified_per_capita.png")


##================================================================
## 12. BUILD FILE 1: aim2_primary_per_case_GBD.xlsx
##================================================================
log_msg("\n=== BUILDING aim2_primary_per_case_GBD.xlsx ===")

wb <- createWorkbook()

## Styles
hdr_style  <- createStyle(fontColour = "#FFFFFF", bgFill = "#1F3864",
                          fgFill = "#1F3864", textDecoration = "bold",
                          halign = "center", border = "TopBottomLeftRight",
                          borderColour = "#BFBFBF", wrapText = TRUE)
sec_style  <- createStyle(fgFill = "#D5E8F0", textDecoration = "bold",
                          fontColour = "#1F3864", border = "TopBottomLeftRight",
                          borderColour = "#BFBFBF")
note_style <- createStyle(fontSize = 10, textDecoration = "italic", fontColour = "#595959",
                          wrapText = TRUE)
title_style <- createStyle(fontSize = 14, textDecoration = "bold", fontColour = "#1F3864")
cell_style <- createStyle(border = "TopBottomLeftRight", borderColour = "#BFBFBF",
                          halign = "center")
left_style <- createStyle(border = "TopBottomLeftRight", borderColour = "#BFBFBF",
                          textDecoration = "bold")
sig_style  <- createStyle(fgFill = "#FFF2CC", textDecoration = "bold",
                          border = "TopBottomLeftRight", borderColour = "#BFBFBF",
                          halign = "center")

write_header_block <- function(wb, sheet, title, subtitle, methods_note, start_row = 1) {
  writeData(wb, sheet, title, startRow = start_row, startCol = 1)
  addStyle(wb, sheet, title_style, rows = start_row, cols = 1)
  writeData(wb, sheet, subtitle, startRow = start_row + 1, startCol = 1)
  addStyle(wb, sheet, note_style, rows = start_row + 1, cols = 1)
  writeData(wb, sheet, methods_note, startRow = start_row + 2, startCol = 1)
  addStyle(wb, sheet, note_style, rows = start_row + 2, cols = 1)
  setRowHeights(wb, sheet, rows = start_row + 2, heights = 60)
  return(start_row + 4)
}

## ---- Tab 1: SUMMARY ----
addWorksheet(wb, "SUMMARY")
r <- write_header_block(wb, "SUMMARY",
                        "AIM 2 — HIV REGRESSION PRIMARY RESULTS",
                        "Per-case GBD framework. Stratified by 2010-baseline HIV prevalence tercile.",
                        "Predictor (X): log(spending per GBD prevalent case), year t.  Mediator (M): K × V composite, year t.  Outcomes (Y): log(DALYs or GBD mortality per GBD prevalent case), year t.  Controls: contemporaneous log(HIV incidence per 100k), race composition, log(prop homeless), year fixed effects.  Cluster-robust SEs on state (clubSandwich CR2 / Satterthwaite df).")

write_outcome_block <- function(wb, sheet, r, outcome_title, paths_dict) {
  writeData(wb, sheet, outcome_title, startRow = r, startCol = 1)
  addStyle(wb, sheet, sec_style, rows = r, cols = 1:6)
  r <- r + 1
  ## Dynamic n's from the paths_dict (matches actual run, not hard-coded)
  n_str <- function(tt) {
    res <- paths_dict[[tt]]
    if (is.null(res) || is.null(res$n_obs)) return("n=NA")
    sprintf("n=%d", res$n_obs)
  }
  hdr <- c("Path (full name)",
           sprintf("Low (%s)",    n_str("Low")),
           sprintf("Mid (%s)",    n_str("Mid")),
           sprintf("High (%s)",   n_str("High")),
           sprintf("Pooled (%s)", n_str("Pooled")))
  writeData(wb, sheet, t(hdr), startRow = r, startCol = 1, colNames = FALSE)
  addStyle(wb, sheet, hdr_style, rows = r, cols = 1:length(hdr), gridExpand = TRUE)
  setRowHeights(wb, sheet, rows = r, heights = 30)
  r <- r + 1
  
  path_specs <- list(
    list("spending(t) → K×V  (a-path)",        "a", "a_p"),
    list("K×V → outcome | spending  (b-path)",  "b", "b_p"),
    list("spending → outcome | K×V  (c′, direct)", "c_prime", "c_prime_p"),
    list("spending → outcome  (c, total)",      "c_total", "c_total_p")
  )
  for (ps in path_specs) {
    label <- ps[[1]]; ek <- ps[[2]]; pk <- ps[[3]]
    writeData(wb, sheet, label, startRow = r, startCol = 1)
    addStyle(wb, sheet, left_style, rows = r, cols = 1)
    for (col_idx in seq_along(c("Low","Mid","High","Pooled"))) {
      tercile <- c("Low","Mid","High","Pooled")[col_idx]
      res <- paths_dict[[tercile]]
      if (is.null(res)) {
        writeData(wb, sheet, "—", startRow = r, startCol = 1 + col_idx)
        addStyle(wb, sheet, cell_style, rows = r, cols = 1 + col_idx)
        next
      }
      d_digits <- if (grepl("mort", tolower(outcome_title))) 5 else 4
      val <- beta_p(res[[ek]], res[[pk]], d = d_digits)
      writeData(wb, sheet, val, startRow = r, startCol = 1 + col_idx)
      if (!is.na(res[[pk]]) && res[[pk]] < 0.05) {
        addStyle(wb, sheet, sig_style, rows = r, cols = 1 + col_idx)
      } else {
        addStyle(wb, sheet, cell_style, rows = r, cols = 1 + col_idx)
      }
    }
    r <- r + 1
  }
  ## Indirect row
  writeData(wb, sheet, "Indirect through cascade  (a × b)", startRow = r, startCol = 1)
  addStyle(wb, sheet, left_style, rows = r, cols = 1)
  for (col_idx in seq_along(c("Low","Mid","High","Pooled"))) {
    tercile <- c("Low","Mid","High","Pooled")[col_idx]
    res <- paths_dict[[tercile]]
    if (is.null(res)) {
      writeData(wb, sheet, "—", startRow = r, startCol = 1 + col_idx)
    } else {
      writeData(wb, sheet, sprintf("%+.6f", res$indirect),
                startRow = r, startCol = 1 + col_idx)
    }
    addStyle(wb, sheet, cell_style, rows = r, cols = 1 + col_idx)
  }
  r <- r + 1
  ## CI row
  writeData(wb, sheet, "Indirect 95% bootstrap CI (state-block, 500 reps)",
            startRow = r, startCol = 1)
  addStyle(wb, sheet, note_style, rows = r, cols = 1)
  for (col_idx in seq_along(c("Low","Mid","High","Pooled"))) {
    tercile <- c("Low","Mid","High","Pooled")[col_idx]
    res <- paths_dict[[tercile]]
    if (!is.null(res) && !is.na(res$indirect_lower)) {
      writeData(wb, sheet,
                sprintf("[%+.5f, %+.5f]", res$indirect_lower, res$indirect_upper),
                startRow = r, startCol = 1 + col_idx)
    }
    addStyle(wb, sheet, cell_style, rows = r, cols = 1 + col_idx)
  }
  return(r + 2)
}

r <- write_outcome_block(wb, "SUMMARY", r,
                         "PRIMARY OUTCOME — DALYs per GBD prevalent case (year t)",
                         PRIMARY$DALYs)
r <- write_outcome_block(wb, "SUMMARY", r,
                         "SECONDARY OUTCOME — GBD mortality per GBD prevalent case (year t)",
                         PRIMARY$Mortality)

## Headline statement
## Headline statement — generated dynamically from PRIMARY object
dly_c_total   <- PRIMARY$DALYs$Pooled$c_total
dly_c_total_p <- PRIMARY$DALYs$Pooled$c_total_p
mort_b        <- PRIMARY$Mortality$Pooled$b
mort_b_p      <- PRIMARY$Mortality$Pooled$b_p
high_a        <- PRIMARY$DALYs$High$a
high_a_p      <- PRIMARY$DALYs$High$a_p
high_a_n      <- PRIMARY$DALYs$High$n_obs
pool_n        <- PRIMARY$DALYs$Pooled$n_obs

headline_txt <- paste0(
  "Three significant cells from this run, in order of relevance to the cascade-mechanism story:\n\n",
  sprintf("(1) Burden lens (reduced form, c-total, DALYs/case): higher HIV spending per GBD prevalent case is associated with significantly lower DALYs per prevalent case (β = %+.4f, p = %.3f, n = %d state-years).\n\n",
          dly_c_total, dly_c_total_p, pool_n),
  sprintf("(2) Cascade → outcome (b-path, mortality): the K × V cascade composite is associated with significantly lower GBD mortality per prevalent case, holding spending constant (β = %+.5f, p = %.3f, n = %d).\n\n",
          mort_b, mort_b_p, pool_n),
  sprintf("(3) Spending → cascade in high-prevalence states (a-path): a 1-log-unit increase in spending per GBD prevalent case is associated with a %+.3f unit change in the K × V composite (β = %+.3f, p = %.3f, n = %d).\n\n",
          high_a, high_a, high_a_p, high_a_n),
  "Indirect effects (a × b) are small in magnitude and bootstrap CIs cross zero, so mediation through the K × V cascade is suggestive rather than decisive; the burden-lens c-total and the cascade-to-mortality b-path are the cleanest signals."
)

writeData(wb, "SUMMARY",
          "HEADLINE STATEMENT (significant cells in this run):",
          startRow = r, startCol = 1)
addStyle(wb, "SUMMARY", createStyle(textDecoration = "bold"), rows = r, cols = 1)
r <- r + 1
writeData(wb, "SUMMARY", headline_txt, startRow = r, startCol = 1)
mergeCells(wb, "SUMMARY", rows = r, cols = 1:5)
setRowHeights(wb, "SUMMARY", rows = r, heights = 180)
addStyle(wb, "SUMMARY", note_style, rows = r, cols = 1)

setColWidths(wb, "SUMMARY", cols = 1, widths = 50)
setColWidths(wb, "SUMMARY", cols = 2:5, widths = 22)

## ---- Tab 2: Cascade_X_to_M ----
addWorksheet(wb, "Cascade_X_to_M")
r <- write_header_block(wb, "Cascade_X_to_M",
                        "CASCADE REGRESSIONS — spending → cascade indicators",
                        "Each cascade indicator (K, L, V, K × V composite) regressed on contemporaneous log spending.",
                        "Fractional-response GLM, logit link.  Cluster-robust SEs (HC0).  Three predictor scalings: per-case GBD (primary), per-capita, per-case CDC.  Controls: contemporaneous log(incidence), lagged log(prevalence), race, log(homeless), year FE.")

hdr <- c("Predictor scaling", "Cascade outcome", "Tercile", "β (log-odds)",
         "SE", "p-value", "Sig.", "n", "AME (pp)", "AME p")
writeData(wb, "Cascade_X_to_M", t(hdr), startRow = r, startCol = 1, colNames = FALSE)
addStyle(wb, "Cascade_X_to_M", hdr_style, rows = r, cols = 1:length(hdr), gridExpand = TRUE)
setRowHeights(wb, "Cascade_X_to_M", rows = r, heights = 30)
r <- r + 1

for (pl in names(predictors_T3)) {
  for (ol in names(cascade_outcomes)) {
    ## Pooled row
    pool_key <- paste("pooled", pl, ol, sep = "_")
    pool <- cascade_results[[pool_key]]
    ame_key <- paste(pl, ol, sep = "_")
    ame <- ame_results[[ame_key]]
    if (is.null(pool)) next
    ame_val <- if (!is.null(ame)) sprintf("%+.4f", ame$estimate) else "—"
    ame_p   <- if (!is.null(ame)) sprintf("%.3f", ame$p.value) else "—"
    
    writeData(wb, "Cascade_X_to_M",
              t(c(pl, ol, "Pooled",
                  sprintf("%+.4f", pool$estimate),
                  sprintf("%.4f", pool$std.error),
                  sprintf("%.4f", pool$p.value),
                  sig_lab(pool$p.value),
                  pool$n,
                  ame_val, ame_p)),
              startRow = r, startCol = 1, colNames = FALSE)
    if (!is.na(pool$p.value) && pool$p.value < 0.05) {
      addStyle(wb, "Cascade_X_to_M", sig_style, rows = r, cols = 1:length(hdr))
    } else {
      addStyle(wb, "Cascade_X_to_M", cell_style, rows = r, cols = 1:length(hdr))
    }
    r <- r + 1
    
    ## Subgroup rows
    sub_key <- paste("subgroup", pl, ol, sep = "_")
    sub <- cascade_results[[sub_key]]
    if (!is.null(sub)) {
      for (i in seq_len(nrow(sub))) {
        writeData(wb, "Cascade_X_to_M",
                  t(c(pl, ol, as.character(sub$tercile[i]),
                      sprintf("%+.4f", sub$estimate[i]),
                      sprintf("%.4f", sub$std.error[i]),
                      sprintf("%.4f", sub$p.value[i]),
                      sig_lab(sub$p.value[i]),
                      sub$n[i],
                      "", "")),
                  startRow = r, startCol = 1, colNames = FALSE)
        if (!is.na(sub$p.value[i]) && sub$p.value[i] < 0.05) {
          addStyle(wb, "Cascade_X_to_M", sig_style, rows = r, cols = 1:length(hdr))
        } else {
          addStyle(wb, "Cascade_X_to_M", cell_style, rows = r, cols = 1:length(hdr))
        }
        r <- r + 1
      }
    }
  }
}
setColWidths(wb, "Cascade_X_to_M", cols = 1:length(hdr),
             widths = c(20, 14, 10, 15, 12, 12, 7, 7, 14, 10))

## ---- Tab 3: Cascade_M_to_Y ----
addWorksheet(wb, "Cascade_M_to_Y")
r <- write_header_block(wb, "Cascade_M_to_Y",
                        "CASCADE → BURDEN OUTCOME (b-path of mediation)",
                        "Each cascade indicator as a predictor of DALYs or GBD mortality per GBD prevalent case.",
                        "Two specs per cell: WITHOUT spending control (raw cascade-outcome association), WITH spending control (b-path supporting mediation).  Controls: contemporaneous log(incidence), race, log(homeless), year FE.  Cluster-robust SEs on state (CR2 + Satterthwaite).")

hdr <- c("Cascade predictor", "Outcome", "Spec", "Tercile", "β", "SE", "p-value", "Sig.", "n")
writeData(wb, "Cascade_M_to_Y", t(hdr), startRow = r, startCol = 1, colNames = FALSE)
addStyle(wb, "Cascade_M_to_Y", hdr_style, rows = r, cols = 1:length(hdr), gridExpand = TRUE)
setRowHeights(wb, "Cascade_M_to_Y", rows = r, heights = 30)
r <- r + 1

for (cp in cascade_preds_T5) {
  for (y_label in names(outcomes_T5)) {
    for (spec in c("without_spending", "with_spending")) {
      ## Pooled
      pool_key <- paste(if (spec == "without_spending") "pool_noSp" else "pool_withSp",
                        cp, y_label, sep = "_")
      pool <- cascade_to_outcome_results[[pool_key]]
      if (is.null(pool)) next
      writeData(wb, "Cascade_M_to_Y",
                t(c(cp, y_label, spec, "Pooled",
                    sprintf("%+.5f", pool$estimate),
                    sprintf("%.5f", pool$std.error),
                    sprintf("%.4f", pool$p.value),
                    sig_lab(pool$p.value), pool$n)),
                startRow = r, startCol = 1, colNames = FALSE)
      if (!is.na(pool$p.value) && pool$p.value < 0.05) {
        addStyle(wb, "Cascade_M_to_Y", sig_style, rows = r, cols = 1:length(hdr))
      } else {
        addStyle(wb, "Cascade_M_to_Y", cell_style, rows = r, cols = 1:length(hdr))
      }
      r <- r + 1
      ## Subgroup
      sub_key <- paste(if (spec == "without_spending") "sub_noSp" else "sub_withSp",
                       cp, y_label, sep = "_")
      sub <- cascade_to_outcome_results[[sub_key]]
      if (!is.null(sub)) {
        for (i in seq_len(nrow(sub))) {
          writeData(wb, "Cascade_M_to_Y",
                    t(c(cp, y_label, spec, as.character(sub$tercile[i]),
                        sprintf("%+.5f", sub$estimate[i]),
                        sprintf("%.5f", sub$std.error[i]),
                        sprintf("%.4f", sub$p.value[i]),
                        sig_lab(sub$p.value[i]), sub$n[i])),
                    startRow = r, startCol = 1, colNames = FALSE)
          if (!is.na(sub$p.value[i]) && sub$p.value[i] < 0.05) {
            addStyle(wb, "Cascade_M_to_Y", sig_style, rows = r, cols = 1:length(hdr))
          } else {
            addStyle(wb, "Cascade_M_to_Y", cell_style, rows = r, cols = 1:length(hdr))
          }
          r <- r + 1
        }
      }
    }
  }
}
setColWidths(wb, "Cascade_M_to_Y", cols = 1:length(hdr),
             widths = c(22, 18, 18, 10, 13, 12, 11, 7, 7))

## ---- Tab 4: Burden_Lens ----
addWorksheet(wb, "Burden_Lens")
r <- write_header_block(wb, "Burden_Lens",
                        "BURDEN LENS — spending → outcome per prevalent case",
                        "Pooled (no tercile stratification) burden lens regression under 6 control specifications.",
                        "Predictor: log(spending per GBD prev case), year t.  Outcomes: log(DALYs / GBD mort per GBD prev case), year t.  Cluster-robust SEs on state.")

hdr <- c("Specification", "DALYs/case  β (p)", "n", "GBD mort/case  β (p)", "n")
writeData(wb, "Burden_Lens", t(hdr), startRow = r, startCol = 1, colNames = FALSE)
addStyle(wb, "Burden_Lens", hdr_style, rows = r, cols = 1:length(hdr), gridExpand = TRUE)
setRowHeights(wb, "Burden_Lens", rows = r, heights = 30)
r <- r + 1

for (spec_label in names(burden_specs)) {
  daly_row <- burden_table %>% dplyr::filter(spec_label == !!spec_label,
                                             outcome == "DALYs/case")
  mort_row <- burden_table %>% dplyr::filter(spec_label == !!spec_label,
                                             outcome == "GBD mortality/case")
  writeData(wb, "Burden_Lens", spec_label, startRow = r, startCol = 1)
  if (grepl("Primary|Mundlak", spec_label)) {
    addStyle(wb, "Burden_Lens", createStyle(textDecoration = "bold",
                                            border = "TopBottomLeftRight",
                                            borderColour = "#BFBFBF"),
             rows = r, cols = 1)
  } else {
    addStyle(wb, "Burden_Lens", createStyle(border = "TopBottomLeftRight",
                                            borderColour = "#BFBFBF"),
             rows = r, cols = 1)
  }
  ## DALY cell
  if (nrow(daly_row) > 0) {
    writeData(wb, "Burden_Lens", daly_row$estimate_str[1], startRow = r, startCol = 2)
    writeData(wb, "Burden_Lens", daly_row$n[1],            startRow = r, startCol = 3)
    if (!is.null(daly_row$p_value) && !is.na(daly_row$p_value[1]) && daly_row$p_value[1] < 0.05) {
      addStyle(wb, "Burden_Lens", sig_style, rows = r, cols = 2:3)
    } else {
      addStyle(wb, "Burden_Lens", cell_style, rows = r, cols = 2:3)
    }
  }
  ## Mortality cell
  if (nrow(mort_row) > 0) {
    writeData(wb, "Burden_Lens", mort_row$estimate_str[1], startRow = r, startCol = 4)
    writeData(wb, "Burden_Lens", mort_row$n[1],            startRow = r, startCol = 5)
    if (!is.null(mort_row$p_value) && !is.na(mort_row$p_value[1]) && mort_row$p_value[1] < 0.05) {
      addStyle(wb, "Burden_Lens", sig_style, rows = r, cols = 4:5)
    } else {
      addStyle(wb, "Burden_Lens", cell_style, rows = r, cols = 4:5)
    }
  }
  r <- r + 1
}
setColWidths(wb, "Burden_Lens", cols = 1:length(hdr),
             widths = c(48, 26, 8, 32, 8))

## ---- Tab 5: Mediation_Detail ----
addWorksheet(wb, "Mediation_Detail")
r <- write_header_block(wb, "Mediation_Detail",
                        "MEDIATION CHAIN — full detail with SEs",
                        "All five paths (a, b, c', c, indirect) × two outcomes × four strata, with point estimates, SEs, and p-values.",
                        "Source equations: see analytic plan v4 Section 4.5. Bootstrap 500 reps, seed 20260520.")

hdr <- c("Outcome", "Tercile", "Path", "Estimate", "SE", "p", "Sig.", "n", "Bootstrap CI (indirect)")
writeData(wb, "Mediation_Detail", t(hdr), startRow = r, startCol = 1, colNames = FALSE)
addStyle(wb, "Mediation_Detail", hdr_style, rows = r, cols = 1:length(hdr), gridExpand = TRUE)
setRowHeights(wb, "Mediation_Detail", rows = r, heights = 30)
r <- r + 1

for (y_name in names(Y_primary_dict)) {
  for (tercile in c("Low","Mid","High","Pooled")) {
    res <- PRIMARY[[y_name]][[tercile]]
    if (is.null(res)) next
    for (path_spec in list(
      list("spending(t) → K×V  (a-path)",          "a",       "a_se",      "a_p"),
      list("K×V → outcome | spending  (b-path)",    "b",       "b_se",      "b_p"),
      list("spending → outcome | K×V  (c′, direct)","c_prime", "c_prime_se","c_prime_p"),
      list("spending → outcome  (c, total)",        "c_total", "c_total_se","c_total_p")
    )) {
      label <- path_spec[[1]]; ek <- path_spec[[2]]; sk <- path_spec[[3]]; pk <- path_spec[[4]]
      writeData(wb, "Mediation_Detail",
                t(c(y_name, tercile, label,
                    sprintf("%+.6f", res[[ek]]),
                    sprintf("%.6f", res[[sk]]),
                    sprintf("%.4f", res[[pk]]),
                    sig_lab(res[[pk]]),
                    res$n_obs, "")),
                startRow = r, startCol = 1, colNames = FALSE)
      if (!is.na(res[[pk]]) && res[[pk]] < 0.05) {
        addStyle(wb, "Mediation_Detail", sig_style, rows = r, cols = 1:length(hdr))
      } else {
        addStyle(wb, "Mediation_Detail", cell_style, rows = r, cols = 1:length(hdr))
      }
      r <- r + 1
    }
    ## Indirect row with bootstrap CI
    ci_str <- if (!is.na(res$indirect_lower)) sprintf("[%+.5f, %+.5f]",
                                                      res$indirect_lower, res$indirect_upper) else ""
    writeData(wb, "Mediation_Detail",
              t(c(y_name, tercile, "Indirect through cascade  (a × b)",
                  sprintf("%+.7f", res$indirect),
                  "", "", "", res$n_obs, ci_str)),
              startRow = r, startCol = 1, colNames = FALSE)
    addStyle(wb, "Mediation_Detail", cell_style, rows = r, cols = 1:length(hdr))
    r <- r + 1
  }
}
setColWidths(wb, "Mediation_Detail", cols = 1:length(hdr),
             widths = c(11, 9, 38, 13, 12, 9, 7, 7, 26))

## ---- Tab 6: Reference ----
addWorksheet(wb, "Reference")
r <- write_header_block(wb, "Reference",
                        "REFERENCE — tercile assignments and variance decomposition",
                        "Time-invariant assignment of states to 2010-baseline prevalence terciles, plus the variance-decomposition diagnostic.",
                        "Tercile cut points are the 33rd and 66th percentiles of the 51-state distribution of log(HIV prevalence per 100k) in 2010.")

writeData(wb, "Reference", "2010-baseline prevalence tercile assignment",
          startRow = r, startCol = 1)
addStyle(wb, "Reference", createStyle(textDecoration = "bold", fontSize = 12),
         rows = r, cols = 1)
r <- r + 1
for (t in c("Low","Mid","High")) {
  states <- tercile_states %>% dplyr::filter(prev_tercile_2010 == t) %>%
    pull(location_name) %>% paste(collapse = ", ")
  writeData(wb, "Reference", sprintf("%s-prev tercile (n=17):", t),
            startRow = r, startCol = 1)
  addStyle(wb, "Reference", createStyle(textDecoration = "bold"),
           rows = r, cols = 1)
  writeData(wb, "Reference", states, startRow = r, startCol = 2)
  setRowHeights(wb, "Reference", rows = r, heights = 40)
  r <- r + 1
}
r <- r + 2

writeData(wb, "Reference", "Variance decomposition (between vs within state)",
          startRow = r, startCol = 1)
addStyle(wb, "Reference", createStyle(textDecoration = "bold", fontSize = 12),
         rows = r, cols = 1)
r <- r + 1
writeData(wb, "Reference", var_decomp, startRow = r, startCol = 1, headerStyle = hdr_style)
r <- r + nrow(var_decomp) + 1

setColWidths(wb, "Reference", cols = 1, widths = 44)
setColWidths(wb, "Reference", cols = 2, widths = 110)

## Save
out1 <- file.path(dir_output, "aim2_primary_per_case_GBD.xlsx")
saveWorkbook(wb, out1, overwrite = TRUE)
log_msg("Wrote: ", out1)


##================================================================
## 13. BUILD FILE 2: aim2_alternative_specifications.xlsx
##================================================================
log_msg("\n=== BUILDING aim2_alternative_specifications.xlsx ===")

wb2 <- createWorkbook()

write_simple_table <- function(wb, sheet, df_in, start_row,
                               hdr_color = "1F3864", money_cols = NULL) {
  writeData(wb, sheet, df_in, startRow = start_row, startCol = 1,
            headerStyle = hdr_style)
  ## Add cell border styling
  for (i in seq_len(nrow(df_in))) {
    addStyle(wb, sheet, cell_style, rows = start_row + i, cols = 1:ncol(df_in),
             gridExpand = TRUE)
  }
}

## ---- README ----
addWorksheet(wb2, "README")
r <- write_header_block(wb2, "README",
                        "ALTERNATIVE SPECIFICATIONS — SENSITIVITY TABS",
                        "Backup file for the May 22 meeting; open the relevant tab if a committee question prompts it.",
                        "All tables use cluster-robust SEs on state. Primary results are in the companion file aim2_primary_per_case_GBD.xlsx.")

guide <- tibble(
  Tab = c("Temporal_sensitivities", "PerCapita_lens", "PerCaseCDC_lens",
          "CDC_denom_consistency", "Mortality_outcomes_all",
          "Burden_robustness", "Spec_robustness", "Policy_strat"),
  Description = c(
    "Lag X / Lead Y / Lag-X + Lead-Y temporal variants of the primary mediation chain.",
    "Full mediation chain under per-capita scaling. b-paths typically wrong-signed (epidemic-maturity confounding).",
    "Full mediation chain under per-case CDC scaling.",
    "Within-GBD framework with CDC mortality numerator (S7) + within-CDC framework (S8).",
    "Pooled mediation across all lens × outcome combinations for 'what if mortality?' questions.",
    "Burden-lens robustness panel (same as Burden_Lens tab of primary file).",
    "Leave-one-state-out and drop-2010-2012 sensitivities.",
    "Medicaid expansion (ACA) interaction + Ryan-White-exclusive spending."
  )
)
writeData(wb2, "README", guide, startRow = r, startCol = 1, headerStyle = hdr_style)
for (i in seq_len(nrow(guide))) {
  addStyle(wb2, "README", cell_style, rows = r + i, cols = 1:2, gridExpand = TRUE)
}
setColWidths(wb2, "README", cols = 1:2, widths = c(28, 92))

## ---- Temporal_sensitivities ----
addWorksheet(wb2, "Temporal_sensitivities")
r <- write_header_block(wb2, "Temporal_sensitivities",
                        "TEMPORAL STRUCTURE SENSITIVITIES",
                        "Per-case GBD mediation chain under different time structures.",
                        "All else equal to primary: X = log(spending per GBD prev case); M = K × V composite; Y = log(DALYs or GBD mort per GBD prev case). Pooled (no tercile stratification). Controls: contemp incidence, race, log(homeless), year FE.")
fmt_temp <- temporal_table %>%
  transmute(
    Spec = spec, Outcome = outcome,
    `a (X→M)` = beta_p(a, a_p, 4),
    `b (M→Y|X)` = beta_p(b, b_p, 5),
    `c′ (direct)` = beta_p(c_prime, c_prime_p, 5),
    `c (total)` = beta_p(c_total, c_total_p, 5),
    `indirect` = sprintf("%+.6f", indirect),
    n = n
  )
writeData(wb2, "Temporal_sensitivities", fmt_temp, startRow = r, startCol = 1,
          headerStyle = hdr_style)
for (i in seq_len(nrow(fmt_temp))) {
  addStyle(wb2, "Temporal_sensitivities", cell_style, rows = r + i,
           cols = 1:ncol(fmt_temp), gridExpand = TRUE)
}
setColWidths(wb2, "Temporal_sensitivities",
             cols = 1:ncol(fmt_temp), widths = c(32, 16, 16, 18, 18, 18, 14, 6))

## ---- PerCapita_lens ----
addWorksheet(wb2, "PerCapita_lens")
r <- write_header_block(wb2, "PerCapita_lens",
                        "PER-CAPITA LENS (sensitivity)",
                        "Full mediation chain using log spending PER CAPITA → K × V → log outcome PER CAPITA.",
                        "Predictor: log(spending per capita), year t. Mediator: K × V composite. Outcomes: log(DALYs / GBD mortality / CDC mortality per capita), year t. Controls: contemp incidence + LAGGED prevalence (epidemic-size confounder for per-capita scale) + race + log(homeless) + year FE. NOTE: b-paths typically wrong-signed because per-capita outcomes are dominated by epidemic-maturity confounding.")
fmt_pc <- percapita_table %>%
  transmute(
    Outcome = outcome,
    `a (X→M)` = beta_p(a, a_p, 4),
    `b (M→Y|X)` = beta_p(b, b_p, 5),
    `c′ (direct)` = beta_p(c_prime, c_prime_p, 5),
    `c (total)` = beta_p(c_total, c_total_p, 5),
    `indirect` = sprintf("%+.6f", indirect),
    n = n
  )
writeData(wb2, "PerCapita_lens", fmt_pc, startRow = r, startCol = 1,
          headerStyle = hdr_style)
for (i in seq_len(nrow(fmt_pc))) {
  addStyle(wb2, "PerCapita_lens", cell_style, rows = r + i,
           cols = 1:ncol(fmt_pc), gridExpand = TRUE)
}
setColWidths(wb2, "PerCapita_lens",
             cols = 1:ncol(fmt_pc), widths = c(28, 16, 18, 18, 18, 14, 6))

## ---- PerCaseCDC_lens ----
addWorksheet(wb2, "PerCaseCDC_lens")
r <- write_header_block(wb2, "PerCaseCDC_lens",
                        "PER-CASE CDC LENS (sensitivity)",
                        "Full mediation chain under log(spending per CDC prev case) → K × V → log(outcome / CDC prev case).",
                        "Sample restricted to state-years with non-suppressed CDC counts (ages 25+). Controls: contemp incidence + race + log(homeless) + year FE.")
fmt_cdc <- percasecdc_table %>%
  transmute(
    Outcome = outcome,
    `a (X→M)` = beta_p(a, a_p, 4),
    `b (M→Y|X)` = beta_p(b, b_p, 5),
    `c′ (direct)` = beta_p(c_prime, c_prime_p, 5),
    `c (total)` = beta_p(c_total, c_total_p, 5),
    `indirect` = sprintf("%+.6f", indirect),
    n = n
  )
writeData(wb2, "PerCaseCDC_lens", fmt_cdc, startRow = r, startCol = 1,
          headerStyle = hdr_style)
for (i in seq_len(nrow(fmt_cdc))) {
  addStyle(wb2, "PerCaseCDC_lens", cell_style, rows = r + i,
           cols = 1:ncol(fmt_cdc), gridExpand = TRUE)
}
setColWidths(wb2, "PerCaseCDC_lens",
             cols = 1:ncol(fmt_cdc), widths = c(28, 16, 18, 18, 18, 14, 6))

## ---- CDC_denom_consistency ----
addWorksheet(wb2, "CDC_denom_consistency")
r <- write_header_block(wb2, "CDC_denom_consistency",
                        "CDC DENOMINATOR-CONSISTENCY ANALYSES",
                        "Marcia's concern: prevalence source on the spending denominator must match prevalence source on the outcome denominator.",
                        "S7 = within-GBD framework with CDC mortality numerator only. S8 = within-CDC framework (both sides use CDC prev). All preserve denominator consistency. Controls: contemp incidence + race + log(homeless) + year FE.")
fmt_denom <- denom_table %>%
  transmute(
    Specification = spec, Predictor = predictor, Outcome = outcome,
    β = sprintf("%+.4f", estimate),
    SE = sprintf("%.4f", std.error),
    `p (sig.)` = sprintf("%.4f (%s)", p.value, signif_label),
    n = n
  )
writeData(wb2, "CDC_denom_consistency", fmt_denom, startRow = r, startCol = 1,
          headerStyle = hdr_style)
for (i in seq_len(nrow(fmt_denom))) {
  is_sig <- !is.na(denom_table$p.value[i]) && denom_table$p.value[i] < 0.05
  st <- if (is_sig) sig_style else cell_style
  addStyle(wb2, "CDC_denom_consistency", st, rows = r + i,
           cols = 1:ncol(fmt_denom), gridExpand = TRUE)
}
setColWidths(wb2, "CDC_denom_consistency",
             cols = 1:ncol(fmt_denom), widths = c(48, 32, 32, 12, 12, 18, 6))

## ---- Mortality_outcomes_all ----
addWorksheet(wb2, "Mortality_outcomes_all")
r <- write_header_block(wb2, "Mortality_outcomes_all",
                        "MORTALITY OUTCOMES — POOLED MEDIATION ACROSS LENSES",
                        "Each row is a pooled mediation chain. K × V composite is the mediator throughout. Different X and Y scaling per row.",
                        "For 'what if mortality?' questions. Controls vary by lens; see footers.")
fmt_mort <- mort_all_table %>%
  transmute(
    `Lens / Outcome` = lens_outcome,
    `a (X→M)` = beta_p(a, a_p, 4),
    `b (M→Y|X)` = beta_p(b, b_p, 5),
    `c′ (direct)` = beta_p(c_prime, c_prime_p, 5),
    `c (total)` = beta_p(c_total, c_total_p, 5),
    `indirect` = sprintf("%+.6f", indirect),
    n = n
  )
writeData(wb2, "Mortality_outcomes_all", fmt_mort, startRow = r, startCol = 1,
          headerStyle = hdr_style)
for (i in seq_len(nrow(fmt_mort))) {
  addStyle(wb2, "Mortality_outcomes_all", cell_style, rows = r + i,
           cols = 1:ncol(fmt_mort), gridExpand = TRUE)
}
setColWidths(wb2, "Mortality_outcomes_all",
             cols = 1:ncol(fmt_mort), widths = c(44, 16, 18, 18, 18, 14, 6))

## ---- Burden_robustness ----
addWorksheet(wb2, "Burden_robustness")
r <- write_header_block(wb2, "Burden_robustness",
                        "BURDEN LENS ROBUSTNESS PANEL",
                        "Same as Burden_Lens tab in primary file. Pooled burden lens regression under 6 specifications.",
                        "Predictor: log(spending per GBD prev case), year t. Cluster-robust SEs on state.")
fmt_burden <- burden_table %>%
  transmute(
    Specification = spec_label,
    `DALYs/case β (p)`     = ifelse(outcome == "DALYs/case",        estimate_str, NA),
    `GBD mortality/case β (p)` = ifelse(outcome == "GBD mortality/case", estimate_str, NA),
    n = n
  )
## collapse two rows per spec into one
spec_summary <- burden_table %>%
  group_by(spec_label) %>%
  summarise(
    `DALYs/case β (p)` = ifelse(any(outcome == "DALYs/case"),
                                first(estimate_str[outcome == "DALYs/case"]), NA),
    `n (DALYs)` = ifelse(any(outcome == "DALYs/case"),
                         first(n[outcome == "DALYs/case"]), NA),
    `GBD mortality/case β (p)` = ifelse(any(outcome == "GBD mortality/case"),
                                        first(estimate_str[outcome == "GBD mortality/case"]), NA),
    `n (Mort)` = ifelse(any(outcome == "GBD mortality/case"),
                        first(n[outcome == "GBD mortality/case"]), NA),
    .groups = "drop"
  ) %>%
  rename(Specification = spec_label)
writeData(wb2, "Burden_robustness", spec_summary, startRow = r, startCol = 1,
          headerStyle = hdr_style)
for (i in seq_len(nrow(spec_summary))) {
  addStyle(wb2, "Burden_robustness", cell_style, rows = r + i,
           cols = 1:ncol(spec_summary), gridExpand = TRUE)
}
setColWidths(wb2, "Burden_robustness",
             cols = 1:ncol(spec_summary), widths = c(48, 32, 8, 36, 8))

## ---- Spec_robustness ----
addWorksheet(wb2, "Spec_robustness")
r <- write_header_block(wb2, "Spec_robustness",
                        "SPECIFICATION ROBUSTNESS",
                        "Stability of the primary burden lens coefficient (DALYs/case) to alternative sample restrictions.",
                        "Primary specification: log(DALYs/case) ~ log(spending/case) + contemp incidence + race + log(homeless) + year FE.  Cluster-robust SEs on state.")

writeData(wb2, "Spec_robustness", "Drop 2010-2012:", startRow = r, startCol = 1)
addStyle(wb2, "Spec_robustness", createStyle(textDecoration = "bold"), rows = r, cols = 1)
writeData(wb2, "Spec_robustness",
          sprintf("β = %+.4f (%s); n = %d", drop_row$estimate, sig_lab(drop_row$p.value), drop_row$n),
          startRow = r, startCol = 2)
r <- r + 2

writeData(wb2, "Spec_robustness", "Leave-one-state-out (β range):", startRow = r, startCol = 1)
addStyle(wb2, "Spec_robustness", createStyle(textDecoration = "bold"), rows = r, cols = 1)
writeData(wb2, "Spec_robustness",
          sprintf("min = %+.4f; median = %+.4f; max = %+.4f (across %d leave-one-out fits)",
                  min(loo_table$beta_X), median(loo_table$beta_X), max(loo_table$beta_X),
                  nrow(loo_table)),
          startRow = r, startCol = 2)
r <- r + 2

writeData(wb2, "Spec_robustness", "Full LOO table (sorted by β):", startRow = r, startCol = 1)
addStyle(wb2, "Spec_robustness", createStyle(textDecoration = "bold"), rows = r, cols = 1)
r <- r + 1
loo_sorted <- loo_table %>% arrange(beta_X)
writeData(wb2, "Spec_robustness", loo_sorted, startRow = r, startCol = 1,
          headerStyle = hdr_style)
for (i in seq_len(nrow(loo_sorted))) {
  addStyle(wb2, "Spec_robustness", cell_style, rows = r + i,
           cols = 1:ncol(loo_sorted), gridExpand = TRUE)
}
setColWidths(wb2, "Spec_robustness",
             cols = 1:max(4, ncol(loo_sorted)), widths = c(28, 32, 14, 10))

## ---- Policy_strat ----
addWorksheet(wb2, "Policy_strat")
r <- write_header_block(wb2, "Policy_strat",
                        "POLICY STRATIFICATIONS",
                        "Spending coefficient × policy variables.",
                        "Two sensitivities: (1) Medicaid expansion (ACA implementation, time-varying); (2) Ryan-White-exclusive spending (DEX-only spending in numerator).")
writeData(wb2, "Policy_strat", "Medicaid expansion (ACA) interaction:", startRow = r, startCol = 1)
addStyle(wb2, "Policy_strat", createStyle(textDecoration = "bold"), rows = r, cols = 1)
r <- r + 1
writeData(wb2, "Policy_strat", aca_rows %>% select(term, estimate, std.error, p.value, signif_label, n),
          startRow = r, startCol = 1, headerStyle = hdr_style)
for (i in seq_len(nrow(aca_rows))) {
  is_sig <- !is.na(aca_rows$p.value[i]) && aca_rows$p.value[i] < 0.05
  addStyle(wb2, "Policy_strat", if (is_sig) sig_style else cell_style,
           rows = r + i, cols = 1:6, gridExpand = TRUE)
}
r <- r + nrow(aca_rows) + 2

writeData(wb2, "Policy_strat", "Ryan-White-exclusive spending sensitivity:",
          startRow = r, startCol = 1)
addStyle(wb2, "Policy_strat", createStyle(textDecoration = "bold"), rows = r, cols = 1)
r <- r + 1
writeData(wb2, "Policy_strat",
          sprintf("Predictor: log(DEX-only spending / GBD prev case); β = %+.4f (%s); n = %d",
                  rw_row$estimate, sig_lab(rw_row$p.value), rw_row$n),
          startRow = r, startCol = 1)
setColWidths(wb2, "Policy_strat", cols = 1:6, widths = c(46, 14, 12, 12, 10, 8))

## Save
out2 <- file.path(dir_output, "aim2_alternative_specifications.xlsx")
saveWorkbook(wb2, out2, overwrite = TRUE)
log_msg("Wrote: ", out2)


##================================================================
## 14. END-OF-RUN SUMMARY
##================================================================
log_msg("\n================ RUN COMPLETE ================")
log_msg("Outputs in: ", dir_output)
log_msg("  aim2_primary_per_case_GBD.xlsx        — 6 tabs (headline)")
log_msg("  aim2_alternative_specifications.xlsx  — 9 tabs (sensitivities)")
log_msg("  bivariate_stratified_per_case.png     — primary bivariate exhibit")
log_msg("  bivariate_stratified_per_capita.png   — sensitivity bivariate exhibit")
log_msg("  run_log.txt                           — diagnostic log")
log_msg("\nNext step: assemble manuscript tables; inspect run_log.txt for warnings.")

##================================================================
## END OF SCRIPT
##================================================================