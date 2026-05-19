##----------------------------------------------------------------
##  Title:    aim2_regressions_combined.R
##  Purpose:  End-to-end regression pipeline for Aim 2 HIV paper.
##            Implements the pre-specified analytic plan agreed
##            with the committee on 2026-05-18.
##
##  Replaces the exploratory C_02_HIV_per_case_models.R,
##  C_04_HIV_per_capita_models.R, and C_05_HIV_per_capita_cascade.R
##  for the regression slice of the manuscript.
##
##  Produces:
##    - Table 2  : Burden lens, 8 panels x 3 outcome columns
##    - Table 3  : Cascade regressions, per-capita X primary + per-case X sensitivity
##    - Table 4  : Cascade -> DALY mediation chained to burden lens primary
##    - Table S1 : Per-capita reduced form (methodological exhibit)
##    - Table S2 : Exploratory prevention lens (incidence outcome)
##    - Table S3 : Leave-one-state-out for burden lens Panel B
##    - Table S4 : Drop 2010-2012 (early cascade-reporting years)
##    - Table S5 : Medicaid expansion stratification
##    - Table S6 : Ryan-White-exclusive spending sensitivity
##    - Exhibit 1: Stratified bivariate scatter (3 terciles x 3 outcomes)
##    - Exhibit S1: Unstratified bivariate scatter
##
##  Identification framing (one paragraph for methods):
##    >95% of variance in HIV spending and HIV outcomes is between
##    states; U.S. HIV dollars are allocated to where the burden is.
##    Regression coefficients are partial associations, not causal
##    estimates. Current prevalence is excluded (descendant of
##    spending via ART and U=U). Current incidence is replaced by
##    its one-year lag (partial mediator via PrEP / testing / U=U).
##    Primary specification interacts spending x terciles of
##    2010-baseline prevalence (time-invariant, pre-treatment).
##    Mundlak _B/_W decomposition is the bridge to frontier and SE.
##
##  Author / runner: Bulat Idrisov  (script by Claude, 2026-05-18)
##----------------------------------------------------------------


##================================================================
## 0.  SETUP
##================================================================
rm(list = ls())

## ---- paths --------------------------------------------------------
## Default to the IHME-cluster layout; override the two paths below
## if running locally.
if (Sys.info()[["sysname"]] == "Linux") {
  h <- paste0("/ihme/homes/", Sys.info()[["user"]], "/")
} else if (Sys.info()[["sysname"]] == "Darwin") {
  h <- paste0("/Volumes/", Sys.info()[["user"]], "/")
} else {
  h <- "H:/"
}

## EDIT THESE TWO PATHS for a fresh run.
## INPUT: the panel produced by C_05 diagnostic block
##        (cascade indicators already merged into the per-capita panel)
panel_input_path <- file.path(
  h, "aim_outputs/Aim2/C_frontier_analysis",
  "20260517", "analysis_per_capita",
  "df_hiv_cascade_panel.csv"
)
## OUTPUT: a fresh date-stamped directory under your aim_outputs tree
output_date <- format(Sys.time(), "%Y%m%d_%H%M")
dir_output  <- file.path(
  h, "aim_outputs/Aim2/C_frontier_analysis",
  output_date, "analytic_plan_regressions"
)
if (!dir.exists(dir_output)) dir.create(dir_output, recursive = TRUE)


## ---- libraries ----------------------------------------------------
user_lib <- file.path(h, "R_packages")
if (!dir.exists(user_lib)) dir.create(user_lib, recursive = TRUE)
.libPaths(c(user_lib, .libPaths()))

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(
  data.table, tidyverse, glue, broom,
  lmtest, sandwich, clubSandwich,
  ggplot2, scales,
  marginaleffects
)
tryCatch(conflicted::conflicts_prefer(dplyr::filter, .quiet = TRUE),
         error = function(e) invisible(NULL))


## ---- run log ------------------------------------------------------
log_file <- file.path(dir_output, "run_log.txt")
log_msg <- function(...) {
  msg <- paste0(..., collapse = "")
  cat(msg, "\n")
  cat(msg, "\n", file = log_file, append = TRUE)
}
cat(sprintf("Run: %s\nInput: %s\nOutput: %s\n\n",
            Sys.time(), panel_input_path, dir_output),
    file = log_file)


##================================================================
## 1.  LOAD AND VALIDATE PANEL
##================================================================
if (!file.exists(panel_input_path)) {
  stop("Input panel not found: ", panel_input_path,
       "\nUpdate `panel_input_path` at the top of this script.")
}
df <- read.csv(panel_input_path, stringsAsFactors = FALSE)
df <- df %>% dplyr::filter(acause == "hiv")
log_msg("Loaded ", nrow(df), " rows, ", ncol(df), " columns from cascade panel")

## Columns we rely on -- fail fast if any are missing
required_cols <- c(
  "location_id", "location_name", "year_id", "acause",
  # outcomes
  "as_daly_prev_ratio_log", "as_mort_prev_ratio_log",
  "log_daly_per_capita", "log_mortality_per_capita",
  "log_cdc_mortality_per_capita", "log_cdc_prevalence_per_100k",
  "log_incidence_per_100k",
  # predictors
  "rw_dex_hiv_prev_ratio_log",
  "log_spending_per_capita",
  # lagged
  "rw_dex_hiv_prev_ratio_log_l1",
  "log_spending_per_capita_l1",
  "log_incidence_per_100k_l1",
  "log_prevalence_per_100k_l1",
  # Mundlak B/W
  "rw_dex_hiv_prev_ratio_log_B", "rw_dex_hiv_prev_ratio_log_W",
  "log_spending_per_capita_B",   "log_spending_per_capita_W",
  # covariates
  "race_prop_BLCK", "race_prop_HISP",
  "log_prop_homeless",
  "log_ldi_pc", "edu_yrs", "aca_implemented_status",
  "opioid_prevalence_counts",
  # prevalence (for tercile and CDC outcome)
  "log_prevalence_per_100k",
  "log_cdc_prevalence_per_100k",
  # cascade
  "cdc_knowledge_status", "cdc_linkage_1mo",
  "cdc_viral_suppress",
  # for RW-exclusive sensitivity
  "ryan_white_funding_final", "spend_all",
  "hiv_prevalence_counts"
)
missing_required <- setdiff(required_cols, names(df))
if (length(missing_required) > 0) {
  stop("Required columns missing from panel: ",
       paste(missing_required, collapse = ", "))
}


##================================================================
## 2.  DERIVE 2010-BASELINE PREVALENCE TERCILES (TIME-INVARIANT)
##================================================================
## qcut-3 on the 51-state distribution of prevalence_per_100k in 2010.
## Each state's tercile is fixed for all 10 years of the panel.
prev_2010 <- df %>%
  dplyr::filter(year_id == 2010) %>%
  select(location_id, log_prevalence_per_100k) %>%
  rename(log_prev_2010 = log_prevalence_per_100k)

cut_points <- quantile(prev_2010$log_prev_2010,
                       probs = c(1/3, 2/3),
                       na.rm = TRUE)
log_msg("Tercile cut points (log prevalence per 100k, 2010): ",
        sprintf("low<%.3f  high>%.3f", cut_points[1], cut_points[2]))

prev_2010 <- prev_2010 %>%
  mutate(
    prev_tercile_2010 = case_when(
      log_prev_2010 <= cut_points[1] ~ "Low",
      log_prev_2010 <= cut_points[2] ~ "Mid",
      TRUE                           ~ "High"
    ),
    prev_tercile_2010 = factor(prev_tercile_2010,
                               levels = c("Low", "Mid", "High")),
    high_prev_2010 = as.integer(prev_tercile_2010 == "High")
  )

df <- df %>%
  left_join(prev_2010 %>% select(location_id, prev_tercile_2010,
                                 high_prev_2010, log_prev_2010),
            by = "location_id")

stopifnot(all(!is.na(df$prev_tercile_2010)))
log_msg("Tercile assignment (n states): ",
        paste(sprintf("%s=%d",
                      levels(df$prev_tercile_2010),
                      tapply(rep(1, nrow(df)),
                             df$prev_tercile_2010, sum) / 10),
              collapse = ", "))


##================================================================
## 3.  DERIVE CDC PER-CASE OUTCOME AND UPSTREAM-RISK-FACTOR VARS
##================================================================

## CDC mortality per CDC prevalent case (log scale).
## Uses CDC counts on both numerator and denominator so the column
## is internally consistent on age scope (CDC mortality is reported
## at ages 13+ / 25+; we use the CDC published prevalence count).
df <- df %>%
  mutate(
    cdc_mort_prev_ratio = exp(log_cdc_mortality_per_capita) /
      (exp(log_cdc_prevalence_per_100k) / 1e5),
    cdc_mort_prev_ratio_log = ifelse(cdc_mort_prev_ratio > 0,
                                     log(cdc_mort_prev_ratio),
                                     NA_real_)
  )

## Upstream HIV risk-factor variables (Panel G).
## opioid_prevalence_counts has no pre-built log; safe-log here.
safe_log <- function(x) ifelse(is.finite(x) & x > 0, log(x), NA_real_)
df <- df %>%
  mutate(
    log_opioid_prev = safe_log(opioid_prevalence_counts)
  )

## K x V composite mediator (cascade)
df <- df %>%
  mutate(
    cdc_kv_composite = ifelse(
      !is.na(cdc_knowledge_status) & !is.na(cdc_viral_suppress) &
        cdc_knowledge_status >= 0 & cdc_knowledge_status <= 1 &
        cdc_viral_suppress   >= 0 & cdc_viral_suppress   <= 1,
      cdc_knowledge_status * cdc_viral_suppress,
      NA_real_
    )
  )

## Leading DALY-per-case for the lagged-spending->mediator->next-year-Y mediation
df <- df %>%
  arrange(location_id, year_id) %>%
  group_by(location_id) %>%
  mutate(
    as_daly_prev_ratio_log_f1 = dplyr::lead(as_daly_prev_ratio_log, 1)
  ) %>%
  ungroup()


##================================================================
## 4.  HELPER FUNCTIONS
##================================================================

## Cluster-robust SEs on lm via clubSandwich (CR2 + Satterthwaite)
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
    message("CR2 SE extraction failed: ", e$message, " -- falling back to vcovCL HC0")
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

## Cluster-robust SEs on quasibinomial GLM
extract_glm_CL <- function(fit, cluster_var) {
  vc <- vcovCL(fit, cluster = cluster_var, type = "HC0")
  ct <- coeftest(fit, vcov. = vc)
  tibble(
    term      = rownames(ct),
    estimate  = ct[, "Estimate"],
    std.error = ct[, "Std. Error"],
    statistic = ct[, "z value"],
    df        = NA_real_,
    p.value   = ct[, "Pr(>|z|)"]
  )
}

## Significance label
sig_lab <- function(p) {
  case_when(
    is.na(p)     ~ "NA",
    p < 0.001    ~ "p < 0.001",
    p < 0.01     ~ "p < 0.01",
    p < 0.05     ~ "p < 0.05",
    p < 0.10     ~ "p < 0.10",
    TRUE         ~ "ns"
  )
}

## Joint Wald test of two coefficients = 0
## Computed manually against a cluster-robust vcov so we do not depend
## on the (version-fragile) clubSandwich::Wald_test API.
wald_joint <- function(fit, cluster_var, coef_names) {
  vc <- tryCatch(vcovCR(fit, cluster = cluster_var, type = "CR2"),
                 error = function(e) vcovCL(fit, cluster = cluster_var, type = "HC0"))
  cf <- coef(fit)
  present <- coef_names %in% names(cf)
  if (!all(present)) {
    return(tibble(F_stat = NA_real_, df_num = NA_integer_,
                  df_denom = NA_integer_, p_value = NA_real_,
                  test = paste("Joint F:", paste(coef_names[!present],
                                                 collapse = ", "),
                               "not in model")))
  }
  beta_sub <- cf[coef_names]
  V_sub    <- vc[coef_names, coef_names, drop = FALSE]
  V_inv    <- tryCatch(solve(V_sub),
                       error = function(e) MASS::ginv(V_sub))
  W        <- as.numeric(t(beta_sub) %*% V_inv %*% beta_sub)
  q        <- length(coef_names)
  ## Use cluster count - 1 as denominator df (conservative)
  n_clusters <- length(unique(cluster_var))
  df_d <- max(n_clusters - q, 1)
  F_stat <- W / q
  p_val  <- pf(F_stat, df1 = q, df2 = df_d, lower.tail = FALSE)
  tibble(F_stat = F_stat, df_num = q, df_denom = df_d,
         p_value = p_val,
         test = paste0("Joint F: ", paste(coef_names, collapse = " = "),
                       " = 0"))
}

## Helper: collapse residual covariate set into a string
rhs_baseline <- function() {
  "race_prop_BLCK + race_prop_HISP + log_prop_homeless + year_factor"
}
rhs_primary <- function() {
  paste(rhs_baseline(),
        "+ log_incidence_per_100k_l1")
}
rhs_upstream <- function() {
  paste(rhs_baseline(),
        "+ log_opioid_prev + aca_implemented_status",
        "+ edu_yrs + log_ldi_pc")
}

## Make sure year_factor exists
if (!"year_factor" %in% names(df)) {
  df$year_factor <- factor(df$year_id)
}


##================================================================
## 5.  TABLE 2  —  BURDEN LENS (8 PANELS x 3 OUTCOME COLUMNS)
##================================================================
## All burden specs use spending-per-prevalent-case as the predictor
## (rw_dex_hiv_prev_ratio_log) and one of three outcomes:
##   - as_daly_prev_ratio_log         (DALYs / prev case, GBD)        PRIMARY
##   - as_mort_prev_ratio_log         (mortality / prev case, GBD)    SECONDARY
##   - cdc_mort_prev_ratio_log        (CDC mort / CDC prev, footnoted) ROBUSTNESS
##================================================================

outcomes <- list(
  "DALYs_per_case_GBD"        = "as_daly_prev_ratio_log",
  "Mortality_per_case_GBD"    = "as_mort_prev_ratio_log",
  "Mortality_per_case_CDC25p" = "cdc_mort_prev_ratio_log"
)

table2_rows <- list()

for (out_lab in names(outcomes)) {
  y <- outcomes[[out_lab]]
  
  ## --- A: Bivariate (no covariates) ---
  panel_label <- "A_Bivariate"
  fA <- lm(as.formula(paste(y, "~ rw_dex_hiv_prev_ratio_log")),
           data = df %>% dplyr::filter(!is.na(.data[[y]])))
  cA <- extract_lm_CR2(fA, df %>% dplyr::filter(!is.na(.data[[y]])) %>% pull(location_id)) %>%
    dplyr::filter(term == "rw_dex_hiv_prev_ratio_log") %>%
    mutate(panel = panel_label, spec_label = "Bivariate",
           outcome = out_lab, n = nobs(fA),
           adj_r2 = summary(fA)$adj.r.squared)
  table2_rows[[paste0(out_lab, "_", panel_label)]] <- cA
  
  ## --- B: PRIMARY (with tercile interaction) ---
  panel_label <- "B_Primary_tercile_interaction"
  formula_B <- paste0(
    y, " ~ rw_dex_hiv_prev_ratio_log * prev_tercile_2010 + ",
    rhs_primary()
  )
  d_B <- df %>% dplyr::filter(!is.na(.data[[y]]),
                              !is.na(log_incidence_per_100k_l1))
  fB <- lm(as.formula(formula_B), data = d_B)
  cB <- extract_lm_CR2(fB, d_B$location_id) %>%
    dplyr::filter(grepl("rw_dex_hiv_prev_ratio_log|prev_tercile_2010", term)) %>%
    mutate(panel = panel_label, spec_label = "Primary x tercile",
           outcome = out_lab, n = nobs(fB),
           adj_r2 = summary(fB)$adj.r.squared)
  ## Joint F-test on the two interaction terms
  wB <- wald_joint(
    fB, d_B$location_id,
    coef_names = c("rw_dex_hiv_prev_ratio_log:prev_tercile_2010Mid",
                   "rw_dex_hiv_prev_ratio_log:prev_tercile_2010High")
  ) %>% mutate(panel = panel_label, outcome = out_lab)
  table2_rows[[paste0(out_lab, "_", panel_label)]] <- cB
  table2_rows[[paste0(out_lab, "_", panel_label, "_wald")]] <- wB
  
  ## Subgroup spending effects (re-parameterized for direct reading)
  ## beta_Low  = main; beta_Mid = main + Mid interaction; beta_High = main + High interaction
  b_main <- cB$estimate[cB$term == "rw_dex_hiv_prev_ratio_log"]
  b_mid_int  <- cB$estimate[cB$term == "rw_dex_hiv_prev_ratio_log:prev_tercile_2010Mid"]
  b_high_int <- cB$estimate[cB$term == "rw_dex_hiv_prev_ratio_log:prev_tercile_2010High"]
  subgroups <- tibble(
    panel = panel_label, spec_label = "Subgroup spending beta",
    outcome = out_lab,
    term = c("beta_Low", "beta_Mid", "beta_High"),
    estimate = c(b_main,
                 b_main + ifelse(length(b_mid_int)  > 0, b_mid_int,  0),
                 b_main + ifelse(length(b_high_int) > 0, b_high_int, 0))
  )
  table2_rows[[paste0(out_lab, "_", panel_label, "_subgroups")]] <- subgroups
  
  ## --- C: No interaction (single headline coefficient) ---
  panel_label <- "C_No_interaction"
  formula_C <- paste0(y, " ~ rw_dex_hiv_prev_ratio_log + ", rhs_primary())
  d_C <- df %>% dplyr::filter(!is.na(.data[[y]]),
                              !is.na(log_incidence_per_100k_l1))
  fC <- lm(as.formula(formula_C), data = d_C)
  cC <- extract_lm_CR2(fC, d_C$location_id) %>%
    dplyr::filter(term == "rw_dex_hiv_prev_ratio_log") %>%
    mutate(panel = panel_label, spec_label = "Primary, no interaction",
           outcome = out_lab, n = nobs(fC),
           adj_r2 = summary(fC)$adj.r.squared)
  table2_rows[[paste0(out_lab, "_", panel_label)]] <- cC
  
  ## --- D: Binary (median-split) interaction ---
  panel_label <- "D_Binary_interaction"
  formula_D <- paste0(
    y, " ~ rw_dex_hiv_prev_ratio_log * high_prev_2010 + ", rhs_primary()
  )
  d_D <- df %>% dplyr::filter(!is.na(.data[[y]]),
                              !is.na(log_incidence_per_100k_l1))
  fD <- lm(as.formula(formula_D), data = d_D)
  cD <- extract_lm_CR2(fD, d_D$location_id) %>%
    dplyr::filter(grepl("rw_dex_hiv_prev_ratio_log|high_prev_2010", term)) %>%
    mutate(panel = panel_label, spec_label = "Primary x binary (high prev)",
           outcome = out_lab, n = nobs(fD),
           adj_r2 = summary(fD)$adj.r.squared)
  table2_rows[[paste0(out_lab, "_", panel_label)]] <- cD
  
  ## --- E: Sensitivity, contemporaneous incidence ---
  panel_label <- "E_Sens_contemporaneous_incidence"
  formula_E <- paste0(
    y, " ~ rw_dex_hiv_prev_ratio_log + ",
    rhs_baseline(), " + log_incidence_per_100k"
  )
  d_E <- df %>% dplyr::filter(!is.na(.data[[y]]),
                              !is.na(log_incidence_per_100k))
  fE <- lm(as.formula(formula_E), data = d_E)
  cE <- extract_lm_CR2(fE, d_E$location_id) %>%
    dplyr::filter(term == "rw_dex_hiv_prev_ratio_log") %>%
    mutate(panel = panel_label, spec_label = "Contemp incidence (no interaction)",
           outcome = out_lab, n = nobs(fE),
           adj_r2 = summary(fE)$adj.r.squared)
  table2_rows[[paste0(out_lab, "_", panel_label)]] <- cE
  
  ## --- F: Sensitivity, drop incidence ---
  panel_label <- "F_Sens_drop_incidence"
  formula_F <- paste0(y, " ~ rw_dex_hiv_prev_ratio_log + ", rhs_baseline())
  d_F <- df %>% dplyr::filter(!is.na(.data[[y]]))
  fF <- lm(as.formula(formula_F), data = d_F)
  cF <- extract_lm_CR2(fF, d_F$location_id) %>%
    dplyr::filter(term == "rw_dex_hiv_prev_ratio_log") %>%
    mutate(panel = panel_label, spec_label = "Drop incidence (no interaction)",
           outcome = out_lab, n = nobs(fF),
           adj_r2 = summary(fF)$adj.r.squared)
  table2_rows[[paste0(out_lab, "_", panel_label)]] <- cF
  
  ## --- G: Sensitivity, upstream risk-factor replacement ---
  panel_label <- "G_Sens_upstream_risk_factors"
  formula_G <- paste0(y, " ~ rw_dex_hiv_prev_ratio_log + ", rhs_upstream())
  d_G <- df %>%
    dplyr::filter(!is.na(.data[[y]]),
                  !is.na(log_opioid_prev),
                  !is.na(aca_implemented_status),
                  !is.na(edu_yrs),
                  !is.na(log_ldi_pc))
  fG <- lm(as.formula(formula_G), data = d_G)
  cG <- extract_lm_CR2(fG, d_G$location_id) %>%
    dplyr::filter(term == "rw_dex_hiv_prev_ratio_log") %>%
    mutate(panel = panel_label, spec_label = "Upstream risk factors replace incidence",
           outcome = out_lab, n = nobs(fG),
           adj_r2 = summary(fG)$adj.r.squared)
  table2_rows[[paste0(out_lab, "_", panel_label)]] <- cG
  
  ## --- H: Mundlak between/within decomposition ---
  panel_label <- "H_Mundlak_B_W"
  formula_H <- paste0(
    y, " ~ rw_dex_hiv_prev_ratio_log_B + rw_dex_hiv_prev_ratio_log_W + ",
    rhs_primary()
  )
  d_H <- df %>%
    dplyr::filter(!is.na(.data[[y]]),
                  !is.na(rw_dex_hiv_prev_ratio_log_B),
                  !is.na(rw_dex_hiv_prev_ratio_log_W),
                  !is.na(log_incidence_per_100k_l1))
  fH <- lm(as.formula(formula_H), data = d_H)
  cH <- extract_lm_CR2(fH, d_H$location_id) %>%
    dplyr::filter(grepl("rw_dex_hiv_prev_ratio_log_[BW]", term)) %>%
    mutate(panel = panel_label, spec_label = "Mundlak B / W decomposition",
           outcome = out_lab, n = nobs(fH),
           adj_r2 = summary(fH)$adj.r.squared)
  table2_rows[[paste0(out_lab, "_", panel_label)]] <- cH
}

table2 <- bind_rows(table2_rows) %>%
  mutate(signif_label = ifelse(is.na(p.value), NA, sig_lab(p.value))) %>%
  select(panel, outcome, spec_label, term, estimate, std.error,
         statistic, df, p.value, signif_label, n, adj_r2,
         F_stat, df_num, df_denom, p_value, test)

write.csv(table2,
          file.path(dir_output, "table2_burden_panels_A_to_H.csv"),
          row.names = FALSE)
log_msg("Wrote Table 2 (burden lens, 8 panels x 3 outcomes)")


##================================================================
## 6.  TABLE 3  —  CASCADE REGRESSIONS
##================================================================
## Primary column:  per-capita X (log_spending_per_capita_l1)
## Sensitivity:     per-case X  (rw_dex_hiv_prev_ratio_log_l1)
## Outcomes: K, L, V, KxV  (R dropped)
## Family:  quasibinomial(logit)
## Report:  log-odds coefficients + AMEs
##================================================================

cascade_outcomes <- list(
  K   = "cdc_knowledge_status",
  L   = "cdc_linkage_1mo",
  V   = "cdc_viral_suppress",
  KxV = "cdc_kv_composite"
)

predictors <- list(
  per_capita = "log_spending_per_capita_l1",
  per_case   = "rw_dex_hiv_prev_ratio_log_l1"
)

cascade_results  <- list()
cascade_ame_rows <- list()

for (predictor_label in names(predictors)) {
  xvar <- predictors[[predictor_label]]
  for (outcome_label in names(cascade_outcomes)) {
    yvar <- cascade_outcomes[[outcome_label]]
    d <- df %>%
      dplyr::filter(!is.na(.data[[yvar]]), !is.na(.data[[xvar]]),
                    !is.na(log_prevalence_per_100k_l1)) %>%
      mutate(.outcome = pmin(pmax(.data[[yvar]], 1e-6), 1 - 1e-6))
    if (nrow(d) < 100) {
      log_msg(sprintf("WARNING: cascade %s ~ %s has only n = %d, skipping",
                      outcome_label, predictor_label, nrow(d)))
      next
    }
    f <- as.formula(paste0(
      ".outcome ~ ", xvar,
      " + log_prevalence_per_100k_l1 + ", rhs_baseline()
    ))
    fit <- glm(f, data = d, family = quasibinomial(link = "logit"))
    coefs <- extract_glm_CL(fit, cluster_var = d$location_id) %>%
      dplyr::filter(term == xvar) %>%
      mutate(
        outcome   = outcome_label,
        predictor = predictor_label,
        scale     = "log_odds",
        n         = sum(!is.na(d$.outcome)),
        signif    = sig_lab(p.value)
      )
    cascade_results[[paste(predictor_label, outcome_label, sep = "_")]] <- coefs
    
    ## Average marginal effect (percentage-point change per unit log change in X)
    ame_row <- tryCatch({
      s <- marginaleffects::avg_slopes(
        fit, variables = xvar,
        vcov = vcovCL(fit, cluster = ~ location_id, type = "HC0")
      )
      tibble(
        outcome   = outcome_label,
        predictor = predictor_label,
        scale     = "AME_percentage_points",
        term      = xvar,
        estimate  = as.numeric(s$estimate[1]),
        std.error = as.numeric(s$std.error[1]),
        p.value   = as.numeric(s$p.value[1]),
        n         = sum(!is.na(d$.outcome))
      )
    }, error = function(e) {
      message("AME failed for ", outcome_label, " (", predictor_label, "): ", e$message)
      tibble(outcome = outcome_label, predictor = predictor_label,
             scale = "AME_percentage_points", term = xvar,
             estimate = NA, std.error = NA, p.value = NA, n = NA)
    })
    cascade_ame_rows[[paste(predictor_label, outcome_label, "ame", sep = "_")]] <- ame_row
  }
}

table3 <- bind_rows(cascade_results) %>%
  bind_rows(bind_rows(cascade_ame_rows)) %>%
  select(predictor, outcome, scale, term, estimate, std.error,
         statistic, p.value, signif_label = signif, n)

write.csv(table3,
          file.path(dir_output, "table3_cascade_regressions.csv"),
          row.names = FALSE)
log_msg("Wrote Table 3 (cascade regressions, per-capita primary + per-case sensitivity)")


##================================================================
## 7.  TABLE 4  —  MEDIATION (cascade chained to burden primary)
##================================================================
##   X = log(spending per prevalent case), lagged t-1
##   M = K x V composite, year t
##   Y = log(DALYs per prevalent case), year t+1
##   Covariates: lagged log incidence + log homeless + race + year FE
##   Bootstrap: state-block, 1000 reps
##================================================================

X_med <- "rw_dex_hiv_prev_ratio_log_l1"
M_med <- "cdc_kv_composite"
Y_med <- "as_daly_prev_ratio_log_f1"
covars_med <- paste(
  "log_incidence_per_100k_l1",
  "log_prop_homeless",
  "race_prop_BLCK", "race_prop_HISP",
  "year_factor",
  sep = " + "
)

fit_one_mediation <- function(d) {
  if (nrow(d) < 50) return(NULL)
  fA <- lm(as.formula(paste(M_med, "~", X_med, "+", covars_med)), data = d)
  fB <- lm(as.formula(paste(Y_med, "~", M_med, "+", X_med, "+", covars_med)), data = d)
  fC <- lm(as.formula(paste(Y_med, "~", X_med, "+", covars_med)), data = d)
  a   <- coef(fA)[X_med]
  b   <- coef(fB)[M_med]
  cpr <- coef(fB)[X_med]
  ctot <- coef(fC)[X_med]
  ind <- as.numeric(a) * as.numeric(b)
  tibble(a = as.numeric(a), b = as.numeric(b),
         c_prime = as.numeric(cpr), c_total = as.numeric(ctot),
         indirect = ind,
         prop_mediated = ind / as.numeric(ctot),
         n_obs = nrow(d))
}

d_med <- df %>%
  dplyr::filter(!is.na(.data[[X_med]]), !is.na(.data[[M_med]]),
                !is.na(.data[[Y_med]]),
                !is.na(log_incidence_per_100k_l1))

med_point <- fit_one_mediation(d_med) %>% mutate(label = "point_estimate")
log_msg(sprintf("Mediation point estimate: a=%.4f, b=%.4f, ab=%.4f (n=%d)",
                med_point$a, med_point$b, med_point$indirect, med_point$n_obs))

## State-block bootstrap
n_boot <- 1000
state_ids <- unique(d_med$location_id)
set.seed(20260518)
boot_indirect <- numeric(n_boot)
boot_a <- numeric(n_boot); boot_b <- numeric(n_boot)
n_failed <- 0
for (i in seq_len(n_boot)) {
  samp_ids <- sample(state_ids, length(state_ids), replace = TRUE)
  d_boot <- bind_rows(lapply(samp_ids, function(s) d_med[d_med$location_id == s, ]))
  ## Avoid duplicate-state collinearity in year FE: re-id sampled states
  d_boot$location_id <- as.integer(factor(paste(d_boot$location_id,
                                                ave(seq_along(samp_ids),
                                                    samp_ids,
                                                    FUN = seq_along))))
  res <- tryCatch(fit_one_mediation(d_boot),
                  error = function(e) { n_failed <<- n_failed + 1; NULL })
  if (is.null(res)) { boot_indirect[i] <- NA; next }
  boot_indirect[i] <- res$indirect
  boot_a[i] <- res$a
  boot_b[i] <- res$b
}
boot_indirect <- boot_indirect[!is.na(boot_indirect)]
log_msg(sprintf("Mediation bootstrap: %d successful reps (%d failed)",
                length(boot_indirect), n_failed))

table4 <- tibble(
  X = X_med, M = M_med, Y = Y_med,
  a = med_point$a, b = med_point$b,
  c_prime = med_point$c_prime, c_total = med_point$c_total,
  indirect_point = med_point$indirect,
  indirect_lower_95 = quantile(boot_indirect, 0.025, na.rm = TRUE),
  indirect_upper_95 = quantile(boot_indirect, 0.975, na.rm = TRUE),
  prop_mediated_point = med_point$prop_mediated,
  n_obs = med_point$n_obs,
  n_boot_successful = length(boot_indirect),
  n_boot_failed = n_failed
)
write.csv(table4,
          file.path(dir_output, "table4_mediation.csv"),
          row.names = FALSE)
log_msg("Wrote Table 4 (mediation, state-block bootstrap 1000 reps)")


##================================================================
## 8.  TABLE S1  —  PER-CAPITA REDUCED FORM (methodological exhibit)
##================================================================
formula_S1 <- paste0(
  "log_daly_per_capita ~ log_spending_per_capita + log_incidence_per_100k_l1 + ",
  rhs_baseline()
)
d_S1 <- df %>% dplyr::filter(!is.na(log_daly_per_capita),
                             !is.na(log_spending_per_capita),
                             !is.na(log_incidence_per_100k_l1))
fS1 <- lm(as.formula(formula_S1), data = d_S1)
cS1 <- extract_lm_CR2(fS1, d_S1$location_id) %>%
  dplyr::filter(term == "log_spending_per_capita") %>%
  mutate(spec_label = "Per-capita reduced form (methodological exhibit)",
         n = nobs(fS1), adj_r2 = summary(fS1)$adj.r.squared,
         signif_label = sig_lab(p.value))

## Variance decomposition (key methodological number)
state_means <- df %>%
  group_by(location_id) %>%
  summarise(
    spending_mean = mean(log_spending_per_capita, na.rm = TRUE),
    daly_mean     = mean(log_daly_per_capita,     na.rm = TRUE),
    .groups = "drop"
  )
var_decomp <- tibble(
  variable = c("log_spending_per_capita", "log_daly_per_capita"),
  total_var = c(var(df$log_spending_per_capita, na.rm = TRUE),
                var(df$log_daly_per_capita, na.rm = TRUE)),
  between_var = c(var(state_means$spending_mean, na.rm = TRUE),
                  var(state_means$daly_mean, na.rm = TRUE))
) %>%
  mutate(within_var = total_var - between_var,
         between_pct = round(between_var / total_var * 100, 1),
         within_pct  = round(within_var  / total_var * 100, 1))

write.csv(cS1, file.path(dir_output, "table_S1_per_capita_reduced_form.csv"),
          row.names = FALSE)
write.csv(var_decomp,
          file.path(dir_output, "table_S1b_per_capita_variance_decomposition.csv"),
          row.names = FALSE)
log_msg("Wrote Table S1 (per-capita reduced form + variance decomposition)")


##================================================================
## 9.  TABLE S2  —  EXPLORATORY PREVENTION LENS (incidence outcome)
##================================================================
formula_S2 <- paste0(
  "log_incidence_per_100k ~ log_spending_per_capita_l1 + log_prevalence_per_100k_l1 + ",
  rhs_baseline()
)
d_S2 <- df %>% dplyr::filter(!is.na(log_incidence_per_100k),
                             !is.na(log_spending_per_capita_l1),
                             !is.na(log_prevalence_per_100k_l1))
fS2 <- lm(as.formula(formula_S2), data = d_S2)
cS2 <- extract_lm_CR2(fS2, d_S2$location_id) %>%
  dplyr::filter(term == "log_spending_per_capita_l1") %>%
  mutate(spec_label = "Prevention lens (incidence outcome)",
         n = nobs(fS2), adj_r2 = summary(fS2)$adj.r.squared,
         signif_label = sig_lab(p.value))
write.csv(cS2, file.path(dir_output, "table_S2_prevention_lens.csv"),
          row.names = FALSE)
log_msg("Wrote Table S2 (prevention lens, incidence outcome)")


##================================================================
## 10. TABLE S3  —  LEAVE-ONE-STATE-OUT (burden Panel B)
##================================================================
state_list <- unique(df$location_id)
state_names <- df %>% group_by(location_id) %>%
  summarise(location_name = first(location_name), .groups = "drop")
loo_rows <- list()
for (sid in state_list) {
  d_loo <- df %>%
    dplyr::filter(location_id != sid,
                  !is.na(as_daly_prev_ratio_log),
                  !is.na(log_incidence_per_100k_l1))
  f_loo <- lm(as.formula(paste(
    "as_daly_prev_ratio_log ~ rw_dex_hiv_prev_ratio_log * prev_tercile_2010 +",
    rhs_primary())),
    data = d_loo)
  cf <- coef(f_loo)
  loo_rows[[as.character(sid)]] <- tibble(
    excluded_location_id = sid,
    excluded_location_name = state_names$location_name[
      state_names$location_id == sid],
    beta_main = cf["rw_dex_hiv_prev_ratio_log"],
    beta_int_Mid  = cf["rw_dex_hiv_prev_ratio_log:prev_tercile_2010Mid"],
    beta_int_High = cf["rw_dex_hiv_prev_ratio_log:prev_tercile_2010High"]
  )
}
table_S3 <- bind_rows(loo_rows) %>%
  mutate(
    beta_Low  = beta_main,
    beta_Mid  = beta_main + ifelse(is.na(beta_int_Mid),  0, beta_int_Mid),
    beta_High = beta_main + ifelse(is.na(beta_int_High), 0, beta_int_High)
  )
write.csv(table_S3, file.path(dir_output, "table_S3_leave_one_state_out.csv"),
          row.names = FALSE)
log_msg(sprintf("Wrote Table S3 (LOO, %d states): beta_Low range [%.3f, %.3f]",
                nrow(table_S3),
                min(table_S3$beta_Low, na.rm = TRUE),
                max(table_S3$beta_Low, na.rm = TRUE)))


##================================================================
## 11. TABLE S4  —  DROP 2010-2012 (early cascade-reporting years)
##================================================================
d_S4 <- df %>% dplyr::filter(year_id >= 2013,
                             !is.na(as_daly_prev_ratio_log),
                             !is.na(log_incidence_per_100k_l1))
formula_S4 <- paste(
  "as_daly_prev_ratio_log ~ rw_dex_hiv_prev_ratio_log * prev_tercile_2010 +",
  rhs_primary()
)
fS4 <- lm(as.formula(formula_S4), data = d_S4)
cS4 <- extract_lm_CR2(fS4, d_S4$location_id) %>%
  dplyr::filter(grepl("rw_dex_hiv_prev_ratio_log|prev_tercile_2010", term)) %>%
  mutate(spec_label = "Drop 2010-2012",
         n = nobs(fS4), adj_r2 = summary(fS4)$adj.r.squared,
         signif_label = sig_lab(p.value))
write.csv(cS4, file.path(dir_output, "table_S4_drop_early_years.csv"),
          row.names = FALSE)
log_msg(sprintf("Wrote Table S4 (drop 2010-2012, n=%d)", nrow(d_S4)))


##================================================================
## 12. TABLE S5  —  MEDICAID EXPANSION STRATIFICATION
##================================================================
## aca_implemented_status is a state-year binary indicator.
## Stratify the burden Panel C specification by ACA status.
d_S5 <- df %>% dplyr::filter(!is.na(as_daly_prev_ratio_log),
                             !is.na(log_incidence_per_100k_l1),
                             !is.na(aca_implemented_status))
formula_S5 <- paste(
  "as_daly_prev_ratio_log ~ rw_dex_hiv_prev_ratio_log * aca_implemented_status +",
  rhs_primary()
)
fS5 <- lm(as.formula(formula_S5), data = d_S5)
cS5 <- extract_lm_CR2(fS5, d_S5$location_id) %>%
  dplyr::filter(grepl("rw_dex_hiv_prev_ratio_log|aca", term)) %>%
  mutate(spec_label = "Medicaid expansion interaction",
         n = nobs(fS5), adj_r2 = summary(fS5)$adj.r.squared,
         signif_label = sig_lab(p.value))
write.csv(cS5, file.path(dir_output, "table_S5_medicaid_expansion.csv"),
          row.names = FALSE)
log_msg("Wrote Table S5 (Medicaid expansion stratification)")


##================================================================
## 13. TABLE S6  —  RW-EXCLUSIVE SPENDING SENSITIVITY
##================================================================
## RW-inclusive (the variable used throughout this script) is:
##    rw_dex_hiv_prev_ratio = (spend_all + ryan_white_funding_final)
##                            / hiv_prevalence_counts
## where `spend_all` is DEX-only and `ryan_white_funding_final` is
## the separate Ryan White program funding stream.
##
## RW-EXCLUSIVE per-case spending is therefore simply
##    spend_all / hiv_prevalence_counts
## (no subtraction or back-out arithmetic needed).
df_S6 <- df %>%
  mutate(
    spend_excl_rw_per_case = ifelse(
      !is.na(spend_all) & !is.na(hiv_prevalence_counts) &
        spend_all > 0 & hiv_prevalence_counts > 0,
      spend_all / hiv_prevalence_counts,
      NA_real_
    ),
    log_spend_excl_rw_per_case = ifelse(spend_excl_rw_per_case > 0,
                                        log(spend_excl_rw_per_case),
                                        NA_real_)
  )
formula_S6 <- paste(
  "as_daly_prev_ratio_log ~ log_spend_excl_rw_per_case * prev_tercile_2010 +",
  rhs_primary()
)
d_S6 <- df_S6 %>% dplyr::filter(!is.na(as_daly_prev_ratio_log),
                                !is.na(log_spend_excl_rw_per_case),
                                !is.na(log_incidence_per_100k_l1))
fS6 <- lm(as.formula(formula_S6), data = d_S6)
cS6 <- extract_lm_CR2(fS6, d_S6$location_id) %>%
  dplyr::filter(grepl("log_spend_excl_rw_per_case|prev_tercile_2010", term)) %>%
  mutate(spec_label = "RW-exclusive (comparability anchor)",
         n = nobs(fS6), adj_r2 = summary(fS6)$adj.r.squared,
         signif_label = sig_lab(p.value))
write.csv(cS6, file.path(dir_output, "table_S6_RW_exclusive.csv"),
          row.names = FALSE)
log_msg(sprintf("Wrote Table S6 (RW-exclusive, n=%d)", nrow(d_S6)))


##================================================================
## 14. EXHIBIT 1  —  STRATIFIED BIVARIATE SCATTER (3 x 3 grid)
##================================================================
## Rows  = 2010-baseline prevalence tercile (Low, Mid, High)
## Cols  = outcome (DALYs/case GBD; mortality/case GBD; CDC mort/case 25+)
## X axis = log(spending per prevalent case)
## Each panel: log-log scatter + linear bivariate fit + 95% CI band
##================================================================
scatter_panel_data <- bind_rows(
  df %>% transmute(
    location_name, year_id,
    tercile = prev_tercile_2010,
    log_x   = rw_dex_hiv_prev_ratio_log,
    log_y   = as_daly_prev_ratio_log,
    outcome = "A: DALYs per prevalent case (GBD)"
  ),
  df %>% transmute(
    location_name, year_id,
    tercile = prev_tercile_2010,
    log_x   = rw_dex_hiv_prev_ratio_log,
    log_y   = as_mort_prev_ratio_log,
    outcome = "B: Mortality per prevalent case (GBD)"
  ),
  df %>% transmute(
    location_name, year_id,
    tercile = prev_tercile_2010,
    log_x   = rw_dex_hiv_prev_ratio_log,
    log_y   = cdc_mort_prev_ratio_log,
    outcome = "C: Mortality per CDC prevalent case (ages 25+)"
  )
) %>% dplyr::filter(!is.na(log_x), !is.na(log_y))

## Pearson r per facet
r_labels <- scatter_panel_data %>%
  group_by(tercile, outcome) %>%
  summarise(r = cor(log_x, log_y, use = "complete.obs"),
            n = dplyr::n(),
            .groups = "drop") %>%
  mutate(label = sprintf("r = %+.2f (n = %d)", r, n))

p_exhibit1 <- ggplot(scatter_panel_data, aes(x = log_x, y = log_y)) +
  geom_point(alpha = 0.35, size = 0.7) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
              colour = "navy", fill = "navy", alpha = 0.18) +
  geom_text(data = r_labels, aes(x = -Inf, y = Inf, label = label),
            hjust = -0.05, vjust = 1.4, size = 3, inherit.aes = FALSE) +
  facet_grid(tercile ~ outcome, scales = "free", switch = "y") +
  labs(
    title    = "Bivariate association between HIV spending per prevalent case and outcomes, by 2010-baseline prevalence tercile",
    subtitle = "Each point is one state-year (2010-2019). Tercile assignment is fixed at 2010 baseline.",
    x        = "log HIV spending per prevalent case",
    y        = "log outcome per prevalent case"
  ) +
  theme_minimal(base_size = 10) +
  theme(strip.placement = "outside",
        strip.text = element_text(face = "bold"),
        panel.spacing = unit(0.5, "lines"))

ggsave(file.path(dir_output, "exhibit_1_stratified_bivariate.png"),
       p_exhibit1, width = 11, height = 9, dpi = 220)
log_msg("Wrote Exhibit 1 (stratified bivariate scatter)")


##================================================================
## 15. EXHIBIT S1  —  UNSTRATIFIED BIVARIATE SCATTER (3 panels)
##================================================================
p_exhibit_S1 <- ggplot(scatter_panel_data, aes(x = log_x, y = log_y)) +
  geom_point(alpha = 0.35, size = 0.7) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
              colour = "navy", fill = "navy", alpha = 0.18) +
  facet_wrap(~ outcome, scales = "free", nrow = 1) +
  labs(
    title    = "Bivariate association between HIV spending per prevalent case and outcomes (unstratified)",
    subtitle = "Each point is one state-year, 2010-2019.",
    x        = "log HIV spending per prevalent case",
    y        = "log outcome per prevalent case"
  ) +
  theme_minimal(base_size = 10) +
  theme(strip.text = element_text(face = "bold"))

ggsave(file.path(dir_output, "exhibit_S1_unstratified_bivariate.png"),
       p_exhibit_S1, width = 12, height = 4.2, dpi = 220)
log_msg("Wrote Exhibit S1 (unstratified bivariate scatter)")


##================================================================
## 16. END-OF-RUN SUMMARY
##================================================================
log_msg("\n================ RUN COMPLETE ================")
log_msg("Outputs in: ", dir_output)
log_msg("Tables: 2 (burden), 3 (cascade), 4 (mediation), S1-S6")
log_msg("Exhibits: 1 (stratified bivariate), S1 (unstratified bivariate)")
log_msg("Next step: assemble manuscript tables from these CSVs;",
        " inspect run_log.txt for any warnings.")

##================================================================
## END
##================================================================