##---------------------------------------------------------------
##  C_02_HIV_regressions_FINAL.R
##
##  Buildup regression analysis: K × V (cascade) and incidence outcomes
##  across multiple covariate specifications. Output as ONE long-format
##  CSV for committee review.
##
##  Per Marcia/Joe May 28 feedback:
##    - Show how β_spend evolves as covariates are added (buildup table)
##    - Include with-prev and without-prev variants
##    - Per-case AND per-capita spending versions for K × V
##    - Use lagged prevalence (primary candidate) AND contemp prev (sensitivity)
##
##  Families (5):           kxv_per_case, kxv_per_capita,
##                          k_only_per_case, k_only_per_capita,
##                          incidence_per_capita
##  Buildup steps (5):      01_bivariate → 02_race → 03_homeless
##                          → 04_lagPrev → 05_contempPrev
##                          (incidence reorders 04/05 — contemp is primary)
##  Total models:           25
##
##  K-only families serve as a SENSITIVITY: K (knowledge of HIV status) is
##  reported by CDC ATLAS for all 51 jurisdictions × 10 years (n = 510),
##  whereas K × V has CDC ATLAS suppression in V for 6 small-population
##  states (n = 347). Including K-only addresses Marcia's missingness
##  question by showing whether conclusions hold on the FULL sample.
##
##  All models include year fixed effects (clustered on state).
##  Year FE coefficients are NOT shown in the output rows; a metadata
##  column flags that year FE is always included.
##
##  Cluster-robust SEs: HC1 with cadjust = TRUE (sandwich::vcovCL),
##  clustered on location_id. Matches the memo's primary regression
##  exactly.
##
##  Output: ONE CSV
##    regression_buildup_KxV_incidence.csv
##---------------------------------------------------------------

rm(list = ls())

##================================================================
## 1.  SETUP & PATHS  (matches your existing scripts)
##================================================================
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

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(
  data.table, tidyverse, glue, broom,
  lmtest, sandwich, conflicted
)

# ---------- resolve filter / lag conflicts ----------
suppressMessages({
  conflicted::conflicts_prefer(dplyr::filter, .quiet = TRUE)
  conflicted::conflicts_prefer(dplyr::lag,    .quiet = TRUE)
})

# ---------- input/output paths ----------
input_date  <- "20260517"
panel_input_path <- file.path(
  h, "aim_outputs/Aim2/C_frontier_analysis",
  input_date, "analysis_per_capita", "df_hiv_cascade_panel.csv"
)

output_date <- format(Sys.time(), "%Y%m%d_%H%M")
dir_output  <- file.path(
  h, "aim_outputs/Aim2/C_frontier_analysis",
  output_date, "regression_buildup_KxV_incidence"
)
if (!dir.exists(dir_output)) dir.create(dir_output, recursive = TRUE)

cat("Input panel : ", panel_input_path, "\n")
cat("Output dir  : ", dir_output,        "\n\n")

##================================================================
## 2.  LOAD PANEL & FILTER TO HIV
##================================================================
if (!file.exists(panel_input_path)) {
  stop("Input panel not found: ", panel_input_path)
}

df <- read.csv(panel_input_path, stringsAsFactors = FALSE) %>%
  dplyr::filter(acause == "hiv")
cat("Loaded ", nrow(df), " state-year rows (HIV)\n\n", sep = "")

##================================================================
## 3.  BUILD VARIABLES FROM RAW  (matches v5 Path B frontier)
##================================================================

# Cascade column names (CDC ATLAS)
COL_K <- "cdc_knowledge_status"
COL_V <- "cdc_viral_suppress"
LOGIT_EPS <- 1e-4

for (col in c(COL_K, COL_V)) {
  if (!(col %in% names(df))) {
    stop(sprintf("Column '%s' not found in panel.", col))
  }
}

# ---- Spending (with Ryan White) ----
df$hiv_spend_total      <- df$spend_all +
  ifelse(is.na(df$ryan_white_funding_final), 0, df$ryan_white_funding_final)
df$spend_per_case       <- df$hiv_spend_total / df$hiv_prevalence_counts
df$spend_per_capita     <- df$hiv_spend_total / df$population
df$log_spend_per_case   <- log(df$spend_per_case)
df$log_spend_per_capita <- log(df$spend_per_capita)

# ---- K × V outcome (logit-transformed) ----
df$K_raw     <- pmin(pmax(df[[COL_K]], 0), 1)
df$V_raw     <- pmin(pmax(df[[COL_V]], 0), 1)
df$kxv_raw   <- df$K_raw * df$V_raw
df$kxv_clip  <- pmin(pmax(df$kxv_raw, LOGIT_EPS), 1 - LOGIT_EPS)
df$logit_kxv <- log(df$kxv_clip / (1 - df$kxv_clip))

# ---- K-only outcome (logit-transformed) — sensitivity for missingness ----
# CDC ATLAS reports K (knowledge of status) for all 51 jurisdictions × 10y.
# V (viral suppression) is the column with suppression. So K-only models
# use the FULL sample (n = 510 / 459-with-lag).
df$K_clip    <- pmin(pmax(df$K_raw, LOGIT_EPS), 1 - LOGIT_EPS)
df$logit_K   <- log(df$K_clip / (1 - df$K_clip))

# ---- Incidence (log per 100k) ----
df$inc_per_100k     <- df$incidence_counts / df$population * 1e5
df$log_inc_per_100k <- log(df$inc_per_100k)

# ---- Prevalence (log per 100k) and FRESH 1-year lag ----
df$prev_per_100k     <- df$hiv_prevalence_counts / df$population * 1e5
df$log_prev_per_100k <- log(df$prev_per_100k)

df <- df %>%
  arrange(location_id, year_id) %>%
  group_by(location_id) %>%
  mutate(log_prev_per_100k_lag1 = dplyr::lag(log_prev_per_100k, 1)) %>%
  ungroup()

# ---- log homelessness ----
if ("prop_homeless" %in% names(df) && !"log_prop_homeless" %in% names(df)) {
  df$log_prop_homeless <- log(df$prop_homeless)
}

# ---- year factor ----
df$year_factor <- factor(df$year_id)

##================================================================
## 4.  HELPER FUNCTIONS
##================================================================

# ---- Cluster-robust SEs (HC1, clustered on state) -- matches memo ----
extract_clustered_HC1 <- function(model, data) {
  vc <- sandwich::vcovCL(model, cluster = data$location_id,
                         type = "HC1", cadjust = TRUE)
  ct <- lmtest::coeftest(model, vcov. = vc)
  df_out <- as.data.frame(unclass(ct))
  colnames(df_out) <- c("estimate", "std.error", "statistic", "p.value")
  df_out$term <- rownames(df_out)
  rownames(df_out) <- NULL
  df_out[, c("term", "estimate", "std.error", "statistic", "p.value")]
}

# ---- Significance markers ----
sig_stars <- function(p) {
  ifelse(is.na(p),  "",
         ifelse(p < 0.001, "***",
                ifelse(p < 0.01,  "**",
                       ifelse(p < 0.05,  "*",
                              ifelse(p < 0.10,  "†", "")))))
}
##================================================================
## 5.  MODEL REGISTRY  (25 specifications: 5 families × 5 buildup steps)
##================================================================
# Each entry: family, model_name, outcome, predictor_label,
#             prev_spec, equation, rhs (the model formula RHS string)

# ---- Helper to build a 5-spec buildup for a given family ----
# Buildup adds covariates one at a time:
#   01_bivariate    : spend + year FE
#   02_race         : + race_BLCK + race_HISP
#   03_homeless     : + log(% homeless)
#   04_lagPrev      : + log(prev/100k) t-1     <-- primary candidate (K × V)
#   05_contempPrev  : + log(prev/100k) contemp (instead of lag) <-- sensitivity
# For incidence, swap order: 04 = contempPrev (primary), 05 = lagPrev.
build_buildup <- function(family_name, outcome_var, outcome_label,
                          predictor_var, predictor_label,
                          predictor_short,    # for equation strings: "Spend/case" or "Spend/cap"
                          outcome_short,      # "logit(KxV)" / "logit(K)" / "log(incidence/100k)"
                          prev_primary = "lag" # "lag" for K × V / K-only, "contemp" for incidence
) {
  base_eq <- paste0(outcome_short, " ~ log(", predictor_short, ")")
  base_rhs <- predictor_var
  
  # 5 spec definitions in canonical order; 04 / 05 swap based on prev_primary
  specs <- list(
    list(
      model_name = "01_bivariate",
      prev_spec  = "none",
      equation   = paste0(base_eq, " + year_FE"),
      rhs        = paste(base_rhs, "+ year_factor")
    ),
    list(
      model_name = "02_race",
      prev_spec  = "none",
      equation   = paste0(base_eq, " + race_BLCK + race_HISP + year_FE"),
      rhs        = paste(base_rhs, "+ race_prop_BLCK + race_prop_HISP + year_factor")
    ),
    list(
      model_name = "03_homeless",
      prev_spec  = "none",
      equation   = paste0(base_eq, " + race_BLCK + race_HISP + log(% homeless) + year_FE"),
      rhs        = paste(base_rhs, "+ race_prop_BLCK + race_prop_HISP + log_prop_homeless + year_factor")
    ),
    list(
      model_name = "04_lagPrev",
      prev_spec  = "lagged_t-1",
      equation   = paste0(base_eq, " + race_BLCK + race_HISP + log(% homeless) + log(prev/100k)_t-1 + year_FE"),
      rhs        = paste(base_rhs, "+ race_prop_BLCK + race_prop_HISP + log_prop_homeless + log_prev_per_100k_lag1 + year_factor")
    ),
    list(
      model_name = "05_contempPrev",
      prev_spec  = "contemp",
      equation   = paste0(base_eq, " + race_BLCK + race_HISP + log(% homeless) + log(prev/100k)_t + year_FE"),
      rhs        = paste(base_rhs, "+ race_prop_BLCK + race_prop_HISP + log_prop_homeless + log_prev_per_100k + year_factor")
    )
  )
  
  # For incidence: contemp is primary, so swap order of 04 and 05
  if (prev_primary == "contemp") {
    specs[[4]]$model_name <- "04_contempPrev"
    specs[[4]]$prev_spec  <- "contemp"
    specs[[4]]$equation   <- paste0(base_eq, " + race_BLCK + race_HISP + log(% homeless) + log(prev/100k)_t + year_FE")
    specs[[4]]$rhs        <- paste(base_rhs, "+ race_prop_BLCK + race_prop_HISP + log_prop_homeless + log_prev_per_100k + year_factor")
    
    specs[[5]]$model_name <- "05_lagPrev"
    specs[[5]]$prev_spec  <- "lagged_t-1"
    specs[[5]]$equation   <- paste0(base_eq, " + race_BLCK + race_HISP + log(% homeless) + log(prev/100k)_t-1 + year_FE")
    specs[[5]]$rhs        <- paste(base_rhs, "+ race_prop_BLCK + race_prop_HISP + log_prop_homeless + log_prev_per_100k_lag1 + year_factor")
  }
  
  # Attach common metadata
  lapply(specs, function(s) {
    s$family            <- family_name
    s$outcome           <- outcome_label
    s$predictor         <- predictor_label
    s$outcome_var       <- outcome_var
    s
  })
}

model_specs <- c(
  ## FAMILY 1: K × V on Spend per CASE
  build_buildup("kxv_per_case", "logit_kxv", "logit(K × V)",
                "log_spend_per_case", "log(Spend per case)",
                "Spend/case", "logit(KxV)", prev_primary = "lag"),
  
  ## FAMILY 2: K × V on Spend per CAPITA  (Marcia's request)
  build_buildup("kxv_per_capita", "logit_kxv", "logit(K × V)",
                "log_spend_per_capita", "log(Spend per capita)",
                "Spend/cap", "logit(KxV)", prev_primary = "lag"),
  
  ## FAMILY 3: K-only on Spend per CASE — SENSITIVITY (full n = 510)
  ## K alone has full CDC ATLAS coverage; V is the column with suppression.
  ## Addresses Marcia's missingness question directly.
  build_buildup("k_only_per_case", "logit_K", "logit(K)",
                "log_spend_per_case", "log(Spend per case)",
                "Spend/case", "logit(K)", prev_primary = "lag"),
  
  ## FAMILY 4: K-only on Spend per CAPITA — SENSITIVITY
  build_buildup("k_only_per_capita", "logit_K", "logit(K)",
                "log_spend_per_capita", "log(Spend per capita)",
                "Spend/cap", "logit(K)", prev_primary = "lag"),
  
  ## FAMILY 5: log(Incidence/100k) on Spend per CAPITA  (R2b prevention eq.)
  ## Here contemp prev is the primary (per Joe's R2b); lag is sensitivity.
  build_buildup("incidence_per_capita", "log_inc_per_100k", "log(incidence/100k)",
                "log_spend_per_capita", "log(Spend per capita)",
                "Spend/cap", "log(incidence/100k)", prev_primary = "contemp")
)

##================================================================
## 6.  FIT MODELS & ASSEMBLE OUTPUT
##================================================================
all_rows <- list()

cat(strrep("=", 84), "\n", sep = "")
cat("FITTING ", length(model_specs), " MODELS\n", sep = "")
cat(strrep("=", 84), "\n\n", sep = "")

for (spec in model_specs) {
  
  model_id <- paste0("hiv__", spec$family, "__", spec$model_name)
  cat("Fitting: ", model_id, "\n", sep = "")
  
  formula_str <- paste(spec$outcome_var, "~", spec$rhs)
  
  # ---- Build complete-case sample for this model ----
  all_vars <- all.vars(as.formula(formula_str))
  needed_cols <- unique(c(all_vars, "location_id"))
  s <- df[, intersect(needed_cols, names(df))]
  s <- s[complete.cases(s), ]
  
  if (nrow(s) < 20) {
    cat("  Skipped (insufficient observations: ", nrow(s), ")\n", sep = "")
    next
  }
  
  # ---- Fit OLS ----
  fit <- lm(as.formula(formula_str), data = s)
  
  # ---- Cluster-robust SEs ----
  coef_tbl <- extract_clustered_HC1(fit, s)
  
  # ---- Filter out year_factor* rows (year FE not shown) ----
  coef_tbl <- coef_tbl[!grepl("^year_factor", coef_tbl$term), ]
  
  # ---- Add metadata and significance ----
  coef_tbl$model_id          <- model_id
  coef_tbl$acause            <- "hiv"
  coef_tbl$family            <- spec$family
  coef_tbl$model_name        <- spec$model_name
  coef_tbl$equation          <- spec$equation
  coef_tbl$outcome           <- spec$outcome
  coef_tbl$predictor_primary <- spec$predictor
  coef_tbl$prev_spec         <- spec$prev_spec
  coef_tbl$stars             <- sig_stars(coef_tbl$p.value)
  coef_tbl$n                 <- nrow(s)
  coef_tbl$n_states          <- length(unique(s$location_id))
  coef_tbl$r2                <- summary(fit)$r.squared
  coef_tbl$adj_r2            <- summary(fit)$adj.r.squared
  
  all_rows[[model_id]] <- coef_tbl
  
  cat(sprintf("  N = %d, states = %d, adj R^2 = %.3f\n",
              nrow(s), length(unique(s$location_id)),
              summary(fit)$adj.r.squared))
}

##================================================================
## 7.  ASSEMBLE FINAL TABLE
##================================================================
results <- bind_rows(all_rows)

# Final column order per committee feedback (May 28):
#   - equation right after model_id
#   - model_name before outcome
#   - drop statistic (redundant with p.value)
#   - drop signif_label (redundant with stars)
#   - acause moved to the end
results <- results %>%
  select(
    model_id, equation,
    family, model_name,
    outcome, predictor_primary,
    term,
    estimate, stars,
    std.error, p.value,
    n, n_states,
    r2, adj_r2,
    acause
  )

# Round numeric columns
results <- results %>%
  mutate(
    estimate  = round(estimate, 4),
    std.error = round(std.error, 4),
    p.value   = round(p.value, 4),
    r2        = round(r2, 4),
    adj_r2    = round(adj_r2, 4)
  )

##================================================================
## 8.  SAVE
##================================================================
out_csv <- file.path(dir_output, "regression_buildup_KxV_incidence.csv")
write.csv(results, out_csv, row.names = FALSE, na = "")

cat("\n", strrep("=", 84), "\n", sep = "")
cat("DONE\n")
cat(strrep("=", 84), "\n", sep = "")
cat("Saved : ", out_csv, "\n", sep = "")
cat("Rows  : ", nrow(results), " (year FE coefficients filtered out)\n", sep = "")
cat("Models: ", length(unique(results$model_id)), "\n", sep = "")
cat("\nMetadata: All models include year fixed effects (year_factor terms)\n")
cat("clustered-robust SEs (HC1, cadjust=TRUE) on state.\n")
cat("Year FE coefficients are NOT shown in the rows.\n\n")