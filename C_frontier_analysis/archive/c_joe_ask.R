##----------------------------------------------------------------
##  Title:    verify_joe_regressions.R
##  Purpose:  Replicate the six regressions in
##              presentation/joe_three_regressions_results.docx
##              presentation/joe_three_regressions_results.txt
##            against the cascade panel, so the numbers can be
##            independently verified outside Python/statsmodels.
##
##  Specifications:
##    R1  log(DALY/pop)     ~ logS_c * logP_c + controls + year FE
##    R2a log(inc/100k)     ~ log(Spend/cap)             + controls + year FE   (no prev)
##    R2b log(inc/100k)     ~ log(Spend/cap) + log(Prev/100k) + controls + year FE
##    R3a logit(K × V)      ~ log(Spend/case)            + controls + year FE
##    R3b logit(K × L × V)  ~ log(Spend/case)            + controls + year FE
##    R3c logit(V)          ~ log(Spend/case)            + controls + year FE   (Emily simplification)
##
##  All models use cluster-robust standard errors on state
##  (sandwich::vcovCL, HC1 + cluster small-sample correction). Point
##  estimates match statsmodels exactly; SEs match to ~3 decimals
##  (small-sample correction differs by a factor of (n-1)/n ≈ 1.0).
##
##  Output:
##    1. Full coefficient tables printed to console
##    2. headline_coefficients.csv saved in the run output folder
##    3. full_coefficients.csv with every term in every model
##----------------------------------------------------------------
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

dir_output <- file.path(
  h, "aim_outputs/Aim2/C_frontier_analysis",
  format(Sys.time(), "%Y%m%d_%H%M"),
  "verify_joe_regressions"
)
if (!dir.exists(dir_output)) dir.create(dir_output, recursive = TRUE)

cat("Input panel: ", panel_input_path, "\n")
cat("Output dir:  ", dir_output,        "\n\n")

## ---- libraries -------------------------------------------------
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(dplyr, sandwich, lmtest)

## ================================================================
## 1. LOAD PANEL
## ================================================================
if (!file.exists(panel_input_path)) {
  stop("Input panel not found: ", panel_input_path)
}
df <- read.csv(panel_input_path, stringsAsFactors = FALSE)
df <- df %>% filter(acause == "hiv")
cat("Loaded ", nrow(df), " rows (acause == \"hiv\")\n\n", sep = "")

## ================================================================
## 2. BUILD VARIABLES  (mirrors the Python script exactly)
## ================================================================
df$hiv_spend_total  <- df$spend_all +
  ifelse(is.na(df$ryan_white_funding_final), 0, df$ryan_white_funding_final)

df$spend_pc        <- df$hiv_spend_total / df$population
df$spend_per_case  <- df$hiv_spend_total / df$hiv_prevalence_counts
df$daly_pc         <- df$daly_counts / df$population
df$inc_per_100k    <- df$incidence_counts  / df$population * 1e5
df$prev_per_100k   <- df$hiv_prevalence_counts / df$population * 1e5

df$log_spend_pc       <- log(df$spend_pc)
df$log_spend_per_case <- log(df$spend_per_case)
df$log_daly_pc        <- log(df$daly_pc)
df$log_inc_per_100k   <- log(df$inc_per_100k)
df$log_prev_per_100k  <- log(df$prev_per_100k)

## log_prop_homeless: prefer the raw prop_homeless if available
## so the value is computed identically to the Python script.
if ("prop_homeless" %in% names(df)) {
  df$log_prop_homeless <- log(df$prop_homeless)
} else if (!("log_prop_homeless" %in% names(df))) {
  stop("Need either prop_homeless or log_prop_homeless in the panel.")
}

## Cascade composites
in_unit <- function(x) !is.na(x) & x >= 0 & x <= 1
df$KV     <- ifelse(in_unit(df$cdc_knowledge_status) & in_unit(df$cdc_viral_suppress),
                    df$cdc_knowledge_status * df$cdc_viral_suppress, NA_real_)
df$KLV    <- ifelse(in_unit(df$cdc_knowledge_status) & in_unit(df$cdc_linkage_1mo) &
                      in_unit(df$cdc_viral_suppress),
                    df$cdc_knowledge_status * df$cdc_linkage_1mo * df$cdc_viral_suppress,
                    NA_real_)
df$V_only <- ifelse(in_unit(df$cdc_viral_suppress), df$cdc_viral_suppress, NA_real_)

LOGIT_EPS <- 1e-4
clip01 <- function(x) pmin(pmax(x, LOGIT_EPS), 1 - LOGIT_EPS)
df$logit_KV  <- log(clip01(df$KV)     / (1 - clip01(df$KV)))
df$logit_KLV <- log(clip01(df$KLV)    / (1 - clip01(df$KLV)))
df$logit_V   <- log(clip01(df$V_only) / (1 - clip01(df$V_only)))

## Mean-centered S and P for R1 (sample means on the full panel)
S_mean <- mean(df$log_spend_pc,      na.rm = TRUE)
P_mean <- mean(df$log_prev_per_100k, na.rm = TRUE)
df$logS_c <- df$log_spend_pc      - S_mean
df$logP_c <- df$log_prev_per_100k - P_mean

df$year_factor <- factor(df$year_id)

cat("Centering means:  S_mean = ", round(S_mean, 4),
    "   P_mean = ", round(P_mean, 4), "\n\n", sep = "")

## ================================================================
## 3. CLUSTER-ROBUST SE HELPER
## ================================================================
##  statsmodels OLS with cov_type='cluster' applies (Stata-style):
##      sandwich  ×  G/(G-1)  ×  (n-1)/(n-k)
##  sandwich::vcovCL(..., type='HC1', cadjust=TRUE) applies:
##      sandwich  ×  G/(G-1)  ×  n/(n-k)
##  They differ by a factor of (n-1)/n ≈ 1 (negligible at n=510).
##  Point estimates match exactly. SEs match to ~3 decimal places.
cluster_ct <- function(fit, cluster) {
  vc <- sandwich::vcovCL(fit, cluster = cluster, type = "HC1", cadjust = TRUE)
  ct <- lmtest::coeftest(fit, vcov. = vc)
  as.data.frame(unclass(ct))   # columns: Estimate, Std. Error, t value, Pr(>|t|)
}

## ================================================================
## 4. RUN THE SIX REGRESSIONS
## ================================================================
fits <- list(); samples <- list()

## R1 — Treatment equation
samples$R1 <- df %>%
  select(log_daly_pc, logS_c, logP_c, log_inc_per_100k, log_prop_homeless,
         race_prop_BLCK, race_prop_HISP, year_factor, location_id) %>%
  na.omit()
fits$R1 <- lm(log_daly_pc ~ logS_c * logP_c + log_inc_per_100k + log_prop_homeless
              + race_prop_BLCK + race_prop_HISP + year_factor,
              data = samples$R1)

## R2a — incidence, no prevalence on RHS
samples$R2a <- df %>%
  select(log_inc_per_100k, log_spend_pc, log_prop_homeless,
         race_prop_BLCK, race_prop_HISP, year_factor, location_id) %>%
  na.omit()
fits$R2a <- lm(log_inc_per_100k ~ log_spend_pc + log_prop_homeless
               + race_prop_BLCK + race_prop_HISP + year_factor,
               data = samples$R2a)

## R2b — incidence, with prevalence on RHS
samples$R2b <- df %>%
  select(log_inc_per_100k, log_spend_pc, log_prev_per_100k, log_prop_homeless,
         race_prop_BLCK, race_prop_HISP, year_factor, location_id) %>%
  na.omit()
fits$R2b <- lm(log_inc_per_100k ~ log_spend_pc + log_prev_per_100k
               + log_prop_homeless + race_prop_BLCK + race_prop_HISP
               + year_factor,
               data = samples$R2b)

## R3a — logit(K × V) on log(Spend/case)
samples$R3a <- df %>%
  select(logit_KV, log_spend_per_case, log_inc_per_100k, log_prop_homeless,
         race_prop_BLCK, race_prop_HISP, year_factor, location_id) %>%
  na.omit()
fits$R3a <- lm(logit_KV ~ log_spend_per_case + log_inc_per_100k
               + log_prop_homeless + race_prop_BLCK + race_prop_HISP
               + year_factor,
               data = samples$R3a)

## R3b — logit(K × L × V) on log(Spend/case)
samples$R3b <- df %>%
  select(logit_KLV, log_spend_per_case, log_inc_per_100k, log_prop_homeless,
         race_prop_BLCK, race_prop_HISP, year_factor, location_id) %>%
  na.omit()
fits$R3b <- lm(logit_KLV ~ log_spend_per_case + log_inc_per_100k
               + log_prop_homeless + race_prop_BLCK + race_prop_HISP
               + year_factor,
               data = samples$R3b)

## R3c — logit(V only) on log(Spend/case)  [Emily simplification]
samples$R3c <- df %>%
  select(logit_V, log_spend_per_case, log_inc_per_100k, log_prop_homeless,
         race_prop_BLCK, race_prop_HISP, year_factor, location_id) %>%
  na.omit()
fits$R3c <- lm(logit_V ~ log_spend_per_case + log_inc_per_100k
               + log_prop_homeless + race_prop_BLCK + race_prop_HISP
               + year_factor,
               data = samples$R3c)

## ================================================================
## 5. PRINT FULL COEFFICIENT TABLES TO CONSOLE
## ================================================================
print_block <- function(label, fit, d) {
  cat(strrep("=", 84), "\n", sep = "")
  cat(label, "\n", sep = "")
  cat(strrep("=", 84), "\n", sep = "")
  cat(sprintf("  N observations : %d\n",  nrow(d)))
  cat(sprintf("  Clusters (state): %d\n", length(unique(d$location_id))))
  cat(sprintf("  Adjusted R^2   : %.4f\n\n", summary(fit)$adj.r.squared))
  ct <- cluster_ct(fit, d$location_id)
  ## drop year-factor rows from the printed table to keep it short
  keep <- !grepl("^year_factor", rownames(ct))
  print(round(ct[keep, ], 4))
  cat("\n")
}

print_block("R1  — log(DALY/pop) ~ logS_c * logP_c + controls + year FE",  fits$R1,  samples$R1)
print_block("R2a — log(inc/100k) ~ log(Spend/cap) + controls + year FE   (no prev)", fits$R2a, samples$R2a)
print_block("R2b — log(inc/100k) ~ log(Spend/cap) + log(Prev/100k) + controls + year FE", fits$R2b, samples$R2b)
print_block("R3a — logit(K × V)     ~ log(Spend/case) + controls + year FE", fits$R3a, samples$R3a)
print_block("R3b — logit(K × L × V) ~ log(Spend/case) + controls + year FE", fits$R3b, samples$R3b)
print_block("R3c — logit(V only)    ~ log(Spend/case) + controls + year FE  [Emily simplification]", fits$R3c, samples$R3c)

## ================================================================
## 6. HEADLINE COEFFICIENT TABLE  (compare directly to the docx/txt)
## ================================================================
sig_lab <- function(p) {
  ifelse(is.na(p),    "NA",
         ifelse(p < 0.001,   "***",
                ifelse(p < 0.01,    "**",
                       ifelse(p < 0.05,    "*",
                              ifelse(p < 0.10,    "†", "")))))
}

extract_row <- function(spec, outcome, predictor, fit, d, term) {
  ct <- cluster_ct(fit, d$location_id)
  if (!(term %in% rownames(ct))) {
    return(data.frame(Spec = spec, Outcome = outcome, Predictor = predictor,
                      Term = term, Beta = NA, SE = NA, t_stat = NA,
                      p_value = NA, Sig = "", N_obs = nrow(d),
                      Clusters = length(unique(d$location_id)),
                      stringsAsFactors = FALSE))
  }
  v <- ct[term, ]
  data.frame(
    Spec      = spec,
    Outcome   = outcome,
    Predictor = predictor,
    Term      = term,
    Beta      = round(as.numeric(v[1]), 4),
    SE        = round(as.numeric(v[2]), 4),
    t_stat    = round(as.numeric(v[3]), 3),
    p_value   = round(as.numeric(v[4]), 4),
    Sig       = sig_lab(as.numeric(v[4])),
    N_obs     = nrow(d),
    Clusters  = length(unique(d$location_id)),
    stringsAsFactors = FALSE
  )
}

headline <- rbind(
  extract_row("R1",  "log(DALY/pop)",          "log(Spend/cap), centered",     fits$R1,  samples$R1,  "logS_c"),
  extract_row("R1",  "log(DALY/pop)",          "log(Prev/100k), centered",     fits$R1,  samples$R1,  "logP_c"),
  extract_row("R1",  "log(DALY/pop)",          "Spend × Prev interaction",     fits$R1,  samples$R1,  "logS_c:logP_c"),
  extract_row("R2a", "log(inc/100k)",          "log(Spend/cap), no prev",      fits$R2a, samples$R2a, "log_spend_pc"),
  extract_row("R2b", "log(inc/100k)",          "log(Spend/cap), with prev",    fits$R2b, samples$R2b, "log_spend_pc"),
  extract_row("R2b", "log(inc/100k)",          "log(Prev/100k)",               fits$R2b, samples$R2b, "log_prev_per_100k"),
  extract_row("R3a", "logit(K × V)",           "log(Spend/case)",              fits$R3a, samples$R3a, "log_spend_per_case"),
  extract_row("R3b", "logit(K × L × V)",       "log(Spend/case)",              fits$R3b, samples$R3b, "log_spend_per_case"),
  extract_row("R3c", "logit(V)",               "log(Spend/case)",              fits$R3c, samples$R3c, "log_spend_per_case")
)

cat(strrep("=", 84), "\n", sep = "")
cat("HEADLINE COEFFICIENTS — compare directly to joe_three_regressions_results.docx\n")
cat(strrep("=", 84), "\n", sep = "")
print(headline, row.names = FALSE)
cat("\n")

## ================================================================
## 7. SAVE CSVs
## ================================================================
out_headline <- file.path(dir_output, "headline_coefficients.csv")
write.csv(headline, out_headline, row.names = FALSE, na = "")
cat("Saved: ", out_headline, "\n", sep = "")

## Full coefficient tables (every term in every model, year FEs included)
make_full <- function(spec, fit, d) {
  ct <- cluster_ct(fit, d$location_id)
  data.frame(
    Spec      = spec,
    Term      = rownames(ct),
    Beta      = round(ct[, 1], 4),
    SE        = round(ct[, 2], 4),
    t_stat    = round(ct[, 3], 3),
    p_value   = round(ct[, 4], 4),
    Sig       = sig_lab(ct[, 4]),
    N_obs     = nrow(d),
    Clusters  = length(unique(d$location_id)),
    stringsAsFactors = FALSE
  )
}
full <- rbind(
  make_full("R1",  fits$R1,  samples$R1),
  make_full("R2a", fits$R2a, samples$R2a),
  make_full("R2b", fits$R2b, samples$R2b),
  make_full("R3a", fits$R3a, samples$R3a),
  make_full("R3b", fits$R3b, samples$R3b),
  make_full("R3c", fits$R3c, samples$R3c)
)
out_full <- file.path(dir_output, "full_coefficients.csv")
write.csv(full, out_full, row.names = FALSE, na = "")
cat("Saved: ", out_full, "\n", sep = "")

## ================================================================
## END OF SCRIPT
## ================================================================
cat("\nDone.\n")
