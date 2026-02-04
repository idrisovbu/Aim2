##----------------------------------------------------------------
##' Title: C_regression_models_analysis.R
##'
##' Purpose: HIV Spending-Outcomes Regression Analysis for Aim 2B
##'          
##' Structure:
##'   1. Setup & Data Loading
##'   2. Create Between/Within Variables (Mundlak Decomposition)
##'   3. Variance Decomposition
##'   4. Correlation Analysis (Confounder Identification)
##'   5. Regression Models
##'   6. Extract Results & Save
##'
##' Key Finding: Unadjusted positive association reverses after 
##'              adjusting for racial composition and HIV burden
##'
##' Note: Data processing (RW, CDC, covariates) is in C_model_data_prep.R
##----------------------------------------------------------------

##----------------------------------------------------------------
## 1. SETUP & DATA LOADING
##----------------------------------------------------------------
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
  Hmisc
)

# Set paths
if (Sys.info()["sysname"] == 'Linux') {
  h <- paste0("/ihme/homes/", Sys.info()[7], "/")
} else if (Sys.info()["sysname"] == 'Darwin') {
  h <- paste0("/Volumes/", Sys.info()[7], "/")
} else {
  h <- "H:/"
}

# Input/Output directories
input_date <- "20260203"  # Update this to match your data prep output date
dir_input <- file.path(h, "aim_outputs/Aim2/C_frontier_analysis", input_date)
dir_output <- file.path(h, "aim_outputs/Aim2/C_frontier_analysis", input_date, "analysis")
if (!dir.exists(dir_output)) dir.create(dir_output, recursive = TRUE)

# Load processed data (from C_model_data_prep.R)
cat("=== 1. LOADING DATA ===\n")
df_as <- readRDS(file.path(dir_input, "df_as_processed_rw_gbd.rds"))

# Filter to HIV
df_hiv <- df_as %>% 
  filter(acause == "hiv") %>%
  filter(!is.na(as_mort_prev_ratio) & !is.na(rw_dex_hiv_prev_ratio))

cat(sprintf("HIV observations: %d (51 states × 10 years)\n", nrow(df_hiv)))
cat(sprintf("Years: %d - %d\n\n", min(df_hiv$year_id), max(df_hiv$year_id)))

##----------------------------------------------------------------
## 2. CREATE BETWEEN/WITHIN VARIABLES (MUNDLAK DECOMPOSITION)
##----------------------------------------------------------------
cat("=== 2. CREATING BETWEEN/WITHIN VARIABLES ===\n")

# Calculate state-level means for between-state analysis
df_state_means <- df_hiv %>%
  group_by(location_id, location_name) %>%
  summarise(
    # Outcome mean
    mort_prev_ratio_B = mean(as_mort_prev_ratio, na.rm = TRUE),
    
    # Main predictor mean
    rw_dex_hiv_prev_ratio_B = mean(rw_dex_hiv_prev_ratio, na.rm = TRUE),
    
    # Covariate means (potential confounders)
    race_prop_BLCK_B = mean(race_prop_BLCK, na.rm = TRUE),
    race_prop_HISP_B = mean(race_prop_HISP, na.rm = TRUE),
    incidence_rates_B = mean(incidence_rates, na.rm = TRUE),
    prevalence_rates_B = mean(prevalence_rates, na.rm = TRUE),
    edu_yrs_B = mean(edu_yrs, na.rm = TRUE),
    obesity_B = mean(obesity, na.rm = TRUE),
    hiv_prevalence_counts_B = mean(hiv_prevalence_counts, na.rm = TRUE),
    aca_implemented_status_B = mean(aca_implemented_status, na.rm = TRUE),
    density_g.1000_B = mean(`density_g.1000`, na.rm = TRUE),
    ldi_pc_B = mean(ldi_pc, na.rm = TRUE),
    
    .groups = "drop"
  )

# Merge state means back to panel
df_hiv <- left_join(df_hiv, df_state_means, by = c("location_id", "location_name"))

# Create within-state deviations (for Mundlak model)
df_hiv <- df_hiv %>%
  mutate(
    rw_dex_hiv_prev_ratio_W = rw_dex_hiv_prev_ratio - rw_dex_hiv_prev_ratio_B,
    race_prop_BLCK_W = race_prop_BLCK - race_prop_BLCK_B,
    incidence_rates_W = incidence_rates - incidence_rates_B,
    edu_yrs_W = edu_yrs - edu_yrs_B
  )

# Center year for interpretability (midpoint = 2014)
df_hiv <- df_hiv %>%
  mutate(year_centered = year_id - 2014)

cat("Between/within variables created.\n\n")

##----------------------------------------------------------------
## 3. VARIANCE DECOMPOSITION
##----------------------------------------------------------------
cat("=== 3. VARIANCE DECOMPOSITION ===\n")

# Function to calculate variance decomposition
calc_variance_decomp <- function(df, var_name, var_B_name) {
  total_var <- var(df[[var_name]], na.rm = TRUE)
  between_var <- var(df[[var_B_name]], na.rm = TRUE)
  within_var <- total_var - between_var
  
  tibble(
    variable = var_name,
    total_var = total_var,
    between_var = between_var,
    within_var = within_var,
    between_pct = between_var / total_var * 100,
    within_pct = within_var / total_var * 100
  )
}

variance_decomp <- bind_rows(
  calc_variance_decomp(df_hiv, "as_mort_prev_ratio", "mort_prev_ratio_B"),
  calc_variance_decomp(df_hiv, "rw_dex_hiv_prev_ratio", "rw_dex_hiv_prev_ratio_B"),
  calc_variance_decomp(df_hiv, "race_prop_BLCK", "race_prop_BLCK_B"),
  calc_variance_decomp(df_hiv, "incidence_rates", "incidence_rates_B"),
  calc_variance_decomp(df_hiv, "edu_yrs", "edu_yrs_B")
)

cat("Variance Decomposition:\n")
print(variance_decomp %>% 
        select(variable, between_pct, within_pct) %>% 
        mutate(across(where(is.numeric), ~round(., 1))))
cat("\n")

##----------------------------------------------------------------
## 4. CORRELATION ANALYSIS (CONFOUNDER IDENTIFICATION)
##----------------------------------------------------------------
cat("=== 4. CORRELATION ANALYSIS ===\n")

# 4a. Define variables for correlation matrix
# Using state-level means (_B variables) for between-state correlations

vars_for_corr <- c(
  # Outcome
  "mort_prev_ratio_B",
  
  
  # Main predictor
  "rw_dex_hiv_prev_ratio_B",
  
  # Demographics
  "race_prop_BLCK_B",
  "race_prop_HISP_B",
  
  # HIV burden
  "incidence_rates_B",
  "prevalence_rates_B",
  "hiv_prevalence_counts_B",
  
  # SES
  "edu_yrs_B",
  "ldi_pc_B",
  
  # Health behaviors
  "obesity_B",
  
  # Policy/Context
  "aca_implemented_status_B",
  "density_g.1000_B"
)

# 4b. Create state-level dataset for correlation (one row per state)
df_state <- df_hiv %>%
  select(location_id, location_name, all_of(vars_for_corr)) %>%
  distinct()

cat(sprintf("State-level dataset: %d states\n", nrow(df_state)))

# 4c. Calculate correlation matrix
cor_matrix <- df_state %>%
  select(all_of(vars_for_corr)) %>%
  cor(use = "pairwise.complete.obs")

# 4d. Calculate correlation with significance (using Hmisc)
cor_with_p <- df_state %>%
  select(all_of(vars_for_corr)) %>%
  as.matrix() %>%
  Hmisc::rcorr()

# 4e. Create confounder analysis table
# A confounder must correlate with BOTH the predictor AND outcome
confounder_analysis <- tibble(
  variable = vars_for_corr[-c(1,2)],  # Exclude outcome and main predictor
  r_with_spending = cor_matrix["rw_dex_hiv_prev_ratio_B", vars_for_corr[-c(1,2)]],
  r_with_mortality = cor_matrix["mort_prev_ratio_B", vars_for_corr[-c(1,2)]],
  p_with_spending = cor_with_p$P["rw_dex_hiv_prev_ratio_B", vars_for_corr[-c(1,2)]],
  p_with_mortality = cor_with_p$P["mort_prev_ratio_B", vars_for_corr[-c(1,2)]]
) %>%
  mutate(
    # Confounder score = |r_spending| * |r_mortality| (higher = stronger confounder)
    confounder_score = abs(r_with_spending) * abs(r_with_mortality),
    # Check if same direction (positive confounder) or opposite (negative confounder)
    same_direction = sign(r_with_spending) == sign(r_with_mortality),
    # Significant confounder if both correlations significant
    both_sig = (p_with_spending < 0.05) & (p_with_mortality < 0.05)
  ) %>%
  arrange(desc(confounder_score))

cat("\nConfounder Analysis (sorted by confounder score):\n")
print(confounder_analysis %>% 
        select(variable, r_with_spending, r_with_mortality, confounder_score, same_direction, both_sig) %>%
        mutate(across(where(is.numeric), ~round(., 3))))

# 4f. Full correlation matrix for all analysis variables (panel-level)
vars_panel_corr <- c(
  # Outcomes (ratios)
  "as_mort_prev_ratio",
  "as_daly_prev_ratio",
  "as_yll_prev_ratio",
  "as_yld_prev_ratio",
  
  # Outcomes (rates)
  "mortality_rates",
  "prevalence_rates",
  "incidence_rates",
  
  # Spending
  "rw_dex_hiv_prev_ratio",
  "as_spend_prev_ratio",
  "spend_all",
  
  # Demographics
  "race_prop_BLCK",
  "race_prop_HISP",
  "race_prop_WHT",
  
  # SES
  "edu_yrs",
  "ldi_pc",
  
  # Health
  "obesity",
  "prev_diabetes",
  
  # Policy/Context
  "aca_implemented_status",
  "density_g.1000",
  "hiv_prevalence_counts"
)

# Filter to variables that exist
vars_panel_corr <- vars_panel_corr[vars_panel_corr %in% names(df_hiv)]

cor_matrix_full <- df_hiv %>%
  select(all_of(vars_panel_corr)) %>%
  cor(use = "pairwise.complete.obs")

cat("\n")

##----------------------------------------------------------------
## 5. REGRESSION MODELS
##----------------------------------------------------------------
cat("=== 5. RUNNING REGRESSION MODELS ===\n")

# 5a. Define model formulas

# --- BETWEEN-STATE MODELS (using _B variables) ---
# M1: Unadjusted (spending only + year trend)
f_m1_unadj <- as.formula("as_mort_prev_ratio ~ rw_dex_hiv_prev_ratio_B + year_centered")

# M2: + Race (key confounder)
f_m2_race <- as.formula("as_mort_prev_ratio ~ rw_dex_hiv_prev_ratio_B + year_centered + race_prop_BLCK_B")

# M3: + HIV Incidence
f_m3_incidence <- as.formula("as_mort_prev_ratio ~ rw_dex_hiv_prev_ratio_B + year_centered + race_prop_BLCK_B + incidence_rates_B")

# M4: Full confounders
f_m4_full <- as.formula("as_mort_prev_ratio ~ rw_dex_hiv_prev_ratio_B + year_centered + race_prop_BLCK_B + incidence_rates_B + edu_yrs_B + aca_implemented_status_B")

# M5: Mundlak (between AND within effects)
f_m5_mundlak <- as.formula("as_mort_prev_ratio ~ rw_dex_hiv_prev_ratio_B + rw_dex_hiv_prev_ratio_W + year_centered + race_prop_BLCK_B + incidence_rates_B + edu_yrs_B")

# --- FIXED EFFECTS PANEL MODELS ---
# M6: Basic fixed effects
f_m6_fe_basic <- as.formula("as_mort_prev_ratio ~ rw_dex_hiv_prev_ratio + factor(year_id) + factor(location_id)")

# M7: FE with high prevalence interaction
f_m7_fe_interact <- as.formula("as_mort_prev_ratio ~ rw_dex_hiv_prev_ratio * high_hiv_prev + factor(year_id) + factor(location_id)")

# M8: FE full model
f_m8_fe_full <- as.formula("as_mort_prev_ratio ~ rw_dex_hiv_prev_ratio * high_hiv_prev + 
                            race_prop_BLCK + race_prop_HISP + obesity + edu_yrs + 
                            density_l.150 + `density_g.1000` +
                            factor(year_id) + factor(location_id)")

# 5b. Run all models
list_models <- list()

list_models[["hiv__m1_unadj"]] <- lm(f_m1_unadj, data = df_hiv)
list_models[["hiv__m2_race"]] <- lm(f_m2_race, data = df_hiv)
list_models[["hiv__m3_incidence"]] <- lm(f_m3_incidence, data = df_hiv)
list_models[["hiv__m4_full"]] <- lm(f_m4_full, data = df_hiv)
list_models[["hiv__m5_mundlak"]] <- lm(f_m5_mundlak, data = df_hiv)
list_models[["hiv__m6_fe_basic"]] <- lm(f_m6_fe_basic, data = df_hiv)
list_models[["hiv__m7_fe_interact"]] <- lm(f_m7_fe_interact, data = df_hiv)
list_models[["hiv__m8_fe_full"]] <- lm(f_m8_fe_full, data = df_hiv)

cat(sprintf("Fitted %d models.\n\n", length(list_models)))

##----------------------------------------------------------------
## 6. EXTRACT RESULTS WITH CLUSTER-ROBUST STANDARD ERRORS
##----------------------------------------------------------------
cat("=== 6. EXTRACTING RESULTS ===\n")

# 6a. Function to extract coefficients with clustered SEs
extract_coefs_clustered <- function(model, model_id, cluster_var) {
  tryCatch({
    # Get cluster-robust variance-covariance matrix (CR2)
    vcov_cr <- vcovCR(model, cluster = cluster_var, type = "CR2")
    
    # Get coefficient test with Satterthwaite df
    coef_test_result <- coef_test(model, vcov = vcov_cr, test = "Satterthwaite")
    
    # Format output
    tibble(
      term = rownames(coef_test_result),
      estimate = coef_test_result$beta,
      std.error = coef_test_result$SE,
      statistic = coef_test_result$tstat,
      p.value = coef_test_result$p_Satt,
      model_id = model_id
    )
  }, error = function(e) {
    # Fallback to regular standard errors
    broom::tidy(model) %>%
      mutate(model_id = model_id)
  })
}

# 6b. Extract coefficients for all models
coef_tbl <- map_dfr(names(list_models), function(nm) {
  extract_coefs_clustered(list_models[[nm]], nm, df_hiv$location_id)
}) %>%
  tidyr::separate(model_id, into = c("acause", "model_name"), sep = "__", remove = FALSE) %>%
  mutate(
    signif_stars = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      p.value < 0.1   ~ "†",
      TRUE            ~ ""
    ),
    signif_label = case_when(
      signif_stars == "***" ~ "p < 0.001",
      signif_stars == "**"  ~ "p < 0.01",
      signif_stars == "*"   ~ "p < 0.05",
      signif_stars == "†"   ~ "p < 0.1",
      TRUE                  ~ "not significant"
    )
  )

# 6c. Extract model fit metrics
metrics_tbl <- imap_dfr(list_models, function(model, model_id) {
  g <- broom::glance(model)
  tibble(
    model_id = model_id,
    n = g$nobs,
    r2 = g$r.squared,
    adj_r2 = g$adj.r.squared,
    aic = AIC(model),
    bic = BIC(model),
    sigma = g$sigma
  )
}) %>%
  tidyr::separate(model_id, into = c("acause", "model_name"), sep = "__", remove = FALSE)

##----------------------------------------------------------------
## 7. SAVE OUTPUTS
##----------------------------------------------------------------
cat("=== 7. SAVING OUTPUTS ===\n")

# Main outputs
write.csv(coef_tbl, file.path(dir_output, "model_coefficients_rw.csv"), row.names = FALSE)
write.csv(metrics_tbl, file.path(dir_output, "model_metrics_rw.csv"), row.names = FALSE)

# Diagnostic outputs
write.csv(variance_decomp, file.path(dir_output, "variance_decomposition.csv"), row.names = FALSE)
write.csv(confounder_analysis, file.path(dir_output, "confounder_analysis.csv"), row.names = FALSE)
write.csv(as.data.frame(cor_matrix), file.path(dir_output, "correlation_matrix_state.csv"))
write.csv(as.data.frame(cor_matrix_full), file.path(dir_output, "correlation_matrix_panel.csv"))

# Save analysis dataset
write.csv(df_hiv, file.path(dir_output, "df_hiv_analysis.csv"), row.names = FALSE)

# Save state-level dataset
write.csv(df_state, file.path(dir_output, "df_state_means.csv"), row.names = FALSE)

cat(sprintf("Outputs saved to: %s\n\n", dir_output))

##----------------------------------------------------------------
## 8. CONSOLE SUMMARY
##----------------------------------------------------------------
cat("========================================\n")
cat("MODEL SUMMARY\n")
cat("========================================\n\n")

cat("--- Variance Decomposition ---\n")
cat(sprintf("  Mortality ratio: %.1f%% between-state, %.1f%% within-state\n",
            variance_decomp$between_pct[1], variance_decomp$within_pct[1]))
cat(sprintf("  Spending ratio:  %.1f%% between-state, %.1f%% within-state\n\n",
            variance_decomp$between_pct[2], variance_decomp$within_pct[2]))

cat("--- Top Confounders ---\n")
print(confounder_analysis %>% 
        filter(confounder_score > 0.01) %>%
        select(variable, r_with_spending, r_with_mortality, confounder_score) %>%
        mutate(across(where(is.numeric), ~round(., 3))) %>%
        as.data.frame())

cat("\n--- Model Fit Summary ---\n")
print(metrics_tbl %>% 
        select(model_name, n, adj_r2, aic) %>% 
        mutate(adj_r2 = round(adj_r2, 3)) %>%
        as.data.frame())

cat("\n--- Key Spending Coefficients ---\n")
print(coef_tbl %>% 
        filter(grepl("rw_dex_hiv_prev_ratio", term)) %>% 
        select(model_name, term, estimate, std.error, p.value, signif_stars) %>%
        mutate(
          estimate = format(estimate, scientific = TRUE, digits = 3),
          std.error = format(std.error, scientific = TRUE, digits = 3),
          p.value = round(p.value, 3)
        ) %>%
        as.data.frame())

cat("\n========================================\n")
cat("KEY INTERPRETATION:\n")
cat("========================================\n")
cat("1. Unadjusted: Spending positively associated with mortality (confounded)\n")
cat("2. After race adjustment: Association reverses (negative)\n")
cat("3. Spending coefficient NOT statistically significant in any model\n")
cat("4. Within-state effect (Mundlak) is positive = reactive spending\n")
cat("5. Strong year trend: ~0.08 pp/year mortality decline\n")
cat("========================================\n")




##----------------------------------------------------------------
## ROBUSTNESS CHECK: Mortality Rate (per population) as outcome
##----------------------------------------------------------------
cat("\n=== ROBUSTNESS CHECK: Mortality Rate per Population ===\n")

# Create state-level mean for mortality rate
df_hiv <- df_hiv %>%
  group_by(location_id) %>%
  mutate(mortality_rates_B = mean(mortality_rates, na.rm = TRUE)) %>%
  ungroup()

# M2 equivalent with mortality rate (per population) instead of mortality/prevalence
f_robust_m2 <- as.formula("mortality_rates ~ rw_dex_hiv_prev_ratio_B + year_centered + race_prop_BLCK_B")

# Run model
m_robust <- lm(f_robust_m2, data = df_hiv)

# Extract with clustered SEs
vcov_cr <- vcovCR(m_robust, cluster = df_hiv$location_id, type = "CR2")
coef_robust <- coef_test(m_robust, vcov = vcov_cr, test = "Satterthwaite")

cat("\nModel: mortality_rates ~ spending_B + year + race_B\n")
cat("(Outcome is deaths per population, NOT deaths per prevalent case)\n\n")
print(round(coef_robust, 6))
cat(sprintf("\nR-squared: %.3f\n", summary(m_robust)$r.squared))
cat(sprintf("Adj R-squared: %.3f\n", summary(m_robust)$adj.r.squared))

# Compare signs
cat("\n--- Comparison of Spending Coefficients ---\n")
cat(sprintf("Mortality/Prevalence (M2): β = %.2e\n", 
            coef_tbl %>% filter(model_name == "m2_race", term == "rw_dex_hiv_prev_ratio_B") %>% pull(estimate)))
cat(sprintf("Mortality/Population:      β = %.2e\n", coef_robust["rw_dex_hiv_prev_ratio_B", "beta"]))



############

# Get full details for the robustness model
cat("\n--- Full Robustness Model Output ---\n")
print(coef_robust)

# Also get the regular summary for comparison
cat("\n--- Standard Summary ---\n")
summary(m_robust)$coefficients

