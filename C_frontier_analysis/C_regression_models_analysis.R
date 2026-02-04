##----------------------------------------------------------------
##' Title: C_regression_models_analysis_FINAL.R
##'
##' Purpose: HIV Spending-Outcomes Regression Analysis for Aim 2B
##'          
##' Structure:
##'   1. Setup & Data Loading
##'   2. Variance Decomposition (justifies between-state focus)
##'   3. Confounder Analysis (identifies race as key confounder)
##'   4. Regression Models (unadjusted → adjusted progression)
##'   5. Final Model & Formal Tables for Paper
##'
##' Key Finding: Unadjusted positive association reverses after 
##'              adjusting for racial composition and HIV burden
##----------------------------------------------------------------

##----------------------------------------------------------------
## 1. SETUP & DATA LOADING
##----------------------------------------------------------------
rm(list = ls())

pacman::p_load(
  tidyverse,
  glue,
  broom,
  plm,
  lmtest,
  sandwich,
  clubSandwich,
  corrplot,
  ggrepel,
  patchwork,
  knitr,
  kableExtra
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
input_date <- "20260203"
dir_input <- file.path(h, "aim_outputs/Aim2/C_frontier_analysis", input_date)
dir_output <- file.path(h, "aim_outputs/Aim2/C_frontier_analysis", input_date, "analysis_final")
if (!dir.exists(dir_output)) dir.create(dir_output, recursive = TRUE)

# Load and filter data
cat("=== 1. LOADING DATA ===\n")
df_as <- readRDS(file.path(dir_input, "df_as_processed_rw_gbd.rds"))

df_hiv <- df_as %>% 
  filter(acause == "hiv") %>%
  filter(!is.na(as_mort_prev_ratio) & !is.na(rw_dex_hiv_prev_ratio))

cat(sprintf("HIV observations: %d (51 states × 10 years)\n", nrow(df_hiv)))
cat(sprintf("Years: %d - %d\n\n", min(df_hiv$year_id), max(df_hiv$year_id)))

##----------------------------------------------------------------
## 2. VARIANCE DECOMPOSITION
##    Justifies why between-state analysis is primary
##----------------------------------------------------------------
cat("=== 2. VARIANCE DECOMPOSITION ===\n")

pdata <- pdata.frame(df_hiv, index = c("location_id", "year_id"))

vars_decomp <- c("as_mort_prev_ratio", "rw_dex_hiv_prev_ratio", 
                 "hiv_prevalence_counts", "race_prop_BLCK",
                 "incidence_rates", "edu_yrs", "obesity")

variance_table <- map_dfr(vars_decomp, function(v) {
  if (!v %in% names(pdata)) return(NULL)
  x <- pdata[[v]]
  total_var <- var(x, na.rm = TRUE)
  between_var <- var(Between(x), na.rm = TRUE)
  within_var <- var(Within(x), na.rm = TRUE)
  
  tibble(
    Variable = v,
    `Total Variance` = total_var,
    `Between (%)` = round(between_var / total_var * 100, 1),
    `Within (%)` = round(within_var / total_var * 100, 1)
  )
})

cat("\nVariance Decomposition:\n")
print(variance_table)
write_csv(variance_table, file.path(dir_output, "T1_variance_decomposition.csv"))

cat(sprintf("\n→ Key: %.1f%% of spending variation is BETWEEN states\n",
            variance_table$`Between (%)`[variance_table$Variable == "rw_dex_hiv_prev_ratio"]))
cat("→ This justifies cross-sectional (between-state) analysis\n\n")

##----------------------------------------------------------------
## 3. CREATE ANALYSIS VARIABLES
##----------------------------------------------------------------
cat("=== 3. CREATING ANALYSIS VARIABLES ===\n")

# Create between-state means (_B) and within-state deviations (_W)
df_hiv <- df_hiv %>%
  group_by(location_id) %>%
  mutate(
    # Main predictor
    rw_dex_hiv_prev_ratio_B = mean(rw_dex_hiv_prev_ratio, na.rm = TRUE),
    rw_dex_hiv_prev_ratio_W = rw_dex_hiv_prev_ratio - rw_dex_hiv_prev_ratio_B,
    # Confounders
    race_prop_BLCK_B = mean(race_prop_BLCK, na.rm = TRUE),
    incidence_rates_B = mean(incidence_rates, na.rm = TRUE),
    edu_yrs_B = mean(edu_yrs, na.rm = TRUE),
    aca_implemented_status_B = mean(aca_implemented_status, na.rm = TRUE),
    obesity_B = mean(obesity, na.rm = TRUE),
    # For weighting
    mean_prevalence = mean(hiv_prevalence_counts, na.rm = TRUE)
  ) %>%
  ungroup()

# State-level averages for cross-sectional analysis
df_state <- df_hiv %>%
  group_by(location_id, location_name) %>%
  summarize(across(where(is.numeric), ~mean(.x, na.rm = TRUE)), .groups = "drop")

cat(sprintf("Created _B variables for %d states\n\n", nrow(df_state)))

##----------------------------------------------------------------
## 4. CONFOUNDER ANALYSIS
##    Identify variables correlated with BOTH spending AND mortality
##----------------------------------------------------------------
cat("=== 4. CONFOUNDER ANALYSIS ===\n")

# Variables to check
confounder_vars <- c("race_prop_BLCK", "race_prop_HISP", "incidence_rates",
                     "prevalence_rates", "edu_yrs", "ldi_pc", "obesity",
                     "aca_implemented_status", "density_g.1000", "hiv_prevalence_counts")

# Compute correlations with spending and mortality
confounder_analysis <- tibble(Variable = confounder_vars) %>%
  rowwise() %>%
  mutate(
    `r(Spending)` = cor(df_state[[Variable]], df_state$rw_dex_hiv_prev_ratio, use = "complete"),
    `r(Mortality)` = cor(df_state[[Variable]], df_state$as_mort_prev_ratio, use = "complete"),
    `Confounder Score` = abs(`r(Spending)`) * abs(`r(Mortality)`),
    `Same Direction` = sign(`r(Spending)`) == sign(`r(Mortality)`)
  ) %>%
  ungroup() %>%
  arrange(desc(`Confounder Score`))

cat("\nConfounder Assessment (variables predicting BOTH spending AND mortality):\n")
print(confounder_analysis %>% mutate(across(where(is.numeric), ~round(.x, 3))))
write_csv(confounder_analysis, file.path(dir_output, "T2_confounder_analysis.csv"))

cat("\n→ Key confounders identified:\n")
cat("  1. race_prop_BLCK (r=-0.47 spending, r=-0.45 mortality)\n")
cat("  2. incidence_rates (r=-0.35 spending, r=-0.49 mortality)\n")
cat("  3. Both have SAME direction: states with higher values have LOWER spending AND mortality\n\n")

##----------------------------------------------------------------
## 5. REGRESSION MODELS: UNADJUSTED → ADJUSTED PROGRESSION
##----------------------------------------------------------------
cat("=== 5. REGRESSION MODELS ===\n")

# M1: Unadjusted (between-state only)
m1 <- lm(as_mort_prev_ratio ~ rw_dex_hiv_prev_ratio_B + year_centered, 
         data = df_hiv)

# M2: Adjust for race (strongest confounder)
m2 <- lm(as_mort_prev_ratio ~ rw_dex_hiv_prev_ratio_B + 
           race_prop_BLCK_B + year_centered, 
         data = df_hiv)

# M3: + HIV burden (incidence)
m3 <- lm(as_mort_prev_ratio ~ rw_dex_hiv_prev_ratio_B + 
           race_prop_BLCK_B + incidence_rates_B + year_centered, 
         data = df_hiv)

# M4: Full confounders
m4 <- lm(as_mort_prev_ratio ~ rw_dex_hiv_prev_ratio_B + 
           race_prop_BLCK_B + incidence_rates_B + 
           edu_yrs_B + aca_implemented_status_B + year_centered, 
         data = df_hiv)

# M5: Mundlak (within + between)
m5 <- lm(as_mort_prev_ratio ~ 
           rw_dex_hiv_prev_ratio_W +
           rw_dex_hiv_prev_ratio_B + 
           race_prop_BLCK_B + incidence_rates_B + 
           edu_yrs_B + year_centered, 
         data = df_hiv)

# Extract coefficients with clustered SEs
models <- list(m1, m2, m3, m4, m5)
model_names <- c("Unadjusted", "+ Race", "+ Incidence", "Full Confounders", "Mundlak (W+B)")

# Function to extract spending coefficient with clustered SE
extract_coef <- function(m, model_name) {
  robust_vcov <- vcovCR(m, cluster = df_hiv$location_id, type = "CR2")
  robust_test <- coef_test(m, vcov = robust_vcov)
  
  result <- tibble(Model = model_name)
  
  # Between coefficient
  if ("rw_dex_hiv_prev_ratio_B" %in% rownames(robust_test)) {
    idx <- which(rownames(robust_test) == "rw_dex_hiv_prev_ratio_B")
    result$`β_B` <- robust_test$Coef.[idx]
    result$`SE_B` <- robust_test$SE[idx]
    result$`p_B` <- robust_test$`p-val (Satt)`[idx]
  }
  
  # Within coefficient (if present)
  if ("rw_dex_hiv_prev_ratio_W" %in% rownames(robust_test)) {
    idx <- which(rownames(robust_test) == "rw_dex_hiv_prev_ratio_W")
    result$`β_W` <- robust_test$Coef.[idx]
    result$`SE_W` <- robust_test$SE[idx]
    result$`p_W` <- robust_test$`p-val (Satt)`[idx]
  }
  
  result$R2 <- summary(m)$r.squared
  result$N <- nobs(m)
  
  return(result)
}

# Build results table
results_table <- map2_dfr(models, model_names, extract_coef)

cat("\nCoefficient Progression (Clustered SEs):\n")
print(results_table %>% mutate(across(where(is.numeric), ~signif(.x, 3))))

cat("\n→ KEY FINDING: Sign flip from +6.47e-08 (unadjusted) to -5.02e-08 (adjusted for race)\n")
cat("→ After full adjustment: β = -8.59e-08\n")
cat("→ Within-state effect (β_W) is positive, reflecting reactive spending\n\n")

##----------------------------------------------------------------
## 6. VISUALIZATIONS
##----------------------------------------------------------------
cat("=== 6. CREATING VISUALIZATIONS ===\n")

# Residualize for race to show before/after
df_state <- df_state %>%
  mutate(
    spending_resid = residuals(lm(rw_dex_hiv_prev_ratio ~ race_prop_BLCK, data = .)),
    mortality_resid = residuals(lm(as_mort_prev_ratio ~ race_prop_BLCK, data = .))
  )

# Panel A: Unadjusted
p1 <- ggplot(df_state, aes(x = rw_dex_hiv_prev_ratio/1000, y = as_mort_prev_ratio * 100)) +
  geom_point(aes(color = race_prop_BLCK, size = hiv_prevalence_counts/1000), alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "darkred", linewidth = 1) +
  geom_text_repel(aes(label = location_name), size = 2.5, max.overlaps = 12) +
  scale_color_viridis_c(option = "plasma", name = "Proportion\nBlack") +
  scale_size_continuous(name = "HIV Prevalence\n(thousands)", range = c(2, 10)) +
  labs(
    x = "Spending per Prevalent Case ($thousands)",
    y = "Mortality per Prevalent Case (%)",
    title = "A. Unadjusted Association",
    subtitle = sprintf("β = +6.47×10⁻⁸ (positive)")
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "right")

# Panel B: Adjusted for race
p2 <- ggplot(df_state, aes(x = spending_resid/1000, y = mortality_resid * 100)) +
  geom_point(aes(color = race_prop_BLCK, size = hiv_prevalence_counts/1000), alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "darkblue", linewidth = 1) +
  geom_text_repel(aes(label = location_name), size = 2.5, max.overlaps = 12) +
  scale_color_viridis_c(option = "plasma", name = "Proportion\nBlack") +
  scale_size_continuous(name = "HIV Prevalence\n(thousands)", range = c(2, 10)) +
  labs(
    x = "Spending Residual (adjusted for race)",
    y = "Mortality Residual (adjusted for race)",
    title = "B. Adjusted for Racial Composition",
    subtitle = sprintf("β = -5.02×10⁻⁸ (negative)")
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "right")

p_main <- p1 + p2 + plot_layout(guides = "collect") +
  plot_annotation(
    title = "Figure X. HIV Spending and Mortality: Confounding by Racial Composition",
    caption = "Note: Each point represents a state (2010-2019 average). Size indicates HIV prevalence."
  )

ggsave(file.path(dir_output, "F1_spending_mortality_confounding.png"),
       p_main, width = 16, height = 7, dpi = 300)
ggsave(file.path(dir_output, "F1_spending_mortality_confounding.pdf"),
       p_main, width = 16, height = 7)

cat("Saved: F1_spending_mortality_confounding.png/pdf\n")

##----------------------------------------------------------------
## 7. QUADRANT ANALYSIS
##----------------------------------------------------------------
cat("\n=== 7. QUADRANT ANALYSIS ===\n")

df_state <- df_state %>%
  mutate(
    spending_cat = ifelse(rw_dex_hiv_prev_ratio > median(rw_dex_hiv_prev_ratio), 
                          "High Spending", "Low Spending"),
    mortality_cat = ifelse(as_mort_prev_ratio > median(as_mort_prev_ratio),
                           "High Mortality", "Low Mortality"),
    quadrant = paste(spending_cat, mortality_cat, sep = " / ")
  )

quadrant_summary <- df_state %>%
  group_by(quadrant) %>%
  summarize(
    `N States` = n(),
    `Mean Prevalence` = round(mean(hiv_prevalence_counts), 0),
    `Mean % Black` = round(mean(race_prop_BLCK) * 100, 1),
    `Mean Incidence` = round(mean(incidence_rates) * 100000, 1),
    States = paste(location_name, collapse = ", "),
    .groups = "drop"
  ) %>%
  arrange(desc(`Mean Prevalence`))

cat("\nState Distribution by Quadrant:\n")
print(quadrant_summary %>% select(-States))
write_csv(quadrant_summary, file.path(dir_output, "T3_quadrant_analysis.csv"))

##----------------------------------------------------------------
## FIX FOR SECTIONS 8-9: Run this after the main script
## Creates the formal regression table and summary
##----------------------------------------------------------------

# First, let's see what columns we actually have in results_table
cat("Columns in results_table:\n")
print(colnames(results_table))
print(results_table)

##----------------------------------------------------------------
## 8. SIMPLE REGRESSION SUMMARY TABLE
##----------------------------------------------------------------

# Extract coefficients directly from models (simpler approach)
summary_table <- tibble(
  Model = c("M1: Unadjusted", 
            "M2: + Race", 
            "M3: + Incidence", 
            "M4: Full Confounders", 
            "M5: Mundlak"),
  beta_B = c(
    coef(m1)["rw_dex_hiv_prev_ratio_B"],
    coef(m2)["rw_dex_hiv_prev_ratio_B"],
    coef(m3)["rw_dex_hiv_prev_ratio_B"],
    coef(m4)["rw_dex_hiv_prev_ratio_B"],
    coef(m5)["rw_dex_hiv_prev_ratio_B"]
  ),
  beta_W = c(NA, NA, NA, NA, coef(m5)["rw_dex_hiv_prev_ratio_W"]),
  R2 = c(
    summary(m1)$r.squared,
    summary(m2)$r.squared,
    summary(m3)$r.squared,
    summary(m4)$r.squared,
    summary(m5)$r.squared
  )
)

# Get clustered SEs for each model
get_clustered_se <- function(model, var_name) {
  vcov_mat <- vcovCR(model, cluster = df_hiv$location_id, type = "CR2")
  if (var_name %in% rownames(vcov_mat)) {
    return(sqrt(vcov_mat[var_name, var_name]))
  } else {
    return(NA)
  }
}

summary_table$se_B <- c(
  get_clustered_se(m1, "rw_dex_hiv_prev_ratio_B"),
  get_clustered_se(m2, "rw_dex_hiv_prev_ratio_B"),
  get_clustered_se(m3, "rw_dex_hiv_prev_ratio_B"),
  get_clustered_se(m4, "rw_dex_hiv_prev_ratio_B"),
  get_clustered_se(m5, "rw_dex_hiv_prev_ratio_B")
)

summary_table$se_W <- c(NA, NA, NA, NA, get_clustered_se(m5, "rw_dex_hiv_prev_ratio_W"))

# Calculate t-stats and p-values (approximate)
summary_table <- summary_table %>%
  mutate(
    t_B = beta_B / se_B,
    p_B = 2 * pt(-abs(t_B), df = 50),  # approximate with 50 df
    t_W = beta_W / se_W,
    p_W = 2 * pt(-abs(t_W), df = 50)
  )

# Format for display
summary_display <- summary_table %>%
  mutate(
    `Between Effect (β_B)` = sprintf("%.2e", beta_B),
    `SE (β_B)` = sprintf("%.2e", se_B),
    `p-value (β_B)` = sprintf("%.3f", p_B),
    `Within Effect (β_W)` = ifelse(is.na(beta_W), "—", sprintf("%.2e", beta_W)),
    `R²` = sprintf("%.3f", R2)
  ) %>%
  select(Model, `Between Effect (β_B)`, `SE (β_B)`, `p-value (β_B)`, `Within Effect (β_W)`, `R²`)

cat("\n=== REGRESSION SUMMARY TABLE ===\n")
print(summary_display)

write_csv(summary_display, file.path(dir_output, "T5_regression_summary.csv"))
cat("\nSaved: T5_regression_summary.csv\n")

##----------------------------------------------------------------
## 9. DETAILED REGRESSION TABLE FOR PAPER
##----------------------------------------------------------------

# Create a more detailed table with all coefficients
create_model_summary <- function(model, model_name) {
  # Get coefficients
  coefs <- coef(model)
  
  # Get clustered SEs
  vcov_mat <- vcovCR(model, cluster = df_hiv$location_id, type = "CR2")
  ses <- sqrt(diag(vcov_mat))
  
  # Build table
  tibble(
    term = names(coefs),
    estimate = coefs,
    std_error = ses[names(coefs)],
    t_stat = coefs / ses[names(coefs)],
    model = model_name
  )
}

# Combine all models
all_coefs <- bind_rows(
  create_model_summary(m1, "M1: Unadjusted"),
  create_model_summary(m2, "M2: + Race"),
  create_model_summary(m3, "M3: + Incidence"),
  create_model_summary(m4, "M4: Full"),
  create_model_summary(m5, "M5: Mundlak")
)

# Pivot to wide format
regression_table_wide <- all_coefs %>%
  mutate(
    coef_display = sprintf("%.3e\n(%.3e)", estimate, std_error)
  ) %>%
  select(term, model, coef_display) %>%
  pivot_wider(names_from = model, values_from = coef_display)

cat("\n=== DETAILED REGRESSION TABLE ===\n")
print(regression_table_wide)

write_csv(regression_table_wide, file.path(dir_output, "T4_regression_table_detailed.csv"))
cat("\nSaved: T4_regression_table_detailed.csv\n")

##----------------------------------------------------------------
## 10. PUBLICATION-READY TABLE (Cleaner format)
##----------------------------------------------------------------

# Key variables only
key_vars <- c("rw_dex_hiv_prev_ratio_B", "rw_dex_hiv_prev_ratio_W",
              "race_prop_BLCK_B", "incidence_rates_B", "edu_yrs_B",
              "aca_implemented_status_B", "year_centered")

pub_table <- all_coefs %>%
  filter(term %in% key_vars) %>%
  mutate(
    # Add significance stars
    p_val = 2 * pt(-abs(t_stat), df = 50),
    stars = case_when(
      p_val < 0.001 ~ "***",
      p_val < 0.01 ~ "**",
      p_val < 0.05 ~ "*",
      p_val < 0.1 ~ "†",
      TRUE ~ ""
    ),
    # Format coefficient
    coef_formatted = sprintf("%.2e%s", estimate, stars),
    se_formatted = sprintf("(%.2e)", std_error),
    # Clean variable names
    Variable = case_when(
      term == "rw_dex_hiv_prev_ratio_B" ~ "Spending/Case (Between)",
      term == "rw_dex_hiv_prev_ratio_W" ~ "Spending/Case (Within)",
      term == "race_prop_BLCK_B" ~ "Proportion Black",
      term == "incidence_rates_B" ~ "HIV Incidence Rate",
      term == "edu_yrs_B" ~ "Education (Years)",
      term == "aca_implemented_status_B" ~ "ACA Expansion",
      term == "year_centered" ~ "Year Trend",
      TRUE ~ term
    )
  ) %>%
  select(Variable, model, coef_formatted, se_formatted)

# Create final publication table
pub_table_wide <- pub_table %>%
  unite("cell", coef_formatted, se_formatted, sep = "\n") %>%
  pivot_wider(names_from = model, values_from = cell, values_fill = "—")

# Add R² and N rows
r2_row <- tibble(
  Variable = "R²",
  `M1: Unadjusted` = sprintf("%.3f", summary(m1)$r.squared),
  `M2: + Race` = sprintf("%.3f", summary(m2)$r.squared),
  `M3: + Incidence` = sprintf("%.3f", summary(m3)$r.squared),
  `M4: Full` = sprintf("%.3f", summary(m4)$r.squared),
  `M5: Mundlak` = sprintf("%.3f", summary(m5)$r.squared)
)

n_row <- tibble(
  Variable = "N (state-years)",
  `M1: Unadjusted` = "510",
  `M2: + Race` = "510",
  `M3: + Incidence` = "510",
  `M4: Full` = "510",
  `M5: Mundlak` = "510"
)

clusters_row <- tibble(
  Variable = "Clusters (states)",
  `M1: Unadjusted` = "51",
  `M2: + Race` = "51",
  `M3: + Incidence` = "51",
  `M4: Full` = "51",
  `M5: Mundlak` = "51"
)

pub_table_final <- bind_rows(pub_table_wide, r2_row, n_row, clusters_row)

cat("\n=== PUBLICATION TABLE ===\n")
print(pub_table_final)

write_csv(pub_table_final, file.path(dir_output, "T6_publication_table.csv"))
cat("\nSaved: T6_publication_table.csv\n")

##----------------------------------------------------------------
## 11. PRINT FINAL SUMMARY
##----------------------------------------------------------------

cat("\n")
cat("════════════════════════════════════════════════════════════════\n")
cat("                    COEFFICIENT PROGRESSION                      \n")
cat("════════════════════════════════════════════════════════════════\n")
cat(sprintf("M1 (Unadjusted):     β_B = %+.2e\n", coef(m1)["rw_dex_hiv_prev_ratio_B"]))
cat(sprintf("M2 (+ Race):         β_B = %+.2e  ← SIGN FLIP\n", coef(m2)["rw_dex_hiv_prev_ratio_B"]))
cat(sprintf("M3 (+ Incidence):    β_B = %+.2e\n", coef(m3)["rw_dex_hiv_prev_ratio_B"]))
cat(sprintf("M4 (Full):           β_B = %+.2e\n", coef(m4)["rw_dex_hiv_prev_ratio_B"]))
cat(sprintf("M5 (Mundlak):        β_B = %+.2e  (between-state)\n", coef(m5)["rw_dex_hiv_prev_ratio_B"]))
cat(sprintf("                     β_W = %+.2e  (within-state)\n", coef(m5)["rw_dex_hiv_prev_ratio_W"]))
cat("════════════════════════════════════════════════════════════════\n")

cat("\nAll tables saved to:", dir_output, "\n")
##----------------------------------------------------------------
## 10. FINAL INTERPRETATION
##----------------------------------------------------------------
cat("\n")
cat("╔══════════════════════════════════════════════════════════════════╗\n")
cat("║                    FINAL RESULTS SUMMARY                        ║\n")
cat("╠══════════════════════════════════════════════════════════════════╣\n")
cat("║                                                                  ║\n")
cat("║  UNADJUSTED:  β = +6.47×10⁻⁸  (positive, NS)                   ║\n")
cat("║  + RACE:      β = -5.02×10⁻⁸  (SIGN FLIP to negative)          ║\n")
cat("║  FULL MODEL:  β = -8.59×10⁻⁸  (negative, NS)                   ║\n")
cat("║                                                                  ║\n")
cat("║  MUNDLAK DECOMPOSITION:                                         ║\n")
cat("║    Between (β_B): -5.44×10⁻⁸  (cross-state)                    ║\n")
cat("║    Within (β_W):  +8.72×10⁻⁸  (within-state, reactive)         ║\n")
cat("║                                                                  ║\n")
cat("╠══════════════════════════════════════════════════════════════════╣\n")
cat("║  INTERPRETATION:                                                 ║\n")
cat("║                                                                  ║\n")
cat("║  The unadjusted positive association is CONFOUNDED by racial    ║\n")
cat("║  composition. States with higher Black populations have:        ║\n")
cat("║    • LOWER spending per case (economies of scale)               ║\n")
cat("║    • LOWER mortality per case (experienced HIV systems)         ║\n")
cat("║                                                                  ║\n")
cat("║  After adjustment, higher spending is associated with LOWER     ║\n")
cat("║  mortality, though not statistically significant.               ║\n")
cat("║                                                                  ║\n")
cat("║  The positive within-state effect reflects REACTIVE spending:   ║\n")
cat("║  states increase spending in response to worsening outcomes.    ║\n")
cat("║                                                                  ║\n")
cat("╚══════════════════════════════════════════════════════════════════╝\n")

##----------------------------------------------------------------
## 11. SAVE ALL OUTPUTS
##----------------------------------------------------------------
cat("\n=== FILES SAVED ===\n")
cat(sprintf("Output directory: %s\n\n", dir_output))
cat("Tables:\n")
cat("  T1_variance_decomposition.csv\n")
cat("  T2_confounder_analysis.csv\n")
cat("  T3_quadrant_analysis.csv\n")
cat("  T4_regression_table_formal.csv\n")
cat("  T5_regression_summary.csv\n")
cat("\nFigures:\n")
cat("  F1_spending_mortality_confounding.png/pdf\n")

cat("\n=== ANALYSIS COMPLETE ===\n")

