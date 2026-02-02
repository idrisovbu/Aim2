##----------------------------------------------------------------
##' Title: D_decomp_analysis.R
##'
##' Purpose: Das Gupta 4-Factor Decomposition of HIV Spending (2010-2019)
##----------------------------------------------------------------

##----------------------------------------------------------------
## Clear environment and set library paths
##----------------------------------------------------------------
rm(list = ls())

pacman::p_load(dplyr, openxlsx, RMySQL, data.table, ini, DBI, tidyr,RColorBrewer,forcats,openxlsx, reticulate, ggpubr, arrow, scales, Rcpp)
library(lbd.loader, lib.loc = sprintf("/share/geospatial/code/geospatial-libraries/lbd.loader-%s", R.version$major))
if("dex.dbr"%in% (.packages())) detach("package:dex.dbr", unload=TRUE)
library(dex.dbr, lib.loc = lbd.loader::pkg_loc("dex.dbr"))
suppressMessages(devtools::load_all(path = "/ihme/homes/idrisov/repo/dex_us_county/"))

# Set drive paths
if (Sys.info()["sysname"] == 'Linux'){
  j <- "/home/j/"
  h <- paste0("/ihme/homes/",Sys.info()[7],"/")
  l <- '/ihme/limited_use/'
} else if (Sys.info()["sysname"] == 'Darwin'){
  j <- "/Volumes/snfs"
  h <- paste0("/Volumes/",Sys.info()[7],"/")
  l <- '/Volumes/limited_use'
} else {
  j <- "J:/"
  h <- "H:/"
  l <- 'L:/'
}

##----------------------------------------------------------------
## 0. Functions
##----------------------------------------------------------------
ensure_dir_exists <- function(dir_path) {
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
}

convert_to_dollars <- function(df, cols_to_convert) {
  for (col in colnames(df)) {
    if (col %in% cols_to_convert) {
      df[[col]] <- dollar(df[[col]])
    }
  }
  return(df)
}

##----------------------------------------------------------------
## 0.1 Set directories
##----------------------------------------------------------------
date_decomp <- "20260201"
fp_decomp <- file.path(h, "/aim_outputs/Aim2/C_frontier_analysis/", date_decomp, "/df_decomp.csv")

date_today <- format(Sys.time(), "%Y%m%d")
dir_output <- file.path(h, "/aim_outputs/Aim2/D_tables_figures/", date_today)
ensure_dir_exists(dir_output)

##----------------------------------------------------------------
## 0.2 Read in data
##----------------------------------------------------------------
df_decomp <- read_csv(fp_decomp)

##----------------------------------------------------------------
## 1. Transform data
##----------------------------------------------------------------
val_cols <- c("prevalence_counts", "mortality_counts", "daly_counts", 
              "incidence_counts", "yll_counts", "yld_counts", "population", 
              "spend_all")

cols_to_drop <- c("spend_mdcd", "spend_mdcr", "spend_oop", "spend_priv")

df_decomp_p <- df_decomp %>%
  filter(year_id %in% c(2010, 2019)) %>%
  select(!all_of(cols_to_drop)) %>%
  pivot_wider(
    names_from = year_id,
    values_from = all_of(val_cols)
  )

# Filter to HIV only (toggle to "Substance use disorders" to switch)
df_decomp_p <- df_decomp_p %>%
  filter(cause_name == "HIV/AIDS")

###############################################################################
# Das Gupta 4-Factor Decomposition (CORRECTED)
# 
# Identity: Spend_cell = Pop_state × PrevRate_state × PrevShare_cell × SpendPerCase_cell
#
# Factors:
#   1. Population (state total)
#   2. Case rate (state prevalence / state population) 
#   3. Age-sex structure (cell prevalence / state prevalence)
#   4. Spending intensity (cell spending / cell prevalence)
###############################################################################

# ============================================================================
# STEP 1: Define decomposition function (Das Gupta 1991)
# ============================================================================

cppFunction('NumericVector dcomp(NumericMatrix X, NumericMatrix Y){
    int m = X.nrow();
    int n = X.ncol();
    int i, j, k;
    IntegerVector q(n);
    int kcopy, size_q;
    NumericVector prod_x_in_y_out(m);
    NumericVector prod_y_in_x_out(m);
    IntegerVector denoms(n+1);
    NumericMatrix result(m, n);
    
    denoms[0] = 0;
    denoms[1] = n;
    for(j=1; j<n; j++){
      denoms[j+1] = denoms[j] * (n - j) / j;
      for(i=0; i<m; i++){ result(i, j) = 0.0; }
    }
    
    for(k=1; k<pow(2, n); k++){
      kcopy = k;
      size_q = 0;
      for(i=0; i<m; i++){
        prod_x_in_y_out[i] = 1.0;
        prod_y_in_x_out[i] = 1.0;
      }
      for(j=0; j<n; j++){
        q[j] = kcopy % 2;
        if(q[j] == 1){
          size_q++;
          kcopy--;
          for(i=0; i<m; i++){
            prod_x_in_y_out[i] = prod_x_in_y_out[i] * X(i, j);
            prod_y_in_x_out[i] = prod_y_in_x_out[i] * Y(i, j);
          }
        } else{
          for(i=0; i<m; i++){
            prod_x_in_y_out[i] = prod_x_in_y_out[i] * Y(i, j);
            prod_y_in_x_out[i] = prod_y_in_x_out[i] * X(i, j);
          }
        }
        kcopy = kcopy/2;
      }
      for(j=0; j<n; j++){
        if(q[j] == 1){
          for(i=0; i<m; i++){
            result(i, j) = result(i, j) + (prod_y_in_x_out[i] - prod_x_in_y_out[i])/denoms[size_q];
          }
        }
      }
    }
    return result;
}')

decompose <- function(dt, factor_names, start_year, end_year){
  dt[, paste0(factor_names, "_effect") := as.data.table(
    dcomp(
      as.matrix(.SD[, paste(factor_names, start_year, sep="_"), with=FALSE]),
      as.matrix(.SD[, paste(factor_names, end_year, sep="_"), with=FALSE])
    )
  )]
  return(dt[])
}

# ============================================================================
# STEP 2: Prepare data with CORRECT 4-factor identity
# ============================================================================

dt <- as.data.table(df_decomp_p)

# ---- State-level totals ----
dt[, pop_state_2010 := sum(population_2010), by = .(cause_id, location_id)]
dt[, pop_state_2019 := sum(population_2019), by = .(cause_id, location_id)]

dt[, prev_state_2010 := sum(prevalence_counts_2010), by = .(cause_id, location_id)]
dt[, prev_state_2019 := sum(prevalence_counts_2019), by = .(cause_id, location_id)]

# ---- FACTOR 1: Population size (state total) ----
dt[, population_2010 := pop_state_2010]
dt[, population_2019 := pop_state_2019]

# ---- FACTOR 2: Case rate (state prevalence rate = cases per person) ----
dt[, case_rate_2010 := fifelse(pop_state_2010 > 0, prev_state_2010 / pop_state_2010, 0)]
dt[, case_rate_2019 := fifelse(pop_state_2019 > 0, prev_state_2019 / pop_state_2019, 0)]

# ---- FACTOR 3: Age-sex structure (share of cases in this cell) ----
# THIS IS THE KEY FIX: use cell prevalence / state prevalence
dt[, age_sex_frac_2010 := fifelse(prev_state_2010 > 0, prevalence_counts_2010 / prev_state_2010, 0)]
dt[, age_sex_frac_2019 := fifelse(prev_state_2019 > 0, prevalence_counts_2019 / prev_state_2019, 0)]

# ---- FACTOR 4: Spending intensity (spend per case in this cell) ----
dt[, spend_per_case_2010 := fifelse(prevalence_counts_2010 > 0, spend_all_2010 / prevalence_counts_2010, 0)]
dt[, spend_per_case_2019 := fifelse(prevalence_counts_2019 > 0, spend_all_2019 / prevalence_counts_2019, 0)]

# Keep original spending for validation
dt[, spend_2010 := spend_all_2010]
dt[, spend_2019 := spend_all_2019]

# ============================================================================
# STEP 2.5: VALIDATION - Check that identity holds
# ============================================================================

dt[, spend_hat_2010 := population_2010 * case_rate_2010 * age_sex_frac_2010 * spend_per_case_2010]
dt[, spend_hat_2019 := population_2019 * case_rate_2019 * age_sex_frac_2019 * spend_per_case_2019]

dt[, check_2010 := spend_2010 - spend_hat_2010]
dt[, check_2019 := spend_2019 - spend_hat_2019]

cat("\n============ IDENTITY VALIDATION ============\n")
cat("Residuals should be ~0 (tiny numeric noise only)\n\n")
cat("2010 residuals:\n")
print(summary(dt$check_2010))
cat("\n2019 residuals:\n")
print(summary(dt$check_2019))

# ============================================================================
# STEP 3: Run the decomposition
# ============================================================================

factor_names <- c("population", "case_rate", "age_sex_frac", "spend_per_case")
dt <- decompose(dt, factor_names = factor_names, start_year = 2010, end_year = 2019)

# Calculate spending change
dt[, delta_spend := spend_2019 - spend_2010]

# ============================================================================
# STEP 4: Aggregate results
# ============================================================================
# Column definitions:
#   pop_size_effect         = Change due to total state population growth
#   prevalence_rate_effect  = Change due to HIV prevalence rate (cases per capita)
#   case_composition_effect = Change due to shift in age-sex distribution of cases
#   spend_intensity_effect  = Change due to spending per case

# National totals
national <- dt[, .(
  pop_size_effect = sum(population_effect, na.rm = TRUE),
  prevalence_rate_effect = sum(case_rate_effect, na.rm = TRUE),
  case_composition_effect = sum(age_sex_frac_effect, na.rm = TRUE),
  spend_intensity_effect = sum(spend_per_case_effect, na.rm = TRUE),
  delta_spend = sum(delta_spend, na.rm = TRUE),
  spend_2010 = sum(spend_2010, na.rm = TRUE),
  spend_2019 = sum(spend_2019, na.rm = TRUE)
)]

# Add percentages
national[, `:=`(
  pct_pop_size = 100 * pop_size_effect / delta_spend,
  pct_prevalence_rate = 100 * prevalence_rate_effect / delta_spend,
  pct_case_composition = 100 * case_composition_effect / delta_spend,
  pct_spend_intensity = 100 * spend_intensity_effect / delta_spend
)]

# Decomposition validation
national[, sum_effects := pop_size_effect + prevalence_rate_effect + case_composition_effect + spend_intensity_effect]
national[, diff_check := delta_spend - sum_effects]

# By state
by_state <- dt[, .(
  pop_size_effect = sum(population_effect, na.rm = TRUE),
  prevalence_rate_effect = sum(case_rate_effect, na.rm = TRUE),
  case_composition_effect = sum(age_sex_frac_effect, na.rm = TRUE),
  spend_intensity_effect = sum(spend_per_case_effect, na.rm = TRUE),
  delta_spend = sum(delta_spend, na.rm = TRUE),
  spend_2010 = sum(spend_2010, na.rm = TRUE),
  spend_2019 = sum(spend_2019, na.rm = TRUE)
), by = .(location_id, location_name)]

by_state[, `:=`(
  pct_pop_size = 100 * pop_size_effect / delta_spend,
  pct_prevalence_rate = 100 * prevalence_rate_effect / delta_spend,
  pct_case_composition = 100 * case_composition_effect / delta_spend,
  pct_spend_intensity = 100 * spend_intensity_effect / delta_spend
)]

# State-level validation
by_state[, sum_effects := pop_size_effect + prevalence_rate_effect + case_composition_effect + spend_intensity_effect]
by_state[, diff_check := delta_spend - sum_effects]

# ============================================================================
# STEP 5: View results
# ============================================================================

cat("\n=========== NATIONAL DECOMPOSITION RESULTS (2010-2019) ===========\n\n")
cat(sprintf("Total Spending 2010:   $%s\n", format(round(national$spend_2010), big.mark=",")))
cat(sprintf("Total Spending 2019:   $%s\n", format(round(national$spend_2019), big.mark=",")))
cat(sprintf("Total Spending Change: $%s\n\n", format(round(national$delta_spend), big.mark=",")))

cat("Factor Contributions:\n")
cat(sprintf("  1. Population Size:      $%15s  (%6.1f%%)\n", 
            format(round(national$pop_size_effect), big.mark=","), national$pct_pop_size))
cat(sprintf("  2. Prevalence Rate:      $%15s  (%6.1f%%)\n", 
            format(round(national$prevalence_rate_effect), big.mark=","), national$pct_prevalence_rate))
cat(sprintf("  3. Case Composition:     $%15s  (%6.1f%%)\n", 
            format(round(national$case_composition_effect), big.mark=","), national$pct_case_composition))
cat(sprintf("  4. Spending Intensity:   $%15s  (%6.1f%%)\n", 
            format(round(national$spend_intensity_effect), big.mark=","), national$pct_spend_intensity))

cat(sprintf("\nDecomposition Validation (sum - actual, should be ~0): %.6f\n", national$diff_check))

cat("\n=========== TOP 10 STATES BY SPENDING CHANGE ===========\n")
print(by_state[order(-delta_spend)][1:10, .(location_name, delta_spend, 
                                            pct_pop_size, pct_prevalence_rate, 
                                            pct_case_composition, pct_spend_intensity, diff_check)])

# ============================================================================
# STEP 6: Save results
# ============================================================================

write.csv(by_state, file.path(dir_output, "T6_HIV_decomp_by_state.csv"), row.names = FALSE)
write.csv(national, file.path(dir_output, "T7_HIV_decomp_national.csv"), row.names = FALSE)

cat(sprintf("\nResults saved to: %s\n", dir_output))



# ============================================================================
# STEP 7: Visualization of Decomposition Results
# ============================================================================

# ----------------------------------------------------------------------------
# 7.1: Prepare data for plotting
# ----------------------------------------------------------------------------

# Reshape by_state to long format for stacked bar
plot_data <- by_state %>%
  select(location_id, location_name, delta_spend,
         pop_size_effect, prevalence_rate_effect, 
         case_composition_effect, spend_intensity_effect) %>%
  pivot_longer(
    cols = c(pop_size_effect, prevalence_rate_effect, 
             case_composition_effect, spend_intensity_effect),
    names_to = "factor",
    values_to = "effect"
  ) %>%
  mutate(
    # Clean factor names for legend
    factor = case_when(
      factor == "pop_size_effect" ~ "Population Size",
      factor == "prevalence_rate_effect" ~ "Prevalence Rate",
      factor == "case_composition_effect" ~ "Case Composition (Age-Sex)",
      factor == "spend_intensity_effect" ~ "Spending Intensity"
    ),
    # Order factors for stacking
    factor = factor(factor, levels = c("Population Size", 
                                       "Prevalence Rate",
                                       "Case Composition (Age-Sex)", 
                                       "Spending Intensity"))
  )
# ----------------------------------------------------------------------------
# 7.3: State-Level Stacked Horizontal Bar Chart (ALL STATES)
# ----------------------------------------------------------------------------

# Prepare plot data for ALL states
plot_data_all <- plot_data %>%
  mutate(
    # Reorder states by total spending change
    location_name = factor(location_name, 
                           levels = by_state %>% 
                             arrange(delta_spend) %>% 
                             pull(location_name))
  )

# Create stacked horizontal bar chart for ALL STATES
p_states_all <- ggplot(plot_data_all, aes(x = location_name, y = effect/1e6, fill = factor)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", alpha = 0.5) +
  # Add total change marker
  geom_point(data = by_state %>% 
               mutate(location_name = factor(location_name, 
                                             levels = by_state %>% 
                                               arrange(delta_spend) %>% 
                                               pull(location_name))),
             aes(x = location_name, y = delta_spend/1e6),
             inherit.aes = FALSE, shape = 18, size = 2.5, color = "black") +
  coord_flip() +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(labels = dollar_format(suffix = "M")) +
  labs(
    title = "HIV/AIDS Spending Decomposition by State (2010-2019)",
    subtitle = "All states ordered by total spending change | Diamond = Total Change",
    x = "",
    y = "Effect on Spending Change (Millions $)",
    fill = "Factor"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10),
    axis.text.y = element_text(size = 7),
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    panel.grid.minor = element_blank()
  ) +
  guides(fill = guide_legend(nrow = 1))

print(p_states_all)
ggsave(file.path(dir_output, "F4_HIV_decomp_all_states_stacked_bar.png"), p_states_all, 
       width = 10, height = 14, dpi = 300)

# ----------------------------------------------------------------------------
# 7.4: National Bar Chart (Separate Figure)
# ----------------------------------------------------------------------------

# National bar chart (horizontal to match state style)
national_plot_data <- national %>%
  select(pop_size_effect, prevalence_rate_effect, 
         case_composition_effect, spend_intensity_effect) %>%
  pivot_longer(
    cols = everything(),
    names_to = "factor",
    values_to = "effect"
  ) %>%
  mutate(
    factor = case_when(
      factor == "pop_size_effect" ~ "Population Size",
      factor == "prevalence_rate_effect" ~ "Prevalence Rate",
      factor == "case_composition_effect" ~ "Case Composition (Age-Sex)",
      factor == "spend_intensity_effect" ~ "Spending Intensity"
    ),
    factor = factor(factor, levels = c("Population Size", 
                                       "Prevalence Rate",
                                       "Case Composition (Age-Sex)", 
                                       "Spending Intensity")),
    location_name = "United States"
  )

p_national <- ggplot(national_plot_data, aes(x = location_name, y = effect/1e9, fill = factor)) +
  geom_bar(stat = "identity", position = "stack", width = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", alpha = 0.5) +
  geom_point(aes(x = location_name, y = national$delta_spend/1e9),
             inherit.aes = FALSE, shape = 18, size = 4, color = "black") +
  coord_flip() +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(labels = dollar_format(suffix = "B")) +
  labs(
    title = "National HIV/AIDS Spending Decomposition (2010-2019)",
    subtitle = paste0("Total spending change: $", 
                      format(round(national$delta_spend/1e9, 2), nsmall = 2), 
                      " billion | Diamond = Total Change"),
    x = "",
    y = "Effect on Spending Change (Billions $)",
    fill = "Factor"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10),
    axis.text.y = element_text(size = 12, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    panel.grid.minor = element_blank()
  ) +
  guides(fill = guide_legend(nrow = 1))

print(p_national)
ggsave(file.path(dir_output, "F5_HIV_decomp_national_bar.png"), p_national, 
       width = 10, height = 4, dpi = 300)


# ----------------------------------------------------------------------------
# 7.3: State-Level Stacked Horizontal Bar Chart (Top 15 States)
# ----------------------------------------------------------------------------
# Select top 15 states by absolute spending change

top_states <- by_state %>%
  arrange(desc(abs(delta_spend))) %>%
  head(15) %>%
  pull(location_name)
plot_data_top <- plot_data %>%
  filter(location_name %in% top_states) %>%
  mutate(
    # Reorder states by total spending change
    location_name = factor(location_name, 
                           levels = by_state %>% 
                             filter(location_name %in% top_states) %>%
                             arrange(delta_spend) %>% 
                             pull(location_name))
  )
# Create stacked horizontal bar chart
p_states <- ggplot(plot_data_top, aes(x = location_name, y = effect/1e6, fill = factor)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", alpha = 0.5) +
  # Add total change marker
  geom_point(data = by_state %>% 
               filter(location_name %in% top_states) %>%
               mutate(location_name = factor(location_name, 
                                             levels = by_state %>% 
                                               filter(location_name %in% top_states) %>%
                                               arrange(delta_spend) %>% 
                                               pull(location_name))),
             aes(x = location_name, y = delta_spend/1e6),
             inherit.aes = FALSE, shape = 18, size = 3, color = "black") +
  coord_flip() +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(labels = dollar_format(suffix = "M")) +
  labs(
    title = "HIV/AIDS Spending Decomposition by State (2010-2019)",
    subtitle = "Top 15 states by absolute spending change | Diamond = Total Change",
    x = "",
    y = "Effect on Spending Change (Millions $)",
    fill = "Factor"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    panel.grid.minor = element_blank()
  ) +
  guides(fill = guide_legend(nrow = 2))
print(p_states)
ggsave(file.path(dir_output, "F6_HIV_decomp_15_states_stacked_bar.png"), p_states, 
       width = 10, height = 8, dpi = 300)


cat("\n=========== VISUALIZATIONS SAVED ===========\n")
cat(sprintf("Files saved to: %s\n", dir_output))
cat("  - decomp_all_states_stacked_bar.png (all 51 states)\n")
cat("  - decomp_national_bar.png\n")

###############################################

# ============================================================================
# DALY DECOMPOSITION + SPENDING EFFECTIVENESS
# ============================================================================

###############################################################################
# PART A: DALY DECOMPOSITION (parallel to spending decomposition)
###############################################################################

# ----------------------------------------------------------------------------
# A.1: Prepare DALY data with same 4-factor identity
# ----------------------------------------------------------------------------

# Start fresh from df_decomp_p (same filtered data you used for spending)
dt_daly <- as.data.table(df_decomp_p)

# ---- State-level totals ----
dt_daly[, pop_state_2010 := sum(population_2010), by = .(cause_id, location_id)]
dt_daly[, pop_state_2019 := sum(population_2019), by = .(cause_id, location_id)]

dt_daly[, prev_state_2010 := sum(prevalence_counts_2010), by = .(cause_id, location_id)]
dt_daly[, prev_state_2019 := sum(prevalence_counts_2019), by = .(cause_id, location_id)]

# ---- FACTOR 1: Population size (state total) ----
dt_daly[, population_2010 := pop_state_2010]
dt_daly[, population_2019 := pop_state_2019]

# ---- FACTOR 2: Case rate (state prevalence rate = cases per person) ----
dt_daly[, case_rate_2010 := fifelse(pop_state_2010 > 0, prev_state_2010 / pop_state_2010, 0)]
dt_daly[, case_rate_2019 := fifelse(pop_state_2019 > 0, prev_state_2019 / pop_state_2019, 0)]

# ---- FACTOR 3: Age-sex structure (share of cases in this cell) ----
dt_daly[, age_sex_frac_2010 := fifelse(prev_state_2010 > 0, prevalence_counts_2010 / prev_state_2010, 0)]
dt_daly[, age_sex_frac_2019 := fifelse(prev_state_2019 > 0, prevalence_counts_2019 / prev_state_2019, 0)]

# ---- FACTOR 4: DALYs per case (instead of spending per case) ----
dt_daly[, daly_per_case_2010 := fifelse(prevalence_counts_2010 > 0, daly_counts_2010 / prevalence_counts_2010, 0)]
dt_daly[, daly_per_case_2019 := fifelse(prevalence_counts_2019 > 0, daly_counts_2019 / prevalence_counts_2019, 0)]

# Keep original DALYs for validation
dt_daly[, daly_2010 := daly_counts_2010]
dt_daly[, daly_2019 := daly_counts_2019]

# ----------------------------------------------------------------------------
# A.2: Validate DALY identity
# ----------------------------------------------------------------------------

dt_daly[, daly_hat_2010 := population_2010 * case_rate_2010 * age_sex_frac_2010 * daly_per_case_2010]
dt_daly[, daly_hat_2019 := population_2019 * case_rate_2019 * age_sex_frac_2019 * daly_per_case_2019]

dt_daly[, check_2010 := daly_2010 - daly_hat_2010]
dt_daly[, check_2019 := daly_2019 - daly_hat_2019]

cat("\n============ DALY IDENTITY VALIDATION ============\n")
cat("Residuals should be ~0 (tiny numeric noise only)\n\n")
cat("2010 residuals:\n")
print(summary(dt_daly$check_2010))
cat("\n2019 residuals:\n")
print(summary(dt_daly$check_2019))

# ----------------------------------------------------------------------------
# A.3: Run DALY decomposition
# ----------------------------------------------------------------------------

factor_names_daly <- c("population", "case_rate", "age_sex_frac", "daly_per_case")
dt_daly <- decompose(dt_daly, factor_names = factor_names_daly, start_year = 2010, end_year = 2019)

# Calculate DALY change
dt_daly[, delta_daly := daly_2019 - daly_2010]

# ----------------------------------------------------------------------------
# A.4: Aggregate DALY decomposition results
# ----------------------------------------------------------------------------

# National DALY totals
national_daly <- dt_daly[, .(
  daly_pop_size_effect = sum(population_effect, na.rm = TRUE),
  daly_prevalence_rate_effect = sum(case_rate_effect, na.rm = TRUE),
  daly_case_composition_effect = sum(age_sex_frac_effect, na.rm = TRUE),
  daly_intensity_effect = sum(daly_per_case_effect, na.rm = TRUE),
  delta_daly = sum(delta_daly, na.rm = TRUE),
  daly_2010 = sum(daly_2010, na.rm = TRUE),
  daly_2019 = sum(daly_2019, na.rm = TRUE)
)]

# Validation
national_daly[, sum_effects := daly_pop_size_effect + daly_prevalence_rate_effect + 
                daly_case_composition_effect + daly_intensity_effect]
national_daly[, diff_check := delta_daly - sum_effects]

# By state DALY
by_state_daly <- dt_daly[, .(
  daly_pop_size_effect = sum(population_effect, na.rm = TRUE),
  daly_prevalence_rate_effect = sum(case_rate_effect, na.rm = TRUE),
  daly_case_composition_effect = sum(age_sex_frac_effect, na.rm = TRUE),
  daly_intensity_effect = sum(daly_per_case_effect, na.rm = TRUE),
  delta_daly = sum(delta_daly, na.rm = TRUE),
  daly_2010 = sum(daly_2010, na.rm = TRUE),
  daly_2019 = sum(daly_2019, na.rm = TRUE)
), by = .(location_id, location_name)]

# Validation
by_state_daly[, sum_effects := daly_pop_size_effect + daly_prevalence_rate_effect + 
                daly_case_composition_effect + daly_intensity_effect]
by_state_daly[, diff_check := delta_daly - sum_effects]

cat("\n=========== NATIONAL DALY DECOMPOSITION RESULTS (2010-2019) ===========\n\n")
cat(sprintf("Total DALYs 2010:   %s\n", format(round(national_daly$daly_2010), big.mark=",")))
cat(sprintf("Total DALYs 2019:   %s\n", format(round(national_daly$daly_2019), big.mark=",")))
cat(sprintf("Total DALY Change:  %s\n\n", format(round(national_daly$delta_daly), big.mark=",")))

cat("Factor Contributions:\n")
cat(sprintf("  1. Population Size:      %15s\n", format(round(national_daly$daly_pop_size_effect), big.mark=",")))
cat(sprintf("  2. Prevalence Rate:      %15s\n", format(round(national_daly$daly_prevalence_rate_effect), big.mark=",")))
cat(sprintf("  3. Case Composition:     %15s\n", format(round(national_daly$daly_case_composition_effect), big.mark=",")))
cat(sprintf("  4. DALYs per Case:       %15s\n", format(round(national_daly$daly_intensity_effect), big.mark=",")))
cat(sprintf("\nDecomposition Validation (should be ~0): %.6f\n", national_daly$diff_check))


###############################################################################
# PART B: SPENDING EFFECTIVENESS TABLE
###############################################################################

# ----------------------------------------------------------------------------
# B.1: Merge spending and DALY decomposition results
# ----------------------------------------------------------------------------

# Merge by_state (spending) with by_state_daly
spend_eff_table <- merge(
  by_state[, .(location_id, location_name, 
               spend_2010, spend_2019, delta_spend,
               spend_intensity_effect)],
  by_state_daly[, .(location_id, location_name,
                    daly_2010, daly_2019, delta_daly,
                    daly_intensity_effect)],
  by = c("location_id", "location_name")
)

# ----------------------------------------------------------------------------
# B.2: Calculate spending effectiveness using decomposition effects
# ----------------------------------------------------------------------------

# DALYs AVERTED per case effect = negative of DALY intensity effect
# (If DALYs per case decreased, that's positive DALYs averted)
spend_eff_table[, daly_averted_effect := -daly_intensity_effect]

# Spending effectiveness = Change in spending per case / Change in DALYs averted per case
# Using decomposition effects (IHME method)
spend_eff_table[, spend_effectiveness := fifelse(
  daly_averted_effect != 0,
  spend_intensity_effect / daly_averted_effect,
  NA_real_
)]

# ----------------------------------------------------------------------------
# B.3: Categorize results (per IHME methodology)
# ----------------------------------------------------------------------------

spend_eff_table[, category := fcase(
  spend_intensity_effect > 0 & daly_averted_effect > 0, 1L,  # +Spend, +Health (report ratio)
  spend_intensity_effect < 0 & daly_averted_effect > 0, 2L,  # -Spend, +Health (cost-saving)
  spend_intensity_effect > 0 & daly_averted_effect < 0, 3L,  # +Spend, -Health (dominated)
  spend_intensity_effect < 0 & daly_averted_effect < 0, 4L,  # -Spend, -Health (excluded)
  default = NA_integer_
)]

# ASCII-safe category labels (no Unicode arrows)
spend_eff_table[, category_label := fcase(
  category == 1L, "Category 1: +Spend, +Health",
  category == 2L, "Category 2: Cost-saving",
  category == 3L, "Category 3: Dominated",
  category == 4L, "Category 4: -Both",
  default = "N/A"
)]

# Interpretation column
spend_eff_table[, interpretation := fcase(
  category == 1L, paste0("$", format(round(spend_effectiveness), big.mark = ","), " per DALY averted"),
  category == 2L, "Cost-saving (less spending, better health)",
  category == 3L, "Dominated (more spending, worse health)",
  category == 4L, "Excluded from ratio calculation",
  default = "N/A"
)]

# ----------------------------------------------------------------------------
# B.4: Add simple (non-decomposition) spending effectiveness for comparison
# ----------------------------------------------------------------------------

# Simple method: (Spend/Prev in 2019 - Spend/Prev in 2010) / (DALY/Prev in 2010 - DALY/Prev in 2019)
# Get state-level prevalence
state_prev <- dt[, .(
  prev_2010 = sum(prevalence_counts_2010),
  prev_2019 = sum(prevalence_counts_2019)
), by = .(location_id)]

spend_eff_table <- merge(spend_eff_table, state_prev, by = "location_id")

# Simple calculations
spend_eff_table[, spend_per_case_2010 := spend_2010 / prev_2010]
spend_eff_table[, spend_per_case_2019 := spend_2019 / prev_2019]
spend_eff_table[, daly_per_case_2010 := daly_2010 / prev_2010]
spend_eff_table[, daly_per_case_2019 := daly_2019 / prev_2019]

spend_eff_table[, change_spend_per_case := spend_per_case_2019 - spend_per_case_2010]
spend_eff_table[, change_daly_averted_per_case := daly_per_case_2010 - daly_per_case_2019]  # Note: flipped for "averted"

spend_eff_table[, spend_effectiveness_simple := fifelse(
  change_daly_averted_per_case != 0,
  change_spend_per_case / change_daly_averted_per_case,
  NA_real_
)]

# ----------------------------------------------------------------------------
# B.5: Create state-level table
# ----------------------------------------------------------------------------

state_spend_eff <- spend_eff_table[, .(
  Level = location_name,
  `Spending 2010` = spend_2010,
  `Spending 2019` = spend_2019,
  `Spending Change` = delta_spend,
  `DALYs 2010` = daly_2010,
  `DALYs 2019` = daly_2019,
  `DALY Change` = delta_daly,
  `Spend per Case 2010` = spend_per_case_2010,
  `Spend per Case 2019` = spend_per_case_2019,
  `DALY per Case 2010` = daly_per_case_2010,
  `DALY per Case 2019` = daly_per_case_2019,
  `Change in Spend per Case` = change_spend_per_case,
  `Change in DALYs Averted per Case` = change_daly_averted_per_case,
  `Spend Intensity Effect (Decomp)` = spend_intensity_effect,
  `DALY Averted Effect (Decomp)` = daly_averted_effect,
  `Spending Effectiveness - Simple` = spend_effectiveness_simple,
  `Spending Effectiveness - Decomp` = spend_effectiveness,
  Category = category,
  `Category Label` = category_label,
  Interpretation = interpretation
)]

# Order by state name
state_spend_eff <- state_spend_eff[order(Level)]

# ----------------------------------------------------------------------------
# B.6: Create national row and combine with states
# ----------------------------------------------------------------------------

# Calculate national simple spending effectiveness
national_prev_2010 <- sum(spend_eff_table$prev_2010)
national_prev_2019 <- sum(spend_eff_table$prev_2019)
national_spend_pc_2010 <- national$spend_2010 / national_prev_2010
national_spend_pc_2019 <- national$spend_2019 / national_prev_2019
national_daly_pc_2010 <- national_daly$daly_2010 / national_prev_2010
national_daly_pc_2019 <- national_daly$daly_2019 / national_prev_2019
national_change_spend_pc <- national_spend_pc_2019 - national_spend_pc_2010
national_change_daly_averted_pc <- national_daly_pc_2010 - national_daly_pc_2019
national_spend_eff_simple <- national_change_spend_pc / national_change_daly_averted_pc
national_spend_eff_decomp <- national$spend_intensity_effect / (-national_daly$daly_intensity_effect)

# Determine national category
national_category <- fcase(
  national$spend_intensity_effect > 0 & (-national_daly$daly_intensity_effect) > 0, 1L,
  national$spend_intensity_effect < 0 & (-national_daly$daly_intensity_effect) > 0, 2L,
  national$spend_intensity_effect > 0 & (-national_daly$daly_intensity_effect) < 0, 3L,
  national$spend_intensity_effect < 0 & (-national_daly$daly_intensity_effect) < 0, 4L,
  default = NA_integer_
)

national_category_label <- fcase(
  national_category == 1L, "Category 1: +Spend, +Health",
  national_category == 2L, "Category 2: Cost-saving",
  national_category == 3L, "Category 3: Dominated",
  national_category == 4L, "Category 4: -Both",
  default = "N/A"
)

national_interpretation <- fcase(
  national_category == 1L, paste0("$", format(round(national_spend_eff_decomp), big.mark = ","), " per DALY averted"),
  national_category == 2L, "Cost-saving (less spending, better health)",
  national_category == 3L, "Dominated (more spending, worse health)",
  national_category == 4L, "Excluded from ratio calculation",
  default = "N/A"
)

# Create national row
national_row <- data.table(
  Level = "United States (National)",
  `Spending 2010` = national$spend_2010,
  `Spending 2019` = national$spend_2019,
  `Spending Change` = national$delta_spend,
  `DALYs 2010` = national_daly$daly_2010,
  `DALYs 2019` = national_daly$daly_2019,
  `DALY Change` = national_daly$delta_daly,
  `Spend per Case 2010` = national_spend_pc_2010,
  `Spend per Case 2019` = national_spend_pc_2019,
  `DALY per Case 2010` = national_daly_pc_2010,
  `DALY per Case 2019` = national_daly_pc_2019,
  `Change in Spend per Case` = national_change_spend_pc,
  `Change in DALYs Averted per Case` = national_change_daly_averted_pc,
  `Spend Intensity Effect (Decomp)` = national$spend_intensity_effect,
  `DALY Averted Effect (Decomp)` = -national_daly$daly_intensity_effect,
  `Spending Effectiveness - Simple` = national_spend_eff_simple,
  `Spending Effectiveness - Decomp` = national_spend_eff_decomp,
  Category = national_category,
  `Category Label` = national_category_label,
  Interpretation = national_interpretation
)

# Combine national + states (national first)
final_spend_eff <- rbind(national_row, state_spend_eff)

# ----------------------------------------------------------------------------
# B.7: Combine DALY decomposition tables (national + states)
# ----------------------------------------------------------------------------

# Create national DALY row with same columns as by_state_daly
national_daly_row <- data.table(
  location_id = NA_integer_,
  location_name = "United States (National)",
  daly_pop_size_effect = national_daly$daly_pop_size_effect,
  daly_prevalence_rate_effect = national_daly$daly_prevalence_rate_effect,
  daly_case_composition_effect = national_daly$daly_case_composition_effect,
  daly_intensity_effect = national_daly$daly_intensity_effect,
  delta_daly = national_daly$delta_daly,
  daly_2010 = national_daly$daly_2010,
  daly_2019 = national_daly$daly_2019,
  sum_effects = national_daly$sum_effects,
  diff_check = national_daly$diff_check
)

# Combine national + states
final_daly_decomp <- rbind(national_daly_row, by_state_daly)

# ----------------------------------------------------------------------------
# B.8: Print summary
# ----------------------------------------------------------------------------

cat("\n=========== SPENDING EFFECTIVENESS SUMMARY ===========\n\n")

cat("NATIONAL:\n")
cat(sprintf("  Spend Intensity Effect:        $%s\n", 
            format(round(national$spend_intensity_effect), big.mark = ",")))
cat(sprintf("  DALY Averted Effect:           %s DALYs\n", 
            format(round(-national_daly$daly_intensity_effect), big.mark = ",")))
cat(sprintf("  Spending Effectiveness:        $%s per DALY averted\n",
            format(round(national_spend_eff_decomp), big.mark = ",")))
cat(sprintf("  Category:                      %d\n", national_category))

cat("\nSTATE-LEVEL CATEGORY DISTRIBUTION:\n")
print(table(spend_eff_table$category_label))

cat("\nTOP 10 STATES BY SPENDING EFFECTIVENESS (Category 1 only):\n")
print(final_spend_eff[Category == 1][order(`Spending Effectiveness - Decomp`)][1:10, 
                                                                               .(Level, `Spending Effectiveness - Decomp`, Interpretation)])

cat("\nCOST-SAVING STATES (Category 2):\n")
print(final_spend_eff[Category == 2, .(Level, `Change in Spend per Case`, `Change in DALYs Averted per Case`)])

# ----------------------------------------------------------------------------
# B.9: Save results (2 tables instead of 4)
# ----------------------------------------------------------------------------

write.csv(final_spend_eff, file.path(dir_output, "T3_HIV_spending_effectiveness.csv"), row.names = FALSE)
write.csv(final_daly_decomp, file.path(dir_output, "T8_HIV_decomp_daly.csv"), row.names = FALSE)

cat(sprintf("\n=========== FILES SAVED TO: %s ===========\n", dir_output))
cat("  - T3_HIV_spending_effectiveness.csv (national + all states)\n")
cat("  - T8_HIV_decomp_daly.csv (national + all states)\n")

