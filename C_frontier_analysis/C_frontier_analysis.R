##----------------------------------------------------------------
##' Title: C_frontier_analysis.R
##'
##' Purpose: TBD
##----------------------------------------------------------------

##----------------------------------------------------------------
## Clear environment and set library paths
##----------------------------------------------------------------
rm(list = ls())
pacman::p_load(data.table, arrow, tidyverse, glue)

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

library('frontier')
source("/ihme/cc_resources/libraries/current/r/get_age_metadata.R")

##----------------------------------------------------------------
## 0.0 Functions
##----------------------------------------------------------------
ensure_dir_exists <- function(dir_path) {
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
}

'%nin%' <- Negate('%in%')

##----------------------------------------------------------------
## 0.1 Set directories for DEX estimate data / county estimates
##----------------------------------------------------------------
# Set path for data
date_dex <- "20251123"
fp_dex <- file.path(h, "/aim_outputs/Aim2/B_aggregation/", date_dex, "/compiled_dex_data_2010_2019.parquet")

date_gbd <- "20260113"
fp_gbd <- file.path(h, "/aim_outputs/Aim2/A_data_preparation/", date_gbd, "/FA/df_gbd.parquet")

# date_ushd <- "20251204"
# fp_ushd <- file.path(h, "/aim_outputs/Aim2/B_aggregation/", date_ushd, "/compiled_ushd_data_2010_2019.parquet")

# Set output directories
date_today <- format(Sys.time(), "%Y%m%d")
dir_output <- file.path(h, "/aim_outputs/Aim2/C_frontier_analysis/", date_today)
ensure_dir_exists(dir_output)

##----------------------------------------------------------------
## 0.2 Read in data
##----------------------------------------------------------------
# DEX data - filter down to state level, payers = all
ds_des <- open_dataset(fp_dex)

df_dex <- ds_des %>%
  filter(geo == "state") %>%
  #filter(payer == "all") %>% # Need different payers
  select(c("year_id", "geo", "location_name", "fips", "toc", "payer",
           "acause", "cause_name", "age_group_years_start", "age_name", 
           "sex_id", "spend_mean", "spend_lower", "spend_upper")) %>%
  collect()

# Add location_id to DEX data
df_state_names <- fread("/ihme/dex/us_county/maps/states.csv")

df_state_names <- df_state_names %>%
  select(c("state_name", "location_id"))

df_dex <- left_join(
  x = df_dex,
  y = df_state_names,
  by = c("location_name" = "state_name")
)

# GBD Prevalence, Mortality, Population data
df_gbd <- read_parquet(fp_gbd)

# Modify age group names to match DEX age group names
df_gbd <- df_gbd %>%
  mutate(age_name = case_when(
    age_group_name == "0 - <1"   ~ "0 - <1",
    age_group_name == "1 - <5"   ~ "1 - <5",
    age_group_name == "5 to 9"   ~ "5 - <10",
    age_group_name == "10 to 14" ~ "10 - <15",
    age_group_name == "15 to 19" ~ "15 - <20",
    age_group_name == "20 to 24" ~ "20 - <25",
    age_group_name == "25 to 29" ~ "25 - <30",
    age_group_name == "30 to 34" ~ "30 - <35",
    age_group_name == "35 to 39" ~ "35 - <40",
    age_group_name == "40 to 44" ~ "40 - <45",
    age_group_name == "45 to 49" ~ "45 - <50",
    age_group_name == "50 to 54" ~ "50 - <55",
    age_group_name == "55 to 59" ~ "55 - <60",
    age_group_name == "60 to 64" ~ "60 - <65",
    age_group_name == "65 to 69" ~ "65 - <70",
    age_group_name == "70 to 74" ~ "70 - <75",
    age_group_name == "75 to 79" ~ "75 - <80",
    age_group_name == "80 to 84" ~ "80 - <85",
    age_group_name == "85+"      ~ "85+"
  ))

df_gbd <- df_gbd %>%
  ungroup() %>%
  select(!c("age_group_name"))

##----------------------------------------------------------------
## 1. Collapse & Merge DEX & GBD data
##----------------------------------------------------------------
# Collapse on TOC in DEX data
df_dex <- df_dex %>%
  group_by(year_id, geo, location_name, location_id, fips, payer, acause, cause_name, age_group_years_start, age_name, sex_id,) %>%
  summarize(spend_mean = sum(spend_mean))

# Create "_subs" acause
df_dex_sud <- df_dex %>%
  filter(acause != "hiv")

df_dex_sud <- df_dex_sud %>%
  group_by(year_id, geo, location_name, location_id, fips, payer, age_group_years_start, age_name, sex_id) %>%
  summarize(spend_mean = sum(spend_mean)) %>%
  mutate(
    acause = "_subs",
    cause_name = "Substance use disorders"
    )

# Filter on "hiv" acause"
df_dex_hiv <- df_dex %>% 
  filter(acause == "hiv")

# Rbind back "_subs" & "hiv"
df_dex <- rbind(df_dex_hiv, df_dex_sud)

# Pivot wider to have columns for all different payer types
df_dex_pivot <- df_dex %>% pivot_wider(
  names_from  = payer,
  values_from = spend_mean,
  names_prefix = "spend_"
)

# Merge DEX & GBD data
df_m <- left_join(
  x = df_gbd,
  y = df_dex_pivot,
  by = c("location_id", "sex_id", "year_id", "acause", "cause_name", "location_name", "age_name")
)

##----------------------------------------------------------------
## 2. Collapse on sex_id
##----------------------------------------------------------------
df_m <- df_m %>%
  group_by(cause_id, year_id, location_id, location_name, acause, cause_name, age_name, age_group_years_start) %>%
  summarise(
    spend_all = sum(spend_all, na.rm = TRUE),
    spend_mdcd = sum(spend_mdcd, na.rm = TRUE),
    spend_mdcr = sum(spend_mdcr, na.rm = TRUE),
    spend_oop = sum(spend_oop, na.rm = TRUE),
    spend_priv = sum(spend_priv, na.rm = TRUE),
    mortality_counts = sum(mortality_counts),
    prevalence_counts = sum(prevalence_counts),
    daly_counts = sum(daly_counts),
    incidence_counts = sum(incidence_counts),
    population = sum(population),
    .groups = "drop"
  )

##----------------------------------------------------------------
## 3. Create spend_prev_ratio & mort_prev_ratio columns
##----------------------------------------------------------------
df_m <- df_m %>%
  mutate(
    spend_prev_ratio = (spend_all / prevalence_counts),
    mort_prev_ratio = (mortality_counts / prevalence_counts),
    daly_prev_ratio = (daly_counts / prevalence_counts)
  )

##----------------------------------------------------------------
## 4. Format age weights to match DEX age bins
## 
## To read more about GBD age weights and where the below values come from, read:
## https://scicomp-docs.ihme.washington.edu/db_queries/current/get_age_metadata.html
## under "Age group weight"
##----------------------------------------------------------------
# Pull GBD age weights
df_age <- get_age_metadata(release_id = 16)

# Label the 0 - <1, 1 - <5, & 85+ age groups 
df_age <- df_age %>%
  mutate(
    age_name_group = case_when(
      age_group_id %in% c(2, 3, 388, 389) ~ "0 - <1",
      age_group_id %in% c(238, 34) ~ "1 - <5",
      age_group_id %in% c(31, 32, 235) ~ "85+"
                              )
  )

# Filter out the age groups we won't collapse
df_age_non_collapse <- df_age %>%
  filter(is.na(age_name_group))

# Collapse on 0 - <1, 1 - <5, & 85+ age groups 
df_age <- df_age %>%
  filter(!is.na(age_name_group)) %>%
  group_by(age_name_group) %>%
  summarize(age_group_weight_value = sum(age_group_weight_value))

df_age <- df_age %>%
  mutate(
    age_group_years_start = case_when(
      age_name_group == "0 - <1" ~ 0,
      age_name_group == "1 - <5" ~ 1,
      age_name_group == "85+" ~ 85,
    )
  )

df_age <- df_age %>%
  rename(
    "age_group_name" = "age_name_group"
  )

# Rbind age weights back together
df_age_weights <- rbind(df_age_non_collapse, df_age, fill = TRUE)

# Modify age group names to match DEX age group names
df_age_weights <- df_age_weights %>%
  mutate(age_group_name = case_when(
    age_group_name == "0 - <1"   ~ "0 - <1",
    age_group_name == "1 - <5"   ~ "1 - <5",
    age_group_name == "5 to 9"   ~ "5 - <10",
    age_group_name == "10 to 14" ~ "10 - <15",
    age_group_name == "15 to 19" ~ "15 - <20",
    age_group_name == "20 to 24" ~ "20 - <25",
    age_group_name == "25 to 29" ~ "25 - <30",
    age_group_name == "30 to 34" ~ "30 - <35",
    age_group_name == "35 to 39" ~ "35 - <40",
    age_group_name == "40 to 44" ~ "40 - <45",
    age_group_name == "45 to 49" ~ "45 - <50",
    age_group_name == "50 to 54" ~ "50 - <55",
    age_group_name == "55 to 59" ~ "55 - <60",
    age_group_name == "60 to 64" ~ "60 - <65",
    age_group_name == "65 to 69" ~ "65 - <70",
    age_group_name == "70 to 74" ~ "70 - <75",
    age_group_name == "75 to 79" ~ "75 - <80",
    age_group_name == "80 to 84" ~ "80 - <85",
    age_group_name == "85+"      ~ "85+"
  ))

# Select columns we want
df_age_weights <- df_age_weights %>%
  select(c("age_group_name", "age_group_years_start", "age_group_weight_value"))

##----------------------------------------------------------------
## 5. Apply age-standardization
##----------------------------------------------------------------
# Join age weights to data
df_as <- left_join(
  x = df_m,
  y = df_age_weights,
  by = c("age_name" = "age_group_name", "age_group_years_start")
)

# Create age-standardized ratios based on non-sexed GBD age weights (collapsing age groups here) 
df_as <- df_as %>%
  group_by(cause_id, year_id, location_id, location_name, acause, cause_name) %>%
  summarise(
    as_spend_prev_ratio = sum(spend_prev_ratio * age_group_weight_value, na.rm = TRUE),
    as_mort_prev_ratio  = sum(mort_prev_ratio * age_group_weight_value, na.rm = TRUE),
    as_daly_prev_ratio  = sum(daly_prev_ratio * age_group_weight_value, na.rm = TRUE),
    spend_all = sum(spend_all, na.rm = TRUE),
    spend_mdcd = sum(spend_mdcd, na.rm = TRUE),
    spend_mdcr = sum(spend_mdcr, na.rm = TRUE),
    spend_oop = sum(spend_oop, na.rm = TRUE),
    spend_priv = sum(spend_priv, na.rm = TRUE),
    mortality_counts = sum(mortality_counts),
    prevalence_counts = sum(prevalence_counts),
    daly_counts = sum(daly_counts),
    incidence_counts = sum(incidence_counts),
    population = sum(population),
    .groups = "drop"
  )

##----------------------------------------------------------------
## 6. Add Rate columns
##----------------------------------------------------------------
df_as$mortality_rates <- df_as$mortality_counts / df_as$population
df_as$prevalence_rates <- df_as$prevalence_counts / df_as$population
df_as$daly_rates <- df_as$daly_counts / df_as$population
df_as$incidence_rates <- df_as$incidence_counts / df_as$population

##----------------------------------------------------------------
## 7. Add variance column from mortality and deaths data
## Variance column needed for SFMA package 
##----------------------------------------------------------------
df_as$variance <- (df_as$mortality_counts / (df_as$prevalence_counts^2))

# Write out age-standardized data to today's dated folder in C_frontier_analysis
write.csv(x = df_as, row.names = FALSE, file = file.path(dir_output, "df_as.csv"))

##----------------------------------------------------------------
## 7. Collapse on year_id
##----------------------------------------------------------------
# Collapse on year_id
df_as_no_year <- df_as %>%
  group_by(cause_id, location_id, location_name, acause, cause_name) %>%
  summarise(
    spend_all = sum(spend_all, na.rm = TRUE),
    spend_mdcd = sum(spend_mdcd, na.rm = TRUE),
    spend_mdcr = sum(spend_mdcr, na.rm = TRUE),
    spend_oop = sum(spend_oop, na.rm = TRUE),
    spend_priv = sum(spend_priv, na.rm = TRUE),
    mortality_counts = sum(mortality_counts),
    prevalence_counts = sum(prevalence_counts),
    daly_counts = sum(daly_counts),
    incidence_counts = sum(incidence_counts),
    population = sum(population),
    
    spend_prev_ratio = spend_all / prevalence_counts,
    mort_prev_ratio  = mortality_counts / prevalence_counts,
    daly_prev_ratio  = daly_counts / prevalence_counts,
    .groups = "drop"
  )

df_as_no_year$mortality_rates <- df_as_no_year$mortality_counts / df_as_no_year$population
df_as_no_year$prevalence_rates <- df_as_no_year$prevalence_counts / df_as_no_year$population
df_as_no_year$daly_rates <- df_as_no_year$daly_counts / df_as_no_year$population
df_as_no_year$incidence_rates <- df_as_no_year$incidence_counts / df_as_no_year$population

# Save
write.csv(x = df_as_no_year, row.names = FALSE, file = file.path(dir_output, "df_as_no_year.csv"))

# ##----------------------------------------------------------------
# ## 6. Frontier Analysis Model - CURRENTLY UNUSED, PYTHON SFMA SCRIPT USED INSTEAD
# ##
# ## Formula - GBD Mortality / Prevalence Ratio is the outcome, DEX spend_mean / prevalence ratio is the predictor (+ other variables)
# ##----------------------------------------------------------------
# # Loop through our causes, create models for each, extract efficiencies 
# 
# summary(df_as)
# list_dfs <- list()
# list_models <- list()
# 
# for (cause in df_as$acause %>% unique()) {
#   print(cause)
#   
#   # Subset to our particular cause
#   df_loop <- df_as %>% filter(acause == cause)
#   
#   ##----------------------------------------------------------------
#   ## 7. Simple FA Model
#   ##----------------------------------------------------------------
#   # --- Baseline model: log(MX Ratio) on log(spending mean) -----------------
#   # This is the simplest Cobb–Douglas frontier in log-log form.
#   # ineffDecrease = TRUE means inefficiency increases MX Ratio (bad outcome).
#   # Try flipping to FALSE if you want to see the difference in orientation.
#   
#   # Simple Stochastic Frontier Example
#   # Outcome: MX Ratio (as_mort_prev_ratio)
#   # Predictor: Healthcare spending (as_spend_prev_ratio)
#   
#   # Model: MX Ratio ~ spend_mean
#   mod_simple <- frontier::sfa(
#     log(as_mort_prev_ratio) ~ log(as_spend_prev_ratio),
#     data          = df_loop,
#     ineffDecrease = TRUE
#   )
#   
#   
#   # Save model object
#   model_filename <- paste0("mod_", cause, "_simple.rds")
#   saveRDS(mod_simple, file.path(dir_output, model_filename))
#   print(paste0("Saved simple model @ ", dir_output, "/",model_filename))
#   
#   # Save model object to list
#   list_models[[cause]][["simple"]] <- mod_simple
#   
#   ##----------------------------------------------------------------
#   ## 8. Extended FA Model
#   ##----------------------------------------------------------------
#   # --- Extended model: log(MX Ratio) on log(spending mean) + controls -----------------
#   ##  * Still frontier of log(as_mort_prev_ratio) vs log(as_spend_prev_ratio),
#   ##    BUT now controls for composition (age, sex, time, county).
#   ##  * These covariates shift the frontier (what is “expected” given case-mix),
#   ##    so inefficiency is measured after conditioning on them.
#   ##
#   ##  Notes:
#   ##    - factor(year_id) accounts for secular trends in outcomes/spending.
#   mod_extended <- frontier::sfa(
#     log(as_mort_prev_ratio) ~ log(as_spend_prev_ratio) + factor(year_id) + factor(sex_id),
#     data          = df_loop,
#     ineffDecrease = TRUE
#   )
#   
#   # Save model object
#   model_filename <- paste0("mod_", cause, "_extended.rds")
#   saveRDS(mod_extended, file.path(dir_output, model_filename))
#   print(paste0("Saved extended model @ ", dir_output, "/",model_filename))
#   
#   # Save model object to list
#   list_models[[cause]][["extended"]] <- mod_extended
#   
#   ##----------------------------------------------------------------
#   ## 9. Extract efficiencies
#   ##
#   ## Efficiency scores are in [0,1], where 1 = most efficient.
#   ## Because we logged the dependent variable, set logDepVar = TRUE.
#   ## minusU = TRUE gives Farrell-type efficiencies (higher = better).
#   ##----------------------------------------------------------------
#   # Basic Model Efficiency Scores
#   df_loop$eff_simple <- efficiencies(mod_simple, 
#                                    asInData   = TRUE,
#                                    logDepVar  = TRUE,
#                                    minusU     = TRUE)
#   # Extended Model Efficiency Scores
#   df_loop$eff_extended <- efficiencies(mod_extended,
#                                      asInData   = TRUE,
#                                      logDepVar  = TRUE,
#                                      minusU     = TRUE)
#   
#   ##----------------------------------------------------------------
#   ## 10. Add df to list
#   ##----------------------------------------------------------------
#   list_dfs[[cause]] <- df_loop
# }
#   
# ##----------------------------------------------------------------
# ## 11. Combine list of dfs and save to Parquet Files
# ##----------------------------------------------------------------
# df_all <- bind_rows(list_dfs)
# 
# fn_output <- paste0("fa_estimates.parquet")
# write_parquet(df_all, file.path(dir_output, fn_output))
# 
# ##----------------------------------------------------------------
# ## TESTING - SAFE TO DELETE
# ##----------------------------------------------------------------
# ##----------------------------------------------------------------
# ## Results - UNUSED ATM
# ##----------------------------------------------------------------
# # 
# # # Create dollar mx ratio column
# # df_dex_ushd_hiv$dollar_mx_ratio <- df_dex_ushd_hiv$spend_mean / df_dex_ushd_hiv$pred_mean
# # df_dex_ushd_subs$dollar_mx_ratio <- df_dex_ushd_subs$spend_mean / df_dex_ushd_subs$pred_mean
# # 
# # View(df_dex_ushd_hiv[, c("state_name", "cnty_name", "acause", "cause_name", "sex_id", "age_name",
# #                       "pred_mean", "spend_mean", "eff_simple", "dollar_mx_ratio")])
# # View(df_dex_ushd_subs[, c("state_name", "cnty_name", "acause", "cause_name", "sex_id", "age_name",
# #                       "pred_mean", "spend_mean", "eff_simple", "dollar_mx_ratio")])
# 
# # FA TESTING PLOT NEW CODE
# 
# plot_sfa_predicted_line <- function(df_all, list_models, acause, model_type,
#                                     log_axes = TRUE) {
#   
#   df <- df_all %>%
#     filter(acause == !!acause) %>%
#     filter(is.finite(as_spend_prev_ratio), is.finite(as_mort_prev_ratio),
#            as_spend_prev_ratio > 0, as_mort_prev_ratio > 0)
#   
#   mod <- list_models[[acause]][[model_type]]
#   
#   # Predict at observed data points (avoids factor-level headaches)
#   # logDepVar = TRUE because your dependent variable is log(y) in the model
#   # IMPORTANT: predict() returns fitted values on the scale controlled by logDepVar
#   df$yhat <- predict(mod, newdata = df, logDepVar = TRUE)
#   
#   # If predict() returns log-scale, exponentiate to get back to level scale
#   # In frontier, logDepVar=TRUE typically returns predictions in original y scale,
#   # but this can vary. Quick check: compare range of yhat to y.
#   # If yhat looks like ~(-10 to 2), it's log-scale and you need exp().
#   if (all(df$yhat < 10) && all(df$yhat > -50)) {
#     # heuristic: looks like log-scale
#     df$yhat <- exp(df$yhat)
#   }
#   
#   df <- df %>% arrange(as_spend_prev_ratio)
#   
#   p <- ggplot(df, aes(x = as_spend_prev_ratio, y = as_mort_prev_ratio)) +
#     geom_point(alpha = 0.25, size = 0.8) +
#     geom_line(aes(y = yhat), color = "orange", linewidth = 1.1) +
#     theme_minimal() +
#     labs(
#       title = paste0("Observed vs Model-Predicted: ", acause, " (", model_type, ")"),
#       x = "Spending per case (age-standardized)",
#       y = "Mortality–prevalence ratio (age-standardized)"
#     )
#   
#   if (log_axes) {
#     p <- p + scale_x_log10() + scale_y_log10()
#   }
#   
#   p
# }
# 
# # Example:
# plot_sfa_predicted_line(df_all, list_models, "hiv", "simple")
# plot_sfa_predicted_line(df_all, list_models, "hiv", "extended")
# plot_sfa_predicted_line(df_all, list_models, "_subs", "simple")
# plot_sfa_predicted_line(df_all, list_models, "_subs", "extended")
# 
# 
# ##----------------------------------------------------------------
# ## Plots - TO BE MOVED TO D_FIGURES SCRIPT
# ## BELOW IS JUST FOR LOOKING AT DATA
# ##----------------------------------------------------------------
# 
# # Efficiency Score Distribution
# 
# # HIV - Basic
# ggplot(df_all %>% filter(acause == "hiv"), aes(x = eff_simple)) +
#   geom_histogram(bins = 30, fill = "steelblue", color = "white") +
#   labs(title = "Distribution of State Efficiency Scores",
#        x = "Efficiency Score", y = "Number of Counties")
# 
# # HIV - Extended
# ggplot(df_all %>% filter(acause == "hiv"), aes(x = eff_extended)) +
#   geom_histogram(bins = 30, fill = "steelblue", color = "white") +
#   labs(title = "Distribution of State Efficiency Scores",
#        x = "Efficiency Score", y = "Number of Counties")
# 
# # SUD - Basic
# ggplot(df_all %>% filter(acause == "_subs"), aes(x = eff_simple)) +
#   geom_histogram(bins = 30, fill = "steelblue", color = "white") +
#   labs(title = "Distribution of State Efficiency Scores",
#        x = "Efficiency Score", y = "Number of Counties")
# 
# # SUD - Extended
# ggplot(df_all %>% filter(acause == "_subs"), aes(x = eff_extended)) +
#   geom_histogram(bins = 30, fill = "steelblue", color = "white") +
#   labs(title = "Distribution of State Efficiency Scores",
#        x = "Efficiency Score", y = "Number of Counties")
# 
# 
# 
# 
# names(list_models)
# names(list_models[["_subs"]])
# 
# mod <- list_models[["_subs"]][["extended"]]
# is.null(mod)
# class(mod)
# inherits(mod, "sfa")



