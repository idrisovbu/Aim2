##----------------------------------------------------------------
##' Title: C_model_exploration.R
##'
##' Purpose: Explores which models have covariates that are statistically significant
##'          for the frontier analysis
##----------------------------------------------------------------

##----------------------------------------------------------------
## 0. Clear environment and set library paths
##----------------------------------------------------------------
rm(list = ls())
pacman::p_load(data.table, arrow, tidyverse, glue, broom, purrr, readr, lubridate)

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

'%nin%' <- Negate('%in%')

##----------------------------------------------------------------
## 1. Set directories
##----------------------------------------------------------------
# Set fp for age-standardized data
as_date <- "20260115"
fp_as <- file.path(h, '/aim_outputs/Aim2/C_frontier_analysis/', as_date, "df_as.csv")

# Set output directories
date_today <- format(Sys.time(), "%Y%m%d")
dir_output <- file.path(h, "/aim_outputs/Aim2/C_frontier_analysis/", date_today)
ensure_dir_exists(dir_output)

# Covariates data
fp_df_cov <- "/ihme/resource_tracking/us_value/data/sfa_covars2021_shea.csv"
fp_df_race_cov <- "/ihme/resource_tracking/us_value/data/sfa_covars_w_race_fractions.csv"
fp_aca_expansion <- file.path(h, "/aim_outputs/Aim2/R_resources/aca_expansion_formatted.csv")

##----------------------------------------------------------------
## 2. Read in data
##----------------------------------------------------------------
df_as <- read.csv(fp_as)
df_cov <- read.csv(fp_df_cov)
df_race_cov <- read.csv(fp_df_race_cov)
df_aca_expansion <- read.csv(fp_aca_expansion)

##----------------------------------------------------------------
## 3. Format ACA Expansion data
##----------------------------------------------------------------
df_aca_expansion_f <- df_aca_expansion %>%
  mutate(
    imp_year_id = case_when(
      Expansion.Implementation.Date %in% c("N/A", NA) ~ NA_integer_,
      TRUE ~ year(mdy(Expansion.Implementation.Date))
    )
  )

df_aca_expansion_f <- df_aca_expansion_f %>%
  mutate(
    imp_2010 = if_else(!is.na(imp_year_id) & imp_year_id <= 2010, 1L, 0L),
    imp_2011 = if_else(!is.na(imp_year_id) & imp_year_id <= 2011, 1L, 0L),
    imp_2012 = if_else(!is.na(imp_year_id) & imp_year_id <= 2012, 1L, 0L),
    imp_2013 = if_else(!is.na(imp_year_id) & imp_year_id <= 2013, 1L, 0L),
    imp_2014 = if_else(!is.na(imp_year_id) & imp_year_id <= 2014, 1L, 0L),
    imp_2015 = if_else(!is.na(imp_year_id) & imp_year_id <= 2015, 1L, 0L),
    imp_2016 = if_else(!is.na(imp_year_id) & imp_year_id <= 2016, 1L, 0L),
    imp_2017 = if_else(!is.na(imp_year_id) & imp_year_id <= 2017, 1L, 0L),
    imp_2018 = if_else(!is.na(imp_year_id) & imp_year_id <= 2018, 1L, 0L),
    imp_2019 = if_else(!is.na(imp_year_id) & imp_year_id <= 2019, 1L, 0L)
  )

df_aca_expansion_f <- df_aca_expansion_f %>%
  pivot_longer(
    cols = starts_with("imp_20"),
    names_to = "year_id",
    names_prefix = "imp_",
    values_to = "imp"
  ) %>%
  mutate(year_id = as.integer(year_id))

df_aca_expansion_f <- df_aca_expansion_f %>%
  setnames(
    old = c("Location", "imp"),
    new = c("location_name", "aca_implemented_status")
  )

df_aca_expansion_f <- df_aca_expansion_f %>%
  select(c("location_name", "year_id", "aca_implemented_status"))

##----------------------------------------------------------------
## 4. Filter & Merge data
##----------------------------------------------------------------
# Covariate Data
df_cov <- df_cov %>%
  filter(year_id %in% c(2010:2019))

df_race_cov <- df_race_cov %>%
  filter(year_id %in% c(2010:2019))

# Merge covariate data
df_cov <- left_join(
  x = df_cov,
  y = df_race_cov
)

df_cov <- left_join(
  x = df_cov,
  y = df_aca_expansion_f
)

# Merge covariate data w/ age-standardized data
df_as <- left_join(
  x = df_as,
  y = df_cov,
  by = c("year_id", "location_id", "location_name")
)

##----------------------------------------------------------------
## 4. Create "high_prev" variable to use as an interactive term in respective models (HIV models use HIV high_prev, SUD models use SUD high_prev)
##----------------------------------------------------------------
# Create sud_prevalence_counts column
df_sud_prev_count <- df_as %>% 
  filter(acause == "_subs") %>%
  select(c("cause_id", "year_id", "location_id", "location_name", "acause", 
           "cause_name", "prevalence_counts")) %>%
  setnames(old = "prevalence_counts", new = "sud_prevalence_counts")

df_sud_prev_count <- df_sud_prev_count %>%
  mutate(high_sud_prev = ifelse(sud_prevalence_counts >= median(sud_prevalence_counts), 1, 0))

# Create sud_prevalence_counts column
df_hiv_prev_count <- df_as %>% 
  filter(acause == "hiv") %>%
  select(c("cause_id", "year_id", "location_id", "location_name", "acause", 
           "cause_name", "prevalence_counts")) %>%
  setnames(old = "prevalence_counts", new = "hiv_prevalence_counts")

df_hiv_prev_count <- df_hiv_prev_count %>%
  mutate(high_hiv_prev = ifelse(hiv_prevalence_counts >= median(hiv_prevalence_counts), 1, 0))

# Join
df_as <- left_join(
  x = df_as,
  y = df_sud_prev_count %>% select(!c("acause", "cause_id", "cause_name")),
  by = c("year_id", "location_id", "location_name")
)

df_as <- left_join(
  x = df_as,
  y = df_hiv_prev_count %>% select(!c("acause", "cause_id", "cause_name")),
  by = c("year_id", "location_id", "location_name")
)

##----------------------------------------------------------------
## 5. Factor location_id and year_id to add as covariates
##----------------------------------------------------------------
df_as$year_id <- as.factor(df_as$year_id)
df_as$location_id <- as.factor(df_as$location_id)

##----------------------------------------------------------------
## 6. Specify Models
##----------------------------------------------------------------
# Basic Model
model_basic_mort <- "as_mort_prev_ratio ~ as_spend_prev_ratio"
model_basic_mort_interactive_sud <- "as_mort_prev_ratio ~ as_spend_prev_ratio * high_sud_prev"
model_basic_mort_interactive_hiv <- "as_mort_prev_ratio ~ as_spend_prev_ratio * high_hiv_prev"

# Covariate combinations
cov_h <- c('obesity', 'age65', 'cig_pc_10', 'phys_act_10', 'edu_yrs', 'as_spend_prev_ratio')

cov_sud <- c("prevalence_counts")

cov_aca_expansion <- c("aca_implemented_status")

cov_year_loc <- c("year_id", "location_id")

cov_race <- c("race_prop_BLCK", "race_prop_HISP")



cov_risk <- c(
  "prev_diabetes",
  "cig_pc_10",
  "phys_act_10"
)

cov_ses <- c(
  "edu_yrs",
  "ldi_pc"
)

cov_density <- c(
  "density_l.150",
  "density_g.1000"
)

# HIV - Mort / prev - Formulas
f0_hiv <- as.formula(model_basic_mort)
f1_hiv <- as.formula(paste(model_basic_mort, "+", paste(cov_sud, collapse = " + ")))
f2_hiv <- as.formula(paste(model_basic_mort, "+", paste(c(cov_sud, cov_aca_expansion), collapse = " + ")))
f3_hiv <- as.formula(paste(model_basic_mort, "+", paste(c(cov_sud, cov_aca_expansion, cov_year_loc), collapse = " + ")))
f4_hiv <- as.formula(paste(model_basic_mort, "+", paste(c(cov_sud, cov_aca_expansion, cov_year_loc, cov_race), collapse = " + ")))
f5_hiv <- as.formula(paste(model_basic_mort, "+", paste(c(cov_sud, cov_aca_expansion, cov_year_loc, cov_race, cov_risk), collapse = " + ")))
f6_hiv <- as.formula(paste(model_basic_mort, "+", paste(c(cov_sud, cov_aca_expansion, cov_year_loc, cov_race, cov_risk, cov_ses), collapse = " + ")))
f7_hiv <- as.formula(paste(model_basic_mort, "+", paste(c(cov_sud, cov_aca_expansion, cov_year_loc, cov_race, cov_risk, cov_ses, cov_density), collapse = " + ")))
f8_hiv <- as.formula(paste(model_basic_mort, "+", paste(c(cov_h), collapse = " + ")))

# HIV - Mort / prev w/ interaction term - Formulas
f0_hiv_interact <- as.formula(model_basic_mort_interactive_hiv)
f1_hiv_interact <- as.formula(paste(model_basic_mort_interactive_hiv, "+", paste(cov_sud, collapse = " + ")))
f2_hiv_interact <- as.formula(paste(model_basic_mort_interactive_hiv, "+", paste(c(cov_sud, cov_aca_expansion), collapse = " + ")))
f3_hiv_interact <- as.formula(paste(model_basic_mort_interactive_hiv, "+", paste(c(cov_sud, cov_aca_expansion, cov_year_loc), collapse = " + ")))
f4_hiv_interact <- as.formula(paste(model_basic_mort_interactive_hiv, "+", paste(c(cov_sud, cov_aca_expansion, cov_year_loc, cov_race), collapse = " + ")))
f5_hiv_interact <- as.formula(paste(model_basic_mort_interactive_hiv, "+", paste(c(cov_sud, cov_aca_expansion, cov_year_loc, cov_race, cov_risk), collapse = " + ")))
f6_hiv_interact <- as.formula(paste(model_basic_mort_interactive_hiv, "+", paste(c(cov_sud, cov_aca_expansion, cov_year_loc, cov_race, cov_risk, cov_ses), collapse = " + ")))
f7_hiv_interact <- as.formula(paste(model_basic_mort_interactive_hiv, "+", paste(c(cov_sud, cov_aca_expansion, cov_year_loc, cov_race, cov_risk, cov_ses, cov_density), collapse = " + ")))
f8_hiv_interact <- as.formula(paste(model_basic_mort_interactive_hiv, "+", paste(c(cov_h), collapse = " + ")))

# SUD - Mort / prev - Formulas
f0_sud <- as.formula(model_basic_mort)
f1_sud <- as.formula(paste(model_basic_mort, "+", paste(cov_risk, collapse = " + ")))
f2_sud <- as.formula(paste(model_basic_mort, "+", paste(c(cov_risk, cov_aca_expansion), collapse = " + ")))
f3_sud <- as.formula(paste(model_basic_mort, "+", paste(c(cov_risk, cov_aca_expansion, cov_year_loc), collapse = " + ")))
f4_sud <- as.formula(paste(model_basic_mort, "+", paste(c(cov_risk, cov_aca_expansion, cov_year_loc, cov_race), collapse = " + ")))
f5_sud <- as.formula(paste(model_basic_mort, "+", paste(c(cov_risk, cov_aca_expansion, cov_year_loc, cov_race, cov_ses), collapse = " + ")))
f6_sud <- as.formula(paste(model_basic_mort, "+", paste(c(cov_risk, cov_aca_expansion, cov_year_loc, cov_race, cov_ses, cov_density), collapse = " + ")))
f7_sud <- as.formula(paste(model_basic_mort, "+", paste(c(cov_h), collapse = " + ")))

# SUD - Mort / prev w/ interaction term - Formulas
f0_sud_interact <- as.formula(model_basic_mort_interactive_sud)
f1_sud_interact <- as.formula(paste(model_basic_mort_interactive_sud, "+", paste(cov_risk, collapse = " + ")))
f2_sud_interact <- as.formula(paste(model_basic_mort_interactive_sud, "+", paste(c(cov_risk, cov_aca_expansion), collapse = " + ")))
f3_sud_interact <- as.formula(paste(model_basic_mort_interactive_sud, "+", paste(c(cov_risk, cov_aca_expansion, cov_year_loc), collapse = " + ")))
f4_sud_interact <- as.formula(paste(model_basic_mort_interactive_sud, "+", paste(c(cov_risk, cov_aca_expansion, cov_year_loc, cov_race), collapse = " + ")))
f5_sud_interact <- as.formula(paste(model_basic_mort_interactive_sud, "+", paste(c(cov_risk, cov_aca_expansion, cov_year_loc, cov_race, cov_ses), collapse = " + ")))
f6_sud_interact <- as.formula(paste(model_basic_mort_interactive_sud, "+", paste(c(cov_risk, cov_aca_expansion, cov_year_loc, cov_race, cov_ses, cov_density), collapse = " + ")))
f7_sud_interact <- as.formula(paste(model_basic_mort_interactive_sud, "+", paste(c(cov_h), collapse = " + ")))

# Formula List
list_formulas_hiv <- list(
  m_mort_unadj                               = f0_hiv,
  m_mort_plus_sud                            = f1_hiv,
  m_mort_plus_aca_expansion                  = f2_hiv,
  m_mort_plus_year_loc                       = f3_hiv,
  m_mort_plus_race                           = f4_hiv,
  m_mort_plus_diab_cig_phys_ac               = f5_hiv,
  m_mort_plus_edu_ldi                        = f6_hiv,
  m_mort_plus_density                        = f7_hiv,
  m_mort_haley                               = f8_hiv,
  m_mort_int_unadj                           = f0_hiv_interact,
  m_mort_int_plus_sud                        = f1_hiv_interact,
  m_mort_int_plus_aca_expansion              = f2_hiv_interact,
  m_mort_int_plus_year_loc                   = f3_hiv_interact,
  m_mort_int_plus_race                       = f4_hiv_interact,
  m_mort_int_plus_diab_cig_phys_ac           = f5_hiv_interact,
  m_mort_int_plus_edu_ldi                    = f6_hiv_interact,
  m_mort_int_plus_density                    = f7_hiv_interact,
  m_mort_int_haley                           = f8_hiv_interact
)

list_formulas_sud <- list(
  m_mort_unadj                               = f0_sud,
  m_mort_plus_diab_cig_phys_ac               = f1_sud,
  m_mort_plus_aca_expansion                  = f2_sud,
  m_mort_plus_year_loc                       = f3_sud,
  m_mort_plus_race                           = f4_sud,
  m_mort_plus_edu_ldi                        = f5_sud,
  m_mort_plus_density                        = f6_sud,
  m_mort_haley                               = f7_sud,
  m_mort_int_unadj                           = f0_sud_interact,
  m_mort_int_plus_diab_cig_phys_ac           = f1_sud_interact,
  m_mort_int_plus_aca_expansion              = f2_sud_interact,
  m_mort_int_plus_year_loc                   = f3_sud_interact,
  m_mort_int_plus_race                       = f4_sud_interact,
  m_mort_int_plus_edu_ldi                    = f5_sud_interact,
  m_mort_int_plus_density                    = f6_sud_interact,
  m_mort_int_haley                           = f7_sud_interact
)


##----------------------------------------------------------------
## 6. Run Models
##----------------------------------------------------------------
list_models <- list()

# HIV 
df_hiv <- df_as %>% filter(acause == "hiv")

for (nm in names(list_formulas_hiv)) {
  list_models[[paste("hiv", nm, sep = "__")]] <- lm(list_formulas_hiv[[nm]], data = df_hiv)
}

# SUD 
df_sud <- df_as %>% filter(acause == "_subs")

for (nm in names(list_formulas_sud)) {
  list_models[[paste("_subs", nm, sep = "__")]] <- lm(list_formulas_sud[[nm]], data = df_sud)
}

##----------------------------------------------------------------
## 6. Extract coefficients from models
##----------------------------------------------------------------
coef_tbl <- imap_dfr(list_models, ~{
  broom::tidy(.x) %>%
    mutate(model_id = .y,
       signif_stars = case_when(
         p.value < 0.001 ~ "***",
         p.value < 0.01  ~ "**",
         p.value < 0.05  ~ "*",
         TRUE            ~ ""
       ),
       signif_label = case_when(
         signif_stars == "***" ~ "p < 0.001",
         signif_stars == "**"  ~ "p < 0.01",
         signif_stars == "*"   ~ "p < 0.05",
         TRUE                  ~ "not significant"
       )) %>%
    tidyr::separate(model_id, into = c("acause", "model_name"), sep = "__", remove = FALSE)
})

metrics_tbl <- imap_dfr(list_models, ~{
  g <- broom::glance(.x)
  tibble(
    model_id = .y,
    n = g$nobs,
    r2 = g$r.squared,
    adj_r2 = g$adj.r.squared,
    aic = AIC(.x),
    bic = BIC(.x),
    sigma = g$sigma
  ) %>%
    tidyr::separate(model_id, into = c("acause", "model_name"), sep = "__", remove = FALSE)
})

##----------------------------------------------------------------
## 7. Save Outputs
##----------------------------------------------------------------
write.csv(coef_tbl, file.path(dir_output, "model_coefficients.csv"))
write.csv(metrics_tbl, file.path(dir_output, "model_metrics.csv"))
