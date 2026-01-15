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
pacman::p_load(data.table, arrow, tidyverse, glue, broom, purrr, readr)

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
as_date <- "20260113"
fp_as <- file.path(h, '/aim_outputs/Aim2/C_frontier_analysis/', as_date, "df_as.csv")

# Set output directories
date_today <- format(Sys.time(), "%Y%m%d")
dir_output <- file.path(h, "/aim_outputs/Aim2/C_frontier_analysis/", date_today)
ensure_dir_exists(dir_output)

# Covariates data
fp_df_cov <- "/ihme/resource_tracking/us_value/data/sfa_covars2021_shea.csv"

##----------------------------------------------------------------
## 2. Read in data
##----------------------------------------------------------------
df_as <- read.csv(fp_as)
df_cov <- read.csv(fp_df_cov)

##----------------------------------------------------------------
## 3. Filter & Merge data
##----------------------------------------------------------------
# Covariate Data
df_cov <- df_cov %>%
  filter(year_id %in% c(2010:2019))

# Merge
df_as <- left_join(
  x = df_as,
  y = df_cov,
  by = c("year_id", "location_id", "location_name")
)

##----------------------------------------------------------------
## 4. Create SUD "high_prev" variable to use as an interactive term in HIV models
##----------------------------------------------------------------
# Create sud_prevalence_counts column
df_sud_prev_count <- df_as %>% 
  filter(acause == "_subs") %>%
  select(c("cause_id", "year_id", "location_id", "location_name", "acause", 
           "cause_name", "prevalence_counts")) %>%
  setnames(old = "prevalence_counts", new = "sud_prevalence_counts")

df_sud_prev_count <- df_sud_prev_count %>%
  mutate(high_sud_prev = ifelse(sud_prevalence_counts >= median(sud_prevalence_counts), 1, 0))

# Join back to HIV data
df_as <- left_join(
  x = df_as,
  y = df_sud_prev_count %>% select(!c("acause", "cause_id", "cause_name")),
  by = c("year_id", "location_id", "location_name")
)

##----------------------------------------------------------------
## 5. Specify Models
##----------------------------------------------------------------
# Basic Model
model_basic_mort <- "as_mort_prev_ratio ~ as_spend_prev_ratio"
model_basic_mort_interactive <- "as_mort_prev_ratio ~ as_spend_prev_ratio * high_sud_prev"

# Covariate combinations
cov_h <- c('obesity', 'age65', 'cig_pc_10', 'phys_act_10', 'edu_yrs', 'as_spend_prev_ratio')

cov_sud <- c("prevalence_counts")

cov_risk <- c(
  "obesity",
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
f2_hiv <- as.formula(paste(model_basic_mort, "+", paste(c(cov_sud, cov_risk), collapse = " + ")))
f3_hiv <- as.formula(paste(model_basic_mort, "+", paste(c(cov_sud, cov_risk, cov_ses), collapse = " + ")))
f4_hiv <- as.formula(paste(model_basic_mort, "+", paste(c(cov_sud, cov_risk, cov_ses, cov_density), collapse = " + ")))
f5_hiv <- as.formula(paste(model_basic_mort, "+", paste(c(cov_h), collapse = " + ")))

# HIV - Mort / prev w/ interaction term - Formulas
f0_hiv_interact <- as.formula(model_basic_mort_interactive)
f1_hiv_interact <- as.formula(paste(model_basic_mort_interactive, "+", paste(cov_sud, collapse = " + ")))
f2_hiv_interact <- as.formula(paste(model_basic_mort_interactive, "+", paste(c(cov_sud, cov_risk), collapse = " + ")))
f3_hiv_interact <- as.formula(paste(model_basic_mort_interactive, "+", paste(c(cov_sud, cov_risk, cov_ses), collapse = " + ")))
f4_hiv_interact <- as.formula(paste(model_basic_mort_interactive, "+", paste(c(cov_sud, cov_risk, cov_ses, cov_density), collapse = " + ")))
f5_hiv_interact <- as.formula(paste(model_basic_mort_interactive, "+", paste(c(cov_h), collapse = " + ")))

# SUD - Mort / prev - Formulas
f0_sud <- as.formula(model_basic_mort)
f1_sud <- as.formula(paste(model_basic_mort, "+", paste(cov_risk, collapse = " + ")))
f2_sud <- as.formula(paste(model_basic_mort, "+", paste(c(cov_risk, cov_ses), collapse = " + ")))
f3_sud <- as.formula(paste(model_basic_mort, "+", paste(c(cov_risk, cov_ses, cov_density), collapse = " + ")))
f4_sud <- as.formula(paste(model_basic_mort, "+", paste(c(cov_h), collapse = " + ")))

# SUD - Mort / prev w/ interaction term - Formulas
f0_sud_interact <- as.formula(model_basic_mort_interactive)
f1_sud_interact <- as.formula(paste(model_basic_mort_interactive, "+", paste(cov_risk, collapse = " + ")))
f2_sud_interact <- as.formula(paste(model_basic_mort_interactive, "+", paste(c(cov_risk, cov_ses), collapse = " + ")))
f3_sud_interact <- as.formula(paste(model_basic_mort_interactive, "+", paste(c(cov_risk, cov_ses, cov_density), collapse = " + ")))
f4_sud_interact <- as.formula(paste(model_basic_mort_interactive, "+", paste(c(cov_h), collapse = " + ")))

# Formula List
list_formulas_hiv <- list(
  m0_mort_unadj                               = f0_hiv,
  m1_mort_plus_sud                            = f1_hiv,
  m2_mort_plus_obesity_cig_phys_ac            = f2_hiv,
  m3_mort_plus_edu_ldi                        = f3_hiv,
  m4_mort_plus_density                        = f4_hiv,
  m5_mort_haley                               = f5_hiv,
  m6_mort_int_unadj                           = f0_hiv_interact,
  m7_mort_int_plus_sud                        = f1_hiv_interact,
  m8_mort_int_plus_obesity_cig_phys_ac        = f2_hiv_interact,
  m9_mort_int_plus_edu_ldi                    = f3_hiv_interact,
  m10_mort_int_plus_density                   = f4_hiv_interact,
  m11_mort_int_haley                          = f5_hiv_interact
)

list_formulas_sud <- list(
  m0_mort_unadj                               = f0_sud,
  m1_mort_plus_obesity_cig_phys_ac            = f1_sud,
  m2_mort_plus_edu_ldi                        = f2_sud,
  m3_mort_plus_density                        = f3_sud,
  m4_mort_haley                               = f4_sud,
  m5_mort_int_unadj                           = f0_sud_interact,
  m6_mort_int_plus_obesity_cig_phys_ac        = f1_sud_interact,
  m7_mort_int_plus_edu_ldi                    = f2_sud_interact,
  m8_mort_int_plus_density                    = f3_sud_interact,
  m9_mort_int_haley                           = f4_sud_interact
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
## 6. Save Outputs
##----------------------------------------------------------------
write.csv(coef_tbl, file.path(dir_output, "model_coefficients.csv"))
write.csv(metrics_tbl, file.path(dir_output, "model_metrics.csv"))


