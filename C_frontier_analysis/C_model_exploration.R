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
## 4. Specify Models
##----------------------------------------------------------------
# Basic Model
model_basic_mort <- "as_mort_prev_ratio ~ as_spend_prev_ratio"
model_basic_daly <- "as_daly_prev_ratio ~ as_spend_prev_ratio"

# Covariate combinations
cov_h <- c('obesity', 'age65', 'cig_pc_10', 'phys_act_10', 'edu_yrs', 'as_spend_prev_ratio')

cov_incidence <- c("incidence_counts")

cov_age  <- c("age65")

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

# Mort / prev - Formulas
f0 <- as.formula(model_basic_mort)
f1 <- as.formula(paste(model_basic_mort, "+", paste(cov_incidence, collapse = " + ")))
f2 <- as.formula(paste(model_basic_mort, "+", paste(c(cov_incidence, cov_age), collapse = " + ")))
f3 <- as.formula(paste(model_basic_mort, "+", paste(c(cov_incidence, cov_age, cov_risk), collapse = " + ")))
f4 <- as.formula(paste(model_basic_mort, "+", paste(c(cov_incidence, cov_age, cov_risk, cov_ses), collapse = " + ")))
f5 <- as.formula(paste(model_basic_mort, "+", paste(c(cov_incidence, cov_age, cov_risk, cov_ses, cov_density), collapse = " + ")))
f6 <- as.formula(paste(model_basic_mort, "+", paste(c(cov_h), collapse = " + ")))

# DALY / prev - Formulas
f7 <- as.formula(model_basic_mort)
f8 <- as.formula(paste(model_basic_daly, "+", paste(cov_incidence, collapse = " + ")))
f9 <- as.formula(paste(model_basic_daly, "+", paste(c(cov_incidence, cov_age), collapse = " + ")))
f10 <- as.formula(paste(model_basic_daly, "+", paste(c(cov_incidence, cov_age, cov_risk), collapse = " + ")))
f11 <- as.formula(paste(model_basic_daly, "+", paste(c(cov_incidence, cov_age, cov_risk, cov_ses), collapse = " + ")))
f12 <- as.formula(paste(model_basic_daly, "+", paste(c(cov_incidence, cov_age, cov_risk, cov_ses, cov_density), collapse = " + ")))
f13 <- as.formula(paste(model_basic_daly, "+", paste(c(cov_h), collapse = " + ")))

# Formula List
list_formulas <- list(
  m0_mort_unadj                     = f0,
  m1_mort_inc                       = f1,
  m2_mort_age65                     = f2,
  m3_mort_plus_obesity_cig_phys_ac  = f3,
  m4_mort_plus_edu_ldi              = f4,
  m5_mort_plus_density              = f5,
  m6_mort_haley                     = f6,
  m7_daly_unadj                     = f7,
  m8_daly_inc                       = f8,
  m9_daly_age65                     = f9,
  m10_daly_plus_obesity_cig_phys_ac  = f10,
  m11_daly_plus_edu_ldi              = f11,
  m12_daly_plus_density              = f12,
  m13_daly_haley                     = f13
)

##----------------------------------------------------------------
## 5. Run Models
##----------------------------------------------------------------
acauses <- c("hiv", "_subs")

list_models <- list()

for (a in acauses) {
  df_a <- df_as %>% filter(acause == a)
  
  for (nm in names(list_formulas)) {
    list_models[[paste(a, nm, sep = "__")]] <- lm(list_formulas[[nm]], data = df_a)
  }
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


