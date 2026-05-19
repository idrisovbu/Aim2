##================================================================
##  Title:     C_05_HIV_per_capita_cascade.R     (Lens 3 module)
##  Folder:    C_frontier_analysis/
##  Purpose:   POST-MAY-6-COMMITTEE Lens 3 — Cascade outcomes.
##
##             Ingests CDC HIV cascade data (Knowledge of Status,
##             Linkage to care 1 month, Receipt of care, Viral suppression),
##             merges into the existing per-capita state-year panel,
##             runs fractional-response + OLS regressions, and runs a
##             mediation analysis using the K x V composite as mediator
##             between spending and DALYs/capita.
##
##  RUN ORDER:
##    Phase 1 — Diagnostic block (always runs)
##    Phase 2 — Regressions + mediation (gated by RUN_REGRESSIONS flag)
##
##    On first run, leave RUN_REGRESSIONS = FALSE. Inspect diagnostic
##    output. Flip the flag to TRUE only after the diagnostic looks clean.
##
##  WHY 13+ AGGREGATE (no age standardization):
##    Knowledge of Status is only published at 13+. For consistency across
##    cascade outcomes, all four use the 13+ aggregate. Cascade indicators
##    are conditional probabilities within the HIV+ subpopulation; age-
##    standardizing to GBD weights is conceptually different from age-std
##    of incidence/mortality rates and is not standard in cascade lit.
##
##  WHY ONLY K x V FOR THE COMPOSITE:
##    K conditions on all PLWH; V conditions on diagnosed PLWH. K x V is
##    a clean conditional-probability product approximating
##    "fraction of all PLWH who are virally suppressed."
##    Linkage (denominator: newly diagnosed in year) and Receipt
##    (denominator: diagnosed PLWH at end of year) do not chain cleanly.
##    For descriptive purposes only, a geometric mean of all four
##    indicators is computed and clearly labeled as a system-performance
##    score, NOT a probability.
##
##  Inputs (must already exist):
##    - df_hiv_per_capita_panel.csv  (from C_04_HIV_per_capita_models.R)
##    - CDC_Cascade_Data.csv         (CDC Atlas export, 13+ aggregate)
##
##  Outputs (in dir_output):
##    df_hiv_cascade_panel.csv
##    cascade_diagnostic_report.txt          (Phase 1 only)
##    [Phase 2 only:]
##    regression_results_cascade.csv
##    cascade_marginal_effects.csv
##    mediation_results.csv
##    spending_coefficients_summary.csv      (APPENDED with cascade rows)
##    model_registry.csv                     (APPENDED with cascade rows)
##================================================================


##================================================================
## 0.  SETUP
##================================================================
rm(list = ls())

# >>>> GATE: flip to TRUE only after diagnostic block is reviewed <<<<
RUN_REGRESSIONS <- T

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

user_lib <- file.path(h, "R_packages")
if (!dir.exists(user_lib)) dir.create(user_lib, recursive = TRUE)
.libPaths(c(user_lib, .libPaths()))

pacman::p_load(
  data.table, tidyverse, glue, broom,
  lmtest, sandwich, clubSandwich,
  marginaleffects, boot
)

if ("plotly" %in% loadedNamespaces()) filter <- dplyr::filter
tryCatch(conflicted::conflicts_prefer(dplyr::filter, .quiet = TRUE),
         error = function(e) invisible(NULL))
tryCatch(conflicted::conflicts_prefer(purrr::discard, .quiet = TRUE),
         error = function(e) invisible(NULL))

# ---------- IO ----------
panel_date  <- "20260517"     # <-- EDIT: date of Lens 1/2 panel output
dir_panel   <- file.path(h, "aim_outputs/Aim2/C_frontier_analysis",
                         panel_date, "analysis_per_capita")
fp_panel    <- file.path(dir_panel, "df_hiv_per_capita_panel.csv")

fp_cdc_cascade <- file.path(h, "aim_outputs/Aim2/R_resources/CDC_data",
                            "CDC_Cascade_Data.csv")

output_date <- format(Sys.time(), "%Y%m%d")
dir_output  <- file.path(h, "aim_outputs/Aim2/C_frontier_analysis",
                         output_date, "analysis_per_capita")
if (!dir.exists(dir_output)) dir.create(dir_output, recursive = TRUE)


##================================================================
## 1.  LOAD EXISTING PANEL
##================================================================
if (!file.exists(fp_panel)) {
  stop("Existing panel not found at: ", fp_panel,
       "\nRun C_04_HIV_per_capita_models.R first to produce df_hiv_per_capita_panel.csv,",
       "\nor update panel_date at top of this script.")
}
df_panel <- read.csv(fp_panel, stringsAsFactors = FALSE)

cat("Loaded existing panel:", nrow(df_panel), "rows,",
    ncol(df_panel), "cols\n")


##================================================================
## 2.  LOAD AND PARSE CDC CASCADE DATA
##================================================================
##  CDC_Cascade_Data.csv format:
##    Rows 1-8: metadata + footnotes
##    Row 9   : header "Indicator,Year,Geography,FIPS,Cases,Percent (95% CI RSE)"
##    Rows 10+: data
##  Percent column has formats like:
##    "76.8 (73.1 - 81.0, 2.6)"   (point + CI + RSE)
##    "55.5"                       (point only — no CI/RSE supplied)
##    "Data not available"         (suppressed)
##================================================================

# Find the header row dynamically (more robust than hard-coded skip)
raw_lines <- readLines(fp_cdc_cascade, n = 50)
header_row <- which(grepl("^Indicator,Year,Geography", raw_lines))
if (length(header_row) == 0) stop("Could not locate header row in CDC cascade file.")

df_cdc_raw <- read.csv(fp_cdc_cascade, skip = header_row - 1,
                       stringsAsFactors = FALSE, na.strings = c("", "NA"))

# Clean column names (read.csv may mangle the parenthesized one)
names(df_cdc_raw) <- c("Indicator", "Year", "Geography", "FIPS",
                       "Cases", "Percent_raw")

# Parse the Percent column ----
# Regex: leading number, optional " (lower - upper, rse)"
parse_pct <- function(x) {
  # Suppression / missingness
  x <- as.character(x)
  x[is.na(x) | tolower(x) %in% c("data not available", "na", "n/a", "", "*")] <- NA_character_
  
  # Try "PE (LB - UB, RSE)"
  m_full  <- str_match(x, "^\\s*([0-9.]+)\\s*\\(\\s*([0-9.]+)\\s*-\\s*([0-9.]+)\\s*,\\s*([0-9.]+)\\s*\\)")
  # Fallback: just a number
  m_point <- str_match(x, "^\\s*([0-9.]+)\\s*$")
  
  pe  <- ifelse(!is.na(m_full[, 2]), as.numeric(m_full[, 2]), as.numeric(m_point[, 2]))
  lb  <- as.numeric(m_full[, 3])
  ub  <- as.numeric(m_full[, 4])
  rse <- as.numeric(m_full[, 5])
  
  tibble(point = pe, lower = lb, upper = ub, rse = rse)
}

parsed <- parse_pct(df_cdc_raw$Percent_raw)

df_cdc_long <- df_cdc_raw %>%
  select(Indicator, Year, Geography, FIPS, Cases) %>%
  bind_cols(parsed) %>%
  mutate(
    Year = as.integer(Year),
    Cases = suppressWarnings(as.numeric(gsub(",", "", Cases)))
  )

# Map CDC indicator names -> short snake_case names
indicator_map <- c(
  "Knowledge of Status"            = "cdc_knowledge_status",
  "Linkage to HIV medical care"    = "cdc_linkage_1mo",
  "Receipt of HIV medical care"    = "cdc_receipt_care",
  "HIV viral suppression"          = "cdc_viral_suppress"
)
df_cdc_long <- df_cdc_long %>%
  mutate(indicator_short = unname(indicator_map[Indicator]))

unmapped <- df_cdc_long %>%
  filter(is.na(indicator_short)) %>%
  distinct(Indicator) %>%
  pull(Indicator)
if (length(unmapped) > 0) {
  warning("Unmapped CDC indicators (will be dropped): ",
          paste(unmapped, collapse = "; "))
  df_cdc_long <- df_cdc_long %>% filter(!is.na(indicator_short))
}

# Convert percent (0-100) -> fraction (0-1).  Fractional response models
# REQUIRE [0, 1]. CDC publishes as percent so this is necessary.
df_cdc_long <- df_cdc_long %>%
  mutate(
    point_frac = point / 100,
    lower_frac = lower / 100,
    upper_frac = upper / 100
  )

# Pivot wide: one row per (Geography, Year), one column per indicator
df_cdc_wide <- df_cdc_long %>%
  select(Geography, Year, FIPS, indicator_short, point_frac) %>%
  pivot_wider(names_from = indicator_short, values_from = point_frac)

# Also keep RSE columns wide for the suppression diagnostic
df_cdc_rse <- df_cdc_long %>%
  select(Geography, Year, indicator_short, rse) %>%
  mutate(indicator_short = paste0(indicator_short, "__rse")) %>%
  pivot_wider(names_from = indicator_short, values_from = rse)

df_cdc_wide <- df_cdc_wide %>%
  left_join(df_cdc_rse, by = c("Geography", "Year"))

# Fix state-name quirks consistent with existing prep_A pattern
df_cdc_wide <- df_cdc_wide %>%
  mutate(
    Geography = case_when(
      Geography == "Mississippi^"   ~ "Mississippi",
      Geography == "West Virginia^" ~ "West Virginia",
      TRUE                          ~ Geography
    )
  )


##================================================================
## 3.  MERGE INTO EXISTING PANEL
##================================================================
##  Match on (location_name, year_id). 50 states + DC expected.
##  Territories (PR, USVI, GU, etc.) in CDC export will fail the join
##  and be dropped — that's intentional.
##================================================================

panel_locs_before <- nrow(df_panel)

df_cascade <- df_panel %>%
  left_join(
    df_cdc_wide %>% rename(location_name = Geography, year_id = Year),
    by = c("location_name", "year_id")
  )

panel_locs_after  <- nrow(df_cascade)

# CDC geographies that did NOT match a panel state (territories etc.)
unmatched_cdc <- df_cdc_wide %>%
  anti_join(df_panel %>% distinct(location_name),
            by = c("Geography" = "location_name")) %>%
  distinct(Geography) %>%
  pull(Geography)


##================================================================
## 4.  ============ DIAGNOSTIC BLOCK ============
##================================================================
##  Prints 7 checks to console (and to cascade_diagnostic_report.txt).
##  Hard-stops via the RUN_REGRESSIONS flag at the bottom.
##================================================================

# Redirect to both console AND a report file
report_path <- file.path(dir_output, "cascade_diagnostic_report.txt")
sink_con <- file(report_path, open = "wt")
tee <- function(...) {
  msg <- paste0(..., collapse = "")
  cat(msg, "\n")
  cat(msg, "\n", file = sink_con)
}

tee("==============================================================")
tee("  LENS 3 CASCADE — DIAGNOSTIC REPORT")
tee("  Run date: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
tee("  Input file: ", fp_cdc_cascade)
tee("  Panel file: ", fp_panel)
tee("==============================================================")

# ---- 4.1 Raw ingest summary ----
tee("\n----- 4.1 RAW INGEST SUMMARY -----")
tee("CDC long-format rows after parse: ", nrow(df_cdc_long))
tee("Unique (Geography, Year, indicator) combos: ",
    df_cdc_long %>% distinct(Geography, Year, indicator_short) %>% nrow())
tee("Indicators found in file (mapped):")
print(table(df_cdc_long$indicator_short)); cat("\n", file = sink_con)
capture.output(print(table(df_cdc_long$indicator_short)), file = sink_con)

expected_inds <- c("cdc_knowledge_status", "cdc_linkage_1mo",
                   "cdc_receipt_care", "cdc_viral_suppress")
missing_inds  <- setdiff(expected_inds, unique(df_cdc_long$indicator_short))
if (length(missing_inds) > 0) {
  tee("!! MISSING expected indicators: ", paste(missing_inds, collapse = ", "))
} else {
  tee("All 4 expected indicators present.")
}

geos_in_cdc <- sort(unique(df_cdc_long$Geography))
tee("\nCDC geographies (", length(geos_in_cdc), " total): ",
    paste(head(geos_in_cdc, 5), collapse = ", "), " ...")
tee("Non-state geographies (territories etc., flagged): ",
    if (length(unmatched_cdc) == 0) "none" else paste(unmatched_cdc, collapse = ", "))

# ---- 4.2 Range and unit check ----
tee("\n----- 4.2 RANGE / UNIT CHECK -----")
tee("Reported in CDC export as PERCENT (0-100). Pipeline converts to fraction (0-1).")
range_chk <- df_cdc_long %>%
  group_by(indicator_short) %>%
  summarise(
    n_nonNA   = sum(!is.na(point)),
    min_pct   = round(min(point,   na.rm = TRUE), 1),
    mean_pct  = round(mean(point,  na.rm = TRUE), 1),
    max_pct   = round(max(point,   na.rm = TRUE), 1),
    examples  = paste(round(head(point[!is.na(point)], 3), 1), collapse = ", "),
    .groups   = "drop"
  )
capture.output(print(as.data.frame(range_chk)), file = sink_con)
print(as.data.frame(range_chk))

if (any(range_chk$max_pct > 100, na.rm = TRUE)) tee("!! WARNING: max > 100 — unit issue?")
if (any(range_chk$max_pct < 1,   na.rm = TRUE)) tee("!! WARNING: max < 1 — already fractions?")

# ---- 4.3 State-year coverage matrix ----
tee("\n----- 4.3 STATE-YEAR COVERAGE (count of non-missing) -----")
coverage <- df_cdc_long %>%
  filter(!is.na(point), Year %in% 2010:2019) %>%
  count(indicator_short, Year) %>%
  pivot_wider(names_from = Year, values_from = n, values_fill = 0L) %>%
  arrange(indicator_short)
capture.output(print(as.data.frame(coverage)), file = sink_con)
print(as.data.frame(coverage))

# ---- 4.4 Suppression and reliability flags ----
tee("\n----- 4.4 SUPPRESSION / RELIABILITY -----")
rel_summary <- df_cdc_long %>%
  group_by(indicator_short) %>%
  summarise(
    n_total        = n(),
    n_data_avail   = sum(!is.na(point)),
    n_suppressed   = sum(is.na(point)),
    n_rse_high     = sum(!is.na(rse) & rse > 50, na.rm = TRUE),     # unreliable
    n_rse_moderate = sum(!is.na(rse) & rse >= 30 & rse <= 50, na.rm = TRUE),
    .groups = "drop"
  )
capture.output(print(as.data.frame(rel_summary)), file = sink_con)
print(as.data.frame(rel_summary))
tee("Note: CDC reports estimates with RSE > 50% as 'Data not available' upstream,")
tee("      so n_rse_high in available rows should typically be 0.")

# ---- 4.5 Composite feasibility check ----
tee("\n----- 4.5 COMPOSITE FEASIBILITY (K x V) -----")
kv_feasible <- df_cdc_wide %>%
  filter(!is.na(cdc_knowledge_status), !is.na(cdc_viral_suppress)) %>%
  nrow()
kv_total <- df_cdc_wide %>% nrow()
tee("State-years with BOTH K and V non-missing: ", kv_feasible, " / ", kv_total)
tee("  -> This is the panel size for K x V composite and mediation analysis.")
tee("  -> If < 200, mediation is underpowered; consider restricting years.")

# ---- 4.6 National-mean sanity check ----
tee("\n----- 4.6 NATIONAL-MEAN SANITY CHECK -----")
tee("Simple unweighted state means (population-weighted would need pop merge).")
for (yr in c(2014, 2019)) {
  yr_means <- df_cdc_wide %>%
    filter(Year == yr) %>%
    summarise(
      year = yr,
      K_mean = mean(cdc_knowledge_status, na.rm = TRUE),
      L_mean = mean(cdc_linkage_1mo,      na.rm = TRUE),
      R_mean = mean(cdc_receipt_care,     na.rm = TRUE),
      V_mean = mean(cdc_viral_suppress,   na.rm = TRUE)
    ) %>%
    mutate(across(ends_with("_mean"), \(x) round(x, 3)))
  capture.output(print(as.data.frame(yr_means)), file = sink_con)
  print(as.data.frame(yr_means))
}
tee("Published CDC national values (rough): K ~0.86 (2019), V ~0.66 (2019).")
tee("If our means are wildly different, units or scope are off.")

# ---- 4.7 Existing-pattern compatibility check ----
tee("\n----- 4.7 EXISTING-PATTERN COMPATIBILITY -----")
tee("Panel rows before cascade merge: ", panel_locs_before)
tee("Panel rows after  cascade merge: ", panel_locs_after,
    "  (should be unchanged — left join)")
tee("Unmatched CDC geographies (dropped): ",
    if (length(unmatched_cdc) == 0) "none" else paste(unmatched_cdc, collapse = ", "))

# How many panel state-years got CDC data for at least one indicator?
df_cascade_chk <- df_cascade %>%
  mutate(any_cdc = !is.na(cdc_knowledge_status) | !is.na(cdc_linkage_1mo) |
           !is.na(cdc_receipt_care)    | !is.na(cdc_viral_suppress))
tee("Panel state-years matched to at least one CDC indicator: ",
    sum(df_cascade_chk$any_cdc), " / ", nrow(df_cascade_chk))

# Year range comparison
panel_yrs <- sort(unique(df_cascade$year_id))
cdc_yrs   <- sort(unique(df_cdc_wide$Year))
tee("Panel year range: ", min(panel_yrs), "-", max(panel_yrs))
tee("CDC   year range: ", min(cdc_yrs),   "-", max(cdc_yrs))

tee("\n==============================================================")
tee("  DIAGNOSTIC BLOCK COMPLETE — review output before continuing.")
tee("  To proceed: set RUN_REGRESSIONS <- TRUE at top, re-source.")
tee("==============================================================\n")

close(sink_con)

# Save the merged cascade panel even if we don't proceed — useful for review
write.csv(df_cascade,
          file.path(dir_output, "df_hiv_cascade_panel.csv"),
          row.names = FALSE)

if (!isTRUE(RUN_REGRESSIONS)) {
  message("\n>>> RUN_REGRESSIONS is FALSE. Stopping after diagnostic block.")
  message(">>> Diagnostic report saved to: ", report_path)
  message(">>> Merged panel saved to:      ",
          file.path(dir_output, "df_hiv_cascade_panel.csv"))
  stop("DIAGNOSTIC BLOCK COMPLETE — review output before continuing to regressions.",
       call. = FALSE)
}


##================================================================
## 5.  HARD SANITY CHECKS (after diagnostic gate)
##================================================================

# 5.1 All cascade outcomes must be in [0, 1]
cascade_outcome_cols <- c("cdc_knowledge_status", "cdc_linkage_1mo",
                          "cdc_receipt_care", "cdc_viral_suppress")
for (cc in cascade_outcome_cols) {
  vals <- df_cascade[[cc]]
  vals <- vals[!is.na(vals)]
  if (length(vals) == 0) next
  if (any(vals < 0) || any(vals > 1)) {
    stop("Cascade outcome ", cc, " has values outside [0, 1] — abort.")
  }
}

# 5.2 Build composite + descriptive geometric-mean score
df_cascade <- df_cascade %>%
  mutate(
    cdc_kv_composite = cdc_knowledge_status * cdc_viral_suppress,
    # Descriptive only — labeled as system-performance score, not probability
    cdc_klrv_geo_mean = (cdc_knowledge_status * cdc_linkage_1mo *
                           cdc_receipt_care    * cdc_viral_suppress) ^ (1/4)
  )

# K x V quartile cross-tab — confirm composite is not degenerate
kv_qtab <- df_cascade %>%
  filter(!is.na(cdc_knowledge_status), !is.na(cdc_viral_suppress)) %>%
  mutate(K_q = ntile(cdc_knowledge_status, 4),
         V_q = ntile(cdc_viral_suppress,   4)) %>%
  count(K_q, V_q) %>%
  pivot_wider(names_from = V_q, values_from = n, values_fill = 0L,
              names_prefix = "V_q")
cat("\n--- K x V quartile cross-tab ---\n")
print(as.data.frame(kv_qtab))


##================================================================
## 6.  LENS 3 REGRESSIONS — 5 outcomes x 4 specs x 2 model classes
##================================================================

list_models    <- list()
model_data     <- list()
model_registry <- tibble(model_id = character(), lens = character(),
                         spec = character(), model_class = character(),
                         outcome = character(), is_final = logical())

register_cascade_model <- function(outcome_var, spec, model_class,
                                   formula_str, data, is_final = FALSE) {
  model_id <- paste0("hiv__cascade__", outcome_var, "__", spec, "__", model_class)
  fit <- if (model_class == "frac") {
    glm(as.formula(formula_str), data = data,
        family = quasibinomial(link = "logit"))
  } else {
    lm(as.formula(formula_str), data = data)
  }
  list_models[[model_id]]   <<- fit
  model_data[[model_id]]    <<- data
  model_registry <<- bind_rows(
    model_registry,
    tibble(model_id = model_id, lens = "cascade", spec = spec,
           model_class = model_class, outcome = outcome_var,
           is_final = is_final)
  )
  invisible(fit)
}

# Build formula strings programmatically
build_formula <- function(outcome, spec) {
  rhs <- switch(spec,
                bivariate = "log_spending_per_capita + year_factor",
                primary   = paste("log_spending_per_capita + year_factor +",
                                  "race_prop_BLCK + race_prop_HISP +",
                                  "log_prop_homeless + log_prevalence_per_100k"),
                no_prev   = paste("log_spending_per_capita + year_factor +",
                                  "race_prop_BLCK + race_prop_HISP +",
                                  "log_prop_homeless"),
                lag_spending_l1 = paste("log_spending_per_capita_l1 + year_factor +",
                                        "race_prop_BLCK + race_prop_HISP +",
                                        "log_prop_homeless + log_prevalence_per_100k")
  )
  paste0(outcome, " ~ ", rhs)
}

cascade_outcomes_for_models <- c(cascade_outcome_cols, "cdc_kv_composite")
specs <- c("bivariate", "primary", "no_prev", "lag_spending_l1")
classes <- c("frac", "ols")

for (oc in cascade_outcomes_for_models) {
  for (sp in specs) {
    for (cl in classes) {
      register_cascade_model(
        outcome_var = oc, spec = sp, model_class = cl,
        formula_str = build_formula(oc, sp),
        data        = df_cascade,
        is_final    = (sp == "primary" && cl == "frac")
      )
    }
  }
}

cat("\nFit ", length(list_models), " cascade models.\n", sep = "")


##================================================================
## 7.  EXTRACT COEFFICIENTS + MARGINAL EFFECTS
##================================================================

extract_clustered_cascade <- function(model, model_id, model_data_df) {
  is_glm <- inherits(model, "glm")
  tryCatch({
    if (is_glm) {
      vc <- vcovCL(model, cluster = ~ location_id, type = "HC0")
      ct <- coeftest(model, vcov. = vc)
    } else {
      vc <- vcovCR(model, cluster = model_data_df$location_id, type = "CR2")
      ct <- coef_test(model, vcov = vc, test = "Satterthwaite")
      return(tibble(
        term      = rownames(ct),
        estimate  = ct$beta,
        std.error = ct$SE,
        statistic = ct$tstat,
        p.value   = ct$p_Satt,
        model_id  = model_id
      ))
    }
    tibble(
      term      = rownames(ct),
      estimate  = ct[, "Estimate"],
      std.error = ct[, "Std. Error"],
      statistic = ct[, "z value"],
      p.value   = ct[, "Pr(>|z|)"],
      model_id  = model_id
    )
  }, error = function(e) broom::tidy(model) %>% mutate(model_id = model_id))
}

coef_tbl <- map_dfr(names(list_models), function(nm) {
  extract_clustered_cascade(list_models[[nm]], nm, model_data[[nm]])
})

coef_tbl <- coef_tbl %>%
  left_join(model_registry %>%
              select(model_id, lens, spec, model_class, outcome),
            by = "model_id") %>%
  mutate(
    acause = "hiv",
    signif_stars = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE            ~ ""
    ),
    signif_label = case_when(
      p.value < 0.001 ~ "p < 0.001",
      p.value < 0.01  ~ "p < 0.01",
      p.value < 0.05  ~ "p < 0.05",
      TRUE            ~ "not significant"
    )
  )

# Convergence check on every GLM
nonconv_glm <- map_chr(names(list_models), function(nm) {
  m <- list_models[[nm]]
  if (inherits(m, "glm") && isFALSE(m$converged)) nm else NA_character_
}) %>% discard(is.na)
if (length(nonconv_glm) > 0) {
  warning("Non-converged GLMs: ", paste(nonconv_glm, collapse = ", "))
}

# Average marginal effects for fractional GLMs (interpretable as
# fraction-point change in outcome per unit change in predictor)
ame_tbl <- map_dfr(names(list_models), function(nm) {
  m <- list_models[[nm]]
  if (!inherits(m, "glm")) return(NULL)
  tryCatch({
    s <- avg_slopes(m, vcov = vcovCL(m, cluster = ~ location_id, type = "HC0"))
    tibble(
      model_id  = nm,
      term      = s$term,
      ame       = s$estimate,
      ame_se    = s$std.error,
      ame_pval  = s$p.value,
      ame_low   = s$conf.low,
      ame_high  = s$conf.high
    )
  }, error = function(e) tibble(model_id = nm, term = NA, ame = NA))
})

# Predicted-value sanity check for fractional GLMs
for (nm in names(list_models)) {
  m <- list_models[[nm]]
  if (!inherits(m, "glm")) next
  pr <- predict(m, type = "response")
  if (any(pr < 0 | pr > 1, na.rm = TRUE)) {
    warning("Predictions out of [0,1] for ", nm)
  }
}

metrics_tbl <- imap_dfr(list_models, function(model, model_id) {
  if (inherits(model, "glm")) {
    tibble(model_id = model_id,
           n        = length(model$fitted.values),
           r2       = NA_real_, adj_r2 = NA_real_,
           aic      = AIC(model), bic = BIC(model),
           sigma    = NA_real_)
  } else {
    g <- broom::glance(model)
    tibble(model_id = model_id,
           n = g$nobs, r2 = g$r.squared, adj_r2 = g$adj.r.squared,
           aic = AIC(model), bic = BIC(model), sigma = g$sigma)
  }
})

regression_results <- coef_tbl %>%
  left_join(metrics_tbl, by = "model_id") %>%
  select(model_id, acause, lens, outcome, spec, model_class, term,
         estimate, std.error, statistic, p.value,
         signif_stars, signif_label,
         n, r2, adj_r2, aic, bic, sigma)

write.csv(regression_results,
          file.path(dir_output, "regression_results_cascade.csv"),
          row.names = FALSE)
write.csv(ame_tbl,
          file.path(dir_output, "cascade_marginal_effects.csv"),
          row.names = FALSE)


##================================================================
## 8.  MEDIATION ANALYSIS
##================================================================
##  X = log_spending_per_capita
##  M = cdc_kv_composite     (or its lag)
##  Y = log_daly_per_capita
##  Covariates: year_factor + race_prop_BLCK + race_prop_HISP +
##              log_prop_homeless + log_prevalence_per_100k
##
##  Contemporaneous: framed as decomposition of association.
##  Lagged: spending(t-1) -> mediator(t) -> outcome(t+1).
##  Bootstrap: state-block (cluster = location_id), 500 reps.
##================================================================

med_covars <- "year_factor + race_prop_BLCK + race_prop_HISP +
               log_prop_homeless + log_prevalence_per_100k"

run_one_mediation <- function(df, X, M, Y, covars, label) {
  fa <- as.formula(paste(M, "~", X, "+", covars))   # a
  fb <- as.formula(paste(Y, "~", M, "+", X, "+", covars))  # b (and c')
  fc <- as.formula(paste(Y, "~", X, "+", covars))   # c (total)
  
  fit_a <- lm(fa, data = df)
  fit_b <- lm(fb, data = df)
  fit_c <- lm(fc, data = df)
  
  a   <- coef(fit_a)[X]
  b   <- coef(fit_b)[M]
  cpr <- coef(fit_b)[X]
  ctot <- coef(fit_c)[X]
  
  indirect <- as.numeric(a) * as.numeric(b)
  prop_med <- indirect / as.numeric(ctot)
  
  tibble(label = label, a = a, b = b, c_prime = cpr, c_total = ctot,
         indirect = indirect, prop_mediated = prop_med,
         n_obs = nrow(df %>% drop_na(all_of(c(X, M, Y)))))
}

# State-block bootstrap (cluster on location_id)
boot_mediation <- function(df, X, M, Y, covars, label, reps = 500) {
  state_ids <- unique(df$location_id)
  est_orig  <- run_one_mediation(df, X, M, Y, covars, label)
  
  boot_stats <- map_dfr(seq_len(reps), function(i) {
    set.seed(20260516 + i)
    samp_ids   <- sample(state_ids, size = length(state_ids), replace = TRUE)
    df_boot    <- map_dfr(samp_ids, ~ df %>% filter(location_id == .x))
    tryCatch(run_one_mediation(df_boot, X, M, Y, covars, label),
             error = function(e) tibble())
  })
  
  if (nrow(boot_stats) < 50) {
    warning("Bootstrap for ", label, " produced too few successful reps (", nrow(boot_stats), ")")
    return(est_orig %>% mutate(indirect_lower = NA_real_, indirect_upper = NA_real_,
                               prop_med_lower = NA_real_, prop_med_upper = NA_real_,
                               n_boot_reps = nrow(boot_stats)))
  }
  
  est_orig %>%
    mutate(
      indirect_lower = quantile(boot_stats$indirect,       0.025, na.rm = TRUE),
      indirect_upper = quantile(boot_stats$indirect,       0.975, na.rm = TRUE),
      prop_med_lower = quantile(boot_stats$prop_mediated,  0.025, na.rm = TRUE),
      prop_med_upper = quantile(boot_stats$prop_mediated,  0.975, na.rm = TRUE),
      n_boot_reps    = nrow(boot_stats)
    )
}

# Contemporaneous mediation
med_contemp <- boot_mediation(
  df = df_cascade,
  X  = "log_spending_per_capita",
  M  = "cdc_kv_composite",
  Y  = "log_daly_per_capita",
  covars = med_covars,
  label  = "contemporaneous"
)

# Lagged mediation: spending(t-1) -> mediator(t) -> outcome(t+1)
df_cascade_lag <- df_cascade %>%
  arrange(location_id, year_id) %>%
  group_by(location_id) %>%
  mutate(
    log_daly_per_capita_f1 = dplyr::lead(log_daly_per_capita, 1)
  ) %>%
  ungroup()

med_lagged <- boot_mediation(
  df = df_cascade_lag,
  X  = "log_spending_per_capita_l1",
  M  = "cdc_kv_composite",
  Y  = "log_daly_per_capita_f1",
  covars = med_covars,
  label  = "lagged_spending_l1_outcome_f1"
)

mediation_results <- bind_rows(med_contemp, med_lagged)

write.csv(mediation_results,
          file.path(dir_output, "mediation_results.csv"),
          row.names = FALSE)


##================================================================
## 9.  APPEND TO EXISTING OUTPUTS
##================================================================
##  Append, don't overwrite. The Lens 1/2 outputs from
##  C_04_HIV_per_capita_models.R already exist; we add cascade rows.
##================================================================

cascade_spending_summary <- regression_results %>%
  filter(grepl("log_spending_per_capita", term)) %>%
  select(model_id, lens, outcome, spec, model_class, term,
         estimate, std.error, p.value, signif_label, n) %>%
  mutate(estimate = round(estimate, 5),
         std.error = round(std.error, 5),
         p.value   = round(p.value, 4))

fp_spend_summary <- file.path(dir_output, "spending_coefficients_summary.csv")
if (file.exists(fp_spend_summary)) {
  existing <- read.csv(fp_spend_summary, stringsAsFactors = FALSE)
  bind_rows(existing, cascade_spending_summary) %>%
    write.csv(fp_spend_summary, row.names = FALSE)
} else {
  write.csv(cascade_spending_summary, fp_spend_summary, row.names = FALSE)
}

fp_registry <- file.path(dir_output, "model_registry.csv")
if (file.exists(fp_registry)) {
  existing <- read.csv(fp_registry, stringsAsFactors = FALSE)
  bind_rows(existing, model_registry) %>%
    write.csv(fp_registry, row.names = FALSE)
} else {
  write.csv(model_registry, fp_registry, row.names = FALSE)
}


##================================================================
## 10. END-OF-RUN SUMMARY
##================================================================

cat("\n================ LENS 3 DONE ================\n")
cat("Cascade models fit:    ", nrow(model_registry), "\n")
cat("Outputs written to:    ", dir_output, "\n\n")

cat("---- Spending coefficient in primary fractional models ----\n")
primary_frac <- regression_results %>%
  filter(spec == "primary", model_class == "frac",
         term == "log_spending_per_capita") %>%
  select(outcome, estimate, std.error, p.value, signif_label, n)
print(as.data.frame(primary_frac))

cat("\n---- Mediation results (K x V composite) ----\n")
print(as.data.frame(mediation_results %>%
                      select(label, a, b, c_prime, c_total,
                             indirect, indirect_lower, indirect_upper,
                             prop_mediated, prop_med_lower, prop_med_upper,
                             n_obs, n_boot_reps)))

##================================================================
## END OF PIPELINE
##================================================================