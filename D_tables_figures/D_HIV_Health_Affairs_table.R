##----------------------------------------------------------------
##' Title: D_HIV_Health_Affairs_table.R
##'
##' Purpose: Build manuscript-ready HIV supplemental table by combining
##'          Table 3 spending effectiveness output with frontier summary.
##----------------------------------------------------------------

##----------------------------------------------------------------
## 0. Clear environment and load packages
##----------------------------------------------------------------
rm(list = ls())

pacman::p_load(
  dplyr, openxlsx, RMySQL, data.table, ini, DBI, tidyr,
  reticulate, ggpubr, arrow, scales, readr, stringr
)
library(lbd.loader, lib.loc = sprintf("/share/geospatial/code/geospatial-libraries/lbd.loader-%s", R.version$major))
if("dex.dbr"%in% (.packages())) detach("package:dex.dbr", unload=TRUE)
library(dex.dbr, lib.loc = lbd.loader::pkg_loc("dex.dbr"))
suppressMessages(devtools::load_all(path = "/ihme/homes/idrisov/repo/dex_us_county/"))

library(conflicted)
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::lag)

# Set drive paths
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

##----------------------------------------------------------------
## 1. Functions
##----------------------------------------------------------------
ensure_dir_exists <- function(dir_path) {
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
}

first_existing_col <- function(df, candidates, required = TRUE) {
  hit <- candidates[candidates %in% names(df)][1]
  if (is.na(hit) && required) {
    stop(sprintf("None of these columns were found: %s", paste(candidates, collapse = ", ")))
  }
  hit
}

##----------------------------------------------------------------
## 2. Paths
##----------------------------------------------------------------
date_t1 <- "20260411"
date_t3 <- "20260410"
date_frontier <- "20260411"

fp_t1 <- file.path(h, "/aim_outputs/Aim2/D_tables_figures/", date_t1, "/T1_HIV_2019_UI.csv")
fp_t3 <- file.path(h, "/aim_outputs/Aim2/D_tables_figures/", date_t3, "/T3_HIV_spending_effectiveness.csv")
fp_frontier <- file.path(h, "/aim_outputs/Aim2/C_frontier_analysis/", date_frontier, "/hiv_yfe_daly_state_summary.csv")

date_today <- format(Sys.time(), "%Y%m%d")
dir_output <- file.path(h, "/aim_outputs/Aim2/D_tables_figures/", date_today)
ensure_dir_exists(dir_output)
fp_out <- file.path(dir_output, "HIV_Health_Affairs_table.csv")

##----------------------------------------------------------------
## 3. Read data
##----------------------------------------------------------------
t1 <- fread(fp_t1)
t3 <- fread(fp_t3)
frontier <- fread(fp_frontier)

t1 <- t1 %>%
  dplyr::mutate(
    dplyr::across(
      c(spend_all_mean, spend_all_lower, spend_all_upper),
      ~ readr::parse_number(.x)
    )
  )

##----------------------------------------------------------------
## 4. Flexible column mapping
##----------------------------------------------------------------
col_location <- first_existing_col(t3, c("Level", "location_name", "Location", "state_name"))

col_spend_pc_2019 <- first_existing_col(t3, c("Spend per Case 2019", "spend_per_case_2019"))
col_spend_pc_2019_l <- first_existing_col(t3, c("Spend per Case 2019 Lower", "spend_per_case_2019_lower", "spend_per_case_2019_l"), required = FALSE)
col_spend_pc_2019_u <- first_existing_col(t3, c("Spend per Case 2019 Upper", "spend_per_case_2019_upper", "spend_per_case_2019_u"), required = FALSE)

col_daly_pc_2019 <- first_existing_col(t3, c("DALY per Case 2019", "daly_per_case_2019"))
col_daly_pc_2019_l <- first_existing_col(t3, c("DALY per Case 2019 Lower", "daly_per_case_2019_lower", "daly_per_case_2019_l"), required = FALSE)
col_daly_pc_2019_u <- first_existing_col(t3, c("DALY per Case 2019 Upper", "daly_per_case_2019_upper", "daly_per_case_2019_u"), required = FALSE)

col_se_median <- first_existing_col(t3, c("spend_effectiveness_median", "Spending Effectiveness - Decomp", "spending_effectiveness_decomp"))
col_se_q25 <- first_existing_col(t3, c("spend_effectiveness_q25", "spending_effectiveness_q25"), required = FALSE)
col_se_q75 <- first_existing_col(t3, c("spend_effectiveness_q75", "spending_effectiveness_q75"), required = FALSE)

col_se_med_cat <- first_existing_col(t3, c("spend_effectiveness_median_category", "Category", "category"))
col_se_q25_cat <- first_existing_col(t3, c("spend_effectiveness_q25_category", "spending_effectiveness_q25_category"))
col_se_q75_cat <- first_existing_col(t3, c("spend_effectiveness_q75_category", "spending_effectiveness_q75_category"))

frontier_location <- first_existing_col(frontier, c("location_name", "Level", "Location"))
frontier_ineff_mean <- first_existing_col(frontier, c("ineff_mean", "mean_ineff", "inefficiency_mean"))
frontier_ineff_change <- first_existing_col(frontier, c("ineff_change", "delta_frontier_inefficiency", "frontier_ineff_change"))

t1_location <- first_existing_col(t1, c("location_name", "Level", "Location", "State", "state_name"))
t1_cause <- first_existing_col(t1, c("Cause", "cause_name", "cause"), required = FALSE)
t1_spend_all_mean <- first_existing_col(t1, c("spend_all_mean", "spend_all", "total_spending_2019"))
t1_spend_all_lower <- first_existing_col(t1, c("spend_all_lower", "spend_all_l", "total_spending_2019_lower"), required = FALSE)
t1_spend_all_upper <- first_existing_col(t1, c("spend_all_upper", "spend_all_u", "total_spending_2019_upper"), required = FALSE)

##----------------------------------------------------------------
## 5. Prepare Table 3 fields
##----------------------------------------------------------------
t3_prepped <- t3 %>%
  mutate(
    location_name = .data[[col_location]],
    spend_per_case_2019 = .data[[col_spend_pc_2019]],
    spend_per_case_2019_lower = if (!is.na(col_spend_pc_2019_l)) .data[[col_spend_pc_2019_l]] else NA_real_,
    spend_per_case_2019_upper = if (!is.na(col_spend_pc_2019_u)) .data[[col_spend_pc_2019_u]] else NA_real_,
    daly_per_case_2019 = .data[[col_daly_pc_2019]],
    daly_per_case_2019_lower = if (!is.na(col_daly_pc_2019_l)) .data[[col_daly_pc_2019_l]] else NA_real_,
    daly_per_case_2019_upper = if (!is.na(col_daly_pc_2019_u)) .data[[col_daly_pc_2019_u]] else NA_real_,
    spend_effectiveness_median = .data[[col_se_median]],
    spend_effectiveness_q25 = if (!is.na(col_se_q25)) .data[[col_se_q25]] else NA_real_,
    spend_effectiveness_q75 = if (!is.na(col_se_q75)) .data[[col_se_q75]] else NA_real_,
    spend_effectiveness_median_category = as.integer(.data[[col_se_med_cat]]),
    spend_effectiveness_q25_category = as.integer(.data[[col_se_q25_cat]]),
    spend_effectiveness_q75_category = as.integer(.data[[col_se_q75_cat]])
  ) %>%
  mutate(
    sort_note = if_else(
      spend_effectiveness_q25_category == 1L & spend_effectiveness_q75_category == 1L,
      1L,
      2L
    )
  ) %>%
  select(
    sort_note,
    spend_effectiveness_median_category,
    location_name,
    spend_per_case_2019,
    spend_per_case_2019_lower,
    spend_per_case_2019_upper,
    daly_per_case_2019,
    daly_per_case_2019_lower,
    daly_per_case_2019_upper,
    spend_effectiveness_median,
    spend_effectiveness_q25,
    spend_effectiveness_q75
  )

##----------------------------------------------------------------
## 6. Prepare frontier fields and T1 fields
##----------------------------------------------------------------
frontier_prepped <- frontier %>%
  transmute(
    location_name = .data[[frontier_location]],
    ineff_mean = .data[[frontier_ineff_mean]],
    ineff_change = .data[[frontier_ineff_change]]
  )

t1_prepped <- t1 %>%
  {
    if (!is.na(t1_cause)) {
      dplyr::filter(., stringr::str_detect(tolower(.data[[t1_cause]]), "hiv"))
    } else {
      .
    }
  } %>%
  transmute(
    location_name = stringr::str_trim(.data[[t1_location]]),
    spend_all_mean = .data[[t1_spend_all_mean]],
    spend_all_lower = if (!is.na(t1_spend_all_lower)) .data[[t1_spend_all_lower]] else NA_real_,
    spend_all_upper = if (!is.na(t1_spend_all_upper)) .data[[t1_spend_all_upper]] else NA_real_
  )

##----------------------------------------------------------------
## 7. Formatting helpers (use plain hyphen instead of en-dash
##    to avoid Excel encoding issues)
##----------------------------------------------------------------
fmt_ci_dollar <- function(mean, low, high, with_dollar = TRUE) {
  if (is.na(mean)) return(NA_character_)
  mean_txt <- format(round(mean, 0), big.mark = ",", scientific = FALSE, trim = TRUE)
  if (with_dollar) mean_txt <- paste0("$", mean_txt)
  has_ci <- !is.na(low) & !is.na(high)
  if (has_ci) {
    low_txt <- format(round(low, 0), big.mark = ",", scientific = FALSE, trim = TRUE)
    high_txt <- format(round(high, 0), big.mark = ",", scientific = FALSE, trim = TRUE)
    return(paste0(mean_txt, " (", low_txt, "-", high_txt, ")"))
  }
  mean_txt
}

fmt_ci_daly <- function(mean, low, high) {
  if (is.na(mean)) return(NA_character_)
  mean_txt <- format(round(mean, 3), nsmall = 3, trim = TRUE)
  has_ci <- !is.na(low) & !is.na(high)
  if (has_ci) {
    low_txt <- format(round(low, 3), nsmall = 3, trim = TRUE)
    high_txt <- format(round(high, 3), nsmall = 3, trim = TRUE)
    return(paste0(mean_txt, " (", low_txt, "-", high_txt, ")"))
  }
  mean_txt
}

category_label <- function(x) {
  dplyr::case_when(
    x == 1L ~ "Category 1: Increased spending, improved health",
    x == 2L ~ "Category 2: Cost-saving (improved health, lower spending)",
    x == 3L ~ "Category 3",
    x == 4L ~ "Category 4",
    TRUE ~ paste0("Category ", x)
  )
}

##----------------------------------------------------------------
## 8. Merge, format, sort
##----------------------------------------------------------------
manuscript_table <- t3_prepped %>%
  left_join(t1_prepped, by = "location_name") %>%
  left_join(frontier_prepped, by = "location_name") %>%
  mutate(
    effectiveness_category = category_label(spend_effectiveness_median_category),
    `Location` = location_name,
    `Total spending (2019)` = mapply(fmt_ci_dollar, spend_all_mean, spend_all_lower, spend_all_upper, TRUE),
    `Spending per case (2019)` = mapply(fmt_ci_dollar, spend_per_case_2019, spend_per_case_2019_lower, spend_per_case_2019_upper, TRUE),
    `DALY per case (2019)` = mapply(fmt_ci_daly, daly_per_case_2019, daly_per_case_2019_lower, daly_per_case_2019_upper),
    `Spending effectiveness ($/DALY averted)` = mapply(fmt_ci_dollar, spend_effectiveness_median, spend_effectiveness_q25, spend_effectiveness_q75, FALSE),
    `Mean inefficiency (2010-2019)` = if_else(is.na(ineff_mean), NA_character_, format(round(ineff_mean, 3), nsmall = 3, trim = TRUE)),
    `Change in inefficiency` = if_else(is.na(ineff_change), NA_character_, format(round(ineff_change, 3), nsmall = 3, trim = TRUE))
  ) %>%
  arrange(sort_note, spend_effectiveness_median_category, ineff_mean, location_name) %>%
  select(
    effectiveness_category,
    Location,
    `Total spending (2019)`,
    `Spending per case (2019)`,
    `DALY per case (2019)`,
    `Spending effectiveness ($/DALY averted)`,
    `Mean inefficiency (2010-2019)`,
    `Change in inefficiency`
  )

##----------------------------------------------------------------
## 9. Insert category header rows (appears once per group)
##    The header row puts the category label in the Location column
##    and leaves all other columns blank.
##----------------------------------------------------------------
# Define the final column names (without the internal sorting column)
final_cols <- c(
  "Location",
  "Total spending (2019)",
  "Spending per case (2019)",
  "DALY per case (2019)",
  "Spending effectiveness ($/DALY averted)",
  "Mean inefficiency (2010-2019)",
  "Change in inefficiency"
)

grouped_table <- manuscript_table %>%
  group_by(effectiveness_category) %>%
  group_modify(~ {
    # Create a header row: category label goes in Location column, rest blank
    header_row <- as.data.frame(
      setNames(
        as.list(rep("", length(final_cols))),
        final_cols
      ),
      stringsAsFactors = FALSE
    )
    header_row$Location <- .y$effectiveness_category
    
    # Data rows: drop the category column
    data_rows <- .x %>% select(all_of(final_cols))
    
    bind_rows(header_row, data_rows)
  }) %>%
  ungroup() %>%
  select(all_of(final_cols))

# Replace NAs with footnote marker
grouped_table[is.na(grouped_table)] <- "-*"

##----------------------------------------------------------------
## 10. Write with UTF-8 BOM so Excel reads it correctly
##----------------------------------------------------------------
# Write BOM + CSV content
con <- file(fp_out, open = "wb")
writeBin(charToRaw("\xef\xbb\xbf"), con)  # UTF-8 BOM
close(con)
write.table(
  grouped_table, file = fp_out, append = TRUE,
  sep = ",", row.names = FALSE, quote = TRUE,
  fileEncoding = "UTF-8"
)

cat("\nSaved manuscript table to:\n", fp_out, "\n", sep = "")
cat("Rows:", nrow(grouped_table), "| Columns:", ncol(grouped_table), "\n")