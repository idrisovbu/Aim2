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
date_t3 <- "20260505" # last run per Marcia's feedbaack
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

#####to delete below


##----------------------------------------------------------------
## E_HIV_frontier_vs_effectiveness.R
##
## Purpose: Empirically compare the two efficiency frameworks
##   (frontier / benchmark vs. longitudinal spending effectiveness)
##   to address Weaver (why both?) and Dilemna (any relationship
##   between inefficiency, change in inefficiency, and effectiveness?).
##
## Assumes D_HIV_Health_Affairs_table.R has been run; t3, t3_prepped,
## frontier_prepped, frontier, and dir_output are in the session.
##----------------------------------------------------------------

pacman::p_load(ggplot2, ggrepel, tidyr, dplyr)

##---- E.0 Output location ---------------------------------------------------
dir_E <- file.path(dir_output, "E_reviewer_response")
ensure_dir_exists(dir_E)

##---- E.1 Build merged state-level frame ------------------------------------
# Pull raw change fields and category from t3 (these weren't kept in t3_prepped)
t3_support <- t3 %>%
  transmute(
    location_name                = .data[[col_location]],
    category                     = as.integer(.data[[col_se_med_cat]]),
    spend_effectiveness_median   = .data[[col_se_median]],
    change_spend_per_case        = change_spend_per_case,
    change_daly_per_case         = change_daly_per_case,
    change_daly_averted_per_case = change_daly_averted_per_case,
    delta_spend                  = delta_spend,
    delta_daly                   = delta_daly
  )

# State abbreviations live on the frontier file
frontier_abbrev <- frontier %>%
  distinct(
    location_name = .data[[frontier_location]],
    state_abbrev
  )

merged <- t3_support %>%
  left_join(frontier_prepped, by = "location_name") %>%
  left_join(frontier_abbrev,  by = "location_name")

# Drop national row if present (we want state-level correlations)
merged <- merged %>% filter(!is.na(state_abbrev))

write.csv(merged,
          file.path(dir_E, "E_frontier_x_effectiveness_merged.csv"),
          row.names = FALSE)

##---- E.2 Spearman correlations (point estimates + n) -----------------------
# All pairs among the core variables. Pairs involving spending effectiveness
# are naturally restricted to Cat 1 states because effectiveness is NA elsewhere.
spearman_pair <- function(x, y) {
  keep <- !is.na(x) & !is.na(y)
  if (sum(keep) < 3) {
    return(data.frame(rho = NA_real_, n = sum(keep), p = NA_real_))
  }
  tt <- suppressWarnings(cor.test(x[keep], y[keep],
                                  method = "spearman", exact = FALSE))
  data.frame(rho = unname(tt$estimate),
             n   = sum(keep),
             p   = tt$p.value)
}

core_vars <- list(
  ineff_mean            = merged$ineff_mean,
  ineff_change          = merged$ineff_change,
  spend_effectiveness   = merged$spend_effectiveness_median,
  change_spend_per_case = merged$change_spend_per_case,
  change_daly_per_case  = merged$change_daly_per_case
)

pairs_df <- expand.grid(v1 = names(core_vars),
                        v2 = names(core_vars),
                        stringsAsFactors = FALSE) %>%
  filter(v1 < v2)

cor_tbl <- do.call(rbind, lapply(seq_len(nrow(pairs_df)), function(i) {
  cbind(pairs_df[i, ],
        spearman_pair(core_vars[[ pairs_df$v1[i] ]],
                      core_vars[[ pairs_df$v2[i] ]]))
})) %>%
  mutate(
    rho = round(rho, 2),
    p   = signif(p, 3)
  ) %>%
  arrange(v1, v2)

cat("\n--- Spearman correlations (point estimates, pairwise n) ---\n")
print(cor_tbl, row.names = FALSE)

write.csv(cor_tbl,
          file.path(dir_E, "E_correlations_spearman.csv"),
          row.names = FALSE)

##---- E.3 Category-stratified inefficiency summary --------------------------
cat_summary <- merged %>%
  group_by(category) %>%
  summarise(
    n                 = dplyr::n(),
    ineff_mean_med    = round(median(ineff_mean,   na.rm = TRUE), 3),
    ineff_mean_iqr    = paste0(round(quantile(ineff_mean,   0.25, na.rm = TRUE), 3),
                               " to ",
                               round(quantile(ineff_mean,   0.75, na.rm = TRUE), 3)),
    ineff_change_med  = round(median(ineff_change, na.rm = TRUE), 3),
    ineff_change_iqr  = paste0(round(quantile(ineff_change, 0.25, na.rm = TRUE), 3),
                               " to ",
                               round(quantile(ineff_change, 0.75, na.rm = TRUE), 3)),
    .groups = "drop"
  )

kw_mean   <- kruskal.test(ineff_mean   ~ category, data = merged)
kw_change <- kruskal.test(ineff_change ~ category, data = merged)

cat("\n--- Inefficiency by spending-effectiveness category ---\n")
print(cat_summary, row.names = FALSE)
cat(sprintf("Kruskal-Wallis across categories: ineff_mean p = %.3g | ineff_change p = %.3g\n",
            kw_mean$p.value, kw_change$p.value))

write.csv(cat_summary,
          file.path(dir_E, "E_category_summary.csv"),
          row.names = FALSE)

##---- E.4 Scatter: frontier level vs. change, colored by category ----------
# Use the pipeline palette if available; these are safe defaults.
#c("#8DA0CB", "#66C2A5"),
cat_pal <- c("1" = "#8DA0CB",
             "2" = "#66C2A5",
             "3" = "#F46D43",
             "4" = "#D73027")
cat_lbl <- c("1" = "Cat 1: Increased spending, improved health",
             "2" = "Cat 2: Decreased spending, improved health (cost-saving)",
             "3" = "Cat 3",
             "4" = "Cat 4")

plot_df <- merged %>%
  mutate(cat_f = factor(category, levels = 1:4, labels = cat_lbl[as.character(1:4)]))

p_scatter <- ggplot(plot_df,
                    aes(x = ineff_mean, y = ineff_change)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_vline(xintercept = median(plot_df$ineff_mean, na.rm = TRUE),
             linetype = "dashed", color = "grey60") +
  geom_point(aes(color = cat_f), size = 3, alpha = 0.85) +
  ggrepel::geom_text_repel(
    aes(label = state_abbrev),
    size = 3, max.overlaps = Inf, min.segment.length = 0,
    segment.color = "grey70"
  ) +
  scale_color_manual(values = setNames(cat_pal, cat_lbl[as.character(1:4)]),
                     name   = "Spending-effectiveness category") +
  labs(
    x       = "Mean inefficiency, 2010-2019 (log residual vs. frontier)",
    y       = "Change in inefficiency (2019 vs. 2010)",
    caption = "Lower-left = efficient and improving. Upper-right = inefficient and worsening.\nVertical line at median ineff_mean; horizontal line at no change."
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.position = "bottom",
    legend.direction = "vertical",
    plot.caption = element_text(hjust = 0)
  )

ggsave(file.path(dir_E, "E_scatter_ineff_level_vs_change.pdf"),
       p_scatter, width = 8, height = 7.5)
ggsave(file.path(dir_E, "E_scatter_ineff_level_vs_change.png"),
       p_scatter, width = 8, height = 7.5, dpi = 300)

##---- E.5 2x2: frontier level vs. effectiveness group ----------------------
merged_cross <- merged %>%
  mutate(
    ineff_level = if_else(ineff_mean > median(ineff_mean, na.rm = TRUE),
                          "Above-median ineff_mean",
                          "At/below-median ineff_mean"),
    eff_grp     = if_else(category == 1L, "Cat 1 (spending up + health up)",
                          "Cat 2 (cost-saving + health up)")
  )

cross_tbl <- merged_cross %>%
  count(ineff_level, eff_grp) %>%
  pivot_wider(names_from = eff_grp, values_from = n, values_fill = 0)



tbl_for_fisher <- table(merged_cross$ineff_level, merged_cross$eff_grp)
fisher_res <- fisher.test(tbl_for_fisher)

cat("\n--- 2x2: frontier level x effectiveness group ---\n")
print(cross_tbl, row.names = FALSE)
cat(sprintf("Fisher's exact: OR = %.2f (95%% CI %.2f-%.2f), p = %.3g\n",
            fisher_res$estimate,
            fisher_res$conf.int[1], fisher_res$conf.int[2],
            fisher_res$p.value))

write.csv(cross_tbl,
          file.path(dir_E, "E_2x2_crosstab.csv"),
          row.names = FALSE)

cat("\nAll outputs written to:\n", dir_E, "\n", sep = "")

####checking


##----------------------------------------------------------------
## E.6 Cost per DALY averted vs. frontier efficiency (Joe's ask)
##
## Directly scatters $/DALY averted against frontier inefficiency.
## Uses mean_cat1_ratio (mean of Cat-1 ratios across the 51x51 crossed
## draws) so every state gets a numeric cost — including Cat 2
## cost-saving states, which are NA on spend_effectiveness_median.
## pct_cat1 = share of crossed draws in Cat 1 = reliability flag.
##----------------------------------------------------------------

t3_cost <- t3 %>%
  transmute(
    location_name   = .data[[col_location]],
    category        = as.integer(.data[[col_se_med_cat]]),
    mean_cat1_ratio = mean_cat1_ratio,
    pct_cat1        = pct_cat1
  )

cost_vs_frontier <- t3_cost %>%
  left_join(frontier_prepped, by = "location_name") %>%
  left_join(frontier_abbrev,  by = "location_name") %>%
  filter(
    !is.na(state_abbrev),            # drop national row
    !is.na(mean_cat1_ratio),
    is.finite(mean_cat1_ratio),
    mean_cat1_ratio > 0,             # log-axis safe
    pct_cat1 >= 10                   # reliability floor; relax/raise as needed
  )

##---- E.6.1 Spearman correlations ------------------------------------------
rho_level  <- with(cost_vs_frontier,
                   cor.test(mean_cat1_ratio, ineff_mean,
                            method = "spearman", exact = FALSE))
rho_change <- with(cost_vs_frontier,
                   cor.test(mean_cat1_ratio, ineff_change,
                            method = "spearman", exact = FALSE))

cat(sprintf("\nSpearman: cost vs. ineff_mean   rho = %.2f, p = %.3g, n = %d\n",
            rho_level$estimate,  rho_level$p.value,  nrow(cost_vs_frontier)))
cat(sprintf("Spearman: cost vs. ineff_change rho = %.2f, p = %.3g, n = %d\n",
            rho_change$estimate, rho_change$p.value, nrow(cost_vs_frontier)))

##---- E.6.2 Primary scatter: cost vs. inefficiency level -------------------
cat_pal <- c("1" = "#8DA0CB", "2" = "#66C2A5", "3" = "#F46D43", "4" = "#D73027")
cat_lbl <- c("1" = "Cat 1: +Spend, +Health",
             "2" = "Cat 2: Cost-saving",
             "3" = "Cat 3: Dominated",
             "4" = "Cat 4: -Both")


plot_df <- cost_vs_frontier %>%
  mutate(cat_f = factor(category, levels = 1:4,
                        labels = cat_lbl[as.character(1:4)]))

p_cost_level <- ggplot(plot_df, aes(x = ineff_mean, y = mean_cat1_ratio)) +
  geom_smooth(method = "lm", se = TRUE,
              color = "grey40", fill = "grey85", linewidth = 0.6) +
  geom_point(aes(color = cat_f, size = pct_cat1), alpha = 0.85) +
  ggrepel::geom_text_repel(aes(label = state_abbrev),
                           size = 3, max.overlaps = Inf,
                           min.segment.length = 0,
                           segment.color = "grey70") +
  scale_y_log10(labels = scales::dollar) +
  scale_color_manual(values = setNames(cat_pal, cat_lbl), drop = FALSE,
                     name = "Category (point estimate)") +
  scale_size_continuous(range = c(2, 6), name = "% Cat-1 draws") +
  labs(
    title    = "Spending effectiveness vs. frontier inefficiency, HIV (2010-2019)",
    subtitle = sprintf("Spearman rho = %.2f (p = %.3g, n = %d)",
                       rho_level$estimate, rho_level$p.value, nrow(plot_df)),
    x        = "Mean inefficiency, 2010-2019 (frontier residual)",
    y        = "Mean cost per DALY averted ($ / DALY, log scale)",
    caption  = paste0(
      "Cost = mean of Cat-1 ratios across 51x51 crossed draws (Weaver crossing).\n",
      "Point size = % of crossed draws falling in Cat 1 (reliability)."
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.position  = "bottom",
    legend.box       = "vertical",
    plot.caption     = element_text(hjust = 0)
  )

ggsave(file.path(dir_E, "E6_cost_vs_inefficiency_level.png"),
       p_cost_level, width = 9, height = 7.5, dpi = 300, bg = "white")

##---- E.6.3 Companion scatter: cost vs. change in inefficiency -------------
p_cost_change <- ggplot(plot_df, aes(x = ineff_change, y = mean_cat1_ratio)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
  geom_smooth(method = "lm", se = TRUE,
              color = "grey40", fill = "grey85", linewidth = 0.6) +
  geom_point(aes(color = cat_f, size = pct_cat1), alpha = 0.85) +
  ggrepel::geom_text_repel(aes(label = state_abbrev),
                           size = 3, max.overlaps = Inf,
                           min.segment.length = 0,
                           segment.color = "grey70") +
  scale_y_log10(labels = scales::dollar) +
  scale_color_manual(values = setNames(cat_pal, cat_lbl), drop = FALSE,
                     name = "Category (point estimate)") +
  scale_size_continuous(range = c(2, 6), name = "% Cat-1 draws") +
  labs(
    title    = "Spending effectiveness vs. change in frontier inefficiency",
    subtitle = sprintf("Spearman rho = %.2f (p = %.3g, n = %d)",
                       rho_change$estimate, rho_change$p.value, nrow(plot_df)),
    x        = "Change in inefficiency, 2019 vs. 2010 (negative = improved)",
    y        = "Mean cost per DALY averted ($ / DALY, log scale)"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", legend.box = "vertical")

ggsave(file.path(dir_E, "E6_cost_vs_inefficiency_change.png"),
       p_cost_change, width = 9, height = 7.5, dpi = 300, bg = "white")


##---- E.6.4 Two-by-two: median splits + Fisher's exact ---------------------
cost_quad <- plot_df %>%
  mutate(
    cost_grp  = if_else(mean_cat1_ratio > median(mean_cat1_ratio, na.rm = TRUE),
                        "High $/DALY averted", "Low $/DALY averted"),
    ineff_grp = if_else(ineff_mean > median(ineff_mean, na.rm = TRUE),
                        "High inefficiency", "Low inefficiency")
  )

quad_tbl <- cost_quad %>%
  count(ineff_grp, cost_grp) %>%
  pivot_wider(names_from = cost_grp, values_from = n, values_fill = 0)

quad_fisher <- fisher.test(table(cost_quad$ineff_grp, cost_quad$cost_grp))

cat("\n--- 2x2: cost x inefficiency (median splits) ---\n")
print(quad_tbl, row.names = FALSE)
cat(sprintf("Fisher's exact: OR = %.2f (95%% CI %.2f-%.2f), p = %.3g\n",
            quad_fisher$estimate,
            quad_fisher$conf.int[1], quad_fisher$conf.int[2],
            quad_fisher$p.value))

write.csv(quad_tbl,
          file.path(dir_E, "E6_2x2_cost_x_inefficiency.csv"),
          row.names = FALSE)

##---- E.6.5 Off-diagonal states (the interesting cases) --------------------
off_diag <- cost_quad %>%
  filter((ineff_grp == "Low inefficiency"  & cost_grp == "High $/DALY averted") |
           (ineff_grp == "High inefficiency" & cost_grp == "Low $/DALY averted")) %>%
  arrange(ineff_grp, desc(mean_cat1_ratio)) %>%
  select(state_abbrev, location_name, category,
         ineff_mean, mean_cat1_ratio, pct_cat1)

cat("\n--- Off-diagonal states ---\n")
print(off_diag, row.names = FALSE)

write.csv(off_diag,
          file.path(dir_E, "E6_off_diagonal_states.csv"),
          row.names = FALSE)

##---- E.6.6 Save merged data for the section -------------------------------
write.csv(cost_vs_frontier,
          file.path(dir_E, "E6_cost_vs_frontier_data.csv"),
          row.names = FALSE)

cat("\nE.6 outputs written to:\n", dir_E, "\n", sep = "")

##----------------------------------------------------------------
## F_HIV_state_frontier_arrows.R
## State-level HIV efficiency frontier with 2010-14 -> 2015-19 arrows,
## color-coded by spending-effectiveness category.
##----------------------------------------------------------------

##----------------------------------------------------------------
pacman::p_load(dplyr, tidyr, ggplot2, ggrepel, readr, scales, stringr)

## 1. Yearly frontier output ------------------------------------------------
date_frontier <- "20260411"
fp_yearly <- file.path(
  h, "aim_outputs/Aim2/C_frontier_analysis/",
  date_frontier, "hiv_yfe_daly_output.csv"
)
yearly <- read_csv(fp_yearly, show_col_types = FALSE)

## 2. State early/late means ------------------------------------------------
early_late_state <- yearly %>%
  mutate(period = case_when(
    year_id >= 2010 & year_id <= 2014 ~ "early",
    year_id >= 2015 & year_id <= 2019 ~ "late",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(period)) %>%
  group_by(location_name, location_id, period) %>%
  summarise(
    log_spend = mean(log_spend_orig, na.rm = TRUE),
    y_adj_log = mean(y_adj_log,      na.rm = TRUE),
    .groups = "drop"
  )

## 2b. US = unweighted mean across states, per period -----------------------
early_late_us <- early_late_state %>%
  group_by(period) %>%
  summarise(
    location_name = "United States",
    location_id   = 102,
    log_spend     = mean(log_spend),
    y_adj_log     = mean(y_adj_log),
    .groups       = "drop"
  ) %>%
  select(location_name, location_id, period, log_spend, y_adj_log)

early_late_all <- bind_rows(early_late_state, early_late_us) %>%
  pivot_wider(
    id_cols     = c(location_name, location_id),
    names_from  = period,
    values_from = c(log_spend, y_adj_log)
  )

## 3. Frontier curve --------------------------------------------------------
frontier_curve <- yearly %>%
  distinct(log_spend_orig, y_adj_hat_log) %>%
  arrange(log_spend_orig)

## 4. Categories and abbreviations ------------------------------------------
t3_cat <- t3 %>%
  transmute(
    location_name = location_name,
    se_category   = as.integer(category)
  )

abbrev_all <- frontier %>%
  distinct(location_name, state_abbrev)

## 5. Plot frame ------------------------------------------------------------
cat1_lbl <- "Category 1: Increased spending with improved health"
cat2_lbl <- "Category 2: Reduced spending with improved health (cost-saving)"

plot_df <- early_late_all %>%
  left_join(abbrev_all, by = "location_name") %>%
  left_join(t3_cat,     by = "location_name") %>%
  mutate(
    is_us = location_name == "United States",
    cat_f = factor(
      se_category,
      levels = c(1, 2),
      labels = c(cat1_lbl, cat2_lbl)
    )
  )

cat_pal <- setNames(c("#8DA0CB", "#66C2A5"), c(cat1_lbl, cat2_lbl))

## 6. Axis break definitions (displayed in raw units) -----------------------
dollar_breaks <- c(6000, 8000, 10000, 12000, 15000, 20000)
y_raw_breaks  <- c(0.75, 0.90, 1.00, 1.10, 1.25, 1.50)

## 7. Plot ------------------------------------------------------------------
p_frontier_arrows <- ggplot() +
  geom_line(
    data = frontier_curve,
    aes(x = log_spend_orig, y = y_adj_hat_log),
    color = "black", linewidth = 0.9
  ) +
  # State arrows
  geom_segment(
    data = plot_df %>% filter(!is_us,
                              !is.na(log_spend_early),
                              !is.na(log_spend_late)),
    aes(x    = log_spend_early, y    = y_adj_log_early,
        xend = log_spend_late,  yend = y_adj_log_late,
        color = cat_f),
    arrow     = arrow(length = unit(0.18, "cm"), type = "closed"),
    linewidth = 0.65, alpha = 0.85
  ) +
  # US arrow
  geom_segment(
    data = plot_df %>% filter(is_us),
    aes(x    = log_spend_early, y    = y_adj_log_early,
        xend = log_spend_late,  yend = y_adj_log_late),
    arrow     = arrow(length = unit(0.28, "cm"), type = "closed"),
    color     = "#6F8FC7", linewidth = 1.6
  ) +
  # State labels
  ggrepel::geom_text_repel(
    data = plot_df %>% filter(!is_us),
    aes(x = log_spend_late, y = y_adj_log_late,
        label = state_abbrev, color = cat_f),
    fontface           = "bold", size = 2.8,
    box.padding        = 0.15, point.padding = 0.05,
    segment.color      = "grey75", segment.size = 0.2,
    min.segment.length = 0.2, max.overlaps = Inf,
    show.legend        = FALSE
  ) +
  # US label
  ggrepel::geom_text_repel(
    data = plot_df %>% filter(is_us),
    aes(x = log_spend_late, y = y_adj_log_late, label = state_abbrev),
    fontface = "bold", size = 4.4, color = "#2D5963",
    box.padding = 0.45, show.legend = FALSE
  ) +
  scale_color_manual(
    values = cat_pal,
    name   = "Spending-effectiveness category, 2010-2019:",
    drop   = FALSE
  ) +
  scale_x_continuous(
    breaks = log(dollar_breaks),
    labels = scales::dollar(dollar_breaks),
    name   = "HIV spending per prevalent case (2019 USD)"
  ) +
  scale_y_continuous(
    breaks = log(y_raw_breaks),
    labels = format(y_raw_breaks, nsmall = 2, trim = TRUE),
    name   = "Covariate-adjusted DALY per prevalent case (index)"
  ) +
  labs(
    title    = "State-Level Frontier Benchmarking and Spending Effectiveness, HIV, 2010-2019",
    subtitle = str_wrap(
      paste0(
        "The black curve shows the efficiency frontier (best attainable DALYs per case ",
        "at each spending level); arrows show each state's movement relative to the ",
        "frontier from 2010 to 2019."
      ),
      width = 130
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.background   = element_rect(fill = "white", color = NA),
    panel.background  = element_rect(fill = "white", color = NA),
    panel.grid.major  = element_line(color = "grey92"),
    panel.grid.minor  = element_blank(),
    plot.title        = element_text(face = "bold", size = 13,
                                     margin = margin(b = 4)),
    plot.subtitle     = element_text(color = "grey25", size = 10,
                                     lineheight = 1.15,
                                     margin = margin(b = 10)),
    plot.title.position = "plot",
    plot.margin       = margin(t = 12, r = 14, b = 10, l = 14),
    axis.title.x      = element_text(margin = margin(t = 8)),
    axis.title.y      = element_text(margin = margin(r = 8)),
    # Horizontal legend below the plot
    legend.position   = "bottom",
    legend.direction  = "horizontal",
    legend.box        = "horizontal",
    legend.title      = element_text(face = "bold", size = 10),
    legend.text       = element_text(size = 9),
    legend.key.width  = unit(1.4, "cm"),
    legend.spacing.x  = unit(0.3, "cm"),
    legend.margin     = margin(t = 6)
  ) +
  guides(color = guide_legend(
    title.position = "left",
    title.vjust    = 0.5,
    nrow           = 1,
    override.aes   = list(linewidth = 1.3, size = 3)
  ))

## 8. Save ------------------------------------------------------------------
ggsave(
  file.path(dir_output, "F_state_frontier_arrows.png"),
  p_frontier_arrows,
  width  = 13,
  height = 8.5,
  dpi    = 700,
  bg     = "white"
)