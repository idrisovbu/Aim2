##----------------------------------------------------------------
## F_HIV_state_frontier_arrows_KxV.R
##
## State-level K×V (population viral suppression) frontier with
## 2010-2014 -> 2015-2019 trajectory arrows. Color: per-state V-data
## completeness across 2010-2019 (3 bins). Explicit caption disclosure
## of states with no V data and states with only one period.
##
## Produces TWO figures from the same batch output:
##   1) F_state_frontier_arrows_KxV_lagPrev.png  (matches locked primary)
##   2) F_state_frontier_arrows_KxV_noPrev.png   (no-prev robustness)
##
## Reads batch-fit output from C_03_HIV_sfma_batch_final.py.
##----------------------------------------------------------------

## 0. Packages and paths ---------------------------------------------------
pacman::p_load(dplyr, tidyr, ggplot2, ggrepel, readr, scales, stringr)

if (Sys.info()["sysname"] == "Linux") {
  h <- paste0("/ihme/homes/", Sys.info()[7], "/")
} else if (Sys.info()["sysname"] == "Darwin") {
  h <- paste0("/Volumes/", Sys.info()[7], "/")
} else {
  h <- "H:/"
}

# Update this to the latest batch-fit date
date_batch <- "20260602"

fp_long  <- file.path(h, "aim_outputs/Aim2/C_frontier_analysis",
                      date_batch, "frontier_batch_mvp", "frontiers_long.csv")
fp_meta  <- file.path(h, "aim_outputs/Aim2/C_frontier_analysis",
                      date_batch, "frontier_batch_mvp", "frontiers_metadata.csv")
fp_panel <- file.path(h, "aim_outputs/Aim2/C_frontier_analysis",
                      "20260517", "analysis_per_capita", "df_hiv_cascade_panel.csv")

dir_output <- file.path(h, "aim_outputs/Aim2/D_tables_figures",
                        format(Sys.time(), "%Y%m%d"))
if (!dir.exists(dir_output)) dir.create(dir_output, recursive = TRUE)

## 1. Load shared data once ------------------------------------------------
long_all <- read_csv(fp_long, show_col_types = FALSE)
meta_all <- read_csv(fp_meta, show_col_types = FALSE)
panel    <- read_csv(fp_panel, show_col_types = FALSE) %>% filter(acause == "hiv")

# State abbreviations
STATE_ABBREV <- c(
  "Alabama"="AL","Alaska"="AK","Arizona"="AZ","Arkansas"="AR","California"="CA",
  "Colorado"="CO","Connecticut"="CT","Delaware"="DE","District of Columbia"="DC",
  "Florida"="FL","Georgia"="GA","Hawaii"="HI","Idaho"="ID","Illinois"="IL",
  "Indiana"="IN","Iowa"="IA","Kansas"="KS","Kentucky"="KY","Louisiana"="LA",
  "Maine"="ME","Maryland"="MD","Massachusetts"="MA","Michigan"="MI","Minnesota"="MN",
  "Mississippi"="MS","Missouri"="MO","Montana"="MT","Nebraska"="NE","Nevada"="NV",
  "New Hampshire"="NH","New Jersey"="NJ","New Mexico"="NM","New York"="NY",
  "North Carolina"="NC","North Dakota"="ND","Ohio"="OH","Oklahoma"="OK","Oregon"="OR",
  "Pennsylvania"="PA","Rhode Island"="RI","South Carolina"="SC","South Dakota"="SD",
  "Tennessee"="TN","Texas"="TX","Utah"="UT","Vermont"="VT","Virginia"="VA",
  "Washington"="WA","West Virginia"="WV","Wisconsin"="WI","Wyoming"="WY"
)

# V-data completeness per state (computed from the source panel, not the
# spec-filtered long file — so coverage reflects underlying CDC ATLAS data,
# not the analytic sample which may drop years for other reasons)
v_coverage <- panel %>%
  filter(year_id >= 2010, year_id <= 2019) %>%
  group_by(location_name) %>%
  summarise(
    n_years_V = sum(!is.na(cdc_viral_suppress)),
    .groups   = "drop"
  ) %>%
  mutate(
    coverage_bin = case_when(
      n_years_V >= 9 ~ "Full (9-10 yrs)",
      n_years_V >= 5 ~ "Partial (5-8 yrs)",
      n_years_V >= 1 ~ "Sparse (1-4 yrs)",
      TRUE           ~ "None (0 yrs)"
    ),
    coverage_bin = factor(
      coverage_bin,
      levels = c("Full (9-10 yrs)", "Partial (5-8 yrs)",
                 "Sparse (1-4 yrs)", "None (0 yrs)")
    )
  )

# Coverage palette: sequential dark→light by completeness.
# Sparse bumped from #9ECAE1 to #6BAED6 so hollow circles stay visible
# against the white panel. "None" kept in the palette but dropped from
# the legend via drop=TRUE (those states are off-plot anyway).
coverage_pal <- c(
  "Full (9-10 yrs)"   = "#08519C",  # deep blue
  "Partial (5-8 yrs)" = "#3182BD",  # mid blue
  "Sparse (1-4 yrs)"  = "#6BAED6",  # medium blue — readable for hollow circles
  "None (0 yrs)"      = "grey70"
)

# US accent color: warm amber so it pops against the blue palette
US_COLOR <- "#B45F06"

## 2. Figure-building function --------------------------------------------
build_frontier_arrows_figure <- function(spec_id,
                                         outfile_stub,
                                         spec_label) {
  
  cat("\n", strrep("=", 70), "\n", sep = "")
  cat("Building figure for spec: ", spec_id, "\n", sep = "")
  cat(strrep("=", 70), "\n", sep = "")
  
  long <- long_all %>% filter(spec_id == !!spec_id)
  meta <- meta_all %>% filter(spec_id == !!spec_id)
  
  stopifnot(nrow(long) > 0)
  cat("  rows: ", nrow(long),
      " | states: ", n_distinct(long$location_name),
      " | years: ", min(long$year_id), "-", max(long$year_id), "\n", sep = "")
  
  ## 2.1 Identify states with NO V data in analytic sample ---------------
  states_in_panel <- sort(unique(panel$location_name))
  states_with_v   <- sort(unique(long$location_name))
  states_no_v     <- setdiff(states_in_panel, states_with_v)
  states_no_v_abbrev <- unname(STATE_ABBREV[states_no_v])
  
  ## 2.2 Period means (early 2010-14, late 2015-19) -----------------------
  period_means <- long %>%
    mutate(period = case_when(
      year_id >= 2010 & year_id <= 2014 ~ "early",
      year_id >= 2015 & year_id <= 2019 ~ "late",
      TRUE ~ NA_character_
    )) %>%
    filter(!is.na(period)) %>%
    group_by(location_name, state_abbrev, period) %>%
    summarise(
      log_spend = mean(log_spend_orig, na.rm = TRUE),
      y_adj     = mean(y_obs_adj,      na.rm = TRUE),
      n_years   = dplyr::n(),
      .groups   = "drop"
    )
  
  status <- period_means %>%
    group_by(location_name, state_abbrev) %>%
    summarise(
      has_early = any(period == "early"),
      has_late  = any(period == "late"),
      .groups   = "drop"
    ) %>%
    mutate(role = case_when(
      has_early &  has_late ~ "arrow",
      TRUE                  ~ "dot"
    ))
  
  states_arrow    <- status %>% filter(role == "arrow") %>% pull(state_abbrev)
  states_dot_only <- status %>% filter(role == "dot")   %>% pull(state_abbrev)
  
  cat("  arrows (both periods): ", length(states_arrow),
      " | single-dot: ", length(states_dot_only),
      if (length(states_dot_only)) paste0(" (", paste(states_dot_only, collapse = ", "), ")") else "",
      "\n", sep = "")
  
  ## 2.3 Reshape data ---------------------------------------------------
  arrow_df <- period_means %>%
    filter(state_abbrev %in% states_arrow) %>%
    pivot_wider(
      id_cols     = c(location_name, state_abbrev),
      names_from  = period,
      values_from = c(log_spend, y_adj)
    ) %>%
    left_join(v_coverage, by = "location_name")
  
  dot_df <- period_means %>%
    filter(state_abbrev %in% states_dot_only) %>%
    rename(log_spend_pt = log_spend, y_adj_pt = y_adj) %>%
    left_join(v_coverage, by = "location_name")
  
  ## 2.4 US unweighted-mean trajectory -----------------------------------
  us_df <- period_means %>%
    filter(state_abbrev %in% states_arrow) %>%
    group_by(period) %>%
    summarise(log_spend = mean(log_spend), y_adj = mean(y_adj), .groups = "drop") %>%
    pivot_wider(names_from = period, values_from = c(log_spend, y_adj)) %>%
    mutate(state_abbrev = "US", location_name = "United States")
  
  ## 2.5 Frontier curve --------------------------------------------------
  frontier_curve <- long %>%
    distinct(log_spend_orig, y_frontier) %>%
    arrange(log_spend_orig)
  
  ## 2.6 Captions --------------------------------------------------------
  missing_caption <- if (length(states_no_v_abbrev) > 0) {
    paste0(
      "States with no V data 2010-2019, excluded: ",
      paste(sort(states_no_v_abbrev), collapse = ", "), ". "
    )
  } else ""
  
  dot_caption <- if (length(states_dot_only) > 0) {
    paste0(
      "States shown as single dots (V data in only one period): ",
      paste(sort(states_dot_only), collapse = ", "), ". "
    )
  } else ""
  
  ## 2.7 Plot ------------------------------------------------------------
  dollar_breaks <- c(5000, 7500, 10000, 12500, 15000, 20000)
  
  p <- ggplot() +
    # Reference line at logit = 0  (K × V = 0.50 on the probability scale)
    geom_hline(
      yintercept = 0, color = "grey55",
      linetype = "dashed", linewidth = 0.4
    ) +
    geom_line(
      data = frontier_curve,
      aes(x = log_spend_orig, y = y_frontier),
      color = "black", linewidth = 0.9
    ) +
    # State arrows
    geom_segment(
      data = arrow_df,
      aes(x    = log_spend_early, y    = y_adj_early,
          xend = log_spend_late,  yend = y_adj_late,
          color = coverage_bin),
      arrow     = arrow(length = unit(0.22, "cm"), type = "closed"),
      linewidth = 0.65, alpha = 0.9
    ) +
    ggrepel::geom_text_repel(
      data = arrow_df,
      aes(x = log_spend_late, y = y_adj_late,
          label = state_abbrev, color = coverage_bin),
      fontface = "bold", size = 2.8,
      box.padding = 0.15, point.padding = 0.05,
      segment.color = "grey75", segment.size = 0.2,
      min.segment.length = 0.2, max.overlaps = Inf,
      show.legend = FALSE
    ) +
    # Single-period dots — thicker stroke so the rings read clearly
    geom_point(
      data = dot_df,
      aes(x = log_spend_pt, y = y_adj_pt, color = coverage_bin),
      shape = 21, fill = "white", stroke = 1.4, size = 3.0, alpha = 1
    ) +
    ggrepel::geom_text_repel(
      data = dot_df,
      aes(x = log_spend_pt, y = y_adj_pt,
          label = state_abbrev, color = coverage_bin),
      fontface = "italic", size = 2.8,
      box.padding = 0.15, point.padding = 0.05,
      segment.color = "grey75", segment.size = 0.2,
      min.segment.length = 0.2, max.overlaps = Inf,
      show.legend = FALSE
    ) +
    # US arrow (highlighted — warm amber to pop against the blue palette)
    geom_segment(
      data = us_df,
      aes(x    = log_spend_early, y    = y_adj_early,
          xend = log_spend_late,  yend = y_adj_late),
      arrow     = arrow(length = unit(0.32, "cm"), type = "closed"),
      color     = US_COLOR, linewidth = 1.7
    ) +
    ggrepel::geom_text_repel(
      data = us_df,
      aes(x = log_spend_late, y = y_adj_late, label = state_abbrev),
      fontface = "bold", size = 4.6, color = US_COLOR,
      box.padding = 0.45, show.legend = FALSE
    ) +
    scale_color_manual(
      values = coverage_pal,
      name   = "V data coverage 2010-2019:",
      drop   = TRUE   # hide unused levels (e.g. "None") from the legend
    ) +
    scale_x_continuous(
      breaks = log(dollar_breaks),
      labels = scales::dollar(dollar_breaks),
      name   = "HIV spending per prevalent case (2019 USD, log scale)"
    ) +
    scale_y_continuous(
      name = "Covariate-adjusted population viral suppression, logit(K × V)"
    ) +
    # Clip y-axis to tighten the dense middle band. AR's full early-period
    # tail extends below -1.4 — noted in the caption.
    coord_cartesian(ylim = c(-1.4, 0.05)) +
    labs(
      title    = "State-level population viral suppression frontier, HIV, 2010-2019",
      subtitle = stringr::str_wrap(
        paste0(
          "Black curve: efficiency frontier from SFMA (", spec_label, "). ",
          "Arrows: each state's movement from 2010-2014 to 2015-2019 averages. ",
          "Hollow circles: V data in only one period. ",
          "Color: per-state V-data completeness across 2010-2019."
        ),
        width = 130
      ),
      caption  = stringr::str_wrap(
        paste0(
          missing_caption, dot_caption,
          "Arkansas's 2010-2014 mean falls below the displayed y-range. ",
          "Frontier model: ", spec_id, "; ",
          "n = ", meta$n_observations, " state-years across ", meta$n_states, " states. ",
          "Dashed line at logit(K × V) = 0 marks K × V = 0.50 on the probability scale."
        ),
        width = 150
      )
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.background     = element_rect(fill = "white", color = NA),
      panel.background    = element_rect(fill = "white", color = NA),
      panel.grid.major    = element_line(color = "grey92"),
      panel.grid.minor    = element_blank(),
      plot.title          = element_text(face = "bold", size = 13, margin = margin(b = 4)),
      plot.subtitle       = element_text(color = "grey25", size = 10,
                                         lineheight = 1.15, margin = margin(b = 10)),
      plot.title.position = "plot",
      plot.margin         = margin(t = 12, r = 14, b = 10, l = 14),
      axis.title.x        = element_text(margin = margin(t = 8)),
      axis.title.y        = element_text(margin = margin(r = 8)),
      legend.position     = "bottom",
      legend.direction    = "horizontal",
      legend.title        = element_text(face = "bold", size = 10),
      legend.text         = element_text(size = 9),
      legend.key.width    = unit(1.4, "cm"),
      legend.margin       = margin(t = 6),
      plot.caption        = element_text(hjust = 0, color = "grey30",
                                         size = 8, lineheight = 1.15)
    ) +
    guides(color = guide_legend(
      title.position = "left",
      title.vjust    = 0.5,
      nrow           = 1,
      override.aes   = list(linewidth = 1.3, size = 3)
    ))
  
  ## 2.8 Save -----------------------------------------------------------
  out_png <- file.path(dir_output, paste0(outfile_stub, ".png"))
  out_pdf <- file.path(dir_output, paste0(outfile_stub, ".pdf"))
  ggsave(out_png, p, width = 13, height = 8.5, dpi = 600, bg = "white")
  ggsave(out_pdf, p, width = 13, height = 8.5)
  cat("  wrote: ", out_png, "\n        ", out_pdf, "\n", sep = "")
  
  invisible(p)
}

## 3. Build both figures --------------------------------------------------
# Primary spec — matches locked regression (lagged prevalence control)
build_frontier_arrows_figure(
  spec_id      = "hiv__kxv_per_case__04_lagPrev",
  outfile_stub = "F_state_frontier_arrows_KxV_lagPrev",
  spec_label   = "adjusted for % Black, % Hispanic, % homeless, and lagged prevalence (t-1)"
)

# Robustness spec — no prevalence control (more data, simpler DAG)
build_frontier_arrows_figure(
  spec_id      = "hiv__kxv_per_case__03_homeless",
  outfile_stub = "F_state_frontier_arrows_KxV_noPrev",
  spec_label   = "adjusted for % Black, % Hispanic, and % homeless"
)

cat("\nDone. Figures written to:\n  ", dir_output, "\n", sep = "")