##---------------------------------------------------------------
##  plot_frontiers_mvp.R
##
##  Reads frontier batch-fit output and produces TWO PNGs grouped
##  by predictor scale:
##
##    1) frontier_per_case.png    — 4 panels (2×2)
##         Rows: K × V, K only
##         Cols: 03_homeless (no prev) | 04_lagPrev (with prev)
##
##    2) frontier_per_capita.png  — 6 panels (3×2)
##         Rows: K × V, K only, Incidence
##         Cols: 03_homeless (no prev) | 04_lag/contempPrev (with prev)
##
##  Per-row y-axis range so each outcome reads on its own scale.
##  Shared x-axis within each image (same predictor across panels).
##
##  Annotation simplified: just N state-years and jurisdictions.
##  η / σ / dollar plateaus removed — those live in the metadata CSV.
##---------------------------------------------------------------

rm(list = ls())

## ---- paths -----------------------------------------------------
if (Sys.info()["sysname"] == "Linux") {
  h <- paste0("/ihme/homes/", Sys.info()[7], "/")
} else if (Sys.info()["sysname"] == "Darwin") {
  h <- paste0("/Volumes/", Sys.info()[7], "/")
} else {
  h <- "H:/"
}

batch_root <- file.path(h, "aim_outputs/Aim2/C_frontier_analysis")
candidates <- list.files(batch_root, pattern = "^\\d{8}$",
                         full.names = TRUE, include.dirs = TRUE)
candidates <- candidates[file.exists(file.path(candidates, "frontier_batch_mvp", "frontiers_long.csv"))]
if (length(candidates) == 0) {
  stop("No frontier_batch_mvp output found. Run C_frontier_batch_fit_mvp.py first.")
}
dir_input  <- file.path(sort(candidates, decreasing = TRUE)[1], "frontier_batch_mvp")
dir_output <- file.path(dir_input, "plots")
if (!dir.exists(dir_output)) dir.create(dir_output, recursive = TRUE)
cat("Reading from: ", dir_input,  "\n", sep = "")
cat("Writing to:   ", dir_output, "\n\n", sep = "")

## ---- libraries -------------------------------------------------
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, patchwork, scales, conflicted)
suppressMessages(conflicted::conflicts_prefer(dplyr::filter, .quiet = TRUE))

## ---- load ------------------------------------------------------
long <- read.csv(file.path(dir_input, "frontiers_long.csv"),     stringsAsFactors = FALSE)
meta <- read.csv(file.path(dir_input, "frontiers_metadata.csv"), stringsAsFactors = FALSE)
cat(sprintf("Loaded %d state-year rows across %d specs.\n\n",
            nrow(long), length(unique(long$spec_id))))

## ---- friendly labels -------------------------------------------
# Outcome label used in panel titles (left of the em-dash)
outcome_label_by_family <- c(
  kxv_per_case         = "K × V (cascade)",
  kxv_per_capita       = "K × V (cascade)",
  k_only_per_case      = "K only (knowledge of status)",
  k_only_per_capita    = "K only (knowledge of status)",
  incidence_per_capita = "HIV incidence"
)

# Spec label used in panel titles (right of the em-dash)
spec_label_by_name <- c(
  "01_bivariate"   = "Bivariate (spending only)",
  "03_homeless"    = "Without prevalence",
  "04_lagPrev"     = "With prevalence (lagged t-1)",
  "04_contempPrev" = "With prevalence (contemporaneous)"
)

# Y-axis labels per outcome
y_axis_label <- function(outcome_label) {
  # Strip trailing "/cap" or other detail and append "covariate-adjusted"
  paste0(outcome_label, ", covariate-adjusted")
}

## ---- one-panel builder ----------------------------------------
build_panel <- function(d, m, y_range, x_range, family_name) {
  outcome_kind <- d$outcome_kind[1]
  is_production <- outcome_kind == "production"
  
  outcome_lab <- outcome_label_by_family[family_name]
  if (is.na(outcome_lab)) outcome_lab <- d$outcome_label[1]
  
  spec_lab <- spec_label_by_name[as.character(d$model_name[1])]
  if (is.na(spec_lab)) spec_lab <- d$model_name[1]
  
  panel_title <- paste0(outcome_lab, " — ", spec_lab)
  
  # Simplified annotation: just sample size
  annotation_text <- sprintf("n = %d state-years\n%d jurisdictions",
                             m$n_observations, m$n_states)
  
  # Sort frontier line by x
  d_sorted <- d[order(d$log_spend_orig), ]
  
  ggplot(d, aes(x = log_spend_orig, y = y_obs_adj)) +
    geom_point(alpha = 0.50, size = 1.5, color = "#4682B4") +
    geom_line(data = d_sorted, aes(y = y_frontier),
              color = "#E67E22", linewidth = 1.3) +
    annotate("text",
             x = -Inf, y = ifelse(is_production, -Inf, Inf),
             label = annotation_text,
             hjust = -0.05,
             vjust = ifelse(is_production, -0.15, 1.15),
             size = 3, color = "grey25", fontface = "italic") +
    coord_cartesian(xlim = x_range, ylim = y_range) +
    labs(
      title    = panel_title,
      subtitle = paste0("Equation: ", m$equation),
      x        = d$predictor_label[1],
      y        = y_axis_label(d$outcome_label[1])
    ) +
    theme_minimal(base_size = 10) +
    theme(
      plot.title       = element_text(face = "bold", size = 12),
      plot.subtitle    = element_text(size = 8.5, color = "grey30",
                                      margin = margin(b = 6)),
      axis.title       = element_text(size = 10),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(linewidth = 0.25, color = "grey90"),
      plot.margin      = margin(8, 12, 8, 12)
    )
}

## ---- build_image ----------------------------------------------
# Takes a vector of families (in row order) and the predictor type.
# Returns the assembled image with shared y-axis per row.
build_image <- function(families_in_order, predictor_short, predictor_long,
                        image_subtitle) {
  
  panels <- list()
  
  # Determine global x-range across all families
  fam_long <- long[long$family %in% families_in_order, ]
  x_pad <- 0.05 * diff(range(fam_long$log_spend_orig, na.rm = TRUE))
  x_range <- range(fam_long$log_spend_orig, na.rm = TRUE) + c(-x_pad, x_pad)
  
  for (fam in families_in_order) {
    
    # Y-range shared within this row (this family)
    row_long <- long[long$family == fam, ]
    y_pad <- 0.05 * diff(range(row_long$y_obs_adj, na.rm = TRUE))
    y_range <- range(row_long$y_obs_adj, na.rm = TRUE) + c(-y_pad, y_pad)
    
    # Spec order: bivariate first, then no-prev, then with-prev
    spec_order <- c("01_bivariate", "03_homeless", "04_lagPrev", "04_contempPrev")
    fam_specs <- intersect(spec_order, unique(row_long$model_name))
    
    for (spec in fam_specs) {
      d <- row_long[row_long$model_name == spec, ]
      m <- meta[meta$family == fam & meta$model_name == spec, ][1, ]
      panels[[length(panels) + 1]] <- build_panel(d, m, y_range, x_range, fam)
    }
  }
  
  combined <- wrap_plots(panels, ncol = 3) +
    plot_annotation(
      title    = sprintf("HIV efficiency frontiers — predictor: %s", predictor_long),
      subtitle = image_subtitle,
      caption  = "SFMA noise SE: σ = 0.15 for cascade outcomes (K × V, K only); σ = 0.07 for incidence (lower residual variance).",
      theme = theme(
        plot.title    = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 10, color = "grey25",
                                     margin = margin(b = 10)),
        plot.caption  = element_text(size = 9, color = "grey35",
                                     hjust = 0, margin = margin(t = 12))
      )
    )
  combined
}

## ---- IMAGE 1 — per case (2×3: K × V, K only) -------------------
cat("Building per-case image...\n")
img_per_case <- build_image(
  families_in_order = c("kxv_per_case", "k_only_per_case"),
  predictor_short = "Spend per case",
  predictor_long  = "log(HIV spending per prevalent case)",
  image_subtitle  = "Rows: K × V (cascade composite) | K only (knowledge of HIV status).  Columns: bivariate (spending only) | without prevalence | with lagged prevalence.  Year fixed effects included in every spec; cluster-robust SEs by state."
)
out_per_case <- file.path(dir_output, "frontier_per_case.png")
ggsave(out_per_case, img_per_case,
       width = 21, height = 11, dpi = 150, bg = "white")
cat(sprintf("  Saved: %s\n", out_per_case))

## ---- IMAGE 2 — per capita (3×3: K × V, K only, incidence) -----
cat("Building per-capita image...\n")
img_per_capita <- build_image(
  families_in_order = c("kxv_per_capita", "k_only_per_capita", "incidence_per_capita"),
  predictor_short = "Spend per capita",
  predictor_long  = "log(HIV spending per capita)",
  image_subtitle  = "Rows: K × V (cascade) | K only | HIV incidence.  Columns: bivariate (spending only) | without prevalence | with prevalence (lagged for K outcomes, contemporaneous for incidence).  Year FE in every spec; cluster-robust SEs by state."
)
out_per_capita <- file.path(dir_output, "frontier_per_capita.png")
ggsave(out_per_capita, img_per_capita,
       width = 21, height = 16, dpi = 150, bg = "white")
cat(sprintf("  Saved: %s\n", out_per_capita))

cat("\nDone. Two PNGs are in:\n  ", dir_output, "\n", sep = "")