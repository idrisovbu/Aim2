##----------------------------------------------------------------
##' Title: D_figures.R
##'
##' Purpose: Creates figures for Aim2
##----------------------------------------------------------------

##----------------------------------------------------------------
## 0. Clear environment and set library paths
##----------------------------------------------------------------
rm(list = ls())

pacman::p_load(dplyr, openxlsx, RMySQL, data.table, ini, DBI, tidyr, openxlsx, readxl, reticulate, ggpubr, arrow, grid, gridExtra, scales, ggplot2, shiny)
library(patchwork)
library(lbd.loader, lib.loc = sprintf("/share/geospatial/code/geospatial-libraries/lbd.loader-%s", R.version$major))
if("dex.dbr"%in% (.packages())) detach("package:dex.dbr", unload=TRUE)
library(dex.dbr, lib.loc = lbd.loader::pkg_loc("dex.dbr"))
suppressMessages(devtools::load_all(path = "/ihme/homes/idrisov/repo/dex_us_county/"))

# Needed to use the below code to install "ggpol" for using the facet_share function which somehow we used before and now R can't find the function anymore? Makes no sense
# install.packages(
#   "/ihme/homes/idrisov/ggpol_0.0.7.tar.gz",
#   repos = NULL,
#   type = "source",
#   lib = file.path(h, "R_packages")
# )

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

#library(plotly)
.libPaths(c(file.path(h, "R_packages"), .libPaths()))
library(ggpol)
library(dplyr)

library(conflicted)

conflict_prefer("summarise", "dplyr")
conflict_prefer("summarize", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

##----------------------------------------------------------------
## 0.1 Functions
##----------------------------------------------------------------
ensure_dir_exists <- function(dir_path) {
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
}

save_plot <- function(ggplot_obj, ggplot_name, output_path, width = 14, height = 10, dpi = 500, path = ".") {
  
  # Build full file path
  file_path <- file.path(output_path, paste0(ggplot_name, ".png"))
  
  # Save the plot
  ggsave(
    filename = file_path,
    plot = ggplot_obj + 
      theme(),
    width = width,
    height = height,
    dpi = dpi,
    units = "in"
  )
  
  message("Plot saved as: ", normalizePath(file_path))
}

axis_dollar_mb <- function(x) {
  abs_x <- abs(x)
  ifelse(
    abs_x >= 1e9,
    paste0("$", format(round(abs_x / 1e9, 1), nsmall = 1), "B"),
    paste0("$", format(round(abs_x / 1e6, 0), big.mark = ","), "M")
  )
}

build_county_map <- function(map_sf_data,
                             state_sf,
                             cols,
                             title_text,
                             legend_nrow = 1) {
  
  # Extract valid (non-NA) factor levels for the legend breaks
  valid_levels <- levels(map_sf_data$plot_val)
  # If plot_val was not already a factor, coerce and get levels
  if (is.null(valid_levels)) {
    valid_levels <- levels(factor(map_sf_data$plot_val))
  }
  # Drop any literal "NA" string level (shouldn't happen with cut(), but safety)
  valid_levels <- valid_levels[!is.na(valid_levels) & valid_levels != "NA"]
  
  # Match colors to the number of valid levels
  # (cols may have more entries than valid_levels if some bins are empty)
  if (length(cols) > length(valid_levels)) {
    cols_use <- cols[seq_along(valid_levels)]
  } else {
    cols_use <- cols
  }
  names(cols_use) <- valid_levels
  
  ggplot(data = map_sf_data) +
    geom_sf(aes(fill = plot_val, geometry = geometry), color = NA) +
    geom_sf(data = state_sf, fill = NA, linewidth = 0.4) +
    labs(title = title_text, fill = "") +
    scale_fill_manual(
      values   = cols_use,
      breaks   = valid_levels,
      na.value = "#838484"
    ) +
    theme_map() +
    guides(fill = guide_legend(nrow = legend_nrow))
}

theme_map <- function(title_size = 14, legend_text_size = 11) {
  theme(
    legend.position      = "bottom",
    legend.justification = "center",
    legend.direction     = "horizontal",
    legend.box           = "horizontal",
    legend.text          = element_text(size = legend_text_size),
    legend.title         = element_blank(),
    legend.margin        = margin(t = 2, b = 4, unit = "pt"),
    legend.spacing.x     = unit(4, "pt"),
    legend.key.size      = unit(14, "pt"),
    title                = element_text(size = title_size),
    text                 = element_text(size = 12),
    plot.margin          = margin(0.5, 0.5, 0.8, 0.5, "cm"),
    axis.ticks           = element_blank(),
    axis.text            = element_blank(),
    panel.background     = element_blank()
  )
}

##----------------------------------------------------------------
## 0.2 Set directories for DEX estimate data / county estimates
##----------------------------------------------------------------
# Set path for data
date_dex <- "20260120"
fp_dex <- file.path(h, "/aim_outputs/Aim2/B_aggregation/", date_dex, "/compiled_dex_data_2010_2019.parquet")

date_ushd <- "20251204"
fp_ushd <- file.path(h, "/aim_outputs/Aim2/B_aggregation/", date_ushd, "/compiled_ushd_data_2010_2019.parquet")

date_gbd <- "20260313"
fp_gbd <- file.path(h, "/aim_outputs/Aim2/A_data_preparation/", date_gbd, "/GBD/df_gbd.parquet")

# Ryan White Data
fp_rw_t1 <- file.path(h, "/aim_outputs/Aim2/R_resources/ryan_white_data/rw_title1.xls") # Marcus
fp_rw_t2 <- file.path(h, "/aim_outputs/Aim2/R_resources/ryan_white_data/rw_title2.xls") # Marcus
fp_rw_2016_2019 <- file.path(h, "aim_outputs/Aim2/R_resources/ryan_white_data/ryan_white_data_2016-2019.csv") # Official RW site
fp_rw_agesex_weights <- file.path(h, "aim_outputs/Aim2/R_resources/ryan_white_data/ryan_white_agesex_weights.xlsx")

# FIPS table
fp_fips <- file.path(h, "/aim_outputs/Aim2/R_resources/state_county_city_fips.csv")
fp_cityfips <- file.path(h, "/aim_outputs/Aim2/R_resources/ryan_white_data/t1years.xlsx")

# Set output directories
date_today <- format(Sys.time(), "%Y%m%d")
dir_output <- file.path(h, "/aim_outputs/Aim2/D_tables_figures/", date_today)
ensure_dir_exists(dir_output)

##----------------------------------------------------------------
## 0.3 Set colors and labels for plots
##----------------------------------------------------------------

# colors and labels to use 
# Use full payer name in plot titles and labels
payer_list <- list("mdcr" = "Medicare", 
                   "mdcd" = "Medicaid", 
                   "priv" = "Private Insurance", 
                   "oop" = "Out-of-Pocket",
                   "ryan_white" = "Ryan White")

# payer colors
payer_colors <- list("priv" =	"#FFCB8D", 
                     "mdcr" =	"#3188BD", 
                     "mdcd" =	"#ACDABA", 
                     "oop" =	"#D58192",
                     "ryan_white" =	"#6A51A3")

payer_colors_maps <- list("priv" =	c("#f1f1f1", "#f5dbbc", "#f4c788", "#efb353", "#E69F00"),
                          "mdcr" =	c("#f1f1f1","#c2c9e0", "#93a4d0","#6080bf","#0E5EAE"),
                          "mdcd" =	c("#f1f1f1","#c1dcd0","#91c8b0","#5db391","#009E73"),
                          "oop" =	c("#f1f1f1","#e8c3b7","#da967f","#c6694b","#ae3918"))

#colors for type of care
toc_colors <- c(
  "ED" = "#B6AB98",
  "AM" = "#ACDABA",
  "HH" = "#D68093",
  "IP" = "#2D5963",
  "NF" = "#BCD1DA",
  "DV" = "#FDCD8B",
  "RX" = "#8BD9F5")

# long names for types of care
toc_labels = c(
  "ED" = "Emergency Department",
  "AM" = "Ambulatory",
  "IP" = "Inpatient",
  "HH" = "Home Health",
  "NF" = "Nursing Facility",
  "DV" = "Dental",
  "RX" = "Pharmaceutical"
)

age_factor <- c("0-<1", "1-<5", "5-<10", "10-<15", "15-<20", "20-<25", "25-<30", "30-<35", 
                "35-<40", "40-<45", "45-<50",  "50-<55", "55-<60", "60-<65", 
                "65-<70", "70-<75", "75-<80", "80-<85", "85+")

age_factor_rw <- c("25 - <35", "35 - <45", "45 - <55", "55 - <65", "65+")

sex_factor <- c("Male", "Female")

toc_factor <- c("AM", "ED", "HH", "IP", "NF", "RX")

##----------------------------------------------------------------
## 0.4 Set ggplot theme
##----------------------------------------------------------------

theme_settings <- theme(
  #axis.title.y = element_blank(),
  text = element_text(size = 12),
  axis.text.x = element_text(size = 12), 
  axis.text.y = element_text(size = 14), # age labels
  plot.subtitle = element_text(size = 18), # nothing?
  strip.text = element_text(size = 14), # "Male" "Female" 
  legend.title = element_text(size = 14), 
  legend.text = element_text(size = 12),
  panel.grid.major.x = element_line(color = "grey70", size = 0.5), # thicker lines
  legend.position = "top",               # move legend above plot
  legend.box = "horizontal",             # arrange items horizontally
  legend.background = element_rect(      # put it in a box
    color = "black", fill = "white", 
    linewidth = 0.5, linetype = "solid"),
  legend.key = element_rect(fill = "white"),
  plot.margin = margin(t = 5, r = 5, b = 5, l = -50)
)


###
##----------------------------------------------------------------
## 0.1b  NEW HELPER — build a proper asymmetric age pyramid
##
##  facet_share(scales = "free") often still forces visual symmetry
##  because the underlying panel widths are equal. This helper
##  builds male (left, reversed axis) and female (right) as two
##  separate ggplots and stitches them with patchwork, giving each
##  panel its own true-scale x-axis.
##----------------------------------------------------------------

build_pyramid <- function(df,
                          fill_var,        # unquoted column for fill
                          fill_values,     # named vector of colours
                          fill_labels,     # named vector of labels
                          fill_legend_title = "Payer",
                          y_label = "Inflation Adjusted Spending (2019 USD)",
                          title_text = "",
                          y_label_fn = axis_dollar_mb,
                          bar_width = 1,
                          min_panel_share = 0.25) {
  # min_panel_share: the smaller panel will never be less than this
  # fraction of the data area (excluding the center label strip).
  # 0.25 means the smaller side gets at least 25% of the plot area.
  # Set to 0.5 to force equal widths (like facet_share).
  
  fill_var_str <- deparse(substitute(fill_var))
  
  # --- Compute STACKED totals per age group (for axis limits + widths) ---
  stacked_male <- df %>%
    dplyr::filter(sex_name == "Male") %>%
    dplyr::group_by(age_name) %>%
    dplyr::summarise(total = sum(spend_mean_inverse, na.rm = TRUE), .groups = "drop")
  stacked_female <- df %>%
    dplyr::filter(sex_name == "Female") %>%
    dplyr::group_by(age_name) %>%
    dplyr::summarise(total = sum(spend_mean_inverse, na.rm = TRUE), .groups = "drop")
  
  male_min <- min(stacked_male$total, na.rm = TRUE)       # most negative
  female_max_val <- max(stacked_female$total, na.rm = TRUE) # most positive
  
  # --- Compute data-proportional panel widths with floor ---
  male_max   <- abs(male_min)
  female_max <- abs(female_max_val)
  total      <- male_max + female_max
  
  # Raw proportions
  w_male   <- male_max / total
  w_female <- female_max / total
  
  # Clamp: ensure the smaller panel gets at least min_panel_share
  if (w_male < min_panel_share) {
    w_male   <- min_panel_share
    w_female <- 1 - min_panel_share
  } else if (w_female < min_panel_share) {
    w_female <- min_panel_share
    w_male   <- 1 - min_panel_share
  }
  
  message(sprintf("Pyramid widths — Male: %.0f%%  Female: %.0f%%  (raw ratio %.1f:1)",
                  w_male * 100, w_female * 100, male_max / female_max))
  
  # --- shared theme pieces (matches original facet_share style) ---
  base_theme <- theme_classic() +
    theme(
      text               = element_text(size = 12),
      axis.text.x        = element_text(size = 12),
      axis.text.y        = element_text(size = 14),
      strip.text         = element_text(size = 14),
      legend.title       = element_text(size = 14),
      legend.text        = element_text(size = 12),
      panel.grid.major.x = element_line(color = "grey70", linewidth = 0.5),
      legend.position    = "top",
      legend.box         = "horizontal",
      legend.background  = element_rect(color = "black", fill = "white",
                                        linewidth = 0.5, linetype = "solid"),
      legend.key         = element_rect(fill = "white")
    )
  
  # --- Male panel (left side, bars grow leftward) ---
  # Age labels placed on the RIGHT y-axis of the male panel so they
  # sit at the seam between male and female with no extra panel gap.
  p_male <- df %>%
    dplyr::filter(sex_name == "Male") %>%
    ggplot(aes(x = age_name, y = spend_mean_inverse, fill = .data[[fill_var_str]])) +
    geom_col(color = "black", width = bar_width, linewidth = 0.3) +
    coord_flip() +
    scale_fill_manual(values = fill_values, labels = fill_labels, name = fill_legend_title) +
    scale_y_continuous(labels = function(x) y_label_fn(abs(x)),
                       limits = c(male_min * 1.02, 0),
                       expand = expansion(mult = c(0.02, 0))) +
    scale_x_discrete(drop = FALSE) +
    labs(y = y_label, x = NULL) +
    ggtitle("Male") +
    base_theme +
    theme(
      axis.text.y.left  = element_blank(),
      axis.ticks.y.left = element_blank(),
      axis.text.y.right = element_text(size = 14, hjust = 0.5),
      axis.ticks.y.right = element_blank(),
      axis.title.y      = element_blank(),
      plot.title         = element_text(hjust = 0.5, size = 14),
      plot.margin        = margin(t = 5, r = 0, b = 5, l = 5)
    ) +
    guides(y.sec = guide_axis())  # activate right-side y-axis labels
  
  # --- Female panel (right side, bars grow rightward) ---
  p_female <- df %>%
    dplyr::filter(sex_name == "Female") %>%
    ggplot(aes(x = age_name, y = spend_mean_inverse, fill = .data[[fill_var_str]])) +
    geom_col(color = "black", width = bar_width, linewidth = 0.3) +
    coord_flip() +
    scale_fill_manual(values = fill_values, labels = fill_labels, name = fill_legend_title) +
    scale_y_continuous(labels = y_label_fn,
                       limits = c(0, female_max_val * 1.02),
                       expand = expansion(mult = c(0, 0.02))) +
    scale_x_discrete(drop = FALSE) +
    labs(y = y_label, x = NULL) +
    ggtitle("Female") +
    base_theme +
    theme(
      axis.text.y   = element_blank(),
      axis.ticks.y  = element_blank(),
      axis.title.y  = element_blank(),
      plot.title     = element_text(hjust = 0.5, size = 14),
      plot.margin    = margin(t = 5, r = 5, b = 5, l = 0)
    )
  
  # --- Combine: male | female (no center strip needed) ---
  combined <- p_male + p_female +
    patchwork::plot_layout(
      widths = c(w_male, w_female),
      guides = "collect"
    ) +
    patchwork::plot_annotation(
      title = title_text,
      theme = theme(
        plot.title = element_text(size = 16),
        legend.position = "top"
      )
    )
  
  return(combined)
}



###
##----------------------------------------------------------------
## 0.5 Read in data & loc_ids file & map files
##----------------------------------------------------------------
# Main DEX Data
df_dex <- open_dataset(fp_dex)

# USHD Data - UNUSED ATM
#df_ushd <- read_parquet(fp_ushd)

# GBD Data
df_gbd <- read_parquet(fp_gbd)

# Ryan White Data
df_rw_t1 <- read_excel(fp_rw_t1)
df_rw_t2 <- read_excel(fp_rw_t2)
df_rw_cityfips <- read_excel(fp_cityfips)
df_rw_2016_2019 <- read.csv(fp_rw_2016_2019)
df_rw_agesex_weights <- read_excel(fp_rw_agesex_weights)

df_fips <- read.csv(fp_fips)

# DEX causes that aggregate to "_subs" cause
subs_causes <- c("mental_alcohol", "mental_drug_agg", "mental_drug_opioids")

# File containing main mapping of loc_ids, fips, mcnty, etc.
df_loc_ids <- fread("/ihme/homes/idrisov/aim_outputs/Aim2/R_resources/county_fips_locs.csv")

# Shape files used for large US map plotting by county
mcnty_shapefile <- readRDS("/ihme/dex/us_county/maps/mcnty_sf_shapefile.rds")
state_shapefile <- readRDS("/ihme/dex/us_county/maps/state_sf_shapefile.rds")

##----------------------------------------------------------------
## 0.6 Format RW data
##----------------------------------------------------------------
# Title1 City level Ryan White Data #
# Add padded 0 to cityfip column
df_rw_t1$cityfip <- sprintf("%04d", df_rw_t1$cityfip)

# Cityfip codes
# Remove padded spaces from cityfip
df_rw_cityfips$cityfip <- str_trim(df_rw_cityfips$cityfip)

# Merge w/ Ryan White title1 (city)
df_rw_t1_m <- left_join(
  x = df_rw_t1,
  y = df_rw_cityfips,
  by = "cityfip"
)

# Extract state name
df_rw_t1_m$state_abbr <- substr(df_rw_t1_m$city, nchar(df_rw_t1_m$city) - 1, nchar(df_rw_t1_m$city))

# Create full state names
state_lookup <- setNames(state.name, state.abb)
df_rw_t1_m$state_name <- state_lookup[df_rw_t1_m$state_abbr]
df_rw_t1_m$state_name <- if_else(df_rw_t1_m$state_abbr == "DC", "District of Columbia", df_rw_t1_m$state_name)

# Group by summary for whole state
df_rw_t1_m <- df_rw_t1_m %>%
  group_by(year, state_name) %>%
  summarise(
    rw_title1_funding = sum(title1_funding, na.rm = TRUE)
  )

# Filter on 2010 ~ 2019
df_rw_t1_m <- df_rw_t1_m %>%
  filter(year %in% (2010:2019))

df_rw_t1_m <- df_rw_t1_m %>%
  setnames(
    old = c("year", "state_name"),
    new = c("year_id", "location_name")
  )


# Title2 State level Ryan White Data #
# Format FIPS data
df_fips$State.Name <- str_to_title(tolower(df_fips$State.Name))
df_fips_state <- df_fips %>% select(c("State.Name", "State.Code", "State.FIPS.Code")) %>% unique()

df_rw_t2_m <- left_join(
  x = df_rw_t2,
  y = df_fips_state,
  by = c("statefip" = "State.FIPS.Code")
)

df_rw_t2_m <- df_rw_t2_m %>%
  select(c(year, State.Name, title2_funding_annual)) %>%
  filter(year %in% c(2010:2019))

df_rw_t2_m <- df_rw_t2_m %>%
  setnames(
    old = c("year", "State.Name", "title2_funding_annual"),
    new = c("year_id", "location_name", "rw_title2_funding")
  )


# Merge Title1 and Title2 data #
df_rw_t1_m <- df_rw_t1_m %>%
  ungroup() %>%
  mutate(
    year_id = as.integer(year_id),
    location_name = unname(as.character(location_name)),
    location_name = trimws(location_name)
  )

df_rw_t2_m <- df_rw_t2_m %>%
  mutate(
    year_id = as.integer(year_id),
    location_name = as.character(location_name),
    location_name = trimws(location_name)
  )

# Fix naming issue
df_rw_t2_m$location_name <- if_else(df_rw_t2_m$location_name == "District Of Columbia", "District of Columbia", df_rw_t2_m$location_name)

# Join
df_rw_m <- full_join(
  x = df_rw_t1_m,
  y = df_rw_t2_m,
  by = c("year_id", "location_name")
)

# Combine title1 and title2 grant sums
df_rw_m <- df_rw_m %>%
  mutate(
    rw_funding = rowSums(cbind(rw_title1_funding, rw_title2_funding), na.rm = TRUE)
  )


# 2016 - 2019 Ryan White data
df_rw_2016_2019_f <- df_rw_2016_2019 %>%
  filter(HRSA.Program.Area.Name == "HIV/AIDS") %>%
  filter(Grant.Program.Name %in% c("Ryan White Part A HIV Emergency Relief Grant Program (H89)",
                                   "Ryan White Part B HIV Care Grant Program (X07)",
                                   "Ryan White Part B Supplemental (X08)",
                                   "ADAP Shortfall Relief (X09)"
  )) %>%
  group_by(Award.Year, State.Name) %>%
  summarise(
    `Financial.Assistance` = sum(Financial.Assistance)
  )

df_rw_2016_2019_f <- df_rw_2016_2019_f %>%
  filter(Award.Year %in% c(2016:2019))

df_rw_2016_2019_f <- df_rw_2016_2019_f %>%
  setnames(
    old = c("Award.Year", "State.Name", "Financial.Assistance"),
    new = c("year_id", "location_name", "ryan_white_grant")
  )

# Join to title1 and title2 summed data
df_rw_total <- full_join(
  x = df_rw_m,
  y = df_rw_2016_2019_f,
  by = c("year_id", "location_name")
)

df_rw_total$delta <- (df_rw_total$rw_funding - df_rw_total$ryan_white_grant)

# View(df_rw_total %>% filter(year_id %in% c(2016:2019))) # Checking deltas, it seems some are matching perfectly against Marcus's data, whereas we are off in some rows but unknow why we are off

# Using <=2018 data from Marcus's dataset, 2019 data will come from the official Ryan White grant data
df_rw_total$ryan_white_funding_final <- if_else(df_rw_total$year_id <= 2018, df_rw_total$rw_funding, df_rw_total$ryan_white_grant)

# Inflation adjust RW data before creating (RW Spend + DEX spend_all) / prevalence → RWspend ratio
df_rw_total <- deflate(
  data = df_rw_total,
  val_columns = "ryan_white_funding_final",
  old_year = "year_id",
  new_year = 2019
)

# Apply age sex weights to RW data
df_rw_agesex <- df_rw_total %>%
  select(c("year_id", "location_name", "ryan_white_funding_final"))

df_rw_agesex <- df_rw_agesex %>%
  tidyr::crossing(df_rw_agesex_weights %>% distinct(age_name))

df_rw_agesex <- df_rw_agesex %>%
  left_join(df_rw_agesex_weights, by = c("age_name")) %>%
  mutate(
    spend_male = ryan_white_funding_final * age_weight_m,
    spend_female = ryan_white_funding_final * age_weight_f
  )

df_rw_agesex <- df_rw_agesex %>%
  select(c("year_id", "location_name", "ryan_white_funding_final", "age_name", "spend_male", "spend_female"))

# Pivot longer
df_rw_long <- df_rw_agesex %>%
  pivot_longer(
    cols = c(spend_male, spend_female),
    names_to = "sex_source",
    values_to = "rw_funding"
  ) %>%
  mutate(
    sex_name = case_when(
      sex_source == "spend_male"   ~ "Male",
      sex_source == "spend_female" ~ "Female"
    )
  ) %>%
  select(-sex_source)

# Rename age groups to match format of DEX data
df_rw_long <- df_rw_long %>%
  mutate(
    age_name = recode(
      age_name,
      "25–34" = "25 - <35",
      "35–44" = "35 - <45",
      "45–54" = "45 - <55",
      "55–64" = "55 - <65",
      "≥65"   = "65+"
    )
  )

# Ryan White broad age bins -> DEX age bins using exact age-overlap split
rw_age_split <- tribble(
  ~rw_age,     ~age_name,   ~split_weight,
  
  "<13",       "0-<1",      1/13,
  "<13",       "1-<5",      4/13,
  "<13",       "5-<10",     5/13,
  "<13",       "10-<15",    3/13,
  
  "13–24",     "10-<15",    2/12,
  "13–24",     "15-<20",    5/12,
  "13–24",     "20-<25",    5/12,
  
  "25 - <35",  "25-<30",    1/2,
  "25 - <35",  "30-<35",    1/2,
  
  "35 - <45",  "35-<40",    1/2,
  "35 - <45",  "40-<45",    1/2,
  
  "45 - <55",  "45-<50",    1/2,
  "45 - <55",  "50-<55",    1/2,
  
  "55 - <65",  "55-<60",    1/2,
  "55 - <65",  "60-<65",    1/2,
  
  "65+",       "65-<70",    0.50,
  "65+",       "70-<75",    0.25,
  "65+",       "75-<80",    0.125,
  "65+",       "80-<85",    0.075,
  "65+",       "85+",       0.05
)

##================================================================
## HELPER: Redistribute RW funding into DEX age bins
##   - For total spending figures: use uniform age-overlap weights
##     (rw_age_split above). This is fine because we are NOT dividing
##     by prevalence, so no denominator distortion.
##   - For per-case figures: use prevalence-proportional weights
##     (built below in Section 1.1).
##================================================================

# Build RW spending in DEX age bins using UNIFORM age-overlap weights
# (used for Figure 1a — total spending by payer)
build_rw_dex_bins_uniform <- function(df_rw_long, rw_age_split, yr = 2019) {
  df_rw_long %>%
    filter(year_id == yr) %>%
    dplyr::rename(rw_age = age_name) %>%
    dplyr::inner_join(rw_age_split, by = "rw_age", relationship = "many-to-many") %>%
    dplyr::mutate(spend_mean = rw_funding * split_weight) %>%
    dplyr::group_by(age_name, sex_name) %>%
    dplyr::summarise(spend_mean = sum(spend_mean, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(payer = "ryan_white")
}

# Map GBD age_group_name -> DEX age_name (reusable helper)
map_gbd_to_dex_age <- function(age_group_name) {
  dplyr::case_when(
    age_group_name %in% c("Under 1", "<1 year",
                          "0 to 364 days", "0 to 11 months") ~ "0-<1",
    age_group_name %in% c("1 to 4")   ~ "1-<5",
    age_group_name %in% c("5 to 9")   ~ "5-<10",
    age_group_name %in% c("10 to 14") ~ "10-<15",
    age_group_name %in% c("15 to 19") ~ "15-<20",
    age_group_name %in% c("20 to 24") ~ "20-<25",
    age_group_name %in% c("25 to 29") ~ "25-<30",
    age_group_name %in% c("30 to 34") ~ "30-<35",
    age_group_name %in% c("35 to 39") ~ "35-<40",
    age_group_name %in% c("40 to 44") ~ "40-<45",
    age_group_name %in% c("45 to 49") ~ "45-<50",
    age_group_name %in% c("50 to 54") ~ "50-<55",
    age_group_name %in% c("55 to 59") ~ "55-<60",
    age_group_name %in% c("60 to 64") ~ "60-<65",
    age_group_name %in% c("65 to 69") ~ "65-<70",
    age_group_name %in% c("70 to 74") ~ "70-<75",
    age_group_name %in% c("75 to 79") ~ "75-<80",
    age_group_name %in% c("80 to 84") ~ "80-<85",
    age_group_name %in% c("85 plus", "85+", "95 plus") ~ "85+",
    TRUE ~ NA_character_
  )
}

##================================================================
## 1. Figure 1a — HIV: Spending by insurance type + Ryan White, 2019
##    (original DEX age bins, national, both sexes)
##================================================================

# ── DEX HIV payer-level spending ─────────────────────────────────
df_f1_hiv <- df_dex %>%
  filter(geo == "national", acause == "hiv",
         year_id == 2019, payer != "all") %>%
  collect() %>%
  dplyr::group_by(payer, age_name, sex_name) %>%
  dplyr::summarise(spend_mean = sum(spend_mean, na.rm = TRUE),
                   .groups = "drop") %>%
  dplyr::mutate(age_name = str_replace_all(age_name, " ", ""))

# ── RW redistributed into DEX bins (uniform weights) ────────────
df_f1_rw_dexbins <- build_rw_dex_bins_uniform(df_rw_long, rw_age_split, yr = 2019)

# ── Combine DEX payers + RW ─────────────────────────────────────
df_f1a_hiv <- bind_rows(df_f1_hiv, df_f1_rw_dexbins) %>%
  dplyr::mutate(
    spend_mean_inverse = ifelse(sex_name == "Male", spend_mean * -1, spend_mean),
    age_name = factor(age_name, levels = age_factor),
    sex_name = factor(sex_name, levels = sex_factor)
  )

# ── Plot ─────────────────────────────────────────────────────────
# Delete if with helper is better
f1a_hiv <- ggplot(
  data = df_f1a_hiv,
  aes(age_name, spend_mean_inverse,
      fill = factor(payer, levels = c("mdcr", "mdcd", "priv", "oop", "ryan_white")))
) +
  facet_share(~ sex_name, scales = "free", reverse_num = FALSE) +
  geom_col(color = "black", width = 1, size = 0.3) +
  coord_flip() +
  scale_fill_manual(values = payer_colors, labels = payer_list, name = "Payer") +
  scale_y_continuous(labels = axis_dollar_mb) +
  theme_classic() +
  labs(
    y = "Inflation Adjusted Spending (2019 USD)", x = "",
    title = "HIV spending for each insurance type and Ryan White by sex and age group in 2019"
  ) +
  theme_settings +
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5, nrow = 1))

save_plot(f1a_hiv, "F1a_HIV_spending_by_insurance_plus_RW", dir_output)

### BElOW POTING WITH HELPER 

# ── Plot using new helper ────────────────────────────────────────
f1a_hiv <- build_pyramid(
  df           = df_f1a_hiv,
  fill_var     = payer,
  fill_values  = unlist(payer_colors),
  fill_labels  = unlist(payer_list),
  fill_legend_title = "Payer",
  y_label      = "Inflation Adjusted Spending (2019 USD)",
  title_text   = "HIV spending for each insurance type and Ryan White by sex and age group in 2019"
)

ggsave(file.path(dir_output, "F1a_HIV_spending_by_insurance_plus_RW_helper.png"),
       plot = f1a_hiv, width = 16, height = 10, dpi = 500)



### End ABOVE IS WITH HELPER


##================================================================
## 1. Figure 1a — SUD: Spending by insurance type, 2019
##    (no Ryan White for SUD)
##================================================================

df_f1_sud <- df_dex %>%
  filter(geo == "national", acause %in% subs_causes,
         year_id == 2019, payer != "all") %>%
  collect() %>%
  dplyr::group_by(payer, age_name, sex_name) %>%
  dplyr::summarise(spend_mean = sum(spend_mean, na.rm = TRUE),
                   .groups = "drop") %>%
  dplyr::mutate(
    age_name = str_replace_all(age_name, " ", ""),
    spend_mean_inverse = ifelse(sex_name == "Male", spend_mean * -1, spend_mean),
    age_name = factor(age_name, levels = age_factor),
    sex_name = factor(sex_name, levels = sex_factor)
  )

###BELOW w/o helper

f1a_sud <- ggplot(
  data = df_f1_sud,
  aes(age_name, spend_mean_inverse,
      fill = factor(payer, levels = c("mdcr", "mdcd", "priv", "oop")))
) +
  facet_share(~ sex_name, scales = "free", reverse_num = FALSE) +
  geom_col(color = "black", width = 1, size = 0.3) +
  coord_flip() +
  scale_fill_manual(values = payer_colors, labels = payer_list, name = "Payer") +
  scale_y_continuous(labels = axis_dollar_mb) +
  theme_classic() +
  labs(
    y = "Inflation Adjusted Spending (2019 USD)", x = "",
    title = "SUD spending for each insurance type by sex and age group in 2019"
  ) +
  theme_settings +
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5, nrow = 1)) +
  theme(plot.margin = margin(t = 10, r = 30, b = 0, l = 0))

#save_plot(f1a_sud, "F1a_SUD_spending_by_insurance", dir_output, width = 16)

##================================================================
## 1. Figure 1a — OUD: Spending by insurance type, 2019
##================================================================

df_f1_oud <- df_dex %>%
  filter(geo == "national", acause == "mental_drug_opioids",
         year_id == 2019, payer != "all") %>%
  collect() %>%
  dplyr::group_by(payer, age_name, sex_name) %>%
  dplyr::summarise(spend_mean = sum(spend_mean, na.rm = TRUE),
                   .groups = "drop") %>%
  dplyr::mutate(
    age_name = str_replace_all(age_name, " ", ""),
    spend_mean_inverse = ifelse(sex_name == "Male", spend_mean * -1, spend_mean),
    age_name = factor(age_name, levels = age_factor),
    sex_name = factor(sex_name, levels = sex_factor)
  )
#below is w/o helper
f1a_oud <- ggplot(
  data = df_f1_oud,
  aes(age_name, spend_mean_inverse,
      fill = factor(payer, levels = c("mdcr", "mdcd", "priv", "oop")))
) +
  facet_share(~ sex_name, scales = "free", reverse_num = FALSE) +
  geom_col(color = "black", width = 1, size = 0.3) +
  coord_flip() +
  scale_fill_manual(values = payer_colors, labels = payer_list, name = "Payer") +
  scale_y_continuous(labels = axis_dollar_mb) +
  theme_classic() +
  labs(
    y = "Inflation Adjusted Spending (2019 USD)", x = "",
    title = "OUD spending for each insurance type by sex and age group in 2019"
  ) +
  theme_settings +
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5, nrow = 1)) +
  theme(plot.margin = margin(t = 10, r = 30, b = 0, l = 0))

save_plot(f1a_oud, "F1a_OUD_spending_by_insurance", dir_output, width = 16)

#below is with helper

f1a_oud <- build_pyramid(
  df           = df_f1_oud,
  fill_var     = payer,
  fill_values  = unlist(payer_colors),
  fill_labels  = unlist(payer_list),
  fill_legend_title = "Payer",
  y_label      = "Inflation Adjusted Spending (2019 USD)",
  title_text   = "OUD spending for each insurance type by sex and age group in 2019"
)

ggsave(file.path(dir_output, "F1a_OUD_spending_by_insurance_helper.png"),
       plot = f1a_oud, width = 16, height = 10, dpi = 500)


##================================================================
## 1. Figure 1a — AUD: Spending by insurance type, 2019
##    (no Ryan White for AUD)
##================================================================

df_f1_aud <- df_dex %>%
  filter(geo == "national", acause == "mental_alcohol",
         year_id == 2019, payer != "all") %>%
  collect() %>%
  dplyr::group_by(payer, age_name, sex_name) %>%
  dplyr::summarise(spend_mean = sum(spend_mean, na.rm = TRUE),
                   .groups = "drop") %>%
  dplyr::mutate(
    age_name = str_replace_all(age_name, " ", ""),
    spend_mean_inverse = ifelse(sex_name == "Male", spend_mean * -1, spend_mean),
    age_name = factor(age_name, levels = age_factor),
    sex_name = factor(sex_name, levels = sex_factor)
  )

f1a_aud <- ggplot(
  data = df_f1_aud,
  aes(age_name, spend_mean_inverse,
      fill = factor(payer, levels = c("mdcr", "mdcd", "priv", "oop")))
) +
  facet_share(~ sex_name, scales = "free", reverse_num = FALSE) +
  geom_col(color = "black", width = 1, size = 0.3) +
  coord_flip() +
  scale_fill_manual(values = payer_colors, labels = payer_list, name = "Payer") +
  scale_y_continuous(labels = axis_dollar_mb) +
  theme_classic() +
  labs(
    y = "Inflation Adjusted Spending (2019 USD)", x = "",
    title = "AUD spending for each insurance type by sex and age group in 2019"
  ) +
  theme_settings +
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5, nrow = 1)) +
  theme(plot.margin = margin(t = 10, r = 30, b = 0, l = 0))

#save_plot(f1a_aud, "F1a_AUD_spending_by_insurance", dir_output, width = 16)

##================================================================
## 1. Figure F1_SUD_supplement_a — SUD: Spending by SUD subcause type, 2019
##    (no Ryan White for SUD)
## NOTE: This figure is NOT saved anywhere, it's just for internal reference
##================================================================

df_f1_sud_sup_a <- df_dex %>%
  filter(geo == "national", acause %in% subs_causes,
         year_id == 2019, payer == "all") %>%
  collect() %>%
  dplyr::group_by(acause, age_name, sex_name) %>%
  dplyr::summarise(spend_mean = sum(spend_mean, na.rm = TRUE),
                   .groups = "drop") %>%
  dplyr::mutate(
    age_name = str_replace_all(age_name, " ", ""),
    spend_mean_inverse = ifelse(sex_name == "Male", spend_mean * -1, spend_mean),
    age_name = factor(age_name, levels = age_factor),
    sex_name = factor(sex_name, levels = sex_factor)
  )

f1a_sud_sup_a <- ggplot(
  data = df_f1_sud_sup_a,
  aes(age_name, spend_mean_inverse,
      fill = factor(acause, levels = c("mental_alcohol", "mental_drug_agg", "mental_drug_opioids")))
) +
  facet_share(~ sex_name, scales = "free", reverse_num = FALSE) +
  geom_col(color = "black", width = 1, size = 0.3) +
  coord_flip() +
  #scale_fill_manual(values = payer_colors, labels = payer_list, name = "Payer") +
  scale_y_continuous(labels = axis_dollar_mb) +
  theme_classic() +
  labs(
    y = "Inflation Adjusted Spending (2019 USD)", x = "",
    title = "SUD spending subtype by sex and age group in 2019"
  ) +
  theme_settings +
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5, nrow = 1)) +
  theme(plot.margin = margin(t = 10, r = 30, b = 0, l = 0))

# save_plot(f1a_sud, "F1a_SUD_supplement_a", dir_output, width = 16)

##================================================================
## 1.1 Figure 1b — HIV: Spending per prevalent case
##     (DEX all-payer + RW, prevalence-proportional RW weights)
##
## WHY prevalence-proportional weights here:
##   Uniform age-overlap splits dump substantial RW dollars into
##   pediatric DEX bins (5-<10, 10-<15) where HIV prevalence is
##   near zero → inflated per-case values. Prevalence-proportional
##   weights route funding to where patients actually are.
##================================================================

# ── 1. DEX all-payer HIV spending ────────────────────────────────
df_f1b_dex <- df_dex %>%
  filter(geo == "national", acause == "hiv",
         year_id == 2019, payer == "all") %>%
  collect() %>%
  dplyr::group_by(age_name, sex_name) %>%
  dplyr::summarise(spend_dex = sum(spend_mean, na.rm = TRUE),
                   .groups = "drop") %>%
  dplyr::mutate(age_name = str_replace_all(age_name, " ", ""))

# ── 2. GBD HIV prevalence by DEX age bin ─────────────────────────
df_f1b_gbd <- df_gbd %>%
  filter(cause_name == "HIV/AIDS", year_id == 2019) %>%
  dplyr::group_by(age_group_name, sex_id) %>%
  dplyr::summarise(prevalence_counts = sum(prevalence_counts, na.rm = TRUE),
                   .groups = "drop") %>%
  dplyr::mutate(
    age_name = map_gbd_to_dex_age(age_group_name),
    sex_name = dplyr::case_when(sex_id == 1 ~ "Male",
                                sex_id == 2 ~ "Female",
                                TRUE ~ NA_character_)
  ) %>%
  filter(!is.na(age_name), !is.na(sex_name)) %>%
  dplyr::group_by(sex_name, age_name) %>%
  dplyr::summarise(prevalence_counts = sum(prevalence_counts, na.rm = TRUE),
                   .groups = "drop")

# ── 3. Build PREVALENCE-PROPORTIONAL RW split weights ────────────
#   For each (sex × RW broad age bin), share of HIV prevalence
#   in each constituent DEX bin replaces the uniform split_weight.
dex_to_rw_map <- rw_age_split %>%
  dplyr::select(rw_age, age_name) %>%
  dplyr::distinct()

prev_weights <- df_f1b_gbd %>%
  dplyr::inner_join(dex_to_rw_map, by = "age_name", relationship = "many-to-many") %>%
  dplyr::group_by(sex_name, rw_age) %>%
  dplyr::mutate(
    total_prev = sum(prevalence_counts),
    prev_weight = ifelse(total_prev > 0, prevalence_counts / total_prev, 0)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(-total_prev)

# prev_weights <- df_f1b_gbd %>%
#   dplyr::inner_join(dex_to_rw_map, by = "age_name") %>%
#   dplyr::group_by(sex_name, rw_age) %>%
#   dplyr::mutate(
#     prev_weight = dplyr::if_else(
#       sum(prevalence_counts) > 0,
#       prevalence_counts / sum(prevalence_counts),
#       0
#     )
#   ) %>%
#   dplyr::ungroup()

# ── DIAGNOSTIC: compare old vs new weights ───────────────────────
message("\n=== Prevalence-proportional vs uniform RW split weights ===")
diagnostic <- prev_weights %>%
  dplyr::left_join(
    rw_age_split %>% dplyr::select(rw_age, age_name, split_weight),
    by = c("rw_age", "age_name")
  ) %>%
  dplyr::select(sex_name, rw_age, age_name,
                prevalence_counts, prev_weight, split_weight) %>%
  dplyr::arrange(rw_age, sex_name, age_name)
print(diagnostic, n = Inf)

# ── 4. Redistribute RW funding using prevalence weights ──────────
df_f1b_rw <- df_rw_long %>%
  filter(year_id == 2019) %>%
  dplyr::rename(rw_age = age_name) %>%
  dplyr::group_by(rw_age, sex_name) %>%
  dplyr::summarise(rw_funding_total = sum(rw_funding, na.rm = TRUE),
                   .groups = "drop") %>%
  dplyr::inner_join(
    prev_weights %>% dplyr::select(sex_name, rw_age, age_name, prev_weight),
    by = c("rw_age", "sex_name")
  ) %>%
  dplyr::mutate(spend_rw = rw_funding_total * prev_weight) %>%
  dplyr::group_by(age_name, sex_name) %>%
  dplyr::summarise(spend_rw = sum(spend_rw, na.rm = TRUE),
                   .groups = "drop")

# ── 5. Combine DEX + RW, merge prevalence, compute per-case ─────
df_f1b_hiv <- df_f1b_dex %>%
  dplyr::full_join(df_f1b_rw, by = c("age_name", "sex_name")) %>%
  dplyr::mutate(
    spend_dex = tidyr::replace_na(spend_dex, 0),
    spend_rw  = tidyr::replace_na(spend_rw, 0),
    spend_total = spend_dex + spend_rw
  ) %>%
  dplyr::left_join(df_f1b_gbd, by = c("age_name", "sex_name")) %>%
  dplyr::mutate(
    spend_per_case = dplyr::if_else(
      prevalence_counts > 0,
      spend_total / prevalence_counts,
      NA_real_
    ),
    spend_per_case_inverse = dplyr::if_else(
      sex_name == "Male", spend_per_case * -1, spend_per_case
    )
  )

# After computing df_f1b_hiv, before setting factors:
df_f1b_hiv <- df_f1b_hiv %>%
  filter(!age_name %in% c("0-<1", "1-<5", "5-<10", "10-<15"))

# ── DIAGNOSTIC ───────────────────────────────────────────────────
message("\n=== HIV spending per case by age/sex (DEX + RW, 2019) ===")
print(
  df_f1b_hiv %>%
    dplyr::select(age_name, sex_name, spend_dex, spend_rw,
                  spend_total, prevalence_counts, spend_per_case) %>%
    dplyr::arrange(sex_name, age_name),
  n = Inf
)

# ── Factors ──────────────────────────────────────────────────────
df_f1b_hiv$age_name <- factor(df_f1b_hiv$age_name, levels = age_factor)
df_f1b_hiv$sex_name <- factor(df_f1b_hiv$sex_name, levels = sex_factor)
##CHECK HIV PREP PLOT

max_spend <- max(abs(df_f1b_hiv$spend_per_case_inverse), na.rm = TRUE)
max_prev  <- max(df_f1b_hiv$prevalence_counts, na.rm = TRUE)

df_f1b_hiv <- df_f1b_hiv %>%
  mutate(
    prevalence_scaled = prevalence_counts / max_prev * max_spend,
    prevalence_scaled_inverse = ifelse(
      sex_name == "Male",
      prevalence_scaled * -1,
      prevalence_scaled
    )
  )


###CEHCKING NEW PLOT 
f1b_hiv <- ggplot(
  data = df_f1b_hiv,
  aes(age_name, spend_per_case_inverse)
) +
  facet_share(~ sex_name, scales = "free", reverse_num = FALSE) +
  geom_col(aes(fill = sex_name), width = 0.9) +
  geom_line(
    aes(y = prevalence_scaled_inverse, group = sex_name),
    color = "black", linewidth = 0.7
  ) +
  geom_point(
    aes(y = prevalence_scaled_inverse),
    color = "black", size = 2
  ) +
  coord_flip() +
  scale_y_continuous(
    labels = function(x) scales::dollar(abs(x), accuracy = 100),
    name = "Spending per prevalent case (2019 USD)",
    sec.axis = sec_axis(
      trans = ~ abs(.) / max_spend * max_prev,
      name = "Prevalent cases",
      labels = scales::label_number(big.mark = ",")
    )
  ) +
  theme_classic() +
  labs(
    x = "",
    title = "HIV spending per case by sex and age group, All Payers, 2019"
  ) +
  theme_settings +
  theme(
    legend.position = "none",
    plot.margin = margin(t = 10, r = 90, b = 0, l = 0)
  )

save_plot(f1b_hiv, "F1b_hiv_spending_per_case_with_prevalence_overlay", dir_output)
####


##END CHECK HIV PREV PLOT



# # ── Plot ─────────────────────────────────────────────────────────
# f1b_hiv <- ggplot(
#   data = df_f1b_hiv,
#   aes(age_name, spend_per_case_inverse, fill = sex_name)
# ) +
#   facet_share(~ sex_name, scales = "free", reverse_num = FALSE) +
#   geom_bar(stat = "identity") +
#   coord_flip() +
#   scale_y_continuous(
#     labels = function(x) scales::dollar(abs(x), accuracy = 100)
#   ) +
#   theme_classic() +
#   labs(
#     y = "Spending per prevalent case (2019 USD)", x = "",
#     title = "HIV spending per case by sex and age group, All Payers + Ryan White, 2019"
#   ) +
#   theme_settings +
#   theme(legend.position = "none",
#         plot.margin = margin(t = 10, r = 90, b = 0, l = 0))
# 
# save_plot(f1b_hiv, "F1b_HIV_spending_per_case_RW_DEX_agebins", dir_output)


##================================================================
## 1.1 Figure 1b — SUD: Spending per prevalent case
##     (DEX all-payer only, no Ryan White)
##================================================================

# ── DEX all-payer SUD spending ───────────────────────────────────
df_f1b_sud_dex <- df_dex %>%
  filter(geo == "national", acause %in% subs_causes,
         year_id == 2019, payer == "all") %>%
  collect() %>%
  dplyr::group_by(age_name, sex_name) %>%
  dplyr::summarise(spend_mean = sum(spend_mean, na.rm = TRUE),
                   .groups = "drop") %>%
  dplyr::mutate(age_name = str_replace_all(age_name, " ", ""))

# ── GBD SUD prevalence ──────────────────────────────────────────
df_f1b_sud_gbd <- df_gbd %>%
  filter(cause_name == "Substance use disorders", year_id == 2019) %>%
  dplyr::group_by(age_group_name, sex_id) %>%
  dplyr::summarise(prevalence_counts = sum(prevalence_counts, na.rm = TRUE),
                   .groups = "drop") %>%
  dplyr::mutate(
    age_name = map_gbd_to_dex_age(age_group_name),
    sex_name = dplyr::case_when(sex_id == 1 ~ "Male",
                                sex_id == 2 ~ "Female",
                                TRUE ~ NA_character_)
  ) %>%
  filter(!is.na(age_name), !is.na(sex_name)) %>%
  dplyr::group_by(sex_name, age_name) %>%
  dplyr::summarise(prevalence_counts = sum(prevalence_counts, na.rm = TRUE),
                   .groups = "drop")

# ── DIAGNOSTIC: check join alignment ────────────────────────────
message("\n=== SUD DEX age bins ===")
print(sort(unique(df_f1b_sud_dex$age_name)))
message("=== SUD GBD age bins ===")
print(sort(unique(df_f1b_sud_gbd$age_name)))

# ── Merge and compute per-case ───────────────────────────────────
df_f1b_sud <- df_f1b_sud_dex %>%
  dplyr::left_join(df_f1b_sud_gbd, by = c("age_name", "sex_name")) %>%
  dplyr::mutate(
    spend_per_case = ifelse(
      prevalence_counts > 0,
      spend_mean / prevalence_counts,
      NA_real_
    ),
    spend_per_case_inverse = ifelse(
      sex_name == "Male", spend_per_case * -1, spend_per_case
    )
  )

df_f1b_sud <- df_f1b_sud %>%
  filter(!age_name %in% c("0-<1", "1-<5", "5-<10", "10-<15"))

# ── DIAGNOSTIC ───────────────────────────────────────────────────
message("\n=== SUD spending per case by age/sex (DEX, 2019) ===")
print(
  df_f1b_sud %>%
    dplyr::select(age_name, sex_name, spend_mean,
                  prevalence_counts, spend_per_case) %>%
    dplyr::arrange(sex_name, age_name),
  n = Inf
)

# ── Factors ──────────────────────────────────────────────────────
df_f1b_sud$age_name <- factor(df_f1b_sud$age_name, levels = age_factor)
df_f1b_sud$sex_name <- factor(df_f1b_sud$sex_name, levels = sex_factor)

# ── Plot ─────────────────────────────────────────────────────────
f1b_sud <- ggplot(
  data = df_f1b_sud,
  aes(age_name, spend_per_case_inverse, fill = sex_name)
) +
  facet_share(~ sex_name, scales = "free", reverse_num = FALSE) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous(
    labels = function(x) scales::dollar(abs(x), accuracy = 100)
  ) +
  theme_classic() +
  labs(
    y = "Spending per prevalent case (2019 USD)", x = "",
    title = "SUD spending per case by sex and age group, All Payers, 2019"
  ) +
  theme_settings +
  theme(legend.position = "none",
        plot.margin = margin(t = 10, r = 90, b = 0, l = 0))

#save_plot(f1b_sud, "F1b_SUD_spending_per_case_DEX_agebins", dir_output)

##================================================================
## 1.1 Figure 1b — OUD: Spending per prevalent case
##     (DEX all-payer only, no Ryan White)
##================================================================

# ── DEX all-payer OUD spending ───────────────────────────────────
df_f1b_oud_dex <- df_dex %>%
  filter(geo == "national", acause == "mental_drug_opioids",
         year_id == 2019, payer == "all") %>%
  collect() %>%
  dplyr::group_by(age_name, sex_name) %>%
  dplyr::summarise(spend_mean = sum(spend_mean, na.rm = TRUE),
                   .groups = "drop") %>%
  dplyr::mutate(age_name = str_replace_all(age_name, " ", ""))

# ── GBD OUD prevalence ──────────────────────────────────────────
df_f1b_oud_gbd <- df_gbd %>%
  filter(cause_name == "Opioid use disorders", year_id == 2019) %>%
  dplyr::group_by(age_group_name, sex_id) %>%
  dplyr::summarise(prevalence_counts = sum(prevalence_counts, na.rm = TRUE),
                   .groups = "drop") %>%
  dplyr::mutate(
    age_name = map_gbd_to_dex_age(age_group_name),
    sex_name = dplyr::case_when(sex_id == 1 ~ "Male",
                                sex_id == 2 ~ "Female",
                                TRUE ~ NA_character_)
  ) %>%
  filter(!is.na(age_name), !is.na(sex_name)) %>%
  dplyr::group_by(sex_name, age_name) %>%
  dplyr::summarise(prevalence_counts = sum(prevalence_counts, na.rm = TRUE),
                   .groups = "drop")

# ── Merge and compute per-case ───────────────────────────────────
df_f1b_oud <- df_f1b_oud_dex %>%
  dplyr::left_join(df_f1b_oud_gbd, by = c("age_name", "sex_name")) %>%
  dplyr::mutate(
    spend_per_case = ifelse(
      prevalence_counts > 0,
      spend_mean / prevalence_counts,
      NA_real_
    ),
    spend_per_case_inverse = ifelse(
      sex_name == "Male", spend_per_case * -1, spend_per_case
    )
  )

df_f1b_oud <- df_f1b_oud %>%
  filter(!age_name %in% c("0-<1", "1-<5", "5-<10", "10-<15"))

# ── Factors ──────────────────────────────────────────────────────
df_f1b_oud$age_name <- factor(df_f1b_oud$age_name, levels = age_factor)
df_f1b_oud$sex_name <- factor(df_f1b_oud$sex_name, levels = sex_factor)

#--- adding prev to the plot 
# Create scaled prevalence for overlay
max_spend <- max(abs(df_f1b_oud$spend_per_case_inverse), na.rm = TRUE)
max_prev  <- max(df_f1b_oud$prevalence_counts, na.rm = TRUE)

df_f1b_oud <- df_f1b_oud %>%
  mutate(
    prevalence_scaled = prevalence_counts / max_prev * max_spend,
    prevalence_scaled_inverse = ifelse(
      sex_name == "Male",
      prevalence_scaled * -1,
      prevalence_scaled
    )
  )


f1b_oud <- ggplot(
  data = df_f1b_oud,
  aes(age_name, spend_per_case_inverse)
) +
  facet_share(~ sex_name, scales = "free", reverse_num = FALSE) +
  geom_col(aes(fill = sex_name), width = 0.9) +
  geom_line(
    aes(y = prevalence_scaled_inverse, group = sex_name),
    color = "black", linewidth = 0.7
  ) +
  geom_point(
    aes(y = prevalence_scaled_inverse),
    color = "black", size = 2
  ) +
  coord_flip() +
  scale_y_continuous(
    labels = function(x) scales::dollar(abs(x), accuracy = 100),
    name = "Spending per prevalent case (2019 USD)",
    sec.axis = sec_axis(
      trans = ~ abs(.) / max_spend * max_prev,
      name = "Prevalent cases",
      labels = scales::label_number(big.mark = ",")
    )
  ) +
  theme_classic() +
  labs(
    x = "",
    title = "OUD spending per case by sex and age group, All Payers, 2019"
  ) +
  theme_settings +
  theme(
    legend.position = "none",
    plot.margin = margin(t = 10, r = 90, b = 0, l = 0)
  )

save_plot(f1b_oud, "F1b_OUD_spending_per_case_with_prevalence_overlay", dir_output)
####


##### SANITY CHECKS BELOW

message("\n=== OUD sanity check by age/sex ===")

df_oud_check <- df_f1b_oud %>%
  left_join(
    df_gbd %>%
      filter(cause_name == "Opioid use disorders", year_id == 2019) %>%
      group_by(age_group_name, sex_id) %>%
      summarise(
        prevalence_counts_raw = sum(prevalence_counts, na.rm = TRUE),
        population = sum(population, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        age_name = map_gbd_to_dex_age(age_group_name),
        sex_name = case_when(
          sex_id == 1 ~ "Male",
          sex_id == 2 ~ "Female",
          TRUE ~ NA_character_
        )
      ) %>%
      filter(!is.na(age_name), !is.na(sex_name)) %>%
      group_by(age_name, sex_name) %>%
      summarise(
        prevalence_counts_raw = sum(prevalence_counts_raw, na.rm = TRUE),
        population = sum(population, na.rm = TRUE),
        .groups = "drop"
      ),
    by = c("age_name", "sex_name")
  ) %>%
  mutate(
    prevalence_per_100k = prevalence_counts_raw / population * 1e5
  ) %>%
  select(age_name, sex_name, spend_mean, prevalence_counts, prevalence_counts_raw,
         population, prevalence_per_100k, spend_per_case) %>%
  arrange(sex_name, age_name)

print(df_oud_check, n = Inf)


message("\n=== OUD payer sum vs all-payer check ===")

df_oud_all <- df_dex %>%
  filter(geo == "national", acause == "mental_drug_opioids",
         year_id == 2019, payer == "all") %>%
  collect() %>%
  group_by(age_name, sex_name) %>%
  summarise(spend_all = sum(spend_mean, na.rm = TRUE), .groups = "drop") %>%
  mutate(age_name = str_replace_all(age_name, " ", ""))

df_oud_sum_payers <- df_dex %>%
  filter(geo == "national", acause == "mental_drug_opioids",
         year_id == 2019, payer != "all") %>%
  collect() %>%
  group_by(age_name, sex_name) %>%
  summarise(spend_sum_payers = sum(spend_mean, na.rm = TRUE), .groups = "drop") %>%
  mutate(age_name = str_replace_all(age_name, " ", ""))

df_oud_all %>%
  left_join(df_oud_sum_payers, by = c("age_name", "sex_name")) %>%
  mutate(diff = spend_all - spend_sum_payers) %>%
  arrange(sex_name, age_name) %>%
  print(n = Inf)

ggplot(df_oud_check, aes(age_name, spend_mean, color = sex_name, group = sex_name)) +
  geom_line() +
  geom_point() +
  coord_flip() +
  theme_minimal() +
  labs(title = "OUD raw spending by age and sex, 2019", y = "Spending", x = "")

ggplot(df_oud_check, aes(age_name, prevalence_counts, color = sex_name, group = sex_name)) +
  geom_line() +
  geom_point() +
  coord_flip() +
  theme_minimal() +
  labs(title = "OUD prevalence counts by age and sex, 2019", y = "Prevalence counts", x = "")


df_oud_check %>%
  mutate(
    sparse_prev = prevalence_counts < 5000,
    very_sparse_prev = prevalence_counts < 2000
  ) %>%
  select(age_name, sex_name, prevalence_counts, prevalence_per_100k, spend_per_case,
         sparse_prev, very_sparse_prev) %>%
  print(n = Inf)


####END OF SAMITY CHECKS

##================================================================
## 1.1 Figure 1b — AUD: Spending per prevalent case
##     (DEX all-payer only, no Ryan White)
##================================================================

# ── DEX all-payer SUD spending ───────────────────────────────────
df_f1b_aud_dex <- df_dex %>%
  filter(geo == "national", acause == "mental_alcohol",
         year_id == 2019, payer == "all") %>%
  collect() %>%
  dplyr::group_by(age_name, sex_name) %>%
  dplyr::summarise(spend_mean = sum(spend_mean, na.rm = TRUE),
                   .groups = "drop") %>%
  dplyr::mutate(age_name = str_replace_all(age_name, " ", ""))

# ── GBD SUD prevalence ──────────────────────────────────────────
df_f1b_aud_gbd <- df_gbd %>%
  filter(cause_name == "Alcohol use disorders", year_id == 2019) %>%
  dplyr::group_by(age_group_name, sex_id) %>%
  dplyr::summarise(prevalence_counts = sum(prevalence_counts, na.rm = TRUE),
                   .groups = "drop") %>%
  dplyr::mutate(
    age_name = map_gbd_to_dex_age(age_group_name),
    sex_name = dplyr::case_when(sex_id == 1 ~ "Male",
                                sex_id == 2 ~ "Female",
                                TRUE ~ NA_character_)
  ) %>%
  filter(!is.na(age_name), !is.na(sex_name)) %>%
  dplyr::group_by(sex_name, age_name) %>%
  dplyr::summarise(prevalence_counts = sum(prevalence_counts, na.rm = TRUE),
                   .groups = "drop")

# ── Merge and compute per-case ───────────────────────────────────
df_f1b_aud <- df_f1b_aud_dex %>%
  dplyr::left_join(df_f1b_aud_gbd, by = c("age_name", "sex_name")) %>%
  dplyr::mutate(
    spend_per_case = ifelse(
      prevalence_counts > 0,
      spend_mean / prevalence_counts,
      NA_real_
    ),
    spend_per_case_inverse = ifelse(
      sex_name == "Male", spend_per_case * -1, spend_per_case
    )
  )

df_f1b_aud <- df_f1b_aud %>%
  filter(!age_name %in% c("0-<1", "1-<5", "5-<10", "10-<15"))

# ── Factors ──────────────────────────────────────────────────────
df_f1b_aud$age_name <- factor(df_f1b_aud$age_name, levels = age_factor)
df_f1b_aud$sex_name <- factor(df_f1b_aud$sex_name, levels = sex_factor)

# ── Plot ─────────────────────────────────────────────────────────
f1b_aud <- ggplot(
  data = df_f1b_aud,
  aes(age_name, spend_per_case_inverse, fill = sex_name)
) +
  facet_share(~ sex_name, scales = "free", reverse_num = FALSE) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous(
    labels = function(x) scales::dollar(abs(x), accuracy = 100)
  ) +
  theme_classic() +
  labs(
    y = "Spending per prevalent case (2019 USD)", x = "",
    title = "AUD spending per case by sex and age group, All Payers, 2019"
  ) +
  theme_settings +
  theme(legend.position = "none",
        plot.margin = margin(t = 10, r = 90, b = 0, l = 0))

#save_plot(f1b_aud, "F1b_AUD_spending_per_case_DEX_agebins", dir_output)

####
##================================================================
## 2. Figure 2 — HIV: Spending by type of care + Ryan White, 2019
##    (payer = "all", national, both sexes)
##================================================================

# DEX spending by TOC
df_f2_hiv <- df_dex %>%
  filter(geo == "national", acause == "hiv",
         year_id == 2019, payer == "all") %>%
  collect() %>%
  dplyr::group_by(toc, age_name, sex_name) %>%
  dplyr::summarise(spend_mean = sum(spend_mean, na.rm = TRUE),
                   .groups = "drop") %>%
  dplyr::mutate(age_name = str_replace_all(age_name, " ", ""))

# RW as its own "TOC" — reuse uniform-weight RW already built for F1a
df_f2_rw <- build_rw_dex_bins_uniform(df_rw_long, rw_age_split, yr = 2019) %>%
  dplyr::rename(toc = payer)  # "ryan_white" -> toc column

# Combine
df_f2_hiv <- bind_rows(df_f2_hiv, df_f2_rw) %>%
  dplyr::mutate(
    spend_mean_inverse = ifelse(sex_name == "Male", spend_mean * -1, spend_mean),
    age_name = factor(age_name, levels = age_factor),
    sex_name = factor(sex_name, levels = sex_factor),
    toc = factor(toc, levels = c(toc_factor, "ryan_white"))
  )

# Add RW to color and label vectors
toc_colors_rw <- c(toc_colors, "ryan_white" = "#6A51A3")
toc_labels_rw <- c(toc_labels, "ryan_white" = "Ryan White")

### PYRAMID WITH HELPER
f2_hiv <- build_pyramid(
  df           = df_f2_hiv,
  fill_var     = toc,
  fill_values  = toc_colors_rw,
  fill_labels  = toc_labels_rw,
  fill_legend_title = "Type of care",
  y_label      = "Inflation Adjusted Spending (2019 USD)",
  title_text   = "HIV spending for each type of care and Ryan White by sex and age group in 2019"
)

ggsave(file.path(dir_output, "F2_HIV_spending_by_toc_plus_RW.png"),
       plot = f2_hiv, width = 16, height = 10, dpi = 500)

###

# Plot
# f2_hiv <- ggplot(
#   data = df_f2_hiv,
#   aes(age_name, spend_mean_inverse, fill = toc)
# ) +
#   facet_share(~ sex_name, scales = "free", reverse_num = FALSE) +
#   geom_col(color = "black", width = 1, size = 0.3) +
#   coord_flip() +
#   scale_fill_manual(values = toc_colors_rw, labels = toc_labels_rw, name = "Type of care") +
#   scale_y_continuous(labels = axis_dollar_mb) +
#   #scale_y_continuous(labels = function(x) scales::dollar(abs(x))) +
#   theme_classic() +
#   labs(
#     y = "Inflation Adjusted Spending (2019 USD)", x = "",
#     title = "HIV spending for each type of care and Ryan White by sex and age group in 2019"
#   ) +
#   theme_settings +
#   guides(fill = guide_legend(title.position = "top", title.hjust = 0.5, nrow = 1))
# 
# save_plot(f2_hiv, "F2_HIV_spending_by_toc_plus_RW", dir_output)

##================================================================
## 2. Figure 2 — SUD: Spending by type of care, 2019
##================================================================

df_f2_sud <- df_dex %>%
  filter(geo == "national", acause %in% subs_causes,
         year_id == 2019, payer == "all") %>%
  collect() %>%
  dplyr::group_by(toc, age_name, sex_name) %>%
  dplyr::summarise(spend_mean = sum(spend_mean, na.rm = TRUE),
                   .groups = "drop") %>%
  dplyr::mutate(
    age_name = str_replace_all(age_name, " ", ""),
    spend_mean_inverse = ifelse(sex_name == "Male", spend_mean * -1, spend_mean),
    age_name = factor(age_name, levels = age_factor),
    sex_name = factor(sex_name, levels = sex_factor),
    toc = factor(toc, levels = toc_factor)
  )

f2_sud <- ggplot(
  data = df_f2_sud,
  aes(age_name, spend_mean_inverse, fill = toc)
) +
  facet_share(~ sex_name, scales = "free", reverse_num = FALSE) +
  geom_col(color = "black", width = 1, size = 0.3) +
  coord_flip() +
  scale_fill_manual(values = toc_colors, labels = toc_labels, name = "Type of care") +
  scale_y_continuous(labels = axis_dollar_mb) +
  # scale_y_continuous(
  #   expand = expansion(mult = c(0.15, 0.1)),
  #   labels = function(x) scales::dollar(abs(x))
  # ) +
  theme_classic() +
  labs(
    y = "Inflation Adjusted Spending (2019 USD)", x = "",
    title = "SUD spending for each type of care by sex and age group in 2019"
  ) +
  theme_settings +
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5, nrow = 1)) +
  theme(plot.margin = margin(t = 10, r = 70, b = 0, l = 0))

#save_plot(f2_sud, "F2_SUD_spending_by_toc", dir_output, width = 16)

##================================================================
## 2. Figure 2 — OUD: Spending by type of care, 2019
##================================================================

df_f2_oud <- df_dex %>%
  filter(geo == "national", acause == "mental_drug_opioids",
         year_id == 2019, payer == "all") %>%
  collect() %>%
  dplyr::group_by(toc, age_name, sex_name) %>%
  dplyr::summarise(spend_mean = sum(spend_mean, na.rm = TRUE),
                   .groups = "drop") %>%
  dplyr::mutate(
    age_name = str_replace_all(age_name, " ", ""),
    spend_mean_inverse = ifelse(sex_name == "Male", spend_mean * -1, spend_mean),
    age_name = factor(age_name, levels = age_factor),
    sex_name = factor(sex_name, levels = sex_factor),
    toc = factor(toc, levels = toc_factor)
  )


## PYRPAMID W HELPER


f2_oud <- build_pyramid(
  df           = df_f2_oud,
  fill_var     = toc,
  fill_values  = toc_colors,
  fill_labels  = toc_labels,
  fill_legend_title = "Type of care",
  y_label      = "Inflation Adjusted Spending (2019 USD)",
  title_text   = "OUD spending for each type of care by sex and age group in 2019"
)

ggsave(file.path(dir_output, "F2_OUD_spending_by_toc.png"),
       plot = f2_oud, width = 16, height = 10, dpi = 500)

# f2_oud <- ggplot(
#   data = df_f2_oud,
#   aes(age_name, spend_mean_inverse, fill = toc)
# ) +
#   facet_share(~ sex_name, scales = "free", reverse_num = FALSE) +
#   geom_col(color = "black", width = 1, size = 0.3) +
#   coord_flip() +
#   scale_fill_manual(values = toc_colors, labels = toc_labels, name = "Type of care") +
#   scale_y_continuous(labels = axis_dollar_mb) +
#   # scale_y_continuous(
#   #   expand = expansion(mult = c(0.15, 0.1)),
#   #   labels = function(x) scales::dollar(abs(x))
#   # ) +
#   theme_classic() +
#   labs(
#     y = "Inflation Adjusted Spending (2019 USD)", x = "",
#     title = "OUD spending for each type of care by sex and age group in 2019"
#   ) +
#   theme_settings +
#   guides(fill = guide_legend(title.position = "top", title.hjust = 0.5, nrow = 1)) +
#   theme(plot.margin = margin(t = 10, r = 70, b = 0, l = 0))
# 
# save_plot(f2_oud, "F2_OUD_spending_by_toc", dir_output, width = 16)

##================================================================
## 2. Figure 2 — AUD: Spending by type of care, 2019
##================================================================

df_f2_aud <- df_dex %>%
  filter(geo == "national", acause == "mental_alcohol",
         year_id == 2019, payer == "all") %>%
  collect() %>%
  dplyr::group_by(toc, age_name, sex_name) %>%
  dplyr::summarise(spend_mean = sum(spend_mean, na.rm = TRUE),
                   .groups = "drop") %>%
  dplyr::mutate(
    age_name = str_replace_all(age_name, " ", ""),
    spend_mean_inverse = ifelse(sex_name == "Male", spend_mean * -1, spend_mean),
    age_name = factor(age_name, levels = age_factor),
    sex_name = factor(sex_name, levels = sex_factor),
    toc = factor(toc, levels = toc_factor)
  )

f2_aud <- ggplot(
  data = df_f2_aud,
  aes(age_name, spend_mean_inverse, fill = toc)
) +
  facet_share(~ sex_name, scales = "free", reverse_num = FALSE) +
  geom_col(color = "black", width = 1, size = 0.3) +
  coord_flip() +
  scale_fill_manual(values = toc_colors, labels = toc_labels, name = "Type of care") +
  scale_y_continuous(labels = axis_dollar_mb) +
  # scale_y_continuous(
  #   expand = expansion(mult = c(0.15, 0.1)),
  #   labels = function(x) scales::dollar(abs(x))
  # ) +
  theme_classic() +
  labs(
    y = "Inflation Adjusted Spending (2019 USD)", x = "",
    title = "AUD spending for each type of care by sex and age group in 2019"
  ) +
  theme_settings +
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5, nrow = 1)) +
  theme(plot.margin = margin(t = 10, r = 70, b = 0, l = 0))

#save_plot(f2_aud, "F2_AUD_spending_by_toc", dir_output, width = 16)

##================================================================
## 3. MAPS — HIV
##
## REVISED LAYOUT:
##   Figure 3a  — All-payer county spending (standalone, full width)
##   Figure 3b  — 2x2 payer-specific county maps (mdcr, mdcd, priv, oop)
##   Figure 3c  — State-level spending per prevalent case (standalone)
##
## Key visual fixes:
##   - Larger legend text (size 11-12 vs old 10)
##   - Consistent theme_map() across all maps
##   - Adequate plot margins so legends don't clip
##   - guide_legend(nrow = 1) for horizontal legends
##   - build_county_map() helper for consistency
##================================================================

# ── Prepare data (UNCHANGED logic) ──────────────────────────────

# Payer strata
df_f3_hiv_payer <- df_dex %>%
  filter(geo == "county", year_id == 2019, payer != "all", acause == "hiv") %>%
  collect() %>%
  group_by(payer, state_name, location_name, fips) %>%
  summarize("value" = sum(spend_mean), .groups = "drop")

# Overall
df_f3_hiv_overall <- df_dex %>%
  filter(geo == "county", year_id == 2019, payer == "all", acause == "hiv") %>%
  collect() %>%
  group_by(state_name, location_name, fips) %>%
  summarize("value" = sum(spend_mean), .groups = "drop")

# Merge mcnty
df_f3_hiv_payer <- left_join(df_f3_hiv_payer, df_loc_ids %>% select(mcnty, full_fips_code), by = c("fips" = "full_fips_code"))
df_f3_hiv_overall <- left_join(df_f3_hiv_overall, df_loc_ids %>% select(mcnty, full_fips_code), by = c("fips" = "full_fips_code"))


## ── Figure 3a: All-payer county map (standalone) ────────────────

brks <- quantile(df_f3_hiv_overall$value, probs = c(0, .2, .4, .6, .8, 1), na.rm = TRUE)
brks[1] <- brks[1] - 1
# Deduplicate breaks (can happen when many counties share the same value)
brks <- unique(brks)

labs_all <- paste0("$", format(comma(round(brks[-length(brks)])), trim = TRUE),
                   " \u2013 $", format(comma(round(brks[-1])), trim = TRUE))
labs_all[length(labs_all)] <- paste0("$", format(comma(round(brks[length(brks) - 1])), trim = TRUE), "+")

df_f3_hiv_overall$plot_val <- cut(df_f3_hiv_overall$value, breaks = brks, labels = labs_all, include.lowest = TRUE)

pal_fun <- colorRampPalette(c("#F1F1F1", "#B12BC9"))
cols_5 <- pal_fun(5)

df_f3_hiv_overall_sf <- merge(mcnty_shapefile, df_f3_hiv_overall, by = "mcnty")

f3a_hiv_allpayer <- build_county_map(
  map_sf_data = df_f3_hiv_overall_sf,
  state_sf    = state_shapefile,
  cols        = cols_5,
  title_text  = "HIV all-payer spending by US county, 2019"
)

# Save as standalone figure
ggsave(file.path(dir_output, "F3a_HIV_allpayer_county_map.png"),
       plot = f3a_hiv_allpayer, width = 14, height = 8, dpi = 500)


## ── Figure 3b: 2x2 payer-specific county maps ──────────────────

### CODE Below produced 2x2 table in percent of total

## ── Figure 3b: 2x2 payer-specific county maps (% of all-payer) ─
# Pre-compute all-payer total per county for denominator
# ── Figure 3b: 2x2 payer-specific county maps (% of all-payer) ─
county_totals <- df_f3_hiv_overall %>% select(fips, total_value = value)

f3_payer_plot_list <- list()
for (p in c("mdcr", "mdcd", "priv", "oop")) {
  message("Building map for: ", p)
  
  map_df <- df_f3_hiv_payer %>%
    filter(payer == p) %>%
    left_join(county_totals, by = "fips") %>%
    mutate(pct = ifelse(total_value > 0, value / total_value * 100, 0))
  
  brks <- quantile(map_df$pct, probs = c(0, .2, .4, .6, .8, 1), na.rm = TRUE)
  brks[1] <- brks[1] - 0.01
  brks <- unique(brks)
  
  labs_p <- paste0(
    format(round(brks[-length(brks)], 1), trim = TRUE), "%",
    " \u2013 ",
    format(round(brks[-1], 1), trim = TRUE), "%"
  )
  
  map_df$plot_val <- cut(map_df$pct, breaks = brks,
                         labels = labs_p, include.lowest = TRUE)
  
  cols <- payer_colors_maps[[p]]
  payer_title <- payer_list[[p]]
  
  map_df_sf <- merge(mcnty_shapefile, map_df, by = "mcnty")
  
  map_plot <- build_county_map(
    map_sf_data = map_df_sf,
    state_sf    = state_shapefile,
    cols        = cols,
    title_text  = paste0(payer_title, " spending")
  ) +
    theme(
      legend.text      = element_text(size = 12),
      legend.key.width = unit(0.8, "cm"),
      legend.key.height = unit(0.4, "cm")
    )
  
  f3_payer_plot_list[[length(f3_payer_plot_list) + 1]] <- map_plot
}

# Combine into 2x2 grid
f3b_hiv_payer_grid <- wrap_plots(f3_payer_plot_list, ncol = 2) +
  plot_annotation(
    title = "HIV spending by payer and US county, 2019 (% of all-payer total)",
    theme = theme(
      plot.title = element_text(size = 20, face = "plain", hjust = 0),
      plot.margin = margin(10, 20, 10, 20)
    )
  )

ggsave(file.path(dir_output, "F3b_HIV_payer_county_maps.png"),
       plot = f3b_hiv_payer_grid, width = 18, height = 14, dpi = 500)
###END OF NEW CODE

# f3_payer_plot_list <- list()
# for(p in c("mdcr", "mdcd", "priv", "oop")){
#   message("Building map for: ", p)
#   
#   map_df <- df_f3_hiv_payer %>% filter(payer == p)
#   
#   brks <- c(quantile(map_df$value, .0, na.rm = TRUE),
#             quantile(map_df$value, .2, na.rm = TRUE),
#             quantile(map_df$value, .4, na.rm = TRUE),
#             quantile(map_df$value, .6, na.rm = TRUE),
#             quantile(map_df$value, .8, na.rm = TRUE),
#             quantile(map_df$value, 1, na.rm = TRUE))
#   brks <- unique(brks)
#   brks[1] <- brks[1] - 1
#   labs_p <- paste0("$", format(comma(round(brks[-length(brks)])), trim = TRUE),
#                    " \u2013 $", format(comma(round(brks[-1])), trim = TRUE))
#   labs_p[length(labs_p)] <- paste0("$", format(comma(round(brks[length(brks) - 1])), trim = TRUE), "+")
#   map_df$plot_val <- cut(map_df$value, breaks = brks, labels = labs_p, include.lowest = TRUE)
#   
#   cols <- payer_colors_maps[[p]]
#   payer_title <- payer_list[[p]]
#   
#   map_df_sf <- merge(mcnty_shapefile, map_df, by = "mcnty")
#   
#   map_plot <- build_county_map(
#     map_sf_data = map_df_sf,
#     state_sf    = state_shapefile,
#     cols        = cols,
#     title_text  = paste0(payer_title, " spending")
#   )
#   
#   f3_payer_plot_list[[length(f3_payer_plot_list) + 1]] <- map_plot
# }
# 
# # Combine into 2x2 grid with patchwork
# f3b_hiv_payer_grid <- wrap_plots(f3_payer_plot_list, ncol = 2) +
#   plot_annotation(
#     title = "HIV spending by payer and US county, 2019",
#     theme = theme(plot.title = element_text(size = 16, face = "bold"))
#   )
# 
# ggsave(file.path(dir_output, "F3b_HIV_payer_county_maps.png"),
#        plot = f3b_hiv_payer_grid, width = 16, height = 14, dpi = 500)
# 
# 

## ── Figure 3c: State-level spending per prevalent case ──────────

make_state_spend_map <- function(table_name,
                                 out_stub,
                                 title_text,
                                 dir_output,
                                 state_shapefile) {
  
  table_fp <- file.path(dir_output, table_name)
  df <- read.csv(table_fp, stringsAsFactors = FALSE, check.names = FALSE)
  
  df$spend_per_prev_num <- suppressWarnings(
    as.numeric(gsub("[^0-9.\\-]", "", trimws(df$`Spending per prevalence`)))
  )
  
  df$State <- trimws(df$State)
  
  missing_states <- dplyr::anti_join(df, state_shapefile, by = c("State" = "state_name"))
  if (nrow(missing_states) > 0) {
    message("States in ", table_name, " not found in shapefile: ",
            paste(missing_states$State, collapse = ", "))
  }
  if (nrow(missing_states) > 5) stop("Too many unmatched states (", nrow(missing_states), "). Check name alignment.")
  
  map_state_df <- dplyr::left_join(state_shapefile, df, by = c("state_name" = "State"))
  
  brks <- quantile(map_state_df$spend_per_prev_num, probs = c(0, .2, .4, .6, .8, 1), na.rm = TRUE)
  brks[1] <- brks[1] - 1
  
  labs <- paste0("$", format(scales::comma(round(brks[-length(brks)])), trim = TRUE),
                 " \u2013 $", format(scales::comma(round(brks[-1])), trim = TRUE))
  
  map_state_df$plot_val <- cut(map_state_df$spend_per_prev_num,
                               breaks = brks, labels = labs, include.lowest = TRUE)
  
  pal_fun <- colorRampPalette(c("#E9D3EB", "#B12BC9"))
  cols_5 <- pal_fun(5)
  
  # *** REVISED: use theme_map() for consistency ***
  p <- ggplot2::ggplot(data = map_state_df) +
    ggplot2::geom_sf(ggplot2::aes(fill = plot_val, geometry = geometry),
                     color = "grey30", linewidth = 0.3) +
    ggplot2::labs(title = title_text, fill = "") +
    ggplot2::scale_fill_manual(
      values = cols_5,
      breaks = levels(map_state_df$plot_val),
      na.value = "#838484"
    ) +
    theme_map(title_size = 16, legend_text_size = 12) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1))
  
  # Save standalone
  ggsave(file.path(dir_output, paste0(out_stub, ".png")),
         plot = p, width = 14, height = 8, dpi = 500)
  
  message("NA spend_per_prev_num: ", sum(is.na(map_state_df$spend_per_prev_num)))
  print(table(map_state_df$plot_val, useNA = "ifany"))
  
  invisible(list(plot = p, map_df = map_state_df, data = df))
}

# HIV state map
res_hiv <- make_state_spend_map(
  table_name = "T1_HIV_2019.csv",
  out_stub   = "F3c_HIV_state_spend_per_prev",
  title_text = "HIV spending per prevalent case by state, 2019 (2019 USD)",
  dir_output = dir_output,
  state_shapefile = state_shapefile
)

# SUD state map
res_sud <- make_state_spend_map(
  table_name = "T1_SUD_2019.csv",
  out_stub   = "F3c_SUD_state_spend_per_prev",
  title_text = "SUD spending per prevalent case by state, 2019 (2019 USD)",
  dir_output = dir_output,
  state_shapefile = state_shapefile
)

# OUD state map
res_oud <- make_state_spend_map(
  table_name = "T1_OUD_2019.csv",
  out_stub   = "F3c_OUD_state_spend_per_prev",
  title_text = "OUD spending per prevalent case by state, 2019 (2019 USD)",
  dir_output = dir_output,
  state_shapefile = state_shapefile
)

# AUD state map
res_aud <- make_state_spend_map(
  table_name = "T1_AUD_2019.csv",
  out_stub   = "F3c_AUD_state_spend_per_prev",
  title_text = "AUD spending per prevalent case by state, 2019 (2019 USD)",
  dir_output = dir_output,
  state_shapefile = state_shapefile
)

##================================================================
## 3. MAPS — SUD  (same revised layout)
##
##   Figure 3a_SUD  — All-payer county spending (standalone)
##   Figure 3b_SUD  — 2x2 payer-specific county maps
##================================================================

# ── Prepare data ─────────────────────────────────────────────────

df_f3_sud_payer <- df_dex %>%
  filter(geo == "county", year_id == 2019, acause %in% subs_causes, payer != "all") %>%
  collect() %>%
  group_by(payer, state_name, location_name, fips) %>%
  summarize("value" = sum(spend_mean), .groups = "drop")

df_f3_sud_overall <- df_dex %>%
  filter(geo == "county", year_id == 2019, acause %in% subs_causes, payer == "all") %>%
  collect() %>%
  group_by(state_name, location_name, fips) %>%
  summarize("value" = sum(spend_mean), .groups = "drop")

df_f3_sud_payer <- left_join(df_f3_sud_payer, df_loc_ids %>% select(mcnty, full_fips_code), by = c("fips" = "full_fips_code"))
df_f3_sud_overall <- left_join(df_f3_sud_overall, df_loc_ids %>% select(mcnty, full_fips_code), by = c("fips" = "full_fips_code"))


## ── Figure 3a SUD: All-payer county map ─────────────────────────

brks <- quantile(df_f3_sud_overall$value, probs = c(0, .2, .4, .6, .8, 1), na.rm = TRUE)
brks[1] <- brks[1] - 1
brks <- unique(brks)

labs_all <- paste0("$", format(comma(round(brks[-length(brks)])), trim = TRUE),
                   " \u2013 $", format(comma(round(brks[-1])), trim = TRUE))
labs_all[length(labs_all)] <- paste0("$", format(comma(round(brks[length(brks) - 1])), trim = TRUE), "+")

df_f3_sud_overall$plot_val <- cut(df_f3_sud_overall$value, breaks = brks, labels = labs_all, include.lowest = TRUE)

cols_5 <- colorRampPalette(c("#F1F1F1", "#B12BC9"))(5)

df_f3_sud_overall_sf <- merge(mcnty_shapefile, df_f3_sud_overall, by = "mcnty")

f3a_sud_allpayer <- build_county_map(
  map_sf_data = df_f3_sud_overall_sf,
  state_sf    = state_shapefile,
  cols        = cols_5,
  title_text  = "SUD all-payer spending by US county, 2019"
)

ggsave(file.path(dir_output, "F3a_SUD_allpayer_county_map.png"),
       plot = f3a_sud_allpayer, width = 14, height = 8, dpi = 500)


## ── Figure 3b SUD: 2x2 payer maps ──────────────────────────────

f3_sud_plot_list <- list()
for(p in c("mdcr", "mdcd", "priv", "oop")){
  message("Building SUD map for: ", p)
  
  map_df <- df_f3_sud_payer %>% filter(payer == p)
  
  brks <- c(quantile(map_df$value, .0, na.rm = TRUE),
            quantile(map_df$value, .2, na.rm = TRUE),
            quantile(map_df$value, .4, na.rm = TRUE),
            quantile(map_df$value, .6, na.rm = TRUE),
            quantile(map_df$value, .8, na.rm = TRUE),
            quantile(map_df$value, 1, na.rm = TRUE))
  brks <- unique(brks)
  brks[1] <- brks[1] - 1
  labs_p <- paste0("$", format(comma(round(brks[-length(brks)])), trim = TRUE),
                   " \u2013 $", format(comma(round(brks[-1])), trim = TRUE))
  labs_p[length(labs_p)] <- paste0("$", format(comma(round(brks[length(brks) - 1])), trim = TRUE), "+")
  map_df$plot_val <- cut(map_df$value, breaks = brks, labels = labs_p, include.lowest = TRUE)
  
  cols <- payer_colors_maps[[p]]
  payer_title <- payer_list[[p]]
  
  map_df_sf <- merge(mcnty_shapefile, map_df, by = "mcnty")
  
  map_plot <- build_county_map(
    map_sf_data = map_df_sf,
    state_sf    = state_shapefile,
    cols        = cols,
    title_text  = paste0(payer_title, " spending")
  )
  
  f3_sud_plot_list[[length(f3_sud_plot_list) + 1]] <- map_plot
}

f3b_sud_payer_grid <- wrap_plots(f3_sud_plot_list, ncol = 2) +
  plot_annotation(
    title = "SUD spending by payer and US county, 2019",
    theme = theme(plot.title = element_text(size = 16, face = "bold"))
  )

ggsave(file.path(dir_output, "F3b_SUD_payer_county_maps.png"),
       plot = f3b_sud_payer_grid, width = 16, height = 14, dpi = 500)

pdf(file = file.path(dir_output, "F3b_SUD_payer_county_maps.pdf"), width = 16, height = 14)
print(f3b_sud_payer_grid)
dev.off()


##================================================================
## 3. MAPS — OUD  (same revised layout)
##
##   Figure 3a_OUD  — All-payer county spending (standalone)
##   Figure 3b_OUD  — 2x2 payer-specific county maps
##================================================================

# ── Prepare data ─────────────────────────────────────────────────

df_f3_oud_payer <- df_dex %>%
  filter(geo == "county", year_id == 2019, acause == "mental_drug_opioids", payer != "all") %>%
  collect() %>%
  group_by(payer, state_name, location_name, fips) %>%
  summarize("value" = sum(spend_mean), .groups = "drop")

df_f3_oud_overall <- df_dex %>%
  filter(geo == "county", year_id == 2019, acause == "mental_drug_opioids", payer == "all") %>%
  collect() %>%
  group_by(state_name, location_name, fips) %>%
  summarize("value" = sum(spend_mean), .groups = "drop")

df_f3_oud_payer <- left_join(df_f3_oud_payer, df_loc_ids %>% select(mcnty, full_fips_code), by = c("fips" = "full_fips_code"))
df_f3_oud_overall <- left_join(df_f3_oud_overall, df_loc_ids %>% select(mcnty, full_fips_code), by = c("fips" = "full_fips_code"))


## ── Figure 3a oUD: All-payer county map ─────────────────────────

brks <- quantile(df_f3_oud_overall$value, probs = c(0, .2, .4, .6, .8, 1), na.rm = TRUE)
brks[1] <- brks[1] - 1
brks <- unique(brks)

labs_all <- paste0("$", format(comma(round(brks[-length(brks)])), trim = TRUE),
                   " \u2013 $", format(comma(round(brks[-1])), trim = TRUE))
labs_all[length(labs_all)] <- paste0("$", format(comma(round(brks[length(brks) - 1])), trim = TRUE), "+")

df_f3_oud_overall$plot_val <- cut(df_f3_oud_overall$value, breaks = brks, labels = labs_all, include.lowest = TRUE)

cols_5 <- colorRampPalette(c("#F1F1F1", "#B12BC9"))(5)

df_f3_oud_overall_sf <- merge(mcnty_shapefile, df_f3_oud_overall, by = "mcnty")

f3a_oud_allpayer <- build_county_map(
  map_sf_data = df_f3_oud_overall_sf,
  state_sf    = state_shapefile,
  cols        = cols_5,
  title_text  = "OUD all-payer spending by US county, 2019"
)

ggsave(file.path(dir_output, "F3a_OUD_allpayer_county_map.png"),
       plot = f3a_oud_allpayer, width = 14, height = 8, dpi = 500)


## ── Figure 3b OUD: 2x2 payer maps ──────────────────────────────

county_totals <- df_f3_oud_overall %>% select(fips, total_value = value)

f3_payer_plot_list <- list()
for (p in c("mdcr", "mdcd", "priv", "oop")) {
  message("Building map for: ", p)
  
  map_df <- df_f3_oud_payer %>%
    filter(payer == p) %>%
    left_join(county_totals, by = "fips") %>%
    mutate(pct = ifelse(total_value > 0, value / total_value * 100, 0))
  
  brks <- quantile(map_df$pct, probs = c(0, .2, .4, .6, .8, 1), na.rm = TRUE)
  brks[1] <- brks[1] - 0.01
  brks <- unique(brks)
  
  labs_p <- paste0(
    format(round(brks[-length(brks)], 1), trim = TRUE), "%",
    " \u2013 ",
    format(round(brks[-1], 1), trim = TRUE), "%"
  )
  
  map_df$plot_val <- cut(map_df$pct, breaks = brks,
                         labels = labs_p, include.lowest = TRUE)
  
  cols <- payer_colors_maps[[p]]
  payer_title <- payer_list[[p]]
  
  map_df_sf <- merge(mcnty_shapefile, map_df, by = "mcnty")
  
  map_plot <- build_county_map(
    map_sf_data = map_df_sf,
    state_sf    = state_shapefile,
    cols        = cols,
    title_text  = paste0(payer_title, " spending")
  ) +
    theme(
      legend.text      = element_text(size = 12),
      legend.key.width = unit(0.8, "cm"),
      legend.key.height = unit(0.4, "cm")
    )
  
  f3_payer_plot_list[[length(f3_payer_plot_list) + 1]] <- map_plot
}

# Combine into 2x2 grid
f3b_oud_payer_grid <- wrap_plots(f3_payer_plot_list, ncol = 2) +
  plot_annotation(
    title = "OUD spending by payer and US county, 2019 (% of all-payer total)",
    theme = theme(
      plot.title = element_text(size = 20, face = "plain", hjust = 0),
      plot.margin = margin(10, 20, 10, 20)
    )
  )

ggsave(file.path(dir_output, "F3b_oud_payer_county_maps.png"),
       plot = f3b_oud_payer_grid, width = 18, height = 14, dpi = 500)
##================================================================
## 3. MAPS — AUD  (same revised layout)
##
##   Figure 3a_AUD  — All-payer county spending (standalone)
##   Figure 3b_AUD  — 2x2 payer-specific county maps
##================================================================

# ── Prepare data ─────────────────────────────────────────────────

df_f3_aud_payer <- df_dex %>%
  filter(geo == "county", year_id == 2019, acause == "mental_alcohol", payer != "all") %>%
  collect() %>%
  group_by(payer, state_name, location_name, fips) %>%
  summarize("value" = sum(spend_mean), .groups = "drop")

df_f3_aud_overall <- df_dex %>%
  filter(geo == "county", year_id == 2019, acause == "mental_alcohol", payer == "all") %>%
  collect() %>%
  group_by(state_name, location_name, fips) %>%
  summarize("value" = sum(spend_mean), .groups = "drop")

df_f3_aud_payer <- left_join(df_f3_aud_payer, df_loc_ids %>% select(mcnty, full_fips_code), by = c("fips" = "full_fips_code"))
df_f3_aud_overall <- left_join(df_f3_aud_overall, df_loc_ids %>% select(mcnty, full_fips_code), by = c("fips" = "full_fips_code"))


## ── Figure 3a AUD: All-payer county map ─────────────────────────

brks <- quantile(df_f3_aud_overall$value, probs = c(0, .2, .4, .6, .8, 1), na.rm = TRUE)
brks[1] <- brks[1] - 1
brks <- unique(brks)

labs_all <- paste0("$", format(comma(round(brks[-length(brks)])), trim = TRUE),
                   " \u2013 $", format(comma(round(brks[-1])), trim = TRUE))
labs_all[length(labs_all)] <- paste0("$", format(comma(round(brks[length(brks) - 1])), trim = TRUE), "+")

df_f3_aud_overall$plot_val <- cut(df_f3_aud_overall$value, breaks = brks, labels = labs_all, include.lowest = TRUE)

cols_5 <- colorRampPalette(c("#F1F1F1", "#B12BC9"))(5)

df_f3_aud_overall_sf <- merge(mcnty_shapefile, df_f3_aud_overall, by = "mcnty")

f3a_aud_allpayer <- build_county_map(
  map_sf_data = df_f3_aud_overall_sf,
  state_sf    = state_shapefile,
  cols        = cols_5,
  title_text  = "AUD all-payer spending by US county, 2019"
)

ggsave(file.path(dir_output, "F3a_AUD_allpayer_county_map.png"),
       plot = f3a_aud_allpayer, width = 14, height = 8, dpi = 500)


## ── Figure 3b AUD: 2x2 payer maps ──────────────────────────────

f3_aud_plot_list <- list()
for(p in c("mdcr", "mdcd", "priv", "oop")){
  message("Building AUD map for: ", p)
  
  map_df <- df_f3_aud_payer %>% filter(payer == p)
  
  brks <- c(quantile(map_df$value, .0, na.rm = TRUE),
            quantile(map_df$value, .2, na.rm = TRUE),
            quantile(map_df$value, .4, na.rm = TRUE),
            quantile(map_df$value, .6, na.rm = TRUE),
            quantile(map_df$value, .8, na.rm = TRUE),
            quantile(map_df$value, 1, na.rm = TRUE))
  brks <- unique(brks)
  brks[1] <- brks[1] - 1
  labs_p <- paste0("$", format(comma(round(brks[-length(brks)])), trim = TRUE),
                   " \u2013 $", format(comma(round(brks[-1])), trim = TRUE))
  labs_p[length(labs_p)] <- paste0("$", format(comma(round(brks[length(brks) - 1])), trim = TRUE), "+")
  map_df$plot_val <- cut(map_df$value, breaks = brks, labels = labs_p, include.lowest = TRUE)
  
  cols <- payer_colors_maps[[p]]
  payer_title <- payer_list[[p]]
  
  map_df_sf <- merge(mcnty_shapefile, map_df, by = "mcnty")
  
  map_plot <- build_county_map(
    map_sf_data = map_df_sf,
    state_sf    = state_shapefile,
    cols        = cols,
    title_text  = paste0(payer_title, " spending")
  )
  
  f3_aud_plot_list[[length(f3_aud_plot_list) + 1]] <- map_plot
}

f3b_aud_payer_grid <- wrap_plots(f3_aud_plot_list, ncol = 2) +
  plot_annotation(
    title = "AUD spending by payer and US county, 2019",
    theme = theme(plot.title = element_text(size = 16, face = "bold"))
  )

ggsave(file.path(dir_output, "F3b_AUD_payer_county_maps.png"),
       plot = f3b_aud_payer_grid, width = 16, height = 14, dpi = 500)

pdf(file = file.path(dir_output, "F3b_AUD_payer_county_maps.pdf"), width = 16, height = 14)
print(f3b_aud_payer_grid)
dev.off()
##----------------------------------------------------------------
## 4. Figure 7 - HIV % of Population
#
# Title: Change in US national population HIV % (2019 − 2010)
#
# Basically if we pull HIV prevalence counts, and also combine that with general population data stratified 
# by age bracket and by year and by sex, we can divide the prelance counts by population to get HIV% of that 
# population for the strata, then basically plot, or put all the data in table to show over time how the HIV% population AND normal population changes
#
# Notes: 
##----------------------------------------------------------------
# Group by age, year, sex for HIV, sum prevalence counts and population
df_f7 <- df_gbd %>%
  filter(cause_name == "HIV/AIDS") %>%
  dplyr::group_by(age_group_name, year_id, sex_id) %>%
  dplyr::summarize(
    prevalence_counts = sum(prevalence_counts),
    population = sum(population))

# Create HIV % of population metric
df_f7 <- df_f7 %>%
  mutate(`hiv_%` = prevalence_counts / population)

# Create sex names
df_f7 <- df_f7 %>%
  mutate(sex_name = case_when(sex_id == 1 ~ "Male", 
                              sex_id == 2 ~ "Female"))

# Create plot
age_factor <- c("0 - <1", "1 - <5", "5 to 9", "10 to 14", "15 to 19", "20 to 24", "25 to 29", 
                "30 to 34", "35 to 39", "40 to 44", "45 to 49",  "50 to 54", 
                "55 to 59", "60 to 64", "65 to 69", "70 to 74", "75 to 79", "80 to 84", "85+")

sex_factor <- c("Male", "Female")

# 1) Compute delta (2019 - 2010) by age_group_name (optionally stratified by sex_name)
df_f7_delta <- df_f7 %>%
  filter(year_id %in% c(2010, 2019)) %>%
  mutate(
    sex_name = factor(sex_name, levels = sex_factor),
    age_group_name = factor(age_group_name, levels = age_factor)
  ) %>%
  group_by(age_group_name, sex_name, year_id) %>%
  summarise(hiv = mean(`hiv_%`, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = year_id, values_from = hiv) %>%
  mutate(delta = `2019` - `2010`)

f7_hiv_pop_percent <- ggplot(df_f7_delta, aes(x = delta, y = age_group_name, fill = delta > 0)) +
  geom_col() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_grid(rows = vars(sex_name)) +
  scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "firebrick")) +
  scale_x_continuous(labels = label_percent(accuracy = 0.01)) +
  labs(
    title = "Change in US national population HIV % (2010−2019)",
    x = "Δ HIV%",
    y = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Save plot
save_plot(f7_hiv_pop_percent, "F7_HIV_population_percent", dir_output)

##----------------------------------------------------------------
## Figure X - HIV state inefficiency ranking (between model)
##   Left: state names
##   Right: inefficiency (higher = worse)
##----------------------------------------------------------------

# Read frontier output
fp_frontier <- file.path(h, "aim_outputs/Aim2/C_frontier_analysis/20260303/hiv_between_output.csv")
df_ineff <- read.csv(fp_frontier, stringsAsFactors = FALSE, check.names = FALSE)

# Keep only what we need + clean
df_ineff <- df_ineff %>%
  transmute(
    location_name = trimws(as.character(location_name)),
    ineff = as.numeric(ineff)
  ) %>%
  filter(!is.na(location_name), !is.na(ineff)) %>%
  distinct(location_name, .keep_all = TRUE)

# --- Bin ineff into quintiles (for discrete color scheme like your maps) ---
brks <- quantile(df_ineff$ineff, probs = c(0, .2, .4, .6, .8, 1), na.rm = TRUE)
brks[1] <- brks[1] - 1e-9  # nudge for include.lowest

labs <- paste0(
  round(brks[-length(brks)], 2), " – ", round(brks[-1], 2)
)
labs[length(labs)] <- paste0(round(brks[length(brks) - 1], 2), "+")
df_ineff$ineff_bin <- cut(df_ineff$ineff, breaks = brks, labels = labs, include.lowest = TRUE)

# Palette (match your map vibe: white -> purple)
pal_fun <- colorRampPalette(c("#F1F1F1", "#B12BC9"))
cols_5  <- pal_fun(5)

# Order for plotting (best at top)
df_ineff <- df_ineff %>%
  arrange(ineff) %>%
  mutate(location_name = factor(location_name, levels = rev(location_name)))

# Plot
p_hiv_ineff_rank <- ggplot(df_ineff, aes(x = location_name, y = ineff, fill = ineff_bin)) +
  geom_col(color = "black", linewidth = 0.3) +
  coord_flip() +
  scale_fill_manual(values = cols_5, name = "Inefficiency\n(quintiles)") +
  theme_classic() +
  theme_settings +
  labs(
    title = "HIV inefficiency by state (between model)",
    x = "",
    y = "Inefficiency (higher = worse)"
  ) +
  theme(
    plot.margin = margin(t = 10, r = 30, b = 10, l = 10)
  )

# Save
save_plot(p_hiv_ineff_rank, "F_frontier_HIV_state_inefficiency_rank_between", dir_output, width = 12, height = 14)

##---------------------------------------------------------------
## 3.X Figure - HIV - State level efficiency choropleth (frontier)
##---------------------------------------------------------------

make_state_frontier_map <- function(frontier_fp,
                                    metric_col = c("eff", "ineff"),
                                    out_stub,
                                    title_text,
                                    dir_output,
                                    state_shapefile,
                                    pal = c("#F1F1F1", "#B12BC9")) {
  
  metric_col <- match.arg(metric_col)
  
  # Read frontier output
  df <- read.csv(frontier_fp, stringsAsFactors = FALSE, check.names = FALSE)
  
  # Keep + clean
  df <- df %>%
    transmute(
      state_name = trimws(as.character(location_name)),
      metric     = as.numeric(.data[[metric_col]])
    ) %>%
    filter(!is.na(state_name), !is.na(metric)) %>%
    distinct(state_name, .keep_all = TRUE)
  
  # Sanity check: state name alignment
  missing_states <- dplyr::anti_join(df, state_shapefile, by = "state_name")
  if (nrow(missing_states) > 0) {
    message("States in frontier file not found in shapefile: ",
            paste(missing_states$state_name, collapse = ", "))
  }
  if (nrow(missing_states) > 5) stop("Too many unmatched states (", nrow(missing_states), "). Check name alignment.")
  
  # Join onto sf
  map_state_df <- dplyr::left_join(state_shapefile, df, by = "state_name")
  
  # Quintile breaks
  brks <- quantile(map_state_df$metric, probs = c(0, .2, .4, .6, .8, 1), na.rm = TRUE)
  brks[1] <- brks[1] - 1e-9
  
  # Labels (2 decimals; swap if you want %)
  labs <- paste0(round(brks[-length(brks)], 2), " – ", round(brks[-1], 2))
  labs[length(labs)] <- paste0(round(brks[length(brks) - 1], 2), "+")
  
  # Bin
  map_state_df$plot_val <- cut(map_state_df$metric, breaks = brks, labels = labs, include.lowest = TRUE)
  
  # Palette
  pal_fun <- colorRampPalette(pal)
  cols_5 <- pal_fun(5)
  
  # Plot
  p <- ggplot2::ggplot(data = map_state_df) +
    ggplot2::geom_sf(ggplot2::aes(fill = plot_val, geometry = geometry),
                     color = "grey30", linewidth = 0.3) +
    ggplot2::labs(title = title_text, fill = "") +
    ggplot2::scale_fill_manual(
      values = cols_5,
      breaks = levels(map_state_df$plot_val),
      na.value = "#838484"
    ) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.justification = "center",
      legend.direction = "horizontal",
      legend.box = "horizontal",
      title = ggplot2::element_text(size = 14),
      text = ggplot2::element_text(size = 10),
      plot.margin = ggplot2::margin(0.5, 0, 1, 0, "cm"),
      axis.ticks = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank()
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1))
  
  # Save
  save_plot(p, out_stub, dir_output)
  
  message("NA ", metric_col, ": ", sum(is.na(map_state_df$metric)))
  print(table(map_state_df$plot_val, useNA = "ifany"))
  
  invisible(list(plot = p, map_df = map_state_df, data = df))
}

##---------------------------------------------------------------
## HIV efficiency map (between model)
##---------------------------------------------------------------
fp_frontier <- file.path(h, "aim_outputs/Aim2/C_frontier_analysis/20260303/hiv_between_output.csv")

res_hiv_eff_map <- make_state_frontier_map(
  frontier_fp    = fp_frontier,
  metric_col     = "ineff",  # switch to "ineff" if you want ineff instead
  out_stub       = "F_frontier_HIV_state_efficiency_between",
  title_text     = "HIV efficiency by state (between model)",
  dir_output     = dir_output,
  state_shapefile = state_shapefile
)

# Compute total prevalence by year (national example)
df_prev_totals <- df_gbd %>%
  filter(cause_name == "HIV/AIDS") %>%
  group_by(year_id) %>%
  summarise(total_prev = sum(prevalence_counts), .groups = "drop")

# Merge totals back
df_prev_shares <- df_gbd %>%
  filter(cause_name == "HIV/AIDS") %>%
  group_by(age_group_name, sex_id, year_id) %>%
  summarise(prevalence_counts = sum(prevalence_counts), .groups = "drop") %>%
  left_join(df_prev_totals, by = "year_id") %>%
  mutate(share = prevalence_counts / total_prev)



df_prev_shares %>%
  filter(year_id %in% c(2010, 2019)) %>%
  select(age_group_name, sex_id, year_id, share)


df_prev_shares %>%
  filter(year_id %in% c(2010, 2019)) %>%
  pivot_wider(names_from = year_id, values_from = share) %>%
  mutate(delta = `2019` - `2010`) %>%
  arrange(desc(delta))



df_prev_age_delta <- df_prev_shares %>%
  filter(year_id %in% c(2010, 2019)) %>%
  group_by(age_group_name, year_id) %>%
  summarise(share = sum(share), .groups = "drop") %>%
  pivot_wider(names_from = year_id, values_from = share) %>%
  mutate(delta = `2019` - `2010`) %>%
  arrange(desc(delta))


print(df_prev_age_delta, n = Inf)


ggplot(df_prev_age_delta,
       aes(x = delta,
           y = reorder(age_group_name, delta),
           fill = delta > 0)) +
  geom_col() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = c("TRUE" = "steelblue",
                               "FALSE" = "firebrick")) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 0.01)) +
  labs(title = "Change in share of total HIV cases (2019 − 2010)",
       x = "Δ Share of total HIV prevalence",
       y = NULL) +
  theme_minimal() +
  theme(legend.position = "none")

##----------------------------------------------------------------
## 4.X Figure - Change in share of total HIV cases (2019 − 2010)
##----------------------------------------------------------------

# Total prevalence by year
df_prev_totals <- df_gbd %>%
  filter(cause_name == "HIV/AIDS") %>%
  group_by(year_id) %>%
  summarise(total_prev = sum(prevalence_counts), .groups = "drop")

# Shares by age/sex/year
df_prev_shares <- df_gbd %>%
  filter(cause_name == "HIV/AIDS") %>%
  group_by(age_group_name, sex_id, year_id) %>%
  summarise(prevalence_counts = sum(prevalence_counts), .groups = "drop") %>%
  left_join(df_prev_totals, by = "year_id") %>%
  mutate(share = prevalence_counts / total_prev)

# Delta (sum over sex so it matches your shown figure)
df_prev_age_delta <- df_prev_shares %>%
  filter(year_id %in% c(2010, 2019)) %>%
  group_by(age_group_name, year_id) %>%
  summarise(share = sum(share), .groups = "drop") %>%
  pivot_wider(names_from = year_id, values_from = share) %>%
  mutate(delta = `2019` - `2010`) %>%
  arrange(delta)

# Plot
f_share_delta <- ggplot(
  df_prev_age_delta,
  aes(x = delta, y = reorder(age_group_name, delta), fill = delta > 0)
) +
  geom_col() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "firebrick")) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 0.01)) +
  labs(
    title = "Change in share of total HIV cases (2019 − 2010)",
    x = "Δ Share of total HIV prevalence",
    y = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Save
save_plot(f_share_delta, "F_share_total_HIV_cases_delta_2019_2010", dir_output, width = 16, height = 9)



##BELOW IS A CODE FOR SPEND per case useing panel. Might be needed 
#### CHECK PANEL 
##================================================================
## 1.1 Figure 1b — OUD: Two-panel figure
##     Panel A: Spending per prevalent case
##     Panel B: Prevalence per 100,000 population
##================================================================

# ── DEX all-payer OUD spending ──────────────────────────────────
df_f1b_oud_dex <- df_dex %>%
  filter(geo == "national", acause == "mental_drug_opioids",
         year_id == 2019, payer == "all") %>%
  collect() %>%
  dplyr::group_by(age_name, sex_name) %>%
  dplyr::summarise(spend_mean = sum(spend_mean, na.rm = TRUE),
                   .groups = "drop") %>%
  dplyr::mutate(age_name = stringr::str_replace_all(age_name, " ", ""))

# ── GBD OUD prevalence + population ─────────────────────────────
df_f1b_oud_gbd <- df_gbd %>%
  filter(cause_name == "Opioid use disorders", year_id == 2019) %>%
  dplyr::group_by(age_group_name, sex_id) %>%
  dplyr::summarise(
    prevalence_counts = sum(prevalence_counts, na.rm = TRUE),
    population = sum(population, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    age_name = map_gbd_to_dex_age(age_group_name),
    sex_name = dplyr::case_when(
      sex_id == 1 ~ "Male",
      sex_id == 2 ~ "Female",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(age_name), !is.na(sex_name)) %>%
  dplyr::group_by(sex_name, age_name) %>%
  dplyr::summarise(
    prevalence_counts = sum(prevalence_counts, na.rm = TRUE),
    population = sum(population, na.rm = TRUE),
    .groups = "drop"
  )

# ── Merge and compute metrics ────────────────────────────────────
df_f1b_oud <- df_f1b_oud_dex %>%
  dplyr::left_join(df_f1b_oud_gbd, by = c("age_name", "sex_name")) %>%
  dplyr::mutate(
    spend_per_case = ifelse(prevalence_counts > 0, spend_mean / prevalence_counts, NA_real_),
    prevalence_per_100k = ifelse(population > 0, prevalence_counts / population * 1e5, NA_real_)
  ) %>%
  filter(!age_name %in% c("0-<1", "1-<5", "5-<10", "10-<15"))

# ── Factor ordering ──────────────────────────────────────────────
df_f1b_oud$age_name <- factor(df_f1b_oud$age_name, levels = age_factor)
df_f1b_oud$sex_name <- factor(df_f1b_oud$sex_name, levels = sex_factor)

# ── Panel A: Spending per case ───────────────────────────────────
p_oud_spend_per_case <- ggplot(
  data = df_f1b_oud,
  aes(x = age_name, y = spend_per_case, fill = sex_name)
) +
  geom_col(width = 0.85) +
  coord_flip() +
  facet_wrap(~ sex_name, ncol = 1, scales = "free_y") +
  scale_y_continuous(
    labels = scales::dollar_format(accuracy = 100)
  ) +
  theme_classic() +
  labs(
    x = "",
    y = "Spending per prevalent case (2019 USD)",
    title = "Panel A. OUD spending per prevalent case by sex and age group, 2019"
  ) +
  theme_settings +
  theme(
    legend.position = "none",
    plot.margin = margin(t = 10, r = 20, b = 5, l = 5)
  )

# ── Panel B: Prevalence per 100k ─────────────────────────────────
p_oud_prev_100k <- ggplot(
  data = df_f1b_oud,
  aes(x = age_name, y = prevalence_per_100k, fill = sex_name)
) +
  geom_col(width = 0.85) +
  coord_flip() +
  facet_wrap(~ sex_name, ncol = 1, scales = "free_y") +
  scale_y_continuous(
    labels = scales::label_number(big.mark = ",", accuracy = 1)
  ) +
  theme_classic() +
  labs(
    x = "",
    y = "Prevalence per 100,000 population",
    title = "Panel B. OUD prevalence per 100,000 by sex and age group, 2019"
  ) +
  theme_settings +
  theme(
    legend.position = "none",
    plot.margin = margin(t = 5, r = 20, b = 10, l = 5)
  )

# ── Combine panels ───────────────────────────────────────────────
f1b_oud_two_panel <- p_oud_spend_per_case / p_oud_prev_100k +
  plot_annotation(
    title = "OUD spending per case and prevalence by sex and age group, 2019"
  )

# ── Save combined figure ─────────────────────────────────────────
ggsave(
  filename = file.path(dir_output, "F1b_OUD_spending_per_case_and_prevalence_2panel.png"),
  plot = f1b_oud_two_panel,
  width = 12,
  height = 14,
  dpi = 500,
  units = "in"
)

message("Plot saved as: ", normalizePath(file.path(dir_output, "F1b_OUD_spending_per_case_and_prevalence_2panel.png")))

### END CHECK PANEL 
