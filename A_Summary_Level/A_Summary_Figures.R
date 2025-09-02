##----------------------------------------------------------------
##' Title: A_Summary_Figures.R
##'
##' Purpose: Creates figures for Summary Level data for Aim2
##----------------------------------------------------------------

##----------------------------------------------------------------
## 0. Clear environment and set library paths
##----------------------------------------------------------------
rm(list = ls())

pacman::p_load(dplyr, openxlsx, RMySQL, data.table, ini, DBI, tidyr, openxlsx, reticulate, ggpubr, arrow)
library(lbd.loader, lib.loc = sprintf("/share/geospatial/code/geospatial-libraries/lbd.loader-%s", R.version$major))
if("dex.dbr"%in% (.packages())) detach("package:dex.dbr", unload=TRUE)
library(dex.dbr, lib.loc = lbd.loader::pkg_loc("dex.dbr"))
suppressMessages(devtools::load_all(path = "/ihme/homes/idrisov/repo/dex_us_county/"))

# Load in packages stored in repo
user_lib <- file.path(h, "/repo/Aim2/Y_Utilities/R_Packages/")
.libPaths(c(user_lib, .libPaths()))
library(ggpol)

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

save_plot <- function(ggplot_obj, ggplot_name, output_path, width = 12, height = 10, dpi = 500, path = ".") {
  
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

##----------------------------------------------------------------
## 0.1 Set directories for DEX estimate data / county estimates
##----------------------------------------------------------------
# List out directories
fp_dex_estimates <- file.path(h, "/county_data/")
dirs_dex_estimates <- list.dirs(fp_dex_estimates, recursive = TRUE)[-1]

# # Available columns
# c("year_id", "geo", "location_name", "fips", "payer", "toc", 
#   "acause", "cause_name", "age_group_years_start", "age_name", 
#   "sex_id", "sex_name", "spend_mean", "spend_lower", "spend_upper", 
#   "spend_per_capita_mean", "spend_per_capita_lower", "spend_per_capita_upper", 
#   "spend_per_bene_mean", "spend_per_bene_lower", "spend_per_bene_upper", 
#   "spend_per_vol_mean", "spend_per_vol_lower", "spend_per_vol_upper", 
#   "vol_per_capita_mean", "vol_per_capita_lower", "vol_per_capita_upper", 
#   "vol_per_bene_mean", "vol_per_bene_lower", "vol_per_bene_upper"
# )

# Set the current date for folder naming
date_today <- format(Sys.time(), "%Y%m%d")

# Set output directories
dir_output <- "/mnt/share/scratch/users/idrisov/Aim2_Outputs/"
dir_output_figures <- file.path(dir_output, "A_Figures/", date_today)

ensure_dir_exists(dir_output)
ensure_dir_exists(dir_output_figures)

##----------------------------------------------------------------
## 0.2 Set colors and labels for plots
##----------------------------------------------------------------

# colors and labels to use 
# Use full payer name in plot titles and labels
payer_list <- list("mdcr" = "Medicare", 
                   "mdcd" = "Medicaid", 
                   "priv" = "Private Insurance", 
                   "oop" = "Out-of-Pocket")

# payer colors
payer_colors <- list("priv" =	"#E69F00", 
                     "mdcr" =	"#0E5EAE", 
                     "mdcd" =	"#009E73", 
                     "oop" =	"#ae3918")

#colors for type of care
toc_colors <- c(
  "ED" = "#9B110E",
  "AM" = "#0B775E",
  "HH" = "#F1B9B9",
  "IP" = "#35274A",
  "NF" = "#6e9ab5",
  "DV" = "#E69F00",
  "RX" = "#BC87E4")

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

##----------------------------------------------------------------
## 1. Figure 1 - HIV
## What are the differences in spending per beneficiary for patients with HIV 
## for each age group based on different types of insurance (Medicare, Medicaid, Private)? 
## (all years, all counties)
##
## Notes: TODO calculate CI correctly, needs some research for this
##----------------------------------------------------------------

# List out HIV data files
dirs_dex_estimates_hiv <- dirs_dex_estimates[1]
files_hiv <- list.files(dirs_dex_estimates_hiv, full.names = TRUE)

# Columns of interest
cols_of_int_1_hiv <- c("year_id", "geo", "location_name", "fips","payer", "toc", 
                       "acause", "cause_name", "age_group_years_start", "age_name", 
                       "sex_id", "sex_name",
                       "spend_mean", "spend_lower", "spend_upper")

# Read in HIV CSV files
combined_df_hiv <- rbindlist(
  lapply(
    files_hiv,
    function(f) fread(f, select = cols_of_int_1_hiv, showProgress = FALSE)
  ),
  fill = TRUE
)

# Filter for county, remove "oop" data, remove NA data, remove spending == 0
combined_df_hiv <- combined_df_hiv %>%
  filter(geo == "county") %>%
  filter(payer != "oop") %>%
  filter(!is.na(spend_mean)) %>%
  filter(spend_mean > 0)

# group by: year_id, payer, cause_name
df_hiv <- combined_df_hiv %>%
  group_by(payer, age_name, sex_name) %>%
  summarize(
    "spend_mean" = mean(spend_mean)
  )

# drop combined_df_hiv to save data
rm(combined_df_hiv)

# convert to dollars
for (col in colnames(df_hiv)) {
  if (col == "spend_mean") {
    df_hiv[[col]] <- dollar(df_hiv[[col]])
  }
}

# Create factors
age_factor <- c("0 - <1", "1 - <5", "5 - <10", "10 - <15", "15 - <20", "20 - <25", "25 - <30", 
                "30 - <35", "35 - <40", "40 - <45", "45 - <50",  "50 - <55", 
                "55 - <60", "60 - <65", "65 - <70", "70 - <75", "75 - <80", "80 - <85", 
                "85+")
df_hiv$age_name <- factor(df_hiv$age_name,
                          levels = age_factor) 

sex_factor <- c("Male", "Female")
df_hiv$sex_name <- factor(df_hiv$sex_name,
                          levels = sex_factor) 

# Set male spending as negative
df_hiv <- df_hiv %>%
  mutate(spend_mean_inverse = ifelse(sex_name == "Male", spend_mean*-1, spend_mean))


# Make plot 
# TODO - Have x-axis be the same for both male and female, also fix the age axis labels to make them look more neat
# basically copy from the original script, they are already typed out
# https://github.com/ihmeuw/Resource_Tracking_US_DEX/blob/main/DEX_Capstone_2025/04_figures/figure_1.R
f1 <- ggplot(data = df_hiv, aes(age_name, spend_mean_inverse, fill = factor(payer, levels = c("mdcr", "mdcd", "priv")))) +
  facet_share(~ sex_name, scales = "free", reverse_num = FALSE) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = payer_colors, labels = payer_list, name = "Payer") +
  scale_y_continuous(
    #limits = (),
    breaks = seq(-800000, 800000, 200000),
    labels = function(x) scales::dollar(abs(x))
    ) +
  theme_classic() +
  labs(y = "Estimated Average Spending (USD)",
       x = "",
       title = "Estimated HIV spending by insurance type per year, all years, all counties") +
  theme(#axis.title.y = element_blank(),
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
        ) +
  guides(
    fill = guide_legend(
      title.position = "top",  # put title above the keys
      title.hjust = 0.5,       # center the title
      nrow = 1                 # keep items in one row
    )) +
  geom_col(color = "black", width = 1, size = 0.3) 

# Save plot
save_plot(f1, "F1", dir_output_figures)


##----------------------------------------------------------------
## 2. Figure 2 - SUD
## What are the differences in spending per beneficiary for patients with SUD 
## for each age group based on different types of insurance (Medicare, Medicaid, Private)? 
## (all years, all counties)
##
## Notes: TODO calculate CI correctly, needs some research for this
##----------------------------------------------------------------

# List out HIV data files
dirs_dex_estimates_sud <- dirs_dex_estimates[2:4]
files_sud <- list.files(dirs_dex_estimates_sud, full.names = TRUE)

# Columns of interest
cols_of_int_2_sud <- c("year_id", "geo", "location_name", "fips","payer", "toc", 
                       "acause", "cause_name", "age_group_years_start", "age_name", 
                       "sex_id", "sex_name",
                       "spend_mean", "spend_lower", "spend_upper")

# Read in CSV files
combined_df_sud <- rbindlist(
  lapply(
    files_sud,
    function(f) fread(f, select = cols_of_int_2_sud, showProgress = FALSE)
  ),
  fill = TRUE
)

# Filter for county, remove "oop" data, remove NA data, remove spending == 0
combined_df_sud <- combined_df_sud %>%
  filter(geo == "county") %>%
  filter(payer != "oop") %>%
  filter(!is.na(spend_mean)) %>%
  filter(spend_mean > 0)

# Save combined_df_sud as parquet file for faster reading, read in if trying to save time
write_parquet(combined_df_sud, file.path(dir_output_figures, "combined_df_sud.parquet"))
combined_df_sud <- read_parquet(file.path(dir_output_figures, "combined_df_sud.parquet"))

# group by: year_id, payer, cause_name
df_sud <- combined_df_sud %>%
  group_by(payer, age_name, sex_name) %>%
  summarize(
    "spend_mean" = mean(spend_mean)
  )

# drop combined_df_sud to save data
# rm(combined_df_sud)

# Create factors
age_factor <- c("0 - <1", "1 - <5", "5 - <10", "10 - <15", "15 - <20", "20 - <25", "25 - <30", 
                "30 - <35", "35 - <40", "40 - <45", "45 - <50",  "50 - <55", 
                "55 - <60", "60 - <65", "65 - <70", "70 - <75", "75 - <80", "80 - <85", 
                "85+")
df_sud$age_name <- factor(df_sud$age_name,
                          levels = age_factor) 

sex_factor <- c("Male", "Female")
df_sud$sex_name <- factor(df_sud$sex_name,
                          levels = sex_factor) 

# Set male spending as negative
df_sud <- df_sud %>%
  mutate(spend_mean_inverse = ifelse(sex_name == "Male", spend_mean*-1, spend_mean))


# Make plot 
# TODO - Have x-axis be the same for both male and female, also fix the age axis labels to make them look more neat
# basically copy from the original script, they are already typed out
# https://github.com/ihmeuw/Resource_Tracking_US_DEX/blob/main/DEX_Capstone_2025/04_figures/figure_1.R
ggplot(data = df_sud, aes(age_name, spend_mean_inverse, fill = factor(payer, levels = c("mdcr", "mdcd", "priv")))) +
  facet_share(~ sex_name, scales = "free", reverse_num = FALSE) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = payer_colors, labels = payer_list, name = "Payer") +
  scale_y_continuous(
    #limits = (),
    breaks = seq(-400000, 400000, 50000),
    labels = function(x) scales::dollar(abs(x))
  ) +
  theme_classic() +
  labs(y = "Estimated Average Spending (USD)",
       x = "",
       title = "Estimated SUD spending by insurance type per year, all years, all counties") +
  theme(#axis.title.y = element_blank(),
    text = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 14),
    plot.subtitle = element_text(size = 12),
    strip.text = element_text(size = 14),
    panel.grid.major.x = element_line(color = "grey70", size = 0.5), # thicker lines
    legend.position = "top",               # move legend above plot
    legend.box = "horizontal",             # arrange items horizontally
    legend.background = element_rect(      # put it in a box
      color = "black", fill = "white", 
      linewidth = 0.5, linetype = "solid"),
    legend.key = element_rect(fill = "white"),
    plot.margin = margin(t = 5, r = 5, b = 5, l = -50)
  ) +
  guides(
    fill = guide_legend(
      title.position = "top",  # put title above the keys
      title.hjust = 0.5,       # center the title
      nrow = 1                 # keep items in one row
    )) +
  geom_col(color = "black", width = 0.8, size = 0.3) 

# Save plot
save_plot(f1, "F1", dir_output_figures)


### REFERENCE


# # layout - to cleanly add title to top of figure
# pyramids <- arrangeGrob(grobs = list(pyramid1, pyramid2), nrow = 1, ncol = 2) 
# title_grob <- text_grob("Figure 1. Age pyramids of total spending by payer and spending per capita by type of care in 2019", size = 16)
# layout <- arrangeGrob(title_grob, pyramids, nrow=2, ncol=1, heights=c(0.1, 1))
