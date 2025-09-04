##----------------------------------------------------------------
##' Title: A_Summary_Figures.R
##'
##' Purpose: Creates figures for Summary Level data for Aim2
##----------------------------------------------------------------

##----------------------------------------------------------------
## 0. Clear environment and set library paths
##----------------------------------------------------------------
rm(list = ls())

pacman::p_load(dplyr, openxlsx, RMySQL, data.table, ini, DBI, tidyr, openxlsx, reticulate, ggpubr, arrow, grid, gridExtra)
library(lbd.loader, lib.loc = sprintf("/share/geospatial/code/geospatial-libraries/lbd.loader-%s", R.version$major))
if("dex.dbr"%in% (.packages())) detach("package:dex.dbr", unload=TRUE)
library(dex.dbr, lib.loc = lbd.loader::pkg_loc("dex.dbr"))
suppressMessages(devtools::load_all(path = "/ihme/homes/idrisov/repo/dex_us_county/"))

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

# Load in packages stored in repo
user_lib <- file.path(h, "/repo/Aim2/Y_Utilities/R_Packages/")
.libPaths(c(user_lib, .libPaths()))
library(ggpol)
library(tidycensus)

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
#   "sex_id", "sex_name", 
#   "spend_mean", "spend_lower", "spend_upper", 
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
dir_output_figures <- file.path(dir_output, "A_Figures/")
dir_output_figures_dated <- file.path(dir_output, "A_Figures/", date_today)
parquet_storage <- file.path(dir_output, "Y_Utilities/parquet_storage/")

ensure_dir_exists(dir_output)
ensure_dir_exists(dir_output_figures)
ensure_dir_exists(dir_output_figures_dated)
ensure_dir_exists(parquet_storage)

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
payer_colors <- list("priv" =	"#FFCB8D", 
                     "mdcr" =	"#3188BD", 
                     "mdcd" =	"#ACDABA", 
                     "oop" =	"#D58192")

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

sex_factor <- c("Male", "Female")

toc_factor <- c("AM", "ED", "HH", "IP", "NF", "RX")

##----------------------------------------------------------------
## 0.3 Set ggplot theme
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

##----------------------------------------------------------------
## 0.4 Read in data
##----------------------------------------------------------------

################## HIV Data

# Read in saved parquet file, or read in CSV files
bool_hiv_parquet <- TRUE

if (bool_hiv_parquet) {
  # Save combined_df_sud as parquet file for faster reading, read in if trying to save time
  combined_df_hiv <- read_parquet(file.path(parquet_storage, "combined_df_hiv_county.parquet"))
  
} else if (!bool_hiv_parquet) {
  # List out HIV data files
  dirs_dex_estimates_hiv <- dirs_dex_estimates[1]
  files_hiv <- list.files(dirs_dex_estimates_hiv, full.names = TRUE)
  
  # Columns of interest
  cols_of_int_1_hiv <- c("year_id", "geo", "location_name", "fips","payer", "toc", 
                         "acause", "cause_name", "age_group_years_start", "age_name", 
                         "sex_id", "sex_name",
                         "spend_mean", "spend_lower", "spend_upper")
  # Read in CSV files
  combined_df_hiv <- rbindlist(
    lapply(
      files_hiv,
      function(f) fread(f, select = cols_of_int_1_hiv, showProgress = TRUE)
    ),
    fill = TRUE
  )
  
  # Filter for county, remove "oop" data, remove NA data, remove spending == 0
  combined_df_hiv <- combined_df_hiv %>%
    filter(geo == "county") %>%
    filter(payer != "oop") %>%
    filter(!is.na(spend_mean)) %>%
    filter(spend_mean > 0)
  
  # Save combined_df_hiv as parquet file for faster reading, read in if trying to save time
  write_parquet(combined_df_hiv, file.path(parquet_storage, "combined_df_hiv_county.parquet"))
}

################## SUD Data

# Read in saved parquet file, or read in CSV files
bool_sud_parquet <- TRUE

if (bool_sud_parquet) {
  # Save combined_df_sud as parquet file for faster reading, read in if trying to save time
  combined_df_sud <- read_parquet(file.path(parquet_storage, "combined_df_sud_county.parquet"))
  
} else if (!bool_sud_parquet) {
  # List out SUD data files
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
  write_parquet(combined_df_sud, file.path(parquet_storage, "combined_df_sud_county.parquet"))
}

##----------------------------------------------------------------
## 0.5 Create FIPS code lookup table
##
## This is needed as we need to know the state from which each county
## belongs to, and the FIPS codes in the data are missing a left padded "0"
## which needs to be added back in to match on the state_name
##----------------------------------------------------------------
df_fips_lookup <- fips_codes

df_fips_lookup <- df_fips_lookup %>%
  mutate(full_fips_code = paste0(state_code, county_code))

##----------------------------------------------------------------
## 1. Figure 1 - HIV
## What are the differences in spending for patients with HIV for each age group
## based on different types of insurance (Medicare, Medicaid, Private)? (all years, all counties)
##
## Notes: TODO calculate CI correctly, needs some research for this
##----------------------------------------------------------------

# group by: year_id, payer, cause_name
df_hiv <- combined_df_hiv %>%
  group_by(payer, age_name, sex_name) %>%
  summarize(
    "spend_mean" = mean(spend_mean)
  )

# Remove spaces from "age_name"
df_hiv$age_name <- str_replace_all(df_hiv$age_name, " ", "")

# Set male spending as negative
df_hiv <- df_hiv %>%
  mutate(spend_mean_inverse = ifelse(sex_name == "Male", spend_mean*-1, spend_mean))

# Create factors
df_hiv$age_name <- factor(df_hiv$age_name, levels = age_factor) 
df_hiv$sex_name <- factor(df_hiv$sex_name, levels = sex_factor) 

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
  theme_settings +
  guides(
    fill = guide_legend(
      title.position = "top",  # put title above the keys
      title.hjust = 0.5,       # center the title
      nrow = 1                 # keep items in one row
    )) +
  geom_col(color = "black", width = 1, size = 0.3) 

# Save plot
save_plot(f1, "F1", dir_output_figures_dated)


##----------------------------------------------------------------
## 2. Figure 2 - SUD
## What are the differences in spending for patients with SUD for each age group
## based on different types of insurance (Medicare, Medicaid, Private)? (all years, all counties)
##
## Notes: TODO calculate CI correctly, needs some research for this
##----------------------------------------------------------------

# group by: year_id, payer, cause_name
df_sud <- combined_df_sud %>%
  group_by(payer, age_name, sex_name) %>%
  summarize(
    "spend_mean" = mean(spend_mean)
  )

# Remove spaces from "age_name"
df_sud$age_name <- str_replace_all(df_sud$age_name, " ", "")

# Set male spending as negative
df_sud <- df_sud %>%
  mutate(spend_mean_inverse = ifelse(sex_name == "Male", spend_mean*-1, spend_mean))

# Create factors
df_sud$age_name <- factor(df_sud$age_name, levels = age_factor) 
df_sud$sex_name <- factor(df_sud$sex_name, levels = sex_factor) 

# Make plot 
# TODO - Have x-axis be the same for both male and female, also fix the age axis labels to make them look more neat
# basically copy from the original script, they are already typed out
# https://github.com/ihmeuw/Resource_Tracking_US_DEX/blob/main/DEX_Capstone_2025/04_figures/figure_1.R
f2 <- ggplot(data = df_sud, aes(age_name, spend_mean_inverse, fill = factor(payer, levels = c("mdcr", "mdcd", "priv")))) +
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
  theme_settings +
  guides(
    fill = guide_legend(
      title.position = "top",  # put title above the keys
      title.hjust = 0.5,       # center the title
      nrow = 1                 # keep items in one row
    )) +
  geom_col(color = "black", width = 1, size = 0.3) 

# Save plot
save_plot(f2, "F2", dir_output_figures_dated)

##----------------------------------------------------------------
## 3. Figure 3 - HIV
## What are the differences in spending for patients with HIV for each age group based on different toc?
##
## Notes: TODO calculate CI correctly, needs some research for this, exclude NF just for HIV?
##----------------------------------------------------------------
# group by: year_id, toc, cause_name
df_f3_hiv <- combined_df_hiv %>%
  group_by(toc, age_name, sex_name) %>%
  summarize(
    "spend_mean" = mean(spend_mean)
  )

# Remove spaces from "age_name"
df_f3_hiv$age_name <- str_replace_all(df_f3_hiv$age_name, " ", "")

# Set male spending as negative
df_f3_hiv <- df_f3_hiv %>%
  mutate(spend_mean_inverse = ifelse(sex_name == "Male", spend_mean*-1, spend_mean))

# Create factors
df_f3_hiv$age_name <- factor(df_f3_hiv$age_name, levels = age_factor) 
df_f3_hiv$sex_name <- factor(df_f3_hiv$sex_name, levels = sex_factor) 
df_f3_hiv$toc <- factor(df_f3_hiv$toc, levels = toc_factor)

# Make plot 
# TODO - Have x-axis be the same for both male and female, also fix the age axis labels to make them look more neat
# basically copy from the original script, they are already typed out
# https://github.com/ihmeuw/Resource_Tracking_US_DEX/blob/main/DEX_Capstone_2025/04_figures/figure_1.R
f3 <- ggplot(data = df_f3_hiv, aes(age_name, spend_mean_inverse, fill = toc)) +
  facet_share(~ sex_name, scales = "free", reverse_num = FALSE) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = toc_colors, labels = toc_labels, name = "Type of care") +
  scale_y_continuous(
    #limits = (),
    breaks = seq(-10000000, 800000, 1000000),
    labels = function(x) scales::dollar(abs(x))
  ) +
  theme_classic() +
  labs(y = "Estimated Average Spending (USD)",
       x = "",
       title = "Estimated HIV spending by type of care per year, all years, all counties") +
  theme_settings +
  guides(
    fill = guide_legend(
      title.position = "top",  # put title above the keys
      title.hjust = 0.5,       # center the title
      nrow = 1                 # keep items in one row
    )) +
  geom_col(color = "black", width = 1, size = 0.3) 

# Save plot
save_plot(f3, "F3", dir_output_figures_dated)

##----------------------------------------------------------------
## 4. Figure 4 - SUD
## What are the differences in spending for patients with SUD for each age group based on different toc?
##
## Notes: TODO calculate CI correctly, needs some research for this
##----------------------------------------------------------------
# group by: year_id, toc, cause_name
df_f4_sud <- combined_df_sud %>%
  group_by(toc, age_name, sex_name) %>%
  summarize(
    "spend_mean" = mean(spend_mean)
  )

# Remove spaces from "age_name"
df_f4_sud$age_name <- str_replace_all(df_f4_sud$age_name, " ", "")

# Set male spending as negative
df_f4_sud <- df_f4_sud %>%
  mutate(spend_mean_inverse = ifelse(sex_name == "Male", spend_mean*-1, spend_mean))

# Create factors
df_f4_sud$age_name <- factor(df_f4_sud$age_name, levels = age_factor) 
df_f4_sud$sex_name <- factor(df_f4_sud$sex_name, levels = sex_factor) 
df_f4_sud$toc <- factor(df_f4_sud$toc, levels = toc_factor)

# Make plot 
# TODO - Have x-axis be the same for both male and female, also fix the age axis labels to make them look more neat
# basically copy from the original script, they are already typed out
# https://github.com/ihmeuw/Resource_Tracking_US_DEX/blob/main/DEX_Capstone_2025/04_figures/figure_1.R
f4 <- ggplot(data = df_f4_sud, aes(age_name, spend_mean_inverse, fill = toc)) +
  facet_share(~ sex_name, scales = "free", reverse_num = FALSE) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = toc_colors, labels = toc_labels, name = "Type of care") +
  scale_y_continuous(
    #limits = (),
    breaks = seq(-500000, 500000, 100000),
    labels = function(x) scales::dollar(abs(x))
  ) +
  theme_classic() +
  labs(y = "Estimated Average Spending (USD)",
       x = "",
       title = "Estimated SUD spending by type of care per year, all years, all counties") +
  theme_settings +
  guides(
    fill = guide_legend(
      title.position = "top",  # put title above the keys
      title.hjust = 0.5,       # center the title
      nrow = 1                 # keep items in one row
    )) +
  geom_col(color = "black", width = 1, size = 0.3) 

# Save plot
save_plot(f4, "F4", dir_output_figures_dated)

##----------------------------------------------------------------
## 5. Figure 5 - HIV
## Visually, how does the spending per beneficiary look like stratified based on 
## insurance (medicare, medicaid, private insurance) when plotted by county across
## the US, all years, both sexes, all toc, for HIV? (big USA plot)
##
## Notes: TODO - fix title, possibly change how the data is cut (upper quintile is WAY too big), also add the average spending per county (all insurances)
##----------------------------------------------------------------
# Fix FIPS codes 
df_f5_hiv <- combined_df_hiv %>%
  mutate(
    fips = as.character(fips),             # step 1: convert to character
    fips = ifelse(nchar(fips) == 4,    # step 2: pad 4-digit strings
                  paste0("0", fips),
                  fips)
  ) 

df_f5_hiv <- left_join(x = df_f5_hiv, y = df_fips_lookup, by = c("fips" = "full_fips_code"))

# group by: year_id, toc, cause_name
df_f5_hiv <- df_f5_hiv %>%
  group_by(payer, state_name, location_name, fips) %>%
  summarize(
    "spend_mean" = mean(spend_mean)
  )

plot_data <- copy(df_f5_hiv) %>% 
  as.data.table() %>%
  mutate(value = spend_mean)

# read in county_names (mcnty -> fips)
county_names <- fread("/mnt/share/dex/us_county/maps/merged_counties.csv")[, .(mcnty, fips = cnty)]
county_names <- county_names %>%
  mutate(
    fips = as.character(fips),             # step 1: convert to character
    fips = ifelse(nchar(fips) == 4,    # step 2: pad 4-digit strings
                  paste0("0", fips),
                  fips)
  ) 

# merge with plot_data
plot_data <- left_join(x = plot_data, y = county_names, by = "fips")

# Shape files used for large US map plotting by county
mcnty_shapefile <- readRDS("/ihme/dex/us_county/maps/mcnty_sf_shapefile.rds")
state_shapefile <- readRDS("/ihme/dex/us_county/maps/state_sf_shapefile.rds")

# Spend per beneficiary maps
plot_list <- list()
for(p in c("mdcr", "mdcd", "priv")){
  if(length(plot_list) >= 3){
    plot_list = list()
  }
  print(p)
  map_df <- plot_data[payer == p]
  
  brks <- c(quantile(map_df$value, .0, na.rm = TRUE),
            quantile(map_df$value, .2, na.rm = TRUE),
            quantile(map_df$value, .4, na.rm = TRUE),
            quantile(map_df$value, .6, na.rm = TRUE),
            quantile(map_df$value, .8, na.rm = TRUE),
            quantile(map_df$value, 1, na.rm = TRUE))
  labs <- paste0("$",format(comma(round(brks[-length(brks)]))), " - $", format(comma(round(brks[-1]))))
  
  
  map_df$plot_val <- cut(map_df$value, breaks = c(brks), labels = labs)
  cols = payer_colors_maps[[p]]
  payer_title <- payer_list[[p]]
  if(p == "oop"){
    denom = "capita"
  } else {
    denom = "beneficiary"
  }
  
  #make sf object with county shapefile
  map_df <- merge(mcnty_shapefile, map_df, by = "mcnty")
  
  map <- ggplot(data = map_df)+
    geom_sf(aes(fill = plot_val, geometry = geometry), color = NA)+
    geom_sf(data = state_shapefile, fill = NA, linewidth = .4) +
    labs(title = paste0(payer_title, " spending per ",denom),
         fill = "") +
    scale_fill_manual(values = cols, 
                      breaks = levels(factor(map_df$plot_val))[levels(factor(map_df$plot_val)) != "NA"],
                      na.value = "#838484")+
    theme(legend.position = "bottom",
          legend.justification = "top",
          legend.margin = margin(t = -10, unit = "pt"),  # Adjust top margin of the legend to pull it closer
          legend.spacing.y = unit(0, "cm"),
          plot.margin = margin(0, 0, 1, 0, "cm"),
          title = element_text(size = 12),
          text = element_text(size = 10),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          panel.background = element_blank()) 
  plot_list[[length(plot_list) + 1]] <- map
}

## Arranging PDF layout of maps
payer_maps <- arrangeGrob(grobs = plot_list, nrow = 2, ncol = 2) 
title_grob <- text_grob("Figure 2. Age/sex standardized health care spending per beneficiary by US county in 2019", size = 16)

# make county map a grob object to make compatible with arrangeGrob
county_map_grob <- ggplotGrob(county_map)

layout <- arrangeGrob(title_grob, payer_maps, nrow=3, ncol=1, heights=c(0.05, 1, 1.5))

# Save out
file_name <- "Figure_5_test_hiv.pdf"
full_path <- file.path(dir_output_figures_dated, file_name)

pdf(file = full_path, width = 14, height = 16)
grid.draw(layout)
dev.off()



### REFERENCE CODE ### 




df_high_hiv <- combined_df_hiv %>%
  filter(age_name == "60 - <65") %>%
  filter(toc == "NF")


# # layout - to cleanly add title to top of figure
# pyramids <- arrangeGrob(grobs = list(pyramid1, pyramid2), nrow = 1, ncol = 2) 
# title_grob <- text_grob("Figure 1. Age pyramids of total spending by payer and spending per capita by type of care in 2019", size = 16)
# layout <- arrangeGrob(title_grob, pyramids, nrow=2, ncol=1, heights=c(0.1, 1))
