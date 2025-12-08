##----------------------------------------------------------------
##' Title: D_figures.R
##'
##' Purpose: Creates figures for Aim2
##----------------------------------------------------------------

##----------------------------------------------------------------
## 0. Clear environment and set library paths
##----------------------------------------------------------------
rm(list = ls())

pacman::p_load(dplyr, openxlsx, RMySQL, data.table, ini, DBI, tidyr, openxlsx, reticulate, ggpubr, arrow, grid, gridExtra, scales, ggplot2, shiny)
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

library(plotly)

# # Load in packages stored in repo
# user_lib <- file.path(h, "/repo/Aim2/Y_Utilities/R_Packages/")
# .libPaths(c(user_lib, .libPaths()))
# library(ggpol)
# library(tidycensus)

##----------------------------------------------------------------
## 0.1 Functions
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
## 0.2 Set directories for DEX estimate data / county estimates
##----------------------------------------------------------------
# Set path for data
date_dex <- "20251123"
fp_dex <- file.path(h, "/aim_outputs/Aim2/B_aggregation/", date_dex, "/compiled_dex_data_2010_2019.parquet")

date_ushd <- "20251123"
fp_ushd <- file.path(h, "/aim_outputs/Aim2/B_aggregation/", date_ushd, "/compiled_ushd_data_2010_2019.parquet")

date_fa <- "20251204"
fp_fa_hiv_simple <- file.path(h, "/aim_outputs/Aim2/C_frontier_analysis/", date_fa, "fa_estimates_hiv_simple.parquet")
fp_fa_hiv_extended <- file.path(h, "/aim_outputs/Aim2/C_frontier_analysis/", date_fa, "fa_estimates_hiv_extended.parquet")
fp_fa_sud_simple <- file.path(h, "/aim_outputs/Aim2/C_frontier_analysis/", date_fa, "fa_estimates__subs_simple.parquet")
fp_fa_sud_extended <- file.path(h, "/aim_outputs/Aim2/C_frontier_analysis/", date_fa, "fa_estimates__subs_extended.parquet")

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

##----------------------------------------------------------------
## 0.5 Read in data & loc_ids file & map files
##----------------------------------------------------------------
# Main DEX Data
df_dex <- read_parquet(fp_dex)

# USHD Data
#df_ushd <- read_parquet(fp_ushd)

# Frontier Analysis Data
df_hiv_fa_simple <- read_parquet(fp_fa_hiv_simple)
df_hiv_fa_extended <- read_parquet(fp_fa_hiv_extended)
df_sud_fa_simple <- read_parquet(fp_fa_sud_simple)
df_sud_fa_extended <- read_parquet(fp_fa_sud_extended)

# DEX causes that aggregate to "_subs" cause
subs_causes <- c("mental_alcohol", "mental_drug_agg", "mental_drug_opioids")

# File containing main mapping of loc_ids, fips, mcnty, etc.
df_loc_ids <- fread("/ihme/homes/idrisov/aim_outputs/Aim2/R_resources/county_fips_locs.csv")

# Shape files used for large US map plotting by county
mcnty_shapefile <- readRDS("/ihme/dex/us_county/maps/mcnty_sf_shapefile.rds")
state_shapefile <- readRDS("/ihme/dex/us_county/maps/state_sf_shapefile.rds")

##----------------------------------------------------------------
## 0.6 Join FA simple to extended data together
##----------------------------------------------------------------
# HIV
df_hiv_fa <- left_join(
  x = df_hiv_fa_simple,
  y = df_hiv_fa_extended %>% select(!c("spend_mean", "pred_mean")),
  by = c("state_name", "cnty_name", "fips_ihme", "location_id", "acause", 
         "year_id", "sex_id", "age_name_10_yr_bin")
)

rm(df_hiv_fa_extended)
rm(df_hiv_fa_simple)

# SUD
df_sud_fa <- left_join(
  x = df_sud_fa_simple,
  y = df_sud_fa_extended %>% select(!c("spend_mean", "pred_mean")),
  by = c("state_name", "cnty_name", "fips_ihme", "location_id", "acause", 
         "year_id", "sex_id", "age_name_10_yr_bin")
)

rm(df_sud_fa_extended)
rm(df_sud_fa_simple)

##----------------------------------------------------------------
## 1. Figure 1 - HIV - Spending by insurance
## What are the differences in spending for patients with HIV for each age group
## based on different types of insurance (Medicare, Medicaid, Private)? (all years, all counties)
##
## Notes: TODO calculate CI correctly, needs some research for this
##----------------------------------------------------------------

# group by: year_id, payer, cause_name
df_f1 <- df_dex %>%
  filter(acause == "hiv") %>%
  group_by(payer, age_name, sex_name) %>%
  summarize(
    "spend_mean" = mean(spend_mean)
  )

# Remove spaces from "age_name"
df_f1$age_name <- str_replace_all(df_f1$age_name, " ", "")

# Set male spending as negative
df_f1 <- df_f1 %>%
  mutate(spend_mean_inverse = ifelse(sex_name == "Male", spend_mean*-1, spend_mean))

# Create factors
df_f1$age_name <- factor(df_f1$age_name, levels = age_factor) 
df_f1$sex_name <- factor(df_f1$sex_name, levels = sex_factor) 

# Make plot 
# TODO - Have x-axis be the same for both male and female, also fix the age axis labels to make them look more neat
# basically copy from the original script, they are already typed out
# https://github.com/ihmeuw/Resource_Tracking_US_DEX/blob/main/DEX_Capstone_2025/04_figures/figure_1.R
f1 <- ggplot(data = df_f1, aes(age_name, spend_mean_inverse, fill = factor(payer, levels = c("mdcr", "mdcd", "priv", "oop")))) +
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
       title = "Estimated HIV spending per by insurance type per year, all years, all counties") +
  theme_settings +
  guides(
    fill = guide_legend(
      title.position = "top",  # put title above the keys
      title.hjust = 0.5,       # center the title
      nrow = 1                 # keep items in one row
    )) +
  geom_col(color = "black", width = 1, size = 0.3) 

# Save plot
save_plot(f1, "F1_HIV_spending_by_insurance", dir_output)


##----------------------------------------------------------------
## 2. Figure 2 - SUD - Spending by insurance
## What are the differences in spending for patients with SUD for each age group
## based on different types of insurance (Medicare, Medicaid, Private)? (all years, all counties)
##
## Notes: TODO calculate CI correctly, needs some research for this
##----------------------------------------------------------------

# group by: year_id, payer, cause_name
df_f2 <- df_dex %>%
  filter(acause %in% subs_causes) %>%
  group_by(payer, age_name, sex_name) %>%
  summarize(
    "spend_mean" = mean(spend_mean)
  )

# Remove spaces from "age_name"
df_f2$age_name <- str_replace_all(df_f2$age_name, " ", "")

# Set male spending as negative
df_f2 <- df_f2 %>%
  mutate(spend_mean_inverse = ifelse(sex_name == "Male", spend_mean*-1, spend_mean))

# Create factors
df_f2$age_name <- factor(df_f2$age_name, levels = age_factor) 
df_f2$sex_name <- factor(df_f2$sex_name, levels = sex_factor) 

# Make plot 
# TODO - Have x-axis be the same for both male and female, also fix the age axis labels to make them look more neat
# basically copy from the original script, they are already typed out
# https://github.com/ihmeuw/Resource_Tracking_US_DEX/blob/main/DEX_Capstone_2025/04_figures/figure_1.R
f2 <- ggplot(data = df_f2, aes(age_name, spend_mean_inverse, fill = factor(payer, levels = c("mdcr", "mdcd", "priv", "oop")))) +
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
save_plot(f2, "F2_SUD_spending_by_insurance", dir_output)

##----------------------------------------------------------------
## 3. Figure 3 - HIV - Spending by TOC using payer=all
## What are the differences in spending for patients with HIV for each age group based on different toc?
##
## Notes: TODO calculate CI correctly, needs some research for this, exclude NF just for HIV?
##----------------------------------------------------------------
# group by: year_id, cause_name
df_f3 <- df_dex %>%
  filter(geo == 'national') %>%
  filter(acause == "hiv") %>%
  filter(payer == "all") %>%
  group_by(toc, age_name, sex_name) %>%
  summarize(
    "spend_mean" = mean(spend_mean)
  )

# Remove spaces from "age_name"
df_f3$age_name <- str_replace_all(df_f3$age_name, " ", "")

# Set male spending as negative
df_f3 <- df_f3 %>%
  mutate(spend_mean_inverse = ifelse(sex_name == "Male", spend_mean*-1, spend_mean))

# Create factors
df_f3$age_name <- factor(df_f3$age_name, levels = age_factor) 
df_f3$sex_name <- factor(df_f3$sex_name, levels = sex_factor) 
df_f3$toc <- factor(df_f3$toc, levels = toc_factor)

# Make plot 
# TODO - Have x-axis be the same for both male and female, also fix the age axis labels to make them look more neat
# basically copy from the original script, they are already typed out
# https://github.com/ihmeuw/Resource_Tracking_US_DEX/blob/main/DEX_Capstone_2025/04_figures/figure_1.R
f3 <- ggplot(data = df_f3, aes(age_name, spend_mean_inverse, fill = toc)) +
  facet_share(~ sex_name, scales = "free", reverse_num = FALSE) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = toc_colors, labels = toc_labels, name = "Type of care") +
  scale_y_continuous(
    #limits = (),
    #breaks = seq(-10000000, 800000, 1000000),
    labels = function(x) scales::dollar(abs(x))
  ) +
  theme_classic() +
  labs(y = "Estimated Average Spending (USD)",
       x = "",
       title = "Estimated National Average HIV spending per person by type of care per year, all years, all counties") +
  theme_settings +
  guides(
    fill = guide_legend(
      title.position = "top",  # put title above the keys
      title.hjust = 0.5,       # center the title
      nrow = 1                 # keep items in one row
    )) +
  geom_col(color = "black", width = 1, size = 0.3) 

# Save plot
save_plot(f3, "F3_HIV_spending_by_toc", dir_output)


# Below is code that was trying to look at the differences between the above code written, and what is supplied

# # B's Code #
# df_test <- df_dex %>%
#   filter(geo == 'national') %>%
#   filter(acause == "hiv") %>%
#   filter(toc == "NF") %>%
#   filter(payer == "all") %>%
# 
# df_b <- df_test %>%
#   group_by(age_group_years_start, sex_id, year_id, location_name, geo) %>%
#   summarise(spend_mean = sum(spend_mean, na.rm = TRUE))
# 
# # H's Code #
# 
# # HIV spending at the national level (all counties) by age and sex
# hiv_data <- open_dataset("/mnt/share/dex/us_county/04_final/scaled_version_102/data/geo=national/toc=NF/state=USA/payer=all") %>%
#   collect() %>% as.data.table()
# 
# hiv_data <- hiv_data[acause == 'hiv']
# #hiv_data <- hiv_data[year_id == 2019]
# 
# # this is draw level, so take mean across draws first, then TAKE MEAN up by age + sex
# hiv_data_means <- hiv_data[, .(spend = mean(spend)), by = c('age_group_years_start', 'sex_id', 'year_id','location')]
# hiv_data_means <- hiv_data_means[, .(spend = mean(spend)), by = c('age_group_years_start', 'sex_id')]
# 
# f3_comp <- ggplot(hiv_data_means, aes(x = age_group_years_start, y = spend))+
#   geom_bar(stat = 'identity')+facet_grid(~sex_id)+theme_bw()+labs(x = 'year', y = 'spend', title = 'Nursing facility spend on HIV, national level all years 2010-2019')
# 
# f3_comp 



##----------------------------------------------------------------
## 4. Figure 4 - SUD - Spending by TOC
## What are the differences in spending for patients with SUD for each age group based on different toc?
##
## Notes: TODO calculate CI correctly, needs some research for this
##----------------------------------------------------------------
# group by: year_id, toc, cause_name
df_f4 <- df_dex %>%
  filter(acause %in% subs_causes) %>%
  group_by(toc, age_name, sex_name) %>%
  summarize(
    "spend_mean" = mean(spend_mean)
  )

# Remove spaces from "age_name"
df_f4$age_name <- str_replace_all(df_f4$age_name, " ", "")

# Set male spending as negative
df_f4 <- df_f4 %>%
  mutate(spend_mean_inverse = ifelse(sex_name == "Male", spend_mean*-1, spend_mean))

# Create factors
df_f4$age_name <- factor(df_f4$age_name, levels = age_factor) 
df_f4$sex_name <- factor(df_f4$sex_name, levels = sex_factor) 
df_f4$toc <- factor(df_f4$toc, levels = toc_factor)

# Make plot 
# TODO - Have x-axis be the same for both male and female, also fix the age axis labels to make them look more neat
# basically copy from the original script, they are already typed out
# https://github.com/ihmeuw/Resource_Tracking_US_DEX/blob/main/DEX_Capstone_2025/04_figures/figure_1.R
f4 <- ggplot(data = df_f4, aes(age_name, spend_mean_inverse, fill = toc)) +
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
save_plot(f4, "F4_SUD_spending_by_toc", dir_output)

##----------------------------------------------------------------
## 5. Figure 5 - HIV - USA County Plot Per Bene
## Visually, how does the spending per beneficiary look like stratified based on 
## insurance (medicare, medicaid, private insurance) when plotted by county across
## the US, all years, both sexes, all toc, for HIV? (big USA plot)
##
## Notes: TODO - fix title, possibly change how the data is cut (upper quintile is WAY too big)
##----------------------------------------------------------------

# Prepare data used for mapping

# Payer Strata by County - group by and summarize to get spend_mean (named "value" for plot)
df_f5_payer <- df_dex %>%
  filter(acause == "hiv") %>%
  group_by(payer, state_name, cnty_name, fips_ihme) %>%
  summarize(
    "value" = mean(spend_mean)
  )

# Overall Spending by County - group by and summarize to get spend_mean (named "value" for plot)
df_f5_overall <- df_dex %>%
  filter(acause == "hiv") %>%
  group_by(state_name, cnty_name, fips_ihme) %>%
  summarize(
    "value" = mean(spend_mean)
  )

# Merge with df_loc_ids to get "mcnty" column, used to merge with shapefile
df_f5_payer <- left_join(df_f5_payer, df_loc_ids %>% select(mcnty, full_fips_code), by = c("fips_ihme" = "full_fips_code")) # gets "mcnty" column used by shapefile
df_f5_overall <- left_join(df_f5_overall, df_loc_ids %>% select(mcnty, full_fips_code), by = c("fips_ihme" = "full_fips_code")) # gets "mcnty" column used by shapefile

# Maps - Payer Strata --
f5_payer_plot_list <- list()
for(p in c("mdcr", "mdcd", "priv", "oop")){
  if(length(f5_payer_plot_list) >= 4){
    f5_payer_plot_list = list()
  }
  print(p)
  
  # Filter DF by payer
  map_df <- df_f5_payer %>% filter(payer == p)
  
  # Create value breaks
  brks <- c(quantile(map_df$value, .0, na.rm = TRUE),
            quantile(map_df$value, .2, na.rm = TRUE),
            quantile(map_df$value, .4, na.rm = TRUE),
            quantile(map_df$value, .6, na.rm = TRUE),
            quantile(map_df$value, .8, na.rm = TRUE),
            quantile(map_df$value, 1, na.rm = TRUE))
  labs <- paste0("$",format(comma(round(brks[-length(brks)]))), " - $", format(comma(round(brks[-1]))))
  map_df$plot_val <- cut(map_df$value, breaks = c(brks), labels = labs)
  
  # Colors / labels for plot
  cols = payer_colors_maps[[p]]
  payer_title <- payer_list[[p]]
  if(p == "oop"){
    denom = "capita"
  } else {
    denom = "beneficiary"
  }
  
  # Merge map_df with the county shapefile
  map_df <- merge(mcnty_shapefile, map_df, by = "mcnty")
  
  # Create plot
  map <- ggplot(data = map_df)+
    geom_sf(aes(fill = plot_val, geometry = geometry), color = NA)+
    geom_sf(data = state_shapefile, fill = NA, linewidth = .4) +
   # labs(title = paste0(payer_title, " spending per ",denom),
    labs(title = paste0(payer_title, " spending"),
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
  
  # Append plot to plot list
  f5_payer_plot_list[[length(f5_payer_plot_list) + 1]] <- map
}


# Maps - Overall Spending ---

# THIS SECTION NEEDS CLEANING UP - BEGINNING  ---
# Basically the scale for the all TOC map most likely needs to be manually defined, the code in this section
# is messy and needs to be cleaned up, but maybe can come at a time when we decide on how we want the plot to look

# set breaks and labels for 7 bins
# f5_overall_brks <- sapply(seq(0, 1, by = 1/10), function(x) quantile(df_f5_overall$value, x))
# f5_overall_brks[1] <- f5_overall_brks[1]-1
# labs <- paste0("$",format(comma(round(f5_overall_brks[-length(f5_overall_brks)]))), " - $", format(comma(round(f5_overall_brks[-1]))))

manual_brks <- c(0, 1000, 5000, 10000, 20000, 50000, 100000, 500000, ceiling(max(df_f5_overall$value)))
labs <- paste0("$",format(comma(round(manual_brks[-length(manual_brks)]))), " - $", format(comma(round(manual_brks[-1]))))

# cut data into bins
df_f5_overall$plot_val <- cut(df_f5_overall$value, breaks = manual_brks, labels = labs)

#### COLORS ###
# assign colors to bins
# cols = c("#f1f1f1", "#e9d3eb", "#e0b5e4", "#d696de", "#cb77d7", "#be56d0", "#b12bc9") # static 7 colors

# cols_8 <- colorRampPalette(cols)(10) # this is linear color assignment

# Define endpoints (white → purple)
pal_fun <- colorRampPalette(c("#F1F1F1", "#B12BC9"))

# Create a bias function: low values spaced out, highs compressed into darker range
n <- 8
bias <- 1   # >1 makes it get dark faster; try 2–3
vals <- rescale((1:n)^bias, to = c(0, 1))

# Generate colors
cols_8_biased <- pal_fun(100)[round(vals * 99) + 1]
#### COLORS ###

# THIS SECTION NEEDS CLEANING UP - END  ---

# make sf object with county shapefile
df_f5_overall_map_object <- merge(mcnty_shapefile, df_f5_overall, by = "mcnty")

# create plot
f5_hiv_overall_county_map <-  ggplot(data = df_f5_overall_map_object) +
  geom_sf(aes(fill = plot_val, geometry = geometry), color = NA) + # counties w/o border
  geom_sf(data = state_shapefile, fill = NA, linewidth = .4) +
  labs(title = paste0("Estimated HIV Spending by US county, all years"),
       fill = "") +
  scale_fill_manual(values = cols_8_biased,
                    breaks = levels(factor(df_f5_overall$plot_val))[levels(factor(df_f5_overall$plot_val)) != "NA"],
                    na.value = "#838484") +
  theme(legend.position = "bottom",
        legend.justification = "center",
        legend.direction = "horizontal",
        legend.box = "horizontal",
        title = element_text(size = 12),
        text = element_text(size = 10),
        plot.margin = margin(0.5, 0, 1, 0, "cm"),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.background = element_blank()) +
  guides(fill = guide_legend(nrow = 1))

## Arranging PDF layout of maps
f5_payer_maps <- arrangeGrob(grobs = f5_payer_plot_list, nrow = 2, ncol = 2) 
f5_title_grob <- text_grob("Estimated HIV spending by US county, all years", size = 16)

# make county map a grob object to make compatible with arrangeGrob
f5_hiv_overall_county_map_grob <- ggplotGrob(f5_hiv_overall_county_map) 

# create complete layout
f5_layout <- arrangeGrob(f5_title_grob, f5_hiv_overall_county_map_grob, f5_payer_maps, nrow=3, ncol=1, heights=c(0.05, 1, 1.5))

# Save out
f5_file_name <- "F5_HIV_spending_by_county_map.pdf"
pdf(file = file.path(dir_output, f5_file_name), width = 14, height = 16)
grid.draw(f5_layout)
dev.off()

##----------------------------------------------------------------
## 6. Figure 6 - SUD - USA County Plot Per Bene
## Visually, how does the spending per beneficiary look like stratified based on 
## insurance (medicare, medicaid, private insurance) when plotted by county across
## the US, all years, both sexes, all toc, for SUD? (big USA plot)
##
## Notes: TODO - fix title, possibly change how the data is cut (upper quintile is WAY too big)
##----------------------------------------------------------------

# Prepare data used for mapping

# Payer Strata by County - group by and summarize to get spend_mean (named "value" for plot)
df_f6_payer <- df_dex %>%
  filter(acause %in% subs_causes) %>%
  group_by(payer, state_name, cnty_name, fips_ihme) %>%
  summarize(
    "value" = mean(spend_mean)
  )

# Overall Spending by County - group by and summarize to get spend_mean (named "value" for plot)
df_f6_overall <- df_dex %>%
  filter(acause %in% subs_causes) %>%
  group_by(state_name, cnty_name, fips_ihme) %>%
  summarize(
    "value" = mean(spend_mean)
  )

# Merge with df_loc_ids to get "mcnty" column, used to merge with shapefile
df_f6_payer <- left_join(df_f6_payer, df_loc_ids %>% select(mcnty, full_fips_code), by = c("fips_ihme" = "full_fips_code")) # gets "mcnty" column used by shapefile
df_f6_overall <- left_join(df_f6_overall, df_loc_ids %>% select(mcnty, full_fips_code), by = c("fips_ihme" = "full_fips_code")) # gets "mcnty" column used by shapefile

# Payer Strata by County maps
f6_payer_plot_list <- list()
for(p in c("mdcr", "mdcd", "priv", "oop")){
  if(length(f6_payer_plot_list) >= 4){
    f6_payer_plot_list = list()
  }
  print(p)
  
  # Filter DF by payer
  map_df <- df_f6_payer %>% filter(payer == p)
  
  # Create value breaks
  brks <- c(quantile(map_df$value, .0, na.rm = TRUE),
            quantile(map_df$value, .2, na.rm = TRUE),
            quantile(map_df$value, .4, na.rm = TRUE),
            quantile(map_df$value, .6, na.rm = TRUE),
            quantile(map_df$value, .8, na.rm = TRUE),
            quantile(map_df$value, 1, na.rm = TRUE))
  labs <- paste0("$",format(comma(round(brks[-length(brks)]))), " - $", format(comma(round(brks[-1]))))
  map_df$plot_val <- cut(map_df$value, breaks = c(brks), labels = labs)
  
  # Colors / labels for plot
  cols = payer_colors_maps[[p]]
  payer_title <- payer_list[[p]]
  if(p == "oop"){
    denom = "capita"
  } else {
    denom = "beneficiary"
  }
  
  # Merge map_df with the county shapefile
  map_df <- merge(mcnty_shapefile, map_df, by = "mcnty")
  
  # Create plot
  map <- ggplot(data = map_df)+
    geom_sf(aes(fill = plot_val, geometry = geometry), color = NA)+
    geom_sf(data = state_shapefile, fill = NA, linewidth = .4) +
   # labs(title = paste0(payer_title, " spending per ",denom),
    labs(title = paste0(payer_title, " spending"),
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
  f6_payer_plot_list[[length(f6_payer_plot_list) + 1]] <- map
}

# Maps - Overall Spending ---

# THIS SECTION NEEDS CLEANING UP - BEGINNING  ---
# Basically the scale for the all TOC map most likely needs to be manually defined, the code in this section
# is messy and needs to be cleaned up, but maybe can come at a time when we decide on how we want the plot to look

# Overall Spending by County
# set breaks and labels for 7 bins
# f5_overall_brks <- sapply(seq(0, 1, by = 1/10), function(x) quantile(df_f6_overall$value, x))
# f5_overall_brks[1] <- f5_overall_brks[1]-1
# labs <- paste0("$",format(comma(round(f5_overall_brks[-length(f5_overall_brks)]))), " - $", format(comma(round(f5_overall_brks[-1]))))

manual_brks <- c(0, 1000, 5000, 10000, 20000, 50000, 100000, 250000, ceiling(max(df_f6_overall$value)))
labs <- paste0("$",format(comma(round(manual_brks[-length(manual_brks)]))), " - $", format(comma(round(manual_brks[-1]))))

# cut data into bins
df_f6_overall$plot_val <- cut(df_f6_overall$value, breaks = manual_brks, labels = labs)

#### COLORS ###
# assign colors to bins
# cols = c("#f1f1f1", "#e9d3eb", "#e0b5e4", "#d696de", "#cb77d7", "#be56d0", "#b12bc9") # static 7 colors

# cols_8 <- colorRampPalette(cols)(10) # this is linear color assignment

# Define endpoints (white → purple)
pal_fun <- colorRampPalette(c("#F1F1F1", "#B12BC9"))

# Create a bias function: low values spaced out, highs compressed into darker range
n <- 8
bias <- 0.5   # >1 makes it get dark faster; try 2–3
vals <- rescale((1:n)^bias, to = c(0, 1))

# Generate colors
cols_8_biased <- pal_fun(100)[round(vals * 99) + 1]
#### COLORS ###

# THIS SECTION NEEDS CLEANING UP - END  ---
# Merge with shapefile
df_f6_overall_map_object <- merge(mcnty_shapefile, df_f6_overall, by = "mcnty")

# create plot
f6_sud_overall_county_map <-  ggplot(data = df_f6_overall_map_object) +
  geom_sf(aes(fill = plot_val, geometry = geometry), color = NA) + # counties w/o border
  geom_sf(data = state_shapefile, fill = NA, linewidth = .4) +
  labs(title = paste0("Estimated SUD Spending by US county, all years"),
       fill = "") +
  scale_fill_manual(values = cols_8_biased,
                    breaks = levels(factor(df_f6_overall$plot_val))[levels(factor(df_f6_overall$plot_val)) != "NA"],
                    na.value = "#838484") +
  theme(legend.position = "bottom",
        legend.justification = "center",
        legend.direction = "horizontal",
        legend.box = "horizontal",
        title = element_text(size = 12),
        text = element_text(size = 10),
        plot.margin = margin(0.5, 0, 1, 0, "cm"),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.background = element_blank()) +
  guides(fill = guide_legend(nrow = 1))

## Arranging PDF layout of maps
f6_payer_maps <- arrangeGrob(grobs = f6_payer_plot_list, nrow = 2, ncol = 2) 
f6_title_grob <- text_grob("Estimated SUD spending by US county, all years", size = 16)

# make county map a grob object to make compatible with arrangeGrob
f6_sud_overall_county_map_grob <- ggplotGrob(f6_sud_overall_county_map) 
f6_layout <- arrangeGrob(f6_title_grob, f6_sud_overall_county_map_grob, f6_payer_maps, nrow=3, ncol=1, heights=c(0.05, 1, 1.5))

# Save plot
f6_file_name <- "F6_SUD_spending_by_county_map.pdf"
pdf(file = file.path(dir_output, f6_file_name), width = 14, height = 16)
grid.draw(f6_layout)
dev.off()


##----------------------------------------------------------------
## 7. Figure 6 - HIV & SUD - Spaghetti Plot
## Time trend plot year by year showing efficiency values for each county as its own
## line and tracking changes and general trends over time
##
## TODO - add red average line for each state, make all the counties gray so can see the red line easily
##----------------------------------------------------------------

f6_group_cols <- c(
  "state_name", "cnty_name", "fips_ihme", "location_id", "acause")

# SUD

# Group by summary, collapse to just county*year observations
df_f6_sud <- df_sud_fa %>%
  group_by(across(all_of(c(f6_group_cols, "year_id")))) %>%
  summarize(eff_extended = mean(eff_extended), .groups = "drop")

# Pivot wider
df_f6_sud <- df_f6_sud %>%
  filter(year_id >= 2010 & year_id <= 2019) %>%
  tidyr::pivot_wider(
    names_from = year_id,
    values_from = eff_extended,
    names_prefix = "eff_"
  )

# Create percentages based on % change from 2010
df_f6_sud <- df_f6_sud %>%
  mutate(across(
    starts_with("eff_20"), 
    ~ (.x - eff_2010) / eff_2010 * 100,
    .names = "pct_{col}"
  ))

# Pivot longer so we can plot
df_f6_sud_long <- df_f6_sud %>%
  pivot_longer(
    cols = starts_with("pct_eff_"),
    names_to = "year",
    values_to = "pct_change"
  ) %>%
  mutate(
    year = as.integer(gsub("pct_eff_", "", year))
  )

# HIV

# Group by summary, collapse to just county*year observations
df_f6_hiv <- df_hiv_fa %>%
  group_by(across(all_of(c(f6_group_cols, "year_id")))) %>%
  summarize(eff_extended = mean(eff_extended), .groups = "drop")

# Pivot wider
df_f6_hiv <- df_f6_hiv %>%
  filter(year_id >= 2010 & year_id <= 2019) %>%
  tidyr::pivot_wider(
    names_from = year_id,
    values_from = eff_extended,
    names_prefix = "eff_"
  )

# Create percentages based on % change from 2010
df_f6_hiv <- df_f6_hiv %>%
  mutate(across(
    starts_with("eff_20"), 
    ~ (.x - eff_2010) / eff_2010 * 100,
    .names = "pct_{col}"
  ))

# Pivot longer so we can plot
df_f6_hiv_long <- df_f6_hiv %>%
  pivot_longer(
    cols = starts_with("pct_eff_"),
    names_to = "year",
    values_to = "pct_change"
  ) %>%
  mutate(
    year = as.integer(gsub("pct_eff_", "", year))
  )

# Plot
ggplot(df_f6_hiv_long, aes(x = year, y = pct_change, group = cnty_name)) +
  geom_line() +
  theme_minimal()


# SHINY APP to quickly compare between states
ui <- fluidPage(
  titlePanel("Efficiency % Change vs 2010 by County"),
  sidebarLayout(
    sidebarPanel(
      selectInput("state", "Select state:",
                  choices = sort(unique(df_f6_sud_long$state_name)))
    ),
    mainPanel(
      plotOutput("eff_plot", height = "600px")
    )
  )
)

server <- function(input, output, session) {
  
  df_state <- reactive({
    df_f6_sud_long %>%
      filter(state_name == input$state)
  })
  
  output$eff_plot <- renderPlot({
    ggplot(df_state(), aes(x = year, y = pct_change,
                           group = cnty_name, color = cnty_name)) +
      geom_line(alpha = 0.7) +
      theme_minimal() +
      theme(legend.position = "none")
  })
}

shiny::shinyApp(ui = ui, server = server)

## Plot ##

# SUD
ggplot(df_f6_sud_long, aes(x = year, y = pct_change, group = cnty_name)) +
  geom_line(alpha = 0.4, color = "steelblue") +
  facet_wrap(~ state_name) +
  theme_minimal() +
  labs(
    title = "SUD: % Change in Efficiency vs 2010, by County and State",
    x = "Year",
    y = "Percent change vs 2010"
  ) +
  theme(
    strip.text = element_text(size = 7),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# HIV
ggplot(df_f6_hiv_long, aes(x = year, y = pct_change, group = cnty_name)) +
  geom_line(alpha = 0.4, color = "steelblue") +
  facet_wrap(~ state_name) +
  theme_minimal() +
  labs(
    title = "HIV: % Change in Efficiency vs 2010, by County and State",
    x = "Year",
    y = "Percent change vs 2010"
  ) +
  theme(
    strip.text = element_text(size = 7),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


plot_state_spaghetti <- function(state_to_plot) {
  df_plot <- df_f6_sud_long %>%
    filter(state_name == state_to_plot)
  
  plot_ly(
    data = df_plot,
    x    = ~year,
    y    = ~pct_change,
    split = ~cnty_name,     # one line per county
    type  = 'scatter',
    mode  = 'lines',
    hoverinfo = 'text',
    text = ~paste0(
      "County: ", cnty_name, "<br>",
      "Year: ", year, "<br>",
      "Δ vs 2010: ", round(pct_change, 2), "%"
    )
  ) %>%
    layout(
      title = paste0("SUD: % Change in Efficiency vs 2010 (", state_to_plot, ")"),
      xaxis = list(title = "Year"),
      yaxis = list(title = "Percent change vs 2010"),
      showlegend = FALSE
    )
}

plot_state_spaghetti("West Virginia")

