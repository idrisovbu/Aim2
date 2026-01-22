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

##----------------------------------------------------------------
## 0.2 Set directories for DEX estimate data / county estimates
##----------------------------------------------------------------
# Set path for data
date_dex <- "20260120"
fp_dex <- file.path(h, "/aim_outputs/Aim2/B_aggregation/", date_dex, "/compiled_dex_data_2010_2019.parquet")

date_ushd <- "20251204"
fp_ushd <- file.path(h, "/aim_outputs/Aim2/B_aggregation/", date_ushd, "/compiled_ushd_data_2010_2019.parquet")

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

# USHD Data - UNUSED ATM
#df_ushd <- read_parquet(fp_ushd)

# DEX causes that aggregate to "_subs" cause
subs_causes <- c("mental_alcohol", "mental_drug_agg", "mental_drug_opioids")

# File containing main mapping of loc_ids, fips, mcnty, etc.
df_loc_ids <- fread("/ihme/homes/idrisov/aim_outputs/Aim2/R_resources/county_fips_locs.csv")

# Shape files used for large US map plotting by county
mcnty_shapefile <- readRDS("/ihme/dex/us_county/maps/mcnty_sf_shapefile.rds")
state_shapefile <- readRDS("/ihme/dex/us_county/maps/state_sf_shapefile.rds")

##----------------------------------------------------------------
## 1. Figure 1 - Spending by insurance
## What are the differences in spending for patients with HIV / SUD for each age group
## based on different types of insurance (Medicare, Medicaid, Private)? (all years, all counties)
##
## Notes: TODO calculate CI correctly, needs some research for this
##----------------------------------------------------------------

# HIV #

# Collapse on TOC, location (which is all national), year
df_f1_hiv <- df_dex %>%
  filter(geo == "national") %>%
  filter(acause == "hiv") %>%
  filter(payer != "all") %>%
  group_by(payer, age_name, sex_name) %>%
  summarize(
    "spend_mean" = mean(spend_mean)
  )

# Remove spaces from "age_name"
df_f1_hiv$age_name <- str_replace_all(df_f1_hiv$age_name, " ", "")

# Set male spending as negative
df_f1_hiv <- df_f1_hiv %>%
  mutate(spend_mean_inverse = ifelse(sex_name == "Male", spend_mean*-1, spend_mean))

# Create factors
df_f1_hiv$age_name <- factor(df_f1_hiv$age_name, levels = age_factor) 
df_f1_hiv$sex_name <- factor(df_f1_hiv$sex_name, levels = sex_factor) 

# Make plot 
# TODO - Have x-axis be the same for both male and female, also fix the age axis labels to make them look more neat
# basically copy from the original script, they are already typed out
# https://github.com/ihmeuw/Resource_Tracking_US_DEX/blob/main/DEX_Capstone_2025/04_figures/figure_1.R
f1_hiv <- ggplot(data = df_f1_hiv, aes(age_name, spend_mean_inverse, fill = factor(payer, levels = c("mdcr", "mdcd", "priv", "oop")))) +
  facet_share(~ sex_name, scales = "free", reverse_num = FALSE) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = payer_colors, labels = payer_list, name = "Payer") +
  scale_y_continuous(
    #limits = (),
    #breaks = seq(-800000, 800000, 200000),
    labels = function(x) scales::dollar(abs(x))
    ) +
  theme_classic() +
  labs(y = "Inflation Adjusted Spending (2019 USD)",
       x = "",
       title = "Mean annual HIV spending for each insurance type by sex and age group, 2010 - 2019") +
  theme_settings +
  guides(
    fill = guide_legend(
      title.position = "top",  # put title above the keys
      title.hjust = 0.5,       # center the title
      nrow = 1                 # keep items in one row
    )) +
  geom_col(color = "black", width = 1, size = 0.3) 

# Save plot
save_plot(f1_hiv, "F1_HIV_spending_by_insurance", dir_output)

# SUD #

# Collapse on TOC, location (which is all national), year
df_f1_sud <- df_dex %>%
  filter(geo == "national") %>%
  filter(acause %in% subs_causes) %>%
  filter(payer != "all") %>%
  group_by(payer, age_name, sex_name) %>%
  summarize(
    "spend_mean" = mean(spend_mean)
  )

# Remove spaces from "age_name"
df_f1_sud$age_name <- str_replace_all(df_f1_sud$age_name, " ", "")

# Set male spending as negative
df_f1_sud <- df_f1_sud %>%
  mutate(spend_mean_inverse = ifelse(sex_name == "Male", spend_mean*-1, spend_mean))

# Create factors
df_f1_sud$age_name <- factor(df_f1_sud$age_name, levels = age_factor) 
df_f1_sud$sex_name <- factor(df_f1_sud$sex_name, levels = sex_factor) 

# Make plot 
# TODO - Have x-axis be the same for both male and female, also fix the age axis labels to make them look more neat
# basically copy from the original script, they are already typed out
# https://github.com/ihmeuw/Resource_Tracking_US_DEX/blob/main/DEX_Capstone_2025/04_figures/figure_1.R
f1_sud <- ggplot(data = df_f1_sud, aes(age_name, spend_mean_inverse, fill = factor(payer, levels = c("mdcr", "mdcd", "priv", "oop")))) +
  facet_share(~ sex_name, scales = "free", reverse_num = FALSE) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = payer_colors, labels = payer_list, name = "Payer") +
  scale_y_continuous(
    #limits = (),
    #breaks = seq(-400000, 400000, 50000),
    labels = function(x) scales::dollar(abs(x))
  ) +
  theme_classic() +
  labs(y = "Inflation Adjusted Spending (2019 USD)",
       x = "",
       title = "Mean annual SUD spending for each insurance type by sex and age group, 2010 - 2019") +
  theme_settings +
  guides(
    fill = guide_legend(
      title.position = "top",  # put title above the keys
      title.hjust = 0.5,       # center the title
      nrow = 1                 # keep items in one row
    )) +
  geom_col(color = "black", width = 1, size = 0.3) 

# Save plot
save_plot(f1_sud, "F1_SUD_spending_by_insurance", dir_output)

##----------------------------------------------------------------
## 2. Figure 2 - Spending by TOC using payer=all
## What are the differences in spending for patients with HIV for each age group based on different toc?
##
## Notes: TODO calculate CI correctly, needs some research for this, exclude NF just for HIV?
##----------------------------------------------------------------
# HIV #

# Collapse on payer, location (which is all national), year
df_f2_hiv <- df_dex %>%
  filter(geo == 'national') %>%
  filter(acause == "hiv") %>%
  filter(payer == "all") %>%
  group_by(toc, age_name, sex_name) %>%
  summarize(
    "spend_mean" = mean(spend_mean)
  )

# Remove spaces from "age_name"
df_f2_hiv$age_name <- str_replace_all(df_f2_hiv$age_name, " ", "")

# Set male spending as negative
df_f2_hiv <- df_f2_hiv %>%
  mutate(spend_mean_inverse = ifelse(sex_name == "Male", spend_mean*-1, spend_mean))

# Create factors
df_f2_hiv$age_name <- factor(df_f2_hiv$age_name, levels = age_factor) 
df_f2_hiv$sex_name <- factor(df_f2_hiv$sex_name, levels = sex_factor) 
df_f2_hiv$toc <- factor(df_f2_hiv$toc, levels = toc_factor)

# Make plot 
# TODO - Have x-axis be the same for both male and female, also fix the age axis labels to make them look more neat
# basically copy from the original script, they are already typed out
# https://github.com/ihmeuw/Resource_Tracking_US_DEX/blob/main/DEX_Capstone_2025/04_figures/figure_1.R
f2_hiv <- ggplot(data = df_f2_hiv, aes(age_name, spend_mean_inverse, fill = toc)) +
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
  labs(y = "Inflation Adjusted Spending (2019 USD)",
       x = "",
       title = "Mean annual HIV spending for each type of care by sex and age group, 2010 - 2019") +
  theme_settings +
  guides(
    fill = guide_legend(
      title.position = "top",  # put title above the keys
      title.hjust = 0.5,       # center the title
      nrow = 1                 # keep items in one row
    )) +
  geom_col(color = "black", width = 1, size = 0.3) 

# Save plot
save_plot(f2_hiv, "F2_HIV_spending_by_toc", dir_output)

# SUD #

# Collapse on payer, location (which is all national), year
df_f2_sud <- df_dex %>%
  filter(geo == 'national') %>%
  filter(acause %in% subs_causes) %>%
  filter(payer == "all") %>%
  group_by(toc, age_name, sex_name) %>%
  summarize(
    "spend_mean" = mean(spend_mean)
  )

# Remove spaces from "age_name"
df_f2_sud$age_name <- str_replace_all(df_f2_sud$age_name, " ", "")

# Set male spending as negative
df_f2_sud <- df_f2_sud %>%
  mutate(spend_mean_inverse = ifelse(sex_name == "Male", spend_mean*-1, spend_mean))

# Create factors
df_f2_sud$age_name <- factor(df_f2_sud$age_name, levels = age_factor) 
df_f2_sud$sex_name <- factor(df_f2_sud$sex_name, levels = sex_factor) 
df_f2_sud$toc <- factor(df_f2_sud$toc, levels = toc_factor)

# Make plot 
# TODO - Have x-axis be the same for both male and female, also fix the age axis labels to make them look more neat
# basically copy from the original script, they are already typed out
# https://github.com/ihmeuw/Resource_Tracking_US_DEX/blob/main/DEX_Capstone_2025/04_figures/figure_1.R
max_male <- max(abs(df_f2_sud$spend_mean_inverse[df_f2_sud$sex_name == "Male"]))
max_female <- max(abs(df_f2_sud$spend_mean_inverse[df_f2_sud$sex_name == "Female"]))

f2_sud <- ggplot(data = df_f2_sud, aes(age_name, spend_mean_inverse, fill = toc)) +
  facet_share(~ sex_name, scales = "free", reverse_num = FALSE) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = toc_colors, labels = toc_labels, name = "Type of care") +
  scale_y_continuous(
    expand = expansion(mult = c(0.15, 0.1)),  # Add 10% padding
    labels = function(x) scales::dollar(abs(x))
  ) +
  theme_classic() +
  labs(y = "Inflation Adjusted Spending (2019 USD)",
       x = "",
       title = "Mean annual SUD spending for each type of care by sex and age group, 2010 - 2019") +
  theme_settings +
  guides(
    fill = guide_legend(
      title.position = "top",
      title.hjust = 0.5,
      nrow = 1
    )) +
  geom_col(color = "black", width = 1, size = 0.3)

# Save plot
save_plot(f2_sud, "F2_SUD_spending_by_toc", dir_output)

##----------------------------------------------------------------
## 3. Figure 3 - HIV - USA County Plot Per Bene
## Visually, how does the spending per beneficiary look like stratified based on 
## insurance (medicare, medicaid, private insurance) when plotted by county across
## the US, all years, both sexes, all toc, for HIV? (big USA plot)
##
## Notes: TODO - fix title, possibly change how the data is cut (upper quintile is WAY too big)
##----------------------------------------------------------------
# Prepare data used for mapping

# Payer Strata by County - group by and summarize to get spend_mean (named "value" for plot)
# Collapse on sex, age group, toc, year, spend_mean is the MEAN of all years
df_f3_hiv_payer <- df_dex %>%
  filter(geo == "county") %>%
  filter(payer != "all") %>%
  filter(acause == "hiv") %>%
  group_by(payer, state_name, location_name, fips) %>%
  summarize(
    "value" = mean(spend_mean)
  )

# Overall Spending by County - group by and summarize to get spend_mean (named "value" for plot)
df_f3_hiv_overall <- df_dex %>%
  filter(geo == "county") %>%
  filter(payer == "all") %>%
  filter(acause == "hiv") %>%
  group_by(state_name, location_name, fips) %>%
  summarize(
    "value" = mean(spend_mean)
  )

# Merge with df_loc_ids to get "mcnty" column, used to merge with shapefile
df_f3_hiv_payer <- left_join(df_f3_hiv_payer, df_loc_ids %>% select(mcnty, full_fips_code), by = c("fips" = "full_fips_code")) # gets "mcnty" column used by shapefile
df_f3_hiv_overall <- left_join(df_f3_hiv_overall, df_loc_ids %>% select(mcnty, full_fips_code), by = c("fips" = "full_fips_code")) # gets "mcnty" column used by shapefile

# Maps - Payer Strata --
f3_payer_plot_list <- list()
for(p in c("mdcr", "mdcd", "priv", "oop")){
  if(length(f3_payer_plot_list) >= 4){
    f3_payer_plot_list = list()
  }
  print(p)
  
  # Filter DF by payer
  map_df <- df_f3_hiv_payer %>% filter(payer == p)
  
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
  f3_payer_plot_list[[length(f3_payer_plot_list) + 1]] <- map
}


# Maps - Overall Spending ---

# THIS SECTION NEEDS CLEANING UP - BEGINNING  ---
# Basically the scale for the all TOC map most likely needs to be manually defined, the code in this section
# is messy and needs to be cleaned up, but maybe can come at a time when we decide on how we want the plot to look

# set breaks and labels for 7 bins
# f5_overall_brks <- sapply(seq(0, 1, by = 1/10), function(x) quantile(df_f3_hiv_overall$value, x))
# f5_overall_brks[1] <- f5_overall_brks[1]-1
# labs <- paste0("$",format(comma(round(f5_overall_brks[-length(f5_overall_brks)]))), " - $", format(comma(round(f5_overall_brks[-1]))))

manual_brks <- c(0, 1000, 5000, 10000, 20000, 50000, 100000, 500000, ceiling(max(df_f3_hiv_overall$value)))
labs <- paste0("$",format(comma(round(manual_brks[-length(manual_brks)]))), " - $", format(comma(round(manual_brks[-1]))))

# cut data into bins
df_f3_hiv_overall$plot_val <- cut(df_f3_hiv_overall$value, breaks = manual_brks, labels = labs)

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
df_f3_hiv_overall_map_object <- merge(mcnty_shapefile, df_f3_hiv_overall, by = "mcnty")

# create plot
f3_hiv_overall_county_map <-  ggplot(data = df_f3_hiv_overall_map_object) +
  geom_sf(aes(fill = plot_val, geometry = geometry), color = NA) + # counties w/o border
  geom_sf(data = state_shapefile, fill = NA, linewidth = .4) +
  labs(title = paste0("All payer spending"),
       fill = "") +
  scale_fill_manual(values = cols_8_biased,
                    breaks = levels(factor(df_f3_hiv_overall$plot_val))[levels(factor(df_f3_hiv_overall$plot_val)) != "NA"],
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
f3_payer_maps <- arrangeGrob(grobs = f3_payer_plot_list, nrow = 2, ncol = 2) 
f3_title_grob <- text_grob("Estimated HIV average annual spending by US county (2010 - 2019)", size = 16)

# make county map a grob object to make compatible with arrangeGrob
f3_hiv_overall_county_map_grob <- ggplotGrob(f3_hiv_overall_county_map) 

# create complete layout
f3_layout <- arrangeGrob(f3_title_grob, f3_hiv_overall_county_map_grob, f3_payer_maps, nrow=3, ncol=1, heights=c(0.05, 1, 1.5))

# Save out
f3_file_name <- "F3_HIV_spending_by_county_map.pdf"
pdf(file = file.path(dir_output, f3_file_name), width = 14, height = 16)
grid.draw(f3_layout)
dev.off()

##----------------------------------------------------------------
## 3. Figure 3 - SUD - USA County Plot Per Bene
## Visually, how does the spending per beneficiary look like stratified based on 
## insurance (medicare, medicaid, private insurance) when plotted by county across
## the US, all years, both sexes, all toc, for SUD? (big USA plot)
##
## Notes: TODO - fix title, possibly change how the data is cut (upper quintile is WAY too big)
##----------------------------------------------------------------

# Prepare data used for mapping

# Payer Strata by County - group by and summarize to get spend_mean (named "value" for plot)
# Collapse on sex, age group, toc, year, spend_mean is the MEAN of all years
df_f3_sud_payer <- df_dex %>%
  filter(geo == "county") %>%
  filter(acause %in% subs_causes) %>%
  filter(payer != "all") %>%
  group_by(payer, state_name, location_name, fips) %>%
  summarize(
    "value" = mean(spend_mean)
  )

# Overall Spending by County - group by and summarize to get spend_mean (named "value" for plot)
df_f3_sud_overall <- df_dex %>%
  filter(geo == "county") %>%
  filter(acause %in% subs_causes) %>%
  filter(payer == "all") %>%
  group_by(state_name, location_name, fips) %>%
  summarize(
    "value" = mean(spend_mean)
  )

# Merge with df_loc_ids to get "mcnty" column, used to merge with shapefile
df_f3_sud_payer <- left_join(df_f3_sud_payer, df_loc_ids %>% select(mcnty, full_fips_code), by = c("fips" = "full_fips_code")) # gets "mcnty" column used by shapefile
df_f3_sud_overall <- left_join(df_f3_sud_overall, df_loc_ids %>% select(mcnty, full_fips_code), by = c("fips" = "full_fips_code")) # gets "mcnty" column used by shapefile

# Payer Strata by County maps
f3_payer_plot_list <- list()
for(p in c("mdcr", "mdcd", "priv", "oop")){
  if(length(f3_payer_plot_list) >= 4){
    f3_payer_plot_list = list()
  }
  print(p)
  
  # Filter DF by payer
  map_df <- df_f3_sud_payer %>% filter(payer == p)
  
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
  f3_payer_plot_list[[length(f3_payer_plot_list) + 1]] <- map
}

# Maps - Overall Spending ---

# THIS SECTION NEEDS CLEANING UP - BEGINNING  ---
# Basically the scale for the all TOC map most likely needs to be manually defined, the code in this section
# is messy and needs to be cleaned up, but maybe can come at a time when we decide on how we want the plot to look

# Overall Spending by County
# set breaks and labels for 7 bins
# f5_overall_brks <- sapply(seq(0, 1, by = 1/10), function(x) quantile(df_f3_sud_overall$value, x))
# f5_overall_brks[1] <- f5_overall_brks[1]-1
# labs <- paste0("$",format(comma(round(f5_overall_brks[-length(f5_overall_brks)]))), " - $", format(comma(round(f5_overall_brks[-1]))))

manual_brks <- c(0, 1000, 5000, 10000, 20000, 50000, 100000, 250000, ceiling(max(df_f3_sud_overall$value)))
labs <- paste0("$",format(comma(round(manual_brks[-length(manual_brks)]))), " - $", format(comma(round(manual_brks[-1]))))

# cut data into bins
df_f3_sud_overall$plot_val <- cut(df_f3_sud_overall$value, breaks = manual_brks, labels = labs)

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
df_f3_sud_overall_map_object <- merge(mcnty_shapefile, df_f3_sud_overall, by = "mcnty")

# create plot
f3_sud_overall_county_map <-  ggplot(data = df_f3_sud_overall_map_object) +
  geom_sf(aes(fill = plot_val, geometry = geometry), color = NA) + # counties w/o border
  geom_sf(data = state_shapefile, fill = NA, linewidth = .4) +
  labs(title = paste0("All payer spending"),
       fill = "") +
  scale_fill_manual(values = cols_8_biased,
                    breaks = levels(factor(df_f3_sud_overall$plot_val))[levels(factor(df_f3_sud_overall$plot_val)) != "NA"],
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
f3_payer_maps <- arrangeGrob(grobs = f3_payer_plot_list, nrow = 2, ncol = 2) 
f3_title_grob <- text_grob("Estimated SUD average annual spending by US county (2010 - 2019)", size = 16)

# make county map a grob object to make compatible with arrangeGrob
f3_sud_overall_county_map_grob <- ggplotGrob(f3_sud_overall_county_map) 
f3_layout <- arrangeGrob(f3_title_grob, f3_sud_overall_county_map_grob, f3_payer_maps, nrow=3, ncol=1, heights=c(0.05, 1, 1.5))

# Save plot
f3_file_name <- "F3_SUD_spending_by_county_map.pdf"
pdf(file = file.path(dir_output, f3_file_name), width = 14, height = 16)
grid.draw(f3_layout)
dev.off()


### REFERENCE CODE - SAFE TO DELETE

# Code used to look at the difference between individual payer groups summed up vs. payer = all
# There seems to be a pretty big delta
df_test <- df_dex %>%
  filter(geo == "county") %>%
  filter(acause == "hiv") %>%
  filter(year_id == 2015) %>%
  filter(location_name == "King County") %>%
  filter(fips == 53033) %>%
  filter(age_name == "55 - <60")

# df_test_sex <- df_test %>%
#   group_by(year_id, location_name, fips, payer) %>%
#   summarise(
#     spend_mean = sum(spend_mean)
#   )

# df_test_rc <- df_test %>%
#   select(c("year_id", "geo", "location_name", "fips", "payer", "toc",
#            "acause", "cause_name", "age_group_years_start", "age_name",
#            "sex_id", "sex_name", "spend_mean",
#            "state_name", "location_id", "merged_location_id"))
# 
# df_test_pivot <- df_test_rc %>%
#   pivot_wider(
#     names_from  = payer,
#     values_from = spend_mean
#   )
# 
# df_test_pivot <- df_test_pivot %>%
#   mutate(
#     payer_sum = rowSums(cbind(mdcd, mdcr, oop, priv), na.rm = TRUE),
#     payer_delta = (all - payer_sum)
#   )
# 
# 
# df_test_pivot$payer_sum <- df_test_pivot$mdcd + df_test_pivot$mdcr + df_test_pivot$oop + df_test_pivot$priv
# df_test_pivot$payer_delta <- df_test_pivot$all - df_test_pivot$payer_sum


# CHecking national level
# df_test_nat <- df_dex %>%
#   filter(geo == "national") %>%
#   filter(acause == "hiv") %>%
#   filter(year_id == 2015) %>%
#   filter(age_name == "55 - <60") %>%
#   select(c("year_id", "geo", "location_name", "fips", "payer", "toc",
#            "acause", "cause_name", "age_group_years_start", "age_name",
#            "sex_id", "sex_name", "spend_mean",
#            "state_name", "location_id", "merged_location_id"))
# 
# 
#   pivot_wider(
#     names_from  = payer,
#     values_from = spend_mean
#   ) %>%
#   mutate(
#     payer_sum = rowSums(cbind(mdcd, mdcr, oop, priv), na.rm = TRUE),
#     payer_delta = (all - payer_sum)
#   )
# 
# View(df_test_nat %>% select(c("year_id", "geo", "location_name", "toc", "acause", 
#                                "cause_name", "age_group_years_start", "age_name", "sex_id", 
#                                "all", "mdcd", "mdcr", "oop", "priv", "payer_sum", "payer_delta"
# )))

