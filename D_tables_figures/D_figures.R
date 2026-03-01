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

library(plotly)
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

##----------------------------------------------------------------
## 0.2 Set directories for DEX estimate data / county estimates
##----------------------------------------------------------------
# Set path for data
date_dex <- "20260120"
fp_dex <- file.path(h, "/aim_outputs/Aim2/B_aggregation/", date_dex, "/compiled_dex_data_2010_2019.parquet")

date_ushd <- "20251204"
fp_ushd <- file.path(h, "/aim_outputs/Aim2/B_aggregation/", date_ushd, "/compiled_ushd_data_2010_2019.parquet")

date_gbd <- "20260131"
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
                     "ryan_white" =	"#bc53f5")

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
  collect()
  
df_f1_hiv <- df_f1_hiv %>%
  dplyr::group_by(payer, age_name, sex_name) %>%
  dplyr::summarise(
    spend_mean = mean(spend_mean, na.rm = TRUE),
    .groups = "drop"
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
  collect()

df_f1_sud <- df_f1_sud %>%
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
## 1.1 Figure 1 - Spending by insurance + RW, HIV Only
## What are the differences in spending for patients with HIV / SUD for each age group
## based on different types of insurance (Medicare, Medicaid, Private)? (all years, all counties)
##----------------------------------------------------------------

# HIV + RW data #

# We want just payer, age, sex left over - let's format the RW data first so we know what to match with

# Collapse on TOC, location (which is all national), year
df_f1_hiv_rw <- df_dex %>%
  filter(geo == "national") %>%
  filter(acause == "hiv") %>%
  filter(payer != "all") %>%
  collect()

df_f1_hiv_rw <- df_f1_hiv_rw %>%
  dplyr::group_by(payer, age_name, sex_name) %>%
  dplyr::summarise(
    spend_mean = mean(spend_mean, na.rm = TRUE),
    .groups = "drop"
  )

# Collapse on age groups to match RW age groups: "25 - <35" "35 - <45" "45 - <55" "55 - <65" "65+" 
df_f1_hiv_rw <- df_f1_hiv_rw %>%
  mutate(
    age_name_rw = case_when(
      age_name %in% c("25 - <30", "30 - <35") ~ "25 - <35",
      age_name %in% c("35 - <40", "40 - <45") ~ "35 - <45",
      age_name %in% c("45 - <50", "50 - <55") ~ "45 - <55",
      age_name %in% c("55 - <60", "60 - <65") ~ "55 - <65",
      age_name %in% c("65 - <70", "70 - <75", "75 - <80", "80 - <85", "85+") ~ "65+",
      TRUE ~ NA_character_   # non-overlapping ages: 0-<25, etc.
    )
  )

df_f1_hiv_rw <- df_f1_hiv_rw %>%
  filter(!is.na(age_name_rw))

df_f1_hiv_rw <- df_f1_hiv_rw %>%
  group_by(payer, sex_name, age_name_rw) %>%
  summarize(spend_mean = sum(spend_mean))

df_f1_hiv_rw <- df_f1_hiv_rw %>%
  rename(
    age_name = age_name_rw
  )

# Collapse RW data
df_f1_rw_data <- df_rw_long %>%
  dplyr::group_by(age_name, sex_name) %>%
  dplyr::summarise(
    spend_mean = mean(rw_funding, na.rm = TRUE),
    .groups = "drop"
  )

# Filter out <13 and 13-24 age groups from RW data
df_f1_rw_data <- df_f1_rw_data %>%
  filter(!age_name %in% c("<13", "13–24"))

# Add "payer" column label for RW
df_f1_rw_data$payer <- "ryan_white"

# Combine DEX + RW data
df_f1_hiv_rw_fig_data <- rbind(df_f1_hiv_rw, df_f1_rw_data)

# Remove spaces from "age_name"
#df_f1_hiv_rw_fig_data$age_name <- str_replace_all(df_f1_hiv_rw_fig_data$age_name, " ", "")

# Set male spending as negative
df_f1_hiv_rw_fig_data <- df_f1_hiv_rw_fig_data %>%
  mutate(spend_mean_inverse = ifelse(sex_name == "Male", spend_mean*-1, spend_mean))

# Create factors
df_f1_hiv_rw_fig_data$age_name <- factor(df_f1_hiv_rw_fig_data$age_name, levels = age_factor_rw) 
df_f1_hiv_rw_fig_data$sex_name <- factor(df_f1_hiv_rw_fig_data$sex_name, levels = sex_factor) 

# Make plot 
# TODO - Have x-axis be the same for both male and female, also fix the age axis labels to make them look more neat
# basically copy from the original script, they are already typed out
# https://github.com/ihmeuw/Resource_Tracking_US_DEX/blob/main/DEX_Capstone_2025/04_figures/figure_1.R
f1_hiv_rw <- ggplot(data = df_f1_hiv_rw_fig_data, aes(age_name, spend_mean_inverse, fill = factor(payer, levels = c("mdcr", "mdcd", "priv", "oop", "ryan_white")))) +
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
       title = "Mean annual HIV spending for each insurance type & Ryan White by sex and age group, 2010 - 2019") +
  theme_settings +
  guides(
    fill = guide_legend(
      title.position = "top",  # put title above the keys
      title.hjust = 0.5,       # center the title
      nrow = 1                 # keep items in one row
    )) +
  geom_col(color = "black", width = 1, size = 0.3) 

f1_hiv_rw

# Save plot
save_plot(f1_hiv_rw, "F1_HIV_spending_by_insurance_plus_RW", dir_output)

##----------------------------------------------------------------
## 1.2 Figure 1 - Spending total + RW, spending per case HIV Only
##
## The values in the plot are derived from the sum of all the spending across 2010 ~ 2019
## and then summing the prevalent cases across that same time period for the respective sex & age groups
## then dividing the spending / prevalence counts
##
## In plain English, it represents the HIV spending per case for the entire span of years 2010 ~ 2019
##----------------------------------------------------------------

# HIV + RW data #

# Collapse on TOC, payer, location (which is all national), year
df_f2_hiv_rw <- df_dex %>%
  filter(geo == "national") %>%
  filter(acause == "hiv") %>%
  filter(payer == "all") %>%
  collect()

df_f2_hiv_rw <- df_f2_hiv_rw %>%
  dplyr::group_by(age_name, sex_name) %>%
  dplyr::summarise(
    spend_mean = sum(spend_mean, na.rm = TRUE), # should this be mean or sum? I think sum
    .groups = "drop"
  )

# Collapse on age groups to match RW age groups: "25 - <35" "35 - <45" "45 - <55" "55 - <65" "65+" 
df_f2_hiv_rw <- df_f2_hiv_rw %>%
  mutate(
    age_name_rw = case_when(
      age_name %in% c("25 - <30", "30 - <35") ~ "25 - <35",
      age_name %in% c("35 - <40", "40 - <45") ~ "35 - <45",
      age_name %in% c("45 - <50", "50 - <55") ~ "45 - <55",
      age_name %in% c("55 - <60", "60 - <65") ~ "55 - <65",
      age_name %in% c("65 - <70", "70 - <75", "75 - <80", "80 - <85", "85+") ~ "65+",
      TRUE ~ NA_character_   # non-overlapping ages: 0-<25, etc.
    )
  )

df_f2_hiv_rw <- df_f2_hiv_rw %>%
  filter(!is.na(age_name_rw))

df_f2_hiv_rw <- df_f2_hiv_rw %>%
  dplyr::group_by(sex_name, age_name_rw) %>%
  dplyr::summarize(spend_mean = sum(spend_mean))

df_f2_hiv_rw <- df_f2_hiv_rw %>%
  rename(
    age_name = age_name_rw
  )

# Add "payer" column label for DEX data
df_f2_hiv_rw$payer <- "DEX_all_TOC"

# Collapse RW data
df_f2_rw_data <- df_rw_long %>%
  dplyr::group_by(age_name, sex_name) %>%
  dplyr::summarise(
    spend_mean = sum(rw_funding, na.rm = TRUE), # should this be mean or sum? I think sum
    .groups = "drop"
  )

# Filter out <13 and 13-24 age groups from RW data
df_f2_rw_data <- df_f2_rw_data %>%
  filter(!age_name %in% c("<13", "13–24"))

# Add "payer" column label for RW
df_f2_rw_data$payer <- "ryan_white"

# Sum up DEX total spending + RW data
df_f2_hiv_rw_fig_data <- df_f2_hiv_rw_fig_data %>%
  dplyr::group_by(sex_name, age_name) %>%
  dplyr::summarize(spend_mean = sum(spend_mean))

# Format GBD data
df_f2_gbd <- df_gbd %>%
  filter(cause_name == "HIV/AIDS") %>%
  dplyr::group_by(age_group_name, sex_id) %>%
  dplyr::summarize(prevalence_counts = sum(prevalence_counts))

# Collapse on age groups to match RW age groups: "25 - <35" "35 - <45" "45 - <55" "55 - <65" "65+" 
df_f2_gbd <- df_f2_gbd %>%
  mutate(
    age_name_rw = case_when(
      age_group_name %in% c("25 to 29", "30 to 34") ~ "25 - <35",
      age_group_name %in% c("35 to 39", "40 to 44") ~ "35 - <45",
      age_group_name %in% c("45 to 49", "50 to 54") ~ "45 - <55",
      age_group_name %in% c("55 to 59", "60 to 64") ~ "55 - <65",
      age_group_name %in% c("65 to 69", "70 to 74", "75 to 79", "80 to 84", "85+") ~ "65+",
      TRUE ~ NA_character_   # non-overlapping ages: 0-<25, etc.
    )
  )

df_f2_gbd <- df_f2_gbd %>%
  filter(!is.na(age_name_rw))

df_f2_gbd <- df_f2_gbd %>%
  dplyr::group_by(sex_id, age_name_rw) %>%
  dplyr::summarize(prevalence_counts = sum(prevalence_counts))

df_f2_gbd <- df_f2_gbd %>%
  rename(
    age_name = age_name_rw
  )

df_f2_gbd <- df_f2_gbd %>%
  mutate(
    sex_name = case_when(
      sex_id == 1 ~ "Male",
      sex_id == 2 ~ "Female"
    )
  )

# Combine DEX + RW data
df_f2_hiv_rw_fig_data <- rbind(df_f2_hiv_rw, df_f2_rw_data)

# Merge w/ GBD data to calculate spending per prevalent case
df_f2_hiv_rw_fig_data <- left_join(
  x = df_f2_hiv_rw_fig_data,
  y = df_f2_gbd,
  by = c("sex_name", "age_name")
)

# Create spending per case column
df_f2_hiv_rw_fig_data$spend_per_case <- df_f2_hiv_rw_fig_data$spend_mean / df_f2_hiv_rw_fig_data$prevalence_counts

# Set male spend_per_case as negative
df_f2_hiv_rw_fig_data <- df_f2_hiv_rw_fig_data %>%
  mutate(spend_per_case_inverse = ifelse(sex_name == "Male", spend_per_case*-1, spend_per_case))

# Create factors
df_f2_hiv_rw_fig_data$age_name <- factor(df_f2_hiv_rw_fig_data$age_name, levels = age_factor_rw) 
df_f2_hiv_rw_fig_data$sex_name <- factor(df_f2_hiv_rw_fig_data$sex_name, levels = sex_factor) 

# Make plot 
# TODO - Have x-axis be the same for both male and female, also fix the age axis labels to make them look more neat
# basically copy from the original script, they are already typed out
# https://github.com/ihmeuw/Resource_Tracking_US_DEX/blob/main/DEX_Capstone_2025/04_figures/figure_1.R
f2_hiv_rw <- ggplot(data = df_f2_hiv_rw_fig_data, aes(age_name, spend_per_case_inverse, fill = sex_name)) +
  facet_share(~ sex_name, scales = "free", reverse_num = FALSE) +
  geom_bar(stat = "identity") +
  coord_flip() +
  #scale_fill_manual(values = payer_colors, labels = payer_list, name = "Payer") +
  scale_y_continuous(
    #limits = (),
    #breaks = seq(-800000, 800000, 200000),
    labels = function(x) scales::dollar(abs(x))
  ) +
  theme_classic() +
  labs(y = "Inflation Adjusted Spending (2019 USD)",
       x = "",
       title = "HIV spending per case by sex and age group, DEX + Ryan White spending, 2010 - 2019") +
  theme_settings +
  guides(
    fill = guide_legend(
      title.position = "top",  # put title above the keys
      title.hjust = 0.5,       # center the title
      nrow = 1                 # keep items in one row
    )) + theme(legend.position = "none")
    #+ geom_col(color = "black", width = 1, size = 0.3) 

f2_hiv_rw

# Save plot
save_plot(f2_hiv_rw, "F1_HIV_spending_per_case_RW", dir_output)

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
  collect() 

df_f2_hiv <- df_f2_hiv %>%
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
  collect()

df_f2_sud <- df_f2_sud %>%
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
  collect()

df_f3_hiv_payer <- df_f3_hiv_payer %>%
  group_by(payer, state_name, location_name, fips) %>%
  summarize(
    "value" = mean(spend_mean)
  )

# Overall Spending by County - group by and summarize to get spend_mean (named "value" for plot)
df_f3_hiv_overall <- df_dex %>%
  filter(geo == "county") %>%
  filter(payer == "all") %>%
  filter(acause == "hiv") %>%
  collect()

df_f3_hiv_overall <- df_f3_hiv_overall %>%
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
  collect()

df_f3_sud_payer <- df_f3_sud_payer %>%
  group_by(payer, state_name, location_name, fips) %>%
  summarize(
    "value" = mean(spend_mean)
  )

# Overall Spending by County - group by and summarize to get spend_mean (named "value" for plot)
df_f3_sud_overall <- df_dex %>%
  filter(geo == "county") %>%
  filter(acause %in% subs_causes) %>%
  filter(payer == "all") %>%
  collect()

df_f3_sud_overall <- df_f3_sud_overall %>%
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

##----------------------------------------------------------------
## 3. Figure 3 - HIV - USA State level spending per prevalent case, all payers because prevalence is not payer specific
## 
## Notes: WIP - not finished 3/1
##----------------------------------------------------------------
# # Prepare data used for mapping
# 
# # Payer Strata by STATE - group by and summarize to get spend_mean (named "value" for plot)
# # Collapse on sex, age group, toc, year, spend_mean is the MEAN of all years
# df_f3_hiv_state <- df_dex %>%
#   filter(geo == "state") %>%
#   filter(payer == "all") %>%
#   filter(acause == "hiv") %>%
#   collect()
# 
# df_f3_hiv_state <- df_f3_hiv_state %>%
#   group_by(location_name, fips) %>%
#   summarize(
#     "value" = mean(spend_mean)
#   )
# 
# # Maps - Payer Strata --
# f3_payer_plot_list <- list()
# for(p in c("mdcr", "mdcd", "priv", "oop")){
#   if(length(f3_payer_plot_list) >= 4){
#     f3_payer_plot_list = list()
#   }
#   print(p)
#   
#   # Filter DF by payer
#   map_df <- df_f3_hiv_payer %>% filter(payer == p)
#   
#   # Create value breaks
#   brks <- c(quantile(map_df$value, .0, na.rm = TRUE),
#             quantile(map_df$value, .2, na.rm = TRUE),
#             quantile(map_df$value, .4, na.rm = TRUE),
#             quantile(map_df$value, .6, na.rm = TRUE),
#             quantile(map_df$value, .8, na.rm = TRUE),
#             quantile(map_df$value, 1, na.rm = TRUE))
#   labs <- paste0("$",format(comma(round(brks[-length(brks)]))), " - $", format(comma(round(brks[-1]))))
#   map_df$plot_val <- cut(map_df$value, breaks = c(brks), labels = labs)
#   
#   # Colors / labels for plot
#   cols = payer_colors_maps[[p]]
#   payer_title <- payer_list[[p]]
#   if(p == "oop"){
#     denom = "capita"
#   } else {
#     denom = "beneficiary"
#   }
#   
#   # Merge map_df with the county shapefile
#   map_df <- merge(mcnty_shapefile, map_df, by = "mcnty")
#   
#   # Create plot
#   map <- ggplot(data = map_df)+
#     geom_sf(aes(fill = plot_val, geometry = geometry), color = NA)+
#     geom_sf(data = state_shapefile, fill = NA, linewidth = .4) +
#     # labs(title = paste0(payer_title, " spending per ",denom),
#     labs(title = paste0(payer_title, " spending"),
#          fill = "") +
#     scale_fill_manual(values = cols, 
#                       breaks = levels(factor(map_df$plot_val))[levels(factor(map_df$plot_val)) != "NA"],
#                       na.value = "#838484")+
#     theme(legend.position = "bottom",
#           legend.justification = "top",
#           legend.margin = margin(t = -10, unit = "pt"),  # Adjust top margin of the legend to pull it closer
#           legend.spacing.y = unit(0, "cm"),
#           plot.margin = margin(0, 0, 1, 0, "cm"),
#           title = element_text(size = 12),
#           text = element_text(size = 10),
#           axis.ticks = element_blank(),
#           axis.text = element_blank(),
#           panel.background = element_blank()) 
#   
#   # Append plot to plot list
#   f3_payer_plot_list[[length(f3_payer_plot_list) + 1]] <- map
# }
# 
# ## Arranging PDF layout of maps
# f3_payer_maps <- arrangeGrob(grobs = f3_payer_plot_list, nrow = 2, ncol = 2) 
# f3_title_grob <- text_grob("Estimated HIV average annual spending by US county (2010 - 2019)", size = 16)
# 
# # make county map a grob object to make compatible with arrangeGrob
# f3_hiv_overall_county_map_grob <- ggplotGrob(f3_hiv_overall_county_map) 
# 
# # create complete layout
# f3_layout <- arrangeGrob(f3_title_grob, f3_hiv_overall_county_map_grob, f3_payer_maps, nrow=3, ncol=1, heights=c(0.05, 1, 1.5))
# 
# # Save out
# f3_file_name <- "F3_HIV_spending_by_county_map.pdf"
# pdf(file = file.path(dir_output, f3_file_name), width = 14, height = 16)
# grid.draw(f3_layout)
# dev.off()

##----------------------------------------------------------------
## 4. Figure 4 - HIV % of Population
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
df_f4 <- df_gbd %>%
  filter(cause_name == "HIV/AIDS") %>%
  dplyr::group_by(age_group_name, year_id, sex_id) %>%
  dplyr::summarize(
    prevalence_counts = sum(prevalence_counts),
    population = sum(population))

# Create HIV % of population metric
df_f4 <- df_f4 %>%
  mutate(`hiv_%` = prevalence_counts / population)

# Create sex names
df_f4 <- df_f4 %>%
  mutate(sex_name = case_when(sex_id == 1 ~ "Male", 
                              sex_id == 2 ~ "Female"))

# Create plot
age_factor <- c("0 - <1", "1 - <5", "5 to 9", "10 to 14", "15 to 19", "20 to 24", "25 to 29", 
                "30 to 34", "35 to 39", "40 to 44", "45 to 49",  "50 to 54", 
                "55 to 59", "60 to 64", "65 to 69", "70 to 74", "75 to 79", "80 to 84", "85+")

sex_factor <- c("Male", "Female")

# 1) Compute delta (2019 - 2010) by age_group_name (optionally stratified by sex_name)
df_f4_delta <- df_f4 %>%
  filter(year_id %in% c(2010, 2019)) %>%
  mutate(
    sex_name = factor(sex_name, levels = sex_factor),
    age_group_name = factor(age_group_name, levels = age_factor)
  ) %>%
  group_by(age_group_name, sex_name, year_id) %>%
  summarise(hiv = mean(`hiv_%`, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = year_id, values_from = hiv) %>%
  mutate(delta = `2019` - `2010`)

f4_hiv_pop_percent <- ggplot(df_f4_delta, aes(x = delta, y = age_group_name, fill = delta > 0)) +
  geom_col() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_grid(rows = vars(sex_name)) +
  scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "firebrick")) +
  scale_x_continuous(labels = label_percent(accuracy = 0.01)) +
  labs(
    title = "Change in US national population HIV % (2019 − 2010)",
    x = "Δ HIV%",
    y = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Save plot
save_plot(f4_hiv_pop_percent, "F4_HIV_population_percent", dir_output)


### REFERENCE CODE - SAFE TO DELETE

# Code used to look at the difference between individual payer groups summed up vs. payer = all
# There seems to be a pretty big delta
# df_test <- df_dex %>%
#   filter(geo == "county") %>%
#   filter(acause == "hiv") %>%
#   filter(year_id == 2015) %>%
#   filter(location_name == "King County") %>%
#   filter(fips == 53033) %>%
#   filter(age_name == "55 - <60")

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

