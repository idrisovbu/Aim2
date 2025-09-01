##----------------------------------------------------------------
##' Title: A_Summary_Figures.R
##'
##' Purpose: Creates figures for Summary Level data for Aim2
##----------------------------------------------------------------

##----------------------------------------------------------------
## 0. Clear environment and set library paths
##----------------------------------------------------------------
rm(list = ls())

pacman::p_load(dplyr, openxlsx, RMySQL, data.table, ini, DBI, tidyr, openxlsx, reticulate)
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


##----------------------------------------------------------------
## 0. Functions
##----------------------------------------------------------------
ensure_dir_exists <- function(dir_path) {
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
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

# Set output directories
dir_output <- "/mnt/share/scratch/users/idrisov/Aim2_Outputs/"
dir_output_figures <- file.path(dir_output, "A_Figures/")

ensure_dir_exists(dir_output)
ensure_dir_exists(dir_output_figures)

##----------------------------------------------------------------
## 1. Figure 1
## What are the differences in spending per beneficiary for patients with HIV 
## for each age group based on different types of insurance (Medicare, Medicaid, Private)? 
## (all years, all counties)
##
## Figure 2. Repeat for SUD
## Notes: TODO calculate CI correctly, needs some research for this
##----------------------------------------------------------------

# Figure 1 - HIV

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

# convert to dollars
for (col in colnames(df_hiv)) {
  if (col == "spend_mean") {
    df_hiv[[col]] <- dollar(df_hiv[[col]])
  }
}

# make plot

## -----------------------------------
## Arguments and set up
## -----------------------------------
# # specify scaled_version 
# scaled_version <- "XX"
# 
# # data paths to pull from
# national_data_path <- "FILEPATH"
# 
# out_dir <- paste0("FILEPATH", scaled_version,"FILEPATH")
# if(!dir.exists(out_dir)){
#   dir.create(out_dir, recursive = T)
# }

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

# age range labels
age_labels = c(
  `0` = "0 - <1",
  `1` = "1 - <5",
  `5` = "5 - <10",
  `10` = "10 - <15",
  `15` = "15 - <20",
  `20` = "20 - <25",
  `25` = "25 - <30",
  `30` = "30 - <35",
  `35` = "35 - <40",
  `40` = "40 - <45",
  `45` = "45 - <50",
  `50` = "50 - <55",
  `55` = "55 - <60",
  `60` = "60 - <65",
  `65` = "65 - <70",
  `70` = "70 - <75",
  `75` = "75 - <80",
  `80` = "80 - <85",
  `85` = ">= 85"
)

## -----------------------------------
## Reading in population
## -----------------------------------

# ## Population
# pop <- fread("FILEPATH/pop_age_sex.csv")[year_id == 2019 & geo == "national"]
# 
# # Read in data used for payer age pyramids
# pyramid_data1 <- open_dataset(national_data_path) %>% 
#   filter(year_id == 2019, payer != "oth") %>%
#   group_by(payer, age_group_years_start, sex_id, draw) %>%
#   summarize(spend = sum(spend)) %>%
#   group_by(payer, age_group_years_start, sex_id) %>%
#   summarize(spend = mean(spend)) %>%
#   
#   pyramid_data1[, age_label := factor(age_labels[as.character(age_group_years_start)],
#                                       levels = c(unname(age_labels)))]
# pyramid_data1[, group_spend := sum(spend), .(age_group_years_start, sex_id)]

# Set factors for ages
df$my_column <- factor(df$my_column,
                       levels = c("low", "medium", "high"))

# Plot ## LEFT OFF HERE
ggplot(aes(as.factor(age_label), spend_bil, fill = factor(payer, levels = c("mdcr", "mdcd", "priv", "oop")))) +
  facet_share(~ sex, scales = "free_x", reverse_num = TRUE) +
  geom_bar(stat = "identity") +
  coord_flip() +
  #scale_fill_manual(values = payer_colors, labels = payer_list, name = "Payer") +
  #scale_y_continuous(breaks = seq(-160, 160, 40), labels = seq(-160, 160, 40)) +
  theme_classic() +
  labs(y = "Y Axis",
       x = "X Axis",
       title = "Title",
       subtitle = "Subtitle") +
  theme(axis.title.y = element_blank(),
        text = element_text(size = 12),
        plot.subtitle = element_text(size = 10))



# making pyramid
pyramid1 <- pyramid_data1 %>%
  mutate(spend = ifelse(sex_id == 1, spend*-1, spend),
         sex = ifelse(sex_id == 1, "Male", "Female"),
         sex = factor(sex, levels = c("Male", "Female")),
         spend_bil = spend/1e9) %>%
  ggplot(aes(as.factor(age_label), spend_bil, fill = factor(payer, levels = c("mdcr", "mdcd", "priv", "oop")))) +
  facet_share(~ sex, scales = "free_x", reverse_num = TRUE) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = payer_colors, labels = payer_list, name = "Payer") +
  scale_y_continuous(breaks = seq(-160, 160, 40), labels = seq(-160, 160, 40)) +
  theme_classic() +
  labs(y = "Estimated total Spending (US$ billions, 2019 dollars)",
       title = "Panel A: Estimated spending by age, sex, and payer",
       subtitle = "Total health care spending in 2019: $2.4 trillion") +
  theme(axis.title.y = element_blank(),
        text = element_text(size = 12),
        plot.subtitle = element_text(size = 10))

# Read in data used for type of care age pyramids
pyramid_data2 <- open_dataset(national_data_path) %>%
  filter(year_id == 2019, payer != "oth") %>%
  group_by(toc, age_group_years_start, sex_id, draw) %>%
  summarize(spend = sum(spend)) %>%
  group_by(toc, age_group_years_start, sex_id) %>%
  summarize(spend = mean(spend)) %>%
  collect()

pyramid_data2 <- left_join(pyramid_data2, pop, by = c("age_group_years_start", "sex_id")) %>%
  mutate(spend_pc = spend/pop)
setDT(pyramid_data2)
pyramid_data2[, age_label := factor(age_labels[as.character(age_group_years_start)],
                                    levels = c(unname(age_labels)))]

## National spend per capita for pyramid 2 subtitle
nat_per_cap <- round(sum(pyramid_data1$spend)/sum(pop$pop))

# need to add a title a maybe make some aesthetic changes to facet labels
pyramid2 <- pyramid_data2 %>%
  mutate(spend_pc = ifelse(sex_id == 1, (spend_pc*-1)/1e3, spend_pc/1e3),
         sex = ifelse(sex_id == 1, "Male", "Female"),
         sex = factor(sex, levels = c("Male", "Female"))) %>%
  ggplot(aes(age_label, spend_pc, fill = factor(toc, levels = c("DV", "ED", "HH", "RX", "NF", "IP", "AM")))) +
  facet_share(~ sex, scales = "free_x", reverse_num = TRUE) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = toc_colors, labels = toc_labels, name = "Type of Care") +
  theme_classic() +
  labs(y = "Estimated spending per capita (US$ thousands, 2019 dollars)",
       title = "Panel B: Estimated spending per capita by age, sex, and type of care",
       subtitle = paste0("National average spending per capita in 2019: $", scales::comma(nat_per_cap))) +
  theme(axis.title.y = element_blank(),
        text = element_text(size = 12),
        plot.subtitle = element_text(size = 10))

# layout - to cleanly add title to top of figure
pyramids <- arrangeGrob(grobs = list(pyramid1, pyramid2), nrow = 1, ncol = 2) 
title_grob <- text_grob("Figure 1. Age pyramids of total spending by payer and spending per capita by type of care in 2019", size = 16)
layout <- arrangeGrob(title_grob, pyramids, nrow=2, ncol=1, heights=c(0.1, 1))

# setting file path
file_name <- "Figure_1.pdf"
full_path <- paste0(out_dir, file_name)

pdf(file = full_path, width = 16, height = 5)
grid.draw(layout)
dev.off()