# New covariates file
# October 18, 2021

rm(list=ls())
library(pacman)
library(data.table)
library(dplyr)
library(tidyverse)
library(DataCombine, lib.loc="/ihme/dex/us_county/r_library")
library(readxl)
here <- dirname(if(interactive()) rstudioapi::getSourceEditorContext()$path else rprojroot::thisfile())


source("/ihme/cc_resources/libraries/current/r/get_ids.R")
source("/ihme/cc_resources/libraries/current/r/get_covariate_estimates.R")
source("/ihme/cc_resources/libraries/current/r/get_population.R")
#source("/ihme/dex/us_county/pub_repos/fgh/FUNCTIONS/currency_conversion.R")
source(paste0(here, "/../../5_misc/us_deflate.R"))


# Set up the metadata
release_id <- 9 # GBD 2021
compare_version_id <- 7987

usval_path <- "/ihme/resource_tracking/us_value/data"
data_path <- "/snfs1/Project/IRH/US_value/Data/"
# ------------------------------------------------------------------------------------------------

# Covariates needed:
# 'age65', -- OK
# 'location_id', 'year_id',  -- OK
# 'density_l.150', 'density_g.1000', -- OK
# 'cig_pc', -- OK
# 'location_name', -- OK
# 'edu_yrs' -- OK
# 'obesity', -- OK   
# 'pct_NHwhite', -- OK    
# 'phys_act_10', -- OK
# 'ldi_pc' -- OK 
# 'spending_adj_pc' - uninflate and IRPD inflate

locs <- get_ids("location")
ages <- get_ids("age_group")
states <- locs[locs$location_id>=523 & locs$location_id<=573] %>%  # 51 states including DC
  select(location_id, location_name)
state_ids <- states[,location_id]
year_ids <- 1991:2021

# -------------------------------------------------------------------------------------------
# Covariates from state paper -------------------------------------------------------------------------------------------

covariates <- fread("/ihme/resource_tracking/us_value/data/state_paper_covariate_data_2021.csv") %>%
  mutate(age65 = age65_125, bmi=bmi_std) %>% 
  left_join(locs[ , 1:2], by = "location_id") %>% 
  select(location_name, everything()) 

# extend cig 15 so there are no NAs
covariates <- as.data.table(covariates)
cig_lag <- covariates[year_id==1995, .(cig_pc_15_1995 = cig_pc_15, location_name)]
covariates <- merge(covariates, cig_lag, by = "location_name")
covariates[ year_id < 1995, cig_pc_15 := cig_pc_15_1995][, cig_pc_15_1995:=NULL]

# --------------------------------------------------------------------------------------------  

# OBESITY  
# Obesity estimates are age and sex-specific. Aggregate here.

mergeCols <- c("location_id", "location_name", "year_id")  

age_ids <- c(2:20, 20:32, 34, 235, 238, 388:389)

pop <- get_population(year_id = 1980:2021, 
                      location_id = state_ids, 
                      sex_id=1:2, 
                      age_group_id = age_ids,
                      release_id = release_id,
                      status = 'best') 

obesity <- get_covariate_estimates(covariate_id = 453, # obesity
                                   location_id = state_ids,
                                   year_id =  year_ids,
                                   release_id = release_id,
                                   status = 'best') %>%
  left_join(pop, by = c("location_id", "year_id", "sex_id", "age_group_id")) %>% 
  group_by(location_id, year_id) %>%
  mutate(popsum = sum(population)) %>%
  mutate(pop_wt = population/popsum) %>%
  mutate(weighted_val = mean_value*pop_wt) %>%
  summarise(obesity = sum(weighted_val))
# --------------------------------------------------------------------------------------------  

# SBP - 70
# estimates are sex-specific. Aggregate here.

sbp <- get_covariate_estimates(covariate_id = 70, # sbp
                               location_id = state_ids,
                               year_id =  year_ids,
                               release_id = release_id,
                               status = 'best')%>%
  left_join(pop[age_group_id==22], by = c("location_id", "year_id", "sex_id")) %>% 
  group_by(location_id, year_id) %>%
  mutate(popsum = sum(population)) %>%
  mutate(pop_wt = population/popsum) %>%
  mutate(weighted_val = mean_value*pop_wt) %>%
  summarise(sbp = sum(weighted_val))

# --------------------------------------------------------------------------------------------  

# Diabetes - 30 - DEPRECATED
# estimates are age and sex-specific. Aggregate here.
# 
# diabetes <- get_covariate_estimates(covariate_id = 30, # sbp
#                                location_id = state_ids,
#                                year_id =  year_ids,
#                                release_id = release_id,
#                                status = 'best') %>%
#   left_join(pop, by = c("location_id", "year_id", "sex_id", "age_group_id")) %>% 
#   group_by(location_id, year_id) %>%
#   mutate(popsum = sum(population)) %>%
#   mutate(pop_wt = population/popsum) %>%
#   mutate(weighted_val = mean_value*pop_wt) %>%
#   summarise(prev_diabetes = sum(weighted_val))

# --------------------------------------------------------------------------------------------  

# cholesterol
# estimates are sex-specific. Aggregate here.

cholest <- get_covariate_estimates(covariate_id = 69,
                                   location_id = state_ids,
                                   year_id =  year_ids,
                                   release_id = release_id,
                                   status = 'best') %>%
  left_join(pop[age_group_id==22], by = c("location_id", "year_id", "sex_id")) %>% 
  group_by(location_id, year_id) %>%
  mutate(popsum = sum(population)) %>%
  mutate(pop_wt = population/popsum) %>%
  mutate(weighted_val = mean_value*pop_wt) %>%
  summarise(cholesterol = sum(weighted_val))

# --------------------------------------------------------------------------------------------  

#smoking prev 145
# estimates are age and sex-specific. Aggregate here.


smoking <- get_covariate_estimates(covariate_id = 145,
                                   location_id = state_ids,
                                   year_id =  year_ids,
                                   release_id = release_id,
                                   status = 'best') %>%
  left_join(pop, by = c("location_id", "year_id", "sex_id", "age_group_id")) %>% 
  group_by(location_id, year_id) %>%
  mutate(popsum = sum(population)) %>%
  mutate(pop_wt = population/popsum) %>%
  mutate(weighted_val = mean_value*pop_wt) %>%
  summarise(prev_smoking = sum(weighted_val))

# mean hemoglobin 1140 - DEPRECATED
# # estimates are sex-specific. Aggregate here.
# 
# hem <- get_covariate_estimates(covariate_id = 1140,
#                                    location_id = state_ids,
#                                    year_id =  year_ids,
#                                release_id = release_id,
#                                    status = 'best') %>%
#   left_join(pop[age_group_id==22], by = c("location_id", "year_id", "sex_id")) %>% 
#   group_by(location_id, year_id) %>%
#   mutate(popsum = sum(population)) %>%
#   mutate(pop_wt = population/popsum) %>%
#   mutate(weighted_val = mean_value*pop_wt) %>%
#   summarise(hemoglobin = sum(weighted_val))


# --------------------------------------------------------------------------------------------  

# PHYSICAL ACTIVITY (need to lag by 10 years)-- population weighted by sex and age

#Total Physical Activity (MET-min/week), Age-specific: 1977

phys_act <- get_covariate_estimates(covariate_id = 1977,
                                    year_id = 1980:2022, 
                                    location_id = state_ids, 
                                    release_id = release_id,
                                    status = 'best') %>%
  left_join(pop) %>%
  filter(age_group_id>9) %>% # drop everyone below age 25 ... not included in the model
  group_by(location_id, year_id) %>%
  mutate(popsum=sum(population)) %>%
  mutate(pop_wt = population/popsum) %>%
  mutate(weighted_val = mean_value*pop_wt) %>%
  summarise(phys_act=sum(weighted_val)) %>%
  slide(Var="phys_act",GroupVar="location_id",slideBy=-10) %>%
  mutate(phys_act_10=`phys_act-10`) %>%
  filter(year_id>1990) %>%
  select(location_id, year_id, phys_act, phys_act_10)

# ------------------------------------------------------------------------------------


# add state employement rate, need to agg from county
unemployment <- fread("/mnt/share/dex/us_county/05_analysis/inputs/county_ushd/ushd_covs_county.csv")[covariate == 'unemployed']
c_pop <- fread("/mnt/share/dex/us_county/maps/dex_population/best/pop_age_sex.csv")[geo == 'county']
c_pop <- c_pop[,.(pop = sum(pop)), by = c('year_id', 'state','location')]
c_pop[, mcnty := as.numeric(location)]
st_abbrevs <- fread("/mnt/share/dex/us_county/maps/states.csv")[,.(state_name,location_id, state=abbreviation)]

unemployment <- merge(unemployment, c_pop, by = c('mcnty', 'year_id'))
unemployment <- unemployment[,.(unemployment_rate = sum(cov_value * pop)/(sum(pop))), by = c('year_id', 'state')]
unemployment <- merge(unemployment, st_abbrevs, by = 'state')

unemployment <- unemployment[,.(location_id, year_id, unemployment_rate)]
# extend back to 1991 very roughly
unemployment2000 <- unemployment[year_id == 2000][, year_id := NULL]
unemployment2000 <- tidyr::crossing(unemployment2000, year_id = c(1991:1999)) %>% as.data.table()
unemployment <- rbind(unemployment2000, unemployment)


# add homelessness
extract_homeless <- function(sheet_name){
  
  #sheet_name <- '2019'
  
  homeless_rate <- read_xlsx("/snfs1/Project/IRH/dex_us_county/state_envelope/data/policy_vars/unhoused_prop/2007-2023-PIT-Counts-by-State.xlsx", sheet = sheet_name) %>% as.data.table()
  homeless_rate <- homeless_rate[,.(State, `Overall Homeless`, `Overall Homeless Individuals`, `Sheltered Total Homeless Individuals`)]
  homeless_rate <- homeless_rate[,.(state = State, homeless_ct = `Overall Homeless`, 'year_id' = as.numeric(sheet_name))]
  
  return(homeless_rate)
}

homeless_df <- rbindlist(lapply(as.character(2007:2023), extract_homeless))
s_pop <- fread("/mnt/share/dex/us_county/maps/dex_population/best/pop_age_sex.csv")[geo == 'state']
s_pop <- s_pop[,.(pop = sum(pop)), by = c('year_id', 'state')]
s_pop <- merge(s_pop, st_abbrevs, by = 'state')

homeless_df <- merge(homeless_df, s_pop, by = c('year_id', 'state'))
homeless_df[, prop_homeless := homeless_ct / pop]
homeless_df[, `:=` (homeless_ct = NULL, pop = NULL, state = NULL, state_name = NULL)]

# extend back to 1991 very roughly
homeless_ref <- homeless_df[year_id == 2007][, year_id := NULL]
homeless_ref <- tidyr::crossing(homeless_ref, year_id = c(1991:2006)) %>% as.data.table()
homeless_df <- rbind(homeless_ref, homeless_df)



# ------------------------------------------------------------------------------------



# RACE (% non-Hispanic white)

# # Load data from Laura's team
# race <- readRDS("/snfs1/Project/us_counties/population/counties/prepped/pop_by_race_ethn_1977_1990_to_2019/2021_09_16_12_59_06.rds")
# postal_codes <- read.csv("/ihme/resource_tracking/us_value/data/archive/laura_postal_codes.csv")
# 
# recode_re <- c(
#   "1" = "white",
#   "2" = "black",
#   "3" = "aian",
#   "4" = "asian",
#   "5" = "asian",
#   "6" = "mult",
#   "7" = "hisp")
# race[,re := plyr::revalue(as.character(race), recode_re)]
# race <- race[year %in% 1991:2019,.(race_pop = sum(pop, na.rm = T)), by = .(year,state,re)]
# total_re <- race[,.(race_pop = sum(race_pop, na.rm = T), re = "total"), by = .(year, state)] %>% 
#   rename(total_pop = race_pop)
# race <- race %>% left_join(total_re, by = c("year", "state")) %>% 
#   mutate(pct_race = race_pop/total_pop) %>% 
#   filter(re.x == "white") %>% 
#   rename(pct_NHwhite = pct_race,
#          laura_id = state) %>% 
#   left_join(postal_codes, by = "laura_id") %>% 
#   left_join(states, by = "location_name") %>% 
#   select(location_name, location_id, year, pct_NHwhite)
# setnames(race, "year", "year_id")

#fwrite(race, "/ihme/resource_tracking/us_value/data/race2.csv")

# ------------------------------------------------------------------------------------


# LOAD SHEA ------------------------------------------------------------------
shea <- "/snfs1/DATA/Incoming Data/USA/cms_health expenditure accounts/residence state estimates/US_AGGREGATE20.CSV"


# Expects a csv that contains total spending in millions of dollars
process_shea <- function(f){
  df <- fread(f)
  df <- df[str_detect(Item, "Personal Health Care")]
  df <- melt(df, id.vars = c("Region_Number","State_Name","Item"), measure.vars = patterns("Y\\d{4}"),
             variable.name = "year_id", value.name = "tot_spending")
  df[, year_id := as.numeric(str_remove(year_id,"Y"))][, iso3 := "USA"][, year := year_id]
  df[, model := str_remove(Item, "/Personal Health Care \\(Millions of Dollars\\)")][, Item := NULL]
  setnames(df,"State_Name","location_name")
  df[, tot_spending := 1000000*tot_spending]
  
  #df <- df[model == "Personal Health Care (Millions of Dollars)"]
  df[, model := NULL]
  df[, Region_Number := NULL]
  #df <- df[year_id != 2020]
  df <- df[location_name != ""]
  
  return(df)
}

# Get data set of SHEA, in constant currency 
cms_data <- rbindlist(lapply(shea, process_shea))

# import IRPD
irpd <- read_csv(paste0(data_path,"/BEA/IRPD_2008-19.csv")) %>%
  select(-GeoFips) %>%
  reshape2::melt(id.var="GeoName", 
                 variable.name= "year", 
                 value.name="irpd") %>% 
  mutate(irpd_100 = irpd/100,
         location_name=GeoName,
         irpd_year=as.numeric(as.character(year))) %>%
  select(-c(GeoName, year)) %>% as.data.table()

# duplicate 2019 for 2020
irpd <- rbind(irpd, copy(irpd[irpd_year == 2019][,  irpd_year := 2020]))


# We only have IRPD for 2008-2019, so we want to inflate estimates pre 2008 up to 2008
cms_inflated91_07 <- cms_data %>%
  filter(year_id<2008) %>%
  #mutate(Year=year_id) %>%
  deflate(val_columns = 'tot_spending',
          old_year = 'year_id', 
          new_year = 2008) %>%
  select(-iso3) %>% rename(spending2008 = tot_spending)


# For 2008-2019, use uninflated estimates

final_spending <- 
  # take population, rpp and cpi from cov file
  covariates[, c("location_name", "location_id", "year_id", "population", "rpp", "cpi")] %>% 
  # all years of CMS data (constant total spending)
  left_join(cms_data, by = c("year_id", "location_name")) %>% 
  # pre 2008 of CMS data (inflated to 2008, total spending)
  left_join(cms_inflated91_07, by = c("year_id", "location_name")) %>% 
  # use the 2008 IRPD value for pre 2008
  mutate(irpd_year=ifelse(year_id<2008, 2008, year_id)) %>%
  left_join(irpd, by = c("irpd_year", "location_name")) %>% 
  # convert to pc spending, using either2008 spending or total spending
  mutate(spending_unadj_pc = ifelse(year_id<2008, spending2008 / population, tot_spending / population)) %>% 
  
  # divide the estimate by IRPD (which was divided by 100)
  mutate(spending_adj_pc = spending_unadj_pc/irpd_100)  %>%
  as.data.table()


# The IRPD estimates are relative to 2012 USD, so inflate up to 2019
final_spending <- final_spending[, currency_year := 2012] %>%
  deflate(val_columns = 'spending_adj_pc',
          old_year = 'currency_year', 
          new_year = 2020)

# Remove unnecessary columns
final_spending <- final_spending[,.(location_name, location_id, year_id, spending_adj_pc, irpd_100)]


# draw_skel <- as.data.table(expand_grid("draw"=1:1000, "location_name"=states$location_name))
# add_draws <- merge(final_spending, draw_skel, by = "location_name", allow.cartesian = T)
# final_spending <- as.data.table(dcast(add_draws, ... ~ paste0("spending_adj_pc_", draw), value.var = "spending_adj_pc"))
# 



# -----------------------------------------------------------------------------------

sfa_covars <- covariates[, c("location_id",
                             "location_name",
                             "year_id",
                             'age65',
                             'density_l.150', 'density_g.1000',
                             'cig_pc', 'cig_pc_10', 'cig_pc_15',
                             'bmi',
                             'edu_yrs',
                             'ldi_pc')] %>% 
  left_join(obesity, by = c("location_id", "year_id")) %>% 
  left_join(unemployment, by = c("location_id", "year_id")) %>% 
  left_join(homeless_df, by = c("location_id", "year_id")) %>% 
  left_join(phys_act, by = c("location_id", "year_id")) %>% 
  left_join(sbp, by = c("location_id", "year_id")) %>% 
  #left_join(hem, by = c("location_id", "year_id")) %>% 
  left_join(cholest, by = c("location_id", "year_id")) %>% 
  left_join(smoking, by = c("location_id", "year_id")) %>% 
  #left_join(diabetes, by = c("location_id", "year_id")) %>% 
  #left_join(race, by = c("location_name", "location_id", "year_id")) %>%
  left_join(final_spending, by = c("location_name", "location_id", "year_id")) %>%
  as.data.table()


path <- "/ihme/resource_tracking/us_value/data/sfa_covars2021_shea.csv"

if(file.exists(path)){
  
  old <- fread(path)
  write.csv(old, paste0("/ihme/resource_tracking/us_value/data/archive/sfa_covars2021_shea_", Sys.Date(), ".csv"))
  
}




fwrite(sfa_covars, "/ihme/resource_tracking/us_value/data/sfa_covars2021_shea.csv")




# Added for 2025 paper sensitivity by hkl1

# save state race fractions

template <- fread("/ihme/resource_tracking/us_value/data/sfa_covars2021_shea.csv")
state_race_fractions <- fread('/mnt/share/dex/us_county/maps/dex_population/v6/state_county_population_age_sex_race.csv')


state_race_fractions <- state_race_fractions[,.(pop = sum(pop)), by = c('race_cd', 'state_name', 'abbreviation', 'year_id')]
state_race_fractions[, tot_pop := sum(pop), by = c('state_name', 'year_id')]
state_race_fractions[, race_frac := pop/tot_pop]


state_race_fractions_wide <- dcast(state_race_fractions, state_name + year_id ~ race_cd, value.var = 'race_frac')
setnames(state_race_fractions_wide, unique(state_race_fractions$race_cd), 
         paste0('race_prop_', unique(state_race_fractions$race_cd)))



save_race_data <- merge(template, state_race_fractions_wide, by.x = c('location_name', 'year_id'),
                        by.y = c('state_name', 'year_id'))

write.csv(save_race_data, "/ihme/resource_tracking/us_value/data/sfa_covars_w_race_fractions.csv", row.names = F)

