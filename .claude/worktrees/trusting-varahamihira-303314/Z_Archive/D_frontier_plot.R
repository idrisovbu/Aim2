## ----------------------------------------
## SFMA Frontier Plot
## ----------------------------------------
rm(list = ls())

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

library(stringr)
library(data.table)
library(arrow)
library(parallel)
library(matrixStats)
library(ggplot2)
source("/ihme/cc_resources/libraries/current/r/get_age_metadata.R")

##----------------------------------------------------------------
## 0.0 Functions
##----------------------------------------------------------------
ensure_dir_exists <- function(dir_path) {
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
}

'%nin%' <- Negate('%in%')

##----------------------------------------------------------------
## 0.1 Set directories for input data
##----------------------------------------------------------------
# Set input path for data
date_fa <- "20260105"
dir_fa <- file.path(h, "/aim_outputs/Aim2/C_frontier_analysis/", date_fa)

# Read in data
fp_hiv <- file.path(dir_fa, "hiv_output.csv")
fp_sud <- file.path(dir_fa, "_subs_output.csv")

df_hiv <- read.csv(fp_hiv)
df_sud <- read.csv(fp_sud)

# Set output directory
date_today <- format(Sys.time(), "%Y%m%d")
dir_out <- file.path(h, "/aim_outputs/Aim2/D_tables_figures/", date_today)
ensure_dir_exists(dir_out)

##----------------------------------------------------------------
## 1.0 Create plot
##----------------------------------------------------------------

df_hiv %>%
  arrange(as_spend_prev_ratio_copy) %>%
  ggplot(aes(x = as_spend_prev_ratio_copy)) +
  geom_point(aes(y = y_adj),  col = '#2678B2') +
  geom_line(aes(y = y_adj_hat), color = "orange") +
  labs(y = 'Adjusted mortality/prevalence ratio', x = 'Spending/prevalence ratio', title = 'Observed Outcomes and Estimated Value Frontier, SFMA - HIV')

df_sud %>%
  arrange(as_spend_prev_ratio_copy) %>%
  ggplot(aes(x = as_spend_prev_ratio_copy)) +
  geom_point(aes(y = y_adj),  col = '#2678B2') +
  geom_line(aes(y = y_adj_hat), color = "orange") +
  labs(y = 'Adjusted mortality/prevalence ratio', x = 'Spending/prevalence ratio', title = 'Observed Outcomes and Estimated Value Frontier, SFMA - Subtance use disorder')

##----------------------------------------------------------------
## REFERENCE CODE BELOW
##----------------------------------------------------------------

# if(mortality_cause_weights){
#   cause_list <- fread('/ihme/resource_tracking/us_value/data/metadata/cause_list.csv')
#   #cause_list <- fread('/ihme/resource_tracking/us_value/data/metadata/cause_list_top9.csv')
#   cause_weights <- fread('/ihme/resource_tracking/us_value/data/input_data/agg/best/treatment_cause_weights.csv')
# }else{
#   print("Using the incidence cause hierarchy!")
#   cause_list <- fread('/ihme/resource_tracking/us_value/data/metadata/cause_list_attributable_dalys.csv')
#   cause_weights <- fread('/ihme/resource_tracking/us_value/data/agg/prevention_cause_weights.csv')
# }
# 
# # order causes by weight
# cause_list <- merge(cause_list, cause_weights[year_id==max(year_id),.(cause_id, cause_weight)])
# cause_name_order <- cause_list[rev(order(cause_weight))][1:top_causes, .(cause_id, cause_name)]
# cause_ids <- cause_name_order$cause_id
# 
# # read in data draws
# data_dir <- paste0('/ihme/resource_tracking/us_value/data/sfa_output/sensitivity/', stamp)
# 
# p_files <- list.files(data_dir, full.names = T)
# cause_regex <- paste(paste0(cause_ids, "_d"), collapse = "|")
# p_files <- p_files[str_detect(p_files, cause_regex)]
# 
# cl <- makeCluster(10, type="FORK")
# ps <- parLapply(cl,
#                 p_files,
#                 fread)
# stopCluster(cl)
# 
# p_df <- rbindlist(ps, use.names = T, fill = T)
# 
# rm(ps)
# 
# p_df[is.na(ineff), .N, by = 'cause_id'] 
# p_df <- p_df[!is.na(ineff)]
# 
# 
# p_df <- merge(p_df, cause_name_order[,.(cause_id, cause_name)], by = "cause_id")
# p_df[, cause_name := factor(cause_name, levels = cause_name_order$cause_name)]
# 
# if(! "adj_exp_pc_c" %in% colnames(p_df)){
#   p_df$adj_exp_pc_c <- p_df$adj_exp_pc
# }
# 
# 
# if(!draw_only){
#   ## take means and UIs
#   p_df_stats <- p_df[,.(
#     mean_adj_exp_pc = mean(adj_exp_pc_c), #unnormalized spending column 
#     mean_y_adj = mean(y_adj), 
#     mean_y_adj_hat = mean(y_adj_hat),
#     lower_y_adj_hat = quantile(y_adj_hat, .025),
#     upper_y_adj_hat = quantile(y_adj_hat, .975)
#   ), by = .(cause_name, location_name, year_id)]
#   
#   
#   
#   if(include_uis){
#     ui <- p_df_stats %>%
#       ggplot(aes(x = mean_adj_exp_pc)) +
#       geom_point(aes(y = mean_y_adj), col = '#2678B2') +
#       geom_line(aes(y = mean_y_adj_hat), col = 'orange') +
#       geom_ribbon(aes(ymin = lower_y_adj_hat, ymax = upper_y_adj_hat), fill = 'orange', alpha = .4) +
#       facet_wrap(~cause_name, scales = 'free') +
#       labs(y = 'Adjusted mortality-incidence/prevalence ratio', x = 'Health care spending per capita') +
#       theme(panel.background = element_rect(color = 'black', fill = 'white'),
#             panel.grid = element_blank(),
#             strip.background = element_blank())
#     fn <- paste0(dir_out, '/Figure_1_UI_', stamp,'.pdf')
#     ggsave(plot = ui, filename = fn, height = 6, width = 9)
#   }else{
#     
#     
#     mean <- p_df_stats %>%
#       ggplot(aes(x = mean_adj_exp_pc)) +
#       geom_point(aes(y = mean_y_adj), col = '#2678B2') +
#       geom_line(aes(y = mean_y_adj_hat), col = 'orange') +
#       facet_wrap(~cause_name, scales = 'free') +
#       labs(y = 'Adjusted mortality-incidence/prevalence ratio', x = 'Health care spending per capita') +
#       theme(panel.background = element_rect(color = 'black', fill = 'white'), 
#             panel.grid = element_blank(),
#             strip.background = element_blank())
#     fn <- paste0(dir_out, '/Figure_1_', stamp,"_top", length(cause_ids),'.pdf')
#     ggsave(plot = mean, filename = fn, height = 6, width = 9)
#   }
#   
#   
#   if(state_pops){
#     
#     pdf(paste0(dir_out, '/Statespecific_Figure_1_', stamp,"_top", length(cause_ids),'.pdf'), height = 6, width = 9)
#     
#     for(s in unique(p_df_stats$location_name)){
#       
#       p_df_stats[, state_flag := 0]
#       p_df_stats[location_name == s, state_flag := 1]
#       
#       mean <- p_df_stats %>%
#         ggplot(aes(x = mean_adj_exp_pc)) +
#         geom_point(data = p_df_stats[state_flag != 1 ], aes(y = mean_y_adj), color = "grey") +
#         geom_point(data = p_df_stats[state_flag == 1 ], aes(y = mean_y_adj), color = "#2678B2") +
#         geom_line(aes(y = mean_y_adj_hat), col = 'orange') +
#         facet_wrap(~cause_name, scales = 'free') +
#         labs(y = 'Adjusted mortality-incidence/prevalence ratio', x = 'Health care spending per capita', title = s) +
#         theme(panel.background = element_rect(color = 'black', fill = 'white'), 
#               panel.grid = element_blank(),
#               strip.background = element_blank())
#       
#       print(mean)
#       
#     }
#     
#     dev.off()
#     
#   }
#   
#   
#   
#   
# }else{
#   
#   
#   fn <- paste0(dir_out, '/Figure_1_draw_', stamp,"_top", length(cause_ids),'.pdf')
#   pdf(fn, height = 6, width = 9)
#   
#   for(d in c(0,199,250,500,750,999)){
#     
#     p_df_stats <- p_df[draw ==d]
#     
#     draw <- p_df_stats %>%
#       ggplot(aes(x = adj_exp_pc_c)) +
#       geom_point(aes(y = y_adj), col = '#2678B2') +
#       geom_line(aes(y = y_adj_hat), col = 'orange') +
#       facet_wrap(~cause_name, scales = 'free') +
#       labs(y = 'Adjusted mortality-incidence/prevalence ratio', x = 'Health care spending per capita', title = paste0("Draw ", d)) +
#       theme(panel.background = element_rect(color = 'black', fill = 'white'), 
#             panel.grid = element_blank(),
#             strip.background = element_blank())
#     
#     print(draw)
#     
#     
#   }
#   dev.off()
#   
# }

