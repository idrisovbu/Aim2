#------------------------------------
#### FRONTIER ANALYSIS USING R PACKAGE AND NOT THE IHME SFMA PACKAGE
##    AS A ROBUSTNESS CHECK


#
# hkl1, 7/22/2025
#------------------------------------

Sys.umask(mode = 002)
pacman::p_load(data.table, tidyverse, ggplot2, glue)

# main frontier package
library('frontier')

# static directories
work_dir <- '/mnt/share/resource_tracking/us_value/data/sfa_output/sensitivity/'
df_path <- '/mnt/share/resource_tracking/us_value/data/input_data/agg/best/demean_MI_MP_draws.csv' 
cov_df_path <- '/ihme/resource_tracking/us_value/data/sfa_covars2021_shea.csv'

# LOAD ANY RELEVANT ARGS IF LAUNCHING
#-----------------------------------



# SET UP DIRECTORIES, LOAD MAIN DATA, DETERMINE CAUSE + DRAWS TO ITERATE OVER
#-----------------------------------

#timestamp <- paste0(format(Sys.time(), "%d%b%Y_%H%M"), '_', 'frontier_R_package_no_covs')
timestamp <- "22Jul2025_1748_frontier_R_package_no_covs"
save_dir <- paste0(work_dir, timestamp)
#dir.create(save_dir)

# Set covariates
# covs <- c('obesity', 'age65', 'cig_pc_10',
#         'phys_act_10', 'edu_yrs', 'adj_exp_pc')
covs <- c('adj_exp_pc')

# Load data
all_data <- fread(df_path)
cov_df <- fread(cov_df_path)
all_data <- merge(all_data, cov_df, by=c('location_id', 'year_id'))
all_data <- all_data[location_name != 'District of Columbia' & year_id < 2021]

# some formatting of data
setnames(all_data, 'spending_adj_pc', 'adj_exp_pc')
all_data[, 'adj_exp_pc_c' := adj_exp_pc]

# covariate mean normalization
for( cov in unique(c(covs, 'adj_exp_pc'))){
  col <- all_data[,get(cov)]
  all_data[, paste0(cov) := (col - mean(col))/sd(col)]
}

all_data <- all_data[order(adj_exp_pc)]

draw_list <- colnames(all_data)[grepl('demeaned_draw', colnames(all_data))]
draw_list <- as.numeric(gsub("demeaned_draw_", "", draw_list))

# Set list of causes to run. each takes ~1-2 min
cause_list <- fread('/ihme/resource_tracking/us_value/data/metadata/cause_list.csv')
cause_weights <- fread('/ihme/resource_tracking/us_value/data/input_data/agg/best/treatment_cause_weights.csv')
# order causes by weight
cause_list <- merge(cause_list, cause_weights[year_id==max(year_id),.(cause_id, cause_weight)])
cause_name_order <- cause_list[rev(order(cause_weight))]

top9_cause_ids <- c(493, 426, 509, 441, 976, 429, 693, 496, 456)

cause_ids <- setdiff(cause_name_order$cause_id, top9_cause_ids)

# SET SFA FIT FUNCTION
#   Using the default fitting method in the frontier package, (Battese and Coelli (1992) specification — the error components model with time-varying inefficiency).
#-----------------------------------
fit_sfa_nocovs <- function(cause, draw, data, return_fig = FALSE) {
  
  draw_column <<- glue('demeaned_draw_{draw}')
  df <- data[cause_id == cause]
  demean_cols <- colnames(df)[grepl('demeaned_draw', colnames(df))]
  demean_cols <- setdiff(demean_cols, draw_column)
  df[, (demean_cols) := NULL]
  
  
  # Rescale because y variable can't be negative with the sfa function
  # Magnitude seems to matter too, so moving everything to have similar magnitude
  rescale_to_mean_sd <- function(x, target_mean = 0.6, target_sd = 0.1) {
    scaled <- (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
    rescaled <- scaled * target_sd + target_mean
    return(rescaled)
  }
  
  df[, paste0(draw_column) := rescale_to_mean_sd(df[, get(draw_column)])]
  
  if (nrow(df[get(draw_column) < 0]) > 0) {
    stop('some output is negative!')
  }
  
  # Fit model, single covariate for now
  # IneffDecrease = FALSE is important: indicates that more inefficiency in a data point INCREASES the y variable (high MI ratio)
  # RestartFactor also seems to have mild effect 
  mod_form <<- as.formula(paste0(draw_column, " ~ ", paste0(covs, collapse = " + ")))
  
  model <- sfa(mod_form, data = df, ineffDecrease = FALSE, restartMax = 5, restartFactor = 0.01) 
  
  df$prediction <- predict(model, asInData = TRUE, logDepVar = FALSE)
  
  df_null_covs <- copy(df)
  null_covs <- setdiff(covs, "adj_exp_pc")
  df_null_covs[, (null_covs) := 0]
  df_null_covs$null_prediction <- predict(model, newdata = df_null_covs, logDepVar = FALSE)
  
  df <- merge(df, df_null_covs[, .(location_id, year_id, cause_id, null_prediction)],
              by = c('location_id', 'year_id', 'cause_id'))
  df[, cov_hat := prediction - null_prediction]
  df[, y_adj := get(draw_column) - cov_hat]
  
  # Can't impose monotonic decrease, so check direction and set to flat if it is increasing
  check_dir <- cor(df$adj_exp_pc_c, df$null_prediction)
  if (check_dir >= 0) {
    
    # set prediction and null_prediction everywhere to be the minimum
    # both because just covs rn. 
    min_val <- min(df$null_prediction)
    min_val_check <- min(df$prediction)
    if(min_val != min_val_check){stop('are you running with multiple predictors? need to revisit setting to flat line with multiple')}
    df[, `:=` (prediction = min_val, null_prediction = min_val)]
    
  }
  
  
  if (return_fig) {
    plot <- ggplot(df, aes(x = adj_exp_pc_c)) +
      geom_point(aes(y = y_adj), col = '#2678B2') +
      geom_line(aes(y = null_prediction), col = 'orange')
    return(plot)
  }
  
  # Calculate inefficiency as vertical distance between observered and predicted (predicted should be LOWER)
  df[, ineff_raw := get(draw_column) - prediction]
  df[ineff_raw < 0, ineff_raw := 0]  # if predicted is higher, perfectly efficient so set to 0
  # Rescale ineff to match what we do in sfma code
  df[, ineff := (ineff_raw - min(ineff_raw)) / (max(ineff_raw) - min(ineff_raw))]
  
  fit_output <- df[, .(location_name, year_id, adj_exp_pc, cause_id, adj_exp_pc_c,
                       mi_ratio = get(draw_column), ineff, y_adj,
                       y_adj_hat = null_prediction,
                       draw = paste0(draw), ineff_raw)]
  
  return(fit_output)
}


# RUN SFA
#-----------------------------------
fit_sfa_nocovs(cause = 563, draw = 10, data = all_data, return_fig = T)

# 563 seems to have glitched! Draw 2.

for (causeid in cause_ids) {
  
  print(causeid)
  t0 <- Sys.time()
  
  results <- rbindlist(lapply(draw_list, function(d) {
    #message(d)
    return(fit_sfa_nocovs(cause = causeid, draw = d, data = all_data))
  }))
  
  print(Sys.time() - t0)
  
  write.csv(results, paste0(save_dir, '/inefficiency_', causeid, "_d500.csv"), row.names = FALSE)
}




# TESTING
#--------------------------------------
# - looks like mean, sd, and increment value don't really change anything
fit_sfa_nocovs_testing <- function(cause, draw, data, mean_y, sd_y, inc_val, return_fig = TRUE) {
  
  draw_column <<- glue('demeaned_draw_{draw}')
  df <- data[cause_id == cause]
  demean_cols <- colnames(df)[grepl('demeaned_draw', colnames(df))]
  demean_cols <- setdiff(demean_cols, draw_column)
  df[, (demean_cols) := NULL]
  
  inc_val <<- inc_val
  
  # Rescale because y variable can't be negative with the sfa function
  # Magnitude seems to matter too, so moving everything to have similar magnitude
  rescale_to_mean_sd <- function(x, target_mean = mean_y, target_sd = sd_y) {
    scaled <- (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
    rescaled <- scaled * target_sd + target_mean
    return(rescaled)
  }
  
  df[, paste0(draw_column) := rescale_to_mean_sd(df[, get(draw_column)])]
  
  if (nrow(df[get(draw_column) < 0]) > 0) {
    stop('some output is negative!')
  }
  
  # Fit model, single covariate for now
  # IneffDecrease = FALSE is important: indicates that more inefficiency in a data point INCREASES the y variable (high MI ratio)
  # RestartFactor also seems to have mild effect 
  mod_form <<- as.formula(paste0(draw_column, " ~ ", paste0(covs, collapse = " + ")))
  
  model <- sfa(mod_form, data = df, ineffDecrease = FALSE, restartMax = 20, restartFactor = inc_val) 
  
  df$prediction <- predict(model, asInData = TRUE, logDepVar = FALSE)
  
  df_null_covs <- copy(df)
  null_covs <- setdiff(covs, "adj_exp_pc")
  df_null_covs[, (null_covs) := 0]
  df_null_covs$null_prediction <- predict(model, newdata = df_null_covs, logDepVar = FALSE)
  
  df <- merge(df, df_null_covs[, .(location_id, year_id, cause_id, null_prediction)],
              by = c('location_id', 'year_id', 'cause_id'))
  df[, cov_hat := prediction - null_prediction]
  df[, y_adj := get(draw_column) - cov_hat]
  
  # Can't impose monotonic decrease, so check direction and set to flat if it is increasing
  check_dir <- cor(df$adj_exp_pc_c, df$null_prediction)
  if (check_dir >= 0) {
    
    # set prediction and null_prediction everywhere to be the minimum
    # both because just covs rn. 
    min_val <- min(df$null_prediction)
    min_val_check <- min(df$prediction)
    if(min_val != min_val_check){stop('are you running with multiple predictors? need to revisit setting to flat line with multiple')}
    df[, `:=` (prediction = min_val, null_prediction = min_val)]
    
  }
  
  
  if (return_fig) {
    plot <- ggplot(df, aes(x = adj_exp_pc_c)) +
      geom_point(aes(y = y_adj), col = '#2678B2') +
      geom_line(aes(y = null_prediction), col = 'orange')+
      labs(title = paste0("cause ",cause, ": d", draw ), 
           subtitle = paste0("y scaled to ", mean_y, "(", sd_y, ") with sfa increment: ", inc_val))
    return(plot)
  }
  
  # Calculate inefficiency as vertical distance between observered and predicted (predicted should be LOWER)
  df[, ineff_raw := get(draw_column) - prediction]
  df[ineff_raw < 0, ineff_raw := 0]  # if predicted is higher, perfectly efficient so set to 0
  # Rescale ineff to match what we do in sfma code
  df[, ineff := (ineff_raw - min(ineff_raw)) / (max(ineff_raw) - min(ineff_raw))]
  
  fit_output <- df[, .(location_name, year_id, adj_exp_pc, cause_id, adj_exp_pc_c,
                       mi_ratio = get(draw_column), ineff, y_adj,
                       y_adj_hat = null_prediction,
                       draw = paste0(draw), ineff_raw)]
  
  return(fit_output)
}
fit_sfa_nocovs_testing(cause = 509, draw = 10, data = all_data, mean_y = 6, sd_y = 0.1, inc_val = 0.99)
fit_sfa_nocovs_testing(cause = 509, draw = 10, data = all_data, mean_y = 0.5, sd_y = 0.1, inc_val = 0.01)