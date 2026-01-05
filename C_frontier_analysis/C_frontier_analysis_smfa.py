"""
Filename: C_frontier_analysis_smfa
Description: Python script for running SFMA for Aim 2, trying to adapt H's code
"""

from pathlib import Path
from datetime import date
import numpy as np
import pandas as pd
import sys
import os
import shutil
import matplotlib.pyplot as plt
from sfma import Data, Variable, SplineVariable, SplineGetter, SplinePriorGetter, UniformPrior, SFMAModel
from anml.data.component import Component

# B's
def runSFA(acause='hiv'):
    """
    Docstring for runSFA
    
    :param cause: The cause to process the model for. 
        Either "hiv" or "_subs"
    :param testplot: Description
    """

    #### Default parameters ####
    #---------------------------------------
    
    # Set fp for age-standardized data
    fp_df_date = '20260104'
    fp_df_base = Path('/ihme/homes/idrisov/aim_outputs/Aim2/C_frontier_analysis/')
    fp_df_as = fp_df_base / fp_df_date / "df_as.csv"

    # Set fp for covariate data
    fp_df_cov = '/ihme/resource_tracking/us_value/data/sfa_covars2021_shea.csv'
    
    # Set covariates
    covs = ['obesity', 'age65', 'cig_pc_10', 'phys_act_10', 'edu_yrs', 'as_spend_prev_ratio']
    pre_selected_covs = ['as_spend_prev_ratio']
    no_prior_covs = [] # this was the default, only changed if we specified a variant to this function

    # Set column names for parameters
    spend_prev_col = 'as_spend_prev_ratio'
    mort_prev_col = 'as_mort_prev_ratio'
    spline_variable_name = None
    variance_column = 'variance'

    # Set model specifications
    test_cov_direction = True # drop covariates that aren't in right direction and rerun
    trimming = True #5%
    l_tail = r_tail = True # doesn't matter since default is no spline
    output_betas = False
    rescale = True 

    ####  Main section
    #---------------------------------------

    # Read in age-standardized data
    df_as = pd.read_csv(fp_df_as) 

    # Filter to our specified cause
    df_as = df_as[df_as['acause'] == acause]
    
    # Read in covariate data
    df_cov = pd.read_csv(fp_df_cov)

    # Filter covariate data to match age-standardized data's years
    as_years = [2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019]
    df_cov = df_cov[df_cov['year_id'].isin(as_years)]

    # Merge age-standardized data & covariate data
    df = pd.merge(df_as, 
              df_cov, 
              on=['location_id', 'year_id'])
    df.drop(columns=["location_name_y"], inplace=True)
    df.rename(columns={"location_name_x": "location_name"}, inplace=True)

    #df.rename(columns={'year': 'year_id', spending_var: 'adj_exp_pc'}, inplace=True) (skipped this)

    # drop DC (outlier) (skipped this)
    #df = df[df['location_name'] != 'District of Columbia']

    # preserve spending/prevalence ratio pre normalization
    df['as_spend_prev_ratio_copy'] = df[spend_prev_col]

    # covariate mean normalization (z-score standardization)
    for cov in np.unique(covs + [spend_prev_col]):
        df[cov] = (df[cov] - df[cov].mean())/df[cov].std()

    # lower envelope of the ratio draw - make it negative (original draw_column)
    df[mort_prev_col] = -df[mort_prev_col]
    
    # take median of the variance within a draw
    df[variance_column] = np.median(df[variance_column])
    
    # Very important to sort here! Will prevent indexes from getting messed up while plotting and pulling inefficiency
    df.sort_values(spend_prev_col, inplace = True)

    def get_model(df, covs, variance_column, obs_column, spline_column):
    
        # set priors on covariates
        no_prior_covs_model = list(set(covs).intersection(set(no_prior_covs)))
        pos_covs = list(set(['pct_NHwhite', 'phys_act_10', 'edu_yrs', 'ldi_pc', 'adj_exp_pc']).intersection(set(covs).difference(set(no_prior_covs))))
        neg_covs = list(set(['obesity', 'age65', 'cig_pc', 'cig_pc_15','cig_pc_10', 'density_g.1000', 'prop_homeless']).intersection(set(covs).difference(set(no_prior_covs))))
        
        # make weights 
        df['standard_error'] = np.sqrt(df[variance_column])
            
        # create data object
        data = Data(
            obs = obs_column,
            obs_se = 'standard_error',
        )

        # create variables and set prior signs
        variables = [Variable(Component("intercept", default_value=1.0))] + [
            Variable(name, priors=[UniformPrior(lb=0.0, ub=np.inf)])
            for name in pos_covs
        ] + [
            Variable(name, priors=[UniformPrior(lb=-np.inf, ub=0.0)])
            for name in neg_covs
        ] +[
            Variable(name)
            for name in no_prior_covs_model
        ]
        
        # convex prior and monotonic decreasing prior
        priors = [SplinePriorGetter(UniformPrior(lb=-np.inf, ub=0.0), order=2, size=100),
              SplinePriorGetter(UniformPrior(lb=0.0, ub=np.inf), order=1, size=100)]
        
        if spline_variable_name is not None:
            variables.append(
                SplineVariable(
                    spline_column,
                    spline = SplineGetter(
                        knots=np.array([0.0, 0.05, 0.01,  1.0]),
                        degree=3,
                        knots_type='rel_freq',
                        include_first_basis=False,
                        l_linear = l_tail,
                        r_linear = r_tail
                    ),
                    priors = priors
                )
            )
        
        model = SFMAModel(data, variables, include_re=False)
        model.attach(df)
        
        return model

    # create model object
    model = get_model(df, covs, variance_column, mort_prev_col, spline_variable_name)

    # drop covariates with very low beta under the prior and rerun #
    if test_cov_direction:
        model.eta = 0.1
        model.beta.fill(1.0)
        model.fit(
            verbose=True, max_iter=10, tol=1e-3,
            #beta_options={"solver_type": "ip", "max_iter": 1000, "verbose": False, "xtol": 0.0, "gtol": 1e-6, "update_mu_every": 10}
            beta_options={"max_iter": 1000, "verbose": False, "xtol": 0.0, "gtol": 1e-6} # "solver_type" and "update_mu_every" parameters not accepted? throws error
        )
        
        beta = model.get_beta_dict()
        
        selected_covs = []
        for name, value in beta.items():
            if value.size == 1 and name != "intercept" and np.abs(value[0]) > 1e-5:
                selected_covs.append(name)
        for cov in pre_selected_covs:
            if cov not in selected_covs:
                selected_covs.append(cov)
        print(selected_covs)

        model = get_model(df, selected_covs, variance_column, mort_prev_col, spline_variable_name)


    if trimming:        
        # fit model with 5% trimming
        model.eta = 0.1
        model.beta.fill(1.0)
        model.fit(
            outlier_pct=0.05, trim_max_iter=5,
            verbose=True, max_iter=5, tol=1e-3,
            #beta_options={"solver_type": "ip", "max_iter": 1000, "verbose": False, "xtol": 0.0, "gtol": 1e-6, "update_mu_every": 10}
            beta_options={"max_iter": 1000, "verbose": False, "xtol": 0.0, "gtol": 1e-6}
        ) 
    else:
        # fit model without trimming
        model.eta = 0.1
        model.beta.fill(1.0)
        model.fit(
            verbose=True, max_iter=10, tol=1e-3,
            #beta_options={"solver_type": "ip", "max_iter": 1000, "verbose": False, "xtol": 0.0, "gtol": 1e-6, "update_mu_every": 10}
            beta_options={"max_iter": 1000, "verbose": False, "xtol": 0.0, "gtol": 1e-6}
        )     
        
    # return covariate information
    if len(selected_covs) > 0:
        covs_df = pd.DataFrame({
            'selected_covs': selected_covs,
            'acause': acause
        })
        
        # add beta information
        beta = model.get_beta_dict()
        
    else:
        covs_df = pd.DataFrame({
            'selected_covs': ["no covs selected"],
            'acause': acause
        })
        
    # Add on betas to cov df
    betas = model.get_beta_dict()

    for name, value in betas.items():
      if value.size == 1 and name != 'adj_exp_pc':
          update_index = covs_df['selected_covs'] == name
          covs_df.loc[update_index, 'beta'] = value

    # output betas if specified
    if output_betas:
        beta = model.get_beta_dict()

        # add draw and cause id columns
        betas['acause'] = acause

    # otherwise output inefficiency & the values needed to make the frontier plot
    else:

        ## X values
        X = df[spend_prev_col].values
        ## Y values
        Y = df[mort_prev_col].values
        ## Predictions
        Y_hat = model.predict(df)
        ## Prediction excluding the impact of the covariates
        df_null_covs = df.copy()
        df_null_covs[list(set(covs).difference([spend_prev_col]))] = 0.0
        Y_hat_adj = model.predict(df_null_covs)
        ## Impact of the covariates
        cov_hat = Y_hat-Y_hat_adj
        ## Y minus the impact of the covariates
        Y_adj = Y - cov_hat
        ## Inefficiency
        ineff = model.get_inefficiency()
        
        ## make results table
        results = pd.DataFrame({
            spend_prev_col: X, 
            'mi_ratio': Y, 
            'ineff': ineff, 
            'y_adj': -Y_adj, 
            'y_adj_hat': -Y_hat_adj
        })
        results.sort_values(spend_prev_col, inplace = True)
        # keep the preserved spending column
        out = df[['location_name', 'year_id', spend_prev_col, 'cause_id', 'as_spend_prev_ratio_copy']].merge(results, on=spend_prev_col)
    
    # normalize to 0-1 and keep unscaled either way
    if rescale:
        out['ineff_raw'] = out['ineff']
        
        out['ineff'] = (out['ineff'] - out.ineff.min()) / (out.ineff.max() - out.ineff.min())

        if out.loc[out.ineff.isnull()].shape[0]:
            out['ineff'] = 0
    else:
        out['ineff_scaled'] = (out['ineff'] - out.ineff.min()) / (out.ineff.max() - out.ineff.min())

        if out.loc[out.ineff_scaled.isnull()].shape[0]:
            out['ineff_scaled'] = 0
            
    return out, covs_df

"""
# H's 
def runSFA(cause, draw, variant, testplot=False):

    #### Default parameters ####
    #---------------------------------------
    
    df_path = '/mnt/share/resource_tracking/us_value/data/input_data/agg/best/demean_MI_MP_draws.csv' 
    cov_df_path = '/ihme/resource_tracking/us_value/data/sfa_covars2021_shea.csv'

    
    covs = ['obesity', 'age65', 'cig_pc_10',
            'phys_act_10', 'edu_yrs', 'adj_exp_pc']
            
    pre_selected_covs = ['adj_exp_pc']
    #no_prior_covs = ['adj_exp_pc']
    no_prior_covs = []
    
    draw_column = 'demeaned_draw_{}'.format(draw)
    variance_column = 'variance_demeaned'
    spline_variable_name = None
    spending_var = 'spending_adj_pc'
    
    test_cov_direction = True # drop covariates that aren't in right direction and rerun
    trimming = True #5%
    l_tail = r_tail = True # doesn't matter since default is no spline
    output_betas = False
    rescale = True 
    
    #### Set up sensitivity parameters #####
    #---------------------------------------
    
    if variant == 'all_covs':
        covs = ['obesity', 'age65', 'cig_pc',
            'phys_act_10', 'edu_yrs', 'ldi_pc', 'adj_exp_pc']
            
    elif variant == "treatment_nospline_w_unemployment":
        covs = ['obesity', 'age65', 'cig_pc_10',
            'phys_act_10', 'unemployment_rate', 'adj_exp_pc']

    elif variant == "treatment_nospline_w_homelessness":
        covs = ['obesity', 'age65', 'cig_pc_10',
            'phys_act_10', 'edu_yrs', 'adj_exp_pc', 'prop_homeless']
    
    elif variant == "treatment_nospline_w_racecovs1":
        cov_df_path = '/ihme/resource_tracking/us_value/data/sfa_covars_w_race_fractions.csv'
        covs = ['obesity', 'age65', 'cig_pc_10',
            'phys_act_10', 'edu_yrs', 'adj_exp_pc', 'race_prop_BLCK', 'race_prop_AIAN', 'race_prop_HISP']
        no_prior_covs = ['race_prop_BLCK', 'race_prop_AIAN', 'race_prop_HISP']
        
    elif variant == "treatment_nospline_w_racecovs2":
        cov_df_path = '/ihme/resource_tracking/us_value/data/sfa_covars_w_race_fractions.csv'
        covs = ['obesity', 'age65', 'cig_pc_10',
            'phys_act_10', 'edu_yrs', 'adj_exp_pc', 'race_prop_WHT']
        no_prior_covs = ['race_prop_WHT']
        
    elif variant == "treatment_nospline_w_racecovs_hisp":
        cov_df_path = '/ihme/resource_tracking/us_value/data/sfa_covars_w_race_fractions.csv'
        covs = ['obesity', 'age65', 'cig_pc_10',
            'phys_act_10', 'edu_yrs', 'adj_exp_pc', 'race_prop_HISP']
        no_prior_covs = ['race_prop_HISP']
        
    elif variant == "treatment_nospline_w_racecovs_black":
        cov_df_path = '/ihme/resource_tracking/us_value/data/sfa_covars_w_race_fractions.csv'
        covs = ['obesity', 'age65', 'cig_pc_10',
            'phys_act_10', 'edu_yrs', 'adj_exp_pc', 'race_prop_BLCK']
        no_prior_covs = ['race_prop_BLCK']
        
    elif variant == "treatment_nospline_w_racecovs_api":
        cov_df_path = '/ihme/resource_tracking/us_value/data/sfa_covars_w_race_fractions.csv'
        covs = ['obesity', 'age65', 'cig_pc_10',
            'phys_act_10', 'edu_yrs', 'adj_exp_pc', 'race_prop_API']
        no_prior_covs = ['race_prop_API']
        
        
    elif variant == "treatment_nospline_no_covs":
        covs = ['adj_exp_pc']
    
    elif variant=="treatment_nospline_ranked_postagg":
        df_path = '/mnt/share/resource_tracking/us_value/data/input_data/agg/2022_07_05_ranked_postagg/demean_MI_MP_draws.csv' 
        
    elif variant == "treatment_spline":
        covs = ['obesity', 'age65', 'cig_pc_10',
            'phys_act_10', 'edu_yrs']
        pre_selected_covs = []
        no_prior_covs = []
        spline_variable_name = 'adj_exp_pc'

    ####  Main section
    #---------------------------------------
    
    # read in data and keep cause and draw to run
    cols = ['year_id', 'cause_id', 'location_id', draw_column, variance_column]

    df = pd.read_csv(df_path, usecols=cols) 
    df = df[df['cause_id'] == cause]
    # We only have spend through 2020
    df = df[df['year_id'] < 2021]
    
    cov_df = pd.read_csv(cov_df_path)

    df = pd.merge(df, 
              cov_df, 
              on=['location_id', 'year_id'])
              
    # df = pd.merge(df, 
    #           cov_df[['location_id', 'location_name', 'year_id', 'obesity', 
    #                 'cig_pc', 'age65', 'edu_yrs', 'phys_act_10', 'ldi_pc',
    #                 'cig_pc_15', 'cig_pc_10',
    #                 'density_l.150', 'density_g.1000', spending_var]], 
    #           on=['location_id', 'year_id'])

    df.rename(columns={'year': 'year_id', spending_var: 'adj_exp_pc'}, inplace=True)

    # drop DC (outlier)
    df = df[df['location_name'] != 'District of Columbia']

    # preserve spending pre normalization
    df['adj_exp_pc_c'] = df['adj_exp_pc']

    # covariate mean normalization
    #for cov in covs:
    for cov in np.unique(covs +['adj_exp_pc']):
        df[cov] = (df[cov] - df[cov].mean())/df[cov].std()

    # lower envelope of the ratio draw - make it negative
    df[draw_column] = -df[draw_column]
    
    # take median of the variance within a draw
    df[variance_column] = np.median(df[variance_column])
    
    # Very important to sort here! Will prevent indexes from getting messed up while plotting and pulling inefficiency
    df.sort_values('adj_exp_pc', inplace = True)

    def get_model(df, covs, variance_column, obs_column, spline_column):
    
        # set priors on covariates
        no_prior_covs_model = list(set(covs).intersection(set(no_prior_covs)))
        pos_covs = list(set(['pct_NHwhite', 'phys_act_10', 'edu_yrs', 'ldi_pc', 'adj_exp_pc']).intersection(set(covs).difference(set(no_prior_covs))))
        neg_covs = list(set(['obesity', 'age65', 'cig_pc', 'cig_pc_15','cig_pc_10', 'density_g.1000', 'prop_homeless']).intersection(set(covs).difference(set(no_prior_covs))))
        
        # make weights 
        df['standard_error'] = np.sqrt(df[variance_column])
            
        # create data object
        data = Data(
            obs = obs_column,
            obs_se = 'standard_error',
        )

        # create variables and set prior signs
        variables = [Variable(Component("intercept", default_value=1.0))] + [
            Variable(name, priors=[UniformPrior(lb=0.0, ub=np.inf)])
            for name in pos_covs
        ] + [
            Variable(name, priors=[UniformPrior(lb=-np.inf, ub=0.0)])
            for name in neg_covs
        ] +[
            Variable(name)
            for name in no_prior_covs_model
        ]
        
        # convex prior and monotonic decreasing prior
        priors = [SplinePriorGetter(UniformPrior(lb=-np.inf, ub=0.0), order=2, size=100),
              SplinePriorGetter(UniformPrior(lb=0.0, ub=np.inf), order=1, size=100)]
        
        if spline_variable_name is not None:
            variables.append(
                SplineVariable(
                    spline_column,
                    spline = SplineGetter(
                        knots=np.array([0.0, 0.05, 0.01,  1.0]),
                        degree=3,
                        knots_type='rel_freq',
                        include_first_basis=False,
                        l_linear = l_tail,
                        r_linear = r_tail
                    ),
                    priors = priors
                )
            )
        
        model = SFMAModel(data, variables, include_re=False)
        model.attach(df)
        
        return model


    # create model object
    model = get_model(df, covs, variance_column, draw_column, spline_variable_name)

    # drop covariates with very low beta under the prior and rerun #
    if test_cov_direction:
        model.eta = 0.1
        model.beta.fill(1.0)
        model.fit(
            verbose=True, max_iter=10, tol=1e-3,
            beta_options={"solver_type": "ip", "max_iter": 1000, "verbose": False, "xtol": 0.0, "gtol": 1e-6, "update_mu_every": 10}
        )
        
        beta = model.get_beta_dict()
        
        selected_covs = []
        for name, value in beta.items():
            if value.size == 1 and name != "intercept" and np.abs(value[0]) > 1e-5:
                selected_covs.append(name)
        for cov in pre_selected_covs:
            if cov not in selected_covs:
                selected_covs.append(cov)
        print(selected_covs)

        model = get_model(df, selected_covs, variance_column, draw_column, spline_variable_name)


    if trimming:        
        # fit model with 5% trimming
        model.eta = 0.1
        model.beta.fill(1.0)
        model.fit(
            outlier_pct=0.05, trim_max_iter=5,
            verbose=True, max_iter=5, tol=1e-3,
            beta_options={"solver_type": "ip", "max_iter": 1000, "verbose": False, "xtol": 0.0, "gtol": 1e-6, "update_mu_every": 10}
        )
        
        
    else:
        # fit model without trimming
        model.eta = 0.1
        model.beta.fill(1.0)
        model.fit(
            verbose=True, max_iter=10, tol=1e-3,
            beta_options={"solver_type": "ip", "max_iter": 1000, "verbose": False, "xtol": 0.0, "gtol": 1e-6, "update_mu_every": 10}
        )     
        
    # return covariate information
    if len(selected_covs) > 0:
        covs_df = pd.DataFrame({
            'selected_covs': selected_covs,
            'draw': draw,
            'cause_id': cause
        })
        
        # add beta information
        beta = model.get_beta_dict()
        
    else:
        covs_df = pd.DataFrame({
            'selected_covs': ["no covs selected"],
            'draw': draw,
            'cause_id': cause
        })
        
    # Add on betas to cov df
    betas = model.get_beta_dict()

    for name, value in betas.items():
      if value.size == 1 and name != 'adj_exp_pc':
          update_index = covs_df['selected_covs'] == name
          covs_df.loc[update_index, 'beta'] = value

    # output betas if specified
    if output_betas:
        beta = model.get_beta_dict()

        # add draw and cause id columns
        betas['draw'] = draw
        betas['cause_id'] = cause

    # otherwise output inefficiency & the values needed to make the frontier plot
    else:

        ## X values
        X = df['adj_exp_pc'].values
        ## Y values
        Y = df[draw_column].values
        ## Predictions
        Y_hat = model.predict(df)
        ## Prediction excluding the impact of the covariates
        df_null_covs = df.copy()
        df_null_covs[list(set(covs).difference(["adj_exp_pc"]))] = 0.0
        Y_hat_adj = model.predict(df_null_covs)
        ## Impact of the covariates
        cov_hat = Y_hat-Y_hat_adj
        ## Y minus the impact of the covariates
        Y_adj = Y - cov_hat
        ## Inefficiency
        ineff = model.get_inefficiency()
        
        ## make results table
        results = pd.DataFrame({
            'adj_exp_pc': X, 
            'mi_ratio': Y, 
            'ineff': ineff, 
            'y_adj': -Y_adj, 
            'y_adj_hat': -Y_hat_adj
        })
        results.sort_values('adj_exp_pc', inplace = True)
        # keep the preserved spending column
        out = df[['location_name', 'year_id', 'adj_exp_pc', 'cause_id', 'adj_exp_pc_c']].merge(results, on='adj_exp_pc')

        # add draw column
        out['draw'] = draw
    
    # normalize to 0-1 and keep unscaled either way
    if rescale:
        out['ineff_raw'] = out['ineff']
        
        out['ineff'] = (out['ineff'] - out.ineff.min()) / (out.ineff.max() - out.ineff.min())

        if out.loc[out.ineff.isnull()].shape[0]:
            out['ineff'] = 0
    else:
        out['ineff_scaled'] = (out['ineff'] - out.ineff.min()) / (out.ineff.max() - out.ineff.min())

        if out.loc[out.ineff_scaled.isnull()].shape[0]:
            out['ineff_scaled'] = 0
            
    return out, covs_df
"""

if __name__ == '__main__':
    # Set output directory
    today_yyyymmdd = date.today().strftime("%Y%m%d")
    dir_output = Path('/ihme/homes/idrisov/aim_outputs/Aim2/C_frontier_analysis/')
    dir_output = dir_output / today_yyyymmdd
    Path(dir_output).mkdir(parents=True, exist_ok=True)

    # Specify acause for model run
    acause = "hiv" # can be "hiv" or "_subs"

    # Run model, return outputs to variables
    model_output = runSFA(acause)
    df_output = model_output[0]
    df_covariates = model_output[1]

    # Write to csv    
    df_output.to_csv(os.path.join(dir_output, '{}_out.csv'.format(acause)), index = False)
    df_covariates.to_csv(os.path.join(dir_output, '{}_covariates.csv'.format(acause)), index = False)