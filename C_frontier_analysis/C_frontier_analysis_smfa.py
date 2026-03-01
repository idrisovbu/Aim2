"""
Filename: C_frontier_analysis_sfma_hiv_between.py
Description: Python script for running SFMA frontier analysis for HIV (Aim 2).
             Covariates included INSIDE SFMA with direction priors from regression.
             Spending modeled as SplineVariable for nonlinear frontier.
             Includes homelessness covariate from secondary regression model.

Regression models informing priors:
  Primary:   log(M) ~ log(S) + Year + Black + log(Inc) + Hisp
  Secondary: log(M) ~ log(S) + Year + Black + log(Inc) + Hisp + log(Homeless)

  Coef directions (on MORTALITY, informing SFMA priors on NEGATED mortality):
    rw_dex_hiv_prev_ratio_log_B:  -0.254  → LOWER mort → pos prior on neg_mort [via spline]
    year_centered:                -0.088  → LOWER mort → pos prior on neg_mort
    race_prop_BLCK_B:              0.839  → HIGHER mort → neg prior on neg_mort
    log_incidence_rates_B:        -0.365  → LOWER mort → pos prior on neg_mort
    race_prop_HISP:                0.845  → HIGHER mort → neg prior on neg_mort
    log_prop_homeless_B:           0.070  → HIGHER mort → neg prior on neg_mort

How to run: Modify fp_input / dir_output as needed, then run entire script.
            Outputs to /ihme/homes/idrisov/aim_outputs/Aim2/C_frontier_analysis/<today>/
"""

# ═══════════════════════════════════════════════════════════════════════════════
# DIRECTORIES & CONFIGURATION
# ═══════════════════════════════════════════════════════════════════════════════

from pathlib import Path
from datetime import date
import numpy as np
import pandas as pd
import sys
import os
import matplotlib.pyplot as plt

from sfma import Data, Variable, SplineVariable, SplineGetter, SplinePriorGetter, UniformPrior, GaussianPrior, SFMAModel
from anml.data.component import Component

# ── Paths ──
dir_base = Path('/ihme/homes/idrisov/aim_outputs/Aim2/C_frontier_analysis/')

# Input: pre-merged dataset with between/within decomposition variables
fp_input = Path('/ihme/homes/idrisov/aim_outputs/Aim2/C_frontier_analysis/20260218/analysis/df_hiv_analysis.csv')

# Output: dated folder
today_yyyymmdd = date.today().strftime("%Y%m%d")
dir_output = dir_base / today_yyyymmdd
dir_output.mkdir(parents=True, exist_ok=True)

# ── Column names (exact names from df_hiv_analysis.csv) ──
mort_col     = 'as_mort_prev_ratio_log'          # outcome: log(mortality/prevalence)
spend_col    = 'rw_dex_hiv_prev_ratio_log_B'     # spending: log(spending+RW / prevalence), between-state mean
variance_col = 'variance'
strat_col    = 'high_hiv_prev_B'                  # stratification: 1=high prev, 0=low prev

# ── Model variants ──
# Primary: without homelessness (matches hiv__between__primary regression)
# Secondary: with homelessness (matches hiv__between__secondary_homeless regression)
MODEL_VARIANTS = {
    'primary': {
        'cov_cols': ['year_centered', 'race_prop_BLCK_B', 'log_incidence_rates_B', 'race_prop_HISP'],
        'label': 'Primary (no homelessness)',
    },
    'secondary_homeless': {
        'cov_cols': ['year_centered', 'race_prop_BLCK_B', 'log_incidence_rates_B', 'race_prop_HISP', 'log_prop_homeless_B'],
        'label': 'Secondary (with homelessness)',
    },
}

# ── Prior directions (based on regression coefficients, FLIPPED for negated mortality) ──
# Regression β < 0 on mortality → variable IMPROVES outcomes → POSITIVE prior on neg_mort
# Regression β > 0 on mortality → variable WORSENS outcomes → NEGATIVE prior on neg_mort
pos_prior_vars = ['year_centered', 'log_incidence_rates_B']
neg_prior_vars = ['race_prop_BLCK_B', 'race_prop_HISP', 'log_prop_homeless_B']
# NOTE: spending gets its prior via SplineVariable shape constraints (increasing + concave)

acause = 'hiv'


# ═══════════════════════════════════════════════════════════════════════════════
# MAIN FUNCTION
# ═══════════════════════════════════════════════════════════════════════════════

def runSFA_HIV(df_full, subset_type='all', model_variant='primary'):
    """
    Run SFMA frontier analysis for HIV.
    
    Approach (following Lescinsky/H's method):
      - Covariates go INSIDE the SFMA model with direction priors from regression
      - Spending modeled as SplineVariable (nonlinear frontier, increasing, concave)
      - Outcome is negated log(mortality/prevalence) so SFMA fits upper envelope
      - All variables z-scored for numerical stability (originals preserved for plotting)
    
    Parameters
    ----------
    df_full : pd.DataFrame
        Full analysis dataset (df_hiv_analysis.csv)
    subset_type : str
        'all', 'high_prev', or 'low_prev'
    model_variant : str
        'primary' or 'secondary_homeless'
    
    Returns
    -------
    out : pd.DataFrame
        Output with inefficiency scores, adjusted values, frontier predictions
    covs_df : pd.DataFrame
        Selected covariate information and betas
    """
    
    variant_config = MODEL_VARIANTS[model_variant]
    cov_cols = variant_config['cov_cols']
    all_model_vars = [spend_col] + cov_cols
    
    # ── Subset data ──
    if subset_type == 'high_prev':
        df = df_full[df_full[strat_col] == 1].copy()
        subset_label = 'High-Prevalence States'
    elif subset_type == 'low_prev':
        df = df_full[df_full[strat_col] == 0].copy()
        subset_label = 'Low-Prevalence States'
    else:
        df = df_full.copy()
        subset_label = 'All States'
    
    print(f"\n{'=' * 70}")
    print(f"SFMA HIV FRONTIER: {subset_label.upper()}")
    print(f"  Model: {variant_config['label']}")
    print(f"  Covariates: {cov_cols}")
    print(f"  n = {len(df)}")
    print(f"{'=' * 70}")
    
    # ── Drop NaN in key variables ──
    key_vars = [mort_col, spend_col] + cov_cols + [variance_col]
    n_before = len(df)
    df = df.dropna(subset=key_vars).copy()
    if len(df) < n_before:
        print(f"  Dropped {n_before - len(df)} rows with NaN")
    
    # ── Preserve original spending for plotting (pre-normalization) ──
    df['log_spend_copy'] = df[spend_col].copy()
    
    # ── Z-score normalize all model variables ──
    for var in all_model_vars:
        df[var] = (df[var] - df[var].mean()) / df[var].std()
    
    # ── Negate outcome for SFMA ──
    df[mort_col] = -1.0 * df[mort_col]
    
    # ── Standard error (use median variance, matching H's approach) ──
    df['standard_error'] = np.sqrt(np.median(df[variance_col]))
    
    # ── Sort by spending (critical for index alignment) ──
    df.sort_values(spend_col, inplace=True)
    df.reset_index(drop=True, inplace=True)
    
    # ══════════════════════════════════════════════════════════════════════
    # BUILD SFMA MODEL
    # ══════════════════════════════════════════════════════════════════════
    
    covs = cov_cols.copy()
    pre_selected_covs = []  # spending always in via SplineVariable
    
    test_cov_direction = True
    trimming = True
    rescale = True
    l_tail = r_tail = True
    
    def get_model(df, covs_to_use):
        """Build SFMA model with covariates and spending spline."""
        
        data = Data(obs=mort_col, obs_se='standard_error')
        
        # Linear covariate Variables with direction priors
        variables = [Variable(Component("intercept", default_value=1.0))]
        
        for cov in covs_to_use:
            if cov == spend_col:
                continue
            elif cov in pos_prior_vars:
                variables.append(Variable(cov, priors=[UniformPrior(lb=0.0, ub=np.inf)]))
            elif cov in neg_prior_vars:
                variables.append(Variable(cov, priors=[UniformPrior(lb=-np.inf, ub=0.0)]))
            else:
                variables.append(Variable(cov))
        
        # Spending as SplineVariable: increasing + concave
        spline_priors = [
            SplinePriorGetter(UniformPrior(lb=0.0, ub=np.inf), order=1, size=100),   # increasing
            SplinePriorGetter(UniformPrior(lb=-np.inf, ub=0.0), order=2, size=100),  # concave
        ]
        
        variables.append(
            SplineVariable(
                spend_col,
                spline=SplineGetter(
                    knots=np.array([0.0, 0.33, 0.67, 1.0]),
                    degree=3,
                    knots_type='rel_domain',
                    include_first_basis=False,
                    l_linear=l_tail,
                    r_linear=r_tail,
                ),
                priors=spline_priors,
            )
        )
        
        model = SFMAModel(data, variables, include_re=False)
        model.attach(df)
        
        return model
    
    # ── Initial model with all covariates ──
    model = get_model(df, covs)
    
    # ── Covariate selection ──
    selected_covs = covs.copy()
    
    if test_cov_direction:
        print(f"  Testing covariate directions...")
        model.eta = 0.1
        model.beta.fill(1.0)
        model.fit(
            verbose=True, max_iter=10, tol=1e-3,
            beta_options={"max_iter": 1000, "verbose": False, "xtol": 0.0, "gtol": 1e-6}
        )
        
        beta = model.get_beta_dict()
        selected_covs = []
        for name, value in beta.items():
            if value.size == 1 and name != "intercept" and np.abs(value[0]) > 1e-5:
                selected_covs.append(name)
        print(f"  Selected covariates: {selected_covs}")
        
        model = get_model(df, selected_covs)
    
    # ── Final fit ──
    if trimming:
        print(f"  Fitting with 5% trimming...")
        model.eta = 0.1
        model.beta.fill(1.0)
        model.fit(
            outlier_pct=0.05, trim_max_iter=5,
            verbose=True, max_iter=5, tol=1e-3,
            beta_options={"max_iter": 1000, "verbose": False, "xtol": 0.0, "gtol": 1e-6}
        )
    else:
        print(f"  Fitting without trimming...")
        model.eta = 0.1
        model.beta.fill(1.0)
        model.fit(
            verbose=True, max_iter=10, tol=1e-3,
            beta_options={"max_iter": 1000, "verbose": False, "xtol": 0.0, "gtol": 1e-6}
        )
    
    print(f"  Fitted! eta={model.eta:.6f}")
    betas = model.get_beta_dict()
    print(f"  Betas: {betas}")
    
    # ══════════════════════════════════════════════════════════════════════
    # EXTRACT RESULTS
    # ══════════════════════════════════════════════════════════════════════
    
    X = df[spend_col].values
    Y = df[mort_col].values
    Y_hat = model.predict(df)
    
    # Prediction with covariates zeroed → spending-only frontier
    df_null_covs = df.copy()
    covs_to_zero = [c for c in selected_covs if c != spend_col]
    if covs_to_zero:
        df_null_covs[covs_to_zero] = 0.0
    Y_hat_adj = model.predict(df_null_covs)
    
    cov_hat = Y_hat - Y_hat_adj
    Y_adj = Y - cov_hat
    ineff = model.get_inefficiency()
    
    # Build output
    out = df[['location_name', 'location_id', 'year_id', 'cause_id', 'acause', 'cause_name',
              spend_col, 'log_spend_copy',
              strat_col]].copy()
    
    out[mort_col] = Y
    out['ineff'] = ineff
    out['y_adj'] = -Y_adj
    out['y_adj_hat'] = -Y_hat_adj
    out['y_adj_log'] = out['y_adj']
    out['y_adj_hat_log'] = out['y_adj_hat']
    out['subset_type'] = subset_type
    out['model_variant'] = model_variant
    
    out.sort_values(spend_col, inplace=True)
    
    # Covariate info
    covs_only = [c for c in selected_covs if c != spend_col]
    if len(covs_only) > 0:
        covs_df = pd.DataFrame({'selected_covs': covs_only, 'acause': acause,
                                'subset_type': subset_type, 'model_variant': model_variant})
    else:
        covs_df = pd.DataFrame({'selected_covs': ['no covs selected'], 'acause': acause,
                                'subset_type': subset_type, 'model_variant': model_variant})
    
    for name, value in betas.items():
        if value.size == 1 and name != spend_col:
            update_idx = covs_df['selected_covs'] == name
            covs_df.loc[update_idx, 'beta'] = value[0]
    
    # Rescale inefficiency to 0–1
    if rescale:
        out['ineff_raw'] = out['ineff']
        if out['ineff'].max() > out['ineff'].min():
            out['ineff'] = (out['ineff'] - out.ineff.min()) / (out.ineff.max() - out.ineff.min())
        else:
            out['ineff'] = 0
    
    # Summary
    print(f"\n  Results Summary ({subset_label}, {variant_config['label']}):")
    print(f"    N observations:     {len(out)}")
    print(f"    Mean inefficiency:  {out['ineff_raw'].mean():.4f}")
    print(f"    Median ineff:       {out['ineff_raw'].median():.4f}")
    print(f"    Min/Max ineff:      {out['ineff_raw'].min():.4f} / {out['ineff_raw'].max():.4f}")
    print(f"    eta:                {model.eta:.6f}")
    
    return out, covs_df


# ═══════════════════════════════════════════════════════════════════════════════
# PLOTTING
# ═══════════════════════════════════════════════════════════════════════════════

def plot_frontier(df_output, title_suffix="", save_path=None):
    """
    Frontier visualization in log-log space.
    X-axis: original log(spending) (pre-normalization)
    Y-axis: covariate-adjusted log(mortality)
    """
    
    fig, ax = plt.subplots(figsize=(10, 7))
    
    ax.scatter(
        df_output['log_spend_copy'],
        df_output['y_adj_log'],
        alpha=0.6, s=50,
        label='Observed (covariate-adjusted)',
        c='steelblue', edgecolors='white', linewidth=0.5
    )
    
    df_sorted = df_output.sort_values('log_spend_copy')
    ax.plot(
        df_sorted['log_spend_copy'],
        df_sorted['y_adj_hat_log'],
        color='darkorange', linewidth=3,
        label='Estimated Frontier',
        zorder=5
    )
    
    ax.set_xlabel('log(Spending per Prevalent Case) [Between-State Mean]', fontsize=12)
    ax.set_ylabel('log(Mortality per Prevalent Case) [Covariate-Adjusted]', fontsize=12)
    ax.set_title(f'HIV Healthcare Efficiency Frontier: {title_suffix}\n(n={len(df_output)}, Log-Log Space)',
                 fontsize=14, fontweight='bold')
    ax.legend(loc='upper right', fontsize=10)
    ax.grid(True, alpha=0.3, linestyle='--')
    
    # Get model variant for annotation
    variant = df_output['model_variant'].iloc[0] if 'model_variant' in df_output.columns else 'primary'
    cov_text = MODEL_VARIANTS.get(variant, {}).get('cov_cols', [])
    
    fig.text(
        0.5, 0.01,
        f'Note: Points above frontier = inefficient. Frontier = best achievable mortality at each spending level.\n'
        f'Covariates: {", ".join(cov_text)}',
        ha='center', fontsize=8, style='italic', color='gray'
    )
    
    plt.tight_layout(rect=[0, 0.05, 1, 1])
    
    if save_path:
        plt.savefig(save_path, dpi=150, bbox_inches='tight')
        print(f"  Plot saved to: {save_path}")
    
    plt.close(fig)
    return fig


# ═══════════════════════════════════════════════════════════════════════════════
# MAIN EXECUTION
# ═══════════════════════════════════════════════════════════════════════════════

if __name__ == '__main__':
    
    print("\n" + "=" * 70)
    print("SFMA FRONTIER ANALYSIS - HIV BETWEEN-STATE MODEL")
    print("=" * 70)
    print(f"\n  Input:  {fp_input}")
    print(f"  Output: {dir_output}\n")
    
    # ── Read data ──
    df = pd.read_csv(fp_input)
    print(f"  Loaded {len(df)} observations")
    print(f"  States: {df['location_name'].nunique()}")
    print(f"  Years:  {sorted(df['year_id'].unique())}")
    
    # ── Run both model variants for each stratification ──
    all_results = {}
    
    for variant_key in ['primary', 'secondary_homeless']:
        variant_label = MODEL_VARIANTS[variant_key]['label']
        
        for subset_key, subset_name in [('all', 'All States'), ('high_prev', 'High-Prevalence'), ('low_prev', 'Low-Prevalence')]:
            
            run_key = f"{variant_key}__{subset_key}"
            
            print(f"\n\n{'#' * 70}")
            print(f"# {variant_label} — {subset_name}")
            print(f"{'#' * 70}")
            
            out, covs = runSFA_HIV(df, subset_type=subset_key, model_variant=variant_key)
            all_results[run_key] = (out, covs)
            
            # File naming: hiv_between_{variant}_{subset}_output.csv
            fname_prefix = f"hiv_between_{variant_key}_{subset_key}"
            
            out.to_csv(dir_output / f'{fname_prefix}_output.csv', index=False)
            covs.to_csv(dir_output / f'{fname_prefix}_covariates.csv', index=False)
            
            plot_frontier(
                out,
                title_suffix=f"{subset_name} ({variant_label})",
                save_path=dir_output / f'{fname_prefix}_frontier.png'
            )
    
    # ─────────────────────────────────────────────────────────────────────
    # SUMMARY COMPARISON
    # ─────────────────────────────────────────────────────────────────────
    print("\n\n" + "=" * 90)
    print("SUMMARY COMPARISON: ALL MODELS")
    print("=" * 90)
    
    print(f"\n{'Variant':<25} {'Subset':<20} {'N':>6} {'Mean Ineff':>12} {'Med Ineff':>12} {'Max Ineff':>12}")
    print("-" * 90)
    
    for run_key, (out_df, _) in all_results.items():
        variant_key, subset_key = run_key.split('__')
        print(f"{variant_key:<25} {subset_key:<20} {len(out_df):>6} "
              f"{out_df['ineff_raw'].mean():>12.4f} {out_df['ineff_raw'].median():>12.4f} "
              f"{out_df['ineff_raw'].max():>12.4f}")
    
    print("\n" + "=" * 90)
    print("ANALYSIS COMPLETE!")
    print(f"Output files saved to: {dir_output}")
    print("=" * 90)
    
    print("\nOutput files generated:")
    for f in sorted(dir_output.glob('hiv_between_*')):
        print(f"  - {f.name}")