"""
Filename: C_frontier_analysis_sfma_hiv.py
Description: Python script for running SFMA frontier analysis for HIV (Aim 2).
             Two frontier models, NO stratification by prevalence/incidence.

Model 1: BETWEEN FRONTIER (cross-sectional, ~51 observations)
  Data:    df_hiv_analysis_between.csv
  Outcome: as_mort_prev_ratio_log  (log mortality/prevalence)
  Spending: rw_dex_hiv_prev_ratio_log  (SplineVariable, increasing + concave)
  Covariates: race_prop_BLCK, log_incidence_rates_B, race_prop_HISP, log_prop_homeless_B

  Regression informing priors (hiv__between_true__primary):
    rw_dex_hiv_prev_ratio_log:  -0.253  → LOWER mort → spline: increasing+concave on neg_mort
    race_prop_BLCK:              +0.835  → HIGHER mort → neg prior on neg_mort
    log_incidence_rates_B:       -0.364  → LOWER mort → pos prior on neg_mort
    race_prop_HISP:              +0.840  → HIGHER mort → neg prior on neg_mort
    log_prop_homeless_B:         +0.070  → HIGHER mort → neg prior on neg_mort

Model 2: YEAR-FE FRONTIER (panel, ~510 observations)
  Data:    df_hiv_analysis_panel.csv
  Outcome: as_mort_prev_ratio_log
  Spending: rw_dex_hiv_prev_ratio_log  (SplineVariable, increasing + concave)
  Covariates: factor(year_id) dummies, race_prop_BLCK, log_incidence_rates,
              race_prop_HISP, log_prop_homeless

  Regression informing priors (hiv__between_yfe__primary):
    rw_dex_hiv_prev_ratio_log:  -0.157  → spline: increasing+concave on neg_mort
    year_factor2011..2019:       all neg → LOWER mort → pos prior on neg_mort
    race_prop_BLCK:              +0.624  → HIGHER mort → neg prior on neg_mort (n.s.)
    log_incidence_rates:         -0.297  → LOWER mort → pos prior on neg_mort
    race_prop_HISP:              +0.739  → HIGHER mort → neg prior on neg_mort
    log_prop_homeless:           +0.039  → HIGHER mort → neg prior on neg_mort (n.s.)

How to run: Modify fp_input_* / dir_output as needed, then run entire script.
"""

# ═══════════════════════════════════════════════════════════════════════════════
# IMPORTS & CONFIGURATION
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

fp_input_between = Path('/ihme/homes/idrisov/aim_outputs/Aim2/C_frontier_analysis/20260303/analysis/df_hiv_analysis_between.csv')
fp_input_panel   = Path('/ihme/homes/idrisov/aim_outputs/Aim2/C_frontier_analysis/20260303/analysis/df_hiv_analysis_panel.csv')

today_yyyymmdd = date.today().strftime("%Y%m%d")
dir_output = dir_base / today_yyyymmdd
dir_output.mkdir(parents=True, exist_ok=True)

# ── Common column names ──
mort_col     = 'as_mort_prev_ratio_log'           # outcome: log(mortality/prevalence)
spend_col    = 'rw_dex_hiv_prev_ratio_log'        # spending: log(spending+RW / prevalence)
variance_col = 'variance'

acause = 'hiv'

# ── Model configurations ──
MODEL_CONFIGS = {
    'between': {
        'label': 'Between (Cross-Sectional, ~51 obs)',
        'fp_input': fp_input_between,
        'spend_col': spend_col,
        'cov_cols': ['race_prop_BLCK', 'log_incidence_rates_B', 'race_prop_HISP', 'log_prop_homeless_B'],
        'year_fe': False,
        # Priors on NEGATED mortality (pos = reduces mort, neg = increases mort)
        'pos_prior_vars': ['log_incidence_rates_B'],          # β<0 on mort → pos on neg_mort
        'neg_prior_vars': ['race_prop_BLCK', 'race_prop_HISP', 'log_prop_homeless_B'],  # β>0 on mort → neg on neg_mort
    },
    'year_fe': {
        'label': 'Year-FE Panel (~510 obs)',
        'fp_input': fp_input_panel,
        'spend_col': spend_col,
        'cov_cols': ['race_prop_BLCK', 'log_incidence_rates', 'race_prop_HISP', 'log_prop_homeless'],
        'year_fe': True,
        # Year dummies get pos priors (all year coefficients < 0 on mort → pos on neg_mort)
        'pos_prior_vars': ['log_incidence_rates'],  # + year dummies added dynamically
        'neg_prior_vars': ['race_prop_BLCK', 'race_prop_HISP', 'log_prop_homeless'],
    },
}


# ═══════════════════════════════════════════════════════════════════════════════
# MAIN FUNCTION
# ═══════════════════════════════════════════════════════════════════════════════

def runSFA_HIV(model_key):
    """
    Run SFMA frontier analysis for HIV.

    Approach (following Lescinsky/Haley's method):
      - Covariates go INSIDE the SFMA model with direction priors from regression
      - Spending modeled as SplineVariable (nonlinear frontier, increasing, concave)
      - Outcome is negated log(mortality/prevalence) so SFMA fits upper envelope
      - All continuous variables z-scored for numerical stability

    Parameters
    ----------
    model_key : str
        'between' or 'year_fe'

    Returns
    -------
    out : pd.DataFrame
        Output with inefficiency scores, adjusted values, frontier predictions
    covs_df : pd.DataFrame
        Selected covariate information and betas
    """

    config = MODEL_CONFIGS[model_key]
    cov_cols = config['cov_cols'].copy()
    pos_prior_vars = config['pos_prior_vars'].copy()
    neg_prior_vars = config['neg_prior_vars'].copy()

    # ── Read data ──
    df = pd.read_csv(config['fp_input'])

    print(f"\n{'=' * 70}")
    print(f"SFMA HIV FRONTIER: {config['label'].upper()}")
    print(f"  Input:  {config['fp_input']}")
    print(f"  n = {len(df)}")
    print(f"  Covariates: {cov_cols}")
    if config['year_fe']:
        print(f"  Year FE: YES (dummies for each year except reference)")
    print(f"{'=' * 70}")

    # ── Create year dummies if needed ──
    year_dummy_cols = []
    if config['year_fe']:
        years = sorted(df['year_id'].unique())
        ref_year = years[0]  # 2010 as reference
        print(f"  Reference year: {ref_year}")
        for yr in years[1:]:
            col = f'year_{yr}'
            df[col] = (df['year_id'] == yr).astype(float)
            year_dummy_cols.append(col)
            pos_prior_vars.append(col)  # all year dummies → pos prior on neg_mort
        cov_cols = cov_cols + year_dummy_cols
        print(f"  Year dummies: {year_dummy_cols}")

    all_model_vars = [spend_col] + cov_cols

    # ── Drop NaN in key variables ──
    key_vars = [mort_col, spend_col] + config['cov_cols'] + [variance_col]  # use original cov_cols for NaN check
    n_before = len(df)
    df = df.dropna(subset=key_vars).copy()
    if len(df) < n_before:
        print(f"  Dropped {n_before - len(df)} rows with NaN")

    # ── Preserve original spending for plotting (pre-normalization) ──
    df['log_spend_orig'] = df[spend_col].copy()

    # ── Z-score normalize all continuous model variables ──
    # (year dummies are 0/1, do NOT normalize them)
    vars_to_zscore = [v for v in all_model_vars if v not in year_dummy_cols]
    zscore_params = {}
    for var in vars_to_zscore:
        mu = df[var].mean()
        sd = df[var].std()
        zscore_params[var] = (mu, sd)
        if sd > 0:
            df[var] = (df[var] - mu) / sd
        else:
            print(f"  WARNING: {var} has zero std, skipping z-score")

    # ── Negate outcome for SFMA (so frontier = upper envelope = best outcomes) ──
    df[mort_col] = -1.0 * df[mort_col]

    # ── Standard error (use median variance, matching Haley's approach) ──
    df['standard_error'] = np.sqrt(np.median(df[variance_col]))

    # ── Sort by spending ──
    df.sort_values(spend_col, inplace=True)
    df.reset_index(drop=True, inplace=True)

    # ══════════════════════════════════════════════════════════════════════
    # BUILD SFMA MODEL
    # ══════════════════════════════════════════════════════════════════════

    def get_model(df, covs_to_use):
        """Build SFMA model with covariates and spending spline."""

        data = Data(obs=mort_col, obs_se='standard_error')

        # Intercept
        variables = [Variable(Component("intercept", default_value=1.0))]

        # Linear covariate Variables with direction priors
        for cov in covs_to_use:
            if cov == spend_col:
                continue
            elif cov in pos_prior_vars:
                variables.append(Variable(cov, priors=[UniformPrior(lb=0.0, ub=np.inf)]))
            elif cov in neg_prior_vars:
                variables.append(Variable(cov, priors=[UniformPrior(lb=-np.inf, ub=0.0)]))
            else:
                # No direction constraint (unconstrained)
                variables.append(Variable(cov))

        # Spending as SplineVariable: increasing + concave (on negated mortality)
        # More spending → lower mortality → higher neg_mort → increasing
        # Diminishing returns → concave
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
                    l_linear=True,
                    r_linear=True,
                ),
                priors=spline_priors,
            )
        )

        model = SFMAModel(data, variables, include_re=False)
        model.attach(df)

        return model

    # ── Initial model with all covariates ──
    covs = cov_cols.copy()
    model = get_model(df, covs)

    # ── Covariate direction test (pre-selection) ──
    print(f"\n  Testing covariate directions...")
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

    # Rebuild model with selected covariates
    model = get_model(df, selected_covs)

    # ── Final fit with 5% trimming ──
    print(f"\n  Fitting with 5% trimming...")
    model.eta = 0.1
    model.beta.fill(1.0)
    model.fit(
        outlier_pct=0.05, trim_max_iter=5,
        verbose=True, max_iter=5, tol=1e-3,
        beta_options={"max_iter": 1000, "verbose": False, "xtol": 0.0, "gtol": 1e-6}
    )

    print(f"\n  Fitted! eta={model.eta:.6f}")
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

    # Build output dataframe
    id_cols = ['location_name', 'location_id', 'year_id', 'cause_id', 'acause', 'cause_name']
    # Only include id_cols that exist in df
    id_cols_present = [c for c in id_cols if c in df.columns]

    out = df[id_cols_present + [spend_col, 'log_spend_orig']].copy()
    out[mort_col] = Y
    out['ineff'] = ineff
    out['y_adj'] = -Y_adj           # flip back: lower = better
    out['y_adj_hat'] = -Y_hat_adj   # frontier in original mortality direction
    out['y_adj_log'] = out['y_adj']
    out['y_adj_hat_log'] = out['y_adj_hat']
    out['model_key'] = model_key

    out.sort_values(spend_col, inplace=True)

    # Covariate info
    covs_only = [c for c in selected_covs if c != spend_col]
    if len(covs_only) > 0:
        covs_df = pd.DataFrame({'selected_covs': covs_only, 'acause': acause, 'model_key': model_key})
    else:
        covs_df = pd.DataFrame({'selected_covs': ['no covs selected'], 'acause': acause, 'model_key': model_key})

    for name, value in betas.items():
        if value.size == 1 and name != spend_col:
            update_idx = covs_df['selected_covs'] == name
            covs_df.loc[update_idx, 'beta'] = value[0]

    # Rescale inefficiency to 0–1
    out['ineff_raw'] = out['ineff']
    if out['ineff'].max() > out['ineff'].min():
        out['ineff'] = (out['ineff'] - out['ineff'].min()) / (out['ineff'].max() - out['ineff'].min())
    else:
        out['ineff'] = 0

    # Summary
    print(f"\n  Results Summary ({config['label']}):")
    print(f"    N observations:     {len(out)}")
    print(f"    Mean inefficiency:  {out['ineff_raw'].mean():.4f}")
    print(f"    Median ineff:       {out['ineff_raw'].median():.4f}")
    print(f"    Min/Max ineff:      {out['ineff_raw'].min():.4f} / {out['ineff_raw'].max():.4f}")
    print(f"    eta:                {model.eta:.6f}")

    return out, covs_df


# ═══════════════════════════════════════════════════════════════════════════════
# PLOTTING
# ═══════════════════════════════════════════════════════════════════════════════

def plot_frontier(df_output, config_label, cov_cols_str, save_path=None):
    """
    Frontier visualization in log-log space.
    X-axis: original log(spending) (pre-normalization)
    Y-axis: covariate-adjusted log(mortality)
    """

    fig, ax = plt.subplots(figsize=(10, 7))

    ax.scatter(
        df_output['log_spend_orig'],
        df_output['y_adj_log'],
        alpha=0.6, s=50,
        label='Observed (covariate-adjusted)',
        c='steelblue', edgecolors='white', linewidth=0.5
    )

    df_sorted = df_output.sort_values('log_spend_orig')
    ax.plot(
        df_sorted['log_spend_orig'],
        df_sorted['y_adj_hat_log'],
        color='darkorange', linewidth=3,
        label='Estimated Frontier',
        zorder=5
    )

    ax.set_xlabel('log(Spending per Prevalent Case)', fontsize=12)
    ax.set_ylabel('log(Mortality per Prevalent Case) [Covariate-Adjusted]', fontsize=12)
    ax.set_title(f'HIV Healthcare Efficiency Frontier: {config_label}\n(n={len(df_output)}, Log-Log Space)',
                 fontsize=14, fontweight='bold')
    ax.legend(loc='upper right', fontsize=10)
    ax.grid(True, alpha=0.3, linestyle='--')

    fig.text(
        0.5, 0.01,
        f'Note: Points above frontier = inefficient. Frontier = best achievable mortality at each spending level.\n'
        f'Covariates: {cov_cols_str}',
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
    print("SFMA FRONTIER ANALYSIS - HIV")
    print("Two models: Between (cross-sectional) & Year-FE (panel)")
    print("=" * 70)
    print(f"  Output: {dir_output}\n")

    all_results = {}

    for model_key in ['between', 'year_fe']:
        config = MODEL_CONFIGS[model_key]

        print(f"\n\n{'#' * 70}")
        print(f"# {config['label']}")
        print(f"{'#' * 70}")

        out, covs = runSFA_HIV(model_key)
        all_results[model_key] = (out, covs)

        # Save outputs
        fname_prefix = f"hiv_{model_key}"
        out.to_csv(dir_output / f'{fname_prefix}_output.csv', index=False)
        covs.to_csv(dir_output / f'{fname_prefix}_covariates.csv', index=False)

        # Covariates string for plot annotation
        cov_str = ', '.join(config['cov_cols'])
        if config['year_fe']:
            cov_str = 'factor(year_id), ' + cov_str

        plot_frontier(
            out,
            config_label=config['label'],
            cov_cols_str=cov_str,
            save_path=dir_output / f'{fname_prefix}_frontier.png'
        )

    # ─────────────────────────────────────────────────────────────────────
    # SUMMARY COMPARISON
    # ─────────────────────────────────────────────────────────────────────
    print("\n\n" + "=" * 90)
    print("SUMMARY COMPARISON: BOTH MODELS")
    print("=" * 90)

    print(f"\n{'Model':<30} {'N':>6} {'Mean Ineff':>12} {'Med Ineff':>12} {'Max Ineff':>12}")
    print("-" * 75)

    for model_key, (out_df, _) in all_results.items():
        label = MODEL_CONFIGS[model_key]['label']
        print(f"{label:<30} {len(out_df):>6} "
              f"{out_df['ineff_raw'].mean():>12.4f} {out_df['ineff_raw'].median():>12.4f} "
              f"{out_df['ineff_raw'].max():>12.4f}")

    print("\n" + "=" * 90)
    print("ANALYSIS COMPLETE!")
    print(f"Output files saved to: {dir_output}")
    print("=" * 90)

    print("\nOutput files generated:")
    for f in sorted(dir_output.glob('hiv_*')):
        print(f"  - {f.name}")