"""
Filename: C_frontier_analysis_sfma_hiv.py
Description: SFMA frontier analysis for HIV (Aim 2).
             Year-FE panel model only (~510 obs), two outcomes: Mortality and DALY.
             No stratification by prevalence/incidence.

Both models share identical covariates and prior directions:
  Spending (SplineVariable): rw_dex_hiv_prev_ratio_log  → increasing + concave on neg outcome
  Year dummies (2011–2019, ref=2010):  all neg on outcome → pos prior on neg outcome
  race_prop_BLCK:     +0.624 / +0.492  → neg prior on neg outcome (n.s. in both)
  log_incidence_rates: -0.297 / -0.285 → pos prior on neg outcome
  race_prop_HISP:     +0.739 / +0.780  → neg prior on neg outcome
  log_prop_homeless:  +0.039 / +0.055  → neg prior on neg outcome (n.s. mort / sig DALY)

Outputs per outcome:
  - hiv_yfe_{outcome}_output.csv          (state-year level, n=510)
  - hiv_yfe_{outcome}_state_summary.csv   (state level, n=51)
  - hiv_yfe_{outcome}_covariates.csv
  - hiv_yfe_{outcome}_frontier.png
  - hiv_yfe_{outcome}_state_frontier.png  (state abbreviation labels)

How to run: python C_frontier_analysis_sfma_hiv.py
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

fp_input_panel = Path('/ihme/homes/idrisov/aim_outputs/Aim2/C_frontier_analysis/20260303/analysis/df_hiv_analysis_panel.csv')

today_yyyymmdd = date.today().strftime("%Y%m%d")
dir_output = dir_base / today_yyyymmdd
dir_output.mkdir(parents=True, exist_ok=True)

# ── Common column names ──
spend_col    = 'rw_dex_hiv_prev_ratio_log'
variance_col = 'variance'

acause = 'hiv'

# ── State name → abbreviation mapping ──
STATE_ABBREV = {
    'Alabama': 'AL', 'Alaska': 'AK', 'Arizona': 'AZ', 'Arkansas': 'AR',
    'California': 'CA', 'Colorado': 'CO', 'Connecticut': 'CT', 'Delaware': 'DE',
    'Florida': 'FL', 'Georgia': 'GA', 'Hawaii': 'HI', 'Idaho': 'ID',
    'Illinois': 'IL', 'Indiana': 'IN', 'Iowa': 'IA', 'Kansas': 'KS',
    'Kentucky': 'KY', 'Louisiana': 'LA', 'Maine': 'ME', 'Maryland': 'MD',
    'Massachusetts': 'MA', 'Michigan': 'MI', 'Minnesota': 'MN', 'Mississippi': 'MS',
    'Missouri': 'MO', 'Montana': 'MT', 'Nebraska': 'NE', 'Nevada': 'NV',
    'New Hampshire': 'NH', 'New Jersey': 'NJ', 'New Mexico': 'NM', 'New York': 'NY',
    'North Carolina': 'NC', 'North Dakota': 'ND', 'Ohio': 'OH', 'Oklahoma': 'OK',
    'Oregon': 'OR', 'Pennsylvania': 'PA', 'Rhode Island': 'RI', 'South Carolina': 'SC',
    'South Dakota': 'SD', 'Tennessee': 'TN', 'Texas': 'TX', 'Utah': 'UT',
    'Vermont': 'VT', 'Virginia': 'VA', 'Washington': 'WA', 'West Virginia': 'WV',
    'Wisconsin': 'WI', 'Wyoming': 'WY', 'District of Columbia': 'DC',
}

# ── Outcome configurations ──
OUTCOME_CONFIGS = {
    'mortality': {
        'label': 'HIV Mortality (Year-FE, n≈510)',
        'outcome_col': 'as_mort_prev_ratio_log',
    },
    'daly': {
        'label': 'HIV DALY (Year-FE, n≈510)',
        'outcome_col': 'as_daly_prev_ratio_log',
    },
}

# Shared across both outcomes
COV_COLS = ['race_prop_BLCK', 'log_incidence_rates', 'race_prop_HISP', 'log_prop_homeless']
POS_PRIOR_VARS_BASE = ['log_incidence_rates']
NEG_PRIOR_VARS = ['race_prop_BLCK', 'race_prop_HISP', 'log_prop_homeless']


# ═══════════════════════════════════════════════════════════════════════════════
# MAIN FUNCTION
# ═══════════════════════════════════════════════════════════════════════════════

def runSFA_HIV(outcome_key):
    """
    Run SFMA frontier analysis for HIV, Year-FE panel model.

    Parameters
    ----------
    outcome_key : str
        'mortality' or 'daly'

    Returns
    -------
    out : pd.DataFrame
        State-year output with inefficiency scores
    state_summary : pd.DataFrame
        State-level collapsed summary
    covs_df : pd.DataFrame
        Selected covariate information and betas
    """

    oc = OUTCOME_CONFIGS[outcome_key]
    outcome_col = oc['outcome_col']
    cov_cols = COV_COLS.copy()
    pos_prior_vars = POS_PRIOR_VARS_BASE.copy()
    neg_prior_vars = NEG_PRIOR_VARS.copy()

    # ── Read data ──
    df = pd.read_csv(fp_input_panel)

    print(f"\n{'=' * 70}")
    print(f"SFMA HIV FRONTIER: {oc['label'].upper()}")
    print(f"  Input:    {fp_input_panel}")
    print(f"  Outcome:  {outcome_col}")
    print(f"  n = {len(df)}")
    print(f"  Covariates: {cov_cols}")
    print(f"  Year FE: YES (dummies, ref = first year)")
    print(f"{'=' * 70}")

    # ── Create year dummies ──
    year_dummy_cols = []
    years = sorted(df['year_id'].unique())
    ref_year = years[0]
    print(f"  Reference year: {ref_year}")
    for yr in years[1:]:
        col = f'year_{yr}'
        df[col] = (df['year_id'] == yr).astype(float)
        year_dummy_cols.append(col)
        pos_prior_vars.append(col)
    cov_cols = cov_cols + year_dummy_cols
    print(f"  Year dummies: {year_dummy_cols}")

    all_model_vars = [spend_col] + cov_cols

    # ── Drop NaN ──
    key_vars = [outcome_col, spend_col] + COV_COLS + [variance_col]
    n_before = len(df)
    df = df.dropna(subset=key_vars).copy()
    if len(df) < n_before:
        print(f"  Dropped {n_before - len(df)} rows with NaN")

    # ── Preserve original spending for plotting ──
    df['log_spend_orig'] = df[spend_col].copy()

    # ── Z-score continuous variables only ──
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

    # ── Negate outcome ──
    df[outcome_col] = -1.0 * df[outcome_col]

    # ── Standard error ──
    df['standard_error'] = np.sqrt(np.median(df[variance_col]))

    # ── Sort by spending ──
    df.sort_values(spend_col, inplace=True)
    df.reset_index(drop=True, inplace=True)

    # ══════════════════════════════════════════════════════════════════════
    # BUILD SFMA MODEL
    # ══════════════════════════════════════════════════════════════════════

    def get_model(df, covs_to_use):
        data = Data(obs=outcome_col, obs_se='standard_error')
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

        spline_priors = [
            SplinePriorGetter(UniformPrior(lb=0.0, ub=np.inf), order=1, size=100),
            SplinePriorGetter(UniformPrior(lb=-np.inf, ub=0.0), order=2, size=100),
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

    # ── Initial model ──
    covs = cov_cols.copy()
    model = get_model(df, covs)

    # ── Covariate direction test ──
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

    Y = df[outcome_col].values
    Y_hat = model.predict(df)

    df_null_covs = df.copy()
    covs_to_zero = [c for c in selected_covs if c != spend_col]
    if covs_to_zero:
        df_null_covs[covs_to_zero] = 0.0
    Y_hat_adj = model.predict(df_null_covs)

    cov_hat = Y_hat - Y_hat_adj
    Y_adj = Y - cov_hat
    ineff = model.get_inefficiency()

    id_cols = ['location_name', 'location_id', 'year_id', 'cause_id', 'acause', 'cause_name']
    id_cols_present = [c for c in id_cols if c in df.columns]

    out = df[id_cols_present + [spend_col, 'log_spend_orig']].copy()
    out[outcome_col] = Y
    out['ineff'] = ineff
    out['y_adj'] = -Y_adj
    out['y_adj_hat'] = -Y_hat_adj
    out['y_adj_log'] = out['y_adj']
    out['y_adj_hat_log'] = out['y_adj_hat']
    out['outcome_key'] = outcome_key
    out.sort_values(spend_col, inplace=True)

    # Rescale inefficiency to 0–1
    out['ineff_raw'] = out['ineff']
    if out['ineff'].max() > out['ineff'].min():
        out['ineff'] = (out['ineff'] - out['ineff'].min()) / (out['ineff'].max() - out['ineff'].min())
    else:
        out['ineff'] = 0

    # ══════════════════════════════════════════════════════════════════════
    # STATE-LEVEL COLLAPSED SUMMARY
    # ══════════════════════════════════════════════════════════════════════

    state_mean = out.groupby(['location_name', 'location_id']).agg(
        ineff_raw_mean=('ineff_raw', 'mean'),
        ineff_mean=('ineff', 'mean'),
    ).reset_index()

    early = out[out['year_id'].between(2010, 2014)].groupby(['location_name', 'location_id'])['ineff_raw'].mean().rename('ineff_raw_early')
    late  = out[out['year_id'].between(2015, 2019)].groupby(['location_name', 'location_id'])['ineff_raw'].mean().rename('ineff_raw_late')

    early_sc = out[out['year_id'].between(2010, 2014)].groupby(['location_name', 'location_id'])['ineff'].mean().rename('ineff_early')
    late_sc  = out[out['year_id'].between(2015, 2019)].groupby(['location_name', 'location_id'])['ineff'].mean().rename('ineff_late')

    # Mean spending per state (original scale for state-level plot)
    spend_mean = out.groupby(['location_name', 'location_id'])['log_spend_orig'].mean().rename('log_spend_mean')
    # Mean covariate-adjusted outcome per state
    y_adj_mean = out.groupby(['location_name', 'location_id'])['y_adj_log'].mean().rename('y_adj_log_mean')

    state_summary = state_mean \
        .merge(early, on=['location_name', 'location_id']) \
        .merge(late, on=['location_name', 'location_id']) \
        .merge(early_sc, on=['location_name', 'location_id']) \
        .merge(late_sc, on=['location_name', 'location_id']) \
        .merge(spend_mean, on=['location_name', 'location_id']) \
        .merge(y_adj_mean, on=['location_name', 'location_id'])

    state_summary['ineff_raw_change'] = state_summary['ineff_raw_late'] - state_summary['ineff_raw_early']
    state_summary['ineff_change'] = state_summary['ineff_late'] - state_summary['ineff_early']

    # State abbreviation
    state_summary['state_abbrev'] = state_summary['location_name'].map(STATE_ABBREV).fillna(
        state_summary['location_name'].str[:2].str.upper()
    )

    state_summary['outcome_key'] = outcome_key

    # ── Reorder columns logically ──
    col_order = [
        'location_name', 'location_id', 'state_abbrev', 'outcome_key',
        # Raw inefficiency block
        'ineff_raw_mean', 'ineff_raw_early', 'ineff_raw_late', 'ineff_raw_change',
        # Scaled (0–1) inefficiency block
        'ineff_mean', 'ineff_early', 'ineff_late', 'ineff_change',
        # Spending & outcome for state-level plot
        'log_spend_mean', 'y_adj_log_mean',
    ]
    state_summary = state_summary[col_order]
    state_summary.sort_values('ineff_mean', inplace=True)

    # ── Add "United States" row with mean across all states ──
    us_row = pd.DataFrame([{
        'location_name': 'United States',
        'location_id': 102,
        'state_abbrev': 'US',
        'outcome_key': outcome_key,
        **{c: state_summary[c].mean() for c in col_order
           if c not in ['location_name', 'location_id', 'state_abbrev', 'outcome_key']},
    }])
    state_summary = pd.concat([state_summary, us_row], ignore_index=True)

    # Covariate info
    covs_only = [c for c in selected_covs if c != spend_col]
    if len(covs_only) > 0:
        covs_df = pd.DataFrame({'selected_covs': covs_only, 'acause': acause, 'outcome_key': outcome_key})
    else:
        covs_df = pd.DataFrame({'selected_covs': ['no covs selected'], 'acause': acause, 'outcome_key': outcome_key})

    for name, value in betas.items():
        if value.size == 1 and name != spend_col:
            update_idx = covs_df['selected_covs'] == name
            covs_df.loc[update_idx, 'beta'] = value[0]

    # Summary print
    print(f"\n  Results Summary ({oc['label']}):")
    print(f"    N state-years:      {len(out)}")
    print(f"    N states:           {len(state_summary)}")
    print(f"    Mean ineff (raw):   {out['ineff_raw'].mean():.4f}")
    print(f"    Median ineff (raw): {out['ineff_raw'].median():.4f}")
    print(f"    eta:                {model.eta:.6f}")
    print(f"\n    State-level change (2015-19 minus 2010-14):")
    print(f"      Mean change (scaled): {state_summary['ineff_change'].mean():.4f}")
    print(f"      States improving:     {(state_summary['ineff_change'] < 0).sum()}")
    print(f"      States worsening:     {(state_summary['ineff_change'] > 0).sum()}")

    return out, state_summary, covs_df


# ═══════════════════════════════════════════════════════════════════════════════
# PLOTTING
# ═══════════════════════════════════════════════════════════════════════════════

def plot_frontier(df_output, config_label, outcome_col, save_path=None):
    """Frontier visualization in log-log space (state-year dots)."""

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
    outcome_label = 'Mortality' if 'mort' in outcome_col else 'DALY'
    ax.set_ylabel(f'log({outcome_label} per Prevalent Case) [Covariate-Adjusted]', fontsize=12)
    ax.set_title(f'HIV Healthcare Efficiency Frontier: {config_label}\n(n={len(df_output)}, Log-Log Space)',
                 fontsize=14, fontweight='bold')
    ax.legend(loc='upper right', fontsize=10)
    ax.grid(True, alpha=0.3, linestyle='--')

    cov_str = 'factor(year_id), race_prop_BLCK, log_incidence_rates, race_prop_HISP, log_prop_homeless'
    fig.text(
        0.5, 0.01,
        f'Note: Points above frontier = inefficient. Frontier = best achievable outcome at each spending level.\n'
        f'Covariates: {cov_str}',
        ha='center', fontsize=8, style='italic', color='gray'
    )

    plt.tight_layout(rect=[0, 0.05, 1, 1])

    if save_path:
        plt.savefig(save_path, dpi=150, bbox_inches='tight')
        print(f"  Plot saved to: {save_path}")

    plt.close(fig)
    return fig


def plot_state_frontier(state_summary, config_label, outcome_col, save_path=None):
    """
    State-level frontier plot with state abbreviation labels instead of dots.
    X-axis: mean log(spending) across years
    Y-axis: mean covariate-adjusted log(outcome) across years
    Color: mean inefficiency (scaled 0–1)
    """

    fig, ax = plt.subplots(figsize=(12, 8))

    outcome_label = 'Mortality' if 'mort' in outcome_col else 'DALY'

    # Color by inefficiency (exclude US summary row)
    plot_df = state_summary[state_summary['location_name'] != 'United States'].copy()
    ineff_vals = plot_df['ineff_mean'].values
    norm = plt.Normalize(vmin=ineff_vals.min(), vmax=ineff_vals.max())
    cmap = plt.cm.RdYlGn_r  # red = high inefficiency, green = low

    for _, row in plot_df.iterrows():
        color = cmap(norm(row['ineff_mean']))
        ax.text(
            row['log_spend_mean'],
            row['y_adj_log_mean'],
            row['state_abbrev'],
            fontsize=8, fontweight='bold',
            ha='center', va='center',
            color=color,
            zorder=5
        )

    # Colorbar
    sm = plt.cm.ScalarMappable(cmap=cmap, norm=norm)
    sm.set_array([])
    cbar = plt.colorbar(sm, ax=ax, shrink=0.8, pad=0.02)
    cbar.set_label('Mean Inefficiency (0=most efficient, 1=least)', fontsize=10)

    ax.set_xlabel('Mean log(Spending per Prevalent Case)', fontsize=12)
    ax.set_ylabel(f'Mean log({outcome_label} per Prevalent Case) [Covariate-Adjusted]', fontsize=12)
    ax.set_title(
        f'HIV State-Level Efficiency: {config_label}\n'
        f'(State means across 2010–2019, n={len(plot_df)} states)',
        fontsize=14, fontweight='bold'
    )
    ax.grid(True, alpha=0.3, linestyle='--')

    fig.text(
        0.5, 0.01,
        f'Note: Lower {outcome_label.lower()} = better. Green = more efficient, Red = less efficient.\n'
        f'Each label is a state abbreviation positioned at its mean spending and covariate-adjusted {outcome_label.lower()}.',
        ha='center', fontsize=8, style='italic', color='gray'
    )

    plt.tight_layout(rect=[0, 0.05, 1, 1])

    if save_path:
        plt.savefig(save_path, dpi=150, bbox_inches='tight')
        print(f"  State plot saved to: {save_path}")

    plt.close(fig)
    return fig


# ═══════════════════════════════════════════════════════════════════════════════
# MAIN EXECUTION
# ═══════════════════════════════════════════════════════════════════════════════

if __name__ == '__main__':

    print("\n" + "=" * 70)
    print("SFMA FRONTIER ANALYSIS - HIV")
    print("Year-FE panel model, two outcomes: Mortality & DALY")
    print("=" * 70)
    print(f"  Input:  {fp_input_panel}")
    print(f"  Output: {dir_output}\n")

    all_results = {}

    for outcome_key in ['mortality', 'daly']:
        oc = OUTCOME_CONFIGS[outcome_key]

        print(f"\n\n{'#' * 70}")
        print(f"# {oc['label']}")
        print(f"{'#' * 70}")

        out, state_summary, covs = runSFA_HIV(outcome_key)
        all_results[outcome_key] = (out, state_summary, covs)

        # Save outputs
        fname_prefix = f"hiv_yfe_{outcome_key}"
        out.to_csv(dir_output / f'{fname_prefix}_output.csv', index=False)
        state_summary.to_csv(dir_output / f'{fname_prefix}_state_summary.csv', index=False)
        covs.to_csv(dir_output / f'{fname_prefix}_covariates.csv', index=False)

        # State-year frontier plot
        plot_frontier(
            out,
            config_label=oc['label'],
            outcome_col=oc['outcome_col'],
            save_path=dir_output / f'{fname_prefix}_frontier.png'
        )

        # State-level abbreviation plot
        plot_state_frontier(
            state_summary,
            config_label=oc['label'],
            outcome_col=oc['outcome_col'],
            save_path=dir_output / f'{fname_prefix}_state_frontier.png'
        )

    # ─────────────────────────────────────────────────────────────────────
    # SUMMARY COMPARISON
    # ─────────────────────────────────────────────────────────────────────
    print("\n\n" + "=" * 90)
    print("SUMMARY COMPARISON: MORTALITY vs DALY")
    print("=" * 90)

    print(f"\n{'Outcome':<15} {'N':>6} {'Mean Ineff':>12} {'Med Ineff':>12} {'Max Ineff':>12} {'Δ Ineff':>12}")
    print("-" * 75)

    for outcome_key, (out_df, state_df, _) in all_results.items():
        print(f"{outcome_key:<15} {len(out_df):>6} "
              f"{out_df['ineff_raw'].mean():>12.4f} {out_df['ineff_raw'].median():>12.4f} "
              f"{out_df['ineff_raw'].max():>12.4f} {state_df['ineff_raw_change'].mean():>12.4f}")

    print("\n" + "=" * 90)
    print("ANALYSIS COMPLETE!")
    print(f"Output files saved to: {dir_output}")
    print("=" * 90)

    print("\nOutput files generated:")
    for f in sorted(dir_output.glob('hiv_yfe_*')):
        print(f"  - {f.name}")