"""
Filename: C_frontier_analysis_sfma_hiv_stratified.py
Description: SFMA frontier analysis for HIV (Aim 2), STRATIFIED by 2010 GBD-prevalence tercile.
             Year-FE panel model, two outcomes (Mortality and DALY), three terciles each.
             Produces 6 frontier figures + 6 state-frontier figures + 18 CSVs.

Tercile definition (mirrors aim2 R pipeline C_02_HIV_manuscript_regressions.R, sec 2):
  - Per location, take log_prevalence_per_100k at year_id == 2010 as baseline.
  - Cut at quantiles 1/3 and 2/3 across the 51 baseline values (numpy default = R type 7).
  - Low <= cut1 < Mid <= cut2 < High.
  - Fixed throughout 2010-2019: a state stays in its tercile for all 10 years.
  - Assignment added as 'prev_tercile_GBD' BEFORE any modeling.

SFMA spec is identical to C_frontier_analysis_smfa.py: same covariates, priors, knots,
z-scoring, outcome negation, year-FE construction, and 5% trimming. Only the input
subset changes.

Outputs per (outcome, tercile):
  - hiv_yfe_{outcome}_{tercile}_output.csv          (state-year)
  - hiv_yfe_{outcome}_{tercile}_state_summary.csv   (state)
  - hiv_yfe_{outcome}_{tercile}_covariates.csv
  - hiv_yfe_{outcome}_{tercile}_frontier.png
  - hiv_yfe_{outcome}_{tercile}_state_frontier.png

Plus once globally:
  - hiv_yfe_tercile_assignment.csv

How to run: python C_frontier_analysis_sfma_hiv_stratified.py
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
tercile_col  = 'prev_tercile_GBD'
prev_col     = 'log_prevalence_per_100k'

acause = 'hiv'

MIN_OBS_PER_CELL = 50

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

# ── Outcome configurations (label is templated; updated per cell at runtime) ──
OUTCOME_CONFIGS = {
    'mortality': {
        'label': 'HIV Mortality',
        'outcome_col': 'as_mort_prev_ratio_log',
    },
    'daly': {
        'label': 'HIV DALY',
        'outcome_col': 'as_daly_prev_ratio_log',
    },
}

TERCILE_LABELS = {
    'low':  'Low-prevalence tercile',
    'mid':  'Mid-prevalence tercile',
    'high': 'High-prevalence tercile',
}
TERCILE_R_NAMES = {'low': 'Low', 'mid': 'Mid', 'high': 'High'}

# Shared across both outcomes
COV_COLS = ['race_prop_BLCK', 'log_incidence_rates', 'race_prop_HISP', 'log_prop_homeless']
POS_PRIOR_VARS_BASE = ['log_incidence_rates']
NEG_PRIOR_VARS = ['race_prop_BLCK', 'race_prop_HISP', 'log_prop_homeless']


# ═══════════════════════════════════════════════════════════════════════════════
# TERCILE ASSIGNMENT (mirrors R: C_02_HIV_manuscript_regressions.R sec 2)
# ═══════════════════════════════════════════════════════════════════════════════

def assign_gbd_terciles(df):
    """
    Assign each state to a GBD-prevalence tercile using its 2010 baseline
    log_prevalence_per_100k. Cuts at 1/3 and 2/3 quantiles across the 51 states.
    Returns:
        df_out: full panel with 'prev_tercile_GBD' column ('Low'/'Mid'/'High')
        assign_df: state-level reference table
    """
    if prev_col not in df.columns:
        raise ValueError(
            f"Input panel missing required column '{prev_col}' needed for tercile assignment."
        )

    baseline = (
        df.loc[df['year_id'] == 2010, ['location_id', 'location_name', prev_col]]
          .dropna(subset=[prev_col])
          .drop_duplicates(subset='location_id')
          .copy()
    )
    baseline = baseline.rename(columns={prev_col: 'log_prevalence_per_100k_2010'})

    cuts = np.quantile(
        baseline['log_prevalence_per_100k_2010'].values,
        q=[1/3, 2/3],
    )
    print(f"  GBD tercile cuts (log prev/100k, 2010): low<={cuts[0]:.3f}  high>{cuts[1]:.3f}")

    def _label(x):
        if x <= cuts[0]:
            return 'Low'
        elif x <= cuts[1]:
            return 'Mid'
        else:
            return 'High'

    baseline[tercile_col] = baseline['log_prevalence_per_100k_2010'].apply(_label)
    baseline['state_abbrev'] = baseline['location_name'].map(STATE_ABBREV).fillna(
        baseline['location_name'].str[:2].str.upper()
    )

    df_out = df.merge(
        baseline[['location_id', tercile_col]],
        on='location_id', how='left',
    )

    n_missing = df_out[tercile_col].isna().sum()
    if n_missing > 0:
        print(f"  WARNING: {n_missing} state-year rows have no tercile assignment "
              f"(missing 2010 baseline prevalence).")

    counts = baseline[tercile_col].value_counts().reindex(['Low', 'Mid', 'High']).fillna(0).astype(int)
    print(f"  States per tercile: Low={counts['Low']}, Mid={counts['Mid']}, High={counts['High']}")

    assign_df = baseline[[
        'location_id', 'location_name', 'state_abbrev',
        'log_prevalence_per_100k_2010', tercile_col,
    ]].sort_values(['prev_tercile_GBD', 'location_name']).reset_index(drop=True)

    return df_out, assign_df


# ═══════════════════════════════════════════════════════════════════════════════
# MAIN FUNCTION
# ═══════════════════════════════════════════════════════════════════════════════

def runSFA_HIV(df_full, outcome_key, tercile_key):
    """
    Run SFMA frontier analysis for HIV on a single (outcome, tercile) cell.

    Parameters
    ----------
    df_full : pd.DataFrame
        Full panel with 'prev_tercile_GBD' already assigned.
    outcome_key : str
        'mortality' or 'daly'
    tercile_key : str
        'low', 'mid', or 'high'

    Returns
    -------
    out : pd.DataFrame
    state_summary : pd.DataFrame
    covs_df : pd.DataFrame
    label : str
        Run-time label used in plot titles (includes tercile + n).
    """

    oc = OUTCOME_CONFIGS[outcome_key]
    outcome_col = oc['outcome_col']
    cov_cols = COV_COLS.copy()
    pos_prior_vars = POS_PRIOR_VARS_BASE.copy()
    neg_prior_vars = NEG_PRIOR_VARS.copy()

    tercile_r = TERCILE_R_NAMES[tercile_key]
    tercile_label = TERCILE_LABELS[tercile_key]

    # ── Subset to tercile ──
    df = df_full[df_full[tercile_col] == tercile_r].copy()
    n_subset = len(df)

    label = f"{oc['label']} — {tercile_label} (Year-FE, n≈{n_subset})"

    print(f"\n{'=' * 70}")
    print(f"SFMA HIV FRONTIER: {label.upper()}")
    print(f"  Input:    {fp_input_panel}")
    print(f"  Outcome:  {outcome_col}")
    print(f"  Tercile:  {tercile_r}  ({n_subset} state-year rows pre-NaN-drop)")
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

    if len(df) < MIN_OBS_PER_CELL:
        print(f"  WARNING: Only {len(df)} obs remain after NaN drop "
              f"(< MIN_OBS_PER_CELL={MIN_OBS_PER_CELL}). Skipping this cell.")
        return None, None, None, label

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
    out['prev_tercile_GBD'] = tercile_r
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

    spend_mean = out.groupby(['location_name', 'location_id'])['log_spend_orig'].mean().rename('log_spend_mean')
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

    state_summary['state_abbrev'] = state_summary['location_name'].map(STATE_ABBREV).fillna(
        state_summary['location_name'].str[:2].str.upper()
    )

    state_summary['outcome_key'] = outcome_key
    state_summary['prev_tercile_GBD'] = tercile_r

    col_order = [
        'location_name', 'location_id', 'state_abbrev', 'outcome_key', 'prev_tercile_GBD',
        'ineff_raw_mean', 'ineff_raw_early', 'ineff_raw_late', 'ineff_raw_change',
        'ineff_mean', 'ineff_early', 'ineff_late', 'ineff_change',
        'log_spend_mean', 'y_adj_log_mean',
    ]
    state_summary = state_summary[col_order]
    state_summary.sort_values('ineff_mean', inplace=True)

    # Tercile-level "United States-like" summary row (mean across states in this tercile)
    us_row = pd.DataFrame([{
        'location_name': f'Tercile mean ({tercile_r})',
        'location_id': -1,
        'state_abbrev': 'TM',
        'outcome_key': outcome_key,
        'prev_tercile_GBD': tercile_r,
        **{c: state_summary[c].mean() for c in col_order
           if c not in ['location_name', 'location_id', 'state_abbrev',
                        'outcome_key', 'prev_tercile_GBD']},
    }])
    state_summary = pd.concat([state_summary, us_row], ignore_index=True)

    covs_only = [c for c in selected_covs if c != spend_col]
    if len(covs_only) > 0:
        covs_df = pd.DataFrame({'selected_covs': covs_only, 'acause': acause,
                                'outcome_key': outcome_key, 'prev_tercile_GBD': tercile_r})
    else:
        covs_df = pd.DataFrame({'selected_covs': ['no covs selected'], 'acause': acause,
                                'outcome_key': outcome_key, 'prev_tercile_GBD': tercile_r})

    for name, value in betas.items():
        if value.size == 1 and name != spend_col:
            update_idx = covs_df['selected_covs'] == name
            covs_df.loc[update_idx, 'beta'] = value[0]

    print(f"\n  Results Summary ({label}):")
    print(f"    N state-years:      {len(out)}")
    print(f"    N states:           {len(state_summary) - 1}")  # minus tercile-mean row
    print(f"    Mean ineff (raw):   {out['ineff_raw'].mean():.4f}")
    print(f"    Median ineff (raw): {out['ineff_raw'].median():.4f}")
    print(f"    eta:                {model.eta:.6f}")
    print(f"\n    State-level change (2015-19 minus 2010-14):")
    print(f"      Mean change (scaled): {state_summary['ineff_change'].mean():.4f}")
    print(f"      States improving:     {(state_summary['ineff_change'] < 0).sum()}")
    print(f"      States worsening:     {(state_summary['ineff_change'] > 0).sum()}")

    # Update label with final n (post-NaN-drop)
    label = f"{oc['label']} — {tercile_label} (Year-FE, n≈{len(out)})"

    return out, state_summary, covs_df, label


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

    # Color by inefficiency (exclude tercile-mean summary row, marked with location_id == -1)
    plot_df = state_summary[state_summary['location_id'] != -1].copy()
    ineff_vals = plot_df['ineff_mean'].values
    norm = plt.Normalize(vmin=ineff_vals.min(), vmax=ineff_vals.max())
    cmap = plt.cm.RdYlGn_r

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

    sm = plt.cm.ScalarMappable(cmap=cmap, norm=norm)
    sm.set_array([])
    cbar = plt.colorbar(sm, ax=ax, shrink=0.8, pad=0.02)
    cbar.set_label('Mean Inefficiency (0=most efficient, 1=least)', fontsize=10)

    ax.set_xlabel('Mean log(Spending per Prevalent Case)', fontsize=12)
    ax.set_ylabel(f'Mean log({outcome_label} per Prevalent Case) [Covariate-Adjusted]', fontsize=12)
    ax.set_title(
        f'HIV State-Level Efficiency: {config_label}\n'
        f'(state means 2010–2019, n={len(plot_df)} states)',
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
    print("SFMA FRONTIER ANALYSIS - HIV (STRATIFIED BY GBD-PREVALENCE TERCILE)")
    print("Year-FE panel model | 2 outcomes (Mortality, DALY) x 3 terciles (Low, Mid, High)")
    print("=" * 70)
    print(f"  Input:  {fp_input_panel}")
    print(f"  Output: {dir_output}\n")

    # ── Load full panel + assign terciles ──
    df_full = pd.read_csv(fp_input_panel)
    print(f"\nLoaded full panel: n={len(df_full)} rows, "
          f"{df_full['location_id'].nunique()} states, "
          f"{df_full['year_id'].nunique()} years")

    df_full, assign_df = assign_gbd_terciles(df_full)

    fp_assign = dir_output / 'hiv_yfe_tercile_assignment.csv'
    assign_df.to_csv(fp_assign, index=False)
    print(f"  Tercile assignment saved to: {fp_assign}")

    # ── Run grid of (outcome, tercile) ──
    all_results = {}  # keyed by (outcome_key, tercile_key) -> (out, state_summary, covs, label)

    for outcome_key in ['mortality', 'daly']:
        oc = OUTCOME_CONFIGS[outcome_key]

        for tercile_key in ['low', 'mid', 'high']:
            tercile_label = TERCILE_LABELS[tercile_key]

            print(f"\n\n{'#' * 70}")
            print(f"# {oc['label']} — {tercile_label}")
            print(f"{'#' * 70}")

            out, state_summary, covs, label = runSFA_HIV(df_full, outcome_key, tercile_key)

            if out is None:
                print(f"  Skipping (outcome={outcome_key}, tercile={tercile_key}) — too few obs.")
                all_results[(outcome_key, tercile_key)] = (None, None, None, label)
                continue

            all_results[(outcome_key, tercile_key)] = (out, state_summary, covs, label)

            fname_prefix = f"hiv_yfe_{outcome_key}_{tercile_key}"
            out.to_csv(dir_output / f'{fname_prefix}_output.csv', index=False)
            state_summary.to_csv(dir_output / f'{fname_prefix}_state_summary.csv', index=False)
            covs.to_csv(dir_output / f'{fname_prefix}_covariates.csv', index=False)

            plot_frontier(
                out,
                config_label=label,
                outcome_col=oc['outcome_col'],
                save_path=dir_output / f'{fname_prefix}_frontier.png',
            )

            plot_state_frontier(
                state_summary,
                config_label=label,
                outcome_col=oc['outcome_col'],
                save_path=dir_output / f'{fname_prefix}_state_frontier.png',
            )

    # ─────────────────────────────────────────────────────────────────────
    # SUMMARY GRID: 2 outcomes x 3 terciles
    # ─────────────────────────────────────────────────────────────────────
    print("\n\n" + "=" * 95)
    print("SUMMARY GRID: OUTCOME x GBD-PREVALENCE TERCILE")
    print("=" * 95)
    print(f"\n{'Outcome':<10} {'Tercile':<8} {'N':>6} {'Mean Ineff':>12} "
          f"{'Med Ineff':>12} {'Max Ineff':>12} {'Δ Ineff':>12}")
    print("-" * 85)

    summary_rows = []
    for outcome_key in ['mortality', 'daly']:
        for tercile_key in ['low', 'mid', 'high']:
            out_df, state_df, _, _ = all_results[(outcome_key, tercile_key)]
            if out_df is None:
                print(f"{outcome_key:<10} {tercile_key:<8} {'SKIPPED — too few observations':>60}")
                summary_rows.append({
                    'outcome': outcome_key, 'tercile': tercile_key,
                    'n_state_years': 0, 'mean_ineff_raw': np.nan,
                    'median_ineff_raw': np.nan, 'max_ineff_raw': np.nan,
                    'delta_ineff_raw': np.nan,
                })
                continue
            row = {
                'outcome': outcome_key,
                'tercile': tercile_key,
                'n_state_years': len(out_df),
                'mean_ineff_raw': out_df['ineff_raw'].mean(),
                'median_ineff_raw': out_df['ineff_raw'].median(),
                'max_ineff_raw': out_df['ineff_raw'].max(),
                'delta_ineff_raw': state_df['ineff_raw_change'].mean(),
            }
            summary_rows.append(row)
            print(f"{outcome_key:<10} {tercile_key:<8} {row['n_state_years']:>6} "
                  f"{row['mean_ineff_raw']:>12.4f} {row['median_ineff_raw']:>12.4f} "
                  f"{row['max_ineff_raw']:>12.4f} {row['delta_ineff_raw']:>12.4f}")

    summary_df = pd.DataFrame(summary_rows)
    summary_df.to_csv(dir_output / 'hiv_yfe_summary_grid.csv', index=False)

    print("\n" + "=" * 95)
    print("ANALYSIS COMPLETE!")
    print(f"Output files saved to: {dir_output}")
    print("=" * 95)

    print("\nOutput files generated:")
    for f in sorted(dir_output.glob('hiv_yfe_*')):
        print(f"  - {f.name}")
