"""
Filename: C_frontier_batch_fit_mvp.py
Description: Batch SFMA frontier fitter — MVP version (Option C).

  Fits 10 frontiers across 5 families × 2 specs each:
    - kxv_per_case       : 03_homeless, 04_lagPrev
    - kxv_per_capita     : 03_homeless, 04_lagPrev
    - k_only_per_case    : 03_homeless, 04_lagPrev
    - k_only_per_capita  : 03_homeless, 04_lagPrev
    - incidence_per_capita: 03_homeless, 04_contempPrev

  For each spec: same logic as v5 Path B (dummy-trap fix, hardcoded σ,
  cost-direction handling for incidence) but parameterized.

  Outputs (CSVs only; plotting is done by plot_frontiers_mvp.R):
    frontiers_long.csv      — one row per state-year per spec
    frontiers_metadata.csv  — one row per spec (model summary stats)

How to run: python C_frontier_batch_fit_mvp.py
"""
# ═══════════════════════════════════════════════════════════════════════════════
# IMPORTS & CONFIGURATION
# ═══════════════════════════════════════════════════════════════════════════════
from pathlib import Path
from datetime import date
import numpy as np
import pandas as pd
import sys
import matplotlib.pyplot as plt  # kept for future quick-look only
from sfma import (Data, Variable, SplineVariable, SplineGetter, SplinePriorGetter,
                  UniformPrior, GaussianPrior, SFMAModel)
from anml.data.component import Component

# ── Paths ──
dir_base = Path('/ihme/homes/idrisov/aim_outputs/Aim2/C_frontier_analysis/')
fp_input_panel = Path(
    '/ihme/homes/idrisov/aim_outputs/Aim2/C_frontier_analysis/'
    '20260517/analysis_per_capita/df_hiv_cascade_panel.csv'
)
today = date.today().strftime("%Y%m%d")
dir_output = dir_base / today / "frontier_batch_mvp"
dir_output.mkdir(parents=True, exist_ok=True)

# ── Column names ──
COL_K          = 'cdc_knowledge_status'
COL_V          = 'cdc_viral_suppress'
COL_INCIDENCE  = 'incidence_counts'
COL_PREV_COUNT = 'hiv_prevalence_counts'
COL_POP        = 'population'
COL_SPEND_ALL  = 'spend_all'
COL_RW         = 'ryan_white_funding_final'

LOGIT_EPS = 1e-4
MIN_N     = 30

# ── State abbreviations ──
STATE_ABBREV = {
    'Alabama':'AL','Alaska':'AK','Arizona':'AZ','Arkansas':'AR','California':'CA',
    'Colorado':'CO','Connecticut':'CT','Delaware':'DE','District of Columbia':'DC',
    'Florida':'FL','Georgia':'GA','Hawaii':'HI','Idaho':'ID','Illinois':'IL',
    'Indiana':'IN','Iowa':'IA','Kansas':'KS','Kentucky':'KY','Louisiana':'LA',
    'Maine':'ME','Maryland':'MD','Massachusetts':'MA','Michigan':'MI','Minnesota':'MN',
    'Mississippi':'MS','Missouri':'MO','Montana':'MT','Nebraska':'NE','Nevada':'NV',
    'New Hampshire':'NH','New Jersey':'NJ','New Mexico':'NM','New York':'NY',
    'North Carolina':'NC','North Dakota':'ND','Ohio':'OH','Oklahoma':'OK','Oregon':'OR',
    'Pennsylvania':'PA','Rhode Island':'RI','South Carolina':'SC','South Dakota':'SD',
    'Tennessee':'TN','Texas':'TX','Utah':'UT','Vermont':'VT','Virginia':'VA',
    'Washington':'WA','West Virginia':'WV','Wisconsin':'WI','Wyoming':'WY'
}

# ── Noise σ per outcome family (Path B convention) ──
SIGMA_BY_OUTCOME = {
    'K x V':     0.15,   # within-state SD ~0.25 → σ ~half of that
    'K only':    0.15,   # similar to K × V
    'incidence': 0.07,   # incidence has much lower residual variance (R² ~0.92)
}


# ═══════════════════════════════════════════════════════════════════════════════
# BUILD ALL VARIABLES UP FRONT
# ═══════════════════════════════════════════════════════════════════════════════
def load_and_build_panel(fp):
    print(f"Loading: {fp}")
    df = pd.read_csv(fp)
    df = df[df['acause'] == 'hiv'].copy()
    print(f"  HIV rows: {len(df)}")

    # Spending — total HIV spend with Ryan White
    df['hiv_spend_total'] = df[COL_SPEND_ALL] + df[COL_RW].fillna(0)
    df['spend_per_case']     = df['hiv_spend_total'] / df[COL_PREV_COUNT]
    df['spend_per_capita']   = df['hiv_spend_total'] / df[COL_POP]
    df['log_spend_per_case']   = np.log(df['spend_per_case'])
    df['log_spend_per_capita'] = np.log(df['spend_per_capita'])

    # K × V outcome (logit-transformed)
    df['K_raw']     = df[COL_K].clip(0, 1)
    df['V_raw']     = df[COL_V].clip(0, 1)
    df['kxv_raw']   = df['K_raw'] * df['V_raw']
    df['kxv_clip']  = df['kxv_raw'].clip(LOGIT_EPS, 1 - LOGIT_EPS)
    df['logit_kxv'] = np.log(df['kxv_clip'] / (1 - df['kxv_clip']))

    # K only (logit-transformed) — full coverage
    df['K_clip']  = df['K_raw'].clip(LOGIT_EPS, 1 - LOGIT_EPS)
    df['logit_K'] = np.log(df['K_clip'] / (1 - df['K_clip']))

    # Incidence (log per 100k)
    df['inc_per_100k']     = df[COL_INCIDENCE] / df[COL_POP] * 1e5
    df['log_inc_per_100k'] = np.log(df['inc_per_100k'].replace(0, np.nan))

    # Prevalence (log per 100k) — contemp and FRESH lag t-1
    df['prev_per_100k']     = df[COL_PREV_COUNT] / df[COL_POP] * 1e5
    df['log_prev_per_100k'] = np.log(df['prev_per_100k'])
    df = df.sort_values(['location_id', 'year_id']).reset_index(drop=True)
    df['log_prev_per_100k_lag1'] = df.groupby('location_id')['log_prev_per_100k'].shift(1)

    # log homelessness
    if 'log_prop_homeless' not in df.columns and 'prop_homeless' in df.columns:
        df['log_prop_homeless'] = np.log(df['prop_homeless'])

    # State abbreviation
    df['state_abbrev'] = df['location_name'].map(STATE_ABBREV).fillna(
        df['location_name'].str[:2].str.upper()
    )

    return df


# ═══════════════════════════════════════════════════════════════════════════════
# SPEC LIST  (10 specs total: 5 families × 2 specs)
# ═══════════════════════════════════════════════════════════════════════════════
SPECS = [
    # ── kxv_per_case ────────────────────────────────────────────────
    dict(family='kxv_per_case', model_name='01_bivariate',
         outcome_var='logit_kxv', outcome_label='logit(K × V)',
         outcome_kind='production',
         predictor_var='log_spend_per_case',
         predictor_label='log(Spend per case)',
         predictor_short='Spend/case',
         covariates=[],
         pos_priors=[],
         neg_priors=[],
         prev_spec='none',
         equation='logit(KxV) ~ log(Spend/case) + year_FE'),

    dict(family='kxv_per_case', model_name='03_homeless',
         outcome_var='logit_kxv', outcome_label='logit(K × V)',
         outcome_kind='production',
         predictor_var='log_spend_per_case',
         predictor_label='log(Spend per case)',
         predictor_short='Spend/case',
         covariates=['race_prop_BLCK','race_prop_HISP','log_prop_homeless'],
         pos_priors=['log_prop_homeless'],
         neg_priors=['race_prop_BLCK','race_prop_HISP'],
         prev_spec='none',
         equation='logit(KxV) ~ log(Spend/case) + race_BLCK + race_HISP + log(% homeless) + year_FE'),

    dict(family='kxv_per_case', model_name='04_lagPrev',
         outcome_var='logit_kxv', outcome_label='logit(K × V)',
         outcome_kind='production',
         predictor_var='log_spend_per_case',
         predictor_label='log(Spend per case)',
         predictor_short='Spend/case',
         covariates=['race_prop_BLCK','race_prop_HISP','log_prop_homeless','log_prev_per_100k_lag1'],
         pos_priors=['log_prop_homeless','log_prev_per_100k_lag1'],
         neg_priors=['race_prop_BLCK','race_prop_HISP'],
         prev_spec='lagged_t-1',
         equation='logit(KxV) ~ log(Spend/case) + race_BLCK + race_HISP + log(% homeless) + log(prev/100k)_t-1 + year_FE'),

    # ── kxv_per_capita ──────────────────────────────────────────────
    dict(family='kxv_per_capita', model_name='01_bivariate',
         outcome_var='logit_kxv', outcome_label='logit(K × V)',
         outcome_kind='production',
         predictor_var='log_spend_per_capita',
         predictor_label='log(Spend per capita)',
         predictor_short='Spend/cap',
         covariates=[],
         pos_priors=[],
         neg_priors=[],
         prev_spec='none',
         equation='logit(KxV) ~ log(Spend/cap) + year_FE'),

    dict(family='kxv_per_capita', model_name='03_homeless',
         outcome_var='logit_kxv', outcome_label='logit(K × V)',
         outcome_kind='production',
         predictor_var='log_spend_per_capita',
         predictor_label='log(Spend per capita)',
         predictor_short='Spend/cap',
         covariates=['race_prop_BLCK','race_prop_HISP','log_prop_homeless'],
         pos_priors=['log_prop_homeless'],
         neg_priors=['race_prop_BLCK','race_prop_HISP'],
         prev_spec='none',
         equation='logit(KxV) ~ log(Spend/cap) + race_BLCK + race_HISP + log(% homeless) + year_FE'),

    dict(family='kxv_per_capita', model_name='04_lagPrev',
         outcome_var='logit_kxv', outcome_label='logit(K × V)',
         outcome_kind='production',
         predictor_var='log_spend_per_capita',
         predictor_label='log(Spend per capita)',
         predictor_short='Spend/cap',
         covariates=['race_prop_BLCK','race_prop_HISP','log_prop_homeless','log_prev_per_100k_lag1'],
         pos_priors=['log_prop_homeless','log_prev_per_100k_lag1'],
         neg_priors=['race_prop_BLCK','race_prop_HISP'],
         prev_spec='lagged_t-1',
         equation='logit(KxV) ~ log(Spend/cap) + race_BLCK + race_HISP + log(% homeless) + log(prev/100k)_t-1 + year_FE'),

    # ── k_only_per_case ─────────────────────────────────────────────
    dict(family='k_only_per_case', model_name='01_bivariate',
         outcome_var='logit_K', outcome_label='logit(K)',
         outcome_kind='production',
         predictor_var='log_spend_per_case',
         predictor_label='log(Spend per case)',
         predictor_short='Spend/case',
         covariates=[],
         pos_priors=[],
         neg_priors=[],
         prev_spec='none',
         equation='logit(K) ~ log(Spend/case) + year_FE'),

    dict(family='k_only_per_case', model_name='03_homeless',
         outcome_var='logit_K', outcome_label='logit(K)',
         outcome_kind='production',
         predictor_var='log_spend_per_case',
         predictor_label='log(Spend per case)',
         predictor_short='Spend/case',
         covariates=['race_prop_BLCK','race_prop_HISP','log_prop_homeless'],
         pos_priors=['log_prop_homeless'],
         neg_priors=['race_prop_BLCK','race_prop_HISP'],
         prev_spec='none',
         equation='logit(K) ~ log(Spend/case) + race_BLCK + race_HISP + log(% homeless) + year_FE'),

    dict(family='k_only_per_case', model_name='04_lagPrev',
         outcome_var='logit_K', outcome_label='logit(K)',
         outcome_kind='production',
         predictor_var='log_spend_per_case',
         predictor_label='log(Spend per case)',
         predictor_short='Spend/case',
         covariates=['race_prop_BLCK','race_prop_HISP','log_prop_homeless','log_prev_per_100k_lag1'],
         pos_priors=['log_prop_homeless','log_prev_per_100k_lag1'],
         neg_priors=['race_prop_BLCK','race_prop_HISP'],
         prev_spec='lagged_t-1',
         equation='logit(K) ~ log(Spend/case) + race_BLCK + race_HISP + log(% homeless) + log(prev/100k)_t-1 + year_FE'),

    # ── k_only_per_capita ───────────────────────────────────────────
    dict(family='k_only_per_capita', model_name='01_bivariate',
         outcome_var='logit_K', outcome_label='logit(K)',
         outcome_kind='production',
         predictor_var='log_spend_per_capita',
         predictor_label='log(Spend per capita)',
         predictor_short='Spend/cap',
         covariates=[],
         pos_priors=[],
         neg_priors=[],
         prev_spec='none',
         equation='logit(K) ~ log(Spend/cap) + year_FE'),

    dict(family='k_only_per_capita', model_name='03_homeless',
         outcome_var='logit_K', outcome_label='logit(K)',
         outcome_kind='production',
         predictor_var='log_spend_per_capita',
         predictor_label='log(Spend per capita)',
         predictor_short='Spend/cap',
         covariates=['race_prop_BLCK','race_prop_HISP','log_prop_homeless'],
         pos_priors=['log_prop_homeless'],
         neg_priors=['race_prop_BLCK','race_prop_HISP'],
         prev_spec='none',
         equation='logit(K) ~ log(Spend/cap) + race_BLCK + race_HISP + log(% homeless) + year_FE'),

    dict(family='k_only_per_capita', model_name='04_lagPrev',
         outcome_var='logit_K', outcome_label='logit(K)',
         outcome_kind='production',
         predictor_var='log_spend_per_capita',
         predictor_label='log(Spend per capita)',
         predictor_short='Spend/cap',
         covariates=['race_prop_BLCK','race_prop_HISP','log_prop_homeless','log_prev_per_100k_lag1'],
         pos_priors=['log_prop_homeless','log_prev_per_100k_lag1'],
         neg_priors=['race_prop_BLCK','race_prop_HISP'],
         prev_spec='lagged_t-1',
         equation='logit(K) ~ log(Spend/cap) + race_BLCK + race_HISP + log(% homeless) + log(prev/100k)_t-1 + year_FE'),

    # ── incidence_per_capita (cost outcome — negate before fit) ─────
    # In NEGATED space, priors flip vs original-space direction.
    # log_prev_per_100k: original +0.89 (positive on incidence) → NEG in negated space
    # race_BLCK, race_HISP: positive on incidence → NEG in negated space
    # log_prop_homeless: ~0 (free)
    dict(family='incidence_per_capita', model_name='01_bivariate',
         outcome_var='log_inc_per_100k', outcome_label='log(incidence/100k)',
         outcome_kind='cost',
         predictor_var='log_spend_per_capita',
         predictor_label='log(Spend per capita)',
         predictor_short='Spend/cap',
         covariates=[],
         pos_priors=[],
         neg_priors=[],
         prev_spec='none',
         equation='log(incidence/100k) ~ log(Spend/cap) + year_FE'),

    dict(family='incidence_per_capita', model_name='03_homeless',
         outcome_var='log_inc_per_100k', outcome_label='log(incidence/100k)',
         outcome_kind='cost',
         predictor_var='log_spend_per_capita',
         predictor_label='log(Spend per capita)',
         predictor_short='Spend/cap',
         covariates=['race_prop_BLCK','race_prop_HISP','log_prop_homeless'],
         pos_priors=[],
         neg_priors=['race_prop_BLCK','race_prop_HISP'],
         prev_spec='none',
         equation='log(incidence/100k) ~ log(Spend/cap) + race_BLCK + race_HISP + log(% homeless) + year_FE'),

    dict(family='incidence_per_capita', model_name='04_contempPrev',
         outcome_var='log_inc_per_100k', outcome_label='log(incidence/100k)',
         outcome_kind='cost',
         predictor_var='log_spend_per_capita',
         predictor_label='log(Spend per capita)',
         predictor_short='Spend/cap',
         covariates=['race_prop_BLCK','race_prop_HISP','log_prop_homeless','log_prev_per_100k'],
         pos_priors=[],
         neg_priors=['race_prop_BLCK','race_prop_HISP','log_prev_per_100k'],
         prev_spec='contemp',
         equation='log(incidence/100k) ~ log(Spend/cap) + race_BLCK + race_HISP + log(% homeless) + log(prev/100k)_t + year_FE'),
]


# ═══════════════════════════════════════════════════════════════════════════════
# FIT ONE SPEC
# ═══════════════════════════════════════════════════════════════════════════════
def fit_one_frontier(df_full, spec):
    """Fit SFMA frontier for one spec. Returns (long_rows_df, metadata_dict)."""
    spec_id = f"hiv__{spec['family']}__{spec['model_name']}"
    print(f"\n{'='*78}\nFitting: {spec_id}\n{'='*78}")

    df = df_full.copy()
    outcome_col      = spec['outcome_var']
    spend_col        = spec['predictor_var']
    cov_cols_static  = list(spec['covariates'])    # don't include year FE here
    outcome_kind     = spec['outcome_kind']
    sigma_label      = 'K x V' if 'logit_kxv' == outcome_col else \
                       ('K only' if 'logit_K' == outcome_col else 'incidence')
    sigma_se         = SIGMA_BY_OUTCOME[sigma_label]

    # If cost outcome, prepare the NEGATED outcome column
    if outcome_kind == 'cost':
        df['_outcome_neg'] = -1.0 * df[outcome_col]
        sfma_outcome_col   = '_outcome_neg'
    else:
        sfma_outcome_col   = outcome_col

    # ── Drop NaN on key vars FIRST (before year dummies — v5 fix) ──
    key_vars = [outcome_col, spend_col] + cov_cols_static
    n_before = len(df)
    df = df.dropna(subset=key_vars).copy()
    n_after = len(df)
    n_states = df['location_id'].nunique()
    print(f"  N state-years: {n_after} (dropped {n_before - n_after}), states: {n_states}")

    if n_after < MIN_N:
        print(f"  SKIPPED — fewer than {MIN_N} obs available.")
        return None, None

    # Preserve original for plotting
    df['log_spend_orig'] = df[spend_col].copy()
    df['y_orig']         = df[outcome_col].copy()

    # ── Year dummies (AFTER dropna; reference = first in-sample year) ──
    pos_prior_vars = list(spec['pos_priors'])
    neg_prior_vars = list(spec['neg_priors'])
    year_dummy_cols = []
    years = sorted(df['year_id'].unique())
    ref_year = years[0]
    for yr in years[1:]:
        col = f'year_{yr}'
        df[col] = (df['year_id'] == yr).astype(float)
        year_dummy_cols.append(col)
        pos_prior_vars.append(col)   # year FE: POS in K outcomes, ALSO POS in negated incidence
    all_cov_cols  = cov_cols_static + year_dummy_cols
    all_model_vars = [spend_col] + all_cov_cols
    print(f"  Year FE reference: {ref_year} ({len(year_dummy_cols)} dummies)")

    # ── Z-score continuous (not year dummies) ──
    for var in [v for v in all_model_vars if v not in year_dummy_cols]:
        mu, sd = df[var].mean(), df[var].std()
        if sd > 0:
            df[var] = (df[var] - mu) / sd
    if outcome_kind == 'cost':
        # Re-create the negated outcome from the (now z-scored) outcome
        # — NO, we want NEGATED-OUTCOME not z-scored outcome.
        # SFMA fits the raw negated value; we should NOT z-score the outcome column.
        # Reset the negated column from the un-z-scored y_orig:
        df[sfma_outcome_col] = -1.0 * df['y_orig']

    # ── Hardcoded SE ──
    df['standard_error'] = sigma_se
    print(f"  σ (hardcoded): {sigma_se:.3f}")

    # ── Sort by spending for clean spline fitting ──
    df.sort_values(spend_col, inplace=True)
    df.reset_index(drop=True, inplace=True)

    # ── Build SFMA model ──
    def get_model(df, covs_to_use):
        data = Data(obs=sfma_outcome_col, obs_se='standard_error')
        variables = [Variable(Component('intercept', default_value=1.0))]
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
                    degree=3, knots_type='rel_domain',
                    include_first_basis=False, l_linear=True, r_linear=True,
                ),
                priors=spline_priors,
            )
        )
        model = SFMAModel(data, variables, include_re=False)
        model.attach(df)
        return model

    # ── Initial fit to select covariates ──
    model = get_model(df, all_cov_cols)
    model.eta = 0.1
    model.beta.fill(1.0)
    try:
        model.fit(verbose=False, max_iter=10, tol=1e-3,
                  beta_options={"max_iter": 1000, "verbose": False, "xtol": 0.0, "gtol": 1e-6})
    except Exception as e:
        print(f"  WARNING: initial fit failed: {e}")
        return None, None

    beta = model.get_beta_dict()
    selected_covs = [name for name, value in beta.items()
                     if value.size == 1 and name != 'intercept'
                     and np.abs(value[0]) > 1e-5]
    print(f"  Selected covariates: {len(selected_covs)} of {len(all_cov_cols)}")

    # ── Final fit with trimming ──
    model = get_model(df, selected_covs)
    model.eta = 0.1
    model.beta.fill(1.0)
    try:
        model.fit(outlier_pct=0.05, trim_max_iter=5,
                  verbose=False, max_iter=5, tol=1e-3,
                  beta_options={"max_iter": 1000, "verbose": False, "xtol": 0.0, "gtol": 1e-6})
    except Exception as e:
        print(f"  WARNING: final fit failed: {e}")
        return None, None

    eta_final = float(model.eta)
    print(f"  Fitted! eta = {eta_final:.6f}")

    # ── Extract predictions (covariate-adjusted) ──
    Y = df[sfma_outcome_col].values
    Y_hat = model.predict(df)

    df_null = df.copy()
    covs_to_zero = [c for c in selected_covs if c != spend_col]
    if covs_to_zero:
        df_null[covs_to_zero] = 0.0
    Y_hat_adj = model.predict(df_null)
    cov_hat = Y_hat - Y_hat_adj
    Y_adj   = Y - cov_hat
    ineff   = model.get_inefficiency()

    # For cost outcomes, un-negate Y_adj for plotting (back to log incidence)
    if outcome_kind == 'cost':
        y_obs_adj_plot   = -1.0 * Y_adj
        y_frontier_plot  = -1.0 * Y_hat_adj
    else:
        y_obs_adj_plot   = Y_adj
        y_frontier_plot  = Y_hat_adj

    # ── Long-format output ──
    long_rows = pd.DataFrame({
        'spec_id':           spec_id,
        'family':            spec['family'],
        'model_name':        spec['model_name'],
        'outcome_var':       outcome_col,
        'outcome_label':     spec['outcome_label'],
        'predictor_var':     spend_col,
        'predictor_label':   spec['predictor_label'],
        'outcome_kind':      outcome_kind,
        'prev_spec':         spec['prev_spec'],
        'location_id':       df['location_id'].values,
        'location_name':     df['location_name'].values,
        'state_abbrev':      df['state_abbrev'].values,
        'year_id':           df['year_id'].values,
        'log_spend_orig':    df['log_spend_orig'].values,
        'y_obs_adj':         y_obs_adj_plot,
        'y_frontier':        y_frontier_plot,
        'ineff_raw':         ineff,
        'kxv_raw':           df['kxv_raw'].values if 'kxv_raw' in df.columns else np.nan,
        'k_raw':             df['K_raw'].values if 'K_raw' in df.columns else np.nan,
        'inc_per_100k':      df['inc_per_100k'].values if 'inc_per_100k' in df.columns else np.nan,
    })

    # ── Metadata ──
    # Determine plateau by computing predicted Y_frontier at max log_spend
    sorted_idx = np.argsort(df['log_spend_orig'].values)
    log_spend_sorted = df['log_spend_orig'].values[sorted_idx]
    y_front_sorted   = y_frontier_plot[sorted_idx]
    plateau_logspend = log_spend_sorted[-1]
    plateau_y        = y_front_sorted[-1]

    metadata = {
        'spec_id':             spec_id,
        'family':              spec['family'],
        'model_name':          spec['model_name'],
        'outcome_label':       spec['outcome_label'],
        'predictor_label':     spec['predictor_label'],
        'outcome_kind':        outcome_kind,
        'prev_spec':           spec['prev_spec'],
        'equation':            spec['equation'],
        'n_observations':      n_after,
        'n_states':            n_states,
        'eta':                 round(eta_final, 6),
        'sigma_se':            sigma_se,
        'plateau_log_spend':   round(float(plateau_logspend), 3),
        'plateau_y':           round(float(plateau_y), 3),
        'plateau_spend_usd':   round(float(np.exp(plateau_logspend)), 0),
        'mean_ineff_raw':      round(float(np.mean(ineff)), 4),
        'median_ineff_raw':    round(float(np.median(ineff)), 4),
        'max_ineff_raw':       round(float(np.max(ineff)), 4),
        'n_below_frontier':    int(np.sum(
            (y_obs_adj_plot - y_frontier_plot) * (1 if outcome_kind == 'production' else -1) < 0
        )),
        'n_above_frontier':    int(np.sum(
            (y_obs_adj_plot - y_frontier_plot) * (1 if outcome_kind == 'production' else -1) > 0
        )),
    }
    return long_rows, metadata


# ═══════════════════════════════════════════════════════════════════════════════
# MAIN
# ═══════════════════════════════════════════════════════════════════════════════
if __name__ == '__main__':
    print("\n" + "=" * 78)
    print("FRONTIER BATCH FIT — MVP (Option C: 10 specs, 5 families × 2 specs)")
    print("=" * 78)
    print(f"Output: {dir_output}\n")

    df_full = load_and_build_panel(fp_input_panel)

    long_dfs = []
    metadata_rows = []
    for spec in SPECS:
        long_rows, meta = fit_one_frontier(df_full, spec)
        if long_rows is not None:
            long_dfs.append(long_rows)
            metadata_rows.append(meta)

    if long_dfs:
        long_all = pd.concat(long_dfs, ignore_index=True)
        meta_df  = pd.DataFrame(metadata_rows)

        long_all.to_csv(dir_output / 'frontiers_long.csv', index=False)
        meta_df.to_csv (dir_output / 'frontiers_metadata.csv', index=False)

        print("\n" + "=" * 78)
        print(f"WROTE {len(long_dfs)} of {len(SPECS)} specs successfully")
        print("=" * 78)
        print(f"  - frontiers_long.csv      ({len(long_all)} rows)")
        print(f"  - frontiers_metadata.csv  ({len(meta_df)} rows)")
        print("\nMetadata summary:")
        print(meta_df[['spec_id','n_observations','n_states','eta',
                       'plateau_spend_usd','plateau_y',
                       'n_below_frontier','n_above_frontier']].to_string(index=False))
    else:
        print("\nNO frontiers fit successfully.")
        sys.exit(1)