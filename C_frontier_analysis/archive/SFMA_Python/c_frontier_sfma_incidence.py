"""
Filename: C_frontier_analysis_sfma_hiv_incidence_v1.py
Description: SFMA frontier analysis for HIV (Aim 2) — INCIDENCE outcome.
             Secondary analysis paired with Joe's "Prevention equation" (R2b):

                 log(incidence/100k)  ~  log(spend per capita)
                                        + log(prev/100k)        (contemp)
                                        + race_BLCK, race_HISP
                                        + log_prop_homeless
                                        + year FE

             Mirrors R2b regression exactly. Per-capita (not per-case)
             spending. Contemporaneous (not lagged) prevalence — matching
             the regression to keep the two analyses directly comparable.

             OUTCOME DIRECTION (cost):
                 Incidence is a COST outcome (lower = better, fewer new
                 infections). We NEGATE log(incidence) before fitting so
                 SFMA's standard production-frontier framing fits the
                 UPPER envelope of negated-incidence, which is the
                 LOWER envelope of incidence in original space.

             PRIORS (anchored to R2b coefficient signs):
                 R2b regression coefficients (on log incidence):
                   log(spend/cap)   = -0.286 **  → spending reduces incidence
                   log(prev/100k)   = +0.891 *** → high prev → more incidence
                   log(% homeless)  = +0.022      ns → free
                   race_BLCK        = +1.291 *    → positive on incidence
                   race_HISP        = +0.488 †    → positive on incidence
                   year FE          jointly sig.  → declining over time

                 In NEGATED space (what SFMA sees):
                   spending spline  → monotone↑, concave
                   prev             → NEGATIVE
                   homeless         → free
                   race BLCK/HISP   → NEGATIVE
                   year FE          → POSITIVE (incidence declines → negated↑)

             NOISE SE:
                 Hardcoded SE_FALLBACK = 0.15 (matches v5 Path B convention).
                 Sensitivity sweep across {0.10, 0.20, 0.25} recommended for
                 final paper supplement.

How to run: python C_frontier_analysis_sfma_hiv_incidence_v1.py
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
from sfma import (Data, Variable, SplineVariable, SplineGetter, SplinePriorGetter,
                  UniformPrior, GaussianPrior, SFMAModel)
from anml.data.component import Component

# ── Paths ──
dir_base = Path('/ihme/homes/idrisov/aim_outputs/Aim2/C_frontier_analysis/')

fp_input_panel = Path(
    '/ihme/homes/idrisov/aim_outputs/Aim2/C_frontier_analysis/'
    '20260517/analysis_per_capita/df_hiv_cascade_panel.csv'
)

today_yyyymmdd = date.today().strftime("%Y%m%d")
dir_output = dir_base / today_yyyymmdd / "incidence_frontier_v1"
dir_output.mkdir(parents=True, exist_ok=True)

# ── Common column names ──
spend_col      = 'log_spend_pc'             # log(Spend per Capita), built fresh
outcome_col    = 'log_incidence_neg'        # NEGATED log(incidence/100k) for fit
variance_col   = 'variance'                 # may not exist; not used (hardcoded SE)
acause         = 'hiv'

# Raw column names from the panel
COL_INCIDENCE  = 'incidence_counts'
COL_PREV_COUNT = 'hiv_prevalence_counts'
COL_POP        = 'population'
COL_SPEND_ALL  = 'spend_all'
COL_RW         = 'ryan_white_funding_final'

# Name of the contemporaneous prev covariate
PREV_COL = 'log_prev_per_100k'

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

# ── Outcome configuration ──
OUTCOME_LABEL = 'HIV Incidence per 100,000 population'

# ── Covariates and PRIORS (all in NEGATED log-incidence space) ──
COV_COLS              = [PREV_COL, 'race_prop_BLCK', 'race_prop_HISP', 'log_prop_homeless']
POS_PRIOR_VARS_BASE   = []           # year FE will be added; homeless stays free
NEG_PRIOR_VARS        = [PREV_COL, 'race_prop_BLCK', 'race_prop_HISP']  # all positive on incidence → negative in negated

MIN_N     = 50

# ── Path B style hardcoded SE ──
# Incidence has much lower residual variance than K × V because the R2b
# regression achieves R² ≈ 0.92 (vs ~0.50 for K × V). Residual SD ≈ 0.17,
# within-state year-to-year SD probably ≈ 0.05–0.10. SE_FALLBACK is set
# to roughly half the residual SD so SFMA has noise budget without
# absorbing all residual variation (which would collapse η to 0 and
# produce the OLS conditional-mean line instead of a lower envelope).
SE_FALLBACK = 0.07


# ═══════════════════════════════════════════════════════════════════════════════
# SANITY CHECKS
# ═══════════════════════════════════════════════════════════════════════════════
def run_sanity_checks(fp):
    print("\n" + "█" * 75)
    print("SANITY CHECKS — HIV Incidence Frontier (R2b spec, prev on RHS)")
    print("█" * 75)

    print(f"\n[1/6] Panel file path:")
    print(f"      {fp}")
    if not fp.exists():
        print(f"      ❌ FILE NOT FOUND. Aborting.")
        sys.exit(1)
    print(f"      ✓ File exists ({fp.stat().st_size / 1e6:.1f} MB)")

    print(f"\n[2/6] Loading panel...")
    df = pd.read_csv(fp)
    print(f"      ✓ Loaded: {len(df):,} rows × {df.shape[1]} columns")
    if 'acause' in df.columns:
        df = df[df['acause'] == 'hiv'].copy()
        print(f"      → after filter to HIV: {len(df):,} rows")

    print(f"\n[3/6] Required columns:")
    required = {
        'Incidence counts':     COL_INCIDENCE,
        'HIV prevalence counts':COL_PREV_COUNT,
        'Population':           COL_POP,
        'Spend (all sources)':  COL_SPEND_ALL,
        'Race: prop Black':     'race_prop_BLCK',
        'Race: prop Hispanic':  'race_prop_HISP',
        'log(% homeless)':      'log_prop_homeless',
        'location_id':          'location_id',
        'location_name':        'location_name',
        'year_id':              'year_id',
    }
    optional = {
        'Ryan White funding':   COL_RW,
    }
    missing = []
    for label, col in required.items():
        if col in df.columns:
            n_nonnull = df[col].notna().sum()
            print(f"      ✓ {label:<24s} '{col}'  ({n_nonnull:,} non-null)")
        else:
            print(f"      ❌ {label:<24s} '{col}'  MISSING")
            missing.append(col)
    for label, col in optional.items():
        if col in df.columns:
            n_nonnull = df[col].notna().sum()
            print(f"      ✓ {label:<24s} '{col}'  ({n_nonnull:,} non-null)")
        else:
            print(f"      ⚠ {label:<24s} '{col}'  MISSING (RW will be treated as 0)")
    if missing:
        print(f"\n      ❌ Required columns missing: {missing}. Aborting.")
        sys.exit(1)

    print(f"\n[4/6] Outcome construction (log incidence per 100k):")
    inc = df[COL_INCIDENCE] / df[COL_POP] * 1e5
    log_inc = np.log(inc.replace(0, np.nan))
    print(f"      Incidence/100k range: [{inc.min():.1f}, {inc.max():.1f}]   "
          f"mean = {inc.mean():.1f}")
    print(f"      log(inc/100k) range: [{log_inc.min():.3f}, {log_inc.max():.3f}]   "
          f"mean = {log_inc.mean():.3f}  SD = {log_inc.std():.3f}")

    print(f"\n[5/6] Predictor construction (log spend per capita):")
    rw = df[COL_RW].fillna(0) if COL_RW in df.columns else 0
    total_spend = df[COL_SPEND_ALL] + rw
    spend_pc = total_spend / df[COL_POP]
    log_spend = np.log(spend_pc.replace(0, np.nan))
    print(f"      Spend/capita range:   [${spend_pc.min():.2f}, ${spend_pc.max():.2f}]   "
          f"mean = ${spend_pc.mean():.2f}")
    print(f"      log(Spend/cap) range: [{log_spend.min():.3f}, {log_spend.max():.3f}]   "
          f"SD = {log_spend.std():.3f}")

    print(f"\n[6/6] Year coverage:")
    years = sorted(df['year_id'].unique())
    print(f"      Years in panel: {years}")
    print(f"      States: {df['location_id'].nunique()} (incidence has full coverage)")

    print("\n" + "█" * 75)
    print(f"SANITY CHECKS PASSED — proceeding to SFMA fit")
    print("█" * 75 + "\n")

    return df


# ═══════════════════════════════════════════════════════════════════════════════
# MAIN SFMA FUNCTION
# ═══════════════════════════════════════════════════════════════════════════════
def runSFA_HIV_Incidence(df_full):
    """Run SFMA frontier on log(incidence/100k) with per-capita spending.

    Returns (out, state_summary, covs_df) or (None, None, None) if skipped.
    """
    cov_cols       = COV_COLS.copy()
    pos_prior_vars = POS_PRIOR_VARS_BASE.copy()
    neg_prior_vars = NEG_PRIOR_VARS.copy()

    print(f"\n{'=' * 75}")
    print(f"SFMA HIV INCIDENCE FRONTIER  (R2b spec — prevention equation)")
    print(f"  Outcome:    log({OUTCOME_LABEL})  [COST → negated for fit]")
    print(f"  Predictor:  log(Spend per Capita), with Ryan White")
    print(f"  Frontier interpretation: LOWER envelope of incidence vs spending")
    print(f"  Inefficiency: how far ABOVE the frontier each state-year is")
    print(f"  Covariates: contemporaneous log(prev/100k), race, homelessness, year FE")
    print(f"{'=' * 75}")

    df = df_full.copy()

    # ── Build variables from raw counts ──
    df['_incidence_per_100k'] = df[COL_INCIDENCE] / df[COL_POP] * 1e5
    df['log_incidence']       = np.log(df['_incidence_per_100k'].replace(0, np.nan))

    rw = df[COL_RW].fillna(0) if COL_RW in df.columns else 0
    df['_hiv_spend_total'] = df[COL_SPEND_ALL] + rw
    df['_spend_pc']        = df['_hiv_spend_total'] / df[COL_POP]
    df[spend_col]          = np.log(df['_spend_pc'].replace(0, np.nan))

    df['_prev_per_100k'] = df[COL_PREV_COUNT] / df[COL_POP] * 1e5
    df[PREV_COL]         = np.log(df['_prev_per_100k'].replace(0, np.nan))

    # ── Year dummies (build BEFORE dropna here because no observations are
    #    dropped for lag — incidence sample is full panel) ──
    year_dummy_cols = []
    years = sorted(df['year_id'].unique())
    ref_year = years[0]
    print(f"  Reference year: {ref_year}")
    for yr in years[1:]:
        col = f'year_{yr}'
        df[col] = (df['year_id'] == yr).astype(float)
        year_dummy_cols.append(col)
        pos_prior_vars.append(col)     # year FE → POS in negated (incidence declines)
    cov_cols = cov_cols + year_dummy_cols

    all_model_vars = [spend_col] + cov_cols

    # ── Drop NaN on key vars ──
    key_vars = ['log_incidence', spend_col] + COV_COLS
    n_before = len(df)
    df = df.dropna(subset=key_vars).copy()
    print(f"  N state-years (after NaN drop): {len(df)} (dropped {n_before - len(df)})")
    print(f"  Effective state clusters: {df['location_id'].nunique()}")
    print(f"  log(incidence/100k): range [{df['log_incidence'].min():.3f}, "
          f"{df['log_incidence'].max():.3f}], mean = {df['log_incidence'].mean():.3f}")
    print(f"  log(spend/cap):      range [{df[spend_col].min():.3f}, "
          f"{df[spend_col].max():.3f}], mean = {df[spend_col].mean():.3f}")

    if len(df) < MIN_N:
        print(f"  SKIPPED — fewer than {MIN_N} state-years available.")
        return None, None, None

    # ── Preserve original spending and outcome for plotting ──
    df['log_spend_orig'] = df[spend_col].copy()
    df['log_inc_orig']   = df['log_incidence'].copy()

    # ── Z-score continuous variables (year dummies stay 0/1) ──
    vars_to_zscore = [v for v in all_model_vars if v not in year_dummy_cols]
    for var in vars_to_zscore:
        mu = df[var].mean(); sd = df[var].std()
        if sd > 0:
            df[var] = (df[var] - mu) / sd
        else:
            print(f"  WARNING: {var} has zero SD, skipping z-score")

    # ────────────────────────────────────────────────────────────────────
    # COST OUTCOME: NEGATE log(incidence) before fit.
    # SFMA fits the upper envelope of (-log_incidence) which is the
    # LOWER envelope of log_incidence in original space.
    # Spline priors (1st deriv ≥ 0, 2nd deriv ≤ 0) in negated space mean:
    #   - frontier decreases with spending in original space (more $ → less incidence)
    #   - convex in original space (diminishing returns to spending)
    # ────────────────────────────────────────────────────────────────────
    df[outcome_col] = -1.0 * df['log_incidence']   # negated outcome for SFMA

    # ── Hardcoded SE (Path B convention) ──
    outcome_sd_neg = float(df[outcome_col].std())
    within_state_sd = float(df.groupby('location_id')[outcome_col].std().mean())
    se_val = float(SE_FALLBACK)
    print(f"  Standard error (HARDCODED): {se_val:.4f}")
    print(f"    For reference: negated-outcome SD = {outcome_sd_neg:.4f}, "
          f"within-state SD = {within_state_sd:.4f}")
    df['standard_error'] = se_val

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

        # Spline priors in NEGATED log-incidence space:
        #   1st deriv ≥ 0  → frontier rises (in negated) → falls (in original) with spending
        #   2nd deriv ≤ 0  → concave (in negated) → convex (in original): diminishing returns
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

    covs = cov_cols.copy()
    model = get_model(df, covs)

    print(f"\n  Testing covariate directions (full set)...")
    model.eta = 0.1
    model.beta.fill(1.0)
    try:
        model.fit(
            verbose=True, max_iter=10, tol=1e-3,
            beta_options={"max_iter": 1000, "verbose": False, "xtol": 0.0, "gtol": 1e-6}
        )
    except Exception as e:
        print(f"  WARNING: initial fit failed: {e}. Skipping.")
        return None, None, None

    beta = model.get_beta_dict()
    selected_covs = [name for name, value in beta.items()
                     if value.size == 1 and name != "intercept"
                     and np.abs(value[0]) > 1e-5]
    print(f"  Selected covariates: {selected_covs}")

    model = get_model(df, selected_covs)

    print(f"\n  Final fit with 5% trimming...")
    model.eta = 0.1
    model.beta.fill(1.0)
    try:
        model.fit(
            outlier_pct=0.05, trim_max_iter=5,
            verbose=True, max_iter=5, tol=1e-3,
            beta_options={"max_iter": 1000, "verbose": False, "xtol": 0.0, "gtol": 1e-6}
        )
    except Exception as e:
        print(f"  WARNING: final fit failed: {e}. Skipping.")
        return None, None, None

    print(f"\n  Fitted! eta = {model.eta:.6f}")
    betas = model.get_beta_dict()
    print(f"  Betas (NEGATED space — flip sign for original log-incidence direction): {betas}")

    # ══════════════════════════════════════════════════════════════════════
    # EXTRACT RESULTS
    # ══════════════════════════════════════════════════════════════════════
    Y_neg     = df[outcome_col].values
    Y_hat_neg = model.predict(df)

    df_null_covs = df.copy()
    covs_to_zero = [c for c in selected_covs if c != spend_col]
    if covs_to_zero:
        df_null_covs[covs_to_zero] = 0.0
    Y_hat_adj_neg = model.predict(df_null_covs)

    cov_hat   = Y_hat_neg - Y_hat_adj_neg
    Y_adj_neg = Y_neg - cov_hat

    ineff = model.get_inefficiency()

    # Un-negate for plotting (back to original log-incidence direction)
    Y_adj_log     = -1.0 * Y_adj_neg
    Y_adj_hat_log = -1.0 * Y_hat_adj_neg

    id_cols = ['location_name', 'location_id', 'year_id', 'cause_id', 'acause', 'cause_name']
    id_cols_present = [c for c in id_cols if c in df.columns]

    out = df[id_cols_present + [spend_col, 'log_spend_orig', 'log_inc_orig']].copy()
    out['y_neg']         = Y_neg
    out['ineff']         = ineff
    out['y_adj']         = Y_adj_neg
    out['y_adj_hat']     = Y_hat_adj_neg
    out['y_adj_log']     = Y_adj_log        # un-negated for plotting
    out['y_adj_hat_log'] = Y_adj_hat_log    # un-negated for plotting
    out['outcome_key']   = 'incidence'
    out['incidence_per_100k'] = df['_incidence_per_100k'].values
    out.sort_values(spend_col, inplace=True)

    out['ineff_raw'] = out['ineff']
    if out['ineff'].max() > out['ineff'].min():
        out['ineff'] = (out['ineff'] - out['ineff'].min()) / (out['ineff'].max() - out['ineff'].min())
    else:
        out['ineff'] = 0.0

    # ══════════════════════════════════════════════════════════════════════
    # STATE-LEVEL SUMMARY
    # ══════════════════════════════════════════════════════════════════════
    state_mean = (out.groupby(['location_name', 'location_id'])
                     .agg(ineff_raw_mean=('ineff_raw', 'mean'),
                          ineff_mean    =('ineff',     'mean'))
                     .reset_index())

    early = (out[out['year_id'].between(2010, 2014)]
             .groupby(['location_name', 'location_id'])['ineff_raw']
             .mean().rename('ineff_raw_early'))
    late = (out[out['year_id'].between(2015, 2019)]
            .groupby(['location_name', 'location_id'])['ineff_raw']
            .mean().rename('ineff_raw_late'))
    early_sc = (out[out['year_id'].between(2010, 2014)]
                .groupby(['location_name', 'location_id'])['ineff']
                .mean().rename('ineff_early'))
    late_sc = (out[out['year_id'].between(2015, 2019)]
               .groupby(['location_name', 'location_id'])['ineff']
               .mean().rename('ineff_late'))

    spend_mean = (out.groupby(['location_name', 'location_id'])['log_spend_orig']
                     .mean().rename('log_spend_mean'))
    y_adj_mean = (out.groupby(['location_name', 'location_id'])['y_adj_log']
                     .mean().rename('y_adj_log_mean'))
    inc_mean = (out.groupby(['location_name', 'location_id'])['incidence_per_100k']
                   .mean().rename('incidence_mean'))

    state_summary = (state_mean
                     .merge(early,      on=['location_name', 'location_id'])
                     .merge(late,       on=['location_name', 'location_id'])
                     .merge(early_sc,   on=['location_name', 'location_id'])
                     .merge(late_sc,    on=['location_name', 'location_id'])
                     .merge(spend_mean, on=['location_name', 'location_id'])
                     .merge(y_adj_mean, on=['location_name', 'location_id'])
                     .merge(inc_mean,   on=['location_name', 'location_id']))

    state_summary['ineff_raw_change'] = state_summary['ineff_raw_late'] - state_summary['ineff_raw_early']
    state_summary['ineff_change']     = state_summary['ineff_late']     - state_summary['ineff_early']
    state_summary['state_abbrev']     = state_summary['location_name'].map(STATE_ABBREV).fillna(
        state_summary['location_name'].str[:2].str.upper()
    )
    state_summary['outcome_key'] = 'incidence'

    col_order = [
        'location_name', 'location_id', 'state_abbrev', 'outcome_key',
        'ineff_raw_mean', 'ineff_raw_early', 'ineff_raw_late', 'ineff_raw_change',
        'ineff_mean',     'ineff_early',     'ineff_late',     'ineff_change',
        'log_spend_mean', 'y_adj_log_mean',  'incidence_mean',
    ]
    state_summary = state_summary[col_order]
    state_summary.sort_values('ineff_mean', inplace=True)

    us_row = pd.DataFrame([{
        'location_name': 'United States',
        'location_id': -1,
        'state_abbrev': 'US',
        'outcome_key': 'incidence',
        **{c: state_summary[c].mean() for c in col_order
           if c not in ['location_name', 'location_id', 'state_abbrev', 'outcome_key']},
    }])
    state_summary = pd.concat([state_summary, us_row], ignore_index=True)

    covs_only = [c for c in selected_covs if c != spend_col]
    if len(covs_only) > 0:
        covs_df = pd.DataFrame({'selected_covs': covs_only,
                                'acause': acause,
                                'outcome_key': 'incidence'})
    else:
        covs_df = pd.DataFrame({'selected_covs': ['no covs selected'],
                                'acause': acause,
                                'outcome_key': 'incidence'})
    for name, value in betas.items():
        if value.size == 1 and name != spend_col:
            update_idx = covs_df['selected_covs'] == name
            covs_df.loc[update_idx, 'beta_negated_space']  = value[0]
            covs_df.loc[update_idx, 'beta_original_space'] = -value[0]

    print(f"\n  Results Summary (HIV Incidence frontier):")
    print(f"    N state-years:      {len(out)}")
    print(f"    N states:           {state_summary['location_id'].nunique() - 1}")
    print(f"    Mean ineff (raw):   {out['ineff_raw'].mean():.4f}")
    print(f"    Median ineff (raw): {out['ineff_raw'].median():.4f}")
    print(f"    Max  ineff (raw):   {out['ineff_raw'].max():.4f}")
    print(f"    eta:                {model.eta:.6f}")
    print(f"\n    State-level change in inefficiency (2015–19 minus 2010–14):")
    print(f"      Mean change (scaled):     {state_summary['ineff_change'].mean():.4f}")
    print(f"      States MORE efficient:    {(state_summary['ineff_change'] < 0).sum()}")
    print(f"      States LESS efficient:    {(state_summary['ineff_change'] > 0).sum()}")

    return out, state_summary, covs_df


# ═══════════════════════════════════════════════════════════════════════════════
# PLOTTING
# ═══════════════════════════════════════════════════════════════════════════════
def plot_frontier(df_output, save_path=None):
    """State-year scatter with LOWER-envelope frontier (cost outcome)."""
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
        label='Estimated Frontier (lower envelope)',
        zorder=5
    )

    ax.set_xlabel('log(HIV Spending per Capita, USD)', fontsize=12)
    ax.set_ylabel('log(HIV Incidence per 100k), covariate-adjusted', fontsize=12)
    ax.set_title(
        f'HIV Prevention Frontier: Achievable Incidence vs Spending per Capita\n'
        f'U.S. states, 2010–2019  |  n = {len(df_output)} state-years, '
        f'{df_output["location_id"].nunique()} jurisdictions',
        fontsize=13, fontweight='bold'
    )
    ax.legend(loc='upper right', fontsize=10)
    ax.grid(True, alpha=0.3, linestyle='--')

    fig.text(
        0.5, 0.01,
        'Outcome: HIV incidence (new diagnoses per 100,000 population).\n'
        'Frontier = LOWER envelope of achievable incidence at each spending level; '
        'points above the frontier indicate efficiency gaps.\n'
        'Adjusted for: contemporaneous HIV prevalence per 100k, race composition '
        '(% Black, % Hispanic), homelessness rate, and year fixed effects.',
        ha='center', fontsize=8, style='italic', color='gray'
    )

    plt.tight_layout(rect=[0, 0.08, 1, 1])
    if save_path:
        plt.savefig(save_path, dpi=150, bbox_inches='tight')
        print(f"  Plot saved to: {save_path}")
    plt.close(fig)
    return fig


def plot_state_frontier(state_summary, save_path=None):
    """State-level frontier with state-abbreviation labels."""
    fig, ax = plt.subplots(figsize=(12, 8))

    plot_df = state_summary[state_summary['location_name'] != 'United States'].copy()
    if len(plot_df) == 0:
        plt.close(fig)
        return None

    ineff_vals = plot_df['ineff_mean'].values
    norm = plt.Normalize(vmin=ineff_vals.min(), vmax=ineff_vals.max())
    cmap = plt.cm.RdYlGn_r       # red = inefficient, green = efficient

    for _, row in plot_df.iterrows():
        color = cmap(norm(row['ineff_mean']))
        ax.text(
            row['log_spend_mean'],
            row['y_adj_log_mean'],
            row['state_abbrev'],
            fontsize=10, fontweight='bold',
            ha='center', va='center',
            color=color, zorder=5
        )

    sm = plt.cm.ScalarMappable(cmap=cmap, norm=norm); sm.set_array([])
    cbar = plt.colorbar(sm, ax=ax, shrink=0.8, pad=0.02)
    cbar.set_label('Mean Inefficiency  (0 = most efficient, 1 = least)', fontsize=10)

    ax.set_xlabel('Mean log(HIV Spending per Capita, USD)', fontsize=12)
    ax.set_ylabel('Mean log(HIV Incidence per 100k), covariate-adjusted', fontsize=12)
    ax.set_title(
        f'HIV Prevention Efficiency by State: Incidence vs Spending per Capita\n'
        f'State means, 2010–2019  |  n = {len(plot_df)} U.S. states',
        fontsize=13, fontweight='bold'
    )
    ax.grid(True, alpha=0.3, linestyle='--')

    fig.text(
        0.5, 0.01,
        'LOWER log(incidence) = better.  Green = more efficient (closer to frontier), red = less efficient.\n'
        'Adjusted for contemporaneous prevalence, race composition, homelessness, and year fixed effects.',
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
    print("\n" + "=" * 75)
    print("SFMA FRONTIER ANALYSIS — HIV INCIDENCE (R2b prevention equation)")
    print(f"Path B convention: hardcoded SE = {SE_FALLBACK}, cost-outcome negation")
    print("=" * 75)
    print(f"  Output: {dir_output}")

    df_full = run_sanity_checks(fp_input_panel)

    out, state_summary, covs = runSFA_HIV_Incidence(df_full)

    if out is None:
        print("\n  ERROR: SFMA fit failed or insufficient data. No output written.")
        sys.exit(1)

    fname_prefix = "hiv_incidence"

    out.to_csv(dir_output           / f'{fname_prefix}_output.csv',        index=False)
    state_summary.to_csv(dir_output / f'{fname_prefix}_state_summary.csv', index=False)
    covs.to_csv(dir_output          / f'{fname_prefix}_covariates.csv',    index=False)

    plot_frontier(
        out,
        save_path=dir_output / f'{fname_prefix}_frontier.png'
    )
    plot_state_frontier(
        state_summary,
        save_path=dir_output / f'{fname_prefix}_state_frontier.png'
    )

    print("\n" + "=" * 75)
    print("ANALYSIS COMPLETE!")
    print(f"Output files saved to: {dir_output}")
    print("=" * 75)
    print("\nOutput files generated:")
    for f in sorted(dir_output.glob(f'{fname_prefix}_*')):
        print(f"  - {f.name}")
    print()