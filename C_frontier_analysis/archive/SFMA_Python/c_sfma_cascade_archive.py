"""
Filename: C_frontier_analysis_sfma_hiv_KxV_v5_pathB.py
Description: SFMA frontier analysis for HIV (Aim 2) — K × V composite outcome.
             *** PATH B VARIANT of v5 ***

             v5 with lag_prev as a covariate produced a flat frontier — the
             inefficiency parameter (η) collapsed to ~0 because the outcome-SD
             based SE (≈ 0.42) gave SFMA too much "noise allowance," so all
             residual variance was absorbed into noise instead of inefficiency.

             Within-state year-to-year SD of logit(K × V) is ≈ 0.247 (closer
             to true measurement noise). The total SD (≈ 0.418) is dominated
             by systematic between-state variation, not noise.

             PATH B fixes the SE side of the identification problem:
               - HARDCODE SE_FALLBACK = 0.15 (a defensible noise level
                 between the absurd 0.0006 from the variance column and the
                 over-generous 0.418 from outcome SD)
               - Skip the variance-column logic entirely for K × V (the
                 variance column was populated for a DALY outcome, not for
                 logit(K × V))
               - All other v5 fixes preserved (dummy-trap fix, lag prev,
                 freshly-built lag).

             Companion regression results that motivated keeping lag_prev:
               Adding contemp prev:    β_spend = +0.196 (was +0.081)
               Adding lagged prev:     β_spend = +0.221 (was +0.081)
               → prev is a SUPPRESSOR — including it almost triples the
                 spending coefficient and tightens its SE.

             v5 CHANGES (May 28, after v4 produced a degenerate fit):
               1. FIXED dummy-variable trap. v4 built year dummies BEFORE the
                  lag-induced NaN drop, leaving year_2010 as the nominal
                  reference but with zero observations after the drop. The
                  sum of remaining year dummies (2011..2019) equalled 1 for
                  every row, perfectly collinear with the intercept. The
                  SFMA optimizer drifted to a degenerate solution
                  (intercept ≈ -177,000, year FE ≈ +177,000 each) that fit
                  the data but had near-zero residual inefficiency.
                  v5 builds year dummies AFTER the dropna, so the reference
                  year is the first in-sample year (2011) and the rank is
                  correct.
               2. FIXED standard-error scale. The cascade panel's 'variance'
                  column was populated for a different outcome (DALY) and
                  gives SE ≈ 0.0006 for logit(K × V), which is ~1000× too
                  small and makes the SFMA Hessian nearly singular when
                  combined with the lagged-prev covariate. v5 detects this
                  mismatch (variance-derived SE < outcome SD / 20) and
                  falls back to outcome SD with a warning. (Already applied
                  during v4 debugging; retained here for completeness.)

             v4 CHANGES (kept from v4):
               1. Lagged prevalence (t-1) as a covariate with POSITIVE prior.
                  R verification: β = +0.37, p = 0.04 * on logit(K × V).
                  Adjusts the frontier for prevalence-induced achievable
                  ceiling and isolates the spending effect more cleanly.
               2. Lag is constructed FRESH (sort by location_id × year_id,
                  group-shift by 1). Does NOT trust any pre-computed lag
                  variable in the panel.
               3. Lagged (not contemporaneous) prevalence breaks the
                  simultaneity between current cascade and current
                  prevalence — DAG-clean per Cinelli, Forney & Pearl 2020.
               4. NaN drop requires non-NaN lag — drops 2010 observations.

             v3 CHANGES (kept from v3):
               - NO outcome negation: K × V is a production outcome,
                 frontier is the upper envelope of logit(K × V).
               - Spline priors: monotone increasing + concave in original
                 logit(K × V) space.

             v2 CHANGES (kept from v2):
               - Panel path updated to cascade panel
               - variance column is optional
               - Loud sanity checks at the top

How to run: python C_frontier_analysis_sfma_hiv_KxV_v5_pathB.py
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

# CASCADE panel (has CDC K, V columns) — not the analysis panel
fp_input_panel = Path(
    '/ihme/homes/idrisov/aim_outputs/Aim2/C_frontier_analysis/'
    '20260517/analysis_per_capita/df_hiv_cascade_panel.csv'
)

today_yyyymmdd = date.today().strftime("%Y%m%d")
dir_output = dir_base / today_yyyymmdd / "kxv_frontier_v5_pathB"
dir_output.mkdir(parents=True, exist_ok=True)

# ── Common column names ──
spend_col     = 'rw_dex_hiv_prev_ratio_log'   # log(Spend per Prevalent Case)
variance_col  = 'variance'                     # may not exist in cascade panel
acause        = 'hiv'

# Cascade component columns (CDC ATLAS)
COL_K = 'cdc_knowledge_status'
COL_V = 'cdc_viral_suppress'

# Raw columns needed to build log_prev_per_100k_lag1
COL_PREV_COUNT = 'hiv_prevalence_counts'
COL_POP        = 'population'

# Name of the lag covariate constructed inside the script
PREV_LAG_COL = 'log_prev_per_100k_lag1'

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
OUTCOME_LABEL = 'K × V (joint suppressed/total PWH)'
OUTCOME_NOTE  = ('Denominator: estimated total PWH (CDC back-calculation). '
                 'K × V = diagnosed × suppressed; the clean joint probability.')

# ── Covariates and PRIORS ──
# v4: log_prev_per_100k_lag1 added with POSITIVE prior, motivated by R
#     verification (β = +0.37, p = 0.04 on logit(K × V) per case spec).
#     log_prop_homeless kept POSITIVE per earlier K × V GLM (+0.16 **).
COV_COLS              = ['race_prop_BLCK', 'race_prop_HISP', 'log_prop_homeless',
                         PREV_LAG_COL]
POS_PRIOR_VARS_BASE   = ['log_prop_homeless', PREV_LAG_COL]
NEG_PRIOR_VARS        = ['race_prop_BLCK', 'race_prop_HISP']

MIN_N     = 50
LOGIT_EPS = 1e-4

# ── Path B: HARDCODED noise SE ──
# Chosen to sit between the absurd 0.0006 from the panel's variance column
# (populated for a DALY outcome, not for logit(K × V)) and the over-generous
# 0.418 = total SD of logit(K × V), which causes η to collapse to ~0.
# Within-state year-to-year SD of logit(K × V) ≈ 0.247; SE = 0.15 is roughly
# half of that, leaving genuine room for inefficiency variance.
# Toggle this if you want to sweep [0.10, 0.15, 0.20, 0.25] as a robustness test.
SE_FALLBACK = 0.15


# ═══════════════════════════════════════════════════════════════════════════════
# SANITY CHECKS — runs FIRST so console output is self-documenting
# ═══════════════════════════════════════════════════════════════════════════════
def run_sanity_checks(fp):
    print("\n" + "█" * 75)
    print("SANITY CHECKS — verifying inputs before fitting (v5 Path B: lagged prev + hardcoded SE)")
    print("█" * 75)

    print(f"\n[1/7] Panel file path:")
    print(f"      {fp}")
    if not fp.exists():
        print(f"      ❌ FILE NOT FOUND. Aborting.")
        sys.exit(1)
    print(f"      ✓ File exists ({fp.stat().st_size / 1e6:.1f} MB)")

    print(f"\n[2/7] Loading panel...")
    df = pd.read_csv(fp)
    print(f"      ✓ Loaded: {len(df):,} rows × {df.shape[1]} columns")
    if 'acause' in df.columns:
        causes = df['acause'].value_counts()
        print(f"      acause distribution: {dict(causes)}")
        df_hiv = df[df['acause'] == 'hiv'].copy() if 'hiv' in causes.index else df
        print(f"      → after filter to HIV: {len(df_hiv):,} rows")
    else:
        print(f"      WARNING: no 'acause' column; assuming all rows are HIV")
        df_hiv = df.copy()

    print(f"\n[3/7] Required columns:")
    required = {
        'CDC K (cdc_knowledge_status)':  COL_K,
        'CDC V (cdc_viral_suppress)':    COL_V,
        'Spending per case (log)':       spend_col,
        'HIV prevalence counts':         COL_PREV_COUNT,
        'Population':                    COL_POP,
        'Race: prop Black':              'race_prop_BLCK',
        'Race: prop Hispanic':           'race_prop_HISP',
        'log(% homeless)':               'log_prop_homeless',
        'location_id':                   'location_id',
        'location_name':                 'location_name',
        'year_id':                       'year_id',
    }
    optional = {
        'variance (for SE)':             variance_col,
    }
    missing = []
    for label, col in required.items():
        if col in df_hiv.columns:
            n_nonnull = df_hiv[col].notna().sum()
            print(f"      ✓ {label:<35s} '{col}'  ({n_nonnull:,} non-null)")
        else:
            print(f"      ❌ {label:<35s} '{col}'  MISSING")
            missing.append(col)
    for label, col in optional.items():
        if col in df_hiv.columns:
            n_nonnull = df_hiv[col].notna().sum()
            print(f"      ✓ {label:<35s} '{col}'  ({n_nonnull:,} non-null)")
        else:
            print(f"      ⚠ {label:<35s} '{col}'  MISSING (fallback SE will be used)")
    if missing:
        print(f"\n      ❌ Required columns missing: {missing}")
        print(f"      Aborting — check the panel file is correct.")
        sys.exit(1)

    print(f"\n[4/7] K × V outcome construction:")
    k_valid = df_hiv[COL_K].where((df_hiv[COL_K] >= 0) & (df_hiv[COL_K] <= 1))
    v_valid = df_hiv[COL_V].where((df_hiv[COL_V] >= 0) & (df_hiv[COL_V] <= 1))
    kxv = k_valid * v_valid
    n_kxv = kxv.notna().sum()
    print(f"      K non-null:           {df_hiv[COL_K].notna().sum():,}")
    print(f"      V non-null:           {df_hiv[COL_V].notna().sum():,}")
    print(f"      Both non-null:        {n_kxv:,} (these become the K × V sample)")
    if n_kxv >= 30:
        print(f"      K × V range:          [{kxv.min():.3f}, {kxv.max():.3f}]")
        print(f"      K × V mean / median:  {kxv.mean():.3f} / {kxv.median():.3f}")
    states_with_kxv = df_hiv.loc[kxv.notna(), 'location_id'].nunique()
    print(f"      States with ≥1 K × V observation: {states_with_kxv}")

    print(f"\n[5/7] Spending predictor distribution:")
    sp = df_hiv[spend_col].dropna()
    if len(sp) > 0:
        print(f"      log(Spend/case) range:  [{sp.min():.3f}, {sp.max():.3f}]")
        print(f"      log(Spend/case) mean:   {sp.mean():.3f}  (SD = {sp.std():.3f})")
        print(f"      $/case range:           [${np.exp(sp.min()):,.0f}, ${np.exp(sp.max()):,.0f}]")
    else:
        print(f"      ❌ Spending column is all-NaN. Aborting.")
        sys.exit(1)

    print(f"\n[6/7] Lagged prevalence (t-1) construction:")
    # Build prev/100k and its log
    df_hiv['_prev_per_100k'] = df_hiv[COL_PREV_COUNT] / df_hiv[COL_POP] * 1e5
    df_hiv['_log_prev']      = np.log(df_hiv['_prev_per_100k'].replace(0, np.nan))
    # Fresh 1-year lag (sort, group-shift)
    df_hiv = df_hiv.sort_values(['location_id', 'year_id']).reset_index(drop=True)
    df_hiv[PREV_LAG_COL] = df_hiv.groupby('location_id')['_log_prev'].shift(1)
    contemp_mean = df_hiv['_log_prev'].mean()
    lag_mean     = df_hiv[PREV_LAG_COL].mean()
    print(f"      Contemp log(prev/100k): mean = {contemp_mean:.3f}  "
          f"(exp = {np.exp(contemp_mean):.1f} per 100k)")
    print(f"      FRESH 1-year lag:       mean = {lag_mean:.3f}  "
          f"(exp = {np.exp(lag_mean):.1f} per 100k)")
    if 'log_prevalence_per_100k_l1' in df_hiv.columns:
        panel_lag_mean = df_hiv['log_prevalence_per_100k_l1'].mean()
        print(f"      Panel's log_prevalence_per_100k_l1: mean = {panel_lag_mean:.3f}  "
              f"(exp = {np.exp(panel_lag_mean):.1f} per 100k)")
        corr = df_hiv['log_prevalence_per_100k_l1'].corr(df_hiv[PREV_LAG_COL])
        print(f"      Correlation panel-lag vs fresh-lag: {corr:.3f}")
        if abs(panel_lag_mean - lag_mean) > 0.5:
            print(f"      ⚠ Panel's lag variable mean differs substantially from fresh lag.")
            print(f"        Using the FRESH lag computed in this script.")
    n_lag_nan = df_hiv[PREV_LAG_COL].isna().sum()
    print(f"      Rows with NaN lag (first year per state will drop): {n_lag_nan:,}")

    print(f"\n[7/7] Year coverage:")
    years = sorted(df_hiv['year_id'].unique())
    print(f"      Years in panel: {years}")
    print(f"      Lag spec drops year {years[0]} — effective panel: {years[1:]}.")
    print(f"      v5 fix: year dummies will be built AFTER the lag-NaN drop,")
    print(f"              so reference year will be {years[1]} (first in-sample year).")

    print("\n" + "█" * 75)
    print(f"SANITY CHECKS PASSED — proceeding to SFMA fit (v5 Path B: lag prev + hardcoded SE)")
    print("█" * 75 + "\n")

    return df_hiv


# ═══════════════════════════════════════════════════════════════════════════════
# MAIN SFMA FUNCTION
# ═══════════════════════════════════════════════════════════════════════════════
def runSFA_HIV_KxV(df_full):
    """Run SFMA frontier on K × V with per-case spending + lagged prev.

    Returns (out, state_summary, covs_df) or (None, None, None) if skipped.
    """
    cov_cols       = COV_COLS.copy()
    pos_prior_vars = POS_PRIOR_VARS_BASE.copy()
    neg_prior_vars = NEG_PRIOR_VARS.copy()

    print(f"\n{'=' * 75}")
    print(f"SFMA HIV K × V FRONTIER  (v5 Path B — lagged prev + HARDCODED SE = {SE_FALLBACK})")
    print(f"  Outcome:    {OUTCOME_LABEL}")
    print(f"  Predictor:  log(Spend per Prevalent Case)  [{spend_col}]")
    print(f"  Frontier interpretation: UPPER envelope of logit(K × V) vs spending")
    print(f"  Inefficiency: how far BELOW the frontier each state-year is")
    print(f"  Lagged prev (t-1) included as covariate with POSITIVE prior")
    print(f"{'=' * 75}")

    df = df_full.copy()

    # ── Build K × V from CDC ATLAS components ──
    k_valid = df[COL_K].where((df[COL_K] >= 0) & (df[COL_K] <= 1))
    v_valid = df[COL_V].where((df[COL_V] >= 0) & (df[COL_V] <= 1))
    df['_kxv_raw']     = k_valid * v_valid
    df['_kxv_clipped'] = df['_kxv_raw'].clip(LOGIT_EPS, 1 - LOGIT_EPS)
    df['kxv_logit']    = np.log(df['_kxv_clipped'] / (1 - df['_kxv_clipped']))
    outcome_col        = 'kxv_logit'

    # ── Ensure lag is present (build if not done in sanity checks) ──
    if PREV_LAG_COL not in df.columns:
        df['_prev_per_100k'] = df[COL_PREV_COUNT] / df[COL_POP] * 1e5
        df['_log_prev']      = np.log(df['_prev_per_100k'].replace(0, np.nan))
        df = df.sort_values(['location_id', 'year_id']).reset_index(drop=True)
        df[PREV_LAG_COL] = df.groupby('location_id')['_log_prev'].shift(1)

    # ── Drop NaN on key vars FIRST (v5 fix: BEFORE year-dummy construction) ──
    # CRITICAL: dropna must happen BEFORE year-dummy construction. Otherwise,
    # if the lag-NaN filter drops 2010, year_2010 remains the nominal reference
    # but has zero observations — and the sum of remaining year dummies equals
    # 1 for every row, which is perfectly collinear with the intercept (the
    # dummy-variable trap). SFMA's optimizer then drifts to a degenerate
    # solution with huge offsetting intercept + year-FE coefficients
    # (observed in v4 first pass: intercept ≈ −177k, year FE ≈ +177k).
    key_vars = [outcome_col, spend_col] + COV_COLS
    if variance_col in df.columns:
        key_vars.append(variance_col)
    n_before = len(df)
    df = df.dropna(subset=key_vars).copy()
    print(f"  N state-years (after NaN drop on K × V + lag prev + key vars): {len(df)} "
          f"(dropped {n_before - len(df)})")
    print(f"  Effective state clusters: {df['location_id'].nunique()}")
    print(f"  Years in fitted sample: {sorted(df['year_id'].unique())}")
    print(f"  K × V range: [{df['_kxv_raw'].min():.3f}, {df['_kxv_raw'].max():.3f}], "
          f"mean = {df['_kxv_raw'].mean():.3f}")
    print(f"  Lag prev range: [{df[PREV_LAG_COL].min():.3f}, {df[PREV_LAG_COL].max():.3f}], "
          f"mean = {df[PREV_LAG_COL].mean():.3f}")

    if len(df) < MIN_N:
        print(f"  SKIPPED — fewer than {MIN_N} state-years available.")
        return None, None, None

    # ── Year dummies (built AFTER dropna so reference year is in-sample) ──
    year_dummy_cols = []
    years = sorted(df['year_id'].unique())
    ref_year = years[0]
    print(f"  Reference year (post-drop, in-sample): {ref_year}")
    for yr in years[1:]:
        col = f'year_{yr}'
        df[col] = (df['year_id'] == yr).astype(float)
        year_dummy_cols.append(col)
        pos_prior_vars.append(col)
    cov_cols = cov_cols + year_dummy_cols

    all_model_vars = [spend_col] + cov_cols

    # ── Preserve original spending for plotting ──
    df['log_spend_orig'] = df[spend_col].copy()

    # ── Z-score continuous variables only (year dummies stay as 0/1) ──
    vars_to_zscore = [v for v in all_model_vars if v not in year_dummy_cols]
    for var in vars_to_zscore:
        mu = df[var].mean(); sd = df[var].std()
        if sd > 0:
            df[var] = (df[var] - mu) / sd
        else:
            print(f"  WARNING: {var} has zero SD, skipping z-score")

    # ────────────────────────────────────────────────────────────────────
    # v3/v4/v5 CONVENTION: DO NOT NEGATE the outcome.
    # K × V is a "production" outcome (higher = better cascade coverage).
    # SFMA fits the upper envelope directly. Spline priors (monotone
    # increasing + concave) operate in ORIGINAL logit(K × V) space.
    # ────────────────────────────────────────────────────────────────────

    # ── Path B: HARDCODED noise SE = SE_FALLBACK ──
    #    Bypasses the panel's variance column (populated for the DALY
    #    outcome, not logit(K × V)) and the outcome-SD fallback (which
    #    caused η to collapse to 0 in v5). The hardcoded value sits in a
    #    defensible range based on the within-state year-to-year SD of
    #    logit(K × V) ≈ 0.247 (closer to true measurement noise).
    outcome_sd = float(df[outcome_col].std())
    within_state_sd = float(df.groupby('location_id')[outcome_col].std().mean())
    se_val = float(SE_FALLBACK)
    print(f"  Standard error (HARDCODED, Path B): {se_val:.4f}")
    print(f"    For reference: outcome total SD = {outcome_sd:.4f}, "
          f"within-state SD = {within_state_sd:.4f}")
    print(f"    Path B intentionally uses a smaller SE than total SD so that")
    print(f"    residual variance can be attributed to inefficiency, not noise.")
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

        # Spline priors in ORIGINAL logit(K × V) space:
        #   1st deriv ≥ 0  → frontier increases with spending (more $ → higher K × V achievable)
        #   2nd deriv ≤ 0  → concave (diminishing returns)
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

    # ── Initial model with all covariates ──
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
    Y_adj   = Y - cov_hat

    ineff = model.get_inefficiency()

    id_cols = ['location_name', 'location_id', 'year_id', 'cause_id', 'acause', 'cause_name']
    id_cols_present = [c for c in id_cols if c in df.columns]

    out = df[id_cols_present + [spend_col, 'log_spend_orig']].copy()
    out[outcome_col]   = Y
    out['ineff']       = ineff
    # No un-negation (production outcome). Plot values already in logit(K × V).
    out['y_adj']       = Y_adj
    out['y_adj_hat']   = Y_hat_adj
    out['y_adj_log']   = out['y_adj']
    out['y_adj_hat_log'] = out['y_adj_hat']
    out['outcome_key']   = 'KxV'
    out['kxv_raw']     = df['_kxv_raw'].values
    out.sort_values(spend_col, inplace=True)

    out['ineff_raw'] = out['ineff']
    if out['ineff'].max() > out['ineff'].min():
        out['ineff'] = (out['ineff'] - out['ineff'].min()) / (out['ineff'].max() - out['ineff'].min())
    else:
        out['ineff'] = 0.0

    # ══════════════════════════════════════════════════════════════════════
    # STATE-LEVEL COLLAPSED SUMMARY
    # ══════════════════════════════════════════════════════════════════════
    state_mean = (out.groupby(['location_name', 'location_id'])
                     .agg(ineff_raw_mean=('ineff_raw', 'mean'),
                          ineff_mean    =('ineff',     'mean'))
                     .reset_index())

    early = (out[out['year_id'].between(2011, 2014)]
             .groupby(['location_name', 'location_id'])['ineff_raw']
             .mean().rename('ineff_raw_early'))
    late = (out[out['year_id'].between(2015, 2019)]
            .groupby(['location_name', 'location_id'])['ineff_raw']
            .mean().rename('ineff_raw_late'))
    early_sc = (out[out['year_id'].between(2011, 2014)]
                .groupby(['location_name', 'location_id'])['ineff']
                .mean().rename('ineff_early'))
    late_sc = (out[out['year_id'].between(2015, 2019)]
               .groupby(['location_name', 'location_id'])['ineff']
               .mean().rename('ineff_late'))

    spend_mean = (out.groupby(['location_name', 'location_id'])['log_spend_orig']
                     .mean().rename('log_spend_mean'))
    y_adj_mean = (out.groupby(['location_name', 'location_id'])['y_adj_log']
                     .mean().rename('y_adj_log_mean'))
    kxv_mean = (out.groupby(['location_name', 'location_id'])['kxv_raw']
                   .mean().rename('kxv_raw_mean'))

    state_summary = (state_mean
                     .merge(early,      on=['location_name', 'location_id'])
                     .merge(late,       on=['location_name', 'location_id'])
                     .merge(early_sc,   on=['location_name', 'location_id'])
                     .merge(late_sc,    on=['location_name', 'location_id'])
                     .merge(spend_mean, on=['location_name', 'location_id'])
                     .merge(y_adj_mean, on=['location_name', 'location_id'])
                     .merge(kxv_mean,   on=['location_name', 'location_id']))

    state_summary['ineff_raw_change'] = state_summary['ineff_raw_late'] - state_summary['ineff_raw_early']
    state_summary['ineff_change']     = state_summary['ineff_late']     - state_summary['ineff_early']

    state_summary['state_abbrev'] = state_summary['location_name'].map(STATE_ABBREV).fillna(
        state_summary['location_name'].str[:2].str.upper()
    )
    state_summary['outcome_key'] = 'KxV'

    col_order = [
        'location_name', 'location_id', 'state_abbrev', 'outcome_key',
        'ineff_raw_mean', 'ineff_raw_early', 'ineff_raw_late', 'ineff_raw_change',
        'ineff_mean',     'ineff_early',     'ineff_late',     'ineff_change',
        'log_spend_mean', 'y_adj_log_mean',  'kxv_raw_mean',
    ]
    state_summary = state_summary[col_order]
    state_summary.sort_values('ineff_mean', inplace=True)

    us_row = pd.DataFrame([{
        'location_name': 'United States',
        'location_id': -1,
        'state_abbrev': 'US',
        'outcome_key': 'KxV',
        **{c: state_summary[c].mean() for c in col_order
           if c not in ['location_name', 'location_id', 'state_abbrev', 'outcome_key']},
    }])
    state_summary = pd.concat([state_summary, us_row], ignore_index=True)

    # ── Covariate summary ──
    covs_only = [c for c in selected_covs if c != spend_col]
    if len(covs_only) > 0:
        covs_df = pd.DataFrame({'selected_covs': covs_only,
                                'acause':        acause,
                                'outcome_key':   'KxV'})
    else:
        covs_df = pd.DataFrame({'selected_covs': ['no covs selected'],
                                'acause':        acause,
                                'outcome_key':   'KxV'})
    for name, value in betas.items():
        if value.size == 1 and name != spend_col:
            update_idx = covs_df['selected_covs'] == name
            covs_df.loc[update_idx, 'beta'] = value[0]

    # ── Print summary ──
    print(f"\n  Results Summary ({OUTCOME_LABEL}):")
    print(f"    N state-years:      {len(out)}")
    print(f"    N states:           {state_summary['location_id'].nunique() - 1}")
    print(f"    Mean ineff (raw):   {out['ineff_raw'].mean():.4f}")
    print(f"    Median ineff (raw): {out['ineff_raw'].median():.4f}")
    print(f"    Max  ineff (raw):   {out['ineff_raw'].max():.4f}")
    print(f"    eta:                {model.eta:.6f}")
    print(f"\n    State-level change in inefficiency (2015–19 minus 2011–14):")
    print(f"      Mean change (scaled):     {state_summary['ineff_change'].mean():.4f}")
    print(f"      States MORE efficient:    {(state_summary['ineff_change'] < 0).sum()}")
    print(f"      States LESS efficient:    {(state_summary['ineff_change'] > 0).sum()}")

    return out, state_summary, covs_df


# ═══════════════════════════════════════════════════════════════════════════════
# PLOTTING
# ═══════════════════════════════════════════════════════════════════════════════
def plot_frontier(df_output, save_path=None):
    """Frontier visualization in log–logit space (state-year dots)."""
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
        label='Estimated Frontier (upper envelope)',
        zorder=5
    )

    ax.set_xlabel('log(HIV Spending per Prevalent Case, USD)', fontsize=12)
    ax.set_ylabel('logit(K × V), covariate-adjusted', fontsize=12)
    ax.set_title(
        f'HIV Cascade Efficiency Frontier: Achievable K × V vs Spending per Case\n'
        f'U.S. states, 2011–2019  |  n = {len(df_output)} state-years, '
        f'{df_output["location_id"].nunique()} jurisdictions',
        fontsize=13, fontweight='bold'
    )
    ax.legend(loc='lower right', fontsize=10)
    ax.grid(True, alpha=0.3, linestyle='--')

    fig.text(
        0.5, 0.01,
        'Outcome: K × V = joint probability of HIV diagnosis × viral suppression '
        'among people living with HIV (CDC ATLAS).\n'
        'Frontier = upper envelope of achievable K × V at each spending level; '
        'points below the frontier indicate efficiency gaps.\n'
        'Adjusted for: race composition (% Black, % Hispanic), homelessness rate, '
        '1-year-lagged HIV prevalence (per 100k), and year fixed effects.',
        ha='center', fontsize=8, style='italic', color='gray'
    )

    plt.tight_layout(rect=[0, 0.08, 1, 1])

    if save_path:
        plt.savefig(save_path, dpi=150, bbox_inches='tight')
        print(f"  Plot saved to: {save_path}")
    plt.close(fig)
    return fig


def plot_state_frontier(state_summary, save_path=None):
    """State-level frontier plot with state-abbreviation labels."""
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

    sm = plt.cm.ScalarMappable(cmap=cmap, norm=norm)
    sm.set_array([])
    cbar = plt.colorbar(sm, ax=ax, shrink=0.8, pad=0.02)
    cbar.set_label('Mean Inefficiency  (0 = most efficient, 1 = least)', fontsize=10)

    ax.set_xlabel('Mean log(HIV Spending per Prevalent Case, USD)', fontsize=12)
    ax.set_ylabel('Mean logit(K × V), covariate-adjusted', fontsize=12)
    ax.set_title(
        f'HIV Cascade Efficiency by State: K × V vs Spending per Case\n'
        f'State means, 2011–2019  |  n = {len(plot_df)} U.S. states',
        fontsize=13, fontweight='bold'
    )
    ax.grid(True, alpha=0.3, linestyle='--')

    fig.text(
        0.5, 0.01,
        'Outcome: joint probability of HIV diagnosis × viral suppression (CDC ATLAS K × V).   '
        'Green = more efficient, red = less efficient.\n'
        'Adjusted for race composition, homelessness rate, 1-year-lagged HIV prevalence, '
        'and year fixed effects.',
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
    print("SFMA FRONTIER ANALYSIS — HIV K × V (single pooled outcome)  v5 Path B")
    print(f"v5 Path B: lagged prev (t-1) covariate + HARDCODED SE = {SE_FALLBACK}")
    print("=" * 75)
    print(f"  Output: {dir_output}")

    df_full = run_sanity_checks(fp_input_panel)

    out, state_summary, covs = runSFA_HIV_KxV(df_full)

    if out is None:
        print("\n  ERROR: SFMA fit failed or insufficient data. No output written.")
        sys.exit(1)

    fname_prefix = "hiv_kxv"

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