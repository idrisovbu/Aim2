# C_frontier_analysis — README

Efficiency-frontier and spending–outcome regression code for Aim 2 (HIV) and
Aim 3 (OUD/SUD). Upstream panel + covariates are built in C_00 / C_01 and
shared by both diseases; the disease-specific modeling diverges here.

## Key decision (committee, June 3, 2026)

**The frontier (SFMA efficiency) analysis is an HIV method. It is DROPPED for SUD/OUD.**
  
  - **HIV (Aim 2): KEEP the frontier.** Outcome = **population viral suppression
(K × V)** — the share of all PLHIV who are diagnosed and virally suppressed
(suppressed ÷ all PLHIV, CDC ATLAS). Predictor = log(HIV spending per prevalent
                                                     case). The achievable K × V rises with spending and then plateaus — a valid
production relationship — so an SFMA efficiency frontier is appropriate.
- Do NOT call K × V the "cascade" (per Joe); use "population viral suppression."
- The regression is a means to set frontier priors; the conditional-mean
spending coefficient is null (β ≈ +0.22, p ≈ 0.22) — the story is the
frontier SHAPE, not a significant slope.
- Controls: % Black, % Hispanic, log(homelessness), lagged (t−1) prevalence,
year FE. Secondary/sensitivity: K-only (full coverage) for V-missingness.

- **OUD/SUD (Aim 3): DROP the frontier.** Empirically it comes out flat /
  degenerate: more OUD spending tracks WORSE per-case outcomes (reactive
                                                                allocation — worst-epidemic states receive the most money), so SFMA's
  monotonicity assumption is violated and there is no production relationship
  to trace. OUD instead uses **spending-effectiveness + Das Gupta decomposition**
  (in D_tables_figures/), which fit OUD because there is no treatment-as-prevention
  channel as in HIV. The spending→outcome regression for OUD is retained only as a
  DIAGNOSTIC (it explains why the frontier fails), not as a reported result.

## Files

- `C_00_prep_A_panel_foundation.R` / `C_01_prep_B_regression_covariates.R`
  — shared panel + covariate construction (both diseases).
- `C_02_HIV_*` — HIV regressions feeding the frontier (final = fully adjusted spec).
- `C_03_SUD_per_case_models.R` — OUD/SUD spending–outcome regressions (diagnostic).
- `*.py` (SFMA) — frontier fits (IHME `sfma` package), HIV cascade/tertiles/incidence.
- `archive/` — superseded versions.

## TO UPDATE (scripts predate the June 3 decisions)

The HIV regression script header still documents the older v4 plan
(DALYs-per-case as primary outcome, K × L × V as the primary mediator, "cascade"
language). Bring the HIV scripts in line with the locked spec:
  1. K × V (population viral suppression) as the PRIMARY outcome (K × L × V → drop/sensitivity only).
  2. Drop the DALYs-per-case spending-effectiveness analysis (kept only as decomposition, descriptive).
  3. Replace "cascade" wording with "population viral suppression."
  4. Add a proper multiple-imputation sensitivity for V-missingness (MCAR is implausible).