# Validation and evidence limits

## Verified locally

- R 4.5.3 and Quarto 1.11.9 are installed in the prepared cloud environment.
- The reusable setup script was rerun successfully with an explicit package lock.
- Offline Python tests verify data-catalog handling, file checksums/cache
  behavior, safe extraction, and the empirical release package's provenance,
  completeness, and stale-output checks.
- R preparation tests cover timing, population boundaries, weights, missingness,
  and invalid source codes.
- Estimator tests cover known and null contrasts, composition adjustment, both
  sides of double robustness, weight/offset invariance, overlap/ESS failures,
  and covariate balance including zero target variance.
- Annual-model tests recover known gap changes and preserve weight invariance.
  An independent review matched coefficients and HC1 standard errors against
  weighted `lm` with `sandwich::vcovHC` on a noisy generated example.
- An end-to-end test writes 9,600 simulated SAS records in a temporary project
  and runs the real analysis script. It checks 24 stratum/outcome/window
  combinations, 96 method estimates, 72 annual contrasts, 96 cell diagnostics,
  384 balance rows, and 216 trend-sensitivity rows. It also checks 32 placebo,
  80 distributional, and 64 richer-adjustment estimate rows; 44 extension
  diagnostic rows; and 32 bootstrap summaries from three exploratory refits per
  stratum/outcome. Exported LaTeX/Markdown table values match their CSV sources.
  Direct outcome-perturbation refits agree with the sensitivity calculation.
  All temporary data are removed. Three draws test software; they are not a
  substitute for the 199-draw empirical uncertainty workflow.
- Bootstrap unit tests verify nonzero uncertainty, reproducibility, preserved
  cell counts and RNG state, invariance to survey-weight units, and retained
  failures with uncertainty suppression.
- Extended-analysis tests recover pre-policy nulls and known CDF contrasts,
  and check that primary/rich comparisons use identical observations.
- The supplied empirical dataset has now been executed through the complete R
  pipeline. All 1,592 requested bootstrap refits (199 for each of eight
  stratum/outcome analyses) succeeded. There are no failed deterministic
  extension scenarios. The run uses seed 256, three workers, and one BLAS thread;
  serial/parallel reproducibility is covered by the bootstrap tests.
- Selected published-specification reconstruction matches 265/272 checked
  quantities at printed precision. All 48 observation counts agree. The seven
  discrepancies were visually checked in the provided proof; two disputed
  estimates/standard errors were independently refitted with `stats::lm`.
- The manuscript (16 pages including references) and appendix (11 pages) render from actual CSV results. Earlier no-data
  and simulated-fixture rendering checks were software checks only and are not
  counted as empirical evidence.
- The original R/Quarto/LaTeX source is available in Git history at
  [commit `3334cc82f099958be49fd4bc2f76c26b0ed00b21`](https://github.com/Frankyin-jpg/Stat-256-Final-Project/tree/3334cc82f099958be49fd4bc2f76c26b0ed00b21).

`make test` runs the maintained test suites. The SAS fixture writer emits a
deprecation warning in Haven; it remains intentional because the importer must
be exercised against a real `.sas7bdat` file structure, rather than another format.

## Source and interpretation limits

The user-supplied GitHub release contains the SAS extract and a 45-page checked
July 2001 JPE proof. The acquired ZIP and its extracted data/PDF have recorded
hashes. The pipeline verifies a hash-bound source audit before marking the
observations empirical and the mappings publication/source-label verified.
This supplied archive has not been established to be the complete Dataverse
deposit, and no original SAS analysis programs are present. The checked proof
has not been independently compared with the final journal printing.

The data contain 971,058 records. The age-eligible population contains 759,771
records before weight restrictions, while the regression data contain 709,125
positive-weight observations. Preserving these two populations reconciles the
paper's descriptive and regression counts. The publication reconstruction is
limited to selected Table 1 summaries and eight no-trend Table 2 models; it does
not claim that Tables 3–6 or original programming have been reproduced.

The initial 0.01 joint-cell probability guard was revised after a support-only
diagnosis, before extension effect estimates were examined. The numerical
floor, unchanged effective-sample-size rule, retained warnings, and supporting
ratios are recorded in `docs/support-amendment.md`. No observations were trimmed.

Agreement with source estimates is computational validation, not proof of
causal identification. The adjusted contrasts remain vulnerable to endogenous
disability reporting, comparison trends, compositional change, and the survey
redesign. Individual-bootstrap intervals condition on a working independence
model and do not supply policy-assignment or full CPS-design uncertainty.

The configured methodology uses Peng Ding's *A First Course in Causal Inference*
as its working reference. The cloud setup/startup configuration is saved as a
draft; saving is distinct from environment publication and from a GitHub push.
