# Employment protection, disability, and employment

A reproducible research rebuild of the Stat 256 project on Acemoglu and Angrist
(2001), with a repeated-cross-section causal inference extension.

Read the results: [paper PDF](https://github.com/Frankyin-jpg/Stat-256-Final-Project/blob/main/release/manuscript/paper.pdf) and [technical appendix PDF](https://github.com/Frankyin-jpg/Stat-256-Final-Project/blob/main/release/appendix/appendix.pdf).

The analysis uses the supplied SAS extract: **971,058 input records and 709,125
positive-weight observations aged 21–58**. A reconstruction of selected Tables
1–2 matches **265 of 272 checked entries**, including all 48 observation counts,
at the precision printed in the supplied July 2001 paper proof. Seven remaining
discrepancies are retained in the [replication audit](docs/replication-audit.md).
The archive contains the dataset and paper, but no original analysis programs;
this is a published-specification reconstruction, not a verified SAS-code translation.
Simulated tests are kept separate from every empirical result.

## Research design

How did the employment gap between people reporting a work-limiting disability
and other working-age people change around the ADA? The extension compares
regression standardization, IPW, and four-cell AIPW for the **same disabled-post
target population**. Unadjusted DID is a benchmark based on each cell's observed
distribution. Extensions examine annual gaps, timing windows, pre-policy
placebos, employment-distribution shifts, richer covariate adjustment, overlap,
and sensitivity to parallel trends. A causal policy
interpretation additionally requires stable disability reporting/composition
and credible comparison-group trends.

The methodological reference is provisionally Peng Ding's *A First Course in
Causal Inference*, pending confirmation of the intended textbook/edition.
See [the analysis plan](docs/analysis-plan.md) and
[the report source](report/paper.qmd). The final main paper has 15 pages of text plus one page of references,
with an 11-page [technical appendix](report/appendix.qmd). A
[generic causal-paper template](docs/causal-paper-template.md) makes the structure
reusable for other causal inference projects.

## Layout

| Path | Purpose |
| --- | --- |
| `scripts/fetch_release.py` | Retrieve the pinned supplied data/paper archive and verify its SHA-256 |
| `scripts/fetch_dataverse.py` | Optional separate acquisition from the repository's original Dataverse link |
| `R/prepare_data.R` | Validate source codes and create one canonical analysis frame |
| `R/replication.R` | Reconstruct selected published Tables 1–2 and compare 272 numerical targets |
| `R/event_study.R` | Weighted summaries, annual contrasts, period pooling |
| `R/estimate_did.R` | Common-target RA/IPW/AIPW estimates and overlap diagnostics |
| `R/extended_analysis.R` | Pre-policy placebos, CDF contrasts, and common-sample richer adjustment |
| `R/bootstrap_did.R` | Full-refit, cell-stratified individual bootstrap with every failure retained |
| `R/export_tables.R` | LaTeX and Markdown tables generated from the result CSVs |
| `scripts/run_analysis.R` | Produce empirical tables from the raw input |
| `report/` | Executable research report and bibliography |
| `tests/` | Offline tests using explicitly simulated inputs |
| `data/raw/` | Source downloads, catalog, checksum manifest, and original archive contents |
| `data/derived/`, `results/`, `_output/` | Generated data, estimates, and rendered report |
| `env/` | Dependency specification and exact Linux package lock |
| `release/` | Curated final GitHub package, created only from verified empirical outputs |

Working raw data and generated artifacts live inside the project but are ignored
by Git. The final `release/` package includes original and canonical data, analysis
code, result tables, main paper, appendix, and a SHA-256 manifest. Files above
GitHub's ordinary size limit require Git LFS; the packager identifies them rather
than dropping them. Preserve source data and record its redistribution terms.

## Environment

In the prepared cloud environment:

```bash
source /workspace/stat256-environment/activate.sh
cd /workspace/Stat-256-Final-Project
make test
make report
```

Activation enables R 4.5.3 and Quarto 1.11.9 and uses writable cache directories.
Setup and startup instructions are also saved in the environment draft.
No server is needed. For another Linux x86-64 machine with Conda:

```bash
conda create -n stat256 --file env/conda-linux-64.lock
conda activate stat256
```

Other platforms can resolve `env/environment.yml`; those platforms have not
been validated. Python 3.10+ and Make are also required.

## Acquire and analyze the original data

The supplied [GitHub release](https://github.com/Frankyin-jpg/Stat-256-Final-Project/releases/tag/replication-data-v1)
contains `data_and_paper.zip` (91,436,493 bytes), with SHA-256
`a7479a211d034f8fb9209c03435b9be5742f690924c5880cd1a855d5040bc0e4`.
It expands to the SAS data and a 45-page checked JPE proof. Acquire it with:

```bash
make fetch
```

The original repository cites Harvard Dataverse
[DOI 10.7910/DVN/WCNKB1](https://doi.org/10.7910/DVN/WCNKB1), but the supplied
release has not been verified as a complete copy of that deposit. It contains
no original SAS analysis programs. The [source audit](docs/source-audit.md) and
hash-bound `data/source-verification.json` record exactly what was checked.
Direct Dataverse access is unnecessary for the supplied-file workflow.

After verification of the source mappings:

```bash
make analyze                         # point estimates and all deterministic extensions
# Or: make analyze RAW_DATA=/absolute/path/to/marcps_w.sas7bdat
make analyze-full BOOTSTRAP=199 SEED=256 WORKERS=3  # main-window full-refit bootstrap
make report
make prepare-release                 # requires verified data, results, and final renders
```

The pipeline preserves every year, distinguishes survey and outcome year,
keeps zero employment outcomes, reports exclusions, and gives each year equal
weight within each extension period. Unsupported coding, missing years,
nonconvergence, and numerical support failures remain visible. A documented
[support-rule amendment](docs/support-amendment.md) distinguishes rare joint-cell
probabilities from extreme covariate-density ratios: the empirical numerical
floor is 0.000001, the original 0.01 threshold is retained as a warning
diagnostic, and effective sample sizes must exceed 20. No observations are
trimmed and no probabilities or weights are capped.

Annual-model intervals use HC1 under an individual-independence working model.
They do not solve CPS dependence or uncertainty from a single national policy.
`analyze-full` adds percentile intervals from 199 full estimator refits,
resampling individuals separately within the four disability/period cells.
These condition on cell counts and an individual-independence working model;
they do not supply survey-design or policy-assignment uncertainty. Failed draws
are retained, and the default suppresses intervals if any requested refit fails.
Small-bootstrap runs are marked exploratory and are rejected for a final release.

The release process is described in [docs/release.md](docs/release.md). It never
commits or pushes automatically. In particular, a successful software test or
render without data cannot produce a final empirical release. All downstream
figures and tables are regenerated from the same verified analysis run.

## Scope and limitations

The reconstruction covers selected descriptive statistics and the eight
no-trend employment/log-wage models. It does not reproduce every table in the
original study. All source targets come from the supplied checked proof, which
has not been compared with the final journal printing. Numerical discrepancies
are reported without widening their printed-precision tolerances.

The extensions estimate adjusted changes in observed disability-group gaps.
They do not establish that the ADA caused those changes. Disability reporting,
the 1994 survey redesign, comparison-group trends, post-treatment covariates,
and a single national policy timeline remain substantive identification issues.
The individual bootstrap is a conditional sampling approximation, not a remedy
for these issues or for unmodeled CPS dependence.
