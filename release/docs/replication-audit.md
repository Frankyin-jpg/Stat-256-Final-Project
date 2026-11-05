# Selected published-table reconstruction audit

The supplied data reproduce **265 of 272 selected numerical entries at the
precision printed in the supplied paper proof**. All 48 checked observation
counts agree, including all eight Table 2 estimation samples. Seven numerical
discrepancies remain and are listed below. This is a reconstruction from the
paper and supplied extract, not a verified translation of original SAS programs.
The inspected archive contains the SAS dataset and PDF, but no original SAS
analysis programs.

## Source identity and scope

The benchmark source is
`data/raw/extracted/github-release/data_and_paper/Consequences of Employment Protection The Case of.pdf`.
It is a typeset **July 2, 2001 proof**, with footer `09:44 AM JPE v109n5
019509 VML` and pages labeled `Proof 1` / `CHECKED`. It has not been independently
compared with the final journal printing. Its SHA-256 is
`62dbb8a88e1a9eefcaca2807fa294f276d17ba7215718cb9a333b80ed1f1c5ec`.
The source is the Acemoglu and Angrist study “Consequences of Employment
Protection? The Case of the Americans with Disabilities Act.”

Targets in [`R/replication.R`](../R/replication.R) were transcribed from this
source, using extracted text and visual inspection of the actual PDF pages:

- Table 1, proof/PDF pages 13–14: mean age, mean weeks worked, and observation
  counts in all 40 combinations of four age/sex strata, five survey years, and
  disability status. These provide 120 comparisons.
- Table 2, proof/PDF page 17: all nine annual disability interaction estimates,
  their standard errors, and sample counts in the eight no-trend models
  (columns 1, 3, 5, and 7 in both age panels). These provide 152 comparisons.

This selection includes weeks worked and log weekly earnings. It excludes
Table 1 wage/benefit summaries, Table 2 trend columns, and Tables 3–6. Their
omission is explicit; the selected benchmark count does not imply that the
whole paper has been reproduced. Historical tables available in Git history at
[commit `3334cc82f099958be49fd4bc2f76c26b0ed00b21`](https://github.com/Frankyin-jpg/Stat-256-Final-Project/tree/3334cc82f099958be49fd4bc2f76c26b0ed00b21)
were not used as independent targets.

The machine-readable evidence is
[`results/replication_benchmarks.csv`](../results/replication_benchmarks.csv).
Every row contains its source table/page/column, demographic and outcome keys,
printed target and its original precision, reproduced value, tolerance,
difference, pass flag, and status. `original_value`, `reproduced_value`, and
`passed` are aliases used by release validation, and `benchmark_id` uniquely
identifies the row. No discrepancies are overwritten or relabeled as passes.

## Clock, samples, and model

**Table 1 uses survey years; Table 2 uses annual outcome years.** Section IV on
proof pages 11–12 explicitly says March surveys 1988–1997 report annual weeks
and earnings for 1987–1996, and identifies Table 1 as an exception dated by
survey year. Thus `outcome_year = survey_year - 1`. Disability is reported at
the survey interview; this timing difference remains relevant to causal
interpretation. Table 2 normalizes the outcome-year 1987 disability interaction
to zero. The paper describes 1992 as a transition year and 1993–1996 as
posttreatment years on proof page 15.

**Table 1 counts records before weighting restrictions.** The raw extract has
971,058 records. Restriction to ages 21–58 produces 759,771 records, while the
positive nonmissing `FNLWGT2` estimation sample contains 709,125. The distinction
is consequential: in survey-year 1988, young men have 1,059 disabled and 21,532
nondisabled raw age-eligible records, exactly as printed in Table 1, but only
998 and 20,393 records with usable positive analysis weights. All 40 selected
Table 1 counts match when calculated before excluding zero/missing weights.
Weighted means use the positive-weight observations. This count reconciliation
follows the observed definition of the table's population and the source data;
it does not require changing the regression sample to chase a coefficient.

`prepare_cps()` therefore retains the raw count table as
`prepared$population_counts`. The replication API accepts it separately from
`prepared$data`. If it is absent, the API's explicitly labeled fallback is the
canonical positive-weight count, which is not equivalent to the original raw
Table 1 population count and should not be described as such.

**Table 2 reconstructs the full categorical specification.** Proof pages 12/15
and the table note describe individual disability, year, age-group, race,
schooling, and region main effects, plus year interactions with each covariate.
Within each age/sex stratum, the fitted formula is

```r
outcome ~ disabled * factor(outcome_year) +
  factor(outcome_year) * (age_group + race + education + region)
```

Age groups are 20s/30s or 40s/50s; race has three categories, schooling has
less-than-high-school/high-school/some-college-or-more categories, and region
has nine categories. The categorical coding is explicit: numeric region slopes
are not substituted for region indicators. All eight real-data models have
150 estimated parameters. The three schooling categories are also explicitly
listed in Table 3's note; together with age, race, and region they produce the
paper's 162 covariate cells.

The weight is `FNLWGT2 / 100`. The paper's Appendix A says all estimates use CPS
weights updated to 1990 Census population counts, but it does not itself name
this raw variable. Source-label inspection and the numerical comparisons provide
the supporting evidence for the mapping; absence of original programs still
limits verification. Multiplication of all weights by a common positive
constant does not change the reconstructed estimates or conventional WLS
standard errors.

Wage models use positive weeks and annual wage earnings, the legacy real-weekly-
wage eligibility rule `[25, 2000]`, and `log(WSAL_VAL / WKSWORK)`. The legacy CPI
series is indexed by survey year. This precise trimming/deflator implementation
is not established by the supplied paper description alone. Its use is
transparent, and the resulting four wage estimation counts match exactly.
Subtracting a year-specific log deflator from the wage outcome would be absorbed
by the saturated year effects on the same sample; it does not validate the wage
selection rule or establish unconditional effects for nonworkers.

| Stratum | Weeks-worked N | Log-weekly-earnings N |
| --- | ---: | ---: |
| Men, 21–39 | 193,317 | 167,974 |
| Women, 21–39 | 211,910 | 157,688 |
| Men, 40–58 | 146,309 | 116,665 |
| Women, 40–58 | 157,589 | 109,082 |

All eight counts above match Table 2 exactly. There are no missing categorical
covariates in these estimation samples.

## Comparison rules and results

Each target retains its actual printed precision. A value printed to `d`
decimals is accepted within `0.5 * 10^(-d)`, with only `1e-10` floating-point
slack. Observation counts require exact equality. These rules were fixed from
source precision and not enlarged to make mismatches disappear. The comparison
uses unrounded reproduced estimates; `unrun`, `discrepancy`, and
`matched_printed_precision` remain separate statuses.

| Selected component | Matched / checked |
| --- | ---: |
| Table 1 raw population counts | 40 / 40 |
| Table 1 mean age | 40 / 40 |
| Table 1 mean weeks worked | 39 / 40 |
| Table 2 weeks-worked coefficients | 36 / 36 |
| Table 2 weeks-worked standard errors | 35 / 36 |
| Table 2 log-wage coefficients | 35 / 36 |
| Table 2 log-wage standard errors | 32 / 36 |
| Table 2 estimation sample counts | 8 / 8 |
| **Total** | **265 / 272** |

For orientation, the reconstructed no-trend employment interaction estimates
include:

| Stratum | Outcome year 1993 | Outcome year 1996 |
| --- | ---: | ---: |
| Men, 21–39 | -1.440003 | -2.683046 |
| Women, 21–39 | -4.368430 | -4.411634 |
| Men, 40–58 | -2.124296 | -0.753337 |
| Women, 40–58 | -0.922928 | -3.389930 |

All eight entries round to the corresponding Table 2 values. Agreement with a
published regression is a computational validation, not additional evidence for
the identifying assumptions behind that regression.

## Seven discrepancies retained

| Table / subgroup / statistic | Year basis | Printed | Reproduced | Tolerance |
| --- | --- | ---: | ---: | ---: |
| T1, disabled women 40–58, mean weeks | Survey 1996 | 11.3 | 11.755861984 | 0.05 |
| T2, men 40–58, weeks interaction SE | Outcome 1988 | 0.55 | 0.586078357 | 0.005 |
| T2, men 21–39, log-wage interaction SE | Outcome 1991 | 0.043 | 0.043528832 | 0.0005 |
| T2, women 21–39, log-wage interaction coefficient | Outcome 1992 | -0.010 | -0.009174677 | 0.0005 |
| T2, women 21–39, log-wage interaction SE | Outcome 1993 | 0.050 | 0.051200612 | 0.0005 |
| T2, men 40–58, log-wage interaction SE | Outcome 1993 | 0.042 | 0.043269307 | 0.0005 |
| T2, women 40–58, log-wage interaction SE | Outcome 1994 | 0.051 | 0.048878340 | 0.0005 |

The first value, `11.3`, was visually confirmed in the supplied proof rather than
inferred from OCR or copied from the legacy repository. Both source and
reproduced values remain in the comparison file. Its cause is unresolved.

The model reconstruction uses conventional homoskedastic WLS covariance, which
matches 67 of 72 printed standard errors at their source precision. The paper
labels standard errors without giving a sufficiently detailed covariance recipe
to establish every computational convention. An independent `stats::lm` fit on
the actual older-male weeks sample reproduces the disputed 1988 SE
`0.586078357` with 146,309 observations and rank 150; it is not a `fixest`-only
calculation. An independent `stats::lm` fit on the actual young-female wage
sample reproduces the disputed 1992 coefficient `-0.009174677` with 157,688
observations and rank 150. Synthetic tests additionally compare the full
interaction model's standard errors against `stats::lm`.

These checks distinguish numerical implementation problems from unresolved
source/reconstruction differences, but do not prove that the proof contains
typographical errors. Original analysis programs, original execution logs, or an
independently obtained final printing could help resolve them. We do not change
sample definitions, variance rules, target values, or tolerances selectively to
force agreement. The overall status is therefore
`reconstruction_discrepancies_reported`, not `benchmarks_verified`.

## Reproduction and artifacts

From the repository root, with the project R environment activated:

```bash
Rscript tests/test-replication.R
Rscript scripts/run_replication.R data/derived/cps.rds results
```

The regular analysis pipeline can call
`replicate_published(prepared$data, run_dir, population_counts = prepared$population_counts)`
to keep reconstruction outputs in the same run as the extension. The function
performs no source-data reads and does not mutate the prepared input.

Outputs are `replication_table1.csv`, `replication_table2.csv`,
`replication_models.csv`, and `replication_benchmarks.csv`. The convenience script
also writes `replication_metadata.rds`, including the prepared-object checksum,
source limitations, timestamp, and actual comparison totals. Model audit rows
record formulas, outcome selection, weights, sample counts, parameter counts,
and covariance conventions. Synthetic unit tests exercise known annual effects,
categorical interactions, independent WLS standard errors, raw versus weighted
population counts, and failure/status semantics; they do not count toward the
272 empirical comparisons.
