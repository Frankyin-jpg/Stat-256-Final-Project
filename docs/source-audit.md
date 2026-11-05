# Audit of the supplied source data

## What was acquired

The repository author's `replication-data-v1` GitHub release supplies
`data_and_paper.zip`. It contains `marcps_w.sas7bdat` and a PDF of the original
Acemoglu–Angrist paper, plus macOS archive metadata. **It contains no original SAS
programs.** This is an acquisition of the supplied release, not a verified complete
copy of the Harvard Dataverse deposit linked by the historical README.

| Artifact | Bytes | SHA-256 |
| --- | ---: | --- |
| Supplied ZIP | 91,436,493 | `a7479a211d034f8fb9209c03435b9be5742f690924c5880cd1a855d5040bc0e4` |
| SAS data | 1,136,444,416 | `1f5a66450f5307dfc8ed3523abdd0777cb1c13a77d81fee3f6fa152db2560a6d` |
| Original-paper PDF | 674,637 | `62dbb8a88e1a9eefcaca2807fa294f276d17ba7215718cb9a333b80ed1f1c5ec` |

The SAS file MD5 is `98b2459432541e95839e4b19d720f125`. Its MD5 and the SHA-256 of
`data/raw/manifest.json` bind `data/source-verification.json` to the exact acquired
input. The checksums prevent unnoticed substitution; they do not establish an
independent chain of custody back to the complete Dataverse deposit.

The PDF is a checked proof headed **Monday Jul 02 2001 09:44 AM, JPE v109n5,
019509 VML**. Its numbered `CHECKED` pages are the reference for the benchmark
transcriptions. It must not be described as an independently retrieved final
publisher PDF. Source data and the supplied PDF are preserved unchanged.

No separate license or terms document is present in this supplied ZIP. The
release's acquisition catalog records that scope rather than inventing a license
for the third-party source material. The repository's code license is separate.

## Field and calendar checks

The SAS file has **971,058 rows and 145 columns**. Only the fields needed by the
analysis are loaded by `read_cps()`, avoiding repeated allocation of the complete
145-column frame. Inspection was performed with Haven's column selection.

| Source field | Observed coding and interpretation |
| --- | --- |
| `YEAR` | Codes 88–97; SAS label is “year of survey.” Converted to 1988–1997. |
| `WKSWORK` | 0–52, no missing values; label is “number of weeks worked last year.” |
| `AGE` | 17–69 in the supplied extract; analysis restricts to 21–58, inclusive. |
| `SEX` | Codes 1/2, no missing values; male/female strata reproduce the paper's sample counts. |
| `DISABL1` | Binary 0/1, no missing values; exactly equals `DIS_HP == 1` in all supplied rows. |
| `DIS_HP` | Codes 1/2; label is “has health problem?” |
| `RACE` | Codes 1–5, no missing values; the historical specification groups 3–5 together. |
| `LESSHS`, `HSGRAD`, `SOMECO`, `COLGRAD` | Binary, complete, mutually exclusive and exhaustive: every row sums to one. |
| `REGION` | Codes 1–9, no missing values; nine Census divisions. |
| `FNLWGT2` | Nonnegative or missing; strictly positive values define the weighted estimation frame. |
| `WORKING` | Binary, complete; label is “lfsr 1(working) or 2(w/job, not at work).” This is current status, not annual weeks. |
| `SS_YN`, `SSI_YN` | Codes 1/2, complete. Optional canonical variables retain receipt indicators without identifying SS receipt as DI. |
| `WSAL_VAL` | Nonnegative wage/salary amounts. Weekly wage construction and trimming match the paper's four wage-model sample sizes. |

The paper's data section (checked proof pp. 11–12) explicitly states that annual
weeks and earnings cover **1987–1996**, while interviews occur in March
1988–1997. Therefore `outcome_year = survey_year - 1` is required. The paper also
states that Table 1 and Figures 1, 4 and 5 use survey-year labels; the remaining
tables and figures use observation-year labels. These two calendars are retained
separately in the new code.

Disability status is observed at the March interview, while annual outcomes refer
to the previous year. The paper itself notes the ambiguity in the status question
as a lead-in to prior-year disability-income questions. The timing check does not
make disability a predetermined treatment variable or eliminate composition
concerns.

The three education groups used in the historical regressions are less than high
school, high-school graduate, and some college or college graduate. This grouping
is also consistent with the paper's Table 3 description of 162 cells
(2 age × 3 race × 3 education × 9 region). The phrase “three schooling dummies”
in the prose is not used to introduce a fourth group when the explicit cell
specification and numerical comparisons support three groups.

## Population counts and weights

Age eligibility retains **759,771 records** and excludes 211,287. Excluding
20,404 missing and 30,242 zero `FNLWGT2` values leaves **709,125 records** in the
weighted estimation frame. There are no missing disability, sex, weeks, or
analysis covariates in that frame. The weight exclusion is substantial and is
reported explicitly; it is not hidden in model fitting.

| Survey year | Ages 21–58 | Missing weight | Zero weight | Weighted estimation rows |
| --- | ---: | ---: | ---: | ---: |
| 1988 | 78,730 | 0 | 3,976 | 74,754 |
| 1989 | 73,028 | 0 | 3,604 | 69,424 |
| 1990 | 80,285 | 4,734 | 0 | 75,551 |
| 1991 | 80,721 | 5,101 | 0 | 75,620 |
| 1992 | 79,959 | 5,237 | 0 | 74,722 |
| 1993 | 79,498 | 5,332 | 0 | 74,166 |
| 1994 | 76,699 | 0 | 6,611 | 70,088 |
| 1995 | 76,711 | 0 | 5,469 | 71,242 |
| 1996 | 66,619 | 0 | 5,160 | 61,459 |
| 1997 | 67,521 | 0 | 5,422 | 62,099 |

The paper's Appendix A (checked proof p. 37) says that all estimates use CPS
weights updated to 1990 Census population counts and that its extract excludes
the Hispanic oversample. The supplied `FNLWGT2` differs from `FNLWGT` in 1990–1992.
The source weight-construction programs are absent, so the precise historical
reweighting and the reason for each zero/missing weight are not reconstructed or
asserted. The retained weight variable is supported by its published sample-count
and numerical-estimate agreement. Dividing it by 100 is a normalization that
leaves weighted means, regression estimates and effective sample sizes unchanged;
population-total interpretations of its absolute scale are not used here.

A separate distinction matters for **Table 1 observation counts**: those printed
counts include age-eligible rows with zero/missing weights. Its weighted means
use the eligible estimation weights. For example, the 1988 young-men raw counts
are 1,059 disabled and 21,532 nondisabled, exactly as printed; the corresponding
positive-weight counts are 998 and 20,393. The API returns
`prepared$population_counts` for the unweighted population count and
`prepared$data` for estimation. It never puts zero/missing weights into the
estimating equations to force an observation count.

## Independent sample-size checks and limits

All eight Table 2 sample sizes in the supplied checked proof p. 17 agree exactly:

| Stratum | Weeks model | Wage model |
| --- | ---: | ---: |
| Men 21–39 | 193,317 | 167,974 |
| Women 21–39 | 211,910 | 157,688 |
| Men 40–58 | 146,309 | 116,665 |
| Women 40–58 | 157,589 | 109,082 |

The wage samples use positive wages/weeks and the historical CPI/trimming rule.
The paper specifies a $25–$2,000 weekly wage range in 1988 dollars (Appendix C,
checked proof p. 37). Its original CPI implementation is not present. Fields
named `legacy_*` preserve that construction for explicit numerical comparison;
matching sample sizes alone does not prove that every wage transformation is
identical. Negative sentinel values in the unused source `AVGWKLY` and `ERN_VAL`
fields are another reason not to substitute those columns without a separate
definition audit.

`data/source-verification.json` therefore uses
**`publication_and_source_labels_verified`**, not an original-program verification
label. Numerical coefficient/standard-error comparisons are generated separately
in the replication outputs, with discrepancies retained. This source audit does
not certify all historical analyses, accessions/separations, matched samples,
original SAS code, or every table in the paper. It also does not justify a causal
interpretation of the new estimates without the separately stated assumptions.
