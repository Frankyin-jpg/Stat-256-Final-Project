# Reading notes from the actual Acemoglu–Angrist paper

These notes were prepared from the retrieved paper, independently of the old
project's prose and numbers. Statements about the authors' findings below are
summaries of **their paper**, not claims that this repository has reproduced
those findings. Source-program reconstruction and empirical results must be
reported separately.

## Source and citation convention

- Local source: `data/raw/extracted/github-release/data_and_paper/Consequences of Employment Protection The Case of.pdf`.
- SHA-256: `62dbb8a88e1a9eefcaca2807fa294f276d17ba7215718cb9a333b80ed1f1c5ec`.
- Text inspected: `/workspace/stat256-environment/downloads/original-paper.txt`,
  with page boundaries retained.
- The PDF is a July 2001 *Journal of Political Economy* proof. It contains 45
  PDF pages, with article page labels such as `Proof 1` and `CHECKED 2`, followed
  by production material. **All page citations in these notes are PDF/proof
  page numbers.** The published citation is JPE 109(5), 915–957; no conversion
  between proof page numbers and published pagination has been assumed.
- The currently inspected extracted bundle contains the PDF and
  `marcps_w.sas7bdat`. It contains **no original analysis programs**. It must not
  be described as a retrieved original-code replication package. Equations,
  table notes, appendix descriptions, metadata, and numerical benchmarks can
  support an independently reconstructed replication; they cannot establish
  line-by-line translation of missing programs.

## Verified measurement and timing

| Question | What the paper actually says | Location | Consequence for this project |
| --- | --- | --- | --- |
| Survey coverage | March CPS 1988–1997 | p. 11, Section IV | Preserve all ten survey years for the main replication. |
| Main age restriction | Ages 21–58 | p. 11; Table 4 note, p. 25 | Age is the survey-year age, as the table note explicitly states. Do not age people back by one year before applying the original restriction. |
| Employment outcome | Weeks worked during the calendar year before the March supplement | p. 11 | Define `outcome_year = survey_year - 1`; outcomes span 1987–1996. |
| Wage outcome | Average weekly earnings constructed from annual supplement earnings | p. 11 | Audit numerator, weeks denominator, zero employment, and earnings trimming separately. |
| Disability measure | Self-reported health/disability that prevents work or limits its kind or amount | p. 11 | It is a work-limitation measure, not a direct indicator of legal ADA coverage or a randomized treatment. |
| Disability timing | The question appears to refer to interview-time status, while also introducing questions on disability income in the previous year | pp. 11–12 | Do not assert that disability was measured before annual employment. The paper itself recognizes the timing ambiguity. |
| Label exceptions | Table 1 and Figures 1, 4, and 5 use survey years; other tables/figures use year of observation, survey year minus one | p. 12 | A single global relabeling of all original output would be wrong. |
| Enactment and implementation | Signed July 1990; effective July 1992; initially employers with at least 25 employees; expansion to at least 15 employees in 1994 | p. 4 | Separate passage, partial implementation, and later employer coverage. |
| Authors' post period | 1993–1996 posttreatment, 1992 transition | p. 15 | Our main post window agrees with the paper. A 1995–1996 sensitivity has a different, later exposure window. |
| CPS redesign | Computer-assisted interviews and main-survey questions changed in 1994; supplement content did not | p. 11; pp. 37–38 | The 1994 survey measures 1993 annual outcomes. The measurement change overlaps the first large male employment decline. |

The proposed pre window of outcome years 1987–1989 uses March surveys
1988–1990, before July 1990 enactment. It is a conservative design choice,
not the original paper's unique baseline definition. The broader 1987–1991
pre window includes outcomes after enactment and therefore needs the stronger
no-anticipation assumption. Document this difference rather than presenting
both windows as mechanically equivalent.

## Weights, eligibility, and data construction

The appendix states that **all estimates use CPS sample weights updated to
1990 Census population counts** (p. 37, Appendix A). Table 1's note also says
the descriptive statistics use 1990 Census control tallies (p. 14). Neither
statement identifies a source variable named `FNLWGT2`. Therefore:

1. Using `FNLWGT2` is not verified solely by reading the article. Check its
   labels, coverage, missing/zero values, relation to other weight fields, and
   ability to reproduce published sample statistics.
2. A division by 100 cannot change relative weighted means or coefficients;
   this is an implementation invariant, not evidence that the correct weight
   field or eligibility universe was selected.
3. The paper uses the 1988 March **rewrite** file, which added firm-size and
   other variables and revised imputation. It excludes the Hispanic oversample
   in every year and a few dozen 1994 households with duplicate identifiers
   that could not enter matched samples (p. 37). Establish whether the provided
   SAS extract already embodies these restrictions. Do not apply them twice,
   and do not assume a positivity restriction on a weight necessarily encodes
   all of them.
4. The main age restriction is 21–58 at interview. The separate matched-sample
   analysis uses ages 22–58 in both years (p. 38). That matched restriction
   must not replace the cross-section restriction.
5. Earnings below $25 or above $2,000 weekly in 1988 dollars are excluded from
   the wage analysis (p. 37). The stated reason includes subannual earnings
   mistakenly collected in the 1994 supplement. The paper does not give the
   exact CPI series/year lookup in this passage. This rule is not a reason
   to remove zero-work observations from the employment analysis.

Published Table 2 employment sample counts provide useful independent checks:

| Sex/age stratum | Employment observations | Wage observations |
| --- | ---: | ---: |
| Men, 21–39 | 193,317 | 167,974 |
| Women, 21–39 | 211,910 | 157,688 |
| Men, 40–58 | 146,309 | 116,665 |
| Women, 40–58 | 157,589 | 109,082 |

These are transcribed from Table 2 (p. 17). They are benchmarks, not results
of the new pipeline. The descriptive Table 1 displays selected survey years;
that presentation does not authorize globally restricting regressions to even
years.

## What the original estimators compare

### Table 2: annual group-gap changes

Equation (6) compares disability groups over time, allowing demographic
effects to vary by year (p. 11). The main controls are disability, year, two
ten-year age groups within each age stratum, schooling categories, three race
groups, nine Census regions, and year interactions with demographic categories
(pp. 12 and 15; Table 2 note, p. 17). A full set of disability-by-year
interactions uses **outcome year 1987 as its omitted baseline** (p. 15).

The trend model is a different restriction. A disability-specific linear
trend cannot coexist with the full unrestricted disability-by-year set; the
paper therefore retains only 1991–1996 disability interactions in its trend
specifications (p. 15). Our extension's 1989 reference year and age-polynomial
model are not a literal Table 2 replication, even if the resulting plot looks
similar.

### Table 3: the paper already performs semiparametric adjustment

Table 3's baseline drops pre-1991 disability interactions, setting the
1987–1990 interactions to zero (pp. 19–20). Column 2 drops demographic
controls. Column 3 allows year-specific and disability-specific covariate
effects, described by equation (7) (p. 20). Without programs, reconstruct this
carefully: the text's covariate-cell notation and the table's interaction
description should not be used to claim that additive category interactions
and a fully saturated joint-cell specification are equivalent. Benchmark
comparisons can help resolve that ambiguity.

Column 4 estimates cell-specific DID regressions for **162 cells**:
two age groups × three race groups × three schooling groups × nine regions.
It averages their disability-year coefficients using the distribution of
characteristics **among disabled respondents in the 1988–1991 CPS** (survey
years, equivalent to outcomes 1987–1990). The target is therefore an explicitly
pre-ADA disabled distribution, not the full sample or disabled-post distribution
(Table 3 note, p. 19; equation (8) and surrounding text, p. 20).

The new project's RA/IPW/AIPW estimator standardizes to an **observed post-period
disabled** target and pools periods differently. Those choices change the
estimand. Its contribution cannot be described as introducing covariate
standardization where the original paper used only regression. Defensible
contributions are a clear comparison of targets and nuisance estimators,
support/balance diagnostics, transparent augmentation, and sensitivity to the
assumptions that adjustment cannot fix. If practical, add a declared pre-target
versus post-target comparison; it is a target-population sensitivity analysis,
not a search for the preferred sign.

### Complementary original designs

The article does substantially more than Tables 2–4:

- Matched March 1993/1994 respondents provide annual outcomes in 1992/1993 and
  allow restrictions to stable disability, valid supplement responses, and
  stable beneficiary status (pp. 21 and 37–40).
- Matched March/April observations describe hiring and separation transitions
  (p. 27). Disability is measured in March; the transition is March to April.
- Employer-size contrasts use the employer worked for longest in the past
  year, grouped 1–24, 25–99, and 100+ employees, with nonworkers as a reference
  category (p. 29). These descriptive comparisons are not randomized employer
  exposure and can involve selection into employer type.
- State charge-rate models are estimated **separately by disability status**
  and use across-state differences, rather than nondisabled people as the
  direct control group. The OLS charge rate is lagged one year (p. 32).
- The IV models instrument contemporaneous charge rates with interactions of
  post-year indicators and strong preexisting state disability laws; the charge
  rate is no longer lagged (pp. 34–35). The appendix explains the 16-state
  coding, including Alaska (p. 41).

Do not imply that our repeated-cross-section checks replicate these designs,
or treat the absence of those extra data/programs as proof they are unnecessary.

## Inference: what is stated and what is not

The paper reports standard errors in parentheses and describes weighted OLS,
but the inspected article does **not** identify a heteroskedastic-robust,
household-clustered, state-clustered, or full survey-design variance convention
for Table 2. There is no basis to label our HC1 intervals as reproducing its
standard errors without an actual numerical comparison and a stated method.

For Table 3's semiparametric column, footnote 8 explicitly says that standard
errors are calculated from the cell-specific estimates' standard errors,
**treating averaging weights as known** (p. 20). Our cell-stratified bootstrap
refits estimated nuisance models and the target mean, so it differs from that
variance calculation. It also resamples individuals, not the actual CPS
dependence structure; it cannot solve uncertainty from one national policy
timeline.

The source documents genuine repeated observations: for the March 1993/1994
match it uses household identifier, person line number, rotation group, and
same-sex/race checks; 70.9% of eligible March 1993 records meet the final
matching definition (p. 37). March/April matching adds the 1960 Census state
code, with 88.6% satisfying the stated final match criteria (p. 38). Inspect
the supplied extract for these identifiers before assuming independent rows.
The paper also says the 1994 household identifier needed a Census correction
(p. 37), so a naive match on any available identifier would not be validated.

## Findings and limitations actually in the paper

The following statements describe the authors' results; they do not determine
what a new analysis must find.

- For young men, Table 2 reports a decline beginning in outcome year 1993;
  the no-trend 1993 interaction is −1.44 weeks (SE 0.69), becoming −2.93
  (0.72) in 1995 (p. 17). For young women, the decline begins in 1992:
  −2.81 (1.02), followed by −4.37 (1.02) in 1993. The authors emphasize
  these timing differences (p. 18).
- Not every pre-period contrast is close to zero or insignificant. Young men's
  1989 interaction is **+2.00 weeks (0.71)** relative to 1987 (Table 2, p. 17).
  It is inaccurate to summarize the table as showing uniformly insignificant
  leads. This matters for our fixed pre-law placebo and choice of reference year.
- Older men's decline is less persistent: the 1993 no-trend interaction is
  −2.12 (0.56), versus −0.75 (0.55) in 1996 (p. 17). The authors find that
  the older-men result is weaker in some transfer-recipient exclusions
  (pp. 25 and 36).
- For older women, the decline appears earlier and disappears or changes sign
  with a disability-specific trend; the authors do not find clear ADA effects
  for this group (p. 18). They then focus later tables on younger women and
  both male age groups (pp. 18–19). Reporting all four groups in our extension
  is preferable to copying that results-based narrowing without explanation.
- The wage columns show no clear robust post-ADA change once group trends
  are included (p. 18). Conditional wages among workers do not by themselves
  establish the absence of wage effects for a stable population.
- The firm-size contrasts are explicitly described as imprecise; the example
  medium-versus-small contrast for young men has a t statistic of about 1.4
  (p. 29). The IV charge-rate estimates are also imprecise, and the young-men
  estimates in later years are positive (p. 35). Do not describe every
  corroborating exercise as uniformly negative or decisive.
- The authors conclude that the ADA is a likely cause, especially for younger
  groups, but acknowledge weaker older-group evidence and lack of direct
  support from separation-rate changes for a pure firing-cost story
  (p. 36). Their theoretical model has ambiguous net employment predictions
  because accommodation/firing costs and the implicit subsidy to hiring work
  in different directions (pp. 6–10). Our estimates cannot settle mechanisms
  merely by matching a negative sign.

## Rival explanations the paper already investigates

**Disability reporting and composition.** The authors explicitly discuss a
possibility that unemployed/nonparticipating people became more likely to
report disability following the ADA or CPS redesign (p. 21). Figure 1 shows
heterogeneous changes in disability prevalence, with particularly noticeable
changes among older women (p. 12). Their matched-data exercise provides
additional evidence but is not uniformly stable: young-men results fluctuate
with sample restrictions and are sensitive in some cases to weighting
(pp. 21 and 38; Table A1, p. 39). Restricting to people with stable reported
disability can itself select a subgroup; it is evidence to assess, not a proof
of stable group membership for the whole population.

**CPS redesign and allocation.** The supplement questionnaire did not change,
but interview mode, main-survey questions, response rates, and allocation could
affect the observations. The paper reports subannual earnings incorrectly
collected in 1994, relatively low supplement noninterview rates in 1994 and
high rates in 1995, and matching restrictions based on valid responses
(pp. 37–38). Our plot of changing demographics cannot substitute for checking
these specific measurement threats.

**SSI/OASDI growth.** The article acknowledges that benefits receipt is both a
cause and consequence of employment. It treats simple exclusions/controls as
limited strategies and refers to separate endogenous-receipt work in its 1998
working paper (p. 22). It also notes that the CPS does not distinguish OASDI
from DI, so not every social-security recipient can be labeled a DI recipient
from that field alone. The authors conclude that transfers do not explain
most younger-group declines, while older-men results are weaker in some
specifications (p. 25). Our primary adjustment should not turn recipient
status into a baseline confounder by default.

**Macroeconomic and state-level conditions.** The paper notes that the early
declines coincide with a cyclical recovery (p. 18, footnote 7), but this is an
argument, not identification. It acknowledges that charge rates can respond
to local conditions or compliance, motivating an IV exercise (p. 34).
Predetermined legal variation would still require its own exclusion and trend
arguments before serving as an instrument in a new project.

## Which proposed extensions are justified?

| Extension | Assessment after reading the source | Honest interpretation |
| --- | --- | --- |
| Common-target RA/IPW/AIPW | Keep, while disclosing its post-target and period-pooling differences from the original | Compare nuisance-model adjustment for a clearly defined group-disparity functional; it is not a new source of policy assignment. |
| Richer schooling/region adjustment | Keep as a same-sample sensitivity; the original already included these domains | Changes with observed composition/functional form, subject to covariate timing and support; no claim of novel identification. |
| Pre-law placebo, 1987 versus 1988–1989 outcomes | Keep the prespecified comparison | An observed-data diagnostic before enactment; a nonzero result challenges simple trend stability, while a near-zero result does not prove parallel counterfactual trends. |
| CDF thresholds at 0, 13, 26, 39, 51 weeks | Keep after validating weeks/missing codes | Locate whether changes concern no annual work, partial-year work, or near-full-year work. Threshold DID is not a quantile effect or an identified hiring/firing mechanism. |
| Exact differential-trend sensitivity | Keep, with an interpretable scale and original pre-period evidence shown | Quantify the drift required to explain a contrast; it does not bound every possible violation or remove redesign/reporting bias. |
| All four sex/age strata | Keep | Avoid inheriting the original's later focus only on groups with the clearest initial effects. |
| Alternative post windows | Keep as declared sensitivity | 1993–1996 agrees with the authors' post period; 1995–1996 examines later coverage but changes the target and policy horizon. |
| Benefits exclusions as a causal fix | Do not use as a primary identification claim | Potential post-treatment selection; any descriptive comparison needs explicit caveats. |
| Firm-size or state-charge IV as an automatic extension | Defer unless the required fields, external sources, and assumptions are verified | These are different designs, not robustness options to add by name. |

The strongest substantive extension is a **transparent diagnosis of where
employment changes occur and how robust their interpretation is to composition
and counterfactual trends**, anchored to a verified replication benchmark.
Support, sensitivity, and the contrast between no-work and partial/full-year
employment can teach something even if a policy ATT remains unidentified.
The extension should not promise that adding a more elaborate estimator will
settle the ADA debate.

## Immediate decisions for the report and software

1. Cite this actual source for timing, sample, original specifications, and
   limitations; remove stale claims that the paper has not been retrieved.
2. State that the available bundle has data and a paper but no original
   programs. Call new models a documented reconstruction or reanalysis until
   benchmark comparisons establish the degree of replication.
3. Confirm `FNLWGT2`, extract eligibility, and source missing-value conventions
   using metadata and independent sample/mean checks. Source timing and the
   age rule are now supported by the paper; field identities are a separate
   verification problem.
4. Keep an exact-replication specification alongside the extension. Do not
   label our age-polynomial/1989-reference graph as the original Table 2.
5. Report all available original pre-period coefficients, including the
   nonzero 1989 young-men contrast, when discussing the placebo or trend
   sensitivity. Do not choose a reference year to make pre-period evidence
   disappear.
6. Acknowledge the original semiparametric target and matched-data checks in
   the contribution and limitations. Our repeated-cross-section augmentation
   does not supersede those designs.
7. Interpret new numerical outputs only after their own provenance, sample
   checks, diagnostic status, and uncertainties have been reviewed.
