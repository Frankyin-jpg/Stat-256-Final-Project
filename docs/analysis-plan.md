# Analysis plan: replication and a causal inference extension

Status: a prespecified plan, not an empirical result. The historical repository
does not contain the raw dataset. Its tables and prose are not accepted as
verified results. This plan should be reconciled with the original archive,
programs, variable documentation, and published paper before estimates are
interpreted.

The methodological reference is provisionally Peng Ding, *A First Course in
Causal Inference*. The connections are potential outcomes, identification versus
estimation, standardization, propensity weighting, augmented estimators, and
sensitivity to identifying assumptions. No chapter or page references are
claimed without checking the user's intended edition.

## Research question and deliverables

How did the employment gap between people reporting a work-limiting disability
and other working-age people change around the introduction of the Americans
with Disabilities Act (ADA)? How sensitive is that contrast to observable
composition and deviations from parallel trends?

Produce three distinct products:

1. A faithful replication of selected specifications in Acemoglu and Angrist
   (2001), with discrepancies against the original programs and paper reported.
2. A transparent repeated-cross-section difference-in-differences (DID)
   extension with common estimands for regression adjustment, weighting, and
   augmentation.
3. Diagnostics and sensitivity analyses that determine how strongly a policy
   interpretation is supported. Agreement between estimators alone does not
   establish identification.

The archive referenced by the repository is Harvard Dataverse DOI
`10.7910/DVN/WCNKB1`. Its contents, license, download checksums, and original
program language must be recorded from the actual archive. Never substitute
synthetic observations for empirical evidence or silently transcribe historical
LaTeX numbers into new result tables.

## Population, measurement, and time

The starting population is ages 21–58 in the March CPS files used by the original
authors. Preserve the paper's sex and age strata: men/women, ages 21–39/40–58.
The meaning and comparability of these historical recorded categories must be
documented. Report all four groups, not only groups with favorable estimates.

Keep `survey_year` and `outcome_year` as separate variables. Annual weeks and
earnings normally refer to the previous calendar year; verify this from archive
documentation and original programs. Never relabel a raw two-digit `YEAR` as an
outcome year without evidence. Disability status may be measured at interview
and may not correspond to disability throughout the outcome year.

Confirm the legal dates from an authoritative source before final publication:
ADA passage in 1990, Title I implementation for larger employers in July 1992,
and the later employer-size expansion in July 1994. These imply anticipation,
partial-year exposure, and phased coverage. A universal sharp switch in 1992
would be an oversimplification.

Provisional extension windows, expressed in **outcome years**:

| Analysis | Pre window | Post window | Purpose |
| --- | --- | --- | --- |
| Main two-period contrast | 1987–1989 | 1993–1996 | Exclude passage/transition years from the pre comparison |
| Later coverage sensitivity | 1987–1989 | 1995–1996 | Examine a period after the later implementation phase |
| Conventional pre-period sensitivity | 1987–1991 | 1993–1996 | Show dependence on allowing potentially anticipatory pre years |
| Annual event study | Available annual observations | Available annual observations | Display the entire time path, without treating all dates alike |

These windows require the archive to contain the stated outcome years. Fail
clearly if required years are absent; do not silently shift windows. Preserve
the original paper's timing separately for its replication. Freeze any change to
the extension windows before inspecting the extension's estimated signs.

Primary outcome: annual weeks worked, including zero, with its valid range
verified. Secondary employment outcome: any positive weeks worked. Positive
weekly earnings and log wages are secondary descriptive analyses conditional
on employment; employment selection may itself be affected by policy. An
unconditional annual earnings outcome may be useful if the archive's income
definition, zero values, and top coding can be verified.

Preserve survey weights and inspect missing, zero, negative, and extreme values.
Record the original definition of `FNLWGT2` rather than inferring it from its
name. Multiplying all weights by a constant must leave point estimates
unchanged. Pooling years requires a declared target: use equal total survey
weight per year within each analysis period so arbitrary sample-size changes do
not set year importance. Report this choice and retain unnormalized original
weights for strict replication.

## Estimand and identification

Let `G` denote disability group, `T` the pre/post period, `X` baseline or
plausibly predetermined covariates, and `Y` the outcome. Let
`m_gt(x) = E[Y | G=g, T=t, X=x]`. A transparent statistical target is the
standardized contrast in the observed disabled-post population:

```text
theta = E[ m_11(X) - m_10(X) - m_01(X) + m_00(X) | G=1, T=1 ].
```

This is a defined adjusted DID contrast even when a causal interpretation is
not warranted. It is not the average effect of becoming disabled, and disability
is not the intervention. The intervention of interest is the policy regime.

For an ATT interpretation, consider a stable pre-policy group `G*` and potential
outcomes under the ADA and a no-ADA policy regime. The required assumptions
include:

- Conditional parallel trends in **untreated outcomes** for the stable groups.
- No anticipatory effects in the chosen pre window.
- A comparison group whose outcome trend provides the relevant no-policy
  counterfactual, including restrictions on spillovers and general-equilibrium
  effects.
- Repeated-cross-section sampling and stable group composition, or enough
  measured, predetermined covariates to justify the stated standardization.
- Common support for all four group-period cells in the target population.
- Comparable measurement over time and no policy-induced change in how the
  population or disability groups are recorded.

Observed self-reported `G` may change because of labor-market outcomes,
application for benefits, or policy itself. A work-limitation measure can be
endogenous to the employment outcome. Repeated cross sections do not identify
each person's pre-policy disability group. The dataset may therefore support
an adjusted change in group disparities more credibly than the stable-group
ATT. State this distinction in the abstract and conclusion, not only in a
footnote.

A useful causal graph should include underlying health, employment, reporting
of work limitation, benefits, policy, and demographics. Benefits receipt,
current employment, and wages must not enter the main propensity model as if
they were baseline confounders. Current education, region, or marital status
also require care because they may change after policy. Start with a parsimonious
set of age/cohort, recorded sex, and recorded race categories; treat richer
education/geography adjustment as an explicitly assumption-dependent analysis.

## Replication before extension

Read the original programs before declaring any model a replication. Preserve
their sample exclusions, inflation series, earnings trimming, weights,
covariate coding, omitted categories, year labels, and variance calculations.
Distinguish exact replication from a modernized specification.

For each selected table/specification, save:

- Sample counts and total survey weights at every exclusion step.
- Weighted outcome means by year, disability, sex, and age group.
- The exact formula, reference period, and missing-data rule.
- Coefficients, standard errors, and observation counts from executed R code.
- A comparison against the published or original-program benchmark with its
  provenance, numerical difference, and justified rounding tolerance.

Do not call the replication successful just because signs match. If an original
program cannot run without proprietary software, translate it with explicit
line/function mapping and test intermediate calculations. Describe the
remaining limitation accurately.

## Annual event study

Plot weighted annual means and the annual disability gap before estimating
regressions. A provisional regression is:

```text
Y = year fixed effects + disability group + predetermined covariates
    + disability × year interactions (one explicitly omitted year).
```

The strictly replicated model may include year-by-demographic interactions;
the extension should make its own functional-form choices explicit. Use an
outcome-year reference period before passage, provisionally 1989, while
retaining the paper's own reference year for replication. Report the reference
period in every figure caption.

Lead coefficients are diagnostics of observed pre-period differences. A
failure to reject a pre-trend test is not evidence that parallel trends holds.
Few pre years imply low power. Report intervals and substantive magnitudes,
not just a joint p-value. This is a national policy comparison with one policy
timeline, not a staggered-adoption design; modern staggered-adoption estimators
do not solve its identification problem.

## Standardization, IPW, and AIPW extension

Use the same target population, outcome, covariates, period windows, and
missing-data sample for all three estimators. A weighting-plus-regression fit
is not automatically doubly robust.

A transparent four-cell implementation avoids accidentally applying a panel
estimator to repeated cross sections. Let `A=(G,T)`, `a*=11`, and
`q_a(x)=P(A=a|X=x)`. Fit a multinomial cell-membership model and cell-specific
outcome regressions. Let `s_i` be the chosen survey/year analysis weight and
`S*=sum_i s_i I(A_i=11)`. For a non-target cell `a`, estimate its mean standardized
to the target covariate distribution using:

```text
mu_a_RA = sum_i s_i I(A_i=11) m_a_hat(X_i) / S*

mu_a_IPW = sum_i s_i I(A_i=a) [q_11_hat(X_i)/q_a_hat(X_i)] Y_i
           / sum_i s_i I(A_i=a) [q_11_hat(X_i)/q_a_hat(X_i)]

mu_a_AIPW = mu_a_RA
            + sum_i s_i I(A_i=a) [q_11_hat(X_i)/q_a_hat(X_i)]
                    [Y_i - m_a_hat(X_i)] / S*
```

Set `mu_11` to the survey-weighted observed target mean. For each method,
`theta = mu_11 - mu_10 - mu_01 + mu_00`. The IPW formula above uses Hájek
normalization; its finite-sample form differs from the AIPW residual term's
target-mass normalization. Preserve that distinction in code and tests.

For each transported mean, consistency of AIPW requires either the
corresponding outcome regression or the corresponding density ratio to be
correct, plus support and regularity conditions. For the entire contrast,
those conditions must hold for all three transported cells. This protects
against nuisance-model misspecification under the stated conditions; it does
not protect against violations of parallel trends, endogenous disability
reporting, or post-treatment covariates.

Use cross-fitting if flexible nuisance models are introduced. Split at the
largest available dependence unit and ensure each training set contains all
four cells and supported categories. Cross-fitting is not a substitute for
regularization, support, or valid uncertainty calculations. A simple initial
implementation can use prespecified parametric nuisance models with a bootstrap
that refits both nuisance stages.

## Overlap, balance, and stability

For each comparison cell, report estimated cell probabilities and density
ratios, weighted covariate balance against the disabled-post target, maximum
weights, weight concentration, and effective sample size
`(sum w)^2/sum(w^2)`. Plot both unweighted and reweighted covariate distributions.
Use a common target-based standardization for balance measures, and report
category proportions for discrete covariates.

Predetermine a minimum-support sensitivity analysis, such as requiring all
estimated cell probabilities to exceed 0.01 and then 0.025. These are diagnostic
choices, not magic validity thresholds. Trim the target and all source cells
by the same support rule, refit nuisance models, and state that the estimand has
changed to the retained population. Never silently clip extreme weights and
continue to label the estimate as the full-population ATT. Report how much
weighted target mass is lost. If a target covariate category is absent in a
source cell, fail or explicitly restrict the target; do not invent predictions
for an unidentified cell.

## Inference

First inspect the archive for household, person, state, PSU, stratum, and
replicate-weight identifiers. CPS rotation can induce dependence across survey
years. Use documented survey-design information when available, and separately
report the original paper's variance convention for replication.

For extension uncertainty, use a dependence-aware bootstrap or appropriate
sandwich estimator that includes nuisance estimation. Refit all nuisance
models in each bootstrap replicate. A state-cluster bootstrap, if reliable
state identifiers exist, preserves observations across all years within a
resampled state; it addresses within-state dependence but does not remove
nationwide disability-specific shocks. Few clusters need explicit caution.

If design and dependence identifiers are unavailable, label individual-level
HC or bootstrap intervals as conditional sampling approximations, not
design-correct or policy-assignment uncertainty. Millions of respondents do
not create millions of independent policy assignments. Ordinary individual
standard errors cannot resolve the single national policy timeline.

Report point estimates and confidence intervals for all prespecified strata.
Use a joint event-study assessment or simultaneous intervals when making a
claim about the entire dynamic path; do not select individual significant
years. Record bootstrap seeds, replicate counts, and failed replicates.

## Sensitivity beyond estimator comparison

1. **Timing:** use the declared alternative windows, make anticipation and
   partial exposure explicit, and plot the full annual path.
2. **Composition:** show weighted disability prevalence and predetermined
   covariate distributions by year. These can flag changing group membership
   even when outcome regression estimates appear stable.
3. **Parallel-trend violations:** define `b` as an additional untreated annual
   change in the disabled-minus-comparison gap. For a pooled contrast and a
   specified effective pre/post separation `Delta`, display
   `theta(b)=theta_hat-b*Delta` over a prespecified, interpretable range in weeks
   per year. Calculate `Delta` from the declared period/year weights; this is
   a particular linear-bias sensitivity model, not a general robustness bound.
   Show the slope needed to move the estimate to zero, and compare its magnitude
   with observed pre-period gap changes without treating those changes as a
   proven bound on counterfactual trends.
4. **Outcome definition:** compare weeks worked and any work. Treat conditional
   wage estimates separately because selection into work can change.
5. **Covariate specification and support:** compare parsimonious and richer
   adjustment, with overlap diagnostics and any target-population changes.

Do not add benefit-recipient exclusions, state legal variation, or an IV design
as automatic fixes. Benefits receipt can be a mediator/collider; legal
variation and proposed instruments need their own provenance and identifying
argument. Such analyses can be additional projects once their data and
assumptions have been established.

## Reproducibility and reporting gates

All result tables, plots, sample counts, and substantive numerical statements
must be generated from executed code. Keep source data immutable, record
checksums and source metadata, and retain original programs in a clearly marked
source directory. Keep synthetic test fixtures and demonstration outputs in
separate paths with explicit labels.

Tests should catch changes that matter: survey/outcome-year mapping, sample
restrictions, zero employment, missing/invalid weights, factor reference
levels, weight-scale invariance, the four-cell estimator under a known design,
unsupported cells, and nuisance refitting in resampling. An artificial-data
test verifies implementation, not the real-world identifying assumptions.

The final report should contain the question, data provenance and sample flow,
replication comparison, estimand and assumptions, executed extension results,
diagnostics and sensitivity, and a conclusion proportional to the evidence.
If data access or faithful replication remains unresolved, publish the plan
and working pipeline with an explicit incomplete status; do not supply a
plausible-looking empirical conclusion.
