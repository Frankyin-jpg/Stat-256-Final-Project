# A reusable structure for a causal inference paper

This template organizes an approximately 15-page main paper plus a technical
appendix. Length is a budget, not a reason to add filler. Let the research
question, design, and amount of credible evidence determine the final balance.
The structure applies beyond the ADA application.

Maintain four separate statements throughout the paper:

| Question | What belongs in the answer |
| --- | --- |
| What is the estimand? | Population, intervention/comparison, outcome, time horizon, averaging distribution |
| Why is it identified? | A causal argument linking the estimand to observable quantities under explicit assumptions |
| How is it estimated? | Sample construction, formulas, nuisance models, weighting, uncertainty, computation |
| What does the evidence show? | Executed estimates, uncertainty, diagnostics, discrepancies, sensitivity, limits |

An estimator's name is not an identifying argument. A small standard error is
not evidence against unmeasured confounding. A successful software test is not
an empirical result.

## Abstract: one compact paragraph

Write the abstract last. State the substantive question, setting and data,
design, principal estimate with units and uncertainty, main identifying
limitation, and restrained implication. Include a numerical result only when
it is generated from the verified analysis. For an incomplete project, state
that the document is a design/protocol and say which evidence is pending; do
not imitate a completed empirical abstract.

## 1. Introduction: about 1.5–2 pages

Explain why the question matters and why its answer is uncertain. Describe the
variation that could distinguish the proposed causal explanation from its
alternatives. State the contribution precisely: replication, a new target,
new evidence, a new comparison, or a sensitivity analysis.

End with a concise account of the design and findings, once findings exist.
Avoid treating a literature summary as the contribution. Distinguish a finding
about an observable contrast from the stronger policy claim it might support.

## 2. Context and related evidence: about 1–1.5 pages

Present the institutional facts needed for identification: eligibility,
timing, implementation, choice or assignment, exposure, spillovers, and other
contemporaneous changes. Cite primary sources for dates and definitions.

Use prior studies to explain disagreements in designs, measurements, or target
populations. Identify what this paper can resolve and what it cannot. An
adversarial reading of the preferred causal story is more useful than a list
of papers with the same sign.

## 3. Data and sample construction: about 1.5–2 pages

Give source identifiers, versions, access conditions, units of observation,
coverage, and measurement timing. Separate survey/interview dates from outcome
reference periods. Describe missingness, exclusions, recodes, inflation or
other transformations, weighting, and the handling of repeated observations.

Include a sample-flow table and a compact descriptive table. Every exclusion
should have a reason and an auditable count. Explain which values are observed,
constructed, imputed, or unavailable. Preserve original data and record hashes.
If replicating a paper, report the reconciliation of its sample and definitions
before interpreting differences in estimates.

## 4. Causal question and identification: about 2 pages

Define the estimand using potential outcomes or another explicit causal
framework. Name the target population and how it differs from the observed
sample. Explain whether treatment is an intervention, assignment, exposure,
eligibility, or a measured proxy.

Derive the observable contrast under the identifying assumptions. For each
assumption, give a substantive justification, a plausible violation, and what
can be diagnosed from the data. A DAG, timeline, or assignment diagram should
clarify the argument rather than decorate the paper. Treat post-treatment
variables and group composition explicitly.

If the assumptions are uncertain, identify the descriptive or associational
functional that the estimator still measures. Do not hide that distinction
until the final paragraph.

## 5. Estimation and inference: about 1.5–2 pages

Give the main estimator and the choices needed to reproduce it: controls,
reference categories, model forms, weights, tuning, sample restrictions,
cross-fitting if used, and target-population normalization. Explain what each
comparison estimator contributes and whether it estimates the same target.

Describe the variance or resampling procedure in terms of the actual sampling
and assignment units. Account for shared observations and nuisance fitting.
State the limits of the uncertainty calculation. Place algebra, derivations,
algorithm details, and implementation tests in the technical appendix, but
keep the identifying assumptions in the main paper.

## 6. Results: about 2–2.5 pages

Lead with the most interpretable outcome comparison, then show the principal
estimate and uncertainty. Interpret scale and substantive magnitude before
discussing statistical significance. Show the full prespecified set of groups
and periods, including null or conflicting results.

In a replication, distinguish reproduced results, documented discrepancies,
and extensions. Explain a discrepancy through sample, measurement, formula,
or variance evidence; do not explain it away because the headline signs match.

Use generated tables and figures with captions that specify population,
outcome, units, reference group, weighting, estimator, and uncertainty. Keep
the prose linked to the displayed evidence. If outputs are absent or fail a
validation gate, the section should state that status rather than contain
placeholder claims of robustness.

## 7. Diagnostics and robustness: about 1.5–2 pages

Organize analyses by the threat they address rather than by the number of
specifications tried. Examples include support and balance, pre-period
patterns, timing, selection or missingness, alternative outcome definitions,
and an interpretable sensitivity model for a key identifying restriction.

State which analyses retain the same estimand and which change the target.
Report failed support/model checks and why a result is unavailable. Separate
pointwise comparisons from simultaneous evidence. More regressions with the
same sign do not replace an assessment of whether a common bias explains them.

## 8. Discussion: about 1 page

Interpret the evidence against the competing substantive explanations from
the introduction. Explain which inference survives the sensitivity analysis
and which relies on stronger assumptions. Discuss measurement, selection,
external validity, and the distinction between the sampled population and
the policy-relevant population.

Identify what additional data or variation would resolve the main remaining
uncertainty. Avoid unsupported mechanisms: an employment contrast alone does
not identify whether costs, discrimination, labor supply, or reporting caused it.

## 9. Conclusion: about half a page

Answer the question at the strongest level supported by the design and executed
evidence. State the principal limitation with its consequence for interpretation.
Do not introduce a new estimator, claim, or policy recommendation unsupported
by the paper. If the work is incomplete, specify the concrete remaining gates.

## Appendix map

| Appendix component | Purpose |
| --- | --- |
| Data and source crosswalk | Show each raw field, construction, source evidence, missingness, and verification status |
| Estimand derivation | Connect assumptions to the observable functional and expose where each assumption enters |
| Estimator derivation | Specify weights/normalization and prove claimed properties under stated conditions |
| Uncertainty algorithm | Identify resampling units, nuisance refitting, seeds, replicate counts, and failed draws |
| Additional results | Present the complete prespecified diagnostics and specifications without selective reporting |
| Reproduction instructions | Give exact commands, dependencies, source/version identifiers, and artifact boundaries |
| Software checks | Describe tests of meaningful failure modes, clearly separated from empirical evidence |

Keep the main paper readable without opening the appendix. Conversely, the
appendix should enable an informed reader to audit the implementation rather
than repeat the introduction in more words.

## Before calling the paper complete

- Verify source provenance and the substantive meaning of every analyzed field.
- Reproduce the full analysis from a clean checkout with documented data access.
- Ensure every reported number and figure comes from the recorded empirical run.
- Distinguish replication benchmarks, new estimates, synthetic tests, and unrun work.
- Review the estimand, identifying argument, uncertainty, and causal wording together.
- Report unresolved failures and deviations from the original plan, with reasons.
