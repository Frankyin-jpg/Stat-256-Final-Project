# Support-rule amendment after inspection of the supplied sample

The software draft rejected an analysis whenever any fitted four-cell joint
probability in the disabled-post target population was below 0.01. This was a
conservative software guard chosen before the source data were available. It is
not a theorem about overlap and confounds rare disability-cell prevalence with
the covariate density ratios needed for transport.

After obtaining the actual data, support diagnostics were inspected before the
extension effect estimates were examined. In the main window, the original rule
flags only the disabled-pre cell in the two younger strata. The flagged target
survey-weight shares are 0.198% for men and 0.861% for women; minimum fitted
probabilities are 0.009169 and 0.007609. Neither older stratum is flagged. Across
the main source cells, normalized covariate-density ratios range from 0.460 to
3.370, transport effective sample sizes are at least 1,933, and no observation
accounts for more than 0.2092% of a cell's transported weight.

These checks support retaining the full declared population. The empirical
pipeline therefore uses 0.000001 as a numerical lower bound and keeps the
effective-sample-size minimum of 20. It records the weighted target share below
the original 0.01 threshold, fitted probability minima, density-ratio ranges,
effective sample sizes, and largest normalized weight shares. There is no
observation trimming, probability clipping, or weight capping. The same rule
applies to every stratum, outcome, window, and bootstrap refit.

This is an explicit change from the initial software draft. It was motivated
by relative-support diagnostics, not coefficient signs, significance, or a
desire to reproduce the paper's findings. The generic estimator's conservative
default remains 0.01; the empirical runner passes the documented numerical
bound explicitly. A bounded ratio or high ESS does not establish conditional
parallel trends, stable disability reporting, or a correct nuisance model.
