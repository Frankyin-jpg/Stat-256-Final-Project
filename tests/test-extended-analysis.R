# Deterministic simulated design: this tests estimator/pipeline behavior, not
# the empirical CPS study or the policy assumptions needed for causal claims.
source("R/prepare_data.R")
source("R/event_study.R")
source("R/estimate_did.R")
source("R/extended_analysis.R")

fixture <- expand.grid(YEAR = 88:97, SEX = 1:2, age_sample = c("young", "old"),
  DISABL1 = 0:1, age_index = 1:3, RACE = 1:3, education_index = 1:3,
  REGION = 1:3, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
n <- nrow(fixture)
fixture$AGE <- ifelse(fixture$age_sample == "young",
                       c(23, 29, 37)[fixture$age_index], c(43, 49, 57)[fixture$age_index])
# Covariate-dependent positive weights have the same distribution in every
# group/year. Age has three support points, identifying the quadratic design.
fixture$FNLWGT2 <- 100 + 10 * fixture$RACE + 5 * fixture$REGION
fixture$LESSHS <- as.numeric(fixture$education_index == 1)
fixture$HSGRAD <- as.numeric(fixture$education_index == 2)
fixture$SOMECO <- as.numeric(fixture$education_index == 3)
fixture$COLGRAD <- 0
base_weeks <- c(0, 26, 52)[fixture$age_index]
policy <- fixture$DISABL1 == 1 & fixture$YEAR - 1 >= 93
fixture$WKSWORK <- pmax(0, base_weeks - 13 * policy)
fixture$WSAL_VAL <- 500 * fixture$WKSWORK
prepared <- prepare_cps(fixture)
data <- prepared$data
# Create genuine common-sample loss for rich models while leaving every
# covariate level, age point, group, and year represented in the retained data.
missing_education <- fixture$REGION == 3 & fixture$education_index == 3
 data$education[missing_education] <- NA
before <- data
result <- run_extended_analyses(data)
stopifnot(identical(data, before), n == 6480L,
          identical(names(result), c("placebo", "distributional", "rich", "diagnostics", "failures")),
          nrow(result$failures) == 0L,
          nrow(result$placebo) == 32L, nrow(result$distributional) == 80L,
          nrow(result$rich) == 64L, nrow(result$diagnostics) == 44L)
methods <- c("unadjusted_did", "aipw_standardized_did",
             "outcome_standardized_did", "ipw_standardized_did")
stopifnot(setequal(result$placebo$method, methods),
          setequal(result$placebo$sex, c("male", "female")),
          setequal(result$placebo$age_sample, c("young", "old")),
          all(result$placebo$window == "pre_policy"),
          all(abs(result$placebo$estimate) < 1e-7))

# The treated distribution moves from {0,26,52} to {0,13,39}, each at mass 1/3;
# controls never change. These are known CDF-DID changes, not quantile effects.
expected_cdf <- c("0" = 0, "13" = 1/3, "26" = 0, "39" = 1/3, "51" = 1/3)
stopifnot(all(result$distributional$outcome == "cdf_weeks"),
          setequal(result$distributional$threshold, c(0, 13, 26, 39, 51)),
          max(abs(result$distributional$estimate -
                    expected_cdf[as.character(result$distributional$threshold)])) < 1e-6)
expected_rich <- ifelse(result$rich$outcome == "weeks", -26/3, 0)
stopifnot(max(abs(result$rich$estimate - expected_rich)) < 1e-6,
          setequal(result$rich$covariate_set, c("primary_same_sample", "rich")))
primary <- result$rich[result$rich$covariate_set == "primary_same_sample", ]
rich <- result$rich[result$rich$covariate_set == "rich", ]
paired <- merge(primary, rich,
  by = c("sex", "age_sample", "outcome", "window", "method"), suffixes = c("_primary", "_rich"))
stopifnot(nrow(paired) == 32L,
          all(paired$n_stratum_primary == paired$n_stratum_rich),
          all(paired$n_complete_primary == paired$n_complete_rich),
          all(paired$n_window_primary == paired$n_window_rich),
          all(paired$n_stratum_primary == 1620L),
          all(paired$n_complete_primary == 1440L),
          all(paired$n_window_primary == 1008L),
          max(abs(paired$estimate_primary - paired$estimate_rich)) < 1e-6)
diagnostics <- result$diagnostics
stopifnot(all(diagnostics$min_transport_ess >= 20),
          all(diagnostics$min_probability_in_target >= .01),
          all(diagnostics$excluded_complete >= 0),
          all(diagnostics$n_window + diagnostics$excluded_outside_window == diagnostics$n_complete),
          all(diagnostics$n_complete + diagnostics$excluded_complete == diagnostics$n_stratum),
          all(diagnostics$excluded_complete[diagnostics$analysis == "rich"] == 180L),
          all(diagnostics$excluded_complete[diagnostics$analysis != "rich"] == 0L))

# Every scenario requires 1987. Missing a required year must yield explicit
# failures, not estimates from a narrower window or a fabricated replacement.
missing_year <- run_extended_analyses(data[data$outcome_year != 1987, ])
stopifnot(nrow(missing_year$placebo) == 0L,
          nrow(missing_year$distributional) == 0L,
          nrow(missing_year$rich) == 0L, nrow(missing_year$diagnostics) == 0L,
          nrow(missing_year$failures) == 44L,
          setequal(missing_year$failures$analysis, c("placebo", "distribution", "rich")),
          all(missing_year$failures$stage == "pooling"),
          all(grepl("Required outcome years are absent", missing_year$failures$reason, fixed = TRUE)))
cat("PASS: prepolicy null, known CDF-DID and mean effects, shared rich-model samples, counted exclusions, and explicit missing-year failures on simulated data.\n")
