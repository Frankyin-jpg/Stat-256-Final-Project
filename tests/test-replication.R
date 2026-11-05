# Offline design and comparator checks. Fixture estimates are never empirical
# benchmark matches; the real comparison is produced by run_replication.R.
source("R/replication.R")
grid <- expand.grid(outcome_year = 1987:1996, sex = c("male", "female"),
  age_sample = c("young", "old"), disabled = 0:1, age_index = 1:2,
  race = 1:3, education = 1:3, region = 1:2, replicate = 1:2,
  KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
grid$survey_year <- grid$outcome_year + 1L
grid$age <- ifelse(grid$age_sample == "young", c(25, 35)[grid$age_index], c(45, 55)[grid$age_index])
grid$age_group <- factor(10 * floor(grid$age / 10))
grid$weight <- 1 + grid$race / 10 + grid$region / 20
trend <- grid$outcome_year - 1987
noise <- ifelse(grid$replicate == 1, -.2, .2)
# Nonlinear race-category effects changing by year expose accidental numeric
# slopes in place of full categorical year-by-race controls.
grid$weeks <- 25 + grid$age_index + c(0, 3, -2)[grid$race] * (1 + trend/10) +
  c(0, 1, -1)[grid$education] + grid$region * trend/10 +
  grid$disabled * (-4 - .3 * trend) + noise
grid$log_nominal_weekly_wage <- 5 + grid$age_index/10 +
  c(0, .1, -.2)[grid$race] * (1 + trend/10) + grid$education/20 +
  grid$disabled * (-.2 - .03 * trend) + noise/20
for (name in c("race", "education", "region")) grid[[name]] <- factor(grid[[name]])
grid$legacy_wage_eligible <- TRUE
# Include a known raw population larger than the positive-weight fit sample,
# reproducing the necessary distinction for Table 1 observation counts.
counts <- aggregate(rep(1L, nrow(grid)), grid[c("survey_year", "sex", "age_sample", "disabled")], sum)
names(counts)[names(counts) == "x"] <- "n"
counts$n <- counts$n + 7L
original <- grid
output <- tempfile("replication-test-")
dir.create(output)
result <- replicate_published(grid, output, population_counts = counts, nthreads = 1L)
stopifnot(identical(grid, original), nrow(result$table1) == 120L,
          nrow(result$table2) == 152L, nrow(result$model_audit) == 8L,
          nrow(result$benchmarks) == 272L,
          result$status == "reconstruction_discrepancies_reported",
          all(file.exists(file.path(output, c("replication_table1.csv", "replication_table2.csv",
            "replication_models.csv", "replication_benchmarks.csv")))))
coefficients <- subset(result$table2, statistic == "coefficient")
expected <- ifelse(coefficients$outcome == "weeks", -.3, -.03) * (coefficients$year - 1987)
stopifnot(max(abs(coefficients$reproduced - expected)) < 1e-9)
# Independently fit one model with stats::lm to check both design and the
# conventional WLS covariance/degrees of freedom, not just point coefficients.
sample <- droplevels(subset(grid, sex == "male" & age_sample == "young"))
reference <- lm(weeks ~ disabled * factor(outcome_year) + factor(outcome_year) *
                  (age_group + race + education + region), data = sample, weights = weight)
se <- summary(reference)$coefficients[paste0("disabled:factor(outcome_year)", 1988:1996), "Std. Error"]
observed <- subset(result$table2, sex == "male" & age_sample == "young" &
                     outcome == "weeks" & statistic == "std_error")
stopifnot(max(abs(observed$reproduced - se)) < 1e-10,
          all(subset(result$table1, statistic == "n")$reproduced == 79L),
          all(subset(result$table1, statistic == "n")$count_source ==
                "age_eligible_raw_records_before_weight_exclusion"))
# Rounding tolerances come from the transcribed precision, with exact N checks.
targets <- published_replication_targets()
stopifnot(all(targets$tolerance[targets$statistic == "n"] == 0),
          targets$target[targets$table == "Table 1" & targets$sex == "female" &
            targets$age_sample == "old" & targets$outcome == "weeks" & targets$year == 1996 &
            targets$disabled == 1] == 11.3)
keys <- c("table", "sex", "age_sample", "outcome", "statistic", "year", "disabled")
observed <- targets[keys]
observed$reproduced <- targets$target
comparison <- compare_replication_benchmarks(observed)
stopifnot(all(comparison$pass), identical(comparison$passed, comparison$pass),
          identical(comparison$original_value, comparison$target),
          identical(comparison$reproduced_value, comparison$reproduced),
          !anyDuplicated(comparison$benchmark_id))
observed$reproduced[1] <- targets$target[1] + 2 * targets$tolerance[1]
observed$reproduced[2] <- NA_real_
comparison <- compare_replication_benchmarks(observed)
stopifnot(comparison$status[1] == "discrepancy", comparison$status[2] == "unrun",
          sum(comparison$pass) == 270L)
unlink(output, recursive = TRUE)
cat("PASS: documented full-interaction reconstruction, independent WLS standard errors, raw-versus-weighted sample counts, strict source precision, and honest discrepancy statuses.\n")
