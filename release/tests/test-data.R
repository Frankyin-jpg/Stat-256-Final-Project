source("R/prepare_data.R")
expect_error <- function(expr, text) {
  result <- tryCatch({ force(expr); NULL }, error = identity)
  stopifnot(inherits(result, "error"), grepl(text, conditionMessage(result), fixed = TRUE))
}
fixture <- function() data.frame(
  YEAR = c(88, 89, 90, 91, 92, 93), AGE = c(21, 39, 40, 58, 30, 50),
  SEX = c(1, 2, 1, 2, 1, 2), DISABL1 = c(0, 1, 1, 0, 0, 1),
  FNLWGT2 = c(100, 200, 300, 400, 500, 600), WKSWORK = c(0, 10, 52, NA, 20, 50),
  RACE = c(1, 2, 3, 4, 1, 2), LESSHS = c(1, 0, 0, 0, 1, 0),
  HSGRAD = c(0, 1, 0, 0, 0, 1), SOMECO = c(0, 0, 1, 0, 0, 0),
  COLGRAD = c(0, 0, 0, 1, 0, 0), REGION = c(1, 2, 3, 4, 5, 6),
  WSAL_VAL = c(0, 10000, 52000, 0, 10000, 30000)
)
x <- fixture()
y <- prepare_cps(x)
stopifnot(nrow(y$data) == 6L, identical(y$data$weight, as.numeric(1:6)),
          all(y$data$outcome_year == y$data$survey_year - 1),
          any(y$data$survey_year %% 2 == 1),
          identical(as.character(y$data$age_sample), c("young", "young", "old", "old", "young", "old")),
          is.na(y$data$nominal_weekly_wage[1]), is.na(y$data$weeks[4]),
          y$data$nominal_weekly_wage[2] == 1000,
          identical(as.character(y$data$race), c("1", "2", "3", "3", "1", "2")),
          sum(y$exclusions$n) == 0)
# Full year representations preserve outcome timing.
x$YEAR <- x$YEAR + 1900
stopifnot(identical(prepare_cps(x)$data$survey_year, y$data$survey_year))
# Missing/invalid weights and population exclusions are counted exactly once.
x <- fixture(); x$AGE[1] <- 20; x$FNLWGT2[1:3] <- c(NA, 0, -10)
z <- prepare_cps(x)
stopifnot(nrow(z$data) == 3L, sum(z$exclusions$n) == 3L, sum(z$population_counts$n) == 5L,
          z$exclusions$n[z$exclusions$reason == "outside_age_21_58_or_missing"] == 1,
          z$exclusions$n[z$exclusions$reason == "missing_or_nonpositive_weight"] == 2)
# Unknown group codes are never silently recoded as nondisabled.
x <- fixture(); x$DISABL1[1] <- 2
expect_error(prepare_cps(x), "Unsupported DISABL1")
x <- fixture(); x$HSGRAD[1] <- 1
expect_error(prepare_cps(x), "Conflicting education")
x <- fixture(); x$WKSWORK[1] <- 99
expect_error(prepare_cps(x), "WKSWORK outside")
x <- fixture(); x$YEAR[1] <- 2000
expect_error(prepare_cps(x), "Unsupported YEAR")
x <- fixture(); x$DISABL1[1] <- NA
stopifnot(nrow(prepare_cps(x)$data) == 5L)
x <- fixture(); x$HSGRAD[2] <- NA
stopifnot(is.na(prepare_cps(x)$data$education[2]))
x <- fixture(); x$RACE <- NULL
expect_error(prepare_cps(x), "missing: RACE")
x <- fixture(); x$WORKING <- c(0, 1, 1, 0, 1, 1)
x$SS_YN <- c(1, 2, 1, 2, 1, 2); x$SSI_YN <- c(2, 1, 2, 1, 2, 1)
p <- prepare_cps(x)
stopifnot(identical(p$data$current_working, as.integer(x$WORKING)),
          identical(p$data$social_security_receipt, as.integer(x$SS_YN == 1)),
          identical(p$data$ssi_receipt, as.integer(x$SSI_YN == 1)),
          p$metadata$mapping_status == "unverified_input_requires_hash_bound_source_audit",
          identical(p$metadata$original_programs_available, FALSE))
x$SSI_YN[1] <- 9
expect_error(prepare_cps(x), "Unsupported SSI_YN")
cat("PASS: canonical data timing, population boundaries, weights, missingness, and strict source-code validation\n")
