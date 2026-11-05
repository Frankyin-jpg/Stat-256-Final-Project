#!/usr/bin/env Rscript
# Run from the repository root. Small B values test software, not coverage or
# adequate Monte Carlo precision for an empirical paper.
source("R/estimate_did.R")
source("R/bootstrap_did.R")

assert_close <- function(x, y, tolerance = 1e-7) {
  stopifnot(isTRUE(all.equal(as.numeric(x), as.numeric(y), tolerance = tolerance)))
}
assert_error <- function(expression, pattern) {
  message <- tryCatch({ force(expression); NA_character_ },
                      error = function(e) conditionMessage(e))
  stopifnot(!is.na(message), grepl(pattern, message))
}

set.seed(712)
d <- expand.grid(group = 0:1, post = 0:1, person = seq_len(80))
d$x <- rnorm(nrow(d))
d$weight <- exp(.1 * d$x)
d$y <- 5 + 1.2 * d$x + d$group + 3 * d$post +
  2 * d$group * d$post + rnorm(nrow(d), sd = .8)
set.seed(150)
caller_seed <- .Random.seed
caller_kind <- RNGkind()
fit <- bootstrap_did(d, "y", "group", "post", ~ x, "weight", B = 11, seed = 29)
stopifnot(identical(caller_seed, .Random.seed), identical(caller_kind, RNGkind()))
stopifnot(nrow(fit$summary) == 4L, nrow(fit$replicates) == 44L,
          nrow(fit$replicate_status) == 11L, all(fit$replicate_status$success),
          all(fit$summary$intervals_available), all(fit$summary$bootstrap_se > .01),
          all(fit$summary$conf_high > fit$summary$conf_low),
          all(fit$summary$precision_label == "exploratory_B_less_than_199"),
          abs(fit$summary$estimate[2] - 2) < .5,
          all(fit$replicate_status$n == nrow(d)),
          all(as.matrix(fit$replicate_status[c("n_00", "n_01", "n_10", "n_11")]) == 80))
repeat_fit <- bootstrap_did(d, "y", "group", "post", ~ x, "weight", B = 11, seed = 29)
stopifnot(identical(fit$replicates, repeat_fit$replicates),
          identical(fit$summary, repeat_fit$summary))
if (.Platform$OS.type != "windows") {
  parallel_fit <- bootstrap_did(d, "y", "group", "post", ~ x, "weight",
                                B = 11, seed = 29, workers = 2)
  stopifnot(identical(fit$replicates, parallel_fit$replicates),
            identical(fit$replicate_status, parallel_fit$replicate_status),
            identical(fit$summary, parallel_fit$summary),
            parallel_fit$metadata$workers == 2L,
            identical(caller_seed, .Random.seed), identical(caller_kind, RNGkind()))
}

# Changing survey-weight units preserves the same estimates and bootstrap draws.
scaled <- d
scaled$weight <- 100 * scaled$weight
scaled_fit <- bootstrap_did(scaled, "y", "group", "post", ~ x, "weight", B = 11, seed = 29)
assert_close(fit$replicates$estimate, scaled_fit$replicates$estimate)
assert_close(fit$summary$bootstrap_se, scaled_fit$summary$bootstrap_se)
assert_close(100 * fit$replicate_status$weight_sum, scaled_fit$replicate_status$weight_sum)

# A null treatment interaction retains meaningful sampling variability.
null <- d
null$y <- null$y - 2 * null$group * null$post
null_fit <- bootstrap_did(null, "y", "group", "post", ~ x, "weight", B = 9, seed = 41)
stopifnot(abs(null_fit$summary$estimate[2]) < .5,
          all(null_fit$summary$bootstrap_se > .01))

# In one donor cell, X=1 occurs once. Some resamples lose that support and the
# outcome fit fails. Every requested draw and reason must remain in the output.
rare <- expand.grid(group = 0:1, post = 0:1, person = seq_len(50))
rare$x <- as.numeric(rare$person > 25)
rare$x[rare$group == 0 & rare$post == 0] <- 0
rare$x[rare$group == 0 & rare$post == 0 & rare$person == 1] <- 1
rare$y <- 3 + rare$x + rare$group + rare$post + 2 * rare$group * rare$post
failed <- bootstrap_did(rare, "y", "group", "post", ~ x, B = 11, seed = 13,
                        min_probability = 1e-8, min_ess = 1)
stopifnot(nrow(failed$replicate_status) == 11L, nrow(failed$replicates) == 44L,
          any(!failed$replicate_status$success), any(failed$replicate_status$success),
          all(nzchar(failed$replicate_status$failure_reason[!failed$replicate_status$success])),
          any(grepl("rank deficient", failed$replicate_status$failure_reason, fixed = TRUE)),
          !any(failed$summary$intervals_available),
          all(is.na(failed$summary$conf_low)), all(is.na(failed$summary$conf_high)),
          all(is.na(failed$summary$bootstrap_se)),
          all(is.finite(failed$point$estimates$estimate)))

# Explicit opt-in to partial-refit intervals remains visible in every summary row.
partial <- bootstrap_did(rare, "y", "group", "post", ~ x, B = 11, seed = 13,
                         min_probability = 1e-8, min_ess = 1, required_success = 2)
stopifnot(all(partial$summary$intervals_available),
          all(partial$summary$interval_status == "available_partial_refits_explicit_threshold"),
          identical(failed$replicate_status, partial$replicate_status))

# A caller with no initialized RNG state also gets exactly that state back.
rm(".Random.seed", envir = .GlobalEnv)
invisible(bootstrap_did(d, "y", "group", "post", ~ x, "weight", B = 2, seed = 10))
stopifnot(!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
for (bad_B in list(0, 1, -1, 2.5, NA_real_, Inf, "11")) {
  assert_error(bootstrap_did(d, "y", "group", "post", ~ x, B = bad_B), "B must")
}
assert_error(bootstrap_did(d, "y", "group", "post", ~ x, B = 9, required_success = 10),
             "required_success")
assert_error(bootstrap_did(d, "y", "group", "post", ~ x, B = 9, workers = 0), "workers")
cat("PASS: noisy/null effects, nonzero uncertainty, reproducibility/RNG restoration,\n",
    "      serial/parallel equivalence, cell preservation, weight-scale invariance, retained failures,\n",
    "      strict interval suppression, and explicit exploratory/partial labels.\n", sep = "")
