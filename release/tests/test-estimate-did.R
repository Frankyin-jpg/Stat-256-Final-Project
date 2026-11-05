#!/usr/bin/env Rscript
# Run from the repository root: Rscript tests/test-estimate-did.R
source("R/estimate_did.R")

assert_close <- function(actual, expected, tolerance = 1e-5) {
  stopifnot(isTRUE(all.equal(as.numeric(actual), as.numeric(expected),
                            tolerance = tolerance)))
}
assert_error <- function(expression, pattern) {
  message <- tryCatch({ force(expression); NA_character_ },
                      error = function(e) conditionMessage(e))
  stopifnot(!is.na(message), grepl(pattern, message))
}

# Exact finite-population construction, not a sample of the project's CPS data.
# Cell membership depends on X, creating composition bias in the raw contrast.
cells <- c("00", "01", "10", "11")
q <- rbind(c(.35, .25, .25, .15), c(.15, .20, .25, .40))
colnames(q) <- cells
sim <- do.call(rbind, lapply(1:2, function(k) {
  do.call(rbind, lapply(seq_along(cells), function(j) {
    n_cell <- as.integer(2000 * q[k, j])
    data.frame(x = rep(c(-1, 1)[k], n_cell),
               d = rep(as.integer(substr(cells[j], 1, 1)), n_cell),
               post = rep(as.integer(substr(cells[j], 2, 2)), n_cell))
  }))
}))
sim$y <- 5 + 2 * sim$x + sim$d + 3 * sim$post +
  4 * sim$d * sim$post + 1.5 * sim$x * sim$d
sim$w <- 1
fit <- estimate_did(sim, "y", "d", "post", ~ x, weights = "w")
assert_close(fit$estimates$estimate[2], 4)
assert_close(fit$estimates$estimate[3:4], c(4, 4), tolerance = 1e-4)
stopifnot(abs(fit$estimates$estimate[1] - 4) > .1)
stopifnot(all(fit$cells$transport_ess > 20), fit$n == 4000)
# Correct transport probabilities remove the known X-composition imbalance.
balance <- fit$balance[fit$balance$cell != "11", ]
stopifnot(nrow(balance) == 3L,
          max(abs(balance$smd_before)) > .1,
          max(abs(balance$smd_after)) < 1e-4,
          all(abs(balance$smd_after) < abs(balance$smd_before)))
unadjusted <- estimate_did(sim, "y", "d", "post", ~ 1)
assert_close(unadjusted$estimates$estimate,
             rep(unadjusted$estimates$estimate[1], 4))
stopifnot(nrow(unadjusted$balance) == 0L)

# Target support may have a constant covariate even when source models have
# variation. Keep that covariate in balance output with explicit undefined SMD.
constant_target <- sim[!(sim$d == 1 & sim$post == 1 & sim$x == 1), ]
constant_fit <- estimate_did(constant_target, "y", "d", "post", ~ x)
stopifnot(nrow(constant_fit$balance) == 4L,
          all(constant_fit$balance$target_variance_zero),
          all(constant_fit$balance$target_sd == 0),
          all(is.na(constant_fit$balance$smd_before)),
          all(is.na(constant_fit$balance$smd_after)))
assert_close(constant_fit$estimates$estimate[2], 4)

# A common outcome shift and arbitrary survey-weight units leave DID unchanged.
shifted <- sim
shifted$y <- shifted$y + 100
shifted$w <- shifted$w * 17
assert_close(estimate_did(shifted, "y", "d", "post", ~ x, "w")$estimates$estimate,
             fit$estimates$estimate)
zero <- sim
zero$y <- zero$y - 4 * zero$d * zero$post
assert_close(estimate_did(zero, "y", "d", "post", ~ x)$estimates$estimate[2], 0)
unequal_weights <- sim
unequal_weights$w <- 1 + .2 * unequal_weights$x
assert_close(estimate_did(unequal_weights, "y", "d", "post", ~ x, "w")$estimates$estimate[2], 4)

# Check double robustness with exact known nuisance predictions: either the
# probability model or all transported outcome means may be wrong, but not both.
probability <- q[ifelse(sim$x == -1, 1L, 2L), , drop = FALSE]
mu <- sapply(cells, function(cell) {
  d <- as.integer(substr(cell, 1, 1))
  t <- as.integer(substr(cell, 2, 2))
  5 + 2 * sim$x + d + 3 * t + 4 * d * t + 1.5 * sim$x * d
})
wrong_probability <- matrix(.25, nrow(sim), 4, dimnames = list(NULL, cells))
wrong_mu <- matrix(0, nrow(sim), 4, dimnames = list(NULL, cells))
run_predictions <- function(p, m) {
  aipw_did_from_nuisance(sim$y, sim$d, sim$post, sim$w, p, m)$estimates$estimate[2]
}
assert_close(run_predictions(probability, mu), 4)
assert_close(run_predictions(probability, wrong_mu), 4)
assert_close(run_predictions(wrong_probability, mu), 4)
stopifnot(abs(run_predictions(wrong_probability, wrong_mu) - 4) > .1)

# Exact cell-odds/density-ratio diagnostic on the finite construction.
known <- aipw_did_from_nuisance(sim$y, sim$d, sim$post, sim$w, probability, mu)
target <- sim$d == 1 & sim$post == 1
for (cell in cells) {
  donor <- paste0(sim$d, sim$post) == cell
  expected_ratio <- probability[donor, "11"] / probability[donor, cell] *
    sum(sim$w[donor]) / sum(sim$w[target])
  row <- known$cells[known$cells$cell == cell, ]
  assert_close(row$density_ratio_min, min(expected_ratio))
  assert_close(row$density_ratio_max, max(expected_ratio))
  assert_close(row$target_probability_below_001_share,
               weighted.mean(probability[target, cell] < .01, sim$w[target]))
}
stopifnot(all(known$cells$target_probability_below_001_share >= 0),
          all(known$cells$target_probability_below_001_share <= 1))

# Compare the optimized multinomial likelihood to the original individual-row
# likelihood, while retaining native lm/predict semantics for outcome models.
unaggregated_reference <- function(data, covariates) {
  data$cell <- factor(paste0(data$d, data$post), levels = cells)
  data$nw <- data$w / mean(data$w)
  propensity <- nnet::multinom(update(covariates, cell ~ .), data = data,
                               weights = nw, trace = FALSE, summ = 0,
                               maxit = 1000, abstol = 1e-10, reltol = 1e-10)
  stopifnot(propensity$convergence == 0)
  p <- predict(propensity, newdata = data, type = "probs")
  m <- sapply(cells, function(cell) {
    if (cell == "11") return(rep(weighted.mean(data$y[data$cell == cell],
                                               data$w[data$cell == cell]), nrow(data)))
    model <- lm(update(covariates, y ~ .), data = data[data$cell == cell, ], weights = nw)
    as.numeric(predict(model, newdata = data))
  })
  aipw_did_from_nuisance(data$y, data$d, data$post, data$w, p, m)
}
set.seed(513)
repeated <- sim
repeated$w <- runif(nrow(repeated), .5, 2)
repeated$y <- repeated$y + rnorm(nrow(repeated), sd = .3)
reference <- unaggregated_reference(repeated, ~ x)
optimized <- estimate_did(repeated, "y", "d", "post", ~ x, "w")
assert_close(optimized$estimates$estimate, reference$estimates$estimate, tolerance = 1e-7)
assert_close(optimized$cells$transported_mean, reference$cells$transported_mean, tolerance = 1e-7)
# Unique continuous rows do not aggregate, and must retain the same answer too.
continuous <- repeated
continuous$z <- rnorm(nrow(continuous))
continuous$y <- continuous$y + .5 * continuous$z
reference <- unaggregated_reference(continuous, ~ x + z)
optimized <- estimate_did(continuous, "y", "d", "post", ~ x + z, "w")
assert_close(optimized$estimates$estimate, reference$estimates$estimate, tolerance = 1e-7)

# Invalid data and unsupported overlap fail explicitly instead of dropping rows.
assert_error(estimate_did(sim[!(sim$d == 1 & sim$post == 1), ],
                          "y", "d", "post", ~ x), "four.*cells")
bad <- sim
bad$w[1] <- 0
assert_error(estimate_did(bad, "y", "d", "post", ~ x, "w"), "positive weights")
bad <- sim
bad$x[1] <- NA_real_
assert_error(estimate_did(bad, "y", "d", "post", ~ x), "missing values")
bad_probability <- probability
bad_probability[, 1] <- .001
bad_probability[, 2:4] <- bad_probability[, 2:4] /
  rowSums(bad_probability[, 2:4]) * .999
assert_error(run_predictions(bad_probability, mu), "overlap")
assert_error(aipw_did_from_nuisance(sim$y, sim$d, sim$post, sim$w,
                                   probability, mu, min_ess = nrow(sim)), "ESS")
cat("PASS: known-effect and null DID, composition adjustment, double robustness,\n",
    "      shift/weight invariance, covariate balance/constant-target diagnostics,\n",
    "      and invalid-input/overlap/ESS checks.\n", sep = "")
