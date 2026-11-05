# Repeated-cross-section DID standardized to the group=1, period=1 population.
# These functions estimate an adjusted change contrast. Calling that contrast
# causal additionally requires conditional parallel trends, overlap, no
# anticipation, and a defensible stable definition/composition of the groups.
# Survey weights do not by themselves supply survey-design-valid inference.

# This prediction-level function also permits externally fitted / cross-fitted
# nuisance models. Probability and outcome matrices must have columns
# c("00", "01", "10", "11"), where the first digit is group and the second time.
aipw_did_from_nuisance <- function(y, group, period, weights, probability,
                                   outcome_mean, min_probability = 0.01,
                                   min_ess = 20) {
  cells <- c("00", "01", "10", "11")
  n <- length(y)
  if (!is.numeric(y) || !is.numeric(weights) || n == 0L ||
      any(lengths(list(group, period, weights)) != n) ||
      anyNA(c(y, group, period, weights)) ||
      any(!is.finite(c(y, group, period, weights))) ||
      !all(group %in% 0:1) || !all(period %in% 0:1) ||
      any(weights <= 0)) {
    stop("Use complete finite outcomes, binary group/time, and positive weights.")
  }
  if (length(min_probability) != 1L || !is.finite(min_probability) ||
      min_probability <= 0 || min_probability >= 0.25 ||
      length(min_ess) != 1L || !is.finite(min_ess) || min_ess < 1) {
    stop("Require 0 < min_probability < 0.25 and min_ess >= 1.")
  }
  cell <- paste0(as.integer(group), as.integer(period))
  if (!all(cells %in% cell)) stop("All four group-by-time cells must be present.")
  for (matrix_value in list(probability, outcome_mean)) {
    if (!is.matrix(matrix_value) || !is.numeric(matrix_value) ||
        !identical(dim(matrix_value), c(n, 4L)) ||
        !identical(colnames(matrix_value), cells) ||
        any(!is.finite(matrix_value))) {
      stop("Nuisance predictions must be finite numeric n-by-4 matrices named 00, 01, 10, 11.")
    }
  }
  if (any(probability <= 0) || any(probability > 1) ||
      any(abs(rowSums(probability) - 1) > 1e-6)) {
    stop("Cell probabilities must be positive and sum to one in each row.")
  }
  target <- cell == "11"
  # Overlap is required over the target population's covariate support.
  if (any(probability[target, , drop = FALSE] < min_probability)) {
    stop("Insufficient target-population overlap; inspect covariates/support. No trimming was applied.")
  }
  # Normalize survey weights for numerical stability; all estimands are invariant
  # to multiplying every input weight by the same positive constant.
  weights <- weights / mean(weights)
  target_weights <- weights[target] / sum(weights[target])
  diagnostics <- lapply(cells, function(label) {
    donor <- cell == label
    transport_weights <- weights[donor] *
      probability[donor, "11"] / probability[donor, label]
    if (any(!is.finite(transport_weights)) || sum(transport_weights) <= 0) {
      stop("Nonfinite transport weights; inspect the propensity model.")
    }
    normalized_transport_weights <- transport_weights / sum(transport_weights)
    # Normalize posterior cell odds by observed cell masses to obtain the
    # covariate-density ratio f(X | target=11) / f(X | source=label).
    density_ratio <- probability[donor, "11"] / probability[donor, label] *
      sum(weights[donor]) / sum(weights[target])
    ess <- 1 / sum(normalized_transport_weights^2)
    if (ess < min_ess) {
      stop(sprintf("Cell %s transport ESS %.1f is below min_ess %.1f; no trimming was applied.",
                   label, ess, min_ess))
    }
    # Standard AIPW transport uses target sample mass in both terms:
    # sum(w I_11 m_c) / S_11 + sum(w I_c q_11/q_c {Y-m_c}) / S_11.
    # The source transport weights above are normalized only for diagnostics.
    transported <- sum(target_weights * outcome_mean[target, label]) +
      sum(transport_weights * (y[donor] - outcome_mean[donor, label])) /
      sum(weights[target])
    data.frame(cell = label, n = sum(donor),
               raw_mean = weighted.mean(y[donor], weights[donor]),
               outcome_standardized_mean = if (label == "11") {
                 weighted.mean(y[donor], weights[donor])
               } else sum(target_weights * outcome_mean[target, label]),
               ipw_standardized_mean = sum(normalized_transport_weights * y[donor]),
               transported_mean = transported, transport_ess = ess,
               min_probability_in_target = min(probability[target, label]),
               target_probability_below_001_share = sum(target_weights *
                 (probability[target, label] < 0.01)),
               density_ratio_min = min(density_ratio),
               density_ratio_max = max(density_ratio),
               max_transport_weight_share = max(normalized_transport_weights))
  })
  diagnostics <- do.call(rbind, diagnostics)
  signs <- c(1, -1, -1, 1)
  list(
    estimates = data.frame(
      method = c("unadjusted_did", "aipw_standardized_did",
                 "outcome_standardized_did", "ipw_standardized_did"),
      estimate = c(sum(signs * diagnostics$raw_mean),
                   sum(signs * diagnostics$transported_mean),
                   sum(signs * diagnostics$outcome_standardized_mean),
                   sum(signs * diagnostics$ipw_standardized_mean))),
    cells = diagnostics,
    target = "Observed group=1, period=1 covariate distribution",
    n = n,
    inference = "Point estimates only; no independence or survey-design inference is implied."
  )
}

# Covariate balance uses one common denominator: the weighted target-population
# SD of each model-matrix column. Dummy variables therefore remain visible. A
# target-constant column has undefined SMDs, reported as NA rather than omitted.
did_covariate_balance <- function(design, cell, weights, probability) {
  covariates <- setdiff(colnames(design), "(Intercept)")
  empty <- data.frame(covariate = character(), cell = character(),
                      target_mean = numeric(), target_sd = numeric(),
                      donor_mean_before = numeric(), donor_mean_after = numeric(),
                      smd_before = numeric(), smd_after = numeric(),
                      target_variance_zero = logical())
  if (!length(covariates)) return(empty)
  target <- cell == "11"
  result <- lapply(covariates, function(covariate) {
    x <- design[, covariate]
    target_mean <- weighted.mean(x[target], weights[target])
    zero_variance <- all(x[target] == x[target][1L])
    target_sd <- if (zero_variance) 0 else {
      sqrt(weighted.mean((x[target] - target_mean)^2, weights[target]))
    }
    do.call(rbind, lapply(c("00", "01", "10", "11"), function(label) {
      donor <- cell == label
      transport_weights <- weights[donor] * probability[donor, "11"] /
        probability[donor, label]
      before <- weighted.mean(x[donor], weights[donor])
      after <- weighted.mean(x[donor], transport_weights)
      data.frame(covariate = covariate, cell = label,
                 target_mean = target_mean, target_sd = target_sd,
                 donor_mean_before = before, donor_mean_after = after,
                 smd_before = if (zero_variance) NA_real_ else (before - target_mean) / target_sd,
                 smd_after = if (zero_variance) NA_real_ else (after - target_mean) / target_sd,
                 target_variance_zero = zero_variance)
    }))
  })
  do.call(rbind, result)
}

# Fit multinomial cell probabilities and a separate weighted linear outcome
# regression in each transported source cell. The target mean is observed and
# needs no outcome regression. covariates is one-sided (e.g. ~ age + sex).
# No rows are silently omitted and no weights/probabilities are trimmed. Use
# aipw_did_from_nuisance() for predictions from other or cross-fitted models.
estimate_did <- function(data, outcome, group, period, covariates = ~ 1,
                         weights = NULL, min_probability = 0.01, min_ess = 20) {
  if (!inherits(covariates, "formula") || length(covariates) != 2L) {
    stop("covariates must be a one-sided formula.")
  }
  columns <- c(outcome, group, period, weights, all.vars(covariates))
  if (!is.data.frame(data) || !all(columns %in% names(data))) {
    stop("Data must contain all specified outcome, group, period, weight, and covariate columns.")
  }
  if (any(c(".did_cell", ".did_y", ".did_w") %in% names(data))) {
    stop("Input uses a reserved internal .did_ column name.")
  }
  y <- data[[outcome]]
  d <- data[[group]]
  t <- data[[period]]
  w <- if (is.null(weights)) rep(1, nrow(data)) else data[[weights]]
  if (!is.numeric(y) || !is.numeric(w) || anyNA(c(y, d, t, w)) ||
      any(!is.finite(c(y, d, t, w))) || any(w <= 0) ||
      !all(d %in% 0:1) || !all(t %in% 0:1)) {
    stop("Use complete finite outcomes, binary group/time, and positive weights.")
  }
  cells <- c("00", "01", "10", "11")
  data$.did_cell <- factor(paste0(as.integer(d), as.integer(t)), levels = cells)
  if (any(table(data$.did_cell) == 0L)) stop("All four group-by-time cells must be present.")
  data$.did_y <- y
  data$.did_w <- w / mean(w)
  # na.fail covers missing values produced inside formula transformations too.
  design <- model.matrix(covariates, model.frame(covariates, data, na.action = na.fail))
  if (any(!is.finite(design))) stop("Covariate design contains nonfinite values.")
  propensity_formula <- update(covariates, .did_cell ~ .)
  # multinom's summ=2 combines identical model-matrix/response rows and sums
  # their weights in C. This is the same weighted likelihood, not subsampling.
  # Capture only its unconditional dimension print; warnings still propagate.
  invisible(utils::capture.output(
    propensity_fit <- nnet::multinom(propensity_formula, data = data,
                                    weights = .did_w, trace = FALSE,
                                    # nnet's C summarizer mishandles a one-column
                                    # (e.g. intercept-only) design; retain its
                                    # individual-row path in that edge case.
                                    summ = if (ncol(design) > 1L &&
                                      is.null(attr(terms(propensity_formula), "offset"))) 2 else 0,
                                    maxit = 1000, abstol = 1e-10, reltol = 1e-10,
                                    na.action = na.fail)
  ))
  if (propensity_fit$convergence != 0L) stop("Multinomial propensity model did not converge.")
  probability <- as.matrix(predict(propensity_fit, newdata = data, type = "probs"))
  probability <- probability[, cells, drop = FALSE]
  outcome_formula <- update(covariates, .did_y ~ .)
  predictions <- lapply(cells, function(label) {
    if (label == "11") {
      target <- data$.did_cell == "11"
      return(rep(weighted.mean(y[target], w[target]), nrow(data)))
    }
    fit <- lm(outcome_formula, data = data[data$.did_cell == label, , drop = FALSE],
              weights = .did_w, na.action = na.fail)
    if (fit$rank < ncol(design)) {
      stop(sprintf("Outcome design is rank deficient in cell %s; revise common-support covariates.", label))
    }
    as.numeric(predict(fit, newdata = data))
  })
  outcome_mean <- do.call(cbind, predictions)
  colnames(outcome_mean) <- cells
  result <- aipw_did_from_nuisance(y, d, t, w, probability, outcome_mean,
                                  min_probability, min_ess)
  result$covariates <- paste(deparse(covariates), collapse = " ")
  result$balance <- did_covariate_balance(design, as.character(data$.did_cell),
                                         w, probability)
  result
}
