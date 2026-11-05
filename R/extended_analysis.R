# Additional repeated-cross-section contrasts. These helpers return data only:
# callers own serialization and reporting. Distributional estimates are DID
# contrasts in standardized CDF values, never quantile treatment effects.
# Source R/event_study.R and R/estimate_did.R before calling this module.

run_extended_analyses <- function(data, min_probability = .01, min_ess = 20) {
  required <- c("sex", "age_sample", "outcome_year", "disabled", "weight",
                "weeks", "worked_any", "age", "race", "education", "region")
  if (!is.data.frame(data) || !all(required %in% names(data))) {
    stop("Extended analyses require the complete canonical CPS schema: ",
         paste(required, collapse = ", "))
  }
  primary_formula <- ~ age_c + I(age_c^2) + race
  rich_formula <- ~ age_c + I(age_c^2) + race + education + region
  main_pre <- 1987:1989
  main_post <- 1993:1996
  results <- list(placebo = list(), distribution = list(), rich = list())
  diagnostics <- failures <- list()
  empty_labels <- data.frame(analysis = character(), sex = character(),
    age_sample = character(), outcome = character(), window = character(),
    threshold = numeric(), covariate_set = character())
  empty_counts <- data.frame(n_stratum = integer(), n_complete = integer(), n_window = integer())

  make_label <- function(analysis, sex, age_sample, outcome, window,
                         covariate_set = "primary", threshold = NA_real_) {
    data.frame(analysis = analysis, sex = sex, age_sample = age_sample,
               outcome = outcome, window = window, threshold = threshold,
               covariate_set = covariate_set)
  }
  stamp <- function(payload, label, counts) {
    result <- cbind(label[rep(1L, nrow(payload)), , drop = FALSE], payload,
                    counts[rep(1L, nrow(payload)), , drop = FALSE])
    rownames(result) <- NULL
    result
  }
  attempt <- function(stratum, response, label, pre, post, formula,
                      complete_covariates = all.vars(formula)) {
    # Include every variable needed by either specification when comparing
    # primary and rich models. Pooling weights are recomputed after this common
    # complete-case restriction; no cell is silently trimmed or substituted.
    needed <- unique(c(response, "outcome_year", "disabled", "weight", complete_covariates))
    keep <- complete.cases(stratum[, needed, drop = FALSE])
    eligible <- droplevels(stratum[keep, , drop = FALSE])
    counts <- data.frame(n_stratum = nrow(stratum), n_complete = nrow(eligible),
                         n_window = sum(eligible$outcome_year %in% c(pre, post)))
    stage <- "pooling"
    fit <- tryCatch({
      pooled <- pool_periods(eligible, pre = pre, post = post)
      stage <- "estimation"
      estimate_did(pooled, response, "disabled", "post", formula,
                   weights = "analysis_weight", min_probability = min_probability,
                   min_ess = min_ess)
    }, error = function(e) e)
    if (inherits(fit, "error")) {
      failures[[length(failures) + 1L]] <<- stamp(
        data.frame(stage = stage, reason = conditionMessage(fit)), label, counts)
      return(invisible(NULL))
    }
    analysis <- label$analysis[[1]]
    results[[analysis]][[length(results[[analysis]]) + 1L]] <<-
      stamp(fit$estimates, label, counts)
    diagnostic <- data.frame(
      excluded_complete = counts$n_stratum - counts$n_complete,
      excluded_outside_window = counts$n_complete - counts$n_window,
      min_transport_ess = min(fit$cells$transport_ess),
      min_probability_in_target = min(fit$cells$min_probability_in_target),
      max_transport_weight_share = max(fit$cells$max_transport_weight_share))
    diagnostics[[length(diagnostics) + 1L]] <<- stamp(diagnostic, label, counts)
    invisible(NULL)
  }

  for (sex_value in c("male", "female")) for (age_value in c("young", "old")) {
    matches <- !is.na(data$sex) & !is.na(data$age_sample) &
      data$sex == sex_value & data$age_sample == age_value
    stratum <- droplevels(data[matches, , drop = FALSE])
    stratum$age_c <- (stratum$age - 40) / 10
    for (outcome in c("weeks", "worked_any")) {
      attempt(stratum, outcome,
        make_label("placebo", sex_value, age_value, outcome, "pre_policy"),
        pre = 1987, post = 1988:1989, formula = primary_formula)
      for (covariate_set in c("primary_same_sample", "rich")) {
        formula <- if (covariate_set == "rich") rich_formula else primary_formula
        attempt(stratum, outcome,
          make_label("rich", sex_value, age_value, outcome, "main", covariate_set),
          pre = main_pre, post = main_post, formula = formula,
          complete_covariates = all.vars(rich_formula))
      }
    }
    for (threshold in c(0, 13, 26, 39, 51)) {
      distribution_sample <- stratum
      distribution_sample$.cdf_outcome <- as.numeric(stratum$weeks <= threshold)
      attempt(distribution_sample, ".cdf_outcome",
        make_label("distribution", sex_value, age_value, "cdf_weeks", "main",
                   threshold = threshold),
        pre = main_pre, post = main_post, formula = primary_formula)
    }
  }
  bind <- function(values, empty) {
    if (!length(values)) return(empty)
    result <- do.call(rbind, values)
    rownames(result) <- NULL
    result
  }
  empty_estimates <- cbind(empty_labels,
    data.frame(method = character(), estimate = numeric()), empty_counts)
  list(
    placebo = bind(results$placebo, empty_estimates),
    distributional = bind(results$distribution, empty_estimates),
    rich = bind(results$rich, empty_estimates),
    diagnostics = bind(diagnostics, cbind(empty_labels, data.frame(
      excluded_complete = integer(), excluded_outside_window = integer(),
      min_transport_ess = numeric(), min_probability_in_target = numeric(),
      max_transport_weight_share = numeric()), empty_counts)),
    failures = bind(failures, cbind(empty_labels,
      data.frame(stage = character(), reason = character()), empty_counts))
  )
}
