# Descriptive reanalysis, distinct from the yet-to-be-audited original SAS models.
weighted_mean_observed <- function(y, w) {
  keep <- is.finite(y) & is.finite(w) & w > 0
  if (!any(keep)) return(NA_real_)
  sum(y[keep] * w[keep]) / sum(w[keep])
}

describe_cps <- function(data) {
  dplyr::summarise(
    dplyr::group_by(data, outcome_year, sex, age_sample, disabled),
    n = dplyr::n(), weight_sum = sum(weight),
    n_weeks = sum(is.finite(weeks)),
    n_worked_any = sum(is.finite(worked_any)),
    mean_weeks = weighted_mean_observed(weeks, weight),
    mean_worked_any = weighted_mean_observed(worked_any, weight),
    .groups = "drop"
  )
}

fit_event_study <- function(data, outcome = "weeks", reference = 1989L) {
  needed <- c(outcome, "weight", "disabled", "outcome_year", "age", "race")
  if (!all(needed %in% names(data))) stop("Missing event-study columns.")
  keep <- complete.cases(data[, needed]) & is.finite(data[[outcome]]) &
    is.finite(data$weight) & data$weight > 0
  sample <- droplevels(data[keep, , drop = FALSE])
  if (!nrow(sample)) stop("Empty event-study sample.")
  if (!all(sample$disabled %in% 0:1)) stop("Disability must be binary.")
  if (!reference %in% sample$outcome_year) stop("Reference year is absent.")
  if (any(table(sample$outcome_year, factor(sample$disabled, levels = 0:1)) == 0)) {
    stop("Each observed year must contain both disability groups.")
  }
  sample$year_factor <- relevel(factor(sample$outcome_year), as.character(reference))
  formula <- as.formula(paste0(outcome,
    " ~ disabled * year_factor + age + I(age^2) + factor(race)"))
  model <- fixest::feols(formula, data = sample, weights = ~ weight,
                         vcov = "hetero", notes = FALSE)
  coefficients <- fixest::coeftable(model)
  rows <- grepl("^disabled:year_factor[0-9]+$", rownames(coefficients))
  coefficients <- coefficients[rows, , drop = FALSE]
  expected <- length(unique(sample$outcome_year)) - 1L
  if (nrow(coefficients) != expected) stop("Event-study contrasts are not identified.")
  result <- data.frame(
    outcome = outcome,
    year = as.integer(sub("^disabled:year_factor([0-9]+)$", "\\1",
                          rownames(coefficients))),
    estimate = coefficients[, 1], std_error = coefficients[, 2],
    n = nrow(sample), excluded = nrow(data) - nrow(sample),
    reference = reference,
    inference = "HC1; individual independence; not design-based policy uncertainty",
    row.names = NULL
  )
  # Normal intervals describe the working independent-observation model only.
  result$conf_low <- result$estimate - qnorm(.975) * result$std_error
  result$conf_high <- result$estimate + qnorm(.975) * result$std_error
  list(coefficients = result, model = model, sample = sample)
}

pool_periods <- function(data, pre = 1987:1989, post = 1993:1996) {
  if (!length(pre) || !length(post) || anyDuplicated(pre) || anyDuplicated(post) ||
      any(!is.finite(c(pre, post)))) stop("Period windows must be nonempty and contain unique finite years.")
  if (length(intersect(pre, post))) stop("Pre and post windows overlap.")
  if (any(!is.finite(data$weight) | data$weight <= 0)) stop("Period weights must be positive and finite.")
  if (!all(c(pre, post) %in% data$outcome_year)) stop("Required outcome years are absent.")
  data <- data[data$outcome_year %in% c(pre, post), , drop = FALSE]
  data$post <- as.integer(data$outcome_year %in% post)
  # Each year receives equal total weight; each period then has unit total mass.
  totals <- ave(data$weight, data$outcome_year, FUN = sum)
  year_count <- ifelse(data$post == 1, length(post), length(pre))
  data$analysis_weight <- data$weight / totals / year_count
  data
}
