# Full-refit individual bootstrap for the repeated-cross-section DID estimators.
# Resampling is stratified by the four observed group/time cells, conditioning on
# their sample counts. Analysis weights travel with sampled rows and are not
# recomputed. This is a working independent-individual sampling approximation:
# it does not account for CPS survey design, repeated households, common shocks,
# uncertainty in identifying assumptions, or the single national policy change.

bootstrap_did <- function(data, outcome, group, period, covariates = ~ 1,
                          weights = NULL, B = 199L, seed = 256L,
                          min_probability = 0.01, min_ess = 20,
                          required_success = B, workers = 1L) {
  integer_scalar <- function(x, lower, upper = .Machine$integer.max) {
    is.numeric(x) && length(x) == 1L && !is.na(x) && is.finite(x) &&
      x >= lower && x <= upper && x == floor(x)
  }
  if (!integer_scalar(B, 2)) stop("B must be an integer >= 2; B < 199 is exploratory.")
  if (!integer_scalar(seed, 0)) stop("seed must be a nonnegative integer.")
  if (!integer_scalar(workers, 1)) stop("workers must be a positive integer.")
  if (workers > 1L && .Platform$OS.type == "windows") {
    stop("Parallel fork workers are unavailable on Windows; use workers = 1.")
  }
  if (!integer_scalar(required_success, 2, B)) {
    stop("required_success must be an integer between 2 and B.")
  }
  if (!exists("estimate_did", mode = "function")) {
    stop("Source R/estimate_did.R before calling bootstrap_did().")
  }
  B <- as.integer(B)
  workers <- as.integer(workers)
  effective_workers <- min(workers, B)
  required_success <- as.integer(required_success)
  # Fix the algorithm as well as the seed, then restore the caller's complete
  # RNG configuration/state even when an observed-data or bootstrap fit fails.
  old_kind <- RNGkind()
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  on.exit({
    do.call(RNGkind, as.list(old_kind))
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  RNGkind("Mersenne-Twister", "Inversion", "Rejection")
  set.seed(as.integer(seed))

  fit_once <- function(sample) {
    estimate_did(sample, outcome, group, period, covariates, weights,
                 min_probability = min_probability, min_ess = min_ess)
  }
  # A failed observed-data fit is an error; bootstrap failures never erase a
  # successfully fitted observed result.
  point <- fit_once(data)
  methods <- as.character(point$estimates$method)
  cells <- c("00", "01", "10", "11")
  cell <- paste0(as.integer(data[[group]]), as.integer(data[[period]]))
  cell_rows <- split(seq_len(nrow(data)), factor(cell, levels = cells))
  # Assign an explicit seed to each requested draw before dispatch. Serial and
  # forked workers therefore sample/refit identically regardless of scheduling.
  replicate_seeds <- sample.int(.Machine$integer.max, B, replace = FALSE)

  run_replicate <- function(b) {
    set.seed(replicate_seeds[b])
    sampled_rows <- unlist(lapply(cell_rows, function(rows) {
      rows[sample.int(length(rows), size = length(rows), replace = TRUE)]
    }), use.names = FALSE)
    sample <- data[sampled_rows, , drop = FALSE]
    messages <- character()
    error_message <- NA_character_
    fitted <- tryCatch(
      withCallingHandlers({
        fitted <- fit_once(sample)
        if (!identical(as.character(fitted$estimates$method), methods) ||
            length(fitted$estimates$estimate) != length(methods) ||
            any(!is.finite(fitted$estimates$estimate))) {
          stop("Bootstrap fit returned incompatible methods or nonfinite estimates.")
        }
        fitted
      }, warning = function(w) {
        messages <<- c(messages, conditionMessage(w))
        invokeRestart("muffleWarning")
      }),
      error = function(e) {
        error_message <<- conditionMessage(e)
        NULL
      }
    )
    success <- !is.null(fitted)
    values <- if (success) fitted$estimates$estimate else rep(NA_real_, length(methods))
    values <- data.frame(
      replicate = b, method = methods, estimate = values,
      success = success, failure_reason = error_message,
      stringsAsFactors = FALSE
    )
    sampled_cell <- cell[sampled_rows]
    status <- data.frame(
      replicate = b, seed = replicate_seeds[b], success = success, failure_reason = error_message,
      warnings = paste(unique(messages), collapse = " | "),
      n = nrow(sample), n_00 = sum(sampled_cell == "00"),
      n_01 = sum(sampled_cell == "01"), n_10 = sum(sampled_cell == "10"),
      n_11 = sum(sampled_cell == "11"),
      weight_sum = if (is.null(weights)) nrow(sample) else sum(sample[[weights]]),
      stringsAsFactors = FALSE
    )
    list(values = values, status = status)
  }
  fits <- if (effective_workers == 1L) {
    lapply(seq_len(B), run_replicate)
  } else {
    parallel::mclapply(seq_len(B), run_replicate, mc.cores = effective_workers,
                       mc.set.seed = FALSE, mc.preschedule = FALSE)
  }
  # Unexpected worker/process failures also occupy their original draw slots;
  # never shorten the requested bootstrap distribution after a worker failure.
  for (b in seq_len(B)) {
    if (!is.list(fits[[b]]) || !all(c("values", "status") %in% names(fits[[b]]))) {
      reason <- if (inherits(fits[[b]], "try-error")) as.character(fits[[b]]) else "Bootstrap worker returned no result."
      fits[[b]] <- list(
        values = data.frame(replicate = b, method = methods, estimate = NA_real_,
                            success = FALSE, failure_reason = reason),
        status = data.frame(replicate = b, seed = replicate_seeds[b], success = FALSE,
                            failure_reason = reason, warnings = "", n = nrow(data),
                            n_00 = length(cell_rows[["00"]]), n_01 = length(cell_rows[["01"]]),
                            n_10 = length(cell_rows[["10"]]), n_11 = length(cell_rows[["11"]]),
                            weight_sum = NA_real_)
      )
    }
  }
  replicates <- do.call(rbind, lapply(fits, `[[`, "values"))
  status <- do.call(rbind, lapply(fits, `[[`, "status"))
  successes <- sum(status$success)
  available <- successes >= required_success
  interval_status <- if (!available) {
    "unavailable_insufficient_successful_refits"
  } else if (successes < B) {
    "available_partial_refits_explicit_threshold"
  } else "available_all_refits"
  precision_label <- if (B < 199L) "exploratory_B_less_than_199" else "B_at_least_199"
  summary <- point$estimates
  summary$bootstrap_se <- NA_real_
  summary$conf_low <- NA_real_
  summary$conf_high <- NA_real_
  if (available) {
    for (j in seq_along(methods)) {
      draws <- replicates$estimate[replicates$method == methods[j] & replicates$success]
      summary$bootstrap_se[j] <- sd(draws)
      limits <- quantile(draws, c(0.025, 0.975), type = 7, names = FALSE)
      summary$conf_low[j] <- limits[1L]
      summary$conf_high[j] <- limits[2L]
    }
  }
  summary$B <- B
  summary$successes <- successes
  summary$failures <- B - successes
  summary$required_success <- required_success
  summary$intervals_available <- available
  summary$interval_status <- interval_status
  summary$precision_label <- precision_label
  summary$inference <- "Fixed-cell-count individual bootstrap; not CPS-design or national-policy uncertainty"

  list(
    summary = summary,
    point = point,
    replicates = replicates,
    replicate_status = status,
    metadata = list(
      B = B, seed = as.integer(seed), required_success = required_success,
      workers = workers, effective_workers = effective_workers,
      rng = "Explicit per-replicate Mersenne-Twister seeds; serial/parallel outputs identical",
      precision_label = precision_label, confidence_level = 0.95,
      interval_method = "Percentile; R quantile type 7",
      resampling = "Individuals sampled with replacement separately in cells 00, 01, 10, 11; cell counts fixed",
      weights = "Original row analysis weights carried unchanged into each resample",
      nuisance_models = "Multinomial cell probabilities and three source-cell outcome regressions refitted in every draw",
      failure_rule = "By default every requested refit must succeed; otherwise uncertainty columns are NA and all failure reasons retained",
      partial_success_caution = "An explicitly lowered required_success conditions intervals on successful fits and can distort uncertainty; inspect every failed draw",
      limitations = "Independent-individual working approximation; ignores survey strata/PSUs, household overlap, common shocks, and identification uncertainty"
    )
  )
}
