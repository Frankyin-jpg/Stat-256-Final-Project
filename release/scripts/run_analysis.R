#!/usr/bin/env Rscript
# Run from the repository root. All published numbers come from this pipeline.
source("R/prepare_data.R")
source("R/event_study.R")
source("R/estimate_did.R")
source("R/extended_analysis.R")
source("R/bootstrap_did.R")
args <- commandArgs(trailingOnly = TRUE)
bootstrap_B <- 0L
bootstrap_seed <- 256L
bootstrap_workers <- 2L
input_paths <- character()
while (length(args)) {
  if (args[[1]] %in% c("--bootstrap", "--seed", "--workers")) {
    if (length(args) < 2L) stop("Missing value for ", args[[1]])
    value <- suppressWarnings(as.numeric(args[[2]]))
    if (length(value) != 1L || !is.finite(value) || value != floor(value) || value < 0 || value > .Machine$integer.max) stop("Options require nonnegative integer values.")
    if (args[[1]] == "--bootstrap") bootstrap_B <- as.integer(value)
    if (args[[1]] == "--seed") bootstrap_seed <- as.integer(value)
    if (args[[1]] == "--workers") bootstrap_workers <- as.integer(value)
    args <- args[-c(1L, 2L)]
  } else {
    if (startsWith(args[[1]], "--")) stop("Unknown option: ", args[[1]])
    input_paths <- c(input_paths, args[[1]])
    args <- args[-1L]
  }
}
if (length(input_paths) > 1L || bootstrap_B == 1L || bootstrap_workers < 1L) stop("Usage: Rscript scripts/run_analysis.R [raw.sas7bdat] [--bootstrap 199] [--seed 256] [--workers 2]")
if (length(input_paths)) {
  input <- input_paths[[1]]
} else {
  candidates <- list.files("data/raw", pattern = "^marcps_w\\.sas7bdat$",
                           recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  if (length(candidates) != 1L) stop(
    "Expected exactly one marcps_w.sas7bdat under data/raw. Fetch and inspect the original archive; ",
    "if its source format differs, audit and implement that format before analysis. No synthetic fallback is used.")
  input <- candidates[[1]]
}
prepared <- read_cps(input)
prepared$metadata$input_md5 <- unname(tools::md5sum(input))
prepared$metadata$data_kind <- "unverified"
# This record can only be created after the original-source audit. Bind it to
# the exact data file and downloaded manifest, never merely a familiar filename.
verification_path <- "data/source-verification.json"
if (file.exists(verification_path)) {
  verified <- jsonlite::read_json(verification_path, simplifyVector = TRUE)
  manifest_path <- "data/raw/manifest.json"
  manifest_sha <- if (file.exists(manifest_path)) digest::digest(file = manifest_path, algo = "sha256") else NA_character_
  if (!identical(verified$input_md5, prepared$metadata$input_md5) ||
      !identical(verified$raw_manifest_sha256, manifest_sha) ||
      !identical(verified$mapping_status, "publication_and_source_labels_verified") ||
      !identical(verified$data_kind, "empirical")) stop("Source verification does not match the current input/archive.")
  prepared$metadata$mapping_status <- verified$mapping_status
  prepared$metadata$data_kind <- verified$data_kind
  prepared$metadata$raw_manifest_sha256 <- manifest_sha
}
data <- prepared$data
if (!all(1987:1996 %in% data$outcome_year)) stop("Expected all outcome years 1987–1996; inspect source timing.")
dir.create("data/derived", recursive = TRUE, showWarnings = FALSE)
dir.create("results", recursive = TRUE, showWarnings = FALSE)
run_dir <- tempfile("run-", tmpdir = "results")
dir.create(run_dir)
write_result <- function(x, name) write.csv(x, file.path(run_dir, name), row.names = FALSE, na = "")
write_result(prepared$exclusions, "exclusions.csv")
write_result(prepared$quality, "data-quality.csv")
write_result(describe_cps(data), "descriptives.csv")

events <- extensions <- overlap <- balance <- sensitivity <- failures <- samples <- bootstrap <- bootstrap_replicates <- bootstrap_status <- list()
bootstrap_index <- 0L
windows <- list(main = list(pre = 1987:1989, post = 1993:1996),
                later_coverage = list(pre = 1987:1989, post = 1995:1996),
                broader_pre = list(pre = 1987:1991, post = 1993:1996))
for (sex_value in c("male", "female")) for (age_value in c("young", "old")) {
  stratum <- droplevels(data[data$sex == sex_value & data$age_sample == age_value, ])
  for (outcome in c("weeks", "worked_any")) {
    message("Analyzing ", sex_value, "/", age_value, "/", outcome,
            "; stratum observations=", nrow(stratum))
    event <- fit_event_study(stratum, outcome)$coefficients
    events[[length(events) + 1L]] <- cbind(sex = sex_value, age_sample = age_value, event)
    # Nuisance models share the same complete-case sample for each outcome.
    keep <- complete.cases(stratum[, c(outcome, "age", "race")])
    eligible <- droplevels(stratum[keep, ])
    eligible$age_c <- (eligible$age - 40) / 10
    for (window in names(windows)) {
      label <- data.frame(sex = sex_value, age_sample = age_value, outcome = outcome, window = window)
      fit <- tryCatch({
        pooled <- pool_periods(eligible, windows[[window]]$pre, windows[[window]]$post)
        samples[[length(samples) + 1L]] <- cbind(label, n_stratum = nrow(stratum),
          n_complete = nrow(eligible), n_in_window = nrow(pooled))
        point <- estimate_did(pooled, outcome, "disabled", "post", ~ age_c + I(age_c^2) + race,
                              weights = "analysis_weight", min_probability = 1e-6, min_ess = 20)
        # The fitted linear-outcome estimator is linear in Y with fixed X/weights.
        # Its loading on a disability-specific year trend gives an exact
        # sensitivity curve for subtracting b * disabled * (outcome_year - 1987).
        pooled$trend_loading <- pooled$disabled * (pooled$outcome_year - 1987)
        drift <- estimate_did(pooled, "trend_loading", "disabled", "post",
                              ~ age_c + I(age_c^2) + race, weights = "analysis_weight",
                              min_probability = 1e-6, min_ess = 20)
        point$trend_loading <- drift$estimates$estimate[drift$estimates$method == "aipw_standardized_did"]
        point
      }, error = function(e) e)
      if (inherits(fit, "error")) {
        failures[[length(failures) + 1L]] <- cbind(label, analysis = "primary", reason = conditionMessage(fit))
      } else {
        extensions[[length(extensions) + 1L]] <- cbind(label, fit$estimates)
        overlap[[length(overlap) + 1L]] <- cbind(label, fit$cells)
        balance[[length(balance) + 1L]] <- cbind(label, fit$balance)
        theta <- fit$estimates$estimate[fit$estimates$method == "aipw_standardized_did"]
        bias <- if (outcome == "weeks") seq(-1, 1, .25) else seq(-.02, .02, .005)
        sensitivity[[length(sensitivity) + 1L]] <- cbind(label, data.frame(
          method = "aipw_standardized_did", estimate = theta,
          year_separation = fit$trend_loading, trend_bias = bias,
          adjusted_estimate = theta - bias * fit$trend_loading,
          zero_effect_slope = if (abs(fit$trend_loading) > 1e-10) theta / fit$trend_loading else NA_real_))
        if (window == "main" && bootstrap_B > 0L) {
          message("  Starting ", bootstrap_B, " full-refit bootstrap draws with ", bootstrap_workers, " workers")
          bootstrap_index <- bootstrap_index + 1L
          boot <- tryCatch(bootstrap_did(pooled, outcome, "disabled", "post",
            ~ age_c + I(age_c^2) + race, weights = "analysis_weight", B = bootstrap_B,
            seed = bootstrap_seed + bootstrap_index, min_probability = 1e-6, min_ess = 20,
            workers = bootstrap_workers),
            error = function(e) e)
          if (inherits(boot, "error")) {
            failures[[length(failures) + 1L]] <- cbind(label, analysis = "bootstrap", reason = conditionMessage(boot))
          } else {
            boot$summary$n_success <- boot$summary$successes
            boot$summary$n_requested <- boot$summary$B
            boot$summary$std_error <- boot$summary$bootstrap_se
            bootstrap[[length(bootstrap) + 1L]] <- cbind(label, boot$summary)
            bootstrap_replicates[[length(bootstrap_replicates) + 1L]] <- cbind(label, boot$replicates)
            bootstrap_status[[length(bootstrap_status) + 1L]] <- cbind(label, boot$replicate_status)
            message("  Completed bootstrap: ", boot$summary$successes[[1]], "/", bootstrap_B, " successful refits")
            if (!all(boot$summary$intervals_available)) {
              failures[[length(failures) + 1L]] <- cbind(label, analysis = "bootstrap", reason = "Bootstrap replicate failures suppress intervals; see bootstrap_replicates.csv.")
            }
          }
        }
      }
    }
  }
}
write_result(do.call(rbind, events), "event_study.csv")
bind_or_empty <- function(xs, columns) {
  if (length(xs)) dplyr::bind_rows(xs) else as.data.frame(setNames(rep(list(character()), length(columns)), columns))
}
labels <- c("sex", "age_sample", "outcome", "window")
extended <- run_extended_analyses(data, min_probability = 1e-6, min_ess = 20)
message("Extended analyses complete; failures=", nrow(extended$failures))
write_result(extended$placebo, "placebo.csv")
write_result(extended$distributional, "distributional.csv")
write_result(extended$rich, "extension_rich.csv")
write_result(extended$diagnostics, "extended_diagnostics.csv")
if (nrow(extended$failures)) failures[[length(failures) + 1L]] <- extended$failures
write_result(bind_or_empty(extensions, c(labels, "method", "estimate")), "extension.csv")
write_result(bind_or_empty(overlap, c(labels, "cell", "n", "transport_ess")), "overlap.csv")
write_result(bind_or_empty(balance, c(labels, "cell", "covariate", "smd_before", "smd_after")), "balance.csv")
write_result(bind_or_empty(sensitivity, c(labels, "method", "estimate", "year_separation", "trend_bias", "adjusted_estimate", "zero_effect_slope")), "sensitivity.csv")
failure_table <- bind_or_empty(failures, c(labels, "analysis", "reason"))
write_result(failure_table, "extension_failures.csv")
write_result(bind_or_empty(samples, c(labels, "n_stratum", "n_complete", "n_in_window")), "extension_samples.csv")
boot_table <- bind_or_empty(bootstrap, c(labels, "method", "estimate", "conf_low", "conf_high", "n_success", "n_requested", "intervals_available"))
write_result(boot_table, "bootstrap.csv")
write_result(bind_or_empty(bootstrap_replicates, c(labels, "method", "replicate", "estimate", "success", "failure_reason")), "bootstrap_replicates.csv")
write_result(bind_or_empty(bootstrap_status, c(labels, "replicate", "success", "failure_reason", "warnings")), "bootstrap_status.csv")
prepared$metadata$run_utc <- format(Sys.time(), tz = "UTC", usetz = TRUE)
prepared$metadata$git_commit <- system2("git", c("rev-parse", "HEAD"), stdout = TRUE)
prepared$metadata$code_md5 <- as.list(tools::md5sum(c(list.files("R", full.names = TRUE), "scripts/run_analysis.R")))
prepared$metadata$extension_failures <- nrow(failure_table)
prepared$metadata$bootstrap_requested <- bootstrap_B
prepared$metadata$bootstrap_seed <- bootstrap_seed
prepared$metadata$bootstrap_workers <- bootstrap_workers
prepared$metadata$support_numerical_floor <- 1e-6
prepared$metadata$support_warning_threshold <- .01
prepared$metadata$support_amendment <- "docs/support-amendment.md"
source("R/replication.R")
replication <- replicate_published(data, results_dir = run_dir, nthreads = 1L,
                                  population_counts = prepared$population_counts)
message("Published-specification comparisons: ", sum(replication$benchmarks$pass), "/", nrow(replication$benchmarks), " matched")
prepared$metadata$replication_status <- replication$status
saveRDS(prepared$metadata, file.path(run_dir, "run-metadata.rds"))
capture.output(sessionInfo(), file = file.path(run_dir, "session-info.txt"))
saveRDS(prepared, "data/derived/cps.rds")
source("R/export_tables.R")
export_tables(run_dir, file.path(run_dir, "tables"))
# Publish a complete set only after all core computations have finished.
for (relative in list.files(run_dir, recursive = TRUE)) {
  destination <- file.path("results", relative)
  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
  if (!file.copy(file.path(run_dir, relative), destination, overwrite = TRUE)) stop("Cannot publish result: ", relative)
}
cat("Saved reanalysis results. Source mapping status:", prepared$metadata$mapping_status, "\n")
if (nrow(failure_table)) stop(nrow(failure_table), " extension cells/windows failed diagnostics; see results/extension_failures.csv. No trimming or silent substitution applied.")
