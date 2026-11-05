# Offline integration only: all records are simulated, and every generated file
# lives in a temporary project. This test is not an empirical replication.
run_pipeline_test <- function() {
  stopifnot(requireNamespace("haven", quietly = TRUE))
  project <- normalizePath(".")
  stopifnot(file.exists(file.path(project, "scripts", "run_analysis.R")))
  sandbox <- tempfile("stat256-synthetic-pipeline-")
  dir.create(sandbox)
  on.exit(unlink(sandbox, recursive = TRUE), add = TRUE)
  stopifnot(all(file.copy(file.path(project, c("R", "scripts")),
                         sandbox, recursive = TRUE)))
  previous_directory <- getwd()
  setwd(sandbox)
  on.exit(setwd(previous_directory), add = TRUE, after = FALSE)

  # Give the child pipeline real local Git metadata without touching the source
  # checkout or requiring an external remote, account, or network connection.
  git <- function(args) {
    status <- system2("git", args, stdout = FALSE, stderr = FALSE)
    stopifnot(status == 0L)
  }
  git(c("init", "--quiet"))
  git(c("add", "R", "scripts"))
  git(c("-c", "user.name=PipelineTest", "-c", "user.email=pipeline@example.invalid",
        "-c", "commit.gpgsign=false", "commit", "--quiet", "-m", "synthetic-test-fixture"))

  set.seed(2562026)
  grid <- expand.grid(YEAR = 88:97, SEX = 1:2, age_sample = c("young", "old"),
                      replicate = seq_len(240), KEEP.OUT.ATTRS = FALSE,
                      stringsAsFactors = FALSE)
  n <- nrow(grid)
  age <- ifelse(grid$age_sample == "young", sample(21:39, n, replace = TRUE),
                sample(40:58, n, replace = TRUE))
  disability <- rbinom(n, 1, .15)
  race <- sample(1:3, n, replace = TRUE)
  schooling <- sample(1:4, n, replace = TRUE)
  post <- as.integer(grid$YEAR - 1 >= 93)
  employed <- rbinom(n, 1, plogis(1.8 - .8 * disability - .25 * disability * post +
                                  .15 * (age - 40) / 10))
  weeks <- employed * pmax(1, pmin(52, round(42 - 6 * disability -
    2 * disability * post + .1 * (age - 40) + rnorm(n, sd = 6))))
  raw <- data.frame(YEAR = as.numeric(grid$YEAR), AGE = as.numeric(age),
                    SEX = as.numeric(grid$SEX), DISABL1 = as.numeric(disability),
                    FNLWGT2 = runif(n, 75, 125), WKSWORK = as.numeric(weeks),
                    RACE = as.numeric(race), LESSHS = as.numeric(schooling == 1),
                    HSGRAD = as.numeric(schooling == 2), SOMECO = as.numeric(schooling == 3),
                    COLGRAD = as.numeric(schooling == 4),
                    REGION = as.numeric(sample(1:9, n, replace = TRUE)),
                    WSAL_VAL = weeks * runif(n, 300, 800))
  dir.create("data/raw", recursive = TRUE)
  source_path <- "data/raw/marcps_w.sas7bdat"
  haven::write_sas(raw, source_path)

  # A plausible-looking audit record for the wrong file must fail before any
  # fitting or result publication. This deliberately invalid fixture never
  # attests that simulated input is empirical data.
  writeLines('{"fixture": "simulated test manifest"}', "data/raw/manifest.json")
  jsonlite::write_json(list(input_md5 = paste(rep("0", 32), collapse = ""),
    raw_manifest_sha256 = digest::digest(file = "data/raw/manifest.json", algo = "sha256"),
    mapping_status = "original_verified", data_kind = "empirical"),
    "data/source-verification.json", auto_unbox = TRUE)
  rejection_log <- file.path(sandbox, "verification-rejection.log")
  rejection <- system2(file.path(R.home("bin"), "Rscript"),
    c("--vanilla", "scripts/run_analysis.R"), stdout = rejection_log, stderr = rejection_log)
  stopifnot(rejection != 0L, !dir.exists("results"), !dir.exists("data/derived"),
    any(grepl("Source verification does not match the current input/archive",
              readLines(rejection_log, warn = FALSE), fixed = TRUE)))
  unlink(c("data/source-verification.json", "data/raw/manifest.json"))

  # Exercise file discovery, SAS parsing, preparation, every analysis window,
  # diagnostics, serialization, and publication as one actual subprocess.
  bootstrap_B <- 3L # Exploratory integration check, not production uncertainty.
  output_log <- file.path(sandbox, "pipeline.log")
  status <- system2(file.path(R.home("bin"), "Rscript"),
                    c("--vanilla", "scripts/run_analysis.R", "--bootstrap", as.character(bootstrap_B),
                      "--seed", "256"),
                    stdout = output_log, stderr = output_log)
  if (status != 0L) {
    cat(readLines(output_log, warn = FALSE), sep = "\n")
    stop("Synthetic pipeline integration failed with exit status ", status)
  }
  expected_csv <- c("exclusions.csv", "data-quality.csv", "descriptives.csv",
                    "event_study.csv", "extension.csv", "overlap.csv",
                    "balance.csv", "sensitivity.csv", "placebo.csv", "distributional.csv",
                    "extension_rich.csv", "extended_diagnostics.csv", "bootstrap.csv",
                    "bootstrap_replicates.csv", "bootstrap_status.csv",
                    "extension_failures.csv", "extension_samples.csv")
  stopifnot(all(file.exists(file.path("results", expected_csv))),
            file.exists("results/run-metadata.rds"),
            file.info("results/session-info.txt")$size > 0,
            file.exists("data/derived/cps.rds"))
  read_result <- function(name) read.csv(file.path("results", name),
                                        stringsAsFactors = FALSE)
  labels <- c("sex", "age_sample", "outcome", "window")
  expected_labels <- expand.grid(sex = c("male", "female"),
    age_sample = c("young", "old"), outcome = c("weeks", "worked_any"),
    window = c("main", "later_coverage", "broader_pre"), stringsAsFactors = FALSE)
  key <- function(x, columns) unname(apply(x[, columns, drop = FALSE], 1L, paste, collapse = "|"))
  methods <- c("unadjusted_did", "aipw_standardized_did",
               "outcome_standardized_did", "ipw_standardized_did")
  estimates <- read_result("extension.csv")
  expected_estimates <- merge(expected_labels, data.frame(method = methods), by = NULL)
  stopifnot(nrow(estimates) == 96L, all(is.finite(estimates$estimate)),
    identical(sort(key(estimates, c(labels, "method"))),
              sort(key(expected_estimates, c(labels, "method")))))

  main_estimates <- estimates[estimates$window == "main", ]
  main_labels <- expected_labels[expected_labels$window == "main", ]
  placebo <- read_result("placebo.csv")
  expected_placebo <- expected_estimates[expected_estimates$window == "main", ]
  expected_placebo$window <- "pre_policy"
  stopifnot(nrow(placebo) == 32L, all(is.finite(placebo$estimate)),
    identical(sort(key(placebo, c(labels, "method"))),
              sort(key(expected_placebo, c(labels, "method")))),
    all(placebo$n_window == 720L))
  distributional <- read_result("distributional.csv")
  expected_distributional <- expand.grid(sex = c("male", "female"),
    age_sample = c("young", "old"), outcome = "cdf_weeks", window = "main",
    threshold = c(0, 13, 26, 39, 51), method = methods, stringsAsFactors = FALSE)
  stopifnot(nrow(distributional) == 80L, all(is.finite(distributional$estimate)),
    identical(sort(key(distributional, c(labels, "threshold", "method"))),
              sort(key(expected_distributional, c(labels, "threshold", "method")))))
  # With nonnegative weeks, I(weeks<=0) = 1-worked_any. All four linear
  # contrasts must therefore have opposite signs on the identical sample.
  zero_cdf <- distributional[distributional$threshold == 0, ]
  work <- main_estimates[main_estimates$outcome == "worked_any", ]
  complement <- merge(zero_cdf, work, by = c("sex", "age_sample", "window", "method"),
                       suffixes = c("_cdf", "_work"))
  stopifnot(nrow(complement) == 16L,
            max(abs(complement$estimate_cdf + complement$estimate_work)) < 1e-8)
  rich <- read_result("extension_rich.csv")
  expected_rich <- merge(expected_estimates[expected_estimates$window == "main", ],
    data.frame(covariate_set = c("primary_same_sample", "rich")), by = NULL)
  stopifnot(nrow(rich) == 64L, all(is.finite(rich$estimate)),
    identical(sort(key(rich, c(labels, "covariate_set", "method"))),
              sort(key(expected_rich, c(labels, "covariate_set", "method")))),
    all(rich$n_complete == 2400L), all(rich$n_window == 1680L))
  same_sample <- merge(rich[rich$covariate_set == "primary_same_sample", ], main_estimates,
                        by = c(labels, "method"), suffixes = c("_same", "_primary"))
  stopifnot(nrow(same_sample) == 32L,
            max(abs(same_sample$estimate_same - same_sample$estimate_primary)) < 1e-8)
  extended_diagnostics <- read_result("extended_diagnostics.csv")
  extended_cases <- unique(rbind(placebo[, c("analysis", labels, "threshold", "covariate_set")],
    distributional[, c("analysis", labels, "threshold", "covariate_set")],
    rich[, c("analysis", labels, "threshold", "covariate_set")]))
  extended_keys <- c("analysis", labels, "threshold", "covariate_set")
  stopifnot(nrow(extended_diagnostics) == 44L,
    identical(sort(key(extended_diagnostics, extended_keys)), sort(key(extended_cases, extended_keys))),
    all(extended_diagnostics$min_transport_ess >= 20),
    all(extended_diagnostics$min_probability_in_target >= .01),
    all(extended_diagnostics$excluded_complete == 0L))

  bootstrap <- read_result("bootstrap.csv")
  replicates <- read_result("bootstrap_replicates.csv")
  bootstrap_status <- read_result("bootstrap_status.csv")
  expected_bootstrap <- expected_estimates[expected_estimates$window == "main", ]
  expected_replicates <- merge(expected_bootstrap, data.frame(replicate = seq_len(bootstrap_B)), by = NULL)
  expected_status <- merge(main_labels, data.frame(replicate = seq_len(bootstrap_B)), by = NULL)
  stopifnot(nrow(bootstrap) == 32L, nrow(replicates) == bootstrap_B * 32L,
    nrow(bootstrap_status) == bootstrap_B * 8L,
    identical(sort(key(bootstrap, c(labels, "method"))), sort(key(expected_bootstrap, c(labels, "method")))),
    identical(sort(key(replicates, c(labels, "method", "replicate"))),
              sort(key(expected_replicates, c(labels, "method", "replicate")))),
    identical(sort(key(bootstrap_status, c(labels, "replicate"))),
              sort(key(expected_status, c(labels, "replicate")))),
    all(bootstrap$n_success == bootstrap_B), all(bootstrap$n_requested == bootstrap_B),
    all(bootstrap$intervals_available), all(bootstrap$precision_label == "exploratory_B_less_than_199"),
    all(is.finite(bootstrap$std_error)), all(bootstrap$std_error >= 0),
    all(bootstrap$conf_low <= bootstrap$conf_high), all(replicates$success),
    all(is.finite(replicates$estimate)), all(bootstrap_status$success),
    all(bootstrap_status$n == 1680L),
    all(bootstrap_status$n_00 + bootstrap_status$n_01 + bootstrap_status$n_10 + bootstrap_status$n_11 == bootstrap_status$n))
  point_comparison <- merge(bootstrap, main_estimates, by = c(labels, "method"),
                             suffixes = c("_bootstrap", "_primary"))
  stopifnot(nrow(point_comparison) == 32L,
            max(abs(point_comparison$estimate_bootstrap - point_comparison$estimate_primary)) < 1e-8)

  overlap <- read_result("overlap.csv")
  # read.csv interprets labels such as 00 as integers; normalize for comparison.
  overlap$cell <- sprintf("%02d", as.integer(overlap$cell))
  expected_cells <- merge(expected_labels, data.frame(cell = c("00", "01", "10", "11")), by = NULL)
  stopifnot(nrow(overlap) == 96L,
    identical(sort(key(overlap, c(labels, "cell"))),
              sort(key(expected_cells, c(labels, "cell")))),
    all(is.finite(overlap$transport_ess)), all(overlap$transport_ess >= 20),
    all(overlap$min_probability_in_target >= .01),
    all(overlap$max_transport_weight_share > 0 & overlap$max_transport_weight_share <= 1),
    nrow(read_result("extension_failures.csv")) == 0L)
  balance <- read_result("balance.csv")
  balance$cell <- sprintf("%02d", as.integer(balance$cell))
  expected_balance <- merge(expected_cells,
    data.frame(covariate = c("age_c", "I(age_c^2)", "race2", "race3")), by = NULL)
  stopifnot(nrow(balance) == 384L,
    identical(sort(key(balance, c(labels, "cell", "covariate"))),
              sort(key(expected_balance, c(labels, "cell", "covariate")))),
    all(is.finite(balance$smd_before)), all(is.finite(balance$smd_after)),
    all(balance$target_sd > 0), !any(balance$target_variance_zero),
    all(abs(balance$smd_before[balance$cell == "11"]) < 1e-10),
    all(abs(balance$smd_after[balance$cell == "11"]) < 1e-10))

  sensitivity <- read_result("sensitivity.csv")
  expected_sensitivity <- do.call(rbind, lapply(seq_len(nrow(expected_labels)), function(i) {
    row <- expected_labels[i, , drop = FALSE]
    bias <- if (row$outcome == "weeks") seq(-1, 1, .25) else seq(-.02, .02, .005)
    cbind(row[rep(1L, length(bias)), , drop = FALSE], trend_bias = bias)
  }))
  stopifnot(nrow(sensitivity) == 216L,
    identical(sort(key(sensitivity, c(labels, "trend_bias"))),
              sort(key(expected_sensitivity, c(labels, "trend_bias")))),
    all(sensitivity$method == "aipw_standardized_did"),
    all(is.finite(sensitivity$adjusted_estimate)),
    all(is.finite(sensitivity$year_separation)))
  baseline <- sensitivity[abs(sensitivity$trend_bias) < 1e-12, ]
  original_aipw <- estimates[estimates$method == "aipw_standardized_did", ]
  baseline_comparison <- merge(baseline, original_aipw, by = labels,
                               suffixes = c("_curve", "_original"))
  stopifnot(nrow(baseline_comparison) == 24L,
    max(abs(baseline_comparison$adjusted_estimate - baseline_comparison$estimate_original)) < 1e-10,
    max(abs(baseline_comparison$estimate_curve - baseline_comparison$estimate_original)) < 1e-10)

  samples <- read_result("extension_samples.csv")
  stopifnot(nrow(samples) == 24L,
            identical(sort(key(samples, labels)), sort(key(expected_labels, labels))),
            all(samples$n_stratum == 2400L), all(samples$n_complete == 2400L),
            all(samples$n_in_window[samples$window == "main"] == 1680L),
            all(samples$n_in_window[samples$window == "later_coverage"] == 1200L),
            all(samples$n_in_window[samples$window == "broader_pre"] == 2160L))

  events <- read_result("event_study.csv")
  expected_events <- expand.grid(sex = c("male", "female"), age_sample = c("young", "old"),
    outcome = c("weeks", "worked_any"), year = setdiff(1987:1996, 1989), stringsAsFactors = FALSE)
  stopifnot(nrow(events) == 72L,
    identical(sort(key(events, c("sex", "age_sample", "outcome", "year"))),
              sort(key(expected_events, c("sex", "age_sample", "outcome", "year")))),
    all(is.finite(events$estimate)), all(is.finite(events$std_error)),
    all(events$std_error > 0), all(events$conf_low < events$conf_high),
    all(events$reference == 1989L), all(events$n == 2400L), all(events$excluded == 0L))
  descriptive <- read_result("descriptives.csv")
  stopifnot(nrow(descriptive) == 80L, sum(descriptive$n) == n,
            setequal(descriptive$outcome_year, 1987:1996),
            all(descriptive$mean_weeks >= 0 & descriptive$mean_weeks <= 52),
            all(descriptive$mean_worked_any > 0 & descriptive$mean_worked_any < 1),
            sum(read_result("exclusions.csv")$n) == 0L)
  metadata <- readRDS("results/run-metadata.rds")
  prepared <- readRDS("data/derived/cps.rds")
  stopifnot(metadata$input_rows == n, metadata$retained_rows == n,
            identical(metadata$input_md5, unname(tools::md5sum(source_path))),
            grepl("^[[:xdigit:]]{40}$", metadata$git_commit),
            metadata$extension_failures == 0L,
            identical(metadata$data_kind, "unverified"),
            metadata$bootstrap_requested == bootstrap_B, metadata$bootstrap_seed == 256L,
            grepl("unverified", metadata$mapping_status, ignore.case = TRUE),
            identical(metadata$replication_status, "reconstruction_discrepancies_reported"),
            nrow(prepared$data) == n,
            all(prepared$data$outcome_year == prepared$data$survey_year - 1L))
  table_names <- c("01_annual_descriptives", "02_main_estimators", "03_bootstrap",
                   "04_placebo", "04_distributional", "04_extension_rich", "05_failures")
  table_paths <- unlist(lapply(c("md", "tex"), function(extension) {
    file.path("results", "tables", paste0(table_names, ".", extension))
  }))
  stopifnot(all(file.exists(table_paths)), all(file.info(table_paths)$size > 100))
  # Parse rendered table cells and compare their rounded estimates against the
  # actual CSV, rather than merely testing that a file or number is present.
  markdown <- readLines("results/tables/02_main_estimators.md", warn = FALSE)
  table_rows <- markdown[grepl("^\\|", markdown)]
  table_rows <- table_rows[!grepl("^\\|[ :|\\-]+\\|$", table_rows)]
  table_rows <- table_rows[-1L] # header
  rendered <- do.call(rbind, lapply(table_rows, function(row) {
    trimws(strsplit(sub("\\|$", "", sub("^\\|", "", row)), "|", fixed = TRUE)[[1]])
  }))
  stopifnot(nrow(rendered) == 32L, ncol(rendered) == 5L,
            identical(unname(rendered[, 1]), main_estimates$sex),
            identical(unname(rendered[, 2]), main_estimates$age_sample),
            identical(unname(rendered[, 3]), main_estimates$outcome),
            identical(unname(rendered[, 4]), main_estimates$method),
            max(abs(as.numeric(rendered[, 5]) - main_estimates$estimate)) <= .000500001)
  latex <- readLines("results/tables/02_main_estimators.tex", warn = FALSE)
  latex_rows <- latex[grepl("^(male|female) &", latex)]
  latex_estimates <- vapply(strsplit(latex_rows, "&", fixed = TRUE), function(row) {
    as.numeric(gsub("[^0-9eE+.-]", "", tail(row, 1L)))
  }, numeric(1))
  stopifnot(length(latex_estimates) == 32L,
            max(abs(latex_estimates - main_estimates$estimate)) <= .000500001)

  # Refit the actual estimator on changed outcomes, rather than merely checking
  # the sensitivity file against its own subtraction formula. Recreate the exact
  # complete-case sample, centered design, and equal-year/period weights.
  source("R/event_study.R", local = TRUE)
  source("R/estimate_did.R", local = TRUE)
  selected <- droplevels(prepared$data[prepared$data$sex == "male" &
                                        prepared$data$age_sample == "young", ])
  selected <- droplevels(selected[complete.cases(selected[, c("weeks", "age", "race")]), ])
  selected$age_c <- (selected$age - 40) / 10
  selected <- pool_periods(selected, pre = 1987:1989, post = 1993:1996)
  chosen_curve <- sensitivity[sensitivity$sex == "male" & sensitivity$age_sample == "young" &
                               sensitivity$outcome == "weeks" & sensitivity$window == "main", ]
  for (bias in c(-.75, .5)) {
    selected$adjusted_weeks <- selected$weeks - bias * selected$disabled *
      (selected$outcome_year - 1987)
    refit <- estimate_did(selected, "adjusted_weeks", "disabled", "post",
                         ~ age_c + I(age_c^2) + race, weights = "analysis_weight",
                         min_probability = .01, min_ess = 20)
    actual <- refit$estimates$estimate[refit$estimates$method == "aipw_standardized_did"]
    reported <- chosen_curve$adjusted_estimate[abs(chosen_curve$trend_bias - bias) < 1e-12]
    stopifnot(length(reported) == 1L, abs(actual - reported) < 1e-7)
  }
  # Independently verify the reported balance target from prepared records.
  target <- selected$disabled == 1 & selected$post == 1
  target_mean <- weighted.mean(selected$age_c[target], selected$analysis_weight[target])
  target_sd <- sqrt(weighted.mean((selected$age_c[target] - target_mean)^2,
                                  selected$analysis_weight[target]))
  chosen_balance <- balance[balance$sex == "male" & balance$age_sample == "young" &
                             balance$outcome == "weeks" & balance$window == "main" &
                             balance$covariate == "age_c", ]
  stopifnot(nrow(chosen_balance) == 4L,
            max(abs(chosen_balance$target_mean - target_mean)) < 1e-10,
            max(abs(chosen_balance$target_sd - target_sd)) < 1e-10)
  cat("PASS: offline synthetic SAS-to-results integration; primary and extended analyses, exploratory full-refit bootstrap, CSV-derived research tables, sensitivity refits, and unverified-source guards verified. No empirical results tested.\n")
}
run_pipeline_test()
