# Reconstruction of selected Tables 1/2 from the supplied July 2, 2001 proof.
# This is a paper-described model reconstruction, not an original SAS translation.
# Reference values are transcribed independently from proof pages 13, 14, and 17.

published_replication_targets <- function() {
  rows <- list()
  add <- function(table, sex, age_sample, outcome, statistic, year, disabled,
                  values, page, column) {
    text <- as.character(values)
    precision <- if (statistic == "n") rep(0, length(text)) else {
      decimals <- ifelse(grepl(".", text, fixed = TRUE),
                         nchar(sub("^[^.]*\\.", "", text)), 0L)
      .5 * 10^(-decimals)
    }
    rows[[length(rows) + 1L]] <<- data.frame(table = table, sex = sex,
      age_sample = age_sample, outcome = outcome, statistic = statistic,
      year = year, disabled = disabled, target = as.numeric(text), target_text = text,
      tolerance = precision, source_page = page, source_column = column,
      source = "Supplied Acemoglu-Angrist proof, July 2 2001; not independently checked against final print")
  }
  # Each Table 1 vector alternates disabled/nondisabled within survey years
  # 1988, 1990, 1992, 1994, 1996. Preserve printed precision in strings.
  table1 <- list(
    male_young = list(sex = "male", age = "young", page = 13,
      age_mean = c("31.1", "29.9", "31.2", "30.0", "31.4", "30.1", "31.5", "30.3", "32.0", "30.4"),
      weeks = c("20.4", "45.1", "22.8", "45.6", "18.8", "44.1", "17.8", "44.2", "17.4", "45.1"),
      n = c(1059, 21532, 996, 21853, 1063, 21254, 1050, 19683, 770, 16492)),
    female_young = list(sex = "female", age = "young", page = 13,
      age_mean = c("30.8", "29.9", "31.3", "30.0", "31.2", "30.2", "32.1", "30.3", "31.9", "30.4"),
      weeks = c("17.7", "34.6", "19.8", "35.0", "18.8", "35.1", "14.2", "35.1", "16.1", "35.9"),
      n = c(932, 23967, 927, 24022, 963, 23445, 995, 21761, 881, 18262)),
    male_old = list(sex = "male", age = "old", page = 14,
      age_mean = c("49.7", "47.9", "49.3", "47.6", "48.9", "47.6", "49.3", "47.6", "49.0", "47.6"),
      weeks = c("16.4", "47.5", "16.2", "47.9", "16.0", "46.9", "13.6", "46.9", "14.3", "47.2"),
      n = c(1387, 13743, 1491, 14133, 1533, 14558, 1579, 14267, 1429, 13109)),
    female_old = list(sex = "female", age = "old", page = 14,
      age_mean = c("50.2", "47.9", "49.9", "47.7", "49.3", "47.6", "49.5", "47.6", "49.3", "47.7"),
      weeks = c("11.2", "35.1", "11.7", "36.5", "11.6", "37.3", "13.3", "37.9", "11.3", "38.9"),
      n = c(1384, 14726, 1518, 15345, 1579, 15564, 1699, 15665, 1650, 14026)))
  for (spec in table1) {
    for (outcome in c("age", "weeks", "sample")) {
      field <- switch(outcome, age = "age_mean", weeks = "weeks", sample = "n")
      add("Table 1", spec$sex, spec$age, outcome,
          if (outcome == "sample") "n" else "mean",
          rep(seq(1988, 1996, 2), each = 2), rep(c(1L, 0L), 5), spec[[field]],
          if (spec$sex == "male" && spec$age == "old" && outcome != "sample") 13 else spec$page,
          rep(as.character(1:10), 1))
    }
  }
  # Table 2 baseline columns 1/3 (weeks) and 5/7 (log earnings), outcome years
  # 1988--1996 relative to 1987. Trend columns are outside this selected target.
  table2 <- list(
    list(sex = "male", age = "young", outcome = "weeks", column = "1", n = 193317,
      coefficient = c("-0.41", "2.00", "-0.79", "-0.53", "0.57", "-1.44", "-1.63", "-2.93", "-2.68"),
      se = c("0.71", "0.71", "0.71", "0.70", "0.69", "0.69", "0.70", "0.72", "0.70")),
    list(sex = "female", age = "young", outcome = "weeks", column = "3", n = 211910,
      coefficient = c("-1.08", "0.67", "-1.33", "0.08", "-2.81", "-4.37", "-5.00", "-3.93", "-4.41"),
      se = c("1.07", "1.06", "1.05", "1.04", "1.02", "1.02", "1.01", "1.01", "1.03")),
    list(sex = "male", age = "old", outcome = "weeks", column = "1", n = 146309,
      coefficient = c("-0.49", "-0.47", "-0.22", "0.34", "0.77", "-2.12", "-1.57", "-1.83", "-0.75"),
      se = c("0.55", "0.58", "0.58", "0.57", "0.56", "0.56", "0.55", "0.55", "0.55")),
    list(sex = "female", age = "old", outcome = "weeks", column = "3", n = 157589,
      coefficient = c("0.29", "-0.94", "-1.52", "-1.91", "-2.24", "-0.92", "-1.24", "-3.68", "-3.39"),
      se = c("0.88", "0.88", "0.87", "0.86", "0.87", "0.85", "0.83", "0.82", "0.81")),
    list(sex = "male", age = "young", outcome = "log_weekly_wage", column = "5", n = 167974,
      coefficient = c("-0.052", "-0.062", "-0.035", "-0.020", "-0.00", "-0.04", "-0.143", "-0.098", "-0.158"),
      se = c("0.044", "0.043", "0.044", "0.043", "0.043", "0.044", "0.044", "0.046", "0.044")),
    list(sex = "female", age = "young", outcome = "log_weekly_wage", column = "7", n = 157688,
      coefficient = c("-0.055", "0.050", "-0.111", "0.105", "-0.010", "-0.058", "-0.043", "0.005", "-0.111"),
      se = c("0.052", "0.050", "0.050", "0.050", "0.050", "0.050", "0.050", "0.049", "0.050")),
    list(sex = "male", age = "old", outcome = "log_weekly_wage", column = "5", n = 116665,
      coefficient = c("-0.020", "-0.109", "-0.064", "-0.109", "-0.191", "-0.068", "-0.10", "-0.10", "-0.064"),
      se = c("0.044", "0.043", "0.044", "0.043", "0.042", "0.042", "0.042", "0.042", "0.041")),
    list(sex = "female", age = "old", outcome = "log_weekly_wage", column = "7", n = 109082,
      coefficient = c("0.111", "0.039", "0.130", "-0.012", "0.065", "0.098", "0.007", "-0.027", "-0.096"),
      se = c("0.053", "0.053", "0.053", "0.052", "0.053", "0.051", "0.051", "0.049", "0.048")))
  for (spec in table2) {
    add("Table 2", spec$sex, spec$age, spec$outcome, "coefficient", 1988:1996, NA_integer_,
        spec$coefficient, 17, spec$column)
    add("Table 2", spec$sex, spec$age, spec$outcome, "std_error", 1988:1996, NA_integer_,
        spec$se, 17, spec$column)
    add("Table 2", spec$sex, spec$age, spec$outcome, "n", NA_integer_, NA_integer_,
        spec$n, 17, spec$column)
  }
  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  result
}

compare_replication_benchmarks <- function(observed, targets = published_replication_targets()) {
  keys <- c("table", "sex", "age_sample", "outcome", "statistic", "year", "disabled")
  if (!all(c(keys, "reproduced") %in% names(observed))) stop("Missing observed benchmark columns")
  if (anyDuplicated(observed[keys]) || anyDuplicated(targets[keys])) stop("Duplicate replication target keys")
  targets$.target_order <- seq_len(nrow(targets))
  comparison <- merge(targets, observed[c(keys, "reproduced", intersect("count_source", names(observed)))], by = keys, all.x = TRUE, sort = FALSE)
  comparison <- comparison[order(comparison$.target_order), ]
  comparison$.target_order <- NULL
  comparison$difference <- comparison$reproduced - comparison$target
  comparison$absolute_difference <- abs(comparison$difference)
  # Floating-point slack is tiny compared with each source's printed rounding.
  comparison$pass <- is.finite(comparison$reproduced) &
    comparison$absolute_difference <= comparison$tolerance + 1e-10
  comparison$status <- ifelse(!is.finite(comparison$reproduced), "unrun",
                               ifelse(comparison$pass, "matched_printed_precision", "discrepancy"))
  comparison$original_value <- comparison$target
  comparison$reproduced_value <- comparison$reproduced
  comparison$passed <- comparison$pass
  comparison$benchmark_id <- apply(comparison[keys], 1L, paste, collapse = "|")
  rownames(comparison) <- NULL
  comparison
}

replicate_published <- function(data, results_dir = NULL, nthreads = 2L, population_counts = NULL) {
  required <- c("sex", "age_sample", "survey_year", "outcome_year", "disabled",
                "weight", "age", "age_group", "race", "education", "region", "weeks",
                "log_nominal_weekly_wage", "legacy_wage_eligible")
  if (!is.data.frame(data) || !all(required %in% names(data))) stop("Replication needs canonical CPS fields")
  if (anyNA(data[c("sex", "age_sample", "survey_year", "outcome_year", "disabled", "weight")]) ||
      any(!is.finite(data$weight) | data$weight <= 0) || !all(data$disabled %in% 0:1)) {
    stop("Invalid group, year, or weight fields in replication input")
  }
  if (any(data$outcome_year != data$survey_year - 1)) stop("Replication requires outcome_year = survey_year - 1")
  if (!is.null(population_counts)) {
    count_keys <- c("survey_year", "sex", "age_sample", "disabled")
    if (!is.data.frame(population_counts) || !all(c(count_keys, "n") %in% names(population_counts)) ||
        anyDuplicated(population_counts[count_keys])) stop("Invalid raw population count table")
  }
  table1 <- table2 <- audit <- list()
  for (sex_value in c("male", "female")) for (age_value in c("young", "old")) {
    stratum <- droplevels(data[data$sex == sex_value & data$age_sample == age_value, ])
    for (year in seq(1988, 1996, 2)) for (disabled in c(1L, 0L)) {
      cell <- stratum[stratum$survey_year == year & stratum$disabled == disabled, ]
      if (!nrow(cell)) stop("Empty Table 1 demographic/year/disability cell")
      means <- vapply(c("age", "weeks"), function(name) {
        keep <- is.finite(cell[[name]])
        if (!any(keep)) return(NA_real_)
        weighted.mean(cell[[name]][keep], cell$weight[keep])
      }, numeric(1))
      count <- nrow(cell)
      count_source <- "canonical_positive_weight_fallback_not_raw_population"
      if (!is.null(population_counts)) {
        selected_count <- with(population_counts, which(survey_year == year & sex == sex_value &
                                                        age_sample == age_value & disabled == cell$disabled[[1]]))
        if (length(selected_count) != 1L) stop("Missing unique raw Table 1 population count")
        count <- population_counts$n[selected_count]
        if (!is.finite(count) || count < nrow(cell) || count != floor(count)) stop("Invalid raw Table 1 population count")
        count_source <- "age_eligible_raw_records_before_weight_exclusion"
      }
      table1[[length(table1) + 1L]] <- data.frame(table = "Table 1", sex = sex_value,
        age_sample = age_value, outcome = c("age", "weeks", "sample"),
        statistic = c("mean", "mean", "n"), year = year, disabled = disabled,
        reproduced = c(means, count),
        count_source = c(rep("positive_weight_mean", 2), count_source), row.names = NULL)
    }
    for (outcome in c("weeks", "log_weekly_wage")) {
      response <- if (outcome == "weeks") "weeks" else "log_nominal_weekly_wage"
      keep <- complete.cases(stratum[c(response, "age_group", "race", "education", "region")]) &
        is.finite(stratum[[response]])
      if (outcome == "log_weekly_wage") keep <- keep & !is.na(stratum$legacy_wage_eligible) & stratum$legacy_wage_eligible
      sample <- droplevels(stratum[keep, ])
      if (!all(1987:1996 %in% sample$outcome_year)) stop("Table 2 requires all outcome years 1987--1996")
      if (any(table(factor(sample$outcome_year, levels = 1987:1996),
                    factor(sample$disabled, levels = 0:1)) == 0)) stop("Table 2 requires both groups in every year")
      # Direct categorical expansion preserves the documented design and makes
      # conventional WLS residual degrees of freedom explicit (150 in real data).
      formula <- as.formula(paste(response,
        "~ disabled * factor(outcome_year) + factor(outcome_year) * (age_group + race + education + region)"))
      model <- fixest::feols(formula, data = sample, weights = ~ weight,
        vcov = "iid", nthreads = nthreads, lean = TRUE, notes = FALSE)
      coefficients <- fixest::coeftable(model)
      names_expected <- paste0("disabled:factor(outcome_year)", 1988:1996)
      if (!all(names_expected %in% rownames(coefficients))) stop("Unidentified Table 2 interaction")
      coefficients <- coefficients[names_expected, , drop = FALSE]
      table2[[length(table2) + 1L]] <- data.frame(table = "Table 2", sex = sex_value,
        age_sample = age_value, outcome = outcome,
        statistic = c(rep("coefficient", 9), rep("std_error", 9), "n"),
        year = c(1988:1996, 1988:1996, NA_integer_), disabled = NA_integer_,
        reproduced = c(coefficients[, 1], coefficients[, 2], nobs(model)),
        count_source = "positive_weight_complete_estimation_sample")
      audit[[length(audit) + 1L]] <- data.frame(sex = sex_value, age_sample = age_value,
        outcome = outcome, n_stratum = nrow(stratum), n_estimation = nobs(model),
        excluded = nrow(stratum) - nobs(model), parameters = model$nparams,
        reference_outcome_year = 1987L, weight = "FNLWGT2 / 100",
        covariance = "Conventional homoskedastic WLS; reconstruction checked against printed SEs",
        formula = paste(deparse(formula), collapse = " "),
        wage_selection = if (outcome == "weeks") "No wage restriction" else
          "Positive weeks/earnings, legacy survey-year-CPI real weekly wage in [25,2000]; no original SAS supplied")
      rm(model)
    }
  }
  table1 <- do.call(rbind, table1)
  table2 <- do.call(rbind, table2)
  audit <- do.call(rbind, audit)
  benchmarks <- compare_replication_benchmarks(rbind(table1, table2))
  status <- if (all(benchmarks$pass)) "benchmarks_verified" else "reconstruction_discrepancies_reported"
  if (!is.null(results_dir)) {
    dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
    for (entry in list(c("replication_table1.csv", "table1"), c("replication_table2.csv", "table2"),
                       c("replication_models.csv", "audit"), c("replication_benchmarks.csv", "benchmarks"))) {
      write.csv(get(entry[[2]]), file.path(results_dir, entry[[1]]), row.names = FALSE, na = "")
    }
  }
  list(table1 = table1, table2 = table2, model_audit = audit, benchmarks = benchmarks, status = status)
}
