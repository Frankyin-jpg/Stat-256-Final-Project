# Canonical CPS preparation. Source labels and the supplied original-paper
# descriptions have been audited; numerical replication is checked separately.
# The provided archive does not contain the original SAS programs. A caller
# must bind empirical verification to the exact input hash; arbitrary inputs
# passed to prepare_cps() never acquire a verified provenance label automatically.
# This module never imposes a global even-year or positive-wage restriction.

prepare_cps <- function(raw) {
  required <- c("YEAR", "AGE", "SEX", "DISABL1", "FNLWGT2", "WKSWORK",
                "RACE", "LESSHS", "HSGRAD", "SOMECO", "COLGRAD", "REGION", "WSAL_VAL")
  absent <- setdiff(required, names(raw))
  if (length(absent)) stop("Unsupported raw schema; missing: ", paste(absent, collapse = ", "))
  if (!is.data.frame(raw) || !nrow(raw)) stop("Raw input must be a nonempty data frame")
  if (anyDuplicated(names(raw))) stop("Raw column names must be unique")
  for (name in required) {
    if (!is.numeric(raw[[name]])) stop("Expected numeric raw field: ", name)
    if (any(!is.finite(raw[[name]]) & !is.na(raw[[name]]))) stop("Nonfinite raw field: ", name)
  }
  year <- as.numeric(raw$YEAR)
  year[year %in% 88:97] <- year[year %in% 88:97] + 1900
  if (any(!is.na(year) & !(year %in% 1988:1997))) {
    stop("Unsupported YEAR codes; expected 88:97 or 1988:1997")
  }
  # Report sequential exclusions, so counts sum to input minus retained rows.
  eligible <- rep(TRUE, nrow(raw))
  exclusions <- data.frame(reason = character(), n = integer())
  remove <- function(condition, reason) {
    condition[is.na(condition)] <- TRUE
    hit <- eligible & condition
    exclusions <<- rbind(exclusions, data.frame(reason = reason, n = sum(hit)))
    eligible[hit] <<- FALSE
  }
  remove(is.na(year), "missing_survey_year")
  remove(is.na(raw$AGE) | raw$AGE < 21 | raw$AGE > 58, "outside_age_21_58_or_missing")
  # Check coding in the target age/year population before removing missingness.
  check_codes <- function(x, values, name) {
    if (any(eligible & !is.na(x) & !(x %in% values))) stop("Unsupported ", name, " codes")
  }
  check_codes(raw$SEX, 1:2, "SEX")
  check_codes(raw$DISABL1, 0:1, "DISABL1")
  check_codes(raw$REGION, 1:9, "REGION")
  for (name in c("LESSHS", "HSGRAD", "SOMECO", "COLGRAD")) check_codes(raw[[name]], 0:1, name)
  if (any(eligible & !is.na(raw$RACE) & (raw$RACE < 1 | raw$RACE != floor(raw$RACE)))) {
    stop("Unsupported RACE codes; positive integer source categories required")
  }
  if (any(eligible & !is.na(raw$WKSWORK) & (raw$WKSWORK < 0 | raw$WKSWORK > 52))) {
    stop("WKSWORK outside 0:52; audit source missing-value codes")
  }
  remove(is.na(raw$SEX), "missing_sex")
  remove(is.na(raw$DISABL1), "missing_disability")
  # Published Table 1 reports unweighted population counts before exclusion
  # of zero/missing estimation weights. Keep those counts separately; never
  # pass these ineligible weights to the estimating equations.
  population_counts <- as.data.frame(table(
    survey_year = year[eligible],
    sex = ifelse(raw$SEX[eligible] == 1, "male", "female"),
    age_sample = ifelse(raw$AGE[eligible] < 40, "young", "old"),
    disabled = raw$DISABL1[eligible]))
  names(population_counts)[5L] <- "n"
  population_counts$survey_year <- as.integer(as.character(population_counts$survey_year))
  population_counts$disabled <- as.integer(as.character(population_counts$disabled))
  remove(is.na(raw$FNLWGT2) | raw$FNLWGT2 <= 0, "missing_or_nonpositive_weight")
  if (!any(eligible)) stop("No observations remain after population and weight checks")
  x <- raw[eligible, , drop = FALSE]
  year <- year[eligible]
  # Education flags must be mutually exclusive; no precedence rule hides conflicts.
  flags <- as.matrix(x[c("LESSHS", "HSGRAD", "SOMECO", "COLGRAD")])
  observed_flags <- rowSums(!is.na(flags)) == 4L
  sums <- rowSums(flags)
  if (any(observed_flags & sums > 1L)) stop("Conflicting education indicators")
  education <- rep(NA_character_, nrow(x))
  education[observed_flags & sums == 1L & x$LESSHS == 1] <- "less_hs"
  education[observed_flags & sums == 1L & x$HSGRAD == 1] <- "hs"
  education[observed_flags & sums == 1L & (x$SOMECO == 1 | x$COLGRAD == 1)] <- "some_college_plus"
  weekly <- rep(NA_real_, nrow(x))
  wage_observed <- !is.na(x$WKSWORK) & x$WKSWORK > 0 & !is.na(x$WSAL_VAL) & x$WSAL_VAL > 0
  weekly[wage_observed] <- x$WSAL_VAL[wage_observed] / x$WKSWORK[wage_observed]
  # Legacy survey-year CPI mapping is retained only under explicit legacy names.
  # Outcomes refer to the previous calendar year; original-program audit is needed
  # before this deflator and wage-trimming rule can be called a faithful replication.
  legacy_cpi <- c(117, 122.6, 129, 134.3, 138.2, 142.1, 145.6, 149.8, 154.1, 157.6)[match(year, 1988:1997)] / 117
  real_weekly <- weekly / legacy_cpi
  out <- data.frame(
    source_row = which(eligible), survey_year = year, outcome_year = year - 1L,
    age = as.numeric(x$AGE), age_group = factor(10 * floor(x$AGE / 10), levels = c(20, 30, 40, 50)),
    age_sample = factor(ifelse(x$AGE < 40, "young", "old"), levels = c("young", "old")),
    sex = factor(x$SEX, levels = 1:2, labels = c("male", "female")),
    disabled = as.integer(x$DISABL1), weight = as.numeric(x$FNLWGT2) / 100,
    weeks = as.numeric(x$WKSWORK), worked_any = ifelse(is.na(x$WKSWORK), NA_integer_, as.integer(x$WKSWORK > 0)),
    race = factor(pmin(x$RACE, 3), levels = 1:3),
    education = factor(education, levels = c("less_hs", "hs", "some_college_plus")),
    region = factor(x$REGION, levels = 1:9),
    annual_wage = as.numeric(x$WSAL_VAL), nominal_weekly_wage = weekly,
    log_nominal_weekly_wage = log(weekly), legacy_cpi_index = legacy_cpi,
    legacy_real_weekly_wage = real_weekly,
    legacy_wage_eligible = !is.na(real_weekly) & real_weekly >= 25 & real_weekly <= 2000
  )
  # Optional source measures are retained for transparent descriptive checks.
  # SSI/SS receipt is not relabeled as DI, and no unpublished $75 cutoff is used.
  if ("WORKING" %in% names(x)) {
    if (any(!is.na(x$WORKING) & !x$WORKING %in% 0:1)) stop("Unsupported WORKING codes")
    out$current_working <- as.integer(x$WORKING)
  }
  for (name in c("SS_YN", "SSI_YN")) {
    if (name %in% names(x) && any(!is.na(x[[name]]) & !x[[name]] %in% 1:2)) stop("Unsupported ", name, " codes")
  }
  if ("SS_YN" %in% names(x)) out$social_security_receipt <- as.integer(x$SS_YN == 1)
  if ("SSI_YN" %in% names(x)) out$ssi_receipt <- as.integer(x$SSI_YN == 1)
  if ("SS_VAL" %in% names(x)) out$annual_social_security <- as.numeric(x$SS_VAL)
  if ("HG_ST60" %in% names(x)) out$state_1960 <- as.numeric(x$HG_ST60)
  out$complete_covariates <- complete.cases(out[c("age", "race", "education", "region")])
  quality <- data.frame(
    issue = c("missing_weeks", "missing_covariates", "not_legacy_wage_eligible"),
    n = c(sum(is.na(out$weeks)), sum(!out$complete_covariates), sum(!out$legacy_wage_eligible))
  )
  list(data = out, population_counts = population_counts, exclusions = exclusions, quality = quality,
       metadata = list(input_rows = nrow(raw), retained_rows = nrow(out),
                       mapping_status = "unverified_input_requires_hash_bound_source_audit",
                       original_programs_available = FALSE,
                       timing = "outcome_year = survey_year - 1",
                       weight = "FNLWGT2 / 100; relative weights unchanged",
                       wage_note = "Legacy survey-year CPI and trimming retained for comparison only; verify against deposited programs"))
}

read_cps <- function(path) {
  if (!file.exists(path)) stop("Missing raw SAS file: ", path, ". Run the Dataverse downloader and inspect its original archives.")
  if (!requireNamespace("haven", quietly = TRUE)) stop("Install haven before reading the SAS source")
  # Read only fields used by this project: the supplied file has 145 columns.
  fields <- c("YEAR", "AGE", "SEX", "DISABL1", "FNLWGT2", "WKSWORK", "RACE",
              "LESSHS", "HSGRAD", "SOMECO", "COLGRAD", "REGION", "WSAL_VAL",
              "WORKING", "SS_YN", "SSI_YN", "SS_VAL", "HG_ST60")
  result <- prepare_cps(haven::read_sas(path, col_select = tidyselect::any_of(fields)))
  result$metadata$source_file <- normalizePath(path)
  result
}
