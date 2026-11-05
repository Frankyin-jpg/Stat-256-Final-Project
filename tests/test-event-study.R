source("R/event_study.R")
set.seed(256)
d <- expand.grid(outcome_year = 1987:1996, disabled = 0:1, repeat_id = 1:60)
d$age <- sample(21:58, nrow(d), replace = TRUE)
d$race <- sample(1:3, nrow(d), replace = TRUE)
d$weight <- runif(nrow(d), .5, 2)
d$weeks <- 20 + .1 * d$age + d$race + d$disabled *
  (2 + 3 * (d$outcome_year >= 1993)) + (d$outcome_year - 1987)
d$worked_any <- as.numeric(d$weeks > 0)
d$sex <- "male"
d$age_sample <- "young"
fit <- fit_event_study(d)
expected <- ifelse(fit$coefficients$year >= 1993, 3, 0)
stopifnot(max(abs(fit$coefficients$estimate - expected)) < 1e-7)
d2 <- d; d2$weight <- 100 * d$weight
stopifnot(max(abs(fit_event_study(d2)$coefficients$estimate -
                  fit$coefficients$estimate)) < 1e-7)
p <- pool_periods(d)
stopifnot(all(abs(tapply(p$analysis_weight, p$post, sum) - 1) < 1e-12))
stopifnot(inherits(try(pool_periods(d[d$outcome_year != 1987, ]), silent = TRUE), "try-error"))
stopifnot(inherits(try(pool_periods(d, pre = c(1987, 1987, 1988)), silent = TRUE), "try-error"))
stopifnot(inherits(try(pool_periods(d, pre = integer()), silent = TRUE), "try-error"))
stopifnot(inherits(try(fit_event_study(d[d$disabled == 0, ]), silent = TRUE), "try-error"))
stopifnot(nrow(describe_cps(d)) == 20L)
cat("PASS: known event-study effects, weight invariance, timing, and empty-group checks\n")
