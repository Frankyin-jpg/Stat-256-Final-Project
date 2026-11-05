# Render research tables from one completed run. Raw CSVs remain authoritative.
export_tables <- function(results_dir, output_dir = file.path(results_dir, "tables")) {
  if (!file.exists(file.path(results_dir, "run-metadata.rds"))) stop("Tables require analysis-run metadata.")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  read_table <- function(name) {
    path <- file.path(results_dir, paste0(name, ".csv"))
    if (!file.exists(path)) stop("Missing analysis output: ", path)
    read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  }
  save_table <- function(x, name, caption) {
    # Label unavailable analyses explicitly, with no invented numeric values.
    if (!nrow(x)) x <- data.frame(Status = "No successful estimates available in this run")
    for (format in c("pipe", "latex")) {
      suffix <- if (format == "pipe") ".md" else ".tex"
      output <- knitr::kable(x, format = format, digits = 3, row.names = FALSE,
                             caption = caption, booktabs = TRUE, escape = TRUE)
      writeLines(as.character(output), file.path(output_dir, paste0(name, suffix)))
    }
  }
  descriptive <- read_table("descriptives")
  save_table(descriptive, "01_annual_descriptives", "Weighted annual employment summaries; survey outcome reference years.")
  extension <- read_table("extension")
  main <- extension[extension$window == "main", c("sex", "age_sample", "outcome", "method", "estimate")]
  save_table(main, "02_main_estimators", "Main-window DID contrasts. Point estimates; causal interpretation requires stated assumptions.")
  boot <- read_table("bootstrap")
  if (nrow(boot)) boot <- boot[, c("sex", "age_sample", "outcome", "method", "estimate", "conf_low", "conf_high", "n_success", "n_requested", "precision_label")]
  save_table(boot, "03_bootstrap", "Cell-stratified individual-bootstrap uncertainty; not survey-design or national-policy uncertainty.")
  for (name in c("placebo", "distributional", "extension_rich")) {
    tab <- read_table(name)
    if (nrow(tab)) {
      tab <- tab[tab$method == "aipw_standardized_did", , drop = FALSE]
      cols <- intersect(c("sex", "age_sample", "outcome", "window", "threshold", "covariate_set", "estimate", "n_window"), names(tab))
      tab <- tab[, cols, drop = FALSE]
    }
    caption <- switch(name, placebo = "Pre-policy adjusted disparity changes: a diagnostic, not a validation of parallel trends.",
      distributional = "Adjusted CDF changes at prespecified weeks thresholds; these are not quantile treatment effects.",
      extension_rich = "Richer adjustment versus primary adjustment on exactly the same complete-case sample.")
    save_table(tab, paste0("04_", name), caption)
  }
  failures <- read_table("extension_failures")
  if (!nrow(failures)) failures <- data.frame(Status = "No scenario failures in this run")
  save_table(failures, "05_failures", "Complete analysis failure inventory; failed estimates are never silently replaced.")
  invisible(output_dir)
}
