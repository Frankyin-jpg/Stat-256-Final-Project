#!/usr/bin/env Rscript
# Reuse an already prepared canonical RDS; never reread the large source here.
source("R/replication.R")
args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 2L) stop("Usage: Rscript scripts/run_replication.R [prepared.rds] [results_dir]")
input <- if (length(args)) args[[1]] else "data/derived/cps.rds"
output <- if (length(args) > 1L) args[[2]] else "results"
prepared <- readRDS(input)
if (!is.list(prepared) || !is.data.frame(prepared$data)) stop("Expected a prepared CPS object with $data")
result <- replicate_published(prepared$data, output, population_counts = prepared$population_counts)
metadata <- list(status = result$status,
  source_description = "Selected Tables 1/2 in the supplied July 2, 2001 Acemoglu-Angrist proof",
  original_programs_available = FALSE,
  reconstruction = "From paper descriptions and independently transcribed printed targets",
  prepared_file_md5 = unname(tools::md5sum(input)),
  prepared_metadata = prepared$metadata,
  matched = sum(result$benchmarks$pass), compared = nrow(result$benchmarks),
  created_utc = format(Sys.time(), tz = "UTC", usetz = TRUE))
saveRDS(metadata, file.path(output, "replication_metadata.rds"))
cat(result$status, ": ", sum(result$benchmarks$pass), "/", nrow(result$benchmarks),
    " selected proof benchmarks match printed precision.\n", sep = "")
if (any(!result$benchmarks$pass)) {
  print(result$benchmarks[!result$benchmarks$pass,
    c("table", "sex", "age_sample", "outcome", "statistic", "year", "disabled", "target", "reproduced", "tolerance")], row.names = FALSE)
}
