options(stringsAsFactors = FALSE)
for (file in sort(list.files("R", pattern = "[.]R$", full.names = TRUE))) source(file)
dir.create("tabs", showWarnings = FALSE)
dir.create("data/derived", recursive = TRUE, showWarnings = FALSE)
studies <- read_studies()
original_attenuation <- setNames(lapply(1:2, function(i) {
  path <- sprintf("original/data/study%d_attenuation.csv", i)
  text <- paste(readLines(path, warn = FALSE), collapse = "\n")
  read.csv(text = text)
}), c("study1", "study2"))
prepared <- prepare_survey_data(studies)
results <- c(
  analyze_ip(studies), survey_results(studies),
  model_results(prepared, original_attenuation)
)
baseline <- survey_results(studies, corrected = FALSE)
comparison <- merge(baseline$survey_prevalence, results$survey_prevalence,
  by = c("study", "measure"), suffixes = c("_original", "_updated")
)
comparison$change_pp <- 100 * (comparison$proportion_updated - comparison$proportion_original)
results$survey_comparison <- comparison
cores <- as.integer(Sys.getenv("TURK_CORES", "1"))
results$attenuation_bootstrap <- bootstrap_attenuation(prepared, cores = cores)
results$attenuation_cluster_bootstrap <- bootstrap_attenuation(
  prepared,
  cluster = TRUE, cores = cores
)
results <- c(results, analysis_diagnostics(prepared))
for (name in names(results)) {
  stopifnot(is.data.frame(results[[name]]))
  write.csv(results[[name]], file.path("tabs", paste0(name, ".csv")), row.names = FALSE, na = "")
}
saveRDS(results, "data/derived/results.rds", version = 3)
writeLines(capture.output(sessionInfo()), "tabs/session-info.txt")
