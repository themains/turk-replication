options(stringsAsFactors = FALSE)
for (file in sort(list.files("R", pattern = "^0[1-6]_.*[.]R$", full.names = TRUE))) source(file)
dir.create("tabs", showWarnings = FALSE)
dir.create("data/derived", recursive = TRUE, showWarnings = FALSE)
studies <- read_studies()
prepared <- prepare_survey_data(studies)
results <- c(
  analyze_ip(studies), survey_results(studies),
  model_results(prepared)
)
results$platform_summary <- platform_summary(results$ip_summary)
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
