args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) {
  stop("Usage: Rscript scripts/audit_restricted.R RESTRICTED_DATA_DIRECTORY OUTPUT_DIRECTORY")
}
input <- normalizePath(args[1], mustWork = TRUE)
output <- args[2]
dir.create(output, recursive = TRUE, showWarnings = FALSE)

sources <- c(
  "turk_ssi_data/turk_ssi_recoded.csv", "busby_lucid/lucid_ip_recoded.csv",
  "graham_lucid/graham_partisanship_6_2019_ip+kyip-20200724.csv",
  "graham_lucid/graham_partyprime_dem_8_2019_ip+kyip-20200724.csv"
)
records <- lapply(file.path(input, sources), utils::read.csv, stringsAsFactors = FALSE)
names(records) <- sources

audit_group <- function(data, source, group) {
  duplicate <- duplicated(data$ip) | duplicated(data$ip, fromLast = TRUE)
  duplicate[is.na(data$ip)] <- NA
  if (!"blacklisted" %in% names(data)) {
    data$blacklisted <- ifelse(is.na(data$apivoid.blacklists.detections),
      FALSE, data$apivoid.blacklists.detections > 0
    )
    data$missing_ip <- is.na(data$ip)
    data$foreign_ip <- ifelse(is.na(data$maxmind.country.names.en), NA,
      !data$maxmind.country.names.en %in% c("United States", "Puerto Rico")
    )
  }
  corrected <- data$missing_ip | data$blacklisted | data$foreign_ip | duplicate
  archived <- if ("funny_ip" %in% names(data)) sum(data$funny_ip, na.rm = TRUE) else NA_real_
  data.frame(
    source = source, group = group, n = nrow(data),
    archived_duplicated = if ("duplicated" %in% names(data)) {
      sum(data$duplicated, na.rm = TRUE)
    } else {
      NA_real_
    },
    within_group_duplicated = sum(duplicate, na.rm = TRUE),
    archived_flagged = archived, corrected_flagged = sum(corrected, na.rm = TRUE),
    archived_pct = 100 * archived / nrow(data),
    corrected_pct = 100 * sum(corrected, na.rm = TRUE) / nrow(data),
    archived_unknown = if ("funny_ip" %in% names(data)) sum(is.na(data$funny_ip)) else NA_real_,
    corrected_unknown = sum(is.na(corrected))
  )
}

results <- list()
for (i in 1:2) {
  groups <- split(records[[i]], records[[i]]$pollname)
  for (group in names(groups)) {
    results[[length(results) + 1L]] <- audit_group(groups[[group]], sources[i], group)
  }
}
for (i in 3:4) {
  results[[length(results) + 1L]] <- audit_group(
    records[[i]], sources[i], c("Graham June 2019", "Graham August 2019")[i - 2L]
  )
}
utils::write.csv(
  do.call(rbind, results), file.path(output, "restricted-ip-audit.csv"),
  row.names = FALSE
)
manifest <- data.frame(
  source = sources,
  sha256 = vapply(file.path(input, sources), function(path) {
    digest::digest(file = path, algo = "sha256")
  }, character(1), USE.NAMES = FALSE)
)
utils::write.csv(manifest, file.path(output, "restricted-source-manifest.csv"), row.names = FALSE)
