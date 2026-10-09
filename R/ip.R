ip_geography <- function(data, column, threshold) {
  counts <- table(data[[column]], useNA = "no")
  location <- names(counts)
  location[as.integer(counts) <= threshold] <- "Other"
  stats::aggregate(
    list(n = as.integer(counts)), list(location = location), sum
  )
}

ip_reconstruct_flags <- function(data) {
  missing <- is.na(data$ip_index)
  duplicated_ip <- duplicated(data$ip_index) |
    duplicated(data$ip_index, fromLast = TRUE)
  duplicated_ip[missing] <- NA
  country <- data$maxmindcountrynamesen
  country[country == "Puerto Rico" & !is.na(country)] <- "United States"
  foreign <- ifelse(is.na(country), NA, country != "United States")
  if ("apivoidblacklistsdetections" %in% names(data)) {
    detected <- data$apivoidblacklistsdetections
    blacklist <- ifelse(is.na(detected), FALSE, detected > 0)
  } else {
    status <- data$ipvoidblacklist_status
    blacklist <- ifelse(is.na(status), NA, grepl("BLACKLISTED", status))
  }
  data.frame(
    missing_ip = missing, blacklisted = blacklist, duplicated = duplicated_ip,
    foreign_ip = foreign,
    funny_ip = missing | blacklist | duplicated_ip | foreign
  )
}

ip_unit_sensitivity <- function(data) {
  survey_columns <- setdiff(names(data), "worktimeinseconds")
  repeated_survey <- duplicated(data[survey_columns]) & !is.na(data$ip_index)
  scenarios <- list(
    deposited_rows = rep(TRUE, nrow(data)), unique_observed_survey = !repeated_survey
  )
  rare_columns <- c("prosthetic", "blind", "deaf", "gang_resp", "gang_fam", "sleep")
  do.call(rbind, lapply(names(scenarios), function(scenario) {
    subset <- data[scenarios[[scenario]], , drop = FALSE]
    flags <- ip_reconstruct_flags(subset)
    rare_positive <- rowSums(subset[rare_columns], na.rm = TRUE)
    rare_unknown <- rowSums(is.na(subset[rare_columns]))
    rare_lower <- rare_positive >= 2
    rare_upper <- (rare_positive + rare_unknown) >= 2
    totals <- c(
      ip_flagged = sum(flags$funny_ip, na.rm = TRUE),
      rare_lower = sum(rare_lower), rare_upper = sum(rare_upper),
      combined_lower = sum(flags$funny_ip | rare_lower, na.rm = TRUE),
      combined_upper = sum(flags$funny_ip | rare_upper, na.rm = TRUE)
    )
    result <- data.frame(
      scenario,
      n = nrow(subset),
      duplicate_observed_survey_rows = sum(repeated_survey),
      observed_survey_rows = sum(!is.na(subset$ip_index))
    )
    result[names(totals)] <- as.list(totals)
    result[paste0("pct_", names(totals))] <- as.list(100 * totals / nrow(subset))
    result
  }))
}

analyze_ip <- function(studies) {
  result <- lapply(names(studies), function(study_name) {
    data <- studies[[study_name]]
    tag <- data.frame(study = study_name, pollname = unique(data$pollname))
    stopifnot(nrow(tag) == 1L)
    flag_names <- c("missing_ip", "blacklisted", "duplicated", "foreign_ip", "funny_ip")
    totals <- vapply(data[flag_names], sum, numeric(1), na.rm = TRUE)
    summary <- data.frame(
      n = nrow(data), n_ip_observed = sum(!is.na(data$ip_index)),
      n_unique_ip = length(unique(stats::na.omit(data$ip_index)))
    )
    summary[flag_names] <- as.list(totals)
    summary$foreign_or_duplicated <- sum(data$foreign_ip | data$duplicated, na.rm = TRUE)
    summary[paste0("pct_", flag_names)] <- as.list(100 * totals / nrow(data))
    summary <- cbind(tag, summary)

    frequency <- table(table(data$ip_index, useNA = "no"))
    multiplicity <- data.frame(
      multiplicity = as.integer(names(frequency)), n_addresses = as.integer(frequency)
    )
    multiplicity$n_responses <- multiplicity$multiplicity * multiplicity$n_addresses

    unknown <- if ("apivoidblacklistsdetections" %in% names(data)) {
      is.na(data$apivoidblacklistsdetections)
    } else {
      is.na(data$ipvoidblacklist_status)
    }
    unknown_unflagged <- sum(unknown & !data$funny_ip, na.rm = TRUE)
    sensitivity <- data.frame(
      n = nrow(data), unknown_blacklist = sum(unknown),
      unknown_unflagged = unknown_unflagged, flagged = totals[["funny_ip"]],
      pct_lower = 100 * totals[["funny_ip"]] / nrow(data),
      pct_upper = 100 * (totals[["funny_ip"]] + unknown_unflagged) / nrow(data)
    )
    reconstructed <- ip_reconstruct_flags(data)
    checks <- data.frame(
      flag = flag_names,
      disagreements = vapply(flag_names, function(flag) {
        lhs <- as.logical(data[[flag]])
        rhs <- reconstructed[[flag]]
        sum(xor(is.na(lhs), is.na(rhs)) | (!is.na(lhs) & !is.na(rhs) & lhs != rhs))
      }, numeric(1)), row.names = NULL
    )
    list(
      ip_summary = summary,
      ip_multiplicity = cbind(tag, multiplicity),
      country_counts = cbind(tag, ip_geography(data, "maxmindcountrynamesen", 5)),
      city_counts = cbind(tag, ip_geography(data, "maxmindcitynamesen", 25)),
      ip_missing_sensitivity = cbind(tag, sensitivity),
      ip_unit_sensitivity = cbind(tag, ip_unit_sensitivity(data)),
      ip_flag_checks = cbind(tag, checks)
    )
  })
  stats::setNames(lapply(names(result[[1]]), function(name) {
    table <- do.call(rbind, lapply(result, `[[`, name))
    rownames(table) <- NULL
    table
  }), names(result[[1]]))
}
