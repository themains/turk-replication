paper_label <- function(x) {
  labels <- c(
    study1 = "August 2018", study2 = "June 2020", study3 = "July 2020",
    accepted = "Accepted date", other = "Other date entry",
    all = "All", unflagged = "Unflagged", flagged = "Flagged", flagged_ip = "IP flagged",
    rare_responses = "Rare-trait flagged", high_hits = "Over 1,000 HITs",
    unemploy = "Unemployment", inflation = "Inflation", funny = "IP flagged",
    likely_troll = "Rare-trait flagged", insincere = "Insincerity (historical)",
    insincere_si = "Insincerity: some or more", combined_troll_1 = "Index 1",
    combined_troll_2 = "Index 2", combined_troll_3 = "Index 3", respondents = "Sample share",
    time = "Duration (seconds)", fast = "Fast timing screen", slow = "Slow timing screen",
    date_ok = "Month-first date", date_poss_foreign = "Day-first date",
    inattentive = "Non-date",
    day_first = "Day-first date", flagged_date = "Index 2 flagged",
    flagged_any_date = "Index 3 flagged",
    rare_prosthetic = "Prosthetic", rare_blind = "Vision impairment",
    rare_deaf = "Hearing impairment",
    rare_gang_resp = "Gang membership", rare_gang_fam = "Family gang membership",
    rare_sleep = "Unusual sleep",
    james_black = "Black", james_gay = "Same-sex spouse", james_evang = "Evangelical",
    james_aa = "Secular", james_lib = "Liberal activity", james_con = "Conservative activity",
    badactor = "Flagged", out_party_treat = "Out-party cue",
    all_flagged = "All-flag interaction",
    ip_only = "IP-only interaction", rare_only = "Rare-only interaction",
    fast_unflagged = "Fast, unflagged",
    dem_cf = "Democratic conjunction", rep_cf = "Republican conjunction",
    foreign_dummy = "Date convention"
  )
  vapply(as.character(x), function(item) {
    if (item %in% names(labels)) {
      return(unname(labels[item]))
    }
    bits <- strsplit(item, ":", fixed = TRUE)[[1]]
    if (length(bits) > 1) {
      return(paste(paper_label(bits), collapse = " x "))
    }
    gsub("_", " ", item, fixed = TRUE)
  }, character(1), USE.NAMES = FALSE)
}

tex_escape <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- "--"
  for (mark in c("&", "%", "_", "#", "$")) x <- gsub(mark, paste0("\\", mark), x, fixed = TRUE)
  x
}

paper_number <- function(x, digits = 1) {
  ifelse(is.finite(x), formatC(x, format = "f", digits = digits), "--")
}

paper_tabular <- function(data, headers = names(data)) {
  cells <- as.data.frame(lapply(data, tex_escape), stringsAsFactors = FALSE)
  rows <- apply(cells, 1, paste, collapse = " & ")
  c(
    paste0("\\begin{tabular}{l", paste(rep("r", ncol(data) - 1L), collapse = ""), "}"),
    "\\toprule", paste0(paste(tex_escape(headers), collapse = " & "), " \\\\"), "\\midrule",
    paste0(rows, " \\\\"), "\\bottomrule", "\\end{tabular}"
  )
}

paper_longtable <- function(data, caption, headers = names(data), widths = NULL) {
  if (!nrow(data)) stop("Empty manuscript table: ", caption)
  if (!is.null(widths)) widths <- widths / sum(widths) * .78
  columns <- if (is.null(widths)) {
    paste0("l", paste(rep("r", ncol(data) - 1L), collapse = ""))
  } else {
    paste0(">{\\raggedright\\arraybackslash}p{", widths, "\\linewidth}", collapse = "")
  }
  rows <- apply(as.data.frame(lapply(data, tex_escape)), 1, paste, collapse = " & ")
  header <- paste0(paste(tex_escape(headers), collapse = " & "), " \\\\")
  c(
    "{\\small", paste0("\\begin{longtable}{", columns, "}"),
    paste0("\\caption{", caption, "}\\\\"), "\\toprule", header, "\\midrule\\endfirsthead",
    "\\toprule", header, "\\midrule\\endhead", "\\bottomrule\\endfoot",
    paste0(rows, " \\\\"), "\\end{longtable}}"
  )
}

write_paper_tables <- function(results, path = "tabs") {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  put <- function(name, lines) writeLines(lines, file.path(path, paste0(name, ".tex")))
  need <- c(
    "ip_summary", "survey_prevalence", "hits_summary", "experiment_effects",
    "ordered_coefficients", "ordered_margins"
  )
  stopifnot(all(need %in% names(results)))
  d <- results$ip_summary
  table <- data.frame(
    Study = paper_label(d$study), N = d$n,
    Missing = paper_number(d$pct_missing_ip), Blacklisted = paper_number(d$pct_blacklisted),
    Repeated = paper_number(d$pct_duplicated), Foreign = paper_number(d$pct_foreign_ip),
    Any = paper_number(d$pct_funny_ip)
  )
  put("ip_summary", paper_tabular(table))
  prevalence <- results$survey_prevalence
  measures <- c(
    "likely_troll", "insincere", "insincere_si", "combined_troll_1",
    "combined_troll_2", "combined_troll_3",
    "date_poss_foreign", "inattentive"
  )
  table <- data.frame(Measure = paper_label(measures))
  for (study in c("study1", "study2", "study3")) {
    d <- prevalence[prevalence$study == study, ]
    table[[paper_label(study)]] <- paper_number(100 * d$proportion[match(measures, d$measure)])
  }
  put("screen_summary", paper_tabular(table))
  macros <- character()
  for (study in c("study1", "study2", "study3")) {
    word <- c(study1 = "One", study2 = "Two", study3 = "Three")[[study]]
    d <- prevalence[prevalence$study == study, ]
    for (measure in c("likely_troll", "combined_troll_1", "insincere")) {
      key <- c(
        likely_troll = "Rare", combined_troll_1 = "Combined",
        insincere = "Insincere"
      )[[measure]]
      value <- d$proportion[match(measure, d$measure)]
      macros <- c(macros, paste0(
        "\\newcommand{\\", key, word, "}{",
        paper_number(100 * value), "}"
      ))
    }
  }
  d <- results$experiment_effects
  for (group in c("all", "unflagged", "flagged")) {
    for (outcome in c("unemploy", "inflation")) {
      keep <- d$group == group & d$outcome == outcome &
        d$term == "out_party_treat" & d$inference == "conventional"
      row <- d[keep, ]
      key <- paste0(
        c(all = "All", unflagged = "Clean", flagged = "Flagged")[[group]],
        c(unemploy = "Jobs", inflation = "Prices")[[outcome]]
      )
      macros <- c(macros, paste0("\\newcommand{\\", key, "}{", paper_number(
        row$estimate,
        3
      ), "}"))
    }
  }
  for (i in 1:3) {
    word <- c("One", "Two", "Three")[i]
    ip <- results$ip_summary$pct_funny_ip[results$ip_summary$study == paste0("study", i)]
    macros <- c(macros, paste0("\\newcommand{\\Ip", word, "}{", paper_number(ip), "}"))
  }
  d <- results$attenuation_cluster_bootstrap
  for (outcome in c("unemploy", "inflation")) {
    row <- d[d$study == "study2" & d$outcome == outcome, ]
    word <- c(unemploy = "Jobs", inflation = "Prices")[[outcome]]
    for (suffix in c("attenuation", "attenuation_conf_low", "attenuation_conf_high")) {
      key <- c(
        attenuation = "Reduction", attenuation_conf_low = "ReductionLow",
        attenuation_conf_high = "ReductionHigh"
      )[[suffix]]
      macros <- c(macros, paste0(
        "\\newcommand{\\", key, word, "}{",
        paper_number(100 * row[[suffix]], 1), "}"
      ))
    }
  }
  put("macros", macros)
  lines <- c(
    "\\begin{table}[htbp]\\centering",
    "\\caption{Screening and duration by reported task experience}\\label{tab:hits}"
  )
  for (study in c("study2", "study3")) {
    d <- results$hits_summary[results$hits_summary$study == study, ]
    measures <- unique(d$measure)
    table <- data.frame(Measure = paper_label(measures))
    for (h in 1:4) {
      rows <- d[d$hits == h, ]
      values <- rows$estimate[match(measures, rows$measure)]
      table[[c("Under 100", "100--500", "500--1,000", "Over 1,000")[h]]] <-
        paper_number(values * ifelse(measures == "time", 1, 100))
    }
    lines <- c(
      lines, paste0("\\textbf{", paper_label(study), "}\\par\\smallskip"),
      paper_tabular(table), "\\par\\medskip"
    )
  }
  put("hits_summary", c(lines, paste0(
    "\\tabnote{Entries are percentages except mean duration in seconds. ",
    "Task counts are self-reported categories. Item denominators are ",
    "recorded in the accompanying CSV.}"
  ), "\\end{table}"))
  platform <- read.csv("docs/restricted-ip-audit.csv", stringsAsFactors = FALSE)
  platform <- platform[!is.na(platform$archived_pct), ]
  table <- data.frame(
    Survey = platform$group, N = platform$n,
    Archived = paper_number(platform$archived_pct, 2),
    Audited = paper_number(platform$corrected_pct, 2),
    Source = "Restricted aggregate"
  )
  d <- results$ip_summary
  table <- rbind(table, data.frame(
    Survey = paper_label(d$study), N = d$n,
    Archived = paper_number(d$pct_funny_ip, 2), Audited = paper_number(d$pct_funny_ip, 2),
    Source = "Public recomputation"
  ))
  put("platform_summary", c(
    paper_longtable(table,
      "IP-flagged responses across studies: archived and audited percentages\\label{tab:platform}",
      widths = c(.27, .09, .12, .12, .24)
    ),
    paste0(
      "\\noindent\\small AB and AG denote Ahler--Broockman and Ahler--Goggin.",
      " IGS is SSI/Dynata; Busby, Graham and Thompson--Busby use Lucid. ",
      "Restricted-source audits were computed locally from the source files ",
      "identified by the supplied hash manifest. The public build reads these",
      " aggregates; it cannot independently reproduce them without those ",
      "files. Graham DK pools two experiments, so within-experiment ",
      "duplication is additionally reported as a sensitivity ",
      "analysis.\\normalsize"
    )
  ))
  if ("historical_costs" %in% names(results)) {
    d <- results$historical_costs
    table <- data.frame(
      Study = paper_label(d$study), Dollars = paper_number(d$payment, 2),
      Multiplier = paper_number(d$fee_multiplier, 1), Minutes = d$minutes,
      Flagged = paper_number(100 * d$flagged_share, 1),
      Cents = paper_number(d$cents_per_retained_minute, 2)
    )
    cost_table <- c(
      "\\begin{table}[htbp]\\centering",
      "\\caption{Historical cost per IP-unflagged response-minute}",
      "\\resizebox{\\linewidth}{!}{", paper_tabular(table), "}", "\\par\\smallskip"
    )
    put("historical_costs", c(cost_table, paste0(
      "\\noindent\\small Dollars is payment per response. Cents is the cost per ",
      "retained response-minute, including the historical fee multiplier. Lucid rows preserve ",
      "illustrative published price, duration, and rounded IP-rate assumptions; they are not ",
      "updated vendor quotations.\\normalsize\\end{table}"
    )))
  }
  details <- character()
  for (name in c("ip_multiplicity", "country_counts", "city_counts")) {
    d <- results[[name]]
    if (is.null(d)) next
    keep <- setdiff(names(d), "pollname")
    d <- d[keep]
    d$study <- paper_label(d$study)
    caption <- switch(name,
      ip_multiplicity = "Observed IP multiplicity: addresses and response records",
      country_counts = "Response records by recorded country",
      city_counts = "Response records by recorded city"
    )
    details <- c(details, paper_longtable(d, caption, paper_label(names(d))))
  }
  if ("rare_missing_sensitivity" %in% names(results)) {
    d <- results$rare_missing_sensitivity
    table <- data.frame(
      Study = paper_label(d$study), N = d$n, Missing = d$any_missing,
      RareMin = d$rare_lower, RareMax = d$rare_upper,
      IndexMin = d$combined_lower, IndexMax = d$combined_upper
    )
    sensitivity_table <- paper_longtable(table,
      "Sensitivity of screen counts to missing rare-trait items",
      widths = c(.19, .09, .13, .13, .13, .13, .13)
    )
    details <- c(details, sensitivity_table, paste0(
      "\\noindent\\small Minimum sets missing rare-trait items to negative; maximum ",
      "sets them to affirmative. These bound the screen under missing answers, not the number ",
      "of dishonest participants.\\normalsize"
    ))
  }
  if ("ip_unit_sensitivity" %in% names(results)) {
    d <- results$ip_unit_sensitivity
    d <- d[d$study == "study1", ]
    table <- data.frame(
      Unit = ifelse(d$scenario == "deposited_rows", "Paid assignments", "Unique observed survey"),
      N = d$n, IP = d$ip_flagged, Rare = d$rare_lower, Combined = d$combined_lower
    )
    details <- c(
      details,
      "\\par\\noindent\\begin{minipage}{\\linewidth}\\centering",
      "\\captionsetup{hypcap=false}",
      paste0(
        "\\captionof{table}{Study 1 row-unit sensitivity: ",
        "removing the twice-linked observed survey response}"
      ),
      paper_tabular(table), "\\end{minipage}\\par\\medskip"
    )
  }
  put("ip_details", details)
  d <- results$survey_crosstabs
  table <- data.frame(
    Study = paper_label(d$study), Measure = paper_label(d$measure),
    Screen = ifelse(d$group == 1, "Rare-trait flagged", "Not rare-trait flagged"), N = d$n,
    Percent = paper_number(100 * d$proportion), ChiSq = paper_number(d$chi_square),
    p = ifelse(d$p_value < .001, "<0.001", paper_number(d$p_value, 3))
  )
  details <- paper_longtable(table, "Associations with the rare-trait screen")
  d <- results$timing_summary
  table <- data.frame(
    Study = paper_label(d$study), Group = paper_label(d$group), N = d$n,
    Mean = paper_number(d$mean), SD = paper_number(d$sd), Q1 = paper_number(d$p25),
    Median = paper_number(d$median), Q3 = paper_number(d$p75)
  )
  details <- c(details, paper_longtable(
    table,
    "Survey duration in seconds by screening group"
  ))
  d <- results$timing_models
  d <- d[d$term != "(Intercept)", ]
  table <- data.frame(
    Study = paper_label(d$study), Outcome = paper_label(d$outcome),
    Screen = paper_label(d$term), Estimate = paper_number(d$estimate, 3),
    SE = paper_number(d$std_error, 3), N = d$n
  )
  details <- c(details, paper_longtable(table,
    "Timing regressions: differences associated with each screen",
    widths = c(.14, .20, .24, .12, .10, .07)
  ))
  if ("timing_ks" %in% names(results)) {
    d <- results$timing_ks
    d <- d[c("study", "n_accepted", "n_other", "statistic", "p_value")]
    d$study <- paper_label(d$study)
    d$statistic <- paper_number(d$statistic, 4)
    d$p_value <- ifelse(d$p_value < .001, "<0.001", paper_number(d$p_value, 3))
    details <- c(details, paper_longtable(d,
      "Date-convention groups: duration distribution comparison",
      headers = paper_label(names(d))
    ))
  }
  put("survey_details", details)
  details <- character()
  for (name in c("experiment_effects", "experiment_interactions", "study1_linear_models")) {
    d <- results[[name]]
    if (is.null(d)) next
    if ("inference" %in% names(d)) d <- d[d$inference == "conventional", ]
    d <- d[d$term != "(Intercept)", ]
    table <- data.frame(
      Outcome = paper_label(d$outcome), Group = paper_label(d$group),
      Term = paper_label(d$term), Estimate = paper_number(d$estimate, 3),
      SE = paper_number(d$std_error, 3), N = d$n
    )
    caption <- switch(name,
      experiment_effects = "Economic experiment: subgroup contrasts (conventional SEs)",
      experiment_interactions = "Economic experiment: screening and timing interactions",
      study1_linear_models = "James experiment: linear probability specifications"
    )
    details <- c(details, paper_longtable(table, caption,
      widths = c(.17, .19, .24, .10, .09, .06)
    ))
  }
  if ("experiment_cells" %in% names(results)) {
    d <- results$experiment_cells
    table <- data.frame(
      Group = paper_label(d$group), Outcome = paper_label(d$outcome),
      Cue = ifelse(d$out_party == 1, "Out-party", "In-party"), N = d$observed,
      Mean = paper_number(d$mean, 3), Zero = paper_number(100 * d$proportion_zero, 1),
      One = paper_number(100 * d$proportion_one, 1)
    )
    details <- c(details, paper_longtable(table,
      "Economic outcome levels by treatment arm: means and endpoint percentages",
      widths = c(.22, .17, .15, .08, .10, .10, .10)
    ))
  }
  if ("experiment_cluster_inference" %in% names(results)) {
    d <- results$experiment_cluster_inference
    table <- data.frame(
      Group = paper_label(d$group), Outcome = paper_label(d$outcome), N = d$n,
      Clusters = d$clusters, Estimate = paper_number(d$estimate, 3),
      Lower = paper_number(d$conf_low, 3), Upper = paper_number(d$conf_high, 3)
    )
    details <- c(details, paper_longtable(table,
      "Economic experiment: 95 percent intervals with IP-clustered inference",
      widths = c(.19, .19, .08, .10, .12, .12, .12)
    ))
  }
  put("experiment_details", details)
  d <- results$ordered_coefficients
  table <- data.frame(
    Group = paper_label(d$group), Term = paper_label(d$term),
    Estimate = paper_number(d$estimate, 3), SE = paper_number(d$std_error, 3), N = d$n
  )
  details <- paper_longtable(table,
    "James experiment: ordered-logit coefficients and cutpoints",
    widths = c(.25, .32, .12, .10, .08)
  )
  d <- results$ordered_margins
  table <- data.frame(
    Group = paper_label(d$group), Attribute = paper_label(d$term),
    Outcome = c(
      `-1` = "Democratic", `0` = "Sales",
      `1` = "Republican"
    )[as.character(d$outcome)],
    Change = paper_number(100 * d$estimate, 2), SE = paper_number(100 * d$std_error, 2),
    N = d$n
  )
  details <- c(details, paper_longtable(table,
    "James experiment: average discrete changes in outcome probabilities (percentage points)",
    widths = c(.19, .24, .17, .10, .10, .07)
  ))
  if ("ordered_valid_profile_margins" %in% names(results)) {
    d <- results$ordered_valid_profile_margins
    d <- d[d$group %in% c("all", "unflagged") & d$outcome != 0, ]
    original <- results$ordered_margins
    key <- paste(d$group, d$term, d$outcome)
    matched <- match(key, paste(original$group, original$term, original$outcome))
    table <- data.frame(
      Group = paper_label(d$group), Attribute = paper_label(d$term),
      Outcome = ifelse(d$outcome == -1, "Democratic", "Republican"),
      Archived = paper_number(100 * original$estimate[matched], 3),
      Valid = paper_number(100 * d$estimate, 3)
    )
    details <- c(details, paper_longtable(table,
      "James experiment: valid-profile probability contrasts (percentage points)",
      widths = c(.19, .25, .20, .14, .14)
    ))
  }
  put("ordered_details", details)
  d <- results$attenuation
  table <- data.frame(
    Study = paper_label(d$study), Contrast = paper_label(d$outcome),
    Full = paper_number(d$full_effect, 3), Unflagged = paper_number(d$unflagged_effect, 3),
    Ratio = paper_number(d$retained_ratio, 3), Reduction = paper_number(100 * d$attenuation, 1)
  )
  boot <- results$attenuation_cluster_bootstrap
  if (!is.null(boot)) {
    key <- paste(d$study, d$outcome)
    boot <- boot[match(key, paste(boot$study, boot$outcome)), ]
    table$Lower <- paper_number(100 * boot$attenuation_conf_low, 1)
    table$Upper <- paper_number(100 * boot$attenuation_conf_high, 1)
  }
  put("attenuation", c(
    paper_longtable(table,
      "Full-sample and unflagged contrasts: descriptive retention ratios",
      widths = c(.12, .20, .07, .12, .07, .12, .08, .08)
    ),
    paste0(
      "\\noindent\\small Reduction is 100 times one minus the retention ",
      "ratio. It is a descriptive relative change, not the causal effect of ",
      "dishonesty. James contrasts are sign-aligned to the unflagged ",
      "estimate; those signs cancel in the ratios. Lower and Upper are 95 ",
      "percent percentile intervals for the relative reduction from a paired ",
      "IP-cluster bootstrap; full and unflagged estimates are refitted in ",
      "each draw. Respondent-bootstrap results are also supplied in ",
      "CSV.\\normalsize"
    )
  ))
  d <- results$survey_comparison
  if (!is.null(d)) {
    d <- d[abs(d$change_pp) > 1e-12, ]
    table <- data.frame(
      Study = paper_label(d$study), Measure = paper_label(d$measure),
      Original = paper_number(100 * d$proportion_original, 3),
      Corrected = paper_number(100 * d$proportion_updated, 3),
      OriginalN = d$n_original, CorrectedN = d$n_updated
    )
    put("corrections", paper_longtable(table,
      "Changed survey proportions after missing-value repairs",
      widths = c(.16, .24, .12, .12, .12, .12)
    ))
  }
  invisible(path)
}

write_paper_figures <- function(results, path = "figs") {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  d <- results$survey_prevalence
  measures <- c(paste0("rare_", c(
    "prosthetic", "blind", "deaf", "gang_resp", "gang_fam",
    "sleep"
  )), "likely_troll")
  grDevices::pdf(file.path(path, "rare_traits.pdf"),
    width = 7, height = 4.8,
    family = "Helvetica"
  )
  graphics::par(mar = c(4, 11.5, 1, 1), las = 1)
  graphics::plot(NA,
    xlim = c(0, max(d$proportion[d$measure %in% measures]) * 105),
    ylim = c(.5, length(measures) + .5), xlab = "Percent of records", ylab = "",
    yaxt = "n", bty = "n"
  )
  graphics::axis(2,
    at = seq_along(measures), labels = rev(paper_label(measures)),
    tick = FALSE
  )
  for (i in 1:3) {
    x <- d[d$study == paste0("study", i), ]
    graphics::points(100 * x$proportion[match(measures, x$measure)],
      rev(seq_along(measures)) + (i - 2) * .17,
      pch = c(16, 1, 17)[i], col = c("#222222", "#555555", "#176B83")[i]
    )
  }
  graphics::legend("bottomright",
    legend = paper_label(paste0("study", 1:3)),
    pch = c(16, 1, 17), col = c("#222222", "#555555", "#176B83"), bty = "n", cex = .9
  )
  grDevices::dev.off()
  d <- results$experiment_effects
  d <- d[d$term == "out_party_treat" & d$inference == "conventional", ]
  groups <- c("all", "unflagged", "flagged", "flagged_ip", "rare_responses", "high_hits")
  grDevices::pdf(file.path(path, "experiment_effects.pdf"),
    width = 7, height = 4.8,
    family = "Helvetica"
  )
  graphics::par(mfrow = c(1, 2), mar = c(4, 8, 2, 1), las = 1)
  lim <- range(c(d$conf_low, d$conf_high, 0))
  for (outcome in c("unemploy", "inflation")) {
    x <- d[d$outcome == outcome, ]
    x <- x[match(groups, x$group), ]
    y <- rev(seq_along(groups))
    graphics::plot(x$estimate, y,
      xlim = lim, ylim = c(.5, length(groups) + .5),
      xlab = "Effect on 0-1 scale", ylab = "", yaxt = "n", pch = 16, bty = "n",
      main = paper_label(outcome)
    )
    graphics::abline(v = 0, lty = 3, col = "gray60")
    graphics::segments(x$conf_low, y, x$conf_high, y)
    graphics::axis(2, at = y, labels = paper_label(groups), tick = FALSE, cex.axis = .85)
  }
  grDevices::dev.off()
  if ("rare_count_distribution" %in% names(results)) {
    d <- results$rare_count_distribution
    grDevices::pdf(file.path(path, "rare_counts.pdf"), width = 7, height = 3.5)
    graphics::par(mfrow = c(1, 3), mar = c(4, 4, 2, 1), las = 1)
    for (study in paste0("study", 1:3)) {
      x <- d[d$study == study, ]
      graphics::plot(x$count, 100 * x$proportion,
        type = "h", lwd = 8,
        xlim = c(-.3, 6.3), ylim = c(0, 100), xlab = "Affirmative items",
        ylab = "Percent", main = paper_label(study), bty = "n", col = "gray30"
      )
    }
    grDevices::dev.off()
  }
  if ("timing_ecdf" %in% names(results)) {
    d <- results$timing_ecdf
    grDevices::pdf(file.path(path, "date_timing.pdf"), width = 7, height = 3.5)
    graphics::par(mfrow = c(1, 2), mar = c(4, 4, 2, 1), las = 1)
    for (study in c("study2", "study3")) {
      x <- d[d$study == study, ]
      graphics::plot(NA,
        xlim = range(log1p(x$time)), ylim = c(0, 1),
        xlab = "Log(1 + duration in seconds)", ylab = "Cumulative share",
        main = paper_label(study), bty = "n"
      )
      groups <- unique(x$date_group)
      for (j in seq_along(groups)) {
        z <- x[x$date_group == groups[j], ]
        graphics::lines(log1p(z$time), z$cdf, type = "s", lty = j, col = "gray25")
      }
      graphics::legend("bottomright",
        legend = paper_label(groups),
        lty = seq_along(groups), bty = "n", cex = .8
      )
    }
    grDevices::dev.off()
  }
  invisible(path)
}
