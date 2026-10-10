analysis_diagnostics <- function(prepared) {
  d <- prepared$study2
  cells <- inference <- influence <- list()
  for (group in c("all", "unflagged", "flagged")) {
    selected <- switch(group,
      all = rep(TRUE, nrow(d)),
      unflagged = d$combined_troll_1 == 0,
      flagged = d$combined_troll_1 == 1
    )
    for (outcome in c("unemploy", "inflation")) {
      s <- d[which(selected), ]
      for (arm in 0:1) {
        eligible <- s[which(s$out_party_treat == arm), ]
        y <- eligible[[outcome]]
        cells[[length(cells) + 1L]] <- data.frame(
          study = "study2", group, outcome, out_party = arm,
          assigned = length(y), observed = sum(!is.na(y)), missing = sum(is.na(y)),
          mean = mean(y, na.rm = TRUE), sd = stats::sd(y, na.rm = TRUE),
          proportion_zero = mean(y == 0, na.rm = TRUE),
          proportion_one = mean(y == 1, na.rm = TRUE)
        )
      }
      s <- s[stats::complete.cases(s[c(outcome, "out_party_treat")]), ]
      fit <- stats::lm(stats::reformulate("out_party_treat", outcome), s)
      key <- as.character(s$ip_index)
      key[is.na(key)] <- paste0("missing_", which(is.na(key)))
      v <- sandwich::vcovCL(fit, cluster = key, type = "HC1")
      cluster_count <- length(unique(key))
      b <- stats::coef(fit)[["out_party_treat"]]
      se <- sqrt(v["out_party_treat", "out_party_treat"])
      critical <- stats::qt(.975, cluster_count - 1L)
      inference[[length(inference) + 1L]] <- data.frame(
        study = "study2", group, outcome, n = nrow(s), clusters = cluster_count,
        estimate = b, std_error = se, conf_low = b - critical * se,
        conf_high = b + critical * se,
        p_value = 2 * stats::pt(-abs(b / se), cluster_count - 1L)
      )
      deleted <- b - stats::dfbeta(fit)[, "out_party_treat"]
      influence[[length(influence) + 1L]] <- data.frame(
        study = "study2", group, outcome, estimate = b,
        leave_one_out_min = min(deleted), leave_one_out_max = max(deleted),
        max_cooks_distance = max(stats::cooks.distance(fit)),
        largest_absolute_change = max(abs(deleted - b))
      )
    }
  }
  assignment <- lapply(c("congress", "obama"), function(arm) {
    s <- d[which(d$randomization_1 == arm), ]
    data.frame(
      assigned_label = arm, n = nrow(s),
      mean_age_category = mean(numeric_response(s$age), na.rm = TRUE),
      age_observed = sum(!is.na(numeric_response(s$age))),
      flagged_n = sum(s$combined_troll_1, na.rm = TRUE),
      flagged_proportion = mean(s$combined_troll_1, na.rm = TRUE),
      eligible_partisan = sum(!is.na(s$out_party_treat)),
      unemployment_observed = sum(!is.na(s$unemploy)),
      inflation_observed = sum(!is.na(s$inflation))
    )
  })
  missingness <- do.call(rbind, lapply(names(prepared), function(study) {
    s <- prepared[[study]]
    columns <- c("time", "insincere", "funny", "rare_prosthetic", "rare_blind", "rare_deaf")
    data.frame(study,
      variable = columns, n = nrow(s),
      missing = vapply(s[columns], function(x) sum(is.na(x)), integer(1))
    )
  }))
  rare_sensitivity <- do.call(rbind, lapply(names(prepared), function(study) {
    s <- prepared[[study]]
    columns <- c("prosthetic", "blind", "deaf", "gang_resp", "gang_fam", "sleep")
    original <- s[columns]
    missing <- rowSums(is.na(original))
    positive <- rowSums(original == 1, na.rm = TRUE)
    lower <- positive >= 2
    upper <- positive + missing >= 2
    data.frame(study,
      n = nrow(s), any_missing = sum(missing > 0),
      rare_lower = sum(lower), rare_upper = sum(upper),
      combined_lower = sum(lower | s$funny == 1, na.rm = TRUE),
      combined_upper = sum(upper | s$funny == 1, na.rm = TRUE)
    )
  }))
  costs <- data.frame(
    study = c("study2", "study3", "Lucid illustrative", "Lucid targeted illustrative"),
    payment = c(.75, .35, 1, 2), fee_multiplier = c(1.3, 1.3, 1, 1),
    minutes = c(15, 5, 10, 10),
    flagged_share = c(mean(prepared$study2$funny), mean(prepared$study3$funny), .02, .016),
    source = c(
      rep("Published study payment and duration; rebuilt IP rate", 2),
      rep("Published illustrative payment, duration, and rounded IP rate", 2)
    )
  )
  costs$cents_per_retained_minute <-
    100 * costs$payment * costs$fee_multiplier / ((1 - costs$flagged_share) * costs$minutes)
  list(
    historical_costs = costs, rare_missing_sensitivity = rare_sensitivity,
    experiment_cells = do.call(rbind, cells),
    experiment_cluster_inference = do.call(rbind, inference),
    experiment_influence = do.call(rbind, influence),
    assignment_diagnostics = do.call(rbind, assignment),
    analysis_missingness = missingness
  )
}
