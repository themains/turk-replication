# Reconstruct the survey variables used in the archived Stata programs.
numeric_response <- function(x) suppressWarnings(as.numeric(as.character(x)))

binary_response <- function(x, missing_zero = FALSE) {
  out <- rep(NA_real_, length(x))
  out[as.character(x) %in% c("TRUE", "1")] <- 1
  out[as.character(x) %in% c("FALSE", "0")] <- 0
  if (missing_zero) out[is.na(x) | as.character(x) == "NA"] <- 0
  out
}

survey_dates <- function(study) {
  if (study == "study2") {
    list(
      accepted = c(
        "06 29 2020", "06.29.2020", "06\\29\\2020", "06\\30\\2020",
        "6/29/20", "6/29/2020.", "6/29/20209", "6/29/2020`", "60/29/2020",
        "ju/26/2020", "june/29/20", "o6/29/2020", "16/29/2020"
      ),
      day_first = c(
        "20/06/2020", "28.06.2020", "28/06/2020", "28/6/2020",
        " 29 06 2020", "29-06-2020", "29-Jun-20", "29.06 2020",
        "29.06.2020", "29/06/2020", "29/6/2020", " 29/6/2020.",
        "29\\06\\2020", "29|06|2020", "30/06/2020"
      )
    )
  } else {
    list(
      accepted = c(
        "07 11 2020", "07/11.2020", "6/11/20", "7/10/20",
        "7/11/20", "7/11/19", "7/12/00", "7/12/20", "7/13/20"
      ),
      day_first = c(
        "11/7/20", "12/7/20", "12/7/2020", "12/7/2020/",
        "12\\07\\2020", "13/07/2020"
      )
    )
  }
}

prepare_survey_data <- function(inputs, corrected = TRUE) {
  stopifnot(all(c("study1", "study2", "study3") %in% names(inputs)))
  result <- lapply(c("study1", "study2", "study3"), function(study) {
    d <- inputs[[study]]
    indicators <- c(
      black = "blacklisted", miss = "missing_ip", dup = "duplicated",
      foreign = "foreign_ip", funny = "funny_ip"
    )
    for (key in names(indicators)) d[[key]] <- binary_response(d[[indicators[[key]]]])
    rare <- c("prosthetic", "blind", "deaf", "gang_resp", "gang_fam", "sleep")
    for (key in rare) d[[paste0("rare_", key)]] <- binary_response(d[[key]], TRUE)
    # The original deaf recode references blind; this has no effect in the archive.
    if (!corrected && study == "study1") {
      d$rare_deaf <- ifelse(d$deaf %in% 1, 1,
        ifelse(d$deaf %in% 0 | is.na(d$blind), 0, NA_real_)
      )
    }
    d$troll_index <- rowSums(d[paste0("rare_", rare)], na.rm = TRUE)
    d$likely_troll <- as.numeric(d$troll_index >= 2)
    d$combined_troll_1 <- as.numeric(d$funny == 1 | d$likely_troll == 1)
    d$badactor <- d$combined_troll_1
    d$time <- numeric_response(d$durationinseconds)
    thresholds <- switch(study,
      study1 = c(245, 1138),
      study2 = c(231.66667, 1112),
      study3 = c(176.66667, 799.33333)
    )
    d$fast <- as.numeric(d$time <= thresholds[1])
    d$slow <- as.numeric(d$time > thresholds[2])
    if (!corrected) {
      d$fast[is.na(d$time)] <- 0
      d$slow[is.na(d$time)] <- 1
    }
    sincerity <- numeric_response(if (study == "study1") d$trolling else d$sincerity)
    d$insincere <- as.numeric(sincerity >= if (study == "study1") 3 else 4)
    d$insincere_si <- as.numeric(sincerity >= 3)
    if (!corrected && study == "study3") d$insincere[is.na(sincerity)] <- 0
    if (study != "study1") {
      dates <- survey_dates(study)
      d$date_ok <- as.numeric(d$date %in% dates$accepted)
      d$date_poss_foreign <- as.numeric(d$date %in% dates$day_first)
      d$inattentive <- as.numeric(d$date_ok == 0 & d$date_poss_foreign == 0)
      d$combined_troll_2 <- as.numeric(d$combined_troll_1 == 1 | d$date_poss_foreign == 1)
      d$combined_troll_3 <- as.numeric(d$combined_troll_2 == 1 | d$inattentive == 1)
      d$foreign_dummy <- ifelse(d$date_poss_foreign == 1, 1, ifelse(d$date_ok == 1, 0, NA))
      d$hits_recode <- numeric_response(d$hits)
      d$hits_recode[!d$hits_recode %in% 1:4] <- NA_real_
    }
    if (study == "study2") {
      d$pid7 <- NA_real_
      sources <- c("pid_dem", "pid_dem", "pid_ind", "pid_ind", "pid_ind", "pid_rep", "pid_rep")
      values <- c(1, 2, 2, 3, 1, 2, 1)
      for (j in seq_along(sources)) d$pid7[d[[sources[j]]] %in% values[j]] <- j
      d$dem_rep <- ifelse(d$pid7 <= 3, 1, ifelse(d$pid7 >= 5, 0, NA))
      d$dem_treat <- ifelse(d$randomization_1 == "obama", 1,
        ifelse(d$randomization_1 == "congress", 0, NA)
      )
      d$out_party_treat <- as.numeric(d$dem_rep != d$dem_treat)
      for (outcome in c("unemployment", "inflation")) {
        gop <- numeric_response(d[[paste0("gop_", outcome)]])
        obama <- numeric_response(d[[paste0("obama_", outcome)]])
        y <- ifelse(is.na(gop), obama, gop)
        y[!y %in% 1:3] <- NA_real_
        d[[if (outcome == "unemployment") "unemploy" else outcome]] <- (3 - y) / 2
      }
    }
    if (study == "study1") {
      d$james_black <- as.numeric(d$cf_race %in% "black")
      d$james_gay <- as.numeric(d$cf_spouse %in% "Keith")
      d$james_lib <- as.numeric(d$cf_policy %in% "living-wage demonstrations")
      d$james_con <- as.numeric(d$cf_policy %in% "anti-tax demonstrations")
      evangelical <- paste0(
        "leads his son's Cub Scouts group, organized through ",
        "the Baptist Church the family attends"
      )
      d$james_evang <- as.numeric(d$cf_relig %in% evangelical)
      secular <- paste0(
        "leads his son's Junior Explorers group, organized through ",
        "the Secular Families Foundation"
      )
      d$james_aa <- as.numeric(d$cf_relig %in% secular)
      y <- numeric_response(d$james_cf)
      d$dem_cf <- ifelse(y %in% 1:3, as.numeric(y == 2), NA_real_)
      d$rep_cf <- ifelse(y %in% 1:3, as.numeric(y == 3), NA_real_)
      d$cf_ordered <- ordered(ifelse(y == 2, -1, ifelse(y == 3, 1, ifelse(y == 1, 0, NA))),
        levels = c(-1, 0, 1)
      )
    }
    d
  })
  names(result) <- c("study1", "study2", "study3")
  result
}

survey_results <- function(inputs, corrected = TRUE) {
  ds <- prepare_survey_data(inputs, corrected)
  prevalence <- crosses <- hits <- timing <- regressions <- list()
  rare_counts <- timing_ecdf <- timing_ks <- list()
  for (study in names(ds)) {
    d <- ds[[study]]
    rare_counts[[study]] <- data.frame(
      study,
      count = 0:6, n = as.integer(table(factor(d$troll_index, levels = 0:6))),
      proportion = as.numeric(table(factor(d$troll_index, levels = 0:6))) / nrow(d)
    )
    if (study != "study1") {
      for (value in 0:1) {
        time <- sort(d$time[d$date_ok == value & !is.na(d$time)])
        timing_ecdf[[length(timing_ecdf) + 1L]] <- data.frame(
          study,
          date_group = if (value == 1) "accepted" else "other", time,
          cdf = stats::ecdf(time)(time)
        )
      }
      accepted <- d$time[d$date_ok == 1 & !is.na(d$time)]
      other <- d$time[d$date_ok == 0 & !is.na(d$time)]
      comparison <- suppressWarnings(stats::ks.test(accepted, other, exact = FALSE))
      timing_ks[[study]] <- data.frame(
        study,
        n_accepted = length(accepted), n_other = length(other),
        statistic = unname(comparison$statistic), p_value = comparison$p.value,
        method = "Asymptotic two-sample KS; tied durations; descriptive comparison"
      )
    }
    measures <- c(
      "black", "miss", "dup", "foreign", "funny", paste0(
        "rare_",
        c("prosthetic", "blind", "deaf", "gang_resp", "gang_fam", "sleep")
      ),
      "likely_troll", "insincere", "insincere_si", "combined_troll_1", "fast", "slow"
    )
    if (study != "study1") {
      measures <- c(
        measures, "date_ok", "date_poss_foreign",
        "inattentive", "combined_troll_2", "combined_troll_3"
      )
    }
    for (measure in measures) {
      x <- d[[measure]]
      prevalence[[length(prevalence) + 1L]] <- data.frame(study, measure,
        count = sum(x == 1, na.rm = TRUE), n = sum(!is.na(x)), proportion = mean(x, na.rm = TRUE)
      )
    }
    for (measure in c("insincere", "funny")) {
      tab <- table(d[[measure]], d$likely_troll)
      chi <- suppressWarnings(stats::chisq.test(tab, correct = FALSE))
      for (group in 0:1) {
        x <- d[[measure]][d$likely_troll == group]
        crosses[[length(crosses) + 1L]] <- data.frame(study, measure, group,
          n = sum(!is.na(x)), count = sum(x == 1, na.rm = TRUE),
          proportion = mean(x, na.rm = TRUE), chi_square = unname(chi$statistic),
          p_value = chi$p.value
        )
      }
    }
    groups <- list(
      all = rep(TRUE, nrow(d)), flagged = d$combined_troll_1 == 1,
      unflagged = d$combined_troll_1 == 0
    )
    if (study != "study1") {
      groups$day_first <- d$date_poss_foreign == 1
      groups$flagged_date <- d$combined_troll_2 == 1
      groups$flagged_any_date <- d$combined_troll_3 == 1
      for (h in 1:4) {
        for (measure in c(
          "respondents", "combined_troll_1", "combined_troll_2",
          "combined_troll_3", "insincere", "time"
        )) {
          x <- if (measure == "respondents") {
            as.numeric(d$hits_recode == h)
          } else {
            d[[measure]][which(d$hits_recode == h)]
          }
          hits[[length(hits) + 1L]] <- data.frame(study,
            hits = h, measure,
            n = sum(!is.na(x)), estimate = mean(x, na.rm = TRUE)
          )
        }
      }
    }
    for (group in names(groups)) {
      x <- d$time[which(groups[[group]])]
      q <- quantile(x, c(.25, .5, .75), na.rm = TRUE, type = 2)
      timing[[length(timing) + 1L]] <- data.frame(study, group,
        n = sum(!is.na(x)),
        mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE),
        p25 = unname(q[1]), median = unname(q[2]), p75 = unname(q[3])
      )
    }
    predictors <- if (study == "study1") {
      "combined_troll_1"
    } else {
      c("combined_troll_1", "combined_troll_2", "combined_troll_3", "date_poss_foreign")
    }
    if (study == "study3") predictors <- c(predictors, "foreign_dummy")
    for (predictor in predictors) {
      for (outcome in c("time", "fast", "slow")) {
        fit <- stats::lm(stats::reformulate(predictor, outcome), data = d)
        regressions[[length(regressions) + 1L]] <- coefficient_table(fit, study, outcome, predictor)
      }
    }
  }
  d <- ds$study2
  cluster_ids <- c(
    17, 110, 117, 210, 414, 671, 759, 762, 805, 820, 932, 936, 937,
    1088, 1104, 1140, 1157, 1196, 1227, 1279, 1349, 1394
  )
  same_ids <- c(251, 511, 801, 933, 938, 983, 1089, 1114, 1127, 1179, 1199, 1221)
  dup_ids <- unique(d$ip_index[d$dup == 1])
  duplicate_timing <- data.frame(
    ip_index = dup_ids, historical_classification =
      ifelse(dup_ids %in% cluster_ids, "likely cluster",
        ifelse(dup_ids %in% same_ids, "likely same respondent", "uncertain")
      )
  )
  starts <- as.POSIXct(d$startdate, format = "%m/%d/%y %H:%M", tz = "UTC")
  ends <- as.POSIXct(d$enddate, format = "%m/%d/%y %H:%M", tz = "UTC")
  for (i in seq_len(nrow(duplicate_timing))) {
    index <- which(d$ip_index == duplicate_timing$ip_index[i])
    pairs <- utils::combn(index, 2)
    overlap <- pmin(ends[pairs[1, ]], ends[pairs[2, ]]) -
      pmax(starts[pairs[1, ]], starts[pairs[2, ]])
    minutes <- as.numeric(overlap, units = "mins")
    duplicate_timing$max_overlap_minutes[i] <- max(c(0, minutes))
    duplicate_timing$min_gap_minutes[i] <- if (any(minutes > 0)) NA_real_ else min(-minutes)
    duplicate_timing$timing_classification[i] <- if (any(minutes > 0)) {
      "overlapping"
    } else if (min(-minutes) <= 15) {
      "within 15 minutes"
    } else {
      "more than 15 minutes"
    }
  }
  list(
    survey_prevalence = do.call(rbind, prevalence), survey_crosstabs = do.call(rbind, crosses),
    hits_summary = do.call(rbind, hits), timing_summary = do.call(rbind, timing),
    timing_models = do.call(rbind, regressions), duplicate_timing = duplicate_timing,
    rare_count_distribution = do.call(rbind, rare_counts),
    timing_ecdf = do.call(rbind, timing_ecdf), timing_ks = do.call(rbind, timing_ks)
  )
}
