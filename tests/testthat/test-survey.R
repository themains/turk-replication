testthat::test_that("the public waves preserve recorded sample membership", {
  inputs <- read_studies(file.path(project_root, "data/raw"))
  d <- prepare_survey_data(inputs)
  testthat::expect_equal(
    vapply(d, nrow, integer(1)), c(study1 = 2000L, study2 = 1505L, study3 = 409L)
  )
  testthat::expect_equal(
    vapply(d, function(x) sum(x$likely_troll), numeric(1)),
    c(study1 = 125, study2 = 452, study3 = 86)
  )
  testthat::expect_equal(
    vapply(d, function(x) sum(x$combined_troll_1), numeric(1)),
    c(study1 = 493, study2 = 571, study3 = 113)
  )
  testthat::expect_equal(sum(d$study2$date_poss_foreign), 301)
  testthat::expect_equal(sum(d$study3$date_poss_foreign), 67)
  testthat::expect_equal(sum(d$study2$combined_troll_3), 696)
  testthat::expect_equal(sum(d$study3$combined_troll_3), 144)
})

testthat::test_that("missing duration and sincerity remain missing", {
  d <- prepare_survey_data(read_studies(file.path(project_root, "data/raw")))
  testthat::expect_equal(sum(d$study1$slow, na.rm = TRUE), 224)
  testthat::expect_equal(sum(!is.na(d$study1$slow)), 1991)
  testthat::expect_equal(sum(is.na(d$study1$fast)), 9)
  testthat::expect_equal(mean(d$study3$insincere, na.rm = TRUE), 58 / 407)
})

testthat::test_that("rare-item missingness follows the archived index convention explicitly", {
  testthat::expect_equal(binary_response(c(TRUE, FALSE, NA)), c(1, 0, NA))
  testthat::expect_equal(binary_response(c("1", "0", "NA"), TRUE), c(1, 0, 0))
  testthat::expect_equal(binary_response(c("TRUE", "FALSE", NA), TRUE), c(1, 0, 0))
  testthat::expect_equal(binary_response(c("unknown", "")), c(NA_real_, NA_real_))
})

testthat::test_that("published HIT and duplicate classifications retain their denominators", {
  results <- survey_results(read_studies(file.path(project_root, "data/raw")))
  testthat::expect_equal(nrow(results$duplicate_timing), 37L)
  counts <- as.integer(table(results$duplicate_timing$historical_classification))
  testthat::expect_equal(counts, c(22L, 12L, 3L))
  rows <- results$hits_summary[results$hits_summary$measure == "respondents", ]
  testthat::expect_equal(as.numeric(tapply(rows$estimate, rows$study, sum)), c(1, 1))
})

testthat::test_that("minute-resolution timestamps support the archived duplicate timing groups", {
  d <- survey_results(read_studies(file.path(project_root, "data/raw")))$duplicate_timing
  clusters <- d$historical_classification == "likely cluster"
  testthat::expect_true(all(d$max_overlap_minutes[clusters] > 0))
  same <- d$historical_classification == "likely same respondent"
  testthat::expect_true(all(d$max_overlap_minutes[same] == 0))
  testthat::expect_true(all(d$min_gap_minutes[same] <= 15))
  testthat::expect_true(all(d$min_gap_minutes[d$historical_classification == "uncertain"] > 15))
})

testthat::test_that("rare-response and timing distributions conserve their samples", {
  result <- survey_results(read_studies(file.path(project_root, "data/raw")))
  counts <- stats::aggregate(n ~ study, result$rare_count_distribution, sum)
  testthat::expect_equal(counts$n, c(2000L, 1505L, 409L))
  for (study in c("study2", "study3")) {
    for (group in c("accepted", "other")) {
      rows <- result$timing_ecdf$study == study & result$timing_ecdf$date_group == group
      testthat::expect_equal(max(result$timing_ecdf$cdf[rows]), 1)
      testthat::expect_true(all(diff(result$timing_ecdf$cdf[rows]) >= 0))
    }
  }
})
