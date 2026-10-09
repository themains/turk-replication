testthat::test_that("IP flags reproduce independently reconstructed classification", {
  result <- analyze_ip(read_studies(file.path(project_root, "data/raw")))
  testthat::expect_true(all(result$ip_flag_checks$disagreements == 0))
  testthat::expect_equal(result$ip_summary$funny_ip, c(406, 182, 40))
  testthat::expect_equal(result$ip_summary$duplicated, c(106, 81, 13))
  testthat::expect_equal(result$ip_summary$n_unique_ip, c(1924L, 1461L, 402L))
})

testthat::test_that("missing addresses are not counted as one duplicated address", {
  result <- analyze_ip(read_studies(file.path(project_root, "data/raw")))
  observed <- stats::aggregate(n_responses ~ study, result$ip_multiplicity, sum)
  addresses <- stats::aggregate(n_addresses ~ study, result$ip_multiplicity, sum)
  testthat::expect_equal(observed$n_responses, c(1991, 1505, 409))
  testthat::expect_equal(addresses$n_addresses, c(1924, 1461, 402))
  testthat::expect_false(any(result$ip_multiplicity$multiplicity == 9))
})

testthat::test_that("blacklist sensitivity preserves unknown service outcomes", {
  results <- analyze_ip(read_studies(file.path(project_root, "data/raw")))
  sensitivity <- results$ip_missing_sensitivity
  testthat::expect_equal(sensitivity$unknown_blacklist, c(9, 29, 5))
  testthat::expect_equal(sensitivity$unknown_unflagged, c(0, 28, 5))
  testthat::expect_equal(sensitivity$pct_upper, 100 * c(406 / 2000, 210 / 1505, 45 / 409))
})

testthat::test_that("geography frequencies count respondents with observed locations", {
  studies <- read_studies(file.path(project_root, "data/raw"))
  result <- analyze_ip(studies)
  country <- stats::aggregate(n ~ study, result$country_counts, sum)
  city <- stats::aggregate(n ~ study, result$city_counts, sum)
  expected_country <- vapply(studies, function(x) sum(!is.na(x$maxmindcountrynamesen)), integer(1))
  expected_city <- vapply(studies, function(x) sum(!is.na(x$maxmindcitynamesen)), integer(1))
  testthat::expect_equal(country$n, unname(expected_country))
  testthat::expect_equal(city$n, unname(expected_city))
})

testthat::test_that("survey-unit sensitivity recomputes IP duplication without changing baseline", {
  studies <- read_studies(file.path(project_root, "data/raw"))
  result <- analyze_ip(studies)
  sensitivity <- result$ip_unit_sensitivity
  deposited <- sensitivity[sensitivity$scenario == "deposited_rows", ]
  unique_survey <- sensitivity[sensitivity$scenario == "unique_observed_survey", ]
  testthat::expect_equal(deposited$n, c(2000L, 1505L, 409L))
  testthat::expect_equal(unique_survey$n, c(1999L, 1505L, 409L))
  testthat::expect_equal(unique_survey$duplicate_observed_survey_rows, c(1L, 0L, 0L))
  testthat::expect_equal(unique_survey$ip_flagged, c(404, 182, 40))
  testthat::expect_equal(unique_survey$combined_lower, c(491, 571, 113))
  testthat::expect_equal(result$ip_summary$funny_ip, c(406, 182, 40))
  testthat::expect_equal(deposited$rare_lower, c(125, 452, 86))
  testthat::expect_equal(deposited$rare_upper, c(134, 455, 88))
  testthat::expect_equal(deposited$combined_upper, c(493, 574, 113))
  testthat::expect_equal(unique_survey$observed_survey_rows, c(1990, 1505, 409))
})
