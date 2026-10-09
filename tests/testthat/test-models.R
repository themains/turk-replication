testthat::test_that("June regressions match archived Stata estimates, SEs and samples", {
  d <- prepare_survey_data(read_studies(file.path(project_root, "data/raw")))
  result <- experiment_fits(d$study2)
  x <- result[result$term == "out_party_treat" & result$inference == "conventional", ]
  testthat::expect_lt(max(abs(x$estimate - c(
    -.0961677, -.0603604, -.1249392, -.0913058,
    -.0491649, -.0125270, -.0294971, .0228482, -.0434769, -.0170856, -.1495819, -.1006931
  ))), 1e-7)
  testthat::expect_lt(max(abs(x$std_error - c(
    .0154394, .0162842, .0201012, .0208671,
    .0233663, .0259369, .0444569, .0484049, .0255402, .0283762, .0247034, .0261916
  ))), 1e-7)
  testthat::expect_equal(x$n, rep(c(1425, 861, 564, 182, 445, 505), each = 2))
})

testthat::test_that("ordered-logit margins and uncertainty match the original log", {
  d <- prepare_survey_data(read_studies(file.path(project_root, "data/raw")))$study1
  fit <- fit_ordered(d[d$badactor == 1, ])
  testthat::expect_equal(unname(coef(fit)), c(
    -.2103308, -.3829606, -.0534315,
    -.3025838, -.1072810, .4841341
  ), tolerance = 1e-6)
  result <- ordered_margins(fit, "flagged")
  black_dem <- result[result$term == "james_black" & result$outcome == -1, ]
  testthat::expect_equal(black_dem$estimate, .0505539, tolerance = 5e-6)
  # The historical attenuation spreadsheet transcribed this SE as .0430562.
  testthat::expect_equal(black_dem$std_error, .0412444, tolerance = 5e-6)
  testthat::expect_equal(black_dem$n, 484L)
  total <- as.numeric(tapply(result$estimate, result$term, sum))
  testthat::expect_equal(total, rep(0, 6), tolerance = 1e-12)
  full <- ordered_margins(fit_ordered(d), "all")
  testthat::expect_equal(full$estimate[1], .1171434, tolerance = 5e-6)
  testthat::expect_equal(full$std_error[1], .0202718, tolerance = 5e-6)
})

testthat::test_that("HC1 uses the finite-sample sandwich correction", {
  d <- prepare_survey_data(read_studies(file.path(project_root, "data/raw")))$study2
  fit <- stats::lm(unemploy ~ out_party_treat, d)
  expected <- sandwich::vcovHC(fit, type = "HC1")
  testthat::expect_equal(hc1_covariance(fit), expected, tolerance = 1e-12)
})

testthat::test_that("joint bootstrap is deterministic across serial and parallel runs", {
  prepared <- prepare_survey_data(read_studies(file.path(project_root, "data/raw")))
  first <- bootstrap_attenuation(prepared, draws = 9L, seed = 104L)
  second <- bootstrap_attenuation(prepared, draws = 9L, seed = 104L, cores = 2L)
  testthat::expect_equal(first, second)
  testthat::expect_true(all(first$successful_draws == 9L))
  testthat::expect_true(all(first$failed_draws == 0L))
  testthat::expect_equal(tail(first$retained_ratio, 2), c(.7697158, .6610799), tolerance = 5e-6)
  testthat::expect_equal(first$attenuation_conf_low, 1 - first$ratio_conf_high)
})

testthat::test_that("all archived ordered margins reproduce with the documented SE correction", {
  d <- prepare_survey_data(read_studies(file.path(project_root, "data/raw")))$study1
  original <- utils::read.csv(file.path(project_root, "original/data/study1_attenuation.csv"))
  groups <- list(
    unflagged = d$badactor == 0, flagged = d$badactor == 1,
    all = rep(TRUE, nrow(d))
  )
  columns <- c(unflagged = "nonsusp", flagged = "susp", all = "full")
  for (group in names(groups)) {
    margins <- ordered_margins(fit_ordered(d[groups[[group]], ]), group)
    margins <- margins[margins$outcome != 0, ]
    expected <- original[[paste0(columns[group], "_beta")]]
    testthat::expect_lt(max(abs(abs(margins$estimate) - expected)), 1e-7)
    expected_se <- original[[paste0(columns[group], "_se")]]
    if (group == "flagged") expected_se[1] <- .0412444
    testthat::expect_lt(max(abs(margins$std_error - expected_se)), 1e-7)
  }
})

testthat::test_that("interactive models reproduce the archived contrasts", {
  d <- prepare_survey_data(read_studies(file.path(project_root, "data/raw")))
  june <- stats::lm(unemploy ~ out_party_treat * combined_troll_1, d$study2)
  term <- "out_party_treat:combined_troll_1"
  testthat::expect_lt(abs(stats::coef(june)[[term]] - .0757743), 1e-7)
  testthat::expect_lt(abs(sqrt(stats::vcov(june)[term, term]) - .0312104), 1e-7)
  august <- fit_ordered(d$study1, "badactor")
  term <- "james_black:badactor"
  testthat::expect_lt(abs(stats::coef(august)[[term]] - .4126588), 1e-6)
  testthat::expect_lt(abs(sqrt(stats::vcov(august)[term, term]) - .1988947), 1e-6)
})

testthat::test_that("predictive margins sum to one and recover the discrete effects", {
  d <- prepare_survey_data(read_studies(file.path(project_root, "data/raw")))$study1
  fit <- fit_ordered(d)
  predictions <- ordered_predictions(fit, "all")
  margins <- ordered_margins(fit, "all")
  sums <- stats::aggregate(estimate ~ term + value, predictions, sum)
  testthat::expect_equal(sums$estimate, rep(1, 12))
  differences <- predictions$estimate[predictions$value == 1] -
    predictions$estimate[predictions$value == 0]
  testthat::expect_equal(differences, margins$estimate, tolerance = 1e-12)
})

testthat::test_that("sign-aligned attenuation inputs retain reversed evangelical effects", {
  result <- model_results(prepare_survey_data(read_studies(file.path(project_root, "data/raw"))))
  x <- result$attenuation_summary_inputs
  evang <- x[x$study == "study1" & grepl("james_evang", x$group), ]
  testthat::expect_true(all(evang$nonsusp_beta > 0))
  testthat::expect_true(all(evang$susp_beta < 0))
  testthat::expect_lt(max(abs(evang$susp_beta - c(-.0128309, -.0093837))), 1e-7)
})

testthat::test_that("valid factorial contrasts use mutually exclusive category profiles", {
  d <- prepare_survey_data(read_studies(file.path(project_root, "data/raw")))$study1
  fit <- fit_ordered(d)
  original <- ordered_margins(fit, "all")
  valid <- ordered_margins(fit, "all", valid_profiles = TRUE)
  binary <- original$term %in% c("james_black", "james_gay")
  testthat::expect_equal(valid[binary, ], original[binary, ])
  observed <- d[!is.na(d$cf_ordered), ]
  baseline <- exposed <- observed
  baseline$james_aa <- exposed$james_aa <- 0
  baseline$james_evang <- 0
  exposed$james_evang <- 1
  exposed_probability <- stats::predict(fit, exposed, type = "probs")[, "-1"]
  baseline_probability <- stats::predict(fit, baseline, type = "probs")[, "-1"]
  independent <- mean(exposed_probability - baseline_probability)
  row <- valid$term == "james_evang" & valid$outcome == -1
  testthat::expect_equal(valid$estimate[row], independent, tolerance = 1e-12)
  testthat::expect_lt(abs(valid$estimate[row] - (-.04411228)), 1e-7)
})
