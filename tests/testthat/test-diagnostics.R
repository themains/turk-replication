testthat::test_that("cell means reproduce the experimental contrasts", {
  prepared <- prepare_survey_data(read_studies(file.path(project_root, "data/raw")))
  diagnostic <- analysis_diagnostics(prepared)
  cells <- diagnostic$experiment_cells
  effects <- experiment_fits(prepared$study2)
  for (group in unique(cells$group)) {
    for (outcome in unique(cells$outcome)) {
      subset <- cells[cells$group == group & cells$outcome == outcome, ]
      keep <- effects$group == group & effects$outcome == outcome &
        effects$term == "out_party_treat" & effects$inference == "conventional"
      effect <- effects[keep, ]
      testthat::expect_equal(diff(subset$mean), effect$estimate, tolerance = 1e-12)
      testthat::expect_equal(sum(subset$observed), effect$n)
    }
  }
})

testthat::test_that("leave-one-out algebra matches an actual refit", {
  d <- prepare_survey_data(read_studies(file.path(project_root, "data/raw")))$study2
  d <- d[stats::complete.cases(d[c("unemploy", "out_party_treat")]), ]
  fit <- stats::lm(unemploy ~ out_party_treat, d)
  delta <- stats::dfbeta(fit)[, "out_party_treat"]
  j <- which.max(abs(delta))
  refit <- stats::lm(unemploy ~ out_party_treat, d[-j, ])
  testthat::expect_equal(unname(stats::coef(fit)[2] - delta[j]),
    unname(stats::coef(refit)[2]),
    tolerance = 1e-12
  )
})
