testthat::test_that("public inputs retain deposited samples and remove identifying metadata", {
  studies <- read_studies(file.path(project_root, "data/raw"))
  testthat::expect_equal(
    vapply(studies, nrow, integer(1)), c(study1 = 2000L, study2 = 1505L, study3 = 409L)
  )
  prohibited <- c(
    "recipientlastname", "recipientfirstname", "recipientemail", "externaldatareference",
    "shodanhostnames", "virustotaldetected_urls", "abuseipdbhost_name", "ip", "workerid",
    "geonameslat", "geonameslng", "maxmindpostalcode", "abuseipdbpostal_code"
  )
  for (study in studies) {
    testthat::expect_false(any(prohibited %in% names(study)))
    testthat::expect_false(any(vapply(study, is.factor, logical(1))))
  }
  testthat::expect_equal(sum(is.na(studies$study1$ip_index)), 9)
  testthat::expect_equal(sum(is.na(studies$study2$apivoidblacklistsdetections)), 29)
  testthat::expect_equal(sum(studies$study2$date == "[numeric response redacted]"), 1)
  testthat::expect_equal(sum(studies$study3$date == "[numeric response redacted]"), 2)
})

testthat::test_that("public input fingerprints match the provenance manifest", {
  manifest <- utils::read.csv(
    file.path(project_root, "docs/source-manifest.csv"),
    stringsAsFactors = FALSE
  )
  actual <- vapply(manifest$public_file, function(path) {
    digest::digest(file = file.path(project_root, path), algo = "sha256")
  }, character(1), USE.NAMES = FALSE)
  testthat::expect_equal(actual, manifest$public_sha256)
})
