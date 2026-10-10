read_studies <- function(data_dir = "data/raw") {
  files <- file.path(data_dir, paste0("study", 1:3, ".csv"))
  if (!all(file.exists(files))) {
    stop("Missing public study data: ", paste(files[!file.exists(files)], collapse = ", "))
  }
  studies <- lapply(files, utils::read.csv, stringsAsFactors = FALSE)
  names(studies) <- paste0("study", 1:3)
  required <- c(
    "ip_index", "pollname", "blacklisted", "missing_ip", "duplicated",
    "foreign_ip", "funny_ip", "maxmindcountrynamesen", "maxmindcitynamesen"
  )
  for (study in studies) {
    if (!all(required %in% names(study))) stop("Incomplete public study schema")
    if (anyDuplicated(names(study))) stop("Duplicate study columns")
  }
  studies
}

prepare_public_inputs <- function(archive, data_dir = "data/raw") {
  members <- c(
    "replication_public/data/turk_08_17_2018/turk_recoded_public.csv",
    paste0(
      "replication_public/data/turk_06_29_2020/",
      "merged_survey_ip_06_29_2020_final_public.csv"
    ),
    paste0(
      "replication_public/data/turk_07_12_2020/",
      "merged_survey_ip_07_12_2020_final_public.csv"
    )
  )
  omitted <- c(
    "recipientlastname", "recipientfirstname", "recipientemail", "externaldatareference",
    "shodanhostnames", "abuseipdbhost_name", "virustotaldetected_urls"
  )
  retained_metadata <- c(
    "maxmindcitynamesen", "maxmindcountrynamesen", "ipvoidblacklist_status",
    "apivoidblacklistsdetections", "apivoidanonymityis_vpn"
  )
  directory <- tempfile("turk-public-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE))
  utils::unzip(archive, files = members, exdir = directory)
  dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
  manifest <- lapply(seq_along(members), function(i) {
    source_file <- file.path(directory, members[i])
    original <- utils::read.csv(
      source_file,
      stringsAsFactors = FALSE, colClasses = "character",
      na.strings = NULL, check.names = FALSE, fileEncoding = "latin1"
    )
    infrastructure <- grep(
      "^(maxmind|geonames|tzwhere|abuseipdb|ipvoid|apivoid|censys|shodan|virustotal)",
      names(original),
      value = TRUE
    )
    removed <- intersect(
      names(original), union(omitted, setdiff(infrastructure, retained_metadata))
    )
    public <- original[setdiff(names(original), removed)]
    redacted <- 0L
    if ("date" %in% names(public)) {
      phone_shaped <- grepl("^[0-9]{10}$", public$date) & !grepl("2020", public$date)
      redacted <- sum(phone_shaped)
      public$date[phone_shaped] <- "[numeric response redacted]"
    }
    output <- file.path(data_dir, paste0("study", i, ".csv"))
    utils::write.csv(public, output, row.names = FALSE, fileEncoding = "UTF-8")
    data.frame(
      study = paste0("study", i), source_doi = "10.7910/DVN/KF1LAK",
      source_version = "1.0", archive_member = members[i],
      source_sha256 = digest::digest(file = source_file, algo = "sha256"),
      public_file = output,
      public_sha256 = digest::digest(file = output, algo = "sha256"),
      n_rows = nrow(public), original_columns = ncol(original),
      retained_columns = ncol(public), removed_columns = paste(removed, collapse = ";"),
      redacted_date_cells = redacted
    )
  })
  do.call(rbind, manifest)
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) != 1L) stop("Usage: Rscript R/01_anonymize.R ARCHIVE.zip")
  manifest <- prepare_public_inputs(args[1])
  utils::write.csv(manifest, "docs/source-manifest.csv", row.names = FALSE)
}
