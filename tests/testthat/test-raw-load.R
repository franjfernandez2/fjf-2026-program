# Loading raw files into the database. The analysis side cannot use the simulator, so these
# tests build files from the specification itself: one row per table, with valid codes.

root <- test_path("..", "..")
spec <- read_raw_spec(file.path(root, "docs", "specs", "raw-layer.yml"))
protocols <- list(
  `201` = read_protocol(file.path(root, "docs", "protocols", "fjf-2026-201.yml")),
  `301` = read_protocol(file.path(root, "docs", "protocols", "fjf-2026-301.yml"))
)

# One valid row per table, from the specification.
sample_rows <- function(study) {
  lists <- raw_codelists(spec, protocols[[study]])
  tables <- lapply(spec$tables, function(t) {
    row <- lapply(t$columns, function(c) {
      if (!is.null(c$codelist)) {
        return(lists[[c$codelist]][1])
      }
      switch(c$type,
        text = "x",
        integer = 1L,
        numeric = 1.5
      )
    })
    df <- as.data.frame(stats::setNames(row, purrr::map_chr(t$columns, "name")), check.names = FALSE)
    df$subject_id <- paste0(study, "-001-0001")
    df
  })
  stats::setNames(tables, purrr::map_chr(spec$tables, "name"))
}

write_sample <- function(dir) {
  rows <- list()
  for (study in names(protocols)) {
    dir.create(file.path(dir, study))
    for (t in names(sample_rows(study))) {
      f <- file.path(dir, study, paste0(t, ".csv"))
      utils::write.csv(sample_rows(study)[[t]], f, row.names = FALSE, na = "")
      rows[[length(rows) + 1]] <- data.frame(
        file = file.path(study, paste0(t, ".csv")), rows = 1L, sha256 = digest::digest(file = f, algo = "sha256")
      )
    }
  }
  utils::write.csv(do.call(rbind, rows), file.path(dir, "manifest.csv"), row.names = FALSE)
}

test_that("raw files are read with the types of the specification and checked against the manifest", {
  dir <- withr::local_tempdir()
  write_sample(dir)
  raw <- read_raw_files(dir, spec)
  expect_setequal(names(raw), c("201", "301"))
  expect_identical(class(raw[["301"]]$randomisation$record_id), "integer")
  expect_identical(raw[["301"]]$randomisation$arm_code, "PBO")
  cat("tampered\n", file = file.path(dir, "301", "visits.csv"), append = TRUE)
  expect_error(read_raw_files(dir, spec), "Hash")
})

test_that("each study loads once into its schema, audited", {
  with_test_db()
  loader <- test_connect("loader")
  reader <- test_connect("reader")
  withr::defer({
    DBI::dbDisconnect(loader)
    DBI::dbDisconnect(reader)
  })
  dir <- withr::local_tempdir()
  write_sample(dir)
  raw <- read_raw_files(dir, spec)
  for (study in names(raw)) {
    n <- load_raw_study(loader, paste0("raw_", study), raw[[study]])
    expect_identical(sum(n), length(spec$tables))
  }
  got <- DBI::dbGetQuery(reader, "SELECT subject_id FROM raw_201.adverse_events")$subject_id
  expect_identical(got, "201-001-0001")
  logged <- DBI::dbGetQuery(reader, "SELECT count(*) AS n FROM audit.change_log WHERE operation = 'INSERT'")$n
  expect_equal(as.integer(logged), 2L * length(spec$tables))
  expect_error(load_raw_study(loader, "raw_301", raw[["301"]]), "loaded once")
})

test_that("the committed study data, once generated, match their manifest and the specification", {
  dir <- file.path(root, "data", "raw")
  skip_if_not(file.exists(file.path(dir, "manifest.csv")), "Study data not generated yet (after the SAP tags)")
  raw <- read_raw_files(dir, spec)
  expect_setequal(names(raw), c("201", "301"))
})
