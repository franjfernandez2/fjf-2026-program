# Data errors: of the kinds an EDC lets through, inside the specification, and logged for
# these tests only.

clean <- simulate_study(design, planning, "301", 1000, seed = 31)
dirty <- add_data_errors(clean, seed = 31, study = "301", spec = design$raw_spec)
lists <- study_codelists("301")

test_that("every kind of error is introduced", {
  kinds <- unique(dirty$log$kind)
  expected <- c(
    "partial start date", "missing severity", "end before start", "year-only diagnosis date",
    "partial birth date", "height in metres", "visit out of window", "weight in lb", "duplicate record"
  )
  expect_true(all(expected %in% kinds), info = paste(setdiff(expected, kinds), collapse = ", "))
})

test_that("the tables with errors still pass the database's edit checks", {
  for (t in design$raw_spec$tables) {
    d <- dirty$tables[[t$name]]
    expect_identical(names(d), purrr::map_chr(t$columns, "name"))
    expect_identical(d$record_id, seq_len(nrow(d)))
    for (c in t$columns) {
      if (!isTRUE(c$nullable)) expect_false(anyNA(d[[c$name]]), info = paste(t$name, c$name))
      if (!is.null(c$codelist)) expect_true(all(stats::na.omit(d[[c$name]]) %in% lists[[c$codelist]]))
    }
  }
})

test_that("duplicates are exact copies under a new record id", {
  lab <- dirty$tables$lab_results
  key <- do.call(paste, lab[names(lab) != "record_id"])
  n_dup <- sum(dirty$log$kind == "duplicate record" & dirty$log$table == "lab_results")
  expect_gt(n_dup, 0)
  expect_identical(sum(duplicated(key)), n_dup)
  expect_identical(nrow(lab), nrow(clean$lab_results) + n_dup)
})

test_that("a visit moved out of its window takes its measurements with it", {
  v <- dirty$tables$visits
  moved <- unique(dirty$log$subject_id[dirty$log$kind == "visit out of window"])
  expect_gt(length(moved), 0)
  both <- merge(v[v$subject_id %in% moved, ], dirty$tables$lab_results, by = c("subject_id", "visit_code"))
  both <- both[both$visit_code != "UNS", ]
  expect_true(all(both$visit_date == both$collection_date))
})

test_that("the errors depend only on the seed, and leave the efficacy values alone", {
  again <- add_data_errors(clean, seed = 31, study = "301", spec = design$raw_spec)
  expect_identical(again$tables, dirty$tables)
  hb <- function(t) {
    x <- t$lab_results[t$lab_results$test_code == "HBA1C", c("subject_id", "visit_code", "result")]
    unique(x[order(x$subject_id, x$visit_code), ])
  }
  expect_identical(hb(dirty$tables)$result, hb(clean)$result)
})

test_that("generation writes every table with a manifest, and never the truth", {
  dir <- withr::local_tempdir()
  prog <- generate_programme(design, 99, c(`201` = 6, `301` = 8))
  manifest <- write_programme(prog, dir)
  expect_identical(nrow(manifest), 2L * length(design$raw_spec$tables))
  expect_true(all(file.exists(file.path(dir, manifest$file))))
  expect_identical(manifest$sha256[1], digest::digest(file = file.path(dir, manifest$file[1]), algo = "sha256"))
  expect_false(any(grepl("truth", list.files(dir, recursive = TRUE))))
  expect_identical(names(prog), c("201", "301"))
})
