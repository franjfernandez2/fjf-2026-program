# The benchmark's discrepancy log must stay well formed: gate 1 is judged on it.

test_that("the discrepancy log is well formed", {
  log <- utils::read.csv(here::here("studies", "benchmark", "discrepancy-log.csv"), colClasses = "character")
  expect_equal(names(log), c(
    "id", "date", "source", "dataset", "variable", "finding", "evidence", "cause", "decision", "status"
  ))
  expect_true(all(grepl("^D-[0-9]{3}$", log$id)))
  expect_false(anyDuplicated(log$id) > 0)
  expect_false(any(is.na(as.Date(log$date, format = "%Y-%m-%d"))))
  expect_true(all(log$status %in% c("open", "explained")))
  expect_true(all(nzchar(log$finding) & nzchar(log$evidence)))
  # An explained discrepancy must say why and what is done about it.
  explained <- log[log$status == "explained", ]
  expect_true(all(nzchar(explained$cause) & nzchar(explained$decision)))
})

test_that("the traceability matrix is well formed", {
  matrix <- utils::read.csv(here::here("docs", "traceability-matrix.csv"), colClasses = "character")
  expect_equal(names(matrix), c(
    "output_id", "study", "type", "title", "sap_section", "population", "adam_datasets",
    "key_variables", "program", "qc_program", "qc_status", "shell_id"
  ))
  expect_false(anyDuplicated(paste(matrix$study, matrix$output_id)) > 0)
  expect_true(all(matrix$type %in% c("table", "listing", "figure")))
  expect_true(all(matrix$qc_status %in% c("not started", "in progress", "match", "explained", "mismatch")))
})

test_that("traceability rows list every output, and claim a match only where a second programming compared it", {
  outputs <- list(outputs = list(
    list(id = "T-1", type = "table", title = "A", sap = list("S1"), population = "SAF", adam = list("ADAE.AEDECOD", "ADSL.SAFFL"), program = "p.R"),
    list(id = "T-2", type = "table", title = "B", sap = list("S1", "S2"), population = "SAF", adam = list("ADAE.X"), program = "p.R"),
    list(id = "F-1", type = "figure", title = "C", sap = list("S2"), population = "SAF", adam = list(), program = "p.R")
  ))
  comparison <- list(cells = data.frame(id = c("T-1", "T-1", "T-2"), match = c(TRUE, TRUE, FALSE)))
  rows <- traceability_rows(outputs, "FJF-2026-999", "qc.R", comparison)
  expect_identical(rows$qc_status, c("match", "mismatch", "not started"))
  expect_identical(rows$qc_program, c("qc.R", "qc.R", ""))
  expect_identical(rows$adam_datasets[1], "ADAE; ADSL")
  expect_identical(rows$key_variables[1], "AEDECOD; SAFFL")
  expect_identical(rows$sap_section[2], "S1; S2")
  expect_identical(traceability_rows(outputs, "X", "qc.R", NULL, "ADR 1")$qc_status, rep("not started", 3))
})

test_that("the traceability matrix of the programme has a row for each output of each study", {
  matrix <- utils::read.csv(here::here("docs", "traceability-matrix.csv"), colClasses = "character")
  for (spec in c("fjf-2026-301-outputs.yml", "fjf-2026-201-outputs.yml", "fjf-2026-iss-outputs.yml", "fjf-2026-iss-treatment-outputs.yml")) {
    ids <- purrr::map_chr(read_outputs(here::here("docs", "sap", spec))$outputs, "id")
    expect_true(all(ids %in% matrix$output_id), info = spec)
  }
  # A match needs a second programming.
  expect_true(all(nzchar(matrix$qc_program[matrix$qc_status == "match"])))
})
