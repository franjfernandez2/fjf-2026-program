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
  expect_false(anyDuplicated(matrix$output_id) > 0)
  expect_true(all(matrix$type %in% c("table", "listing", "figure")))
  expect_true(all(matrix$qc_status %in% c("not started", "in progress", "match", "mismatch")))
})
