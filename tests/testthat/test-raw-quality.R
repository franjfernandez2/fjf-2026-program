test_that("CRF dates are parsed whatever the locale, and partial dates are found", {
  expect_identical(parse_raw_date(c("09-MAR-2028", "31-DEC-2027")), as.Date(c("2028-03-09", "2027-12-31")))
  expect_true(is.na(parse_raw_date("UN-MAR-2028")))
  expect_true(is.na(parse_raw_date("31-FOO-2028")))
  expect_identical(is_partial_date(c("09-MAR-2028", "UN-MAR-2028", "UN-UNK-2028", NA)), c(FALSE, TRUE, TRUE, FALSE))
})

test_that("check_raw_quality finds each kind of problem", {
  spec <- list(tables = list(
    list(name = "visits", natural_key = list("subject_id", "visit_code")),
    list(name = "randomisation", natural_key = list("subject_id")),
    list(name = "vital_signs", natural_key = list("subject_id", "visit_code")),
    list(name = "demography", natural_key = list("subject_id")),
    list(name = "lab_results", natural_key = list("subject_id", "test_code")),
    list(name = "adverse_events", natural_key = list("subject_id", "ae_seq"))
  ))
  protocol <- list(visits = data.frame(code = c("V2", "V3"), week = c(0, 2), window_days = c(0, 3)))
  tables <- list(
    visits = data.frame(
      subject_id = c("A", "A", "B"), visit_code = c("V2", "V3", "V3"),
      visit_date = c("01-JAN-2028", "20-JAN-2028", "15-JAN-2028"), visit_status = "DONE"
    ),
    randomisation = data.frame(subject_id = c("A", "B"), randomisation_date = "01-JAN-2028"),
    vital_signs = data.frame(
      subject_id = c("A", "A", "B"), visit_code = c("V2", "V2", "V2"), measure_date = "01-JAN-2028",
      weight_unit = c("kg", "kg", "lb")
    ),
    demography = data.frame(
      subject_id = c("A", "B"), birth_date = c("UN-JUN-1970", "01-JUN-1970"), height_cm = c(170, 1.7)
    ),
    lab_results = data.frame(
      subject_id = c("A", "A", "B"), test_code = c("HBA1C", "ALT", "HBA1C"), collection_date = "01-JAN-2028",
      result = c("8.1", "<8", "65"), unit = c("%", "U/L", "%")
    ),
    adverse_events = data.frame(
      subject_id = "A", ae_seq = 1, start_date = "10-JAN-2028", end_date = "05-JAN-2028", severity = NA
    )
  )
  q <- check_raw_quality(tables, spec, protocol, "999")
  n <- function(check, table) sum(q$n[q$check == check & q$table == table])
  expect_identical(n("Duplicated natural key", "vital_signs"), 1L)
  expect_identical(n("Partial date", "demography"), 1L)
  expect_identical(n("Visit outside its window", "visits"), 1L)
  expect_identical(n("Unexpected unit", "vital_signs"), 1L)
  expect_identical(n("Unexpected unit", "demography"), 1L)
  expect_identical(n("Qualified or non-numeric result", "lab_results"), 1L)
  expect_identical(n("Ends before it starts", "adverse_events"), 1L)
  expect_identical(n("Missing required value", "adverse_events"), 1L)
  # The checks must be readable before the database lock: they never use the treatment arm.
  expect_false(any(grepl("arm", deparse(check_raw_quality), fixed = TRUE)))
})
