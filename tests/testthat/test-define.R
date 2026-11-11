# Specifications read from Define-XML 1.0, and checks of data against them (ADR 0016).

fixture <- testthat::test_path("fixtures", "define-v1-mini.xml")

matching_data <- function() {
  data <- data.frame(
    USUBJID = c("01-001", "01-001"), PARAMCD = c("TOT", "ITM1"),
    AVAL = c(10, 3), ADT = as.Date(c("2026-01-05", "2026-01-05"))
  )
  attr(data$USUBJID, "label") <- "Unique Subject Identifier"
  attr(data$PARAMCD, "label") <- "Parameter Code"
  attr(data$AVAL, "label") <- "Analysis Value"
  attr(data$ADT, "label") <- "Analysis Date"
  data
}

test_that("datasets, variables and their order are read", {
  spec <- read_define_v1(fixture)
  expect_equal(spec$datasets$dataset, "ADXX")
  expect_equal(spec$datasets$keys, "USUBJID, PARAMCD")
  expect_equal(spec$variables$variable, c("USUBJID", "PARAMCD", "AVAL", "ADT"))
  expect_equal(spec$variables$order, 1:4)
  expect_equal(spec$variables$mandatory, c(TRUE, FALSE, FALSE, FALSE))
  expect_equal(spec$variables$length[spec$variables$variable == "USUBJID"], 11L)
  expect_equal(spec$variables$format[spec$variables$variable == "ADT"], "DATE9.")
  expect_equal(spec$variables$codelist[spec$variables$variable == "PARAMCD"], "PARAMCD")
  # Blank comments become missing, not " ".
  expect_true(is.na(spec$variables$derivation[spec$variables$variable == "AVAL"]))
})

test_that("value-level metadata take the parameter from the item OID", {
  spec <- read_define_v1(fixture)
  expect_equal(spec$value_level$paramcd, c("TOT", "ITM1"))
  expect_equal(unique(spec$value_level$variable), "AVAL")
  expect_equal(spec$value_level$derivation[1], "Sum of items")
})

test_that("codelists are read, external ones as a single empty row", {
  spec <- read_define_v1(fixture)
  paramcd <- spec$codelists[spec$codelists$code_id == "PARAMCD", ]
  # The fixture pads "ITM1" with a non-breaking space, as the pilot's file does.
  expect_equal(paramcd$code, c("TOT", "ITM1"))
  expect_equal(paramcd$decode, c("Total Score", "Item 1"))
  meddra <- spec$codelists[spec$codelists$code_id == "MEDDRA", ]
  expect_equal(nrow(meddra), 1)
  expect_true(is.na(meddra$code))
})

test_that("analysis results metadata are read", {
  spec <- read_define_v1(fixture)
  d <- spec$displays
  expect_equal(d$display_id, "Table_1")
  expect_equal(d$parameters, "TOT")
  expect_equal(d$datasets, "ADXX")
  expect_equal(d$selection, '[ PARAMCD="TTL" ]')
  expect_equal(d$documentation_ref, "SAP_SEC_1")
  expect_match(d$code, "proc means")
})

test_that("a Define-XML 2.0 file is refused", {
  path <- tempfile(fileext = ".xml")
  writeLines(sub("def/v1.0", "def/v2.0", readLines(fixture)), path)
  expect_error(read_define_v1(path), "not Define-XML 1.0")
})

test_that("a metacore object is built from the specification", {
  mc <- define_v1_to_metacore(read_define_v1(fixture), "ADXX")
  expect_s3_class(mc, "Metacore")
  # metacore adds a label attribute to every column, so compare the values only.
  expect_equal(as.vector(mc$ds_vars$key_seq), c(1L, 2L, NA, NA))
  expect_equal(as.vector(mc$var_spec$variable), c("USUBJID", "PARAMCD", "AVAL", "ADT"))
  expect_true("PARAMCD == 'TOT'" %in% mc$value_spec$where)
})

test_that("data that match the specification give no findings", {
  expect_equal(nrow(check_against_spec(matching_data(), read_define_v1(fixture), "ADXX")), 0)
})

test_that("each kind of mismatch is reported", {
  spec <- read_define_v1(fixture)
  data <- matching_data()
  data$ADT <- NULL # missing variable
  data$EXTRA <- 1 # unexpected variable
  attr(data$AVAL, "label") <- "Value" # label
  data$USUBJID <- structure(1:2, label = "Unique Subject Identifier") # type
  data$PARAMCD[2] <- "NEW" # unlisted PARAMCD and value not in codelist; ITM1 now absent
  data <- data[, c("PARAMCD", "USUBJID", "AVAL", "EXTRA")] # variable order

  checks <- check_against_spec(data, spec, "ADXX")$check
  expect_setequal(checks, c(
    "missing variable", "unexpected variable", "label", "type", "variable order",
    "value not in codelist", "PARAMCD not in value-level metadata", "PARAMCD in value-level metadata but not in data"
  ))
})

test_that("selection criteria naming an undeclared parameter are reported", {
  findings <- check_analysis_results(read_define_v1(fixture))
  expect_equal(findings$display_id, "Table_1")
  expect_equal(findings$declared, "TOT")
  expect_equal(findings$selected, "TTL")
})

test_that("the pilot's specification is read in full and matches its official ADaM", {
  manifest <- read_pilot_manifest(here::here("data", "external", "cdiscpilot01-manifest.csv"))
  local <- here::here(pilot_dir(), manifest$file)
  skip_if_not(all(file.exists(local)), "pilot files not downloaded yet")

  spec <- read_define_v1(pilot_file(local, "adam/define.xml"))
  expect_equal(nrow(spec$datasets), 10)
  expect_equal(sum(spec$variables$dataset == "ADSL"), 48)
  expect_equal(nrow(spec$displays), 18)

  findings <- do.call(rbind, lapply(c("ADSL", "ADAE", "ADTTE", "ADQSADAS"), function(ds) {
    check_against_spec(haven::read_xpt(pilot_file(local, sprintf("adam/%s.xpt", tolower(ds)))), spec, ds)
  }))
  # Discrepancy D-003: the only finding is a BMIBLGR1 value that its codelist does not list.
  expect_equal(findings$variable, "BMIBLGR1")
  expect_equal(findings$found, ">=30")

  # Discrepancy D-001: the primary analysis selects PARAMCD="ATOT", but declares ACTOT.
  arm <- check_analysis_results(spec)
  expect_equal(unique(arm$display_id), "Table_14-3.01")
  expect_equal(unique(arm$selected), "ATOT")
})
