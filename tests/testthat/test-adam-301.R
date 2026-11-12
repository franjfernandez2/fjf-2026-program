root <- test_path("..", "..")
protocol <- read_protocol(file.path(root, "docs", "protocols", "fjf-2026-301.yml"))
adam_spec <- yaml::read_yaml(file.path(root, "docs", "specs", "adam-301.yml"))
sdtm <- fixture_sdtm_301(root)
adsl <- derive_adsl_301(sdtm, protocol, adam_spec)

test_that("ADSL has the variables of the specification, one record per randomised subject", {
  expect_identical(names(adsl), purrr::map_chr(adam_variables(adam_spec, "ADSL"), "name"))
  expect_identical(adsl$USUBJID, c("FJF-2026-301-301-001-0001", "FJF-2026-301-301-001-0003"))
})

test_that("ADSL sets the analysis sets and treatment from the assignment", {
  expect_identical(adsl$FASFL, c("Y", "Y"))
  expect_identical(adsl$SAFFL, c("Y", "N"))
  expect_identical(adsl$TRT01PN, c(20L, 0L))
  expect_identical(adsl$TRT01AN, c(20L, NA))
  expect_identical(adsl$STRAT1R, c("A", "A"))
})

test_that("ADSL derives treatment dates, duration and compliance", {
  expect_identical(adsl$RANDDT, as.Date(c("2028-01-19", "2028-01-19")))
  expect_identical(adsl$TRTEDT[1], as.Date("2028-07-18"))
  expect_identical(adsl$TRTDURD[1], 182L)
  # Only the returned bottle counts: 35 - 7 = 28 tablets over 28 days.
  expect_identical(adsl$COMPLPCT[1], 100)
})

test_that("ADSL keeps the dates of the intercurrent events", {
  expect_identical(adsl$RESCFL, c("Y", "N"))
  expect_identical(adsl$RESCDT[1], as.Date("2028-05-02"))
  expect_identical(adsl$EOTSTT, c("COMPLETED", NA))
  expect_identical(adsl$EOSSTT, c("COMPLETED", "DISCONTINUED"))
  expect_identical(adsl$DCSREAS[2], "WITHDRAWAL BY SUBJECT")
  expect_true(all(is.na(adsl$DCTDT)))
})

test_that("ADSL baselines are the last values on or before randomisation", {
  expect_equal(adsl$HBA1CBL[1], 0.0915 * 64 + 2.15)
  expect_identical(adsl$WEIGHTBL[1], round(180 / 2.20462, 1))
  expect_identical(adsl$HEIGHTBL[1], 170)
  expect_identical(adsl$BMIBL[1], round(adsl$WEIGHTBL[1] / 1.7^2, 1))
  expect_identical(adsl$BMIGR1[1], "<30")
  expect_identical(adsl$AGEGR1, c(">=65", ">=65"))
})

test_that("adam_study_day counts from randomisation with no day 0", {
  r <- as.Date("2028-01-19")
  expect_identical(adam_study_day(as.Date(c("2028-01-18", "2028-01-19", "2028-01-20")), r), c(-1L, 1L, 2L))
})

test_that("the ADaM XPT keeps dates as SAS dates", {
  dir <- withr::local_tempdir()
  x <- haven::read_xpt(write_adam_dataset(adsl, adam_spec, "ADSL", dir)[1])
  expect_identical(attr(x$RANDDT, "format.sas"), "DATE9")
  expect_identical(as.Date(as.vector(x$RANDDT)), adsl$RANDDT)
})
