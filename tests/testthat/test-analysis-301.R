root <- test_path("..", "..")
protocol <- read_protocol(file.path(root, "docs", "protocols", "fjf-2026-301.yml"))
adam_spec <- yaml::read_yaml(file.path(root, "docs", "specs", "adam-301.yml"))
sdtm <- fixture_sdtm_301(root)
adsl0 <- derive_adsl_301(sdtm, protocol, adam_spec)
pps <- add_pps_301(adsl0, derive_adeff_301(sdtm, adsl0, protocol, adam_spec), protocol)
adhypo <- derive_adhypo_301(sdtm, pps$adsl, adam_spec)
adam <- list(ADSL = pps$adsl, ADEFF = pps$adeff, ADTTE = derive_adtte_301(pps$adsl, adhypo, protocol, adam_spec))
a <- analysis_data_301(adam)

test_that("the analysis data have the dry run's shape", {
  expect_named(a, c("adsl", "adeff", "adtte"))
  expect_identical(names(a$adsl), c("USUBJID", "ARMCD", "STRATUM", "AGE", "SEX", "BMIBL", "WEIGHTBL"))
  needed <- c("USUBJID", "ARMCD", "STRATUM", "PARAMCD", "BASE", "AVISITN", "AVISIT", "CHG", "ONTRTFL", "PRERESFL")
  expect_true(all(needed %in% names(a$adeff)))
  expect_identical(a$adtte$PARAMCD, c("TTRESC", "TTRESC"))
})

test_that("the analysis data have one row per patient and scheduled visit", {
  h <- a$adeff[a$adeff$PARAMCD == "HBA1C", ]
  expect_identical(nrow(h), 2L * length(hba1c_visit_weeks(protocol)))
  w <- a$adeff[a$adeff$PARAMCD == "WEIGHT", ]
  expect_identical(nrow(w), 2L * length(assessment_visit_weeks(protocol, "weight")))
  expect_true(all(a$adeff$ONTRTFL %in% c("Y", "N")))
})

test_that("the F2 data preparation accepts the study data", {
  d <- hba1c_data_301(a, "PBO")
  expect_identical(levels(d$ARMCD)[1], "PBO")
  expect_identical(nrow(d), 12L)
})
