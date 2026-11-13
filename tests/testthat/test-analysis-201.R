root <- test_path("..", "..")
protocol_301 <- read_protocol(file.path(root, "docs", "protocols", "fjf-2026-301.yml"))
protocol_201 <- read_protocol(file.path(root, "docs", "protocols", "fjf-2026-201.yml"))
adam_spec <- yaml::read_yaml(file.path(root, "docs", "specs", "adam-301.yml"))
sdtm <- fixture_sdtm_301(root)
adsl <- derive_adsl_301(sdtm, protocol_301, adam_spec)
adeff <- derive_adeff_301(sdtm, adsl, protocol_301, adam_spec)

test_that("the analysis data have the shape of the dry run's, with each arm's dose", {
  d <- analysis_data_201(adeff, protocol_201)
  dry <- c("USUBJID", "ARMCD", "DOSE", "STRATUM", "PARAMCD", "BASE", "AVISITN", "AVISIT", "CHG", "ONTRTFL", "PRERESFL")
  expect_true(all(dry %in% names(d)))
  expect_identical(unique(d$PARAMCD), "HBA1C")
  doses <- c(PBO = 0, FJF5 = 5, FJF10 = 10, FJF20 = 20, FJF40 = 40)
  expect_identical(d$DOSE, unname(doses[d$ARMCD]))
  expect_true(all(d$ONTRTFL %in% c("Y", "N")) && all(d$PRERESFL %in% c("Y", "N")))
})

test_that("an arm the protocol does not have stops the analysis", {
  bad <- adeff
  bad$ARMCD[1] <- "XYZ"
  bad <- bad[bad$USUBJID == bad$USUBJID[1], ]
  bad$ARMCD <- "XYZ"
  expect_error(analysis_data_201(bad, protocol_201), "not in the protocol")
})

test_that("the time-to-event censoring label follows the protocol's primary visit", {
  spec_201 <- yaml::read_yaml(file.path(root, "docs", "specs", "adam-201.yml"))
  adhypo <- derive_adhypo_301(sdtm, adsl, spec_201)
  adtte <- derive_adtte_301(adsl, adhypo, protocol_201, spec_201)
  expect_false(any(adtte$EVNTDESC %in% "WEEK 26"))
  expect_true(all(adtte$EVNTDESC[adtte$PARAMCD == "TTRESC"] %in% c("RESCUE MEDICATION", "END OF STUDY", "WEEK 12")))
})

test_that("the two studies' ADaM specifications differ only where the studies differ", {
  spec_201 <- yaml::read_yaml(file.path(root, "docs", "specs", "adam-201.yml"))
  names_of <- function(s) purrr::map(s$datasets, function(d) purrr::map_chr(d$variables, "name"))
  expect_identical(names_of(spec_201), names_of(adam_spec))
  expect_identical(spec_201$vital_sign_limits, adam_spec$vital_sign_limits)
  expect_identical(spec_201$study, "FJF-2026-201")
})
