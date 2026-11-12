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

adeff_base <- derive_adeff_301(sdtm, adsl, protocol, adam_spec)
pps <- add_pps_301(adsl, adeff_base, protocol)
adeff <- pps$adeff
adtte <- derive_adtte_301(pps$adsl, protocol, adam_spec)
hba1c <- adeff[adeff$PARAMCD == "HBA1C", ]

test_that("ADEFF and ADTTE have the variables of the specification", {
  expect_identical(names(adeff), purrr::map_chr(adam_variables(adam_spec, "ADEFF"), "name"))
  expect_identical(names(adtte), purrr::map_chr(adam_variables(adam_spec, "ADTTE"), "name"))
})

test_that("ADaM windows agree with the SAP windows of the dry run", {
  w <- adam_windows_301(hba1c_visit_weeks(protocol))
  sap <- analysis_windows_301(protocol)
  expect_identical(w$AWLO, as.integer(sap$from_day))
  expect_identical(w$AWHI, as.integer(sap$to_day))
  expect_identical(w$AWTARGET, as.integer(sap$target_day))
})

test_that("ADEFF has one selected record per subject, parameter and scheduled visit", {
  sel <- adeff[!is.na(adeff$ANL01FL) & adeff$AVISITN > 0, ]
  expect_false(anyDuplicated(sel[c("USUBJID", "PARAMCD", "AVISITN")]) > 0)
  expect_identical(nrow(sel[sel$PARAMCD == "HBA1C", ]), 2L * length(hba1c_visit_weeks(protocol)))
  expect_true(all(is.na(sel$AVAL[sel$DTYPE %in% "PHANTOM"])))
})

test_that("ADEFF windows the week-12 HbA1c and derives the change from baseline", {
  w12 <- hba1c[hba1c$USUBJID == adsl$USUBJID[1] & hba1c$AVISITN == 12, ]
  expect_identical(w12$ADY, 85L)
  expect_equal(w12$CHG, w12$AVAL - w12$BASE)
  expect_identical(w12$ANL02FL, "Y")
  expect_identical(hba1c$ABLFL[hba1c$USUBJID == adsl$USUBJID[1] & hba1c$AVISITN == 0], "Y")
})

test_that("ADEFF imputes a missing HbA1c baseline with the stratum mean", {
  s3 <- hba1c[hba1c$USUBJID == adsl$USUBJID[2], ]
  expect_true(all(s3$BIMPFL == "Y"))
  expect_equal(unique(s3$BASE), adsl$HBA1CBL[1])
})

test_that("ADEFF flags records after rescue and after treatment", {
  s1 <- hba1c[hba1c$USUBJID == adsl$USUBJID[1] & hba1c$AVISITN > 0, ]
  # Rescue on 2028-05-02, study day 105: weeks 16 to 26 are after it.
  expect_identical(s1$PRERESFL, c("Y", "Y", "Y", "N", "N", "N"))
  s3 <- hba1c[hba1c$USUBJID == adsl$USUBJID[2], ]
  expect_true(all(s3$ONTRTFL == "N"))
})

test_that("the per-protocol set needs the week-26 HbA1c on treatment and before rescue", {
  expect_identical(pps$adsl$PPROTFL, c("N", "N"))
  a <- adsl
  a$RESCDT <- as.Date(NA)
  e <- adeff_base
  i <- e$PARAMCD == "HBA1C" & e$USUBJID == a$USUBJID[1] & e$AVISITN == 26
  e$AVAL[i] <- 7
  e$DTYPE[i] <- NA
  e$ANL02FL[i] <- "Y"
  expect_identical(add_pps_301(a, e, protocol)$adsl$PPROTFL, c("Y", "N"))
})

test_that("ADTTE counts days to rescue and censors at the end of study or week 26", {
  expect_identical(adtte$CNSR, c(0L, 1L))
  expect_identical(adtte$AVAL[1], as.numeric(as.Date("2028-05-02") - as.Date("2028-01-19")) + 1)
  expect_identical(adtte$EVNTDESC, c("RESCUE MEDICATION", "END OF STUDY"))
  a <- pps$adsl
  a$RESCDT <- as.Date(NA)
  a$EOSDT <- as.Date(NA)
  expect_identical(derive_adtte_301(a, protocol, adam_spec)$AVAL, c(183, 183))
})

test_that("the analysis input of ADEFF has the columns the efficacy models read", {
  d <- adeff_analysis_301(adeff, "HBA1C")
  expect_identical(nrow(d), 12L)
  needed <- c("USUBJID", "ARMCD", "STRATUM", "BASE", "AVISITN", "AVISIT", "CHG", "ONTRTFL", "PRERESFL")
  expect_true(all(needed %in% names(d)))
})
