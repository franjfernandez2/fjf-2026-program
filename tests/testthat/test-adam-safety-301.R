root <- test_path("..", "..")
protocol <- read_protocol(file.path(root, "docs", "protocols", "fjf-2026-301.yml"))
adam_spec <- yaml::read_yaml(file.path(root, "docs", "specs", "adam-301.yml"))
sdtm <- fixture_sdtm_301(root)
adsl0 <- derive_adsl_301(sdtm, protocol, adam_spec)
adsl <- add_pps_301(adsl0, derive_adeff_301(sdtm, adsl0, protocol, adam_spec), protocol)$adsl
adex <- derive_adex_301(adsl, adam_spec)
adae <- derive_adae_301(sdtm, adsl, adam_spec)
adhypo <- derive_adhypo_301(sdtm, adsl, adam_spec)
adlb <- derive_adlb_301(sdtm, adsl, protocol, adam_spec)
advs <- derive_advs_301(sdtm, adsl, protocol, adam_spec)
adtte <- derive_adtte_301(adsl, adhypo, protocol, adam_spec)

test_that("the safety datasets have the variables of the specification", {
  expect_identical(names(adex), purrr::map_chr(adam_variables(adam_spec, "ADEX"), "name"))
  expect_identical(names(adae), purrr::map_chr(adam_variables(adam_spec, "ADAE"), "name"))
  expect_identical(names(adhypo), purrr::map_chr(adam_variables(adam_spec, "ADHYPO"), "name"))
  expect_identical(names(adlb), purrr::map_chr(adam_variables(adam_spec, "ADLB"), "name"))
  expect_identical(names(advs), purrr::map_chr(adam_variables(adam_spec, "ADVS"), "name"))
})

test_that("ADaM has every variable the frozen outputs specification names", {
  outputs <- yaml::read_yaml(file.path(root, "docs", "sap", "fjf-2026-301-outputs.yml"))
  used <- unique(unlist(purrr::map(outputs$outputs, "adam")))
  spec_vars <- unlist(purrr::map(adam_spec$datasets, \(d) {
    paste(d$name, purrr::map_chr(d$variables, "name"), sep = ".")
  }))
  expect_identical(setdiff(used, spec_vars), character(0))
})

test_that("ADEX has the dosed patients with years of exposure", {
  expect_identical(adex$USUBJID, adsl$USUBJID[adsl$SAFFL == "Y"])
  expect_equal(adex$TRTDURY, 182 / 365.25)
})

test_that("a partial AE start date is imputed and flagged", {
  i <- impute_ae_start_301(
    c("2028-02", "2028-01", "2028", "2027", "2028-01"), rep(as.Date("2028-01-19"), 5),
    as.Date(c(NA, NA, NA, NA, "2028-01-10"))
  )
  expect_identical(i$date, as.Date(c("2028-02-01", "2028-01-19", "2028-01-19", "2027-01-01", "2028-01-01")))
  expect_identical(i$flag, c("D", "D", "M", "M", "D"))
  expect_identical(adae$ASTDTF, c(NA, "D", NA))
})

test_that("ADAE flags events from the first dose to 14 days after the last", {
  expect_identical(adae$TRTEMFL, c("Y", "Y", "Y"))
  a <- adsl
  a$TRTEDT <- as.Date("2028-02-20")
  expect_identical(derive_adae_301(sdtm, a, adam_spec)$TRTEMFL, c("Y", "Y", "N"))
})

test_that("ADHYPO keeps the level, glucose and the analysis flags", {
  expect_identical(adhypo$HYPOLVL, c(1L, 2L))
  expect_identical(adhypo$GLUCOSE, c(62, 50))
  expect_identical(adhypo$ONTRTFL, c("Y", "Y"))
  expect_identical(adhypo$PRERESFL, c("Y", "Y"))
})

test_that("ADTTE times the first level 2 or 3 episode from the first dose", {
  h <- adtte[adtte$PARAMCD == "TTHYPO23", ]
  expect_identical(h$USUBJID, adsl$USUBJID[adsl$SAFFL == "Y"])
  expect_identical(h$AVAL, as.numeric(as.Date("2028-03-03") - as.Date("2028-01-19")) + 1)
  expect_identical(h$CNSR, 0L)
  none <- derive_adtte_301(adsl, adhypo[adhypo$HYPOLVL == 1, ], protocol, adam_spec)
  expect_identical(none$EVNTDESC[none$PARAMCD == "TTHYPO23"], "LAST CONTACT")
})

test_that("ADLB baseline is the last value on or before the first dose", {
  h <- adlb[adlb$PARAMCD == "HBA1C", ]
  expect_identical(h$ABLFL, c(NA, "Y", NA))
  expect_identical(h$AVISIT, c(NA, "Baseline", "Week 12"))
  expect_equal(h$CHG[3], h$AVAL[3] - h$AVAL[2])
  expect_identical(h$BNRIND, rep("HIGH", 3))
  expect_identical(h$WORSTFL, c(NA, NA, "Y"))
})

test_that("ADLB worst value is the one furthest outside its range", {
  d <- adlb[adlb$PARAMCD == "HBA1C", ]
  expect_identical(sum(!is.na(adlb$WORSTFL[adlb$ONTRTFL == "N"])), 0L)
  expect_false(anyDuplicated(adlb[!is.na(adlb$WORSTFL), c("USUBJID", "PARAMCD")]) > 0)
  expect_identical(nrow(d), 3L)
})

test_that("ADVS leaves height out and derives the baseline", {
  expect_false("HEIGHT" %in% advs$PARAMCD)
  s <- advs[advs$PARAMCD == "SYSBP", ]
  expect_identical(s$BASE, c(135, 135))
  expect_identical(s$ABLFL, c(NA, "Y"))
})
