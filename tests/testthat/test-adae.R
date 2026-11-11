# ADAE for the CDISC pilot, derived from SDTM AE and our ADSL, compared with the official ADAE.

test_that("ADAE matches its specification and the official ADAE, except D-008", {
  manifest <- read_pilot_manifest(here::here("data", "external", "cdiscpilot01-manifest.csv"))
  local <- here::here(pilot_dir(), manifest$file)
  skip_if_not(all(file.exists(local)), "pilot files not downloaded yet")
  spec <- read_define_v1(pilot_file(local, "adam/define.xml"))
  sdtm <- read_sdtm(local, c("ae", "dm", "ds", "ex", "mh", "qs", "sc", "sv", "vs"))
  adae <- derive_adae(sdtm, derive_adsl(sdtm, spec), spec)
  official <- haven::read_xpt(pilot_file(local, "adam/adae.xpt"))

  expect_equal(nrow(adae), 1191)
  expect_equal(nrow(check_against_spec(adae, spec, "ADAE")), 0)

  diffs <- compare_with_reference(adae, official, keys = c("USUBJID", "AESEQ"))
  expect_setequal(diffs$variable, c("ADURN", "ADURU"))
  expect_true(all(diffs$n == 4))

  # Partial start dates: only a missing day is imputed.
  imputed <- adae[!is.na(adae$ASTDTF), ]
  expect_true(all(imputed$ASTDTF == "D" & format(imputed$ASTDT, "%d") == "01"))
  # One first-occurrence flag per subject with a treatment-emergent event.
  expect_equal(sum(adae$AOCCFL == "Y", na.rm = TRUE), length(unique(adae$USUBJID[adae$TRTEMFL == "Y"])))
  # D-007: AOCC01FL flags dermatologic events only.
  expect_true(all(adae$CQ01NAM[adae$AOCC01FL %in% "Y"] == "DERMATOLOGIC EVENTS"))
})
