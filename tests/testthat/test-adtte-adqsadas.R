# ADTTE and ADQSADAS for the CDISC pilot, compared with the official datasets.

pilot_inputs <- function() {
  manifest <- read_pilot_manifest(here::here("data", "external", "cdiscpilot01-manifest.csv"))
  local <- here::here(pilot_dir(), manifest$file)
  skip_if_not(all(file.exists(local)), "pilot files not downloaded yet")
  spec <- read_define_v1(pilot_file(local, "adam/define.xml"))
  sdtm <- read_sdtm(local, c("ae", "dm", "ds", "ex", "mh", "qs", "sc", "sv", "vs"))
  adsl <- derive_adsl(sdtm, spec)
  list(local = local, spec = spec, sdtm = sdtm, adsl = adsl)
}

test_that("ADAS-Cog(11) totals follow SAP 14.2", {
  items <- function(values) {
    tibble::tibble(
      USUBJID = "1", VISITNUM = 3, QSDTC = "2026-01-01",
      QSTESTCD = sprintf("ACITM%02d", c(1, 2, 4, 5, 6, 7, 8, 11, 12, 13, 14)), QSSTRESN = values
    )
  }
  expect_equal(adas11_total(items(rep(1, 11)))$AVAL, 11)
  # Word recall (maximum 10) missing: the other 10 items are scaled by 70 / 60.
  expect_equal(adas11_total(items(c(NA, rep(1, 10))))$AVAL, 10 * 70 / 60)
  # Four missing items (more than 30%): no total.
  expect_true(is.na(adas11_total(items(c(NA, NA, NA, NA, rep(1, 7))))$AVAL))
  expect_equal(sum(adas11_items()$max_score), 70)
})

test_that("ADTTE matches the official ADTTE with no difference", {
  p <- pilot_inputs()
  adtte <- derive_adtte(p$adsl, derive_adae(p$sdtm, p$adsl, p$spec), p$spec)
  official <- haven::read_xpt(pilot_file(p$local, "adam/adtte.xpt"))
  expect_equal(nrow(check_against_spec(adtte, p$spec, "ADTTE")), 0)
  expect_equal(nrow(compare_with_reference(adtte, official, keys = c("USUBJID", "PARAMCD"))), 0)
})

test_that("ADQSADAS matches the official analysed values; other differences are D-009 and D-010", {
  p <- pilot_inputs()
  ours <- derive_adqsadas(p$sdtm, p$adsl, p$spec)
  official <- haven::read_xpt(pilot_file(p$local, "adam/adqsadas.xpt"))
  expect_equal(nrow(ours), nrow(official))
  expect_equal(nrow(check_against_spec(ours, p$spec, "ADQSADAS")), 0)

  diffs <- compare_with_reference(ours, official, keys = c("USUBJID", "PARAMCD", "AVISIT", "ADT"))
  expect_setequal(
    paste(diffs$variable, diffs$n),
    c("NA 33", "NA 33", "AVAL 19", "CHG 19", "PCHG 19", "DTYPE 19", "QSSEQ 753")
  )

  analysed <- function(d) {
    d <- d[d$PARAMCD == "ACTOT" & d$ANL01FL %in% "Y", c("USUBJID", "AVISIT", "AVAL")]
    d[order(d$USUBJID, d$AVISIT), ]
  }
  expect_equal(as.vector(analysed(ours)$AVAL), as.vector(analysed(official)$AVAL))
  # One analysed total per subject and window.
  expect_equal(nrow(analysed(ours)), 254 * 4)
})
