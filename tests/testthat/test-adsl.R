# ADSL for the CDISC pilot, derived from SDTM and compared with the official ADSL.
# Needs the downloaded pilot files; skipped until they are there.

local_pilot <- function() {
  manifest <- read_pilot_manifest(here::here("data", "external", "cdiscpilot01-manifest.csv"))
  local <- here::here(pilot_dir(), manifest$file)
  skip_if_not(all(file.exists(local)), "pilot files not downloaded yet")
  local
}

test_that("ADSL matches its specification and the official ADSL, except the logged differences", {
  local <- local_pilot()
  spec <- read_define_v1(pilot_file(local, "adam/define.xml"))
  sdtm <- read_sdtm(local, c("dm", "ds", "ex", "mh", "qs", "sc", "sv", "vs"))
  adsl <- derive_adsl(sdtm, spec)
  official <- haven::read_xpt(pilot_file(local, "adam/adsl.xpt"))

  # One record per randomised subject: DM's 306 subjects minus the 52 screen failures.
  expect_equal(nrow(adsl), 254)
  expect_false(anyDuplicated(adsl$USUBJID) > 0)

  # Against the specification: only D-003, the BMIBLGR1 code that BMICAT does not list.
  findings <- check_against_spec(adsl, spec, "ADSL")
  expect_equal(findings$variable, "BMIBLGR1")
  expect_equal(findings$found, ">=30")

  # Against the official ADSL: 48 variables for 254 subjects, and one difference, D-004.
  diffs <- compare_with_reference(adsl, official, keys = "USUBJID")
  expect_equal(diffs$variable, "BMIBLGR1")
  expect_equal(diffs$n, 1)
  expect_equal(diffs$examples, "01-702-1082: <25 -> NA")

  # The logged differences are in the log.
  log <- utils::read.csv(here::here("studies", "benchmark", "discrepancy-log.csv"), colClasses = "character")
  expect_true(all(c("D-002", "D-003", "D-004", "D-005", "D-006") %in% log$id))
})

test_that("the high-dose cumulative dose follows the titration schedule", {
  local <- local_pilot()
  spec <- read_define_v1(pilot_file(local, "adam/define.xml"))
  adsl <- derive_adsl(read_sdtm(local, c("dm", "ds", "ex", "mh", "qs", "sc", "sv", "vs")), spec)
  low <- adsl[adsl$TRT01PN == 54, ]
  expect_equal(as.vector(low$CUMDOSE), as.vector(54 * low$TRTDUR))
  high <- adsl[adsl$TRT01PN == 81, ]
  # Titrated: between 54 and 81 mg a day on average.
  expect_true(all(high$AVGDD >= 54 & high$AVGDD <= 81))
})
