# Double programming of ADSL: the QC program must match production exactly.

test_that("the QC ADSL matches the production ADSL with no difference", {
  manifest <- read_pilot_manifest(here::here("data", "external", "cdiscpilot01-manifest.csv"))
  local <- here::here(pilot_dir(), manifest$file)
  skip_if_not(all(file.exists(local)), "pilot files not downloaded yet")
  spec <- read_define_v1(pilot_file(local, "adam/define.xml"))
  production <- derive_adsl(read_sdtm(local, c("dm", "ds", "ex", "mh", "qs", "sc", "sv", "vs")), spec)
  qc <- qc_adsl(local, spec)
  expect_equal(dim(qc), c(254, 48))
  expect_equal(nrow(compare_with_reference(qc, production, keys = "USUBJID")), 0)
})

test_that("the QC program uses neither admiral nor the production helpers", {
  code <- readLines(here::here("qc", "benchmark", "adsl_qc.R"))
  code <- code[!grepl("^\\s*#", code)]
  expect_false(any(grepl("admiral::|derive_adsl|round_sas|round_to_spec|codelist_lookup|read_sdtm", code)))
})
