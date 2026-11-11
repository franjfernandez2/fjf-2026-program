# Tables of the benchmark, and the formats and reader they rely on (R/tlf.R).

test_that("counts and percentages are formatted as the pilot's report prints them", {
  expect_equal(
    fmt_n_pct(c(14, 8, 1, 86, 0), c(86, 84, 254, 86, 84)),
    c("14 ( 16%)", "8 ( 10%)", "1 ( <1%)", "86 (100%)", "0")
  )
  expect_equal(fmt_n_pct(c(2, 65), 86, digits = 1), c("2 ( 2.3%)", "65 (75.6%)"))
  expect_equal(fmt_num(c(75.25, NA), 1), c("75.3", ""))
  expect_equal(
    fmt_fisher(c(0.58, 0.0003, 0.995, 0.1, 0.5), c(TRUE, TRUE, TRUE, TRUE, FALSE)),
    c("0.58", "0.000*", ">0.99", "0.1*", "")
  )
})

test_that("printed tables are read into labels and values, with wrapped labels joined", {
  page <- paste(
    "Header", "   Preferred Term   n(%)", "",
    "CARDIAC DISORDERS             12 (14.0%) [26]       0                0.831       0.534",
    "  SUPRAVENTRICULAR            1 ( 1.2%) [2]         1 ( 1.2%) [2]    >0.99",
    "EXTRASYSTOLES",
    "Note: footnote 1 2 3",
    sep = "\n"
  )
  rows <- read_printed_table(page, "Preferred Term")
  expect_equal(nrow(rows), 2)
  expect_equal(rows$labels[[2]], "SUPRAVENTRICULAR EXTRASYSTOLES")
  expect_equal(rows$values[[1]], c("12 (14.0%) [26]", "0", "0.831", "0.534"))
  expect_equal(rows$indent, c(0, 2))
})

test_that("the tables match the study report except D-011 and D-012", {
  manifest <- read_pilot_manifest(here::here("data", "external", "cdiscpilot01-manifest.csv"))
  local <- here::here(pilot_dir(), manifest$file)
  skip_if_not(all(file.exists(local)), "pilot files not downloaded yet")
  spec <- read_define_v1(pilot_file(local, "adam/define.xml"))
  sdtm <- read_sdtm(local, c("ae", "dm", "ds", "ex", "mh", "qs", "sc", "sv", "vs"))
  adsl <- derive_adsl(sdtm, spec)
  csr <- pilot_file(local, "csr/cdiscpilot01.pdf")

  # D-011: only the two BMI group rows differ, and counting the subject with no BMI as <25
  # reproduces the printed table exactly.
  t2 <- compare_t_14_2_01(make_t_14_2_01(adsl), csr)
  expect_equal(t2$label, c("<25", "25-<30"))
  as_printed <- adsl
  as_printed$BMIBLGR1[is.na(as_printed$BMIBLGR1)] <- "<25"
  expect_equal(nrow(compare_t_14_2_01(make_t_14_2_01(as_printed), csr)), 0)

  # D-012: only four p-values differ, and rounding them twice gives the printed values.
  t5 <- make_t_14_5_01(adsl, derive_adae(sdtm, adsl, spec))
  diffs <- compare_t_14_5_01(t5, csr)
  expect_equal(diffs$label, c("VOMITING", "SALIVARY HYPERSECRETION", "APPLICATION SITE ERYTHEMA", "SYNCOPE"))
  exact <- c(
    stats::fisher.test(matrix(c(3, 83, 7, 77), 2))$p.value,
    stats::fisher.test(matrix(c(0, 86, 4, 80), 2))$p.value,
    stats::fisher.test(matrix(c(3, 83, 15, 69), 2))$p.value
  )
  expect_equal(round_sas(exact, 3), c(0.208, 0.057, 0.002))
  expect_equal(round_sas(round_sas(exact, 4), 3), c(0.209, 0.058, 0.003))
})

test_that("Table 14-3.01 and Figure 14-1 match the study report", {
  manifest <- read_pilot_manifest(here::here("data", "external", "cdiscpilot01-manifest.csv"))
  local <- here::here(pilot_dir(), manifest$file)
  skip_if_not(all(file.exists(local)), "pilot files not downloaded yet")
  spec <- read_define_v1(pilot_file(local, "adam/define.xml"))
  sdtm <- read_sdtm(local, c("ae", "dm", "ds", "ex", "mh", "qs", "sc", "sv", "vs"))
  adsl <- derive_adsl(sdtm, spec)
  adqsadas <- derive_adqsadas(sdtm, adsl, spec)

  fit <- fit_t_14_3_01(adqsadas)
  expect_equal(nrow(compare_t_14_3_01(make_t_14_3_01(fit), pilot_file(local, "csr/cdiscpilot01.pdf"))), 0)
  # D-013: the code in the metadata, without baseline, would not give the printed 0.245.
  week24 <- fit$data[fit$data$AVISIT == "Week 24", ]
  without_base <- stats::drop1(stats::lm(CHG ~ TRTPN + SITEGR1, data = week24), test = "F")["TRTPN", "Pr(>F)"]
  expect_equal(round_sas(c(fit$dose_p, without_base), 3), c(0.245, 0.253))

  km <- fit_f_14_1(derive_adtte(adsl, derive_adae(sdtm, adsl, spec), spec))
  expect_equal(unname(km$summary$events), c(29, 62, 61))
  expect_equal(unname(km$summary$median[2:3]), c(33, 36))
  expect_lt(km$logrank_p, 0.0001)
})
