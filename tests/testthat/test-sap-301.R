root <- test_path("..", "..")
protocol <- read_protocol(file.path(root, "docs", "protocols", "fjf-2026-301.yml"))
estimands <- read_estimands(file.path(root, "docs", "protocols", "fjf-2026-301-estimands.yml"))
sap <- read_sap(file.path(root, "docs", "sap", "fjf-2026-301-sap.yml"))

test_that("the 301 SAP is consistent with its estimands and protocol", {
  expect_identical(check_sap(sap, estimands, protocol), character())
  expect_identical(sap_item(sap, "A1")$estimand, "E1")
  expect_error(sap_item(sap, "A9"), "not found")
})

test_that("check_sap finds broken analyses", {
  bad <- sap
  bad$analyses[[1]]$estimand <- "E9"
  bad$analyses[[2]]$role <- "primary"
  bad$analyses[[1]]$settings$reference <- "FJF20"
  bad$sensitivity[[1]]$of <- "A7"
  bad$sensitivity[[2]]$settings$strategy_after_withdrawal <- "LOCF"
  bad$sensitivity[[3]]$settings$deltas$from <- 1
  problems <- check_sap(bad, estimands, protocol)
  expect_true(any(grepl("A1: unknown estimand E9", problems)))
  expect_true(any(grepl("exactly one primary analysis", problems)))
  expect_true(any(grepl("reference arm", problems)))
  expect_true(any(grepl("S1: belongs to unknown analysis", problems)))
  expect_true(any(grepl("S2: unknown strategy", problems)))
  expect_true(any(grepl("S3: the delta grid", problems)))

  no_e2 <- sap
  no_e2$analyses <- no_e2$analyses[1]
  expect_true(any(grepl("supplementary estimand E2 has no analysis", check_sap(no_e2, estimands, protocol))))
})

test_that("analysis windows cover every day from day 2 without gaps or overlaps", {
  w <- analysis_windows_301(protocol)
  expect_identical(w$week, c(4, 8, 12, 16, 20, 26))
  expect_identical(w$target_day, w$week * 7 + 1)
  expect_identical(w$from_day[1], 2)
  expect_identical(w$from_day[-1], w$to_day[-nrow(w)] + 1)
  expect_true(all(w$target_day >= w$from_day & w$target_day <= w$to_day))
  # Week 26 (day 183) ends half the gap to week 20 (day 141) after its target.
  expect_identical(w$to_day[6], 204)
})

test_that("windowing keeps the value closest to the target, the later one on a tie", {
  w <- analysis_windows_301(protocol)
  x <- tibble::tibble(
    USUBJID = "P1",
    ADY = c(1, 27, 31, 40, 183, 205),
    AVAL = c(8.0, 7.9, 7.8, 7.7, 7.2, 7.1)
  )
  out <- window_assessments(x, w)
  # Day 1 is baseline and day 205 is outside every window.
  expect_identical(out$AVISIT, c("Week 4", "Week 26"))
  # Days 27 and 31 are both 2 days from the target, day 29: the later one wins.
  expect_identical(out$ADY[out$AVISIT == "Week 4"], 31)
  expect_identical(out$ANL01FL, c("Y", "Y"))
})
