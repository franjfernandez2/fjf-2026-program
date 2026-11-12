root <- test_path("..", "..")
assumptions <- read_assumptions(file.path(root, "docs", "design", "assumptions.yml"))
p301 <- read_protocol(file.path(root, "docs", "protocols", "fjf-2026-301.yml"))

test_that("the 301 design data are consistent", {
  expect_equal(check_protocol(p301, assumptions), character())
  expect_equal(p301$primary_visit_week, 26)
})

test_that("the rescue threshold follows the protocol's periods", {
  expect_equal(rescue_threshold(p301, c(0, 2, 4, 6, 8, 12, 20, 26, 28)), c(NA, 270, 270, 240, 240, 200, 200, 200, NA))
})

test_that("check_protocol finds broken design data", {
  bad <- p301
  bad$visits$week[3] <- bad$visits$week[2]
  bad$rescue$thresholds[[2]]$fpg_mg_dl <- 300
  bad$visits$assessments[[9]] <- setdiff(bad$visits$assessments[[9]], "hba1c")
  bad$arms[[2]]$dose_mg <- 0
  problems <- check_protocol(bad, assumptions)
  expect_true("visit weeks are not strictly increasing" %in% problems)
  expect_true("rescue thresholds increase over time" %in% problems)
  expect_true("no HbA1c at the primary visit" %in% problems)
  expect_true("there must be exactly one placebo arm" %in% problems)
})
