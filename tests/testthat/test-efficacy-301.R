root <- test_path("..", "..")
assumptions <- read_assumptions(file.path(root, "docs", "design", "assumptions.yml"))
protocol <- read_protocol(file.path(root, "docs", "protocols", "fjf-2026-301.yml"))
sap <- read_sap(file.path(root, "docs", "sap", "fjf-2026-301-sap.yml"))
pars <- planning_values_dry_run_301(assumptions)

test_that("dry-run data are reproducible and shaped like the efficacy dataset", {
  a <- simulate_dry_run_301(40, pars, protocol, seed = 3)
  b <- simulate_dry_run_301(40, pars, protocol, seed = 3)
  expect_identical(a, b)
  d <- a$adeff[a$adeff$PARAMCD == "HBA1C", ]
  expect_identical(nrow(d), 80L * 6L)
  expect_identical(sum(a$adeff$PARAMCD == "WEIGHT"), 80L * 2L)
  expect_true(all(d$BASE >= 7 & d$BASE <= 10.5))
  expect_setequal(unique(d$ARMCD), c("PBO", "FJF20"))
  expect_identical(d$STRATUM == ">8.5", d$BASE > 8.5)
  # Nobody has a value after leaving the study.
  left <- a$subjects$USUBJID[is.finite(a$subjects$withdrawal_week)]
  after <- a$adeff |>
    dplyr::inner_join(a$subjects, by = c("USUBJID", "ARMCD")) |>
    dplyr::filter(AVISITN > withdrawal_week)
  expect_true(all(is.na(after$CHG)))
  expect_gt(length(left), 0)
})

test_that("withdrawal starts at the first visit of the trailing block of missing values", {
  d <- data.frame(
    USUBJID = rep(c("A", "B", "C", "D"), each = 3),
    ARMCD = "PBO", STRATUM = "<=8.5", BASE = 8, ONTRTFL = "Y", PRERESFL = "Y",
    AVISITN = rep(c(4, 8, 12), 4),
    AVISIT = rep(c("Week 4", "Week 8", "Week 12"), 4),
    CHG = c(-0.1, NA, -0.2, -0.1, NA, NA, NA, NA, NA, -0.1, -0.2, -0.3)
  )
  d$ARMCD[d$USUBJID %in% c("C", "D")] <- "FJF20"
  ice <- withdrawal_ice_301(prepare_efficacy_301(d), "JR")
  # A: intermittent only; B: left after week 4; C: no value at all; D: complete.
  expect_identical(as.character(ice$USUBJID), c("B", "C"))
  expect_identical(as.character(ice$AVISIT), c("Week 8", "Week 4"))
  expect_identical(levels(prepare_efficacy_301(d)$AVISIT), c("Week 4", "Week 8", "Week 12"))
})

test_that("the hypothetical data drop values after discontinuation or rescue", {
  d <- prepare_efficacy_301(simulate_dry_run_301(40, pars, protocol, seed = 5)$adeff)
  h <- hypothetical_data_301(d)
  excluded <- d$ONTRTFL == "N" | d$PRERESFL == "N"
  expect_true(any(excluded))
  expect_true(all(is.na(h$CHG[excluded])))
  expect_identical(h$CHG[!excluded], d$CHG[!excluded])
})

test_that("tipping_point finds the first delta whose upper limit reaches 0", {
  t <- tibble::tibble(delta = c(0, 1, 2, 3), upper = c(-0.2, -0.1, 0.01, 0.1))
  expect_identical(tipping_point(t), 2)
  expect_true(is.na(tipping_point(t[1:2, ])))
})

test_that("on a large dry run, each analysis recovers the value of its own estimand", {
  dry <- simulate_dry_run_301(1500, pars, protocol, seed = 301)
  wk26 <- dry$adeff[dry$adeff$AVISITN == 26 & dry$adeff$PARAMCD == "HBA1C", ]
  e1_value <- mean(wk26$CHG_FULL[wk26$ARMCD == "FJF20"]) - mean(wk26$CHG_FULL[wk26$ARMCD == "PBO"])
  res <- analyse_efficacy_301(dry$adeff, sap, protocol, imputations = 5, deltas = c(0, 2, 4))
  est <- stats::setNames(res$results$estimate, res$results$analysis)
  # E2 (hypothetical): on treatment and without rescue the effect is the planning effect.
  expect_lt(abs(est[["A2"]] - pars$effect_target_dose), 0.08)
  # E1 under MAR is close to the complete-data treatment-policy value; J2R is more
  # conservative, because it assumes patients who left lost the effect.
  expect_lt(abs(est[["S1"]] - e1_value), 0.08)
  expect_gt(est[["A1"]], est[["S1"]])
  expect_lt(est[["A2"]], est[["A1"]])
  # Larger deltas move the estimate towards 0.
  expect_true(all(diff(res$tipping$estimate) > 0))
})

test_that("the operating characteristics depend on the seed only, not on cores or prior state", {
  run <- function(cores) {
    set.seed(sample.int(1e6, 1)) # a different global state each time
    operating_characteristics_301(60, pars, protocol, sap, trials = 2, imputations = 3, seed = 5, cores = cores)
  }
  expect_identical(run(1), run(2))
})
