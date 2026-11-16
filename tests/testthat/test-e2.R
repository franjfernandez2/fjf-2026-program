root <- test_path("..", "..")
plan_e2 <- read_e2_plan(file.path(root, "docs", "sap", "e2-simulation-plan.yml"))
cells_e2 <- e2_cells(plan_e2)

# Small settings: the generator at 40 patients per arm with the planning values of the study.
assumptions <- read_assumptions(file.path(root, "docs", "design", "assumptions.yml"))
protocol <- read_protocol(file.path(root, "docs", "protocols", "fjf-2026-301.yml"))
sap <- read_sap(file.path(root, "docs", "sap", "fjf-2026-301-sap.yml"))
pars <- planning_values_dry_run_301(assumptions)

test_that("the plan has every mechanism at every rate, with unique cells", {
  expect_identical(nrow(cells_e2), 10L)
  expect_false(anyDuplicated(cells_e2$id) > 0)
  expect_setequal(cells_e2$mechanism, c("MCAR", "MAR", "MNAR_MOD", "MNAR_STR", "MNAR_ACT"))
  expect_setequal(cells_e2$rate, c(0.06, 0.15))
  expect_identical(cells_e2$cell, seq_len(10L))
  # The rate of the planning value is the assumption's planning value.
  expect_equal(plan_e2$dropout$rates[[1]]$rate, pars$study_withdrawal)
  expect_setequal(unlist(lapply(plan_e2$analyses, `[[`, "id")), c("MMRM", "MI_J2R", "MI_MAR"))
})

test_that("the leaving probability has the stated mean and rises with the linear predictor", {
  lin <- stats::qnorm(seq(0.01, 0.99, length.out = 500)) * 1.5
  for (rate in c(0.06, 0.15)) {
    p <- e2_leaving_probability(lin, rate)
    expect_equal(mean(p), rate, tolerance = 1e-8)
    expect_true(all(diff(p) >= 0))
  }
  expect_equal(e2_leaving_probability(rep(0, 10), 0.1), rep(0.1, 10), tolerance = 1e-8)
})

test_that("a trial has the full data of the dry run, and only patients who leave lose values", {
  mcar <- simulate_e2_trial(60, pars, protocol, sap, cells_e2[cells_e2$id == "MCAR_R15", ], seed = 5)
  mnar <- simulate_e2_trial(60, pars, protocol, sap, cells_e2[cells_e2$id == "MNAR_STR_R15", ], seed = 5)
  # The full data do not depend on the mechanism.
  expect_identical(mcar$full, mnar$full)
  expect_identical(mcar$adeff$CHG_FULL, mnar$adeff$CHG_FULL)
  expect_true(all(c("USUBJID", "ARMCD", "AVISIT", "CHG", "CHG_FULL", "BASE", "STRATUM") %in% names(mcar$adeff)))
  expect_identical(unique(mcar$adeff$PARAMCD), "HBA1C")
  # The share who leave is the rate (to the sampling of 120 patients), and they have a trailing block of missing values.
  expect_equal(mnar$left, 0.15, tolerance = 0.1)
  trailing <- function(trial) {
    adeff <- trial$adeff[order(trial$adeff$AVISITN), ]
    by_patient <- split(adeff$CHG, adeff$USUBJID)
    mean(vapply(by_patient, function(x) is.na(x[length(x)]), TRUE))
  }
  expect_gt(trailing(mnar), 0.05)
})

test_that("under MNAR the patients who leave are those with the worse change; under MCAR, not", {
  cell <- cells_e2[cells_e2$id == "MNAR_STR_R15", ]
  gap <- function(cell) {
    mean(vapply(1:30, function(i) {
      t <- simulate_e2_trial(100, pars, protocol, sap, cell, seed = 1000 + i)
      last <- t$adeff[t$adeff$AVISITN == protocol$primary_visit_week, ]
      gone <- is.na(last$CHG)
      mean(last$CHG_FULL[gone]) - mean(last$CHG_FULL[!gone])
    }, 0))
  }
  expect_gt(gap(cell), 0.3)
  expect_lt(abs(gap(cells_e2[cells_e2$id == "MCAR_R15", ])), 0.25)
})

test_that("the target is the full-data difference and the same for every cell", {
  truth <- e2_truth(2000, pars, protocol, sap, seed = 11)
  expect_lt(truth$value, 0)
  expect_gt(truth$value, -0.6)
  expect_lt(truth$se, 0.05)
})

test_that("a trial is analysed by the three analyses and the full-data reference, reproducibly", {
  cell <- cells_e2[cells_e2$id == "MNAR_MOD_R15", ]
  one <- function() {
    trial <- simulate_e2_trial(60, pars, protocol, sap, cell, seed = 21)
    analyse_e2_trial(trial, sap, protocol, imputations = 5, seed = 21)
  }
  a <- one()
  expect_identical(a$analysis, c("FULL", "MMRM", "MI_J2R", "MI_MAR"))
  expect_true(all(a$lower < a$estimate & a$estimate < a$upper))
  expect_equal(one(), a)
})

test_that("a cell does not depend on the number of cores", {
  skip_on_os("windows")
  cell <- cells_e2[cells_e2$id == "MAR_R06", ]
  small <- plan_e2
  serial <- run_e2_cell(cell, small, 40, pars, protocol, sap, trials = 2, imputations = 5, cores = 1)
  parallel <- run_e2_cell(cell, small, 40, pars, protocol, sap, trials = 2, imputations = 5, cores = 2)
  expect_equal(serial, parallel)
  expect_identical(unique(serial$cell_id), "MAR_R06")
})

test_that("the summary gives the bias, its Monte Carlo error, the coverage and the power", {
  trials <- data.frame(
    cell = 1L, cell_id = "MCAR_R06", analysis = "MMRM", trial = 1:4, left = 0.06,
    estimate = c(-0.3, -0.5, -0.4, NA), se = 0.1, lower = c(-0.5, -0.7, -0.6, NA),
    upper = c(-0.1, -0.3, -0.2, NA), p_value = c(0.01, 0.001, 0.01, NA)
  )
  s <- summarise_e2(trials, list(value = -0.4, se = 0.01), cells_e2)
  expect_equal(s$bias, 0)
  expect_equal(s$empirical_sd, 0.1)
  expect_equal(s$bias_mcse, 0.1 / sqrt(3))
  expect_identical(s$failed, 1L)
  expect_equal(s$coverage, 1)
  expect_equal(s$power, 1)
  expect_identical(s$mechanism, "MCAR")
  # A target outside one of the three intervals lowers the coverage.
  expect_equal(summarise_e2(trials, list(value = -0.55, se = 0.01), cells_e2)$coverage, 2 / 3)
})
