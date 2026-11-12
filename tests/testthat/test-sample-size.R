root <- test_path("..", "..")
assumptions <- read_assumptions(file.path(root, "docs", "design", "assumptions.yml"))
pars <- planning_values_301(assumptions)

test_that("the planning model reproduces the diluted treatment-policy effect", {
  # Expected difference: effect x (1 - discontinuation on FJF-2026) + rescue benefit x
  # (rescue on FJF-2026 - rescue on placebo), shrunk towards 0 by study withdrawal.
  on_drug <- pars$effect_target_dose * (1 - pars$discontinuation_active)
  rescue <- pars$rescue_benefit * (pars$rescue_active - pars$rescue_placebo)
  expected <- (on_drug + rescue) * (1 - pars$study_withdrawal)
  sims <- withr::with_seed(1, replicate(400, simulate_301_trial(500, pars)["difference"]))
  expect_lt(abs(mean(sims) - expected), 0.02)
})

test_that("power is reproducible with its seed and grows with the sample size", {
  a <- simulate_power_301(c(60, 200), pars, reps = 400, seed = 7)
  b <- simulate_power_301(c(60, 200), pars, reps = 400, seed = 7)
  expect_identical(a, b)
  expect_lt(a$power[1], a$power[2])
})

test_that("with no effect, the type I error is close to alpha", {
  null <- utils::modifyList(pars, list(effect_target_dose = 0, rescue_active = pars$rescue_placebo))
  x <- simulate_power_301(150, null, reps = 4000, seed = 11)
  # One-sided success (difference < 0) at two-sided 5%: expected 2.5%.
  expect_lt(abs(x$power - 0.025), 3 * x$mcse + 0.005)
})

test_that("choose_sample_size uses the lower limit of the simulated power", {
  t <- tibble::tibble(n_per_arm = c(140, 150), power = c(0.900, 0.912), mcse = c(0.004, 0.004))
  expect_equal(choose_sample_size(t), 150)
  expect_true(is.na(choose_sample_size(t, target = 0.99)))
})
