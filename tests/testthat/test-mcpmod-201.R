root <- test_path("..", "..")
assumptions <- read_assumptions(file.path(root, "docs", "design", "assumptions.yml"))
protocol <- read_protocol(file.path(root, "docs", "protocols", "fjf-2026-201.yml"))
estimands <- read_estimands(file.path(root, "docs", "protocols", "fjf-2026-201-estimands.yml"))
sap <- read_sap(file.path(root, "docs", "sap", "fjf-2026-201-sap.yml"))
doses <- c(0, 5, 10, 20, 40)

test_that("the 201 design files are consistent", {
  expect_identical(check_protocol(protocol, assumptions), character())
  expect_identical(check_estimands(estimands, protocol), character())
  expect_identical(check_sap_201(sap, estimands, protocol), character())
  expect_identical(purrr::map_dbl(protocol$arms, "dose_mg"), doses)
})

test_that("check_sap_201 finds broken plans", {
  bad <- sap
  bad$primary$estimand <- "E9"
  bad$primary$mcp$alpha_one_sided <- 0.05
  bad$primary$mcp$candidate_models[[2]]$ed50 <- 80
  bad$primary$mcp$candidate_models[[3]]$model <- "logistic"
  bad$primary$mod$target_dose$p <- 1.2
  bad$sensitivity[[1]]$settings$seed <- NULL
  problems <- check_sap_201(bad, estimands, protocol)
  for (msg in c("primary estimand", "one-sided alpha", "ED50 outside", "unknown candidate model", "share p", "S1")) {
    expect_true(any(grepl(msg, problems)), info = msg)
  }
})

test_that("the planning curve reproduces the assumptions it comes from", {
  p <- assumptions$parameters
  pv <- function(id) p$planning[p$id == id]
  curve <- planning_curve_201(assumptions, 5, 20, week12 = FALSE)
  expect_equal(emax_effect(curve, 20), pv("effect_target_dose"))
  expect_equal(emax_effect(curve, 5) / emax_effect(curve, 20), pv("lowest_dose_effect_ratio"))
  wk12 <- planning_curve_201(assumptions, 5, 20)
  expect_equal(emax_effect(wk12, 20), pv("effect_target_dose") * pv("effect_ratio_week12"))
  expect_equal(wk12$ed50, curve$ed50)
})

test_that("event rates rescale with a constant hazard", {
  expect_equal(rescale_probability(0.2, 26), 0.2)
  half <- rescale_probability(0.2, 13)
  expect_equal(1 - (1 - half)^2, 0.2)
})

test_that("every candidate model of the SAP is built, linear included", {
  mods <- candidate_models_201(sap, doses, -1)
  expect_identical(names(mods), purrr::map_chr(sap$primary$mcp$candidate_models, "model"))
})

test_that("the target dose follows the SAP rule", {
  # Emax-like: 80% of the maximum (0.5) is 0.4; 20 mg is the smallest dose reaching it.
  expect_identical(target_dose_201(c(0, -0.25, -0.35, -0.42, -0.50), doses, 0.8, 0.3), 20)
  # Umbrella: the maximum is at 20 mg; 10 mg reaches 80% of it.
  expect_identical(target_dose_201(c(0, -0.2, -0.45, -0.5, -0.3), doses, 0.8, 0.3), 10)
  # 80% is reached at 5 mg but is not clinically relevant there: the next dose that is.
  expect_identical(target_dose_201(c(0, -0.29, -0.31, -0.33, -0.34), doses, 0.8, 0.3), 10)
  # Nothing reaches the clinically relevant effect: no dose.
  expect_true(is.na(target_dose_201(c(0, -0.1, -0.15, -0.2, -0.25), doses, 0.8, 0.3)))
})

test_that("power grows with the sample size and the rule takes the worst scenario", {
  pw <- power_mcp_201(assumptions, protocol, sap, 20)
  expect_setequal(unique(pw$scenario), c("linear", "emax", "sigEmax", "quadratic", "planning Emax curve"))
  by_n <- tapply(pw$power, pw$n_per_arm, min)
  expect_false(is.unsorted(by_n))
  n <- choose_sample_size_201(pw, 0.9)
  expect_gte(min(pw$power[pw$n_per_arm == n]), 0.9)
  expect_lt(min(pw$power[pw$n_per_arm == n - sap$sample_size$grid$by]), 0.9)
})

test_that("on a large dry run MCP-Mod recovers the planning curve and selects 20 mg", {
  dry <- simulate_dry_run_201(1000, assumptions, protocol, 20, seed = 11)
  expect_setequal(unique(dry$adeff$DOSE), doses)
  res <- analyse_201(dry$adeff, sap, protocol, averaging = FALSE)
  expect_true(res$a1$signal)
  effects <- res$means$means - res$means$means[1]
  expect_lt(max(abs(effects - emax_effect(dry$curve, doses))), 0.1)
  expect_identical(res$a1$target_dose, 20)
})

test_that("the 201 operating characteristics depend on the seed only", {
  run <- function(cores) {
    set.seed(sample.int(1e6, 1))
    operating_characteristics_201(30, assumptions, protocol, sap, 20, trials = 2, seed = 3, cores = cores)
  }
  expect_identical(run(1), run(2))
})
