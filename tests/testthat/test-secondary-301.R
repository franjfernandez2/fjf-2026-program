root <- test_path("..", "..")
assumptions <- read_assumptions(file.path(root, "docs", "design", "assumptions.yml"))
protocol <- read_protocol(file.path(root, "docs", "protocols", "fjf-2026-301.yml"))
estimands <- read_estimands(file.path(root, "docs", "protocols", "fjf-2026-301-estimands.yml"))
sap <- read_sap(file.path(root, "docs", "sap", "fjf-2026-301-sap.yml"))
outputs <- read_outputs(file.path(root, "docs", "sap", "fjf-2026-301-outputs.yml"))
pars <- planning_values_dry_run_301(assumptions)

test_that("the dry run gives weight, subject-level and time-to-rescue data", {
  a <- simulate_dry_run_301(30, pars, protocol, seed = 9)
  expect_setequal(unique(a$adeff$AVISITN[a$adeff$PARAMCD == "WEIGHT"]), c(12, 26))
  expect_identical(nrow(a$adsl), 60L)
  expect_true(all(a$adsl$AGE >= 18 & a$adsl$AGE <= 80 & a$adsl$BMIBL <= 45))
  # Rescue events are rescues before leaving the study and before week 26; the rest are censored.
  ev <- dplyr::inner_join(a$adtte, a$subjects, by = c("USUBJID", "ARMCD"))
  expect_identical(ev$CNSR == 0, ev$rescue_week <= pmin(ev$withdrawal_week, 26))
  expect_true(all(ev$AVAL <= 26 * 7))
})

test_that("the multiplicity graph sends all alpha through the primary hypothesis", {
  expect_identical(check_multiplicity(sap), character())
  bad <- sap
  bad$multiplicity$hypotheses[[2]]$weight <- 0.2
  expect_true(any(grepl("initial weight", check_multiplicity(bad))))
  bad <- sap
  bad$multiplicity$transitions$H2 <- c(0, 0.5, 0.5, 0.5)
  expect_true(any(grepl("self-loops", check_multiplicity(bad))))
  bad <- sap
  bad$multiplicity$hypotheses <- bad$multiplicity$hypotheses[1:3]
  bad$multiplicity$transitions$H4 <- NULL
  expect_true(any(grepl("does not test B3", check_multiplicity(bad))))
})

test_that("a secondary hypothesis cannot be rejected unless the primary is", {
  results <- tibble::tibble(
    analysis = c("A1", "B1", "B2", "B3"), estimate = c(-0.1, 3, -2, 0.3),
    p_value = c(0.20, 1e-6, 1e-6, 1e-6), null = c(0, 1, 0, 1), favourable = c("lower", "higher", "lower", "lower")
  )
  m <- test_multiplicity_301(results, sap)
  expect_false(any(m$rejected))
  results$p_value[1] <- 0.001
  expect_true(all(test_multiplicity_301(results, sap)$rejected))
})

test_that("one-sided p-values follow the favourable direction", {
  expect_equal(one_sided_p(0.04, -0.3, 0, "lower"), 0.02)
  expect_equal(one_sided_p(0.04, 0.3, 0, "lower"), 0.98)
  expect_equal(one_sided_p(0.04, 1.5, 1, "higher"), 0.02)
})

test_that("every SAP item has an output and every output traces to the SAP", {
  expect_identical(check_outputs(outputs, sap), character())
  bad <- outputs
  bad$outputs[[3]]$sap <- list("A9")
  bad$outputs[[4]]$adam <- list("ADXX.CHG")
  bad$outputs[[5]]$program <- "R/t.R"
  problems <- check_outputs(bad, sap)
  expect_true(any(grepl("unknown SAP item A9", problems)))
  no_b3 <- outputs
  no_b3$outputs <- purrr::discard(no_b3$outputs, \(o) "B3" %in% o$sap)
  expect_true(any(grepl("no output for B3", check_outputs(no_b3, sap))))
  expect_true(any(grepl("bad ADaM reference ADXX.CHG", problems)))
  expect_true(any(grepl("program outside", problems)))
  t <- traceability_301(outputs)
  expect_true(all(c("output", "sap", "adam", "program") %in% names(t)))
})

test_that("the SAP's subgroups are ones the code can derive", {
  expect_true(all(purrr::map_chr(sap$subgroups$variables, "id") %in% subgroup_ids_301))
})

test_that("on a large dry run the secondary analyses find the planning effects", {
  dry <- simulate_dry_run_301(1500, pars, protocol, seed = 302, covariates = sap$dry_run$covariates)
  draws <- draws_301(hba1c_data_301(dry, "PBO"), 5, 1)
  sec <- analyse_secondary_301(dry, sap, protocol, draws, imputations = 5)
  r <- sec$results
  # Weight: the planning effect, diluted by discontinuation, and J2R for those who left.
  expect_lt(r$estimate[r$analysis == "B2"], 0.6 * pars$weight_effect_target_dose)
  expect_gt(r$estimate[r$analysis == "B2"], 1.2 * pars$weight_effect_target_dose)
  # More responders and less rescue on FJF-2026.
  expect_gt(r$estimate[r$analysis == "B1"], 1)
  expect_lt(r$estimate[r$analysis == "B3"], 1)
  # No subgroup effect was built in: interactions are not all significant.
  expect_true(any(sec$subgroups$interaction_p > 0.05))
  expect_true(all(sec$km$survival <= 1 & sec$km$survival >= 0))
})
