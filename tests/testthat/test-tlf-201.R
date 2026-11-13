root <- test_path("..", "..")
sap_201 <- read_sap(file.path(root, "docs", "sap", "fjf-2026-201-sap.yml"))
outputs_201 <- read_outputs(file.path(root, "docs", "sap", "fjf-2026-201-outputs.yml"))

test_that("every SAP item of FJF-2026-201 has an output, with a program that exists", {
  expect_identical(check_outputs(outputs_201, sap_analysis_content(sap_201)), character())
  ids <- purrr::map_chr(outputs_201$outputs, "id")
  expect_setequal(names(tlf_functions_201()), ids)
  for (o in outputs_201$outputs) expect_true(file.exists(file.path(root, o$program)), info = o$program)
})

test_that("an exploratory item without an output is reported", {
  no_x1 <- outputs_201
  no_x1$outputs <- purrr::discard(no_x1$outputs, function(o) "X1" %in% unlist(o$sap))
  expect_true(any(grepl("no output for X1", check_outputs(no_x1, sap_analysis_content(sap_201)))))
})

test_that("utilities scale over the doses, best 1 and worst 0, and a flat component scores 1", {
  expect_equal(scale_utility_201(c(0, 1, 3), higher_better = TRUE), c(0, 1 / 3, 1))
  expect_equal(scale_utility_201(c(0.1, 0.3, 0.2), higher_better = FALSE), c(1, 0, 0.5))
  expect_equal(scale_utility_201(c(0.2, 0.2), higher_better = FALSE), c(1, 1))
})

test_that("the weights of the index are read from the SAP", {
  expect_identical(clinical_utility_weights_201(sap_201), c(effect = 0.7, safety = 0.3))
  bad <- sap_201
  bad$exploratory[[1]]$method <- "A utility weighted 0.6 and 0.3."
  expect_error(clinical_utility_weights_201(bad), "add up to 1")
})

# A small study: three doses, two patients each.
toy_adam_201 <- function() {
  adsl <- data.frame(
    USUBJID = sprintf("P%d", 1:6), SAFFL = "Y", TRT01AN = c(0, 0, 10, 10, 40, 40)
  )
  adae <- data.frame(
    USUBJID = c("P1", "P3", "P3", "P5"), SAFFL = "Y", TRTEMFL = c("Y", "Y", "Y", "N"),
    AEACN = c("DRUG WITHDRAWN", "DRUG WITHDRAWN", "DOSE NOT CHANGED", "DRUG WITHDRAWN")
  )
  list(ADSL = adsl, ADAE = adae)
}
toy_analysis_201 <- function(effects = c(0, -0.2, -0.4)) {
  list(
    means = list(doses = c(0, 10, 40), means = c(-0.1, -0.2, -0.6), S = diag(c(0.01, 0.02, 0.04))),
    a1 = list(model = "linear", effects = stats::setNames(effects, c(0, 10, 40)))
  )
}

test_that("the index combines the fitted effect and the discontinuation rate by dose taken", {
  u <- clinical_utility_201(toy_adam_201(), toy_analysis_201(), sap_201)
  expect_identical(u$n_disc, c(1L, 1L, 0L))
  expect_equal(u$rate, c(0.5, 0.5, 0))
  expect_equal(u$u_effect, c(0, 0.5, 1))
  expect_equal(u$u_safety, c(0, 0, 1))
  expect_equal(u$cui, c(0, 0.35, 1))
  expect_identical(unique(u$effect_source), "linear")
})

test_that("without a selected model the index uses the MMRM differences from placebo", {
  a <- toy_analysis_201()
  a$a1$effects <- NULL
  a$a1$model <- NA_character_
  u <- clinical_utility_201(toy_adam_201(), a, sap_201)
  expect_equal(u$effect, c(0, -0.1, -0.5))
  expect_identical(unique(u$effect_source), "MMRM")
})

test_that("MMRM differences from placebo take the covariance of the means", {
  m <- list(doses = c(0, 10), means = c(-0.1, -0.4), S = matrix(c(0.01, 0.004, 0.004, 0.02), 2))
  est <- mmrm_estimates_201(m)
  expect_true(is.na(est$diff[1]))
  expect_equal(est$diff[2], -0.3)
  expect_equal(est$diff_se[2], sqrt(0.01 + 0.02 - 2 * 0.004))
  expect_equal(est$upper[2] - est$lower[2], 2 * stats::qnorm(0.975) * sqrt(0.022))
})

test_that("doses and models print as in the shells", {
  expect_identical(dose_label_201(c(0, 5, 40)), c("Placebo", "5 mg", "40 mg"))
  expect_identical(model_label_201(c("sigEmax", "linear")), c("Sigmoid Emax", "Linear"))
})
