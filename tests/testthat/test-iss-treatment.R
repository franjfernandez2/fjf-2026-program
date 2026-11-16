root <- test_path("..", "..")
sap_iss <- read_iss_sap(file.path(root, "docs", "sap", "fjf-2026-iss-sap.yml"))
outputs_trt <- read_outputs(file.path(root, "docs", "sap", "fjf-2026-iss-treatment-outputs.yml"))

# A study with the ADaM datasets the integrated analysis reads. `doses` gives the dose of each
# arm; every arm has `n` patients. Events, episodes and laboratory values come from `seed`.
toy_study_trt <- function(study, doses, n, seed, weeks) {
  set.seed(seed)
  dose <- rep(doses, each = n)
  ids <- sprintf("FJF-2026-%s-%03d", study, seq_along(dose))
  arm <- ifelse(dose == 0, "Placebo + metformin", sprintf("FJF-2026 %d mg + metformin", dose))
  days <- pmax(1L, as.integer(round(weeks * 7 - stats::rexp(length(ids), 1 / 10))))
  adsl <- data.frame(
    STUDYID = sprintf("FJF-2026-%s", study), USUBJID = ids, SAFFL = "Y", TRT01A = arm, TRT01AN = dose,
    TRT01P = arm, TRT01PN = dose
  )
  adex <- data.frame(
    USUBJID = ids, TRTA = arm, TRTAN = dose, SAFFL = "Y", TRTDURD = days, TRTDURY = days / 365.25,
    COMPLPCT = round(stats::runif(length(ids), 70, 125), 1)
  )
  terms <- data.frame(
    AEDECOD = c("Nausea", "Diarrhoea", "Headache", "Dizziness", "Nasopharyngitis"),
    AEBODSYS = c(
      "GASTROINTESTINAL DISORDERS", "GASTROINTESTINAL DISORDERS", "NERVOUS SYSTEM DISORDERS",
      "NERVOUS SYSTEM DISORDERS", "INFECTIONS AND INFESTATIONS"
    )
  )
  n_ae <- stats::rpois(length(ids), 0.8 + dose / 40)
  who <- rep(seq_along(ids), n_ae)
  pick <- sample(nrow(terms), length(who), replace = TRUE)
  adae <- data.frame(
    USUBJID = ids[who], TRTA = arm[who], TRTAN = dose[who], SAFFL = "Y",
    TRTEMFL = sample(c("Y", NA), length(who), replace = TRUE, prob = c(0.9, 0.1)),
    AEDECOD = terms$AEDECOD[pick], AEBODSYS = terms$AEBODSYS[pick],
    AEREL = sample(c("RELATED", "NOT RELATED"), length(who), replace = TRUE),
    ASEV = sample(c("MILD", "MODERATE", "SEVERE"), length(who), replace = TRUE, prob = c(0.6, 0.3, 0.1)),
    AESER = sample(c("Y", "N"), length(who), replace = TRUE, prob = c(0.05, 0.95)),
    AEACN = sample(
      c("DOSE NOT CHANGED", "DRUG WITHDRAWN", "DRUG INTERRUPTED"), length(who),
      replace = TRUE, prob = c(0.9, 0.05, 0.05)
    ),
    AEOUT = "RECOVERED"
  )
  n_hypo <- stats::rnbinom(length(ids), size = 0.5, mu = 0.4 + dose / 50)
  who <- rep(seq_along(ids), n_hypo)
  adhypo <- data.frame(
    USUBJID = ids[who], TRTA = arm[who], TRTAN = dose[who], SAFFL = "Y",
    HYPOLVL = sample(1:3, length(who), replace = TRUE, prob = c(0.6, 0.3, 0.1)),
    ONTRTFL = sample(c("Y", NA), length(who), replace = TRUE, prob = c(0.9, 0.1))
  )
  lab <- expand.grid(i = seq_along(ids), PARAMCD = c("ALT", "AST", "BILI"), visit = 1:3, stringsAsFactors = FALSE)
  adlb <- data.frame(
    USUBJID = ids[lab$i], TRTA = arm[lab$i], TRTAN = dose[lab$i], SAFFL = "Y", PARAMCD = lab$PARAMCD,
    ANRHI = ifelse(lab$PARAMCD == "BILI", 1.2, 40), ONTRTFL = ifelse(lab$visit == 3, NA, "Y")
  )
  adlb$AVAL <- adlb$ANRHI * stats::rlnorm(nrow(adlb), log(0.6), 0.5)
  # One patient of the first arm is a potential Hy's law case.
  case <- adlb$USUBJID == ids[1] & adlb$ONTRTFL %in% "Y"
  adlb$AVAL[case] <- adlb$ANRHI[case] * 4
  list(ADSL = adsl, ADAE = adae, ADHYPO = adhypo, ADLB = adlb, ADEX = adex)
}

toy_201 <- toy_study_trt("201", c(0, 5, 10, 20, 40), 30, seed = 11, weeks = 12)
toy_301 <- toy_study_trt("301", c(0, 20), 60, seed = 12, weeks = 26)
datasets_trt <- unlist(outputs_trt$adam_datasets)
toy_pool <- pool_iss_treatment(list(`201` = toy_201, `301` = toy_301), sap_iss, datasets_trt)

test_that("the outputs by treatment cover the frozen plan, and the plan is frozen", {
  expect_identical(check_iss_outputs(outputs_trt, sap_iss), character())
  expect_identical(sap_iss$version, "1.0")
  expect_setequal(purrr::map_chr(sap_iss$analyses, "id"), paste0("I", 1:6))
  for (o in outputs_trt$outputs) expect_true(file.exists(file.path(root, o$program)), info = o$id)
  broken <- outputs_trt
  broken$outputs <- purrr::discard(broken$outputs, function(o) "I5" %in% unlist(o$sap))
  expect_match(check_iss_outputs(broken, sap_iss), "no output for I5")
})

test_that("pooling puts each patient in the column of the dose taken, and in all doses when on FJF-2026", {
  adsl <- toy_pool$ADSL
  expect_identical(as.vector(table(adsl$ISSCOL)[iss_columns(sap_iss)$id]), c(90L, 30L, 30L, 90L, 30L, 180L))
  expect_true(all(adsl$TRT01AN[adsl$ISSCOL == "PBO"] == 6))
  expect_false(any(adsl$ISSDOSE[adsl$ISSCOL == "FJFALL"] == 0))
  expect_identical(sort(unique(adsl$STUDYID[adsl$ISSCOL == "FJF20"])), c("FJF-2026-201", "FJF-2026-301"))
  expect_identical(unique(adsl$STUDYID[adsl$ISSCOL == "FJF40"]), "FJF-2026-201")
  # Columns come out in the order of the plan, placebo first.
  expect_identical(arms_301(adsl, "TRT01A")$label, iss_columns(sap_iss)$label)
  s <- iss_safety_set(toy_pool, sap_iss)
  expect_identical(s$arms$n, c(90L, 30L, 30L, 90L, 30L, 180L))
  expect_equal(sum(iss_years(toy_pool, s)[1:5]), sum(c(toy_201$ADEX$TRTDURY, toy_301$ADEX$TRTDURY)))
})

test_that("a dose without a column, or a patient in two studies, stops the pooling", {
  odd <- toy_study_trt("301", c(0, 15), 5, seed = 1, weeks = 26)
  expect_error(pool_iss_treatment(list(`201` = toy_201, `301` = odd), sap_iss, datasets_trt), "no column in the plan")
  expect_error(pool_iss_treatment(list(`201` = toy_201, `301` = toy_201), sap_iss, datasets_trt), "more than one study")
})

test_that("the Mantel-Haenszel risk difference is the usual one with one stratum, and Sato's with two", {
  one <- mh_risk_difference(12, 80, 5, 60)
  expect_equal(one$estimate, 12 / 80 - 5 / 60)
  expect_equal(one$se, sqrt(12 * 68 / 80^3 + 5 * 55 / 60^3))
  # Two strata, worked by hand: weights 80*60/140 and 150*150/300.
  two <- mh_risk_difference(c(12, 30), c(80, 150), c(5, 18), c(60, 150))
  w <- c(80 * 60 / 140, 75)
  expect_equal(two$estimate, sum(w * c(12 / 80 - 5 / 60, 30 / 150 - 18 / 150)) / sum(w))
  p <- c((80^2 * 5 - 60^2 * 12 + 80 * 60 * (60 - 80) / 2) / 140^2, (150^2 * 18 - 150^2 * 30) / 300^2)
  q <- c((12 * 55 + 5 * 68) / 280, (30 * 132 + 18 * 120) / 600)
  expect_equal(two$se, sqrt(two$estimate * sum(p) + sum(q)) / sum(w))
  expect_equal(c(two$lower, two$upper), two$estimate + c(-1, 1) * stats::qnorm(0.975) * two$se)
  # A stratum without one of the arms is left out; without any stratum there is no estimate.
  expect_equal(mh_risk_difference(c(12, 3), c(80, 10), c(5, 0), c(60, 0))$estimate, one$estimate)
  expect_true(is.na(mh_risk_difference(3, 10, 0, 0)$estimate))
})

test_that("an estimate that rounds to zero is printed without a sign, in both programmings", {
  expect_identical(iss_est_ci(-1e-15, -10.64, 10.64, 1), "0.0 (-10.6, 10.6)")
  expect_identical(iss_est_ci(-0.04, -0.004, 0.2, 2), "-0.04 (0.00, 0.20)")
  expect_identical(qc_num_iss(-1e-15, 1), "0.0")
  expect_identical(qc_num_iss(-0.06, 1), "-0.1")
})

test_that("the Mantel-Haenszel estimate is the one of tern, an independent implementation", {
  skip_if_not_installed("tern")
  two <- iss_two_arms(iss_safety_set(toy_pool, sap_iss))
  ae <- teae_301(toy_pool)
  rsp <- two$USUBJID %in% ae$USUBJID[ae$AEDECOD == "Nausea"]
  grp <- factor(ifelse(two$ISSCOL == "FJF20", "active", "control"), levels = c("control", "active"))
  theirs <- tern::prop_diff_cmh(rsp, grp, factor(two$STUDYID), conf_level = 0.95)
  x <- tapply(rsp, list(two$STUDYID, grp), sum)
  n <- tapply(rsp, list(two$STUDYID, grp), length)
  ours <- mh_risk_difference(x[, "active"], n[, "active"], x[, "control"], n[, "control"])
  expect_equal(100 * ours$estimate, as.numeric(theirs$diff), tolerance = 1e-8)
})

test_that("the negative binomial rate ratio is the one of the model, and is not estimable without events in an arm", {
  s <- iss_safety_set(toy_pool, sap_iss)
  d <- iss_hypo_counts(toy_pool, s)
  expect_identical(nrow(d), 180L)
  expect_identical(sum(d$episodes), {
    h <- rbind(toy_201$ADHYPO, toy_301$ADHYPO)
    sum(h$ONTRTFL %in% "Y" & h$HYPOLVL >= 2 & h$TRTAN %in% c(0, 20))
  })
  fit <- nb_rate_ratio(d$episodes, d$ISSCOL == "FJF20", d$STUDYID, d$years)
  direct <- MASS::glm.nb(episodes ~ I(ISSCOL == "FJF20") + STUDYID + offset(log(years)), data = d)
  expect_equal(fit$estimate, unname(exp(stats::coef(direct)[2])))
  expect_true(fit$lower < fit$estimate && fit$estimate < fit$upper)
  # The second programming maximises the likelihood itself and prints the same cell.
  expect_identical(
    qc_nb_ratio_iss(d$episodes, d$ISSCOL == "FJF20", d$STUDYID, d$years),
    iss_est_ci(fit$estimate, fit$lower, fit$upper, 2)
  )
  none <- nb_rate_ratio(c(0, 0, 1, 2), c(TRUE, TRUE, FALSE, FALSE), rep("A", 4), rep(0.5, 4))
  expect_true(is.na(none$estimate))
  expect_match(none$notes, "not estimable")
  expect_identical(qc_nb_ratio_iss(c(0, 0, 1, 2), c(TRUE, TRUE, FALSE, FALSE), rep("A", 4), rep(0.5, 4)), "-")
})

test_that("every table is made, with the columns of the plan and the cells counted by hand", {
  tables <- tables_iss_treatment(toy_pool, sap_iss, outputs_trt)
  expect_setequal(names(tables), setdiff(purrr::map_chr(outputs_trt$outputs, "id"), c("F-ISS-1", "F-ISS-2")))
  t9 <- tables[["T-ISS-9"]]
  expect_identical(unname(t9$columns[["arm1_n"]]), "Placebo|(N=90)|n (%)")
  expect_identical(sub("[|].*", "", unname(t9$columns[["arm6_n"]])), "FJF-2026 all doses")
  ae <- rbind(toy_201$ADAE, toy_301$ADAE)
  ae <- ae[ae$TRTEMFL %in% "Y", ]
  n_pbo <- length(unique(ae$USUBJID[ae$TRTAN == 0]))
  py_pbo <- sum(c(toy_201$ADEX$TRTDURY[toy_201$ADEX$TRTAN == 0], toy_301$ADEX$TRTDURY[toy_301$ADEX$TRTAN == 0]))
  any_teae <- t9$rows[t9$rows$label == "Any TEAE", ]
  expect_identical(any_teae$arm1_n, fmt_n_pct(n_pbo, 90L, digits = 1))
  expect_identical(any_teae$arm1_r, fmt_num(100 * n_pbo / py_pbo, 1))
  # The comparison shows each study, then both.
  t10 <- tables[["T-ISS-10"]]
  expect_identical(names(t10$columns), c("s1_a", "s1_p", "s2_a", "s2_p", "all_a", "all_p", "rd"))
  expect_match(t10$rows$s1_a[1], "/30 ")
  expect_match(t10$rows$s2_p[1], "/60 ")
  expect_match(t10$rows$all_a[1], "/90 ")
  expect_match(t10$rows$rd[1], "^-?[0-9.]+ \\(-?[0-9.]+, -?[0-9.]+\\)$")
  # The dose-response table has the five arms of FJF-2026-201 and no pooled column.
  t16 <- tables[["T-ISS-16"]]
  expect_identical(unname(t16$columns), sprintf("%s|(N=30)", iss_columns(sap_iss)$label[1:5]))
  # The planted case reaches the liver table and the listing, once.
  t17 <- tables[["T-ISS-17"]]
  expect_identical(t17$rows$arm1[4], fmt_n_pct(2L, 90L, digits = 1))
  listing <- tables[["L-ISS-1"]]
  expect_identical(listing$rows$label, c("FJF-2026-201-001", "FJF-2026-301-001"))
  expect_identical(listing$rows$alt, c("4.00", "4.00"))
})

test_that("without a potential Hy's law case the listing says so and the comparison still holds", {
  calm <- function(a) {
    a$ADLB$AVAL <- a$ADLB$ANRHI * 0.5
    a
  }
  pool <- pool_iss_treatment(list(`201` = calm(toy_201), `301` = calm(toy_301)), sap_iss, datasets_trt)
  tables <- tables_iss_treatment(pool, sap_iss, outputs_trt)
  expect_identical(tables[["L-ISS-1"]]$rows$label, "No records")
  expect_identical(tables[["T-ISS-17"]]$rows$arm6[2:4], c("0", "0", "0"))
  expect_true(compare_tlf_iss_treatment(tables, qc_tlf_iss_treatment(calm(toy_201), calm(toy_301)))$clean)
})

test_that("the second programming finds every one of its cells in the production tables", {
  tables <- tables_iss_treatment(toy_pool, sap_iss, outputs_trt)
  expected <- qc_tlf_iss_treatment(toy_201, toy_301)
  comparison <- compare_tlf_iss_treatment(tables, expected)
  expect_true(comparison$clean, info = paste(utils::capture.output(print(utils::head(
    comparison$cells[!comparison$cells$match, ], 20
  ))), collapse = "\n"))
  expect_setequal(unique(expected$id), names(tables))
  expect_gt(nrow(expected), 500)
  # A wrong cell is caught.
  tables[["T-ISS-9"]]$rows$arm4_r[1] <- "0.0"
  expect_false(compare_tlf_iss_treatment(tables, expected)$clean)
})

test_that("the outputs are written, the listing as a listing, with the blinding note until the unblinding", {
  tables <- tables_iss_treatment(toy_pool, sap_iss, outputs_trt)
  dir <- file.path(tempdir(), "iss-treatment")
  paths <- write_tlf_iss_treatment(tables, toy_pool, sap_iss, outputs_trt, locked = FALSE, dir)
  expect_length(paths, length(outputs_trt$outputs))
  expect_true(all(file.exists(paths)))
  expect_identical(sum(grepl("[.]png$", paths)), 2L)
  listing <- readLines(file.path(dir, "l_iss_1.txt"))
  expect_match(listing[2], "^Listing ISS-1: Patients with peak ALT or AST")
  expect_true(any(grepl("^BLINDED", readLines(file.path(dir, "t_iss_9.txt")))))
  write_tlf_iss_treatment(tables, toy_pool, sap_iss, outputs_trt, locked = TRUE, dir)
  expect_false(any(grepl("^BLINDED", readLines(file.path(dir, "t_iss_9.txt")))))
})
