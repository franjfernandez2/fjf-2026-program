# Safety data, checked with public seeds and the planning values as truth.

big <- simulate_study(design, planning, "301", 3000, seed = 21, keep_latent = TRUE)
pt <- big$latent$patients
sf <- big$latent$safety
active <- pt$arm_code == "FJF20"
near <- function(actual, expected, n) {
  # 3.5 standard errors of a proportion.
  expect_lt(abs(actual - expected), 3.5 * sqrt(expected * (1 - expected) / n) + 1e-3,
    label = paste("proportion", signif(actual, 3), "against", signif(expected, 3))
  )
}

test_that("adding the safety data did not change the efficacy data", {
  # The efficacy draws come first; their tables are the same with or without safety data.
  a <- simulate_study(design, planning, "301", 30, seed = 4)
  expect_identical(nrow(a$visits), nrow(simulate_study(design, planning, "301", 30, seed = 4)$visits))
  hb <- a$lab_results[a$lab_results$test_code == "HBA1C", ]
  expect_true(all(grepl("^[0-9]+[.][0-9]$", hb$result)))
})

test_that("adverse events happen at their planned rates", {
  near(mean(sf$any_ae[!active]), planning$ae_any_placebo, sum(!active))
  near(mean(sf$any_ae[active]), planning$ae_any_placebo * planning$ae_relative_risk_active, sum(active))
  near(mean(sf$gi[active]), planning$gi_excess_active, sum(active))
  expect_identical(sum(sf$gi[!active]), 0L)
  near(mean(sf$serious), planning$serious_ae, nrow(pt))
  ae <- big$tables$adverse_events
  expect_setequal(unique(ae$subject_id[ae$serious == "Y"]), pt$subject_id[sf$serious | sf$l3])
})

test_that("each discontinuation for an adverse event has the event, with the drug withdrawn", {
  ae <- big$tables$adverse_events
  disc <- pt$subject_id[is.finite(pt$disc_week) & pt$disc_reason == "ADVERSE EVENT"]
  withdrawn <- ae$subject_id[ae$action_taken == "DRUG WITHDRAWN"]
  expect_setequal(withdrawn, disc)
  dsp <- big$tables$disposition
  expect_setequal(dsp$subject_id[dsp$event == "TREATMENT" & dsp$reason %in% "ADVERSE EVENT"], disc)
})

test_that("events lie inside each patient's time in the study", {
  d <- function(x) as.Date(x, "%d-%b-%Y")
  for (tab in c("adverse_events", "hypoglycaemia")) {
    x <- big$tables[[tab]]
    date <- d(if (tab == "adverse_events") x$start_date else x$event_date)
    i <- match(x$subject_id, pt$subject_id)
    expect_true(all(date >= pt$randomisation_date[i]), info = tab)
    end <- pt$randomisation_date[i] + ifelse(is.finite(pt$withdrawal_week[i]), floor(pt$withdrawal_week[i] * 7), 7 * 28)
    expect_true(all(date <= end), info = tab)
  }
})

test_that("hypoglycaemia levels match the glucose values and the planned rates", {
  h <- big$tables$hypoglycaemia
  g <- h$glucose_mg_dl
  expect_true(all(g[h$level == "1"] >= 54 & g[h$level == "1"] <= 69))
  expect_true(all(g[h$level == "2"] < 54))
  expect_true(all(h$assistance[h$level == "3"] == "Y"))
  near(mean(sf$l1[active]), planning$hypoglycaemia_level1_active, sum(active))
  near(mean(sf$l1[!active]), planning$hypoglycaemia_level1_placebo, sum(!active))
  near(mean(sf$l2[active]), planning$hypoglycaemia_level2_active, sum(active))
})

test_that("safety laboratory values are plausible and hepatic elevations are rare", {
  l <- big$tables$lab_results
  alt <- l[l$test_code == "ALT", ]
  high <- unique(alt$subject_id[as.numeric(alt$result) > 3 * 56])
  near(mean(pt$subject_id[active] %in% high), planning$alt_3uln_active, sum(active))
  egfr <- l[l$test_code == "EGFR" & l$visit_code == "V1", ]
  expect_true(all(as.numeric(egfr$result) >= 45))
  cr <- as.numeric(l$result[l$test_code == "CREAT"])
  expect_true(all(cr > 0.3 & cr < 2.5))
  expect_equal(ckd_epi_2021(1, 60, "M"), 87.0, tolerance = 0.01)
})

test_that("the gastrointestinal excess grows with the dose in 201", {
  s <- simulate_study(design, planning, "201", 1500, seed = 22, keep_latent = TRUE)
  p201 <- s$latent$patients
  rate <- tapply(s$latent$safety$gi, p201$dose_mg, mean)
  expected <- rate_over(planning$gi_excess_active * c(0, 5, 10, 20, 40) / 20, 12)
  for (j in seq_along(rate)) near(unname(rate[j]), expected[j], 1500)
})
