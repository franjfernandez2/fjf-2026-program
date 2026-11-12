# The simulator is checked with public seeds and the planning values as truth: with many
# patients it must give back the parameters it was given.

no_mnar <- planning
no_mnar$mnar_share_of_dropouts <- 0
big_301 <- simulate_study(design, no_mnar, "301", 3000, seed = 11, keep_latent = TRUE)
big_201 <- simulate_study(design, no_mnar, "201", 1500, seed = 12, keep_latent = TRUE)

num <- function(x) as.numeric(x)
# Absolute tolerance: |actual - expected| < tol.
expect_near <- function(actual, expected, tol, info = NULL) {
  expect_lt(abs(unname(actual) - unname(expected)), tol, label = paste(info, "difference from", signif(expected, 3)))
}

# HbA1c change at a scheduled visit, for patients with no intercurrent event at all.
clean_change <- function(sim, visit) {
  t <- sim$tables
  pt <- sim$latent$patients
  lab <- t$lab_results[t$lab_results$test_code == "HBA1C", ]
  base <- lab[lab$visit_code == "V2", c("subject_id", "result")]
  at <- lab[lab$visit_code == visit, c("subject_id", "result")]
  m <- merge(base, at, by = "subject_id", suffixes = c("_base", "_at"))
  m$chg <- num(m$result_at) - num(m$result_base)
  clean <- pt$subject_id[!is.finite(pt$disc_week) & !is.finite(pt$rescue_week) & !is.finite(pt$withdrawal_week)]
  m <- m[m$subject_id %in% clean, ]
  m$arm <- pt$arm_code[match(m$subject_id, pt$subject_id)]
  m
}

# 3.5 standard errors of a difference in means between two arms: wide enough that a
# correct simulator fails about once in 2000 runs, narrow enough to catch a wrong effect.
tol_diff <- function(m, a, b) 3.5 * stats::sd(m$chg) * sqrt(1 / sum(m$arm == a) + 1 / sum(m$arm == b))

test_that("every table has exactly the columns, types and codelists of the specification", {
  for (study in c("201", "301")) {
    tables <- simulate_study(design, planning, study, 20, seed = 5)
    lists <- study_codelists(study)
    expect_identical(names(tables), purrr::map_chr(design$raw_spec$tables, "name"))
    for (t in design$raw_spec$tables) {
      if (!t$name %in% names(tables)) next
      d <- tables[[t$name]]
      expect_identical(names(d), purrr::map_chr(t$columns, "name"), info = t$name)
      for (c in t$columns) {
        x <- d[[c$name]]
        r_class <- c(text = "character", integer = "integer", numeric = "numeric")[[c$type]]
        expect_identical(class(x), r_class, info = c$name)
        if (!isTRUE(c$nullable)) expect_false(anyNA(x), info = paste(t$name, c$name))
        if (!is.null(c$codelist)) {
          expect_true(all(stats::na.omit(x) %in% lists[[c$codelist]]), info = paste(t$name, c$name))
        }
      }
      expect_identical(d$record_id, seq_len(nrow(d)))
    }
  }
})

test_that("the same seed gives the same data, and another seed other data", {
  a <- simulate_study(design, planning, "201", 10, seed = 7)
  expect_identical(simulate_study(design, planning, "201", 10, seed = 7), a)
  expect_false(identical(simulate_study(design, planning, "201", 10, seed = 8)$lab_results, a$lab_results))
})

test_that("randomisation is balanced within strata and dates follow the schedule", {
  r <- big_301$tables$randomisation
  expect_lte(max(abs(table(r$arm_code) - 3000)), 2)
  expect_equal(sum(big_301$tables$enrolment$eligible == "Y"), nrow(r))
  v <- big_301$latent$visits
  sched <- v[v$code != "UNS" & v$week > 0 & v$done, ]
  day <- num(sched$date - big_301$latent$patients$randomisation_date[sched$pt])
  expect_true(all(abs(day - 7 * sched$week) <= sched$window))
})

test_that("the 301 data give back the week-26 effect and the placebo change", {
  m <- clean_change(big_301, "V9")
  means <- tapply(m$chg, m$arm, mean)
  expect_near(means["PBO"], planning$placebo_change_week24, 3.5 * stats::sd(m$chg) / sqrt(sum(m$arm == "PBO")))
  expect_near(means["FJF20"] - means["PBO"], planning$effect_target_dose, tol_diff(m, "FJF20", "PBO"))
  expect_near(stats::sd(m$chg[m$arm == "PBO"]), planning$change_sd, 0.06)
})

test_that("the 201 data give back the dose-response at week 12", {
  m <- clean_change(big_201, "V6")
  means <- tapply(m$chg, m$arm, mean)
  curve <- true_curve(planning)
  for (d in c(5, 10, 20, 40)) {
    arm <- paste0("FJF", d)
    expected <- dose_effect(curve, d) * planning$effect_ratio_week12
    expect_near(means[arm] - means["PBO"], expected, tol_diff(m, arm, "PBO"), info = arm)
  }
})

test_that("intercurrent events happen at their planned rates", {
  pt <- big_301$latent$patients
  active <- pt$arm_code == "FJF20"
  # Leaving the study also stops treatment, so discontinuation is either event.
  either <- 1 - (1 - planning$discontinuation_placebo) * (1 - planning$study_withdrawal)
  expect_near(mean(is.finite(pt$disc_week[!active])), either, 0.02)
  expect_near(mean(is.finite(pt$withdrawal_week)), planning$study_withdrawal, 0.015)
  expect_near(mean(is.finite(pt$rescue_week[!active])), planning$rescue_placebo, 0.025)
  expect_near(mean(is.finite(pt$rescue_week[active])), planning$rescue_active, 0.015)
  # Shorter study, lower rates: 201 uses the 26-week rates over 12 weeks.
  p201 <- big_201$latent$patients
  expect_near(mean(is.finite(p201$rescue_week[p201$arm_code == "PBO"])), rate_over(planning$rescue_placebo, 12), 0.03)
})

test_that("withdrawals missing not at random are the poorer responders", {
  sim <- simulate_study(design, planning, "301", 3000, seed = 13, keep_latent = TRUE)
  pt <- sim$latent$patients
  expect_gt(mean(pt$shared[pt$withdrawal_mnar]), mean(pt$shared[!is.finite(pt$withdrawal_week)]) + 0.2)
  expect_near(mean(pt$withdrawal_mnar[is.finite(pt$withdrawal_week)]), planning$mnar_share_of_dropouts, 0.05)
})

test_that("glucose follows the protocol's rescue rule", {
  t <- big_301$tables
  pt <- big_301$latent$patients
  v <- big_301$latent$visits
  p <- design$protocols[["301"]]
  g <- v[v$done & v$has_fpg & v$code != "UNS", ]
  g$threshold <- fpg_threshold(p, g$week)
  g$rescue_week <- pt$rescue_week[g$pt]
  under_rule <- !is.na(g$threshold) & g$t < g$rescue_week
  above <- g$fpg > g$threshold
  # Above the threshold under the rule only at the visit that led to rescue.
  rescued_ids <- pt$subject_id[is.finite(pt$rescue_week)]
  expect_true(all(g$subject_id[under_rule & above] %in% rescued_ids))
  expect_identical(sum(under_rule & above), length(rescued_ids))
  # Each rescue has a confirming unscheduled glucose and a rescue medication after it.
  uns <- t$lab_results[t$lab_results$visit_code == "UNS", ]
  expect_setequal(uns$subject_id, rescued_ids)
  cm <- t$concomitant_meds[t$concomitant_meds$rescue == "Y", ]
  expect_setequal(cm$subject_id, rescued_ids)
  d <- parse_crf_date
  expect_true(all(d(cm$start_date) >= d(uns$collection_date[match(cm$subject_id, uns$subject_id)])))
})

test_that("nothing is recorded after a patient leaves the study", {
  v <- big_301$latent$visits
  pt <- big_301$latent$patients
  expect_true(all(v$t <= pt$withdrawal_week[v$pt]))
  gone <- pt$subject_id[is.finite(pt$withdrawal_week)]
  last <- big_301$tables$visits
  expect_false(any(last$subject_id %in% gone & last$visit_code == "V10"))
})
