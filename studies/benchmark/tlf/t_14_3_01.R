# Table 14-3.01: Primary Endpoint Analysis: ADAS Cog (11) - Change from Baseline to
# Week 24 - LOCF (SAP section 10.1.1). Population: efficacy (EFFFL = "Y").
# Source: our ADQSADAS, PARAMCD = "ACTOT", analysed records (ANL01FL = "Y").
#
# The analysis results metadata select PARAMCD = "ATOT"; ACTOT is used (D-001). Their
# SAS code for the dose response leaves out the baseline value, which the table's
# footnote includes; the footnote's model reproduces the printed p-value (D-013).

#' Fit the two ANCOVA models of Table 14-3.01.
#'
#' Dose response: CHG on dose (0, 54, 81 mg, continuous), site group and baseline; the
#' p-value tests the dose coefficient (Type III). Pairwise: the same model with treatment
#' as a factor; differences of least-squares means with their SE, p-value and 95% CI.
#' In an additive model these differences do not depend on how the LS means weight the
#' site groups.
#' @param adqsadas Our ADQSADAS.
#' @return A list with `data` (the analysed records), `dose_p` and `pairs` (a tibble).
fit_t_14_3_01 <- function(adqsadas) {
  data <- adqsadas |>
    dplyr::filter(PARAMCD == "ACTOT", EFFFL == "Y", ANL01FL == "Y") |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.vector))
  week24 <- data |>
    dplyr::filter(AVISIT == "Week 24") |>
    dplyr::mutate(TRT = factor(TRTPN, levels = c(0, 54, 81)))

  dose <- stats::lm(CHG ~ TRTPN + SITEGR1 + BASE, data = week24)
  dose_p <- stats::drop1(dose, test = "F")["TRTPN", "Pr(>F)"]

  pairwise <- stats::lm(CHG ~ TRT + SITEGR1 + BASE, data = week24)
  b <- stats::coef(pairwise)
  v <- stats::vcov(pairwise)
  contrast <- function(name, weights) {
    l <- stats::setNames(rep(0, length(b)), names(b))
    l[names(weights)] <- weights
    est <- sum(l * b)
    se <- sqrt(drop(t(l) %*% v %*% l))
    t_crit <- stats::qt(0.975, stats::df.residual(pairwise))
    tibble::tibble(
      comparison = name, estimate = est, se = se,
      p = 2 * stats::pt(-abs(est / se), stats::df.residual(pairwise)),
      lower = est - t_crit * se, upper = est + t_crit * se
    )
  }
  pairs <- dplyr::bind_rows(
    contrast("Low - Placebo", c(TRT54 = 1)),
    contrast("High - Placebo", c(TRT81 = 1)),
    contrast("High - Low", c(TRT81 = 1, TRT54 = -1))
  )
  list(data = data, dose_p = dose_p, pairs = pairs)
}

#' Build Table 14-3.01 as printed rows.
#' @param fit Output of `fit_t_14_3_01()`.
#' @return A tibble with `label` and `placebo`, `low`, `high` as printed text.
make_t_14_3_01 <- function(fit) {
  arms <- c(0, 54, 81)
  summary_rows <- function(visit, var) {
    d <- fit$data[fit$data$AVISIT == visit, ]
    by_arm <- lapply(arms, function(a) stats::na.omit(d[[var]][d$TRTPN == a]))
    # The report prints the range in whole numbers (e.g. 56.7 as 57).
    range_of <- function(x) paste0(fmt_num(min(x), 0), ";", fmt_num(max(x), 0))
    cells <- function(f) vapply(by_arm, f, "")
    tibble::tibble(
      label = c("n", "Mean (SD)", "Median (Range)"),
      placebo = NA_character_, low = NA_character_, high = NA_character_
    ) |>
      dplyr::mutate(values = list(
        cells(function(x) as.character(length(x))),
        cells(function(x) sprintf("%s (%s)", fmt_num(mean(x), 1), fmt_num(stats::sd(x), 2))),
        cells(function(x) sprintf("%s (%s)", fmt_num(stats::median(x), 1), range_of(x)))
      ))
  }
  p3 <- function(p) formatC(round_sas(p, 3), format = "f", digits = 3)
  diff <- function(r) sprintf("%s (%s)", fmt_num(r$estimate, 1), fmt_num(r$se, 2))
  ci <- function(r) sprintf("(%s;%s)", fmt_num(r$lower, 1), fmt_num(r$upper, 1))
  pr <- split(fit$pairs, fit$pairs$comparison)
  stats_rows <- tibble::tibble(
    label = c(
      "p-value(Dose Response) [1][2]", "p-value(Xan - Placebo) [1][3]", "Diff of LS Means (SE)", "95% CI",
      "p-value(Xan High - Xan Low)[1][3]", "Diff of LS Means (SE)", "95% CI"
    ),
    values = list(
      p3(fit$dose_p),
      c(p3(pr[["Low - Placebo"]]$p), p3(pr[["High - Placebo"]]$p)),
      c(diff(pr[["Low - Placebo"]]), diff(pr[["High - Placebo"]])),
      c(ci(pr[["Low - Placebo"]]), ci(pr[["High - Placebo"]])),
      p3(pr[["High - Low"]]$p), diff(pr[["High - Low"]]), ci(pr[["High - Low"]])
    )
  )
  dplyr::bind_rows(
    tibble::tibble(label = "Baseline", values = list(character())),
    dplyr::select(summary_rows("Baseline", "AVAL"), label, values),
    tibble::tibble(label = "Week 24", values = list(character())),
    dplyr::select(summary_rows("Week 24", "AVAL"), label, values),
    tibble::tibble(label = "Change from Baseline", values = list(character())),
    dplyr::select(summary_rows("Week 24", "CHG"), label, values),
    stats_rows
  )
}
