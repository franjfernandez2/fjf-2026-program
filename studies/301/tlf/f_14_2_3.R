# F-14.2.3 Kaplan-Meier curves of the time to first rescue medication (B3).
# Source: ADTTE (PARAMCD TTRESC), full analysis set.

#' A Kaplan-Meier figure with the patients at risk under it.
#'
#' @param d ADTTE records of one parameter.
#' @param arm Arm label of each record.
#' @param y "survival" (proportion without event) or "incidence" (cumulative incidence).
#' @param x_label,y_label Axis labels.
#' @param every Days between the at-risk counts.
#' @return A ggplot.
km_plot_301 <- function(d, arm, y, x_label, y_label, every = 28) {
  fit <- survival::survfit(survival::Surv(d$AVAL, 1 - d$CNSR) ~ arm)
  # With a single arm survfit has no strata.
  names_of <- if (is.null(fit$strata)) unique(arm) else sub("^arm=", "", names(fit$strata))
  sizes <- if (is.null(fit$strata)) length(fit$time) else fit$strata
  curve <- dplyr::bind_rows(
    tibble::tibble(arm = names_of, time = 0, surv = 1),
    tibble::tibble(arm = rep(names_of, sizes), time = fit$time, surv = fit$surv)
  )
  curve$value <- if (y == "survival") curve$surv else 1 - curve$surv
  days <- seq(0, max(d$AVAL), by = every)
  at_risk <- summary(fit, times = days, extend = TRUE)
  risk_arm <- if (is.null(at_risk$strata)) rep(names_of, length(at_risk$time)) else sub("^arm=", "", at_risk$strata)
  risk <- tibble::tibble(arm = risk_arm, time = at_risk$time, n = at_risk$n.risk)
  low <- if (y == "survival") min(curve$value) else 0
  high <- if (y == "survival") 1 else max(curve$value)
  span <- max(high - low, 0.05)
  arms <- sort(unique(risk$arm), decreasing = TRUE)
  risk$y <- low - span * (0.12 + 0.07 * (match(risk$arm, arms) - 1))
  ggplot2::ggplot(curve, ggplot2::aes(.data$time, .data$value, colour = .data$arm)) +
    ggplot2::geom_step() +
    ggplot2::geom_text(data = risk, ggplot2::aes(.data$time, .data$y, label = .data$n), size = 3, show.legend = FALSE) +
    ggplot2::annotate("text", x = 0, y = low - span * 0.05, label = "Patients at risk", hjust = 0, size = 3) +
    ggplot2::scale_x_continuous(breaks = days) +
    ggplot2::labs(x = x_label, y = y_label, colour = NULL)
}

#' @inheritParams f_14_2_1_301
f_14_2_3_301 <- function(adam, analysis, outputs, path, locked) {
  d <- adam$ADTTE[adam$ADTTE$PARAMCD == "TTRESC" & adam$ADTTE$FASFL == "Y", ]
  plot <- km_plot_301(d, d$TRTP, "survival", "Days from randomisation", "Proportion without rescue medication")
  save_figure_301(plot, "F-14.2.3", outputs, path, locked,
    footnotes = c(
      "Patients without rescue are censored at their last contact.", "Numbers under the curves: patients at risk."
    )
  )
}
