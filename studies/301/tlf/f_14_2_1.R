# F-14.2.1 Tipping-point analysis of the primary estimand (S3). Source: the efficacy analyses.

#' @inheritParams t_14_2_1_301
#' @param path File to write.
#' @param locked Whether the database is locked.
f_14_2_1_301 <- function(adam, analysis, outputs, path, locked) {
  tip <- analysis$efficacy$tipping
  plot <- ggplot2::ggplot(tip, ggplot2::aes(.data$delta, .data$estimate)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$lower, ymax = .data$upper), alpha = 0.2) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::labs(
      x = "Delta added to imputed values of the FJF-2026 arm (HbA1c %)", y = "Difference from placebo (95% CI)"
    )
  save_figure_301(plot, "F-14.2.1", outputs, path, locked,
    footnotes = "Delta-adjusted multiple imputation (S3); the dashed line is no difference."
  )
}
