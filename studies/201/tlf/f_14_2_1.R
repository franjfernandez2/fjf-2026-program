# F-14.2.1 Dose-response of HbA1c at week 12 (A1, S1).
# Source: the MMRM means and the fitted effects of analyse_201().

#' @inheritParams t_14_2_1_201
#' @param path File to write.
#' @param locked Whether the database is locked.
f_14_2_1_201 <- function(adam, analysis, outputs, path, locked) {
  r <- analysis$mcpmod
  est <- mmrm_estimates_201(r$means)
  est$diff[est$dose == 0] <- 0
  curves <- dplyr::bind_rows(
    if (!is.null(r$a1$effects)) {
      tibble::tibble(fit = sprintf("A1: %s", model_label_201(r$a1$model)), dose = r$means$doses, effect = r$a1$effects)
    },
    if (!is.null(r$s1)) tibble::tibble(fit = "S1: model averaging", dose = r$means$doses, effect = r$s1$effects)
  )
  relevance <- analysis$sap$primary$mod$target_dose$clinical_relevance
  plot <- ggplot2::ggplot(est, ggplot2::aes(.data$dose, .data$diff)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey50") +
    ggplot2::geom_hline(yintercept = -relevance, linetype = "dashed", colour = "grey40") +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = .data$lower, ymax = .data$upper), width = 0.8, na.rm = TRUE) +
    ggplot2::geom_point(size = 2) +
    ggplot2::scale_x_continuous(breaks = r$means$doses) +
    ggplot2::labs(x = "Dose (mg)", y = "Difference from placebo in HbA1c change (%)", colour = NULL)
  if (nrow(curves) > 0) {
    plot <- plot + ggplot2::geom_line(data = curves, ggplot2::aes(.data$dose, .data$effect, colour = .data$fit))
  }
  save_figure_301(plot, "F-14.2.1", outputs, path, locked,
    footnotes = c(
      "Points and bars: MMRM difference from placebo (95% CI). Lines: fitted effects at the study doses.",
      sprintf("Dashed line: clinical relevance, %s points better than placebo.", format(relevance))
    )
  )
}
