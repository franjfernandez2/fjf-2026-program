# F-14.2.4 Treatment effect on HbA1c at week 26 by subgroup. Source: the subgroup analysis
# (A1's imputations, ANCOVA within each level).

#' @inheritParams f_14_2_1_301
f_14_2_4_301 <- function(adam, analysis, outputs, path, locked) {
  sg <- analysis$secondary$subgroups
  overall <- analysis$efficacy$results[analysis$efficacy$results$analysis == "A1", ]
  sg$row <- sprintf("%s: %s (n=%d)", sg$subgroup, sg$level, sg$n)
  sg$row <- factor(sg$row, levels = rev(sg$row))
  sg$p <- sprintf("Interaction p=%s", p_301(sg$interaction_p))
  first <- !duplicated(sg$subgroup)
  plot <- ggplot2::ggplot(sg, ggplot2::aes(.data$estimate, .data$row)) +
    ggplot2::geom_vline(xintercept = 0) +
    ggplot2::geom_vline(xintercept = overall$estimate, linetype = "dashed", colour = "grey40") +
    ggplot2::geom_errorbar(ggplot2::aes(xmin = .data$lower, xmax = .data$upper), width = 0.2, orientation = "y") +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_text(data = sg[first, ], ggplot2::aes(x = Inf, label = .data$p), hjust = 1.05, size = 3) +
    ggplot2::labs(x = "Difference from placebo in change from baseline in HbA1c (%) (95% CI)", y = NULL)
  save_figure_301(plot, "F-14.2.4", outputs, path, locked,
    footnotes = c(
      "Dashed line: overall estimate (A1).",
      "Subgroup results are descriptive, without adjustment for multiplicity."
    )
  )
}
