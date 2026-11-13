# F-14.2.2 Mean change in HbA1c over time. Source: ADEFF, observed values (ANL01FL), FAS.

#' @inheritParams f_14_2_1_301
f_14_2_2_301 <- function(adam, analysis, outputs, path, locked) {
  d <- adam$ADEFF
  d <- d[d$FASFL == "Y" & d$PARAMCD == "HBA1C" & d$ANL01FL %in% "Y" & is.na(d$DTYPE), ]
  d <- d[!is.na(d$CHG) & d$AVISIT != "Baseline", ]
  means <- dplyr::summarise(dplyr::group_by(d, .data$TRTP, .data$AVISITN),
    mean = mean(.data$CHG), se = stats::sd(.data$CHG) / sqrt(dplyr::n()), n = dplyr::n(), .groups = "drop"
  )
  plot <- ggplot2::ggplot(means, ggplot2::aes(.data$AVISITN, .data$mean, colour = .data$TRTP, shape = .data$TRTP)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey60") +
    ggplot2::geom_line(position = ggplot2::position_dodge(width = 0.6)) +
    ggplot2::geom_pointrange(ggplot2::aes(ymin = .data$mean - .data$se, ymax = .data$mean + .data$se),
      position = ggplot2::position_dodge(width = 0.6)
    ) +
    ggplot2::scale_x_continuous(breaks = sort(unique(means$AVISITN))) +
    ggplot2::labs(x = "Week", y = "Mean change from baseline in HbA1c (%) (SE)", colour = NULL, shape = NULL)
  save_figure_301(plot, "F-14.2.2", outputs, path, locked,
    footnotes = "Observed values, including those after treatment discontinuation or rescue medication."
  )
}
