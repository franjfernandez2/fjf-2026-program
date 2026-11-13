# F-14.3.2 eDISH plot of peak ALT against peak total bilirubin, on treatment.
# Source: ADLB (ALT and BILI on treatment, ONTRTFL), safety set.

#' @inheritParams f_14_2_1_301
f_14_3_2_301 <- function(adam, analysis, outputs, path, locked) {
  lb <- adam$ADLB
  lb <- lb[lb$SAFFL == "Y" & lb$ONTRTFL %in% "Y" & lb$PARAMCD %in% c("ALT", "BILI"), ]
  lb <- lb[!is.na(lb$AVAL) & !is.na(lb$ANRHI), ]
  lb$xuln <- lb$AVAL / lb$ANRHI
  peak <- if (nrow(lb) == 0) {
    tibble::tibble(USUBJID = character(), TRTA = character(), PARAMCD = character(), xuln = numeric())
  } else {
    grouped <- dplyr::group_by(lb, .data$USUBJID, .data$TRTA, .data$PARAMCD)
    dplyr::summarise(grouped, xuln = max(.data$xuln), .groups = "drop")
  }
  wide <- tidyr::pivot_wider(peak, names_from = "PARAMCD", values_from = "xuln")
  for (p in setdiff(c("ALT", "BILI"), names(wide))) wide[[p]] <- rep(NA_real_, nrow(wide))
  wide <- wide[!is.na(wide$ALT) & !is.na(wide$BILI), ]
  plot <- ggplot2::ggplot(wide, ggplot2::aes(.data$ALT, .data$BILI, colour = .data$TRTA, shape = .data$TRTA)) +
    ggplot2::geom_vline(xintercept = 3, linetype = "dashed") +
    ggplot2::geom_hline(yintercept = 2, linetype = "dashed") +
    ggplot2::geom_point(alpha = 0.7) +
    ggplot2::scale_x_log10() +
    ggplot2::scale_y_log10() +
    ggplot2::labs(
      x = "Peak ALT on treatment (x ULN)", y = "Peak total bilirubin on treatment (x ULN)", colour = NULL, shape = NULL
    )
  save_figure_301(plot, "F-14.3.2", outputs, path, locked,
    footnotes = c(
      "One point per patient. Reference lines: ALT 3x ULN, total bilirubin 2x ULN.", "ULN: upper limit of normal."
    )
  )
}
