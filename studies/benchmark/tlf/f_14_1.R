# Figure 14-1: Time to Dermatologic Event by Treatment Group (SAP section 11.2).
# Population: safety (SAFFL = "Y"). Source: our ADTTE, PARAMCD = "TTDE".

#' Kaplan-Meier estimates and log-rank test for Figure 14-1.
#'
#' Confidence limits of the median use the linear ("plain") transformation, the default
#' of SAS PROC LIFETEST when the report was produced; with it the limits match the report.
#' @param adtte Our ADTTE.
#' @return A list with `fit` (survfit), `curves` (a tibble of the step curves),
#'   `summary` (subjects, events, median and its 95% limits per group) and `logrank_p`.
fit_f_14_1 <- function(adtte) {
  data <- adtte |>
    dplyr::filter(SAFFL == "Y", PARAMCD == "TTDE") |>
    dplyr::mutate(
      dplyr::across(dplyr::everything(), as.vector),
      TRTA = factor(TRTA, levels = c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose")),
      EVENT = 1 - CNSR
    )
  fit <- survival::survfit(survival::Surv(AVAL, EVENT) ~ TRTA, data = data, conf.type = "plain")
  logrank <- survival::survdiff(survival::Surv(AVAL, EVENT) ~ TRTA, data = data)
  strata <- rep(names(fit$strata), fit$strata)
  curves <- tibble::tibble(
    group = sub("^TRTA=", "", strata), time = fit$time, survival = fit$surv, censored = fit$n.censor > 0
  )
  curves <- dplyr::bind_rows(
    tibble::tibble(group = levels(data$TRTA), time = 0, survival = 1, censored = FALSE), curves
  )
  table <- summary(fit)$table
  list(
    fit = fit, curves = curves,
    summary = tibble::tibble(
      group = levels(data$TRTA), subjects = table[, "records"], events = table[, "events"],
      median = table[, "median"], lower = table[, "0.95LCL"], upper = table[, "0.95UCL"]
    ),
    logrank_p = stats::pchisq(logrank$chisq, df = length(logrank$n) - 1, lower.tail = FALSE)
  )
}

#' Draw Figure 14-1 and save it as PNG.
#' @param km Output of `fit_f_14_1()`.
#' @param path File to write.
#' @return `path`, for a file target.
make_f_14_1 <- function(km, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  plot <- ggplot2::ggplot(km$curves, ggplot2::aes(time, survival, colour = group, linetype = group)) +
    ggplot2::geom_step() +
    ggplot2::geom_point(data = km$curves[km$curves$censored, ], shape = 3, size = 1.5, show.legend = FALSE) +
    ggplot2::scale_y_continuous(limits = c(0, 1)) +
    ggplot2::labs(
      title = "Figure 14-1", subtitle = "Time to Dermatologic Event by Treatment Group",
      x = "Days", y = "Probability of no dermatologic event", colour = NULL, linetype = NULL,
      caption = paste0(
        "Population: safety. Kaplan-Meier estimates; + censored. Log-rank p ",
        if (km$logrank_p < 0.0001) "<.0001" else paste("=", fmt_p(km$logrank_p, 4)), ".\n",
        paste(
          sprintf(
            "%s: %d subjects, %d events, median %s days (95%% CI %s-%s)", km$summary$group,
            km$summary$subjects, km$summary$events, fmt_num(km$summary$median, 1),
            fmt_num(km$summary$lower, 1), fmt_num(km$summary$upper, 1)
          ),
          collapse = "\n"
        ),
        "\nSource: CDISC pilot data, re-analysed."
      )
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(legend.position = "bottom")
  ggplot2::ggsave(path, plot, width = 8, height = 5.5, dpi = 150, bg = "white")
  path
}
