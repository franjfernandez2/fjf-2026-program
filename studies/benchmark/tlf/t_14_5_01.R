# Table 14-5.01: Incidence of Treatment Emergent Adverse Events by Treatment Group
# (SAP section 11.2). Population: safety (SAFFL = "Y"). Sources: our ADSL and ADAE.

#' Format a Fisher's exact p-value as the pilot's report does.
#'
#' Up to three decimals with no trailing zeros ("0.58"), "0.000" when it rounds to zero,
#' ">0.99" above 0.99, and an asterisk below 0.15. Blank when neither group has a subject
#' with the event. The p-value is rounded once; the report rounded twice in four rows (D-012).
#' @param p P-values.
#' @param any_event Whether either group has a subject with the event.
fmt_fisher <- function(p, any_event) {
  rounded <- round_sas(p, 3)
  shown <- ifelse(p > 0.99, ">0.99", ifelse(rounded == 0, "0.000", as.character(rounded)))
  shown <- ifelse(p < 0.15, paste0(shown, "*"), shown)
  ifelse(any_event, shown, "")
}

#' Build Table 14-5.01 from ADSL and ADAE.
#'
#' One row for any event, one per system organ class (alphabetical) and one per
#' preferred term within it (by decreasing number of high-dose subjects, then
#' alphabetical, as the report orders them). Each cell gives subjects n (%) and events [n].
#' @param adsl Our ADSL.
#' @param adae Our ADAE.
#' @return A tibble with `soc`, `term` (NA on a class row), `level`, the three treatment
#'   cells and two p-values, as printed text.
make_t_14_5_01 <- function(adsl, adae) {
  arms <- c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose")
  safety <- adsl |> dplyr::filter(SAFFL == "Y")
  big_n <- table(factor(safety$TRT01A, levels = arms))
  teae <- adae |>
    dplyr::filter(SAFFL == "Y", TRTEMFL == "Y") |>
    dplyr::mutate(TRTA = factor(TRTA, levels = arms))

  summarise_group <- function(data) {
    subjects <- tapply(data$USUBJID, data$TRTA, function(x) length(unique(x)), default = 0)
    events <- table(data$TRTA)
    cells <- vapply(seq_along(arms), function(j) {
      if (subjects[j] == 0) {
        return("0")
      }
      paste0(fmt_n_pct(subjects[j], big_n[j], digits = 1), " [", events[j], "]")
    }, "")
    fisher <- function(j) {
      m <- matrix(c(subjects[1], big_n[1] - subjects[1], subjects[j], big_n[j] - subjects[j]), 2)
      fmt_fisher(stats::fisher.test(m)$p.value, subjects[1] + subjects[j] > 0)
    }
    tibble::tibble(
      placebo = cells[1], low = cells[2], high = cells[3],
      p_low = fisher(2), p_high = fisher(3), n_high = as.numeric(subjects[3])
    )
  }

  any_row <- dplyr::bind_cols(
    tibble::tibble(soc = "ANY BODY SYSTEM", term = NA_character_, level = 0),
    summarise_group(teae)
  )
  socs <- sort(unique(teae$AEBODSYS))
  rows <- lapply(socs, function(s) {
    in_soc <- teae[teae$AEBODSYS == s, ]
    soc_row <- dplyr::bind_cols(tibble::tibble(soc = s, term = NA_character_, level = 1), summarise_group(in_soc))
    terms <- lapply(sort(unique(in_soc$AEDECOD)), function(t) {
      dplyr::bind_cols(tibble::tibble(soc = s, term = t, level = 2), summarise_group(in_soc[in_soc$AEDECOD == t, ]))
    })
    terms <- do.call(rbind, terms)
    terms <- terms[order(-terms$n_high, terms$term), ]
    rbind(soc_row, terms)
  })
  dplyr::bind_rows(any_row, rows) |> dplyr::select(-n_high)
}
