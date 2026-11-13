# T-14.3.2 Treatment-emergent adverse events by system organ class and preferred term.
# Source: ADAE (TRTEMFL), ADEX (TRTDURY), safety set.

#' @inheritParams t_14_2_1_301
t_14_3_2_301 <- function(adam, analysis, outputs) {
  s <- safety_set_301(adam)
  ae <- teae_301(adam)
  ex <- adam$ADEX[adam$ADEX$SAFFL == "Y", ]
  years <- stats::setNames(vapply(s$arms$label, function(l) sum(ex$TRTDURY[ex$TRTA == l], na.rm = TRUE), 0), s$arms$key)
  active <- s$arms$label[1]
  cells <- function(d) {
    out <- list()
    for (i in seq_len(nrow(s$arms))) {
      k <- s$arms$key[i]
      in_arm <- d$TRTA == s$arms$label[i]
      out[[paste0(k, "_n")]] <- npct_301(length(unique(d$USUBJID[in_arm])), s$arms$n[i])
      out[[paste0(k, "_rate")]] <- fmt_num(100 * sum(in_arm) / years[[k]], 1)
    }
    out
  }
  # Sorted by the number of patients on FJF-2026, then on all arms, then alphabetically.
  order_of <- function(d, by) {
    terms <- unique(d[[by]])
    n_active <- vapply(terms, function(t) length(unique(d$USUBJID[d[[by]] == t & d$TRTA == active])), 0L)
    n_all <- vapply(terms, function(t) length(unique(d$USUBJID[d[[by]] == t])), 0L)
    terms[order(-n_active, -n_all, terms)]
  }
  rows <- list(table_rows_301("Any TEAE", cells(ae)))
  for (soc in order_of(ae, "AEBODSYS")) {
    d <- ae[ae$AEBODSYS == soc, ]
    rows[[length(rows) + 1]] <- table_rows_301(tools::toTitleCase(tolower(soc)), cells(d))
    for (pt in order_of(d, "AEDECOD")) {
      rows[[length(rows) + 1]] <- table_rows_301(pt, cells(d[d$AEDECOD == pt, ]), 1L)
    }
  }
  columns <- unlist(lapply(seq_len(nrow(s$arms)), function(i) {
    stats::setNames(
      c(sprintf("%s|(N=%d)|n (%%)", s$arms$label[i], s$arms$n[i]), sprintf("|PY=%s|E/100 PY", fmt_num(years[[i]], 1))),
      paste0(s$arms$key[i], c("_n", "_rate"))
    )
  }))
  tlf_table_301("T-14.3.2", outputs, columns, do.call(bind_rows_301, rows),
    footnotes = c(
      "n (%): patients with at least one event. E/100 PY: events per 100 patient-years of exposure (PY, ADEX.TRTDURY).",
      "Sorted by the number of patients with the event on FJF-2026. MedDRA system organ class and preferred term."
    )
  )
}
