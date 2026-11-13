# T-14.3.4 Laboratory shifts from baseline to the worst value on treatment.
# Source: ADLB (WORSTFL records: BNRIND at baseline, ANRIND of the worst value), safety set.
# HbA1c is an efficacy parameter and is not shown.

#' @inheritParams t_14_2_1_301
t_14_3_4_301 <- function(adam, analysis, outputs) {
  s <- safety_set_301(adam)
  lb <- adam$ADLB[adam$ADLB$SAFFL == "Y" & adam$ADLB$WORSTFL %in% "Y" & adam$ADLB$PARAMCD != "HBA1C", ]
  levels <- c(Low = "LOW", Normal = "NORMAL", High = "HIGH")
  params <- unique(lb[order(lb$PARAMCD), c("PARAMCD", "PARAM")])
  rows <- list()
  for (p in seq_len(nrow(params))) {
    rows[[length(rows) + 1]] <- heading_301(params$PARAM[p])
    for (i in seq_len(nrow(s$arms))) {
      d <- lb[lb$PARAMCD == params$PARAMCD[p] & lb$TRTA == s$arms$label[i], ]
      rows[[length(rows) + 1]] <- heading_301(sprintf("%s (n=%d)", s$arms$label[i], nrow(d)), 1L)
      base <- ifelse(is.na(d$BNRIND), "MISSING", d$BNRIND)
      shown <- c(levels, if (any(base == "MISSING")) c(Missing = "MISSING"))
      for (b in names(shown)) {
        in_row <- base == shown[[b]]
        cells <- lapply(levels, function(w) as.character(sum(in_row & d$ANRIND == w)))
        cells$total <- as.character(sum(in_row))
        cells <- stats::setNames(cells, c(tolower(names(levels)), "total"))
        rows[[length(rows) + 1]] <- table_rows_301(sprintf("Baseline %s", tolower(b)), cells, 2L)
      }
    }
  }
  tlf_table_301("T-14.3.4", outputs,
    c(low = "Worst on|treatment:|low", normal = "||normal", high = "||high", total = "||Total"),
    do.call(bind_rows_301, rows),
    footnotes = c(
      "Patients with a value on treatment. Worst value: the one furthest outside its reference range,",
      "as a multiple of the limit crossed; with none outside, the last on treatment (ADR 0028).",
      "Low, normal, high: against the reference range."
    )
  )
}
