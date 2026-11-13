# T-14.1.1 Disposition of patients. Source: ADSL (randomised patients). Percentages are of
# the randomised patients of each column.

#' @param adam Named list of the ADaM datasets.
#' @param analysis Analysis results (unused).
#' @param outputs The outputs specification.
t_14_1_1_301 <- function(adam, analysis, outputs) {
  adsl <- adam$ADSL[adam$ADSL$RANDFL == "Y", ]
  arms <- arms_301(adsl, "TRT01P")
  key <- arm_key_301(adsl$TRT01P, arms)
  keys <- c(arms$key, "total")
  denom <- c(stats::setNames(arms$n, arms$key), total = nrow(adsl))
  count_row <- function(label, flag, indent = 0L) {
    cells <- stats::setNames(lapply(keys, function(k) {
      in_col <- if (k == "total") rep(TRUE, nrow(adsl)) else key == k
      npct_301(sum(flag & in_col, na.rm = TRUE), denom[[k]])
    }), keys)
    table_rows_301(label, cells, indent)
  }
  reasons <- function(status, reason, label) {
    stopped <- adsl[[status]] == "DISCONTINUED"
    found <- adsl[[reason]][stopped & !is.na(adsl[[reason]])]
    levels <- names(sort(table(found), decreasing = TRUE))
    bind_rows_301(
      count_row(label, stopped),
      do.call(rbind, lapply(levels, function(l) count_row(tools::toTitleCase(tolower(l)), adsl[[reason]] == l, 1L)))
    )
  }
  rows <- bind_rows_301(
    count_row("Randomised", rep(TRUE, nrow(adsl))),
    count_row("Treated (safety set)", adsl$SAFFL == "Y"),
    count_row("Full analysis set", adsl$FASFL == "Y"),
    count_row("Per-protocol set", adsl$PPROTFL == "Y"),
    count_row("Completed treatment", adsl$EOTSTT == "COMPLETED"),
    reasons("EOTSTT", "DCTREAS", "Discontinued treatment"),
    count_row("Completed study", adsl$EOSSTT == "COMPLETED"),
    reasons("EOSSTT", "DCSREAS", "Discontinued study")
  )
  tlf_table_301("T-14.1.1", outputs, arm_columns_301(arms, total = TRUE), rows,
    footnotes = c(
      "Percentages are of the patients randomised to each arm.", "Reasons are those recorded on the disposition page."
    )
  )
}
