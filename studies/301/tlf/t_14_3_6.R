# T-14.3.6 Exposure to study drug. Source: ADEX (TRTDURD, TRTDURY, COMPLPCT), safety set.

#' @inheritParams t_14_2_1_301
t_14_3_6_301 <- function(adam, analysis, outputs) {
  s <- safety_set_301(adam)
  ex <- adam$ADEX[adam$ADEX$SAFFL == "Y", ]
  col <- arm_key_301(ex$TRTA, s$arms)
  keys <- s$arms$key
  years <- stats::setNames(lapply(keys, function(k) fmt_num(sum(ex$TRTDURY[col == k], na.rm = TRUE), 1)), keys)
  band <- cut(ex$COMPLPCT, c(-Inf, 80, 120, Inf), labels = c("low", "mid", "high"), right = FALSE)
  band <- ifelse(is.na(ex$COMPLPCT), NA, as.character(band))
  band[!is.na(ex$COMPLPCT) & ex$COMPLPCT == 120] <- "mid"
  rows <- bind_rows_301(
    heading_301("Duration of treatment (days)"),
    continuous_rows_301(ex$TRTDURD, col, keys, 0),
    table_rows_301("Total exposure (patient-years)", years),
    heading_301("Compliance (%)"),
    continuous_rows_301(ex$COMPLPCT, col, keys, 1),
    heading_301("Compliance category, n (%)"),
    categorical_rows_301(band, col, keys, s$denom, c("<80%" = "low", "80% to 120%" = "mid", ">120%" = "high"))
  )
  tlf_table_301("T-14.3.6", outputs, arm_columns_301(s$arms), rows,
    footnotes = c(
      "Duration: date of last dose minus date of first dose plus 1 (ADSL.TRTDURD).",
      "Compliance: 100 x tablets taken over days, for the bottles returned with a count (ADSL.COMPLPCT)."
    )
  )
}
