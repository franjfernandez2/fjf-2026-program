# Analysis input of FJF-2026-201: the HbA1c records of ADEFF in the shape the SAP's analysis
# code reads (R/mcpmod_201.R), the same shape as the dry run's. The analyses themselves are the
# F2 functions, unchanged. Until the lock the arms are the dummy assignment (ADR 0027).

#' Analysis data of FJF-2026-201 from its ADEFF.
#'
#' @param adeff ADEFF of FJF-2026-201.
#' @param protocol The protocol of FJF-2026-201 ([read_protocol()]); gives each arm's dose.
#' @return A data frame, one row per patient of the full analysis set and scheduled
#'   post-baseline HbA1c visit: USUBJID, ARMCD, DOSE, STRATUM, PARAMCD, BASE, AVISITN, AVISIT,
#'   AVAL, CHG, ONTRTFL and PRERESFL (Y or N).
analysis_data_201 <- function(adeff, protocol) {
  d <- adeff_analysis_301(adeff, "HBA1C")
  dose <- stats::setNames(purrr::map_dbl(protocol$arms, "dose_mg"), purrr::map_chr(protocol$arms, "code"))
  if (!all(d$ARMCD %in% names(dose))) stop("an arm code of ADEFF is not in the protocol")
  d$DOSE <- unname(dose[d$ARMCD])
  d[c("USUBJID", "ARMCD", "DOSE", setdiff(names(d), c("USUBJID", "ARMCD", "DOSE")))]
}
