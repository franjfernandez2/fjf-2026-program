# Analysis input of FJF-2026-301: the ADaM datasets in the shape the SAP's analysis code reads
# (R/efficacy_301.R, R/secondary_301.R), the same shape as the dry run's. The analyses
# themselves are the F2 functions, unchanged.

#' Analysis data of FJF-2026-301 from its ADaM datasets.
#'
#' @param adam Named list of the ADaM datasets (ADSL, ADEFF, ADTTE).
#' @return A list with `adsl` (full analysis set: USUBJID, ARMCD, STRATUM, AGE, SEX, BMIBL,
#'   WEIGHTBL), `adeff` (HbA1c and weight, one row per patient and scheduled visit) and
#'   `adtte` (time to first rescue).
analysis_data_301 <- function(adam) {
  adsl <- adam$ADSL[adam$ADSL$FASFL == "Y", ]
  tte <- adam$ADTTE[adam$ADTTE$PARAMCD == "TTRESC" & adam$ADTTE$FASFL == "Y", ]
  list(
    adsl = data.frame(
      USUBJID = adsl$USUBJID, ARMCD = adsl$ARMCD, STRATUM = adsl$STRATAR, AGE = adsl$AGE, SEX = adsl$SEX,
      BMIBL = adsl$BMIBL, WEIGHTBL = adsl$WEIGHTBL
    ),
    adeff = rbind(adeff_analysis_301(adam$ADEFF, "HBA1C"), adeff_analysis_301(adam$ADEFF, "WEIGHT")),
    adtte = data.frame(
      USUBJID = tte$USUBJID, ARMCD = tte$ARMCD, STRATUM = tte$STRATAR, PARAMCD = tte$PARAMCD, AVAL = tte$AVAL,
      CNSR = tte$CNSR
    )
  )
}
