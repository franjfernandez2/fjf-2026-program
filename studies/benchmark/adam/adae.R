# ADAE for the CDISC pilot (CDISCPILOT01), derived from SDTM AE and our ADSL with admiral.
#
# Specification: the pilot's ADaM define.xml (ADR 0016). Readings of the specification are
# stated next to each derivation; differences from the official ADAE are recorded in
# studies/benchmark/discrepancy-log.csv. The official ADAE is used only for comparison.

#' Derive ADAE from SDTM AE and ADSL.
#'
#' @param sdtm Named list of SDTM domains from `read_sdtm()`; must contain `ae`.
#' @param adsl Our ADSL, from `derive_adsl()`.
#' @param spec Output of `read_define_v1()`.
#' @return ADAE with the specification's variables, order, labels and types, one record
#'   per adverse event, sorted by USUBJID and AESEQ.
derive_adae <- function(sdtm, adsl, spec) {
  adsl_vars <- admiral::exprs(
    STUDYID, SITEID,
    TRTA = TRT01A, TRTAN = TRT01AN, AGE, AGEGR1, AGEGR1N, RACE, RACEN, SEX, SAFFL, TRTSDT, TRTEDT
  )
  adae <- sdtm$ae |>
    dplyr::select(-STUDYID) |>
    admiral::derive_vars_merged(
      dataset_add = adsl, by_vars = admiral::exprs(USUBJID), new_vars = adsl_vars,
      check_type = "error"
    )
  if (anyNA(adae$TRTA)) stop("Adverse events of subjects who are not in ADSL.", call. = FALSE)

  # Dates. A missing start day is imputed as the 1st (ASTDTF = "D"). A start with no month
  # is not imputed: it is clearly before treatment. End dates are not imputed.
  adae <- adae |>
    admiral::derive_vars_dt(
      new_vars_prefix = "AST", dtc = AESTDTC,
      highest_imputation = "D", date_imputation = "first", flag_imputation = "date"
    ) |>
    admiral::derive_vars_dt(new_vars_prefix = "AEN", dtc = AEENDTC) |>
    # Study days have no day 0: a date before TRTSDT gives a negative day.
    admiral::derive_vars_dy(reference_date = TRTSDT, source_vars = admiral::exprs(ASTDT, AENDT)) |>
    dplyr::mutate(
      # As the define.xml says, also when ASTDT is imputed. The official ADAE leaves it
      # missing then (discrepancy D-008).
      ADURN = as.numeric(AENDT - ASTDT) + 1,
      ADURU = dplyr::if_else(!is.na(ADURN), "DAY", NA_character_),
      # Treatment emergent: starts on or after the first dose. An event with no start
      # date (only a year) is not treatment emergent.
      TRTEMFL = dplyr::if_else(!is.na(ASTDT) & !is.na(TRTSDT) & ASTDT >= TRTSDT, "Y", "N"),
      # Customised query for dermatologic events. The define.xml writes the SOC as
      # 'SKIN AND SUBC UTANEOUS TISSUE DISORDERS'; the space is a typing error.
      derm_term = stringr::str_detect(AEDECOD, "APPLICATION|DERMATITIS|ERYTHEMA|BLISTER"),
      derm_soc = AEBODSYS == "SKIN AND SUBCUTANEOUS TISSUE DISORDERS" &
        !AEDECOD %in% c("COLD SWEAT", "HYPERHIDROSIS", "ALOPECIA"),
      CQ01NAM = dplyr::if_else(derm_term | derm_soc, "DERMATOLOGIC EVENTS", NA_character_)
    )

  # First-occurrence flags: the first record of each group in the subset, ordered by
  # start date and sequence number. Records outside the subset get no flag.
  first_flag <- function(data, new_var, by_vars, subset) {
    admiral::restrict_derivation(
      data,
      derivation = admiral::derive_var_extreme_flag,
      args = admiral::params(
        by_vars = by_vars, order = admiral::exprs(ASTDT, AESEQ),
        new_var = !!new_var, mode = "first"
      ),
      filter = !!subset
    )
  }
  subj <- admiral::exprs(USUBJID)
  soc <- admiral::exprs(USUBJID, AEBODSYS)
  pt <- admiral::exprs(USUBJID, AEBODSYS, AEDECOD)
  teae <- rlang::expr(TRTEMFL == "Y")
  serious <- rlang::expr(TRTEMFL == "Y" & AESER == "Y")
  # AOCC01FL: the define.xml subsets to CQ01NAM = '' but describes the flag as the first
  # dermatologic event. The description is followed, as the official ADAE does (D-007).
  derm <- rlang::expr(TRTEMFL == "Y" & CQ01NAM == "DERMATOLOGIC EVENTS")
  adae <- adae |>
    first_flag(rlang::sym("AOCCFL"), subj, teae) |>
    first_flag(rlang::sym("AOCCSFL"), soc, teae) |>
    first_flag(rlang::sym("AOCCPFL"), pt, teae) |>
    first_flag(rlang::sym("AOCC02FL"), subj, serious) |>
    first_flag(rlang::sym("AOCC03FL"), soc, serious) |>
    first_flag(rlang::sym("AOCC04FL"), pt, serious) |>
    first_flag(rlang::sym("AOCC01FL"), subj, derm) |>
    dplyr::arrange(USUBJID, AESEQ)

  apply_spec_attributes(adae, spec, "ADAE")
}
