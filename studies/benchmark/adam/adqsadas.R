# ADQSADAS for the CDISC pilot (CDISCPILOT01): ADAS-Cog items and the ADAS-Cog(11) total,
# derived from SDTM QS and our ADSL with admiral.
#
# Specification: the pilot's ADaM define.xml (ADR 0016). SAP sections: 8.2 (assessment
# windows), 10.1.1 (primary analysis, LOCF at week 24) and 14.2 (missing items).
# The official ADQSADAS is used only for comparison.

# Assessment windows for ADAS-Cog (SAP section 8.2), in days from the first dose.
adas_windows <- function() {
  tibble::tibble(
    AVISIT = c("Baseline", "Week 8", "Week 16", "Week 24"),
    AVISITN = c(0, 8, 16, 24),
    AWRANGE = c("<=1", "2-84", "85-140", ">140"),
    AWTARGET = c(1, 56, 112, 168),
    AWLO = c(NA, 2, 85, 141),
    AWHI = c(1, 84, 140, NA),
    AWU = "DAYS"
  )
}

# The 11 items of the ADAS-Cog(11) total and the maximum score of each (SAP section 14.2
# and the protocol's scale): the maxima add up to 70.
adas11_items <- function() {
  tibble::tibble(
    QSTESTCD = sprintf("ACITM%02d", c(1, 2, 4, 5, 6, 7, 8, 11, 12, 13, 14)),
    max_score = c(10, 5, 5, 5, 5, 8, 12, 5, 5, 5, 5)
  )
}

#' ADAS-Cog(11) total from its items (SAP section 14.2).
#'
#' Missing if more than 30% of the 11 items (4 or more) are missing. Otherwise the sum of
#' the items present, scaled to the full 0-70 range: sum * 70 / (70 - maximum of the
#' missing items).
#' @param items Item records: USUBJID, VISITNUM, QSDTC, QSTESTCD, QSSTRESN.
#' @return One row per assessment with `AVAL`.
adas11_total <- function(items) {
  scale <- adas11_items()
  grid <- items |>
    dplyr::distinct(USUBJID, VISITNUM, QSDTC) |>
    tidyr::crossing(scale) |>
    dplyr::left_join(items, by = c("USUBJID", "VISITNUM", "QSDTC", "QSTESTCD"))
  grid |>
    dplyr::summarise(
      n_missing = sum(is.na(QSSTRESN)),
      missing_max = sum(max_score[is.na(QSSTRESN)]),
      total = sum(QSSTRESN, na.rm = TRUE),
      .by = c(USUBJID, VISITNUM, QSDTC)
    ) |>
    dplyr::mutate(
      AVAL = dplyr::if_else(n_missing > 3, NA_real_, total * 70 / (70 - missing_max))
    ) |>
    dplyr::select(USUBJID, VISITNUM, QSDTC, AVAL)
}

#' Derive ADQSADAS from SDTM QS and ADSL.
#'
#' @param sdtm Named list of SDTM domains from `read_sdtm()`; must contain `qs`.
#' @param adsl Our ADSL, from `derive_adsl()`.
#' @param spec Output of `read_define_v1()`.
#' @return ADQSADAS with the specification's variables, order, labels and types.
derive_adqsadas <- function(sdtm, adsl, spec) {
  qs <- sdtm$qs |> dplyr::filter(QSCAT == "ALZHEIMER'S DISEASE ASSESSMENT SCALE")
  items <- qs |>
    dplyr::filter(QSTESTCD != "ACTOT") |>
    dplyr::mutate(PARAMCD = QSTESTCD, AVAL = QSSTRESN)

  # ADAS-Cog(11) total, derived from the items as the define.xml says, not copied from
  # the total that QS also holds. QSSEQ is QS's for the baseline total only.
  qs_total <- qs |> dplyr::filter(QSTESTCD == "ACTOT")
  totals <- adas11_total(items) |>
    dplyr::left_join(
      dplyr::select(qs_total, USUBJID, VISITNUM, QSDTC, VISIT, QSBLFL, QSSEQ),
      by = c("USUBJID", "VISITNUM", "QSDTC")
    ) |>
    dplyr::mutate(
      STUDYID = qs$STUDYID[1], PARAMCD = "ACTOT",
      QSSEQ = dplyr::if_else(QSBLFL %in% "Y", QSSEQ, NA_real_)
    )

  adqs <- dplyr::bind_rows(
    dplyr::select(items, STUDYID, USUBJID, PARAMCD, AVAL, VISIT, VISITNUM, QSDTC, QSBLFL, QSSEQ),
    dplyr::select(totals, STUDYID, USUBJID, PARAMCD, AVAL, VISIT, VISITNUM, QSDTC, QSBLFL, QSSEQ)
  ) |>
    dplyr::select(-STUDYID) |>
    admiral::derive_vars_merged(
      dataset_add = adsl, by_vars = admiral::exprs(USUBJID), check_type = "error",
      new_vars = admiral::exprs(
        STUDYID, SITEID, SITEGR1, TRTSDT, TRTEDT,
        TRTP = TRT01P, TRTPN = TRT01PN,
        AGE, AGEGR1, AGEGR1N, RACE, RACEN, SEX, ITTFL, EFFFL, COMP24FL
      )
    ) |>
    dplyr::mutate(
      PARAM = codelist_lookup(spec, "PARAMCD_ADQSADAS", PARAMCD, from = "code"),
      PARAMN = as.numeric(codelist_lookup(spec, "PARAMN_ADQSADAS", PARAM, from = "decode")),
      ABLFL = dplyr::if_else(QSBLFL %in% "Y", "Y", NA_character_)
    ) |>
    admiral::derive_vars_dt(new_vars_prefix = "A", dtc = QSDTC) |>
    admiral::derive_vars_dy(reference_date = TRTSDT, source_vars = admiral::exprs(ADT))

  # Windows by study day (SAP 8.2). The record closest to the target day is analysed;
  # of two equally close, the earlier one.
  windows <- adas_windows()
  adqs <- adqs |>
    dplyr::mutate(
      AVISITN = dplyr::case_when(ADY <= 1 ~ 0, ADY <= 84 ~ 8, ADY <= 140 ~ 16, ADY > 140 ~ 24)
    ) |>
    dplyr::left_join(windows, by = "AVISITN") |>
    dplyr::mutate(AWTDIFF = abs(AWTARGET - ADY)) |>
    admiral::derive_var_extreme_flag(
      by_vars = admiral::exprs(USUBJID, PARAMCD, AVISITN),
      order = admiral::exprs(AWTDIFF, ADY), new_var = ANL01FL, mode = "first"
    ) |>
    admiral::derive_var_base(by_vars = admiral::exprs(USUBJID, PARAMCD), source_var = AVAL, new_var = BASE)

  # LOCF for the total (SAP 10.1.1): a window with no assessment takes the analysed
  # assessment of the last window before it, with that assessment's date and visit.
  analysed_total <- adqs |> dplyr::filter(PARAMCD == "ACTOT", ANL01FL == "Y")
  expected <- tidyr::crossing(
    dplyr::distinct(analysed_total, USUBJID),
    PARAMCD = "ACTOT", dplyr::filter(windows, AVISITN > 0)
  )
  keep <- admiral::exprs(
    STUDYID, SITEID, SITEGR1, TRTSDT, TRTEDT, TRTP, TRTPN, AGE, AGEGR1, AGEGR1N, RACE, RACEN,
    SEX, ITTFL, EFFFL, COMP24FL, PARAM, PARAMN, VISIT, VISITNUM, ADT, ADY, BASE
  )
  locf <- admiral::derive_locf_records(
    analysed_total,
    dataset_ref = expected,
    by_vars = admiral::exprs(USUBJID, PARAMCD),
    order = admiral::exprs(AVISITN),
    keep_vars = keep
  ) |>
    dplyr::filter(DTYPE %in% "LOCF") |>
    dplyr::mutate(AWTDIFF = abs(AWTARGET - ADY), ANL01FL = "Y", ABLFL = NA_character_, QSSEQ = NA_real_)

  adqs <- dplyr::bind_rows(adqs, locf) |>
    dplyr::mutate(
      CHG = dplyr::if_else(ABLFL %in% "Y", NA_real_, AVAL - BASE),
      PCHG = dplyr::if_else(BASE != 0, 100 * CHG / BASE, NA_real_)
    ) |>
    dplyr::arrange(USUBJID, PARAMN, AVISITN, ADT, DTYPE)

  apply_spec_attributes(adqs, spec, "ADQSADAS")
}
