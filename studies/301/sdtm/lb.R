# SDTM LB of FJF-2026-301. Duplicates keep the first entry (CR01); results in another unit
# are converted to the standard unit (CR06); qualified results such as "<8" stay as text,
# with no numeric value (CR07).

#' Map the raw laboratory results of FJF-2026-301 to LB.
#'
#' @param raw Named list of the study's raw tables.
#' @param raw_spec The raw layer specification, for the natural key.
#' @param dm The DM dataset.
#' @param protocol The protocol design.
#' @param spec The SDTM specification.
#' @param se The SE dataset, for EPOCH.
#' @return A list with the LB dataset and the data issues found while mapping.
map_lb_301 <- function(raw, raw_spec, dm, protocol, spec, se) {
  first <- keep_first_entry(raw$lab_results, natural_key(raw_spec, "lab_results"), "lab_results")
  lb <- first$data
  tests <- spec$lab_tests
  t <- match(lb$test_code, purrr::map_chr(tests, "code"))
  if (anyNA(t)) stop("Laboratory tests not in the specification: ", toString(unique(lb$test_code[is.na(t)])))
  std_unit <- purrr::map_chr(tests, "unit")[t]
  # value * factor + offset; factor 1 for the standard unit, NA for a unit with no factor.
  factor <- offset <- rep(NA_real_, nrow(lb))
  same <- lb$unit == std_unit
  factor[same] <- 1
  offset[same] <- 0
  for (i in seq_along(tests)) {
    for (cv in tests[[i]]$convert) {
      rows <- t == i & lb$unit == cv$from
      factor[rows] <- cv$factor
      offset[rows] <- cv$offset
    }
  }
  num <- suppressWarnings(as.numeric(lb$result))
  stresn <- round(num * factor + offset, 4)
  lo <- round(suppressWarnings(as.numeric(lb$ref_low)) * factor + offset, 4)
  hi <- round(suppressWarnings(as.numeric(lb$ref_high)) * factor + offset, 4)
  stresc <- ifelse(is.na(num), lb$result, ifelse(is.na(stresn), NA, format(stresn, trim = TRUE, drop0trailing = TRUE)))
  nrind <- ifelse(is.na(stresn), NA, ifelse(!is.na(lo) & stresn < lo, "LOW",
    ifelse(!is.na(hi) & stresn > hi, "HIGH", "NORMAL")
  ))
  unknown <- !is.na(num) & is.na(factor)
  issues <- rbind(first$issues, data.frame(
    rule = rep("CR06", sum(unknown)), table = rep("lab_results", sum(unknown)),
    subject_id = lb$subject_id[unknown], record_id = lb$record_id[unknown],
    detail = sprintf("%s in %s: no conversion factor", lb$test_code[unknown], lb$unit[unknown])
  ))
  usubjid <- sdtm_usubjid(spec$study, lb$subject_id)
  dtc <- crf_to_iso(lb$collection_date)
  v <- planned_visit(lb$visit_code, protocol)
  out <- data.frame(
    STUDYID = spec$study, DOMAIN = "LB", USUBJID = usubjid, LBREFID = as.character(lb$record_id),
    LBTESTCD = purrr::map_chr(tests, \(x) if (is.null(x$testcd)) x$code else x$testcd)[t],
    LBTEST = purrr::map_chr(tests, "name")[t],
    LBCAT = purrr::map_chr(tests, "category")[t], LBORRES = lb$result, LBORRESU = lb$unit,
    LBORNRLO = lb$ref_low, LBORNRHI = lb$ref_high, LBSTRESC = stresc, LBSTRESN = stresn,
    LBSTRESU = std_unit, LBSTNRLO = lo, LBSTNRHI = hi, LBNRIND = nrind,
    LBFAST = ifelse(lb$fasting == "Y", "Y", NA),
    LBLOBXFL = last_before_exposure(usubjid, lb$test_code, dtc, stresn, dm),
    VISITNUM = v$VISITNUM, VISIT = v$VISIT, EPOCH = epoch_of(usubjid, dtc, se), LBDTC = dtc,
    LBDY = sdtm_study_day(dtc, dm$RFSTDTC[match(usubjid, dm$USUBJID)])
  )
  out <- out[order(out$USUBJID, out$LBTESTCD, out$VISITNUM, out$LBDTC), ]
  out$LBSEQ <- sdtm_seq(out$USUBJID)
  list(lb = sdtm_conform(out, spec, "LB"), issues = issues)
}
