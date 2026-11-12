# SDTM VS of FJF-2026-301: blood pressure, pulse and weight by visit, and the height of the
# screening visit. Duplicates keep the first entry (CR01); weight in pounds is converted to kg
# (CR04); a height below 3 is taken as metres (CR05).

#' Map the raw vital signs and height of FJF-2026-301 to VS.
#'
#' @inheritParams map_lb_301
#' @return A list with the VS dataset and the data issues found while mapping.
map_vs_301 <- function(raw, raw_spec, dm, protocol, spec, se) {
  first <- keep_first_entry(raw$vital_signs, natural_key(raw_spec, "vital_signs"), "vital_signs")
  vs <- first$data
  tests <- purrr::keep(spec$vital_signs, \(x) x$code != "HEIGHT")
  long <- purrr::list_rbind(purrr::map(tests, \(x) {
    value <- vs[[x$column]]
    unit <- if (x$code == "WEIGHT") vs$weight_unit else rep(x$unit, nrow(vs))
    data.frame(
      subject_id = vs$subject_id, visit_code = vs$visit_code, date = vs$measure_date,
      code = x$code, name = x$name, orres = as.character(value), orresu = unit, std = x$unit
    )
  }))
  # Height is collected once, on the demography page of the screening visit.
  h <- purrr::keep(spec$vital_signs, \(x) x$code == "HEIGHT")[[1]]
  enr <- raw$enrolment
  dem <- raw$demography
  height <- as.numeric(dem$height_cm)
  long <- rbind(long, data.frame(
    subject_id = dem$subject_id, visit_code = "V1",
    date = enr$screening_date[match(dem$subject_id, enr$subject_id)], code = h$code, name = h$name,
    orres = as.character(dem$height_cm), orresu = ifelse(!is.na(height) & height < 3, "m", "cm"), std = h$unit
  ))
  long <- long[!is.na(long$orres) & trimws(long$orres) != "", ]
  value <- as.numeric(long$orres)
  stresn <- value
  lb_rows <- long$code == "WEIGHT" & long$orresu == "lb"
  stresn[lb_rows] <- round(value[lb_rows] / 2.20462, 1)
  m_rows <- long$code == "HEIGHT" & long$orresu == "m"
  stresn[m_rows] <- value[m_rows] * 100
  implausible <- long$code == "HEIGHT" & (stresn < 100 | stresn > 230)
  stresn[implausible] <- NA
  long$orresu[long$orresu == "lb"] <- "LB"
  issues <- rbind(first$issues, data.frame(
    rule = rep("CR05", sum(implausible)), table = rep("demography", sum(implausible)),
    subject_id = long$subject_id[implausible], record_id = rep(NA_integer_, sum(implausible)),
    detail = sprintf("height %s %s implausible after conversion", long$orres[implausible], long$orresu[implausible])
  ))
  usubjid <- sdtm_usubjid(spec$study, long$subject_id)
  dtc <- crf_to_iso(long$date)
  v <- planned_visit(long$visit_code, protocol)
  out <- data.frame(
    STUDYID = spec$study, DOMAIN = "VS", USUBJID = usubjid, VSTESTCD = long$code, VSTEST = long$name,
    VSORRES = long$orres, VSORRESU = long$orresu,
    VSSTRESC = ifelse(is.na(stresn), NA, format(stresn, trim = TRUE, drop0trailing = TRUE)),
    VSSTRESN = stresn, VSSTRESU = long$std,
    VSLOBXFL = last_before_exposure(usubjid, long$code, dtc, stresn, dm),
    VISITNUM = v$VISITNUM, VISIT = v$VISIT, EPOCH = epoch_of(usubjid, dtc, se), VSDTC = dtc,
    VSDY = sdtm_study_day(dtc, dm$RFSTDTC[match(usubjid, dm$USUBJID)])
  )
  out <- out[order(out$USUBJID, out$VSTESTCD, out$VISITNUM), ]
  out$VSSEQ <- sdtm_seq(out$USUBJID)
  list(vs = sdtm_conform(out, spec, "VS"), issues = issues)
}
