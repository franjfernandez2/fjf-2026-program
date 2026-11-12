# SDTM SV of FJF-2026-301: one record per planned visit (done or not) and per unscheduled
# visit. The nominal visit is kept as collected; analysis windows are ADaM's (CR03).

#' Map the raw visits of FJF-2026-301 to SV.
#'
#' @param raw Named list of the study's raw tables.
#' @param dm The DM dataset, for the reference start date.
#' @param protocol The protocol design, for the visit order and labels.
#' @param spec The SDTM specification.
#' @return The SV dataset.
map_sv_301 <- function(raw, dm, protocol, spec) {
  v <- raw$visits
  plan <- protocol$visits
  v$USUBJID <- sdtm_usubjid(spec$study, v$subject_id)
  v$SVSTDTC <- ifelse(v$visit_status == "DONE", crf_to_iso(v$visit_date), NA)
  v$planned <- match(v$visit_code, plan$code)
  v$VISITNUM <- v$planned
  v$VISIT <- toupper(plan$label[v$planned])
  # Unscheduled visits: the last planned visit done before them, plus 0.1, 0.2, ...
  uns <- which(is.na(v$planned))
  for (i in uns) {
    same <- v$USUBJID == v$USUBJID[i] & !is.na(v$planned) & !is.na(v$SVSTDTC) & v$SVSTDTC <= v$SVSTDTC[i]
    v$VISITNUM[i] <- if (any(same)) max(v$planned[same]) else 0
  }
  if (length(uns) > 0) {
    key <- paste(v$USUBJID[uns], v$VISITNUM[uns])
    o <- order(key, v$SVSTDTC[uns])
    step <- integer(length(uns))
    step[o] <- stats::ave(seq_along(o), key[o], FUN = seq_along)
    v$VISITNUM[uns] <- v$VISITNUM[uns] + step / 10
    v$VISIT[uns] <- sprintf("UNSCHEDULED %.1f", v$VISITNUM[uns])
  }
  v$SVPRESP <- ifelse(is.na(v$planned), NA, "Y")
  v$SVOCCUR <- ifelse(is.na(v$planned), NA, ifelse(v$visit_status == "DONE", "Y", "N"))
  v$SVENDTC <- v$SVSTDTC
  ref <- dm$RFSTDTC[match(v$USUBJID, dm$USUBJID)]
  v$SVSTDY <- sdtm_study_day(v$SVSTDTC, ref)
  v$SVENDY <- v$SVSTDY
  v$STUDYID <- spec$study
  v$DOMAIN <- "SV"
  v <- v[order(v$USUBJID, v$VISITNUM), ]
  sdtm_conform(v, spec, "SV")
}
