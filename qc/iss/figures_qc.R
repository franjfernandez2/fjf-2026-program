# Second programming of the figures of the integrated safety analysis by treatment (independent of
# studies/iss/tlf/iss_treatment.R). The data each figure plots are recomputed with base R from the
# ADaM datasets of each study, by the dose actually taken, and compared with the data file the
# production program writes next to the figure (qc/301/figures_qc.R).

#' Comparison of F-ISS-1 and F-ISS-2.
#'
#' @param files Paths written by [write_tlf_iss_treatment()].
#' @param adam_201 ADaM datasets of FJF-2026-201.
#' @param adam_301 ADaM datasets of FJF-2026-301.
#' @return A list: `cells` and `clean`, as [compare_tlf_301()].
qc_figures_iss_treatment <- function(files, adam_201, adam_301) {
  # F-ISS-1: percentage of the patients of FJF-2026-201 with each safety measure, by dose.
  saf <- adam_201$ADSL[adam_201$ADSL$SAFFL == "Y", c("USUBJID", "TRT01AN")]
  ae <- adam_201$ADAE[adam_201$ADAE$SAFFL == "Y" & adam_201$ADAE$TRTEMFL %in% "Y", ]
  hypo <- adam_201$ADHYPO[adam_201$ADHYPO$SAFFL == "Y" & adam_201$ADHYPO$ONTRTFL %in% "Y", ]
  measures <- list(
    "Any TEAE" = ae$USUBJID,
    "TEAE leading to discontinuation of study drug" = ae$USUBJID[ae$AEACN %in% "DRUG WITHDRAWN"],
    "Level 2 or 3 hypoglycaemia" = hypo$USUBJID[as.integer(hypo$HYPOLVL) >= 2]
  )
  points <- do.call(rbind, lapply(names(measures), function(m) {
    do.call(rbind, lapply(sort(unique(saf$TRT01AN)), function(dose) {
      who <- saf$USUBJID[saf$TRT01AN == dose]
      x <- sum(who %in% measures[[m]])
      data.frame(measure = m, dose = dose, n = length(who), x = x, pct = 100 * x / length(who))
    }))
  }))
  first <- qc_figure_compare("F-ISS-1", points, qc_figure_found(files, "F-ISS-1"), c("measure", "dose"))

  # F-ISS-2: peak of ALT or AST and peak of total bilirubin, as multiples of the ULN, of each patient.
  lb <- rbind(
    adam_201$ADLB[, c("USUBJID", "SAFFL", "PARAMCD", "AVAL", "ANRHI", "ONTRTFL")],
    adam_301$ADLB[, c("USUBJID", "SAFFL", "PARAMCD", "AVAL", "ANRHI", "ONTRTFL")]
  )
  lb <- lb[lb$SAFFL %in% "Y" & lb$ONTRTFL %in% "Y" & lb$PARAMCD %in% c("ALT", "AST", "BILI"), ]
  lb <- lb[!is.na(lb$AVAL) & !is.na(lb$ANRHI) & lb$ANRHI > 0, ]
  lb$x <- lb$AVAL / lb$ANRHI
  peak <- function(codes) {
    d <- lb[lb$PARAMCD %in% codes, ]
    stats::setNames(vapply(split(d$x, d$USUBJID), max, 0), names(split(d$x, d$USUBJID)))
  }
  at <- peak(c("ALT", "AST"))
  bili <- peak("BILI")
  who <- intersect(names(at), names(bili))
  second <- qc_figure_compare(
    "F-ISS-2", data.frame(USUBJID = who, AT = unname(at[who]), BILI = unname(bili[who])),
    qc_figure_found(files, "F-ISS-2"), "USUBJID"
  )
  cells <- rbind(first, second)
  list(cells = cells, clean = all(cells$match))
}
