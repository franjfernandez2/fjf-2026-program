# Blinded pooled safety review of FJF-2026-201 and FJF-2026-301 (docs/sap/fjf-2026-iss-outputs.yml,
# ADR 0033). The safety datasets of the two studies are stacked, and the treatment variables are
# replaced by the study: each patient appears once in their study's column and once in the
# pooled column. No column is a treatment arm, so the tables say nothing about FJF-2026 against
# placebo and stay valid while the studies are blinded (ADR 0032). Each table is made by the
# FJF-2026-301 program named in the specification (`source`).

#' Treatment variables replaced by the study in the pooled datasets.
iss_treatment_vars <- c("TRT01P", "TRT01A", "TRTP", "TRTA")

#' Pool the safety datasets of several studies over their arms.
#'
#' @param adam_by_study Named list (by study, e.g. "201") of named lists of ADaM datasets.
#' @param outputs The ISS outputs specification (gives the datasets and the pooled label).
#' @return A named list of ADaM datasets with the study, then the pooled label, in place of the
#'   treatment variables (and their numeric codes, which order the columns: pooled first).
pool_iss <- function(adam_by_study, outputs) {
  studies <- names(adam_by_study)
  pooled <- outputs$pooled_label
  datasets <- unlist(outputs$adam_datasets)
  subjects <- unlist(lapply(adam_by_study, function(a) a$ADSL$USUBJID))
  if (anyDuplicated(subjects)) stop("a USUBJID is in more than one study")
  relabel <- function(d, label, code) {
    for (v in intersect(iss_treatment_vars, names(d))) {
      d[[v]] <- label
      d[[paste0(v, "N")]] <- code
    }
    d
  }
  stats::setNames(lapply(datasets, function(ds) {
    parts <- lapply(seq_along(studies), function(i) {
      d <- adam_by_study[[studies[i]]][[ds]]
      if (is.null(d)) stop("no ", ds, " for study ", studies[i])
      d$STUDYID <- sprintf("FJF-2026-%s", studies[i])
      d
    })
    by_study <- lapply(seq_along(parts), function(i) relabel(parts[[i]], parts[[i]]$STUDYID[1], i))
    all <- lapply(parts, relabel, pooled, length(parts) + 1)
    as.data.frame(dplyr::bind_rows(c(by_study, all)))
  }), datasets)
}

#' The FJF-2026-301 programs behind the ISS tables, by their FJF-2026-301 output id, listed by
#' name so that targets sees them as dependencies.
iss_source_functions <- function() {
  list(
    `T-14.1.2` = t_14_1_2_301, `T-14.3.1` = t_14_3_1_301, `T-14.3.2` = t_14_3_2_301,
    `T-14.3.3` = t_14_3_3_301, `T-14.3.4` = t_14_3_4_301, `T-14.3.5` = t_14_3_5_301,
    `T-14.3.6` = t_14_3_6_301
  )
}

#' The outputs specification of FJF-2026-301 with the ISS titles, so that the 301 programs find
#' their own ids.
#' @param outputs The ISS outputs specification.
iss_as_301_outputs <- function(outputs) {
  list(study = outputs$study, outputs = lapply(outputs$outputs, function(o) {
    list(id = o$source, title = o$title, population = o$population, program = o$program, type = o$type)
  }))
}

#' Footnotes of the 301 programs that speak of arms, replaced for the pooled tables.
#' @param footnotes Footnotes of a 301 table.
iss_footnotes <- function(footnotes) {
  footnotes <- sub(
    "Sorted by the number of patients with the event on FJF-2026.",
    "Sorted by the number of patients in the pooled column.", footnotes,
    fixed = TRUE
  )
  footnotes <- sub("The stratum is the one used at randomisation.", "", footnotes, fixed = TRUE)
  c(
    trimws(footnotes[nzchar(trimws(footnotes))]),
    paste(
      "Blinded pooled review: columns are the studies and all patients, every arm included (placebo too);",
      "no result is by treatment (ADR 0032, ADR 0033)."
    ),
    "FJF-2026-201 lasts 12 weeks and FJF-2026-301 26 weeks: compare rates per patient-year, not percentages."
  )
}

#' Every table of the ISS specification.
#'
#' @param adam Output of [pool_iss()].
#' @param outputs The ISS outputs specification.
#' @return A named list of tables ([tlf_table_301()]), by ISS id.
tables_iss <- function(adam, outputs) {
  as_301 <- iss_as_301_outputs(outputs)
  fns <- iss_source_functions()
  tables <- lapply(outputs$outputs, function(o) {
    f <- fns[[o$source]]
    if (is.null(f)) stop("no FJF-2026-301 program for ", o$source)
    table <- f(adam, NULL, as_301)
    # The pooled column already holds every patient: a total column would count them twice.
    keep <- setdiff(names(table$columns), "total")
    table$columns <- table$columns[keep]
    table$rows <- table$rows[, c("label", "indent", keep)]
    table$id <- o$id
    table$footnotes <- iss_footnotes(table$footnotes)
    table
  })
  stats::setNames(tables, purrr::map_chr(outputs$outputs, "id"))
}

#' Write the ISS tables as text.
#'
#' The tables carry no arm, so they get the pooled-review footnote and not the dummy-arm note.
#' @param tables Output of [tables_iss()].
#' @param dir Output directory.
#' @return The paths written.
write_tlf_iss <- function(tables, dir) {
  unlink(dir, recursive = TRUE)
  vapply(tables, function(t) write_table_301(t, output_file_301(t$id, dir), locked = TRUE), "", USE.NAMES = FALSE)
}
