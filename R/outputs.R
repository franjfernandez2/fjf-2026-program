# Outputs of a study (docs/sap/fjf-2026-NNN-outputs.yml) and their traceability to the SAP.

#' Read a study's outputs file.
#'
#' @param path Path to the YAML file.
#' @return The parsed list.
read_outputs <- function(path) yaml::read_yaml(path)

#' SAP items that an output can implement: analysis sets, analyses, sensitivity, key
#' secondary, safety, and the multiplicity and subgroup sections.
#'
#' @param sap The result of [read_sap()].
#' @return A character vector of ids.
sap_output_targets <- function(sap) {
  items <- c(sap$analysis_sets, sap$analyses, sap$sensitivity, sap$secondary, sap$safety)
  ids <- purrr::map_chr(items, "id")
  c(ids, if (!is.null(sap$multiplicity)) "multiplicity", if (!is.null(sap$subgroups)) "subgroups")
}

#' Check the outputs against the SAP.
#'
#' Every output has a unique id, a known population and SAP item, ADaM references of the
#' form DATASET.VARIABLE with a declared dataset, and a program under studies/<study>/tlf/.
#' Every analysis, sensitivity, key secondary and safety item, the multiplicity and the
#' subgroups have at least one output.
#'
#' @param outputs The result of [read_outputs()].
#' @param sap The result of [read_sap()].
#' @return A character vector of problems; empty when consistent.
check_outputs <- function(outputs, sap) {
  problems <- character()
  outs <- outputs$outputs
  ids <- purrr::map_chr(outs, "id")
  if (anyDuplicated(ids)) problems <- c(problems, sprintf("duplicated output %s", unique(ids[duplicated(ids)])))
  sets <- purrr::map_chr(sap$analysis_sets, "id")
  targets <- sap_output_targets(sap)
  study_dir <- sub("^FJF-2026-", "", outputs$study)
  for (o in outs) {
    if (!o$population %in% sets) problems <- c(problems, sprintf("%s: unknown population %s", o$id, o$population))
    unknown <- setdiff(unlist(o$sap), targets)
    if (length(o$sap) == 0 || length(unknown) > 0) {
      problems <- c(problems, sprintf("%s: unknown SAP item %s", o$id, paste(unknown, collapse = ", ")))
    }
    refs <- unlist(o$adam)
    bad <- refs[!grepl("^[A-Z]+\\.[A-Z0-9]+$", refs) | !sub("\\..*", "", refs) %in% outputs$adam_datasets]
    if (length(bad) > 0) {
      problems <- c(problems, sprintf("%s: bad ADaM reference %s", o$id, paste(bad, collapse = ", ")))
    }
    if (!startsWith(o$program, sprintf("studies/%s/tlf/", study_dir))) {
      problems <- c(problems, sprintf("%s: program outside studies/%s/tlf/", o$id, study_dir))
    }
  }
  covered <- unique(unlist(purrr::map(outs, "sap")))
  must <- setdiff(targets, purrr::map_chr(sap$analysis_sets, "id"))
  uncovered <- setdiff(must, covered)
  if (length(uncovered) > 0) problems <- c(problems, sprintf("no output for %s", paste(uncovered, collapse = ", ")))
  problems
}

#' The traceability matrix: one row per output and ADaM variable.
#'
#' @param outputs The result of [read_outputs()].
#' @return A tibble: output, title, sap, adam, program.
traceability_301 <- function(outputs) {
  purrr::map_dfr(outputs$outputs, function(o) {
    tibble::tibble(
      output = o$id, title = o$title, sap = paste(unlist(o$sap), collapse = ", "),
      adam = if (length(o$adam) == 0) NA_character_ else unlist(o$adam), program = o$program
    )
  })
}
