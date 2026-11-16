# Outputs of a study (docs/sap/fjf-2026-NNN-outputs.yml) and their traceability to the SAP.

#' Read a study's outputs file.
#'
#' @param path Path to the YAML file.
#' @return The parsed list.
read_outputs <- function(path) yaml::read_yaml(path)

#' SAP items that an output can implement: analysis sets, the primary analysis or analyses,
#' sensitivity, key secondary, exploratory, safety, and the multiplicity and subgroup sections.
#'
#' @param sap The result of [read_sap()].
#' @return A character vector of ids.
sap_output_targets <- function(sap) {
  primary <- if (!is.null(sap$primary)) list(sap$primary)
  items <- c(sap$analysis_sets, primary, sap$analyses, sap$sensitivity, sap$secondary, sap$exploratory, sap$safety)
  ids <- purrr::map_chr(items, "id")
  c(ids, if (!is.null(sap$multiplicity)) "multiplicity", if (!is.null(sap$subgroups)) "subgroups")
}

#' Check the outputs against the SAP.
#'
#' Every output has a unique id, a known population and SAP item, ADaM references of the
#' form DATASET.VARIABLE with a declared dataset, and a program under studies/<study>/tlf/.
#' Every analysis, sensitivity, key secondary, exploratory and safety item, the multiplicity and the
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

#' The rows of the traceability matrix for the outputs of one study or analysis.
#'
#' One row per output, in the columns of `docs/traceability-matrix.csv`. The shell of an output is
#' its entry in the outputs specification, which fixes layout, population and variables. An output
#' is `match` when its second programming exists and every cell it compared is equal, and
#' `not started` when it has none (figures, and tables the second programming does not cover):
#' the matrix does not claim a check that was not made.
#'
#' @param outputs The result of [read_outputs()].
#' @param study Study as printed ("FJF-2026-301", ...).
#' @param qc_program Path of the second programming of the study's tables.
#' @param comparison The comparison of the second programming (`cells` with `id` and `match`), or NULL.
#' @param plan What an output without a `sap` entry implements (the blinded pooled review: its ADR).
#' @return A data frame in the columns of the matrix.
traceability_rows <- function(outputs, study, qc_program, comparison = NULL, plan = "") {
  compared <- if (is.null(comparison)) character() else unique(comparison$cells$id)
  clean <- vapply(compared, function(id) all(comparison$cells$match[comparison$cells$id == id]), TRUE)
  rows <- lapply(outputs$outputs, function(o) {
    refs <- unlist(o$adam)
    covered <- o$id %in% compared
    data.frame(
      output_id = o$id, study = study, type = o$type, title = o$title,
      sap_section = if (length(o$sap) == 0) plan else paste(unlist(o$sap), collapse = "; "), population = o$population,
      adam_datasets = paste(unique(sub("[.].*", "", refs)), collapse = "; "),
      key_variables = paste(sub("^[A-Z]+[.]", "", refs), collapse = "; "),
      program = o$program, qc_program = if (covered) qc_program else "",
      qc_status = if (!covered) "not started" else if (clean[[o$id]]) "match" else "mismatch",
      shell_id = o$id, stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

#' Write the traceability matrix: the benchmark rows already in the file, then the rows given.
#'
#' @param rows Data frame in the columns of the matrix (studies and analyses of the programme).
#' @param path Path of the CSV.
#' @return The path.
write_traceability <- function(rows, path) {
  old <- if (file.exists(path)) utils::read.csv(path, colClasses = "character") else NULL
  keep <- if (is.null(old)) NULL else old[old$study == "CDISCPILOT01", ]
  out <- rbind(keep, rows[, names(rows) %in% names(keep) | is.null(keep)])
  utils::write.csv(out, path, row.names = FALSE, na = "")
  path
}
