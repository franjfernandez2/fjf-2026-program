# Makes every table and figure of docs/sap/fjf-2026-301-outputs.yml. Each output's program is
# the file the specification names (studies/301/tlf/<id>.R), with one function
# <id>_301(adam, analysis, outputs[, path, locked]).

#' The functions that make the outputs, by output id. They are listed by name, not looked up,
#' so that targets sees them as dependencies and rebuilds an output when its program changes.
tlf_functions_301 <- function() {
  list(
    `T-14.1.1` = t_14_1_1_301, `T-14.1.2` = t_14_1_2_301,
    `T-14.2.1` = t_14_2_1_301, `T-14.2.2` = t_14_2_2_301, `T-14.2.3` = t_14_2_3_301,
    `F-14.2.1` = f_14_2_1_301, `F-14.2.2` = f_14_2_2_301,
    `T-14.2.4` = t_14_2_4_301, `T-14.2.5` = t_14_2_5_301, `T-14.2.6` = t_14_2_6_301,
    `F-14.2.3` = f_14_2_3_301, `T-14.2.7` = t_14_2_7_301, `F-14.2.4` = f_14_2_4_301,
    `T-14.3.1` = t_14_3_1_301, `T-14.3.2` = t_14_3_2_301, `T-14.3.3` = t_14_3_3_301,
    `F-14.3.1` = f_14_3_1_301, `T-14.3.4` = t_14_3_4_301, `F-14.3.2` = f_14_3_2_301,
    `T-14.3.5` = t_14_3_5_301, `T-14.3.6` = t_14_3_6_301
  )
}

#' The function that makes an output.
#' @param id Output id.
tlf_function_301 <- function(id) {
  f <- tlf_functions_301()[[id]]
  if (is.null(f)) stop("no program for output ", id)
  f
}

#' Every table of the specification.
#'
#' @param adam Named list of the ADaM datasets.
#' @param analysis Analysis results: efficacy, secondary, multiplicity, lsmeans, sap, protocol.
#' @param outputs The outputs specification.
#' @return A named list of tables ([tlf_table_301()]).
tables_301 <- function(adam, analysis, outputs) {
  ids <- purrr::map_chr(purrr::keep(outputs$outputs, function(o) o$type == "table"), "id")
  stats::setNames(lapply(ids, function(id) tlf_function_301(id)(adam, analysis, outputs)), ids)
}

#' Write every output: the tables as text, the figures as PNG.
#'
#' @param tables Output of [tables_301()].
#' @inheritParams tables_301
#' @param locked Whether the database is locked.
#' @param dir Output directory.
#' @return The paths written.
write_tlf_301 <- function(tables, adam, analysis, outputs, locked, dir) {
  unlink(dir, recursive = TRUE)
  vapply(outputs$outputs, function(o) {
    path <- output_file_301(o$id, dir)
    if (o$type == "figure") {
      tlf_function_301(o$id)(adam, analysis, outputs, path, locked)
    } else {
      write_table_301(tables[[o$id]], path, locked)
    }
  }, "")
}
