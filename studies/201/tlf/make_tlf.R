# Makes every table and figure of docs/sap/fjf-2026-201-outputs.yml. Each output's program is
# the file the specification names (studies/201/tlf/<id>.R), with one function
# <id>_201(adam, analysis, outputs[, path, locked]).

#' The functions that make the outputs, by output id, listed by name so that targets sees
#' them as dependencies.
tlf_functions_201 <- function() {
  list(
    `T-14.1.1` = t_14_1_1_201, `T-14.1.2` = t_14_1_2_201,
    `T-14.2.1` = t_14_2_1_201, `T-14.2.2` = t_14_2_2_201, `F-14.2.1` = f_14_2_1_201, `T-14.2.3` = t_14_2_3_201,
    `T-14.3.1` = t_14_3_1_201, `T-14.3.2` = t_14_3_2_201, `T-14.3.3` = t_14_3_3_201,
    `F-14.3.1` = f_14_3_1_201, `T-14.3.4` = t_14_3_4_201, `F-14.3.2` = f_14_3_2_201,
    `T-14.3.5` = t_14_3_5_201, `T-14.3.6` = t_14_3_6_201
  )
}

#' The function that makes an output.
#' @param id Output id.
tlf_function_201 <- function(id) {
  f <- tlf_functions_201()[[id]]
  if (is.null(f)) stop("no program for output ", id)
  f
}

#' Every table of the specification.
#'
#' @param adam Named list of the ADaM datasets.
#' @param analysis Analysis results: mcpmod, cui, sap, protocol.
#' @param outputs The outputs specification.
#' @return A named list of tables ([tlf_table_301()]).
tables_201 <- function(adam, analysis, outputs) {
  ids <- purrr::map_chr(purrr::keep(outputs$outputs, function(o) o$type == "table"), "id")
  stats::setNames(lapply(ids, function(id) tlf_function_201(id)(adam, analysis, outputs)), ids)
}

#' Write every output: the tables as text, the figures as PNG.
#'
#' @param tables Output of [tables_201()].
#' @inheritParams tables_201
#' @param locked Whether the database is locked.
#' @param dir Output directory.
#' @return The paths written.
write_tlf_201 <- function(tables, adam, analysis, outputs, locked, dir) {
  unlink(dir, recursive = TRUE)
  vapply(outputs$outputs, function(o) {
    path <- output_file_301(o$id, dir)
    if (o$type == "figure") {
      tlf_function_201(o$id)(adam, analysis, outputs, path, locked)
    } else {
      write_table_301(tables[[o$id]], path, locked)
    }
  }, "")
}
