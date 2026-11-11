# SAS transport (XPT v5) files of the benchmark ADaM, written with xportr from the
# specification: types, lengths, labels, order and formats come from the define.xml.

#' Write a derived ADaM dataset to XPT with its specification's attributes.
#' @param data The derived dataset.
#' @param metacore The specification as a metacore object (`define_v1_to_metacore()`).
#' @param dataset Dataset name, e.g. `"ADSL"`.
#' @param dir Folder to write to.
#' @return The path of the XPT file, for a file target.
write_adam_xpt <- function(data, metacore, dataset, dir) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(dir, paste0(tolower(dataset), ".xpt"))
  spec <- metacore |> metacore::select_dataset(dataset, verbose = "silent")
  data |>
    xportr::xportr_metadata(spec, domain = dataset, verbose = "message") |>
    xportr::xportr_type() |>
    xportr::xportr_length() |>
    xportr::xportr_label() |>
    xportr::xportr_order() |>
    xportr::xportr_format() |>
    xportr::xportr_df_label() |>
    xportr::xportr_write(path)
  path
}

#' Read our XPT files back and compare each with the official XPT of the pilot.
#'
#' Shows that writing to XPT changes nothing: the differences are the ones already logged
#' for the datasets in memory.
#' @param paths Paths returned by `write_adam_xpt()`.
#' @param pilot_files Paths of the downloaded pilot files.
#' @return The findings of `compare_with_reference()`, with a `dataset` column.
compare_xpt_with_official <- function(paths, pilot_files) {
  keys <- list(
    adsl = "USUBJID", adae = c("USUBJID", "AESEQ"), adtte = c("USUBJID", "PARAMCD"),
    adqsadas = c("USUBJID", "PARAMCD", "AVISIT", "ADT")
  )
  rows <- lapply(paths, function(path) {
    name <- sub("[.]xpt$", "", basename(path))
    ours <- admiral::convert_blanks_to_na(haven::read_xpt(path))
    official <- haven::read_xpt(pilot_file(pilot_files, sprintf("adam/%s.xpt", name)))
    findings <- compare_with_reference(ours, official, keys = keys[[name]])
    tibble::add_column(findings, dataset = toupper(name), .before = 1)
  })
  do.call(rbind, rows)
}
