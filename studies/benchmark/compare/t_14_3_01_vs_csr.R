# Table 14-3.01 against the pilot's study report, page 49.

#' Compare our Table 14-3.01 with the printed table, row by row and cell by cell.
#' @param table Output of `make_t_14_3_01()`.
#' @param csr_path Path of the pilot's study report (PDF).
#' @return Mismatches, from `compare_printed_rows()`.
compare_t_14_3_01 <- function(table, csr_path) {
  printed <- read_printed_table(
    pdftools::pdf_text(csr_path)[49], "\\(N=74\\)",
    value_pattern = "^(-?[0-9.]+( \\(.*\\))?|\\(.*\\))$", join_wrapped = FALSE
  )
  compare_printed_rows(table, printed)
}
