# Table 14-5.01 against the pilot's study report, pages 63-78.

#' Compare our Table 14-5.01 with the printed table, row by row and cell by cell.
#'
#' The report wraps long terms onto a second line and cuts them: system organ classes at
#' 40 characters ("CONGENITAL, FAMILIAL AND GENETIC DISORDE"), preferred terms at 38
#' ("ELECTROCARDIOGRAM ST SEGMENT DEPRESSIO"). Labels are compared on that many characters.
#' @param table Output of `make_t_14_5_01()`.
#' @param csr_path Path of the pilot's study report (PDF).
#' @return Mismatches, from `compare_printed_rows()`.
compare_t_14_5_01 <- function(table, csr_path) {
  printed <- read_printed_table(pdftools::pdf_text(csr_path)[63:78], "Preferred Term")
  cells <- c("placebo", "low", "high", "p_low", "p_high")
  ours <- tibble::tibble(
    label = ifelse(is.na(table$term), substr(table$soc, 1, 40), substr(table$term, 1, 38)),
    values = lapply(seq_len(nrow(table)), function(i) {
      v <- unlist(table[i, cells])
      unname(v[nzchar(v)])
    })
  )
  compare_printed_rows(ours, printed, label_of = function(x) paste(x, collapse = " "))
}
