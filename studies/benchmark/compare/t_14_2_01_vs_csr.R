# Table 14-2.01 against the pilot's study report, pages 46-48.

#' Compare our Table 14-2.01 with the printed table, row by row and cell by cell.
#' @param table Output of `make_t_14_2_01()`.
#' @param csr_path Path of the pilot's study report (PDF).
#' @return Mismatches, from `compare_printed_rows()`.
compare_t_14_2_01 <- function(table, csr_path) {
  printed <- read_printed_table(pdftools::pdf_text(csr_path)[46:48], "\\(N=254\\)")
  cells <- c("placebo", "low", "high", "total", "p_value")
  ours <- tibble::tibble(
    label = table$statistic,
    values = lapply(seq_len(nrow(table)), function(i) {
      v <- unlist(table[i, cells])
      unname(v[nzchar(v)])
    })
  )
  compare_printed_rows(ours, printed)
}
