# Helpers for tables: number formats as the pilot's study report prints them, and a reader
# of the report's printed tables, so that our outputs can be checked against it.

#' Format a number with fixed decimals, rounding as SAS does.
#' @param x Numeric vector.
#' @param digits Decimal places.
fmt_num <- function(x, digits) {
  ifelse(is.na(x), "", formatC(round_sas(x, digits), format = "f", digits = digits))
}

#' Format a count and its percentage as the pilot's report does; a zero count is "0".
#'
#' Whole percentages are printed with one space after the bracket: "14 ( 16%)",
#' "8 ( 9%)", "1 ( <1%)" and "86 (100%)". Percentages with decimals are right-aligned in
#' four characters: "2 ( 2.3%)", "65 (75.6%)".
#' @param n Counts.
#' @param denom Denominators.
#' @param digits Decimals of the percentage: 0 or 1.
fmt_n_pct <- function(n, denom, digits = 0) {
  pct <- round_sas(100 * n / denom, digits)
  shown <- formatC(pct, format = "f", digits = digits)
  if (digits == 0) {
    shown[n > 0 & pct == 0] <- "<1"
    cell <- ifelse(pct >= 100, sprintf("%d (%s%%)", n, shown), sprintf("%d ( %s%%)", n, shown))
  } else {
    cell <- sprintf("%d (%4s%%)", n, shown)
  }
  ifelse(n == 0, "0", cell)
}

#' Format a p-value with a fixed number of decimals.
#' @param p P-values.
#' @param digits Decimal places.
fmt_p <- function(p, digits = 4) {
  ifelse(is.na(p), "", formatC(round_sas(p, digits), format = "f", digits = digits))
}

#' Read a table printed in a PDF study report into rows.
#'
#' Each printed line is split into fields at runs of two or more spaces. Leading fields
#' that do not look like values are the row's labels; the rest are its values. A line
#' with a label and no values continues the label of the row above it (long terms wrap).
#' Lines before the column header (`header_end`) and from the footnotes on are ignored.
#' @param text Page texts, e.g. from `pdftools::pdf_text()`.
#' @param header_end Regular expression matching the last line of the column header.
#' @return A tibble with one row per printed row: `page`, `indent` (leading spaces),
#'   `labels` (a list of label fields) and `values` (a list of value fields).
read_printed_table <- function(text, header_end) {
  value_pattern <- paste0(
    "^(", "\\d+(\\.\\d+)?\\*?", "|", "\\d+ \\( *<?\\d+(\\.\\d+)?%\\)( \\[\\d+\\])?",
    "|", "[<>]\\d*\\.\\d+\\*?", ")$"
  )
  rows <- list()
  for (page in seq_along(text)) {
    lines <- strsplit(text[[page]], "\n", fixed = TRUE)[[1]]
    start <- grep(header_end, lines)
    end <- grep("^\\s*(\\[1\\]|Note:|NOTE:|Source:)", lines)
    if (length(start) == 0) next
    body <- lines[seq_len(if (length(end) > 0) min(end) - 1 else length(lines))]
    body <- body[-seq_len(start[1])]
    for (line in body) {
      if (!nzchar(trimws(line))) next
      fields <- strsplit(trimws(line), "\\s{2,}")[[1]]
      is_value <- grepl(value_pattern, fields)
      first_value <- if (any(is_value)) which(is_value)[1] else length(fields) + 1
      labels <- fields[seq_len(first_value - 1)]
      values <- if (first_value <= length(fields)) fields[first_value:length(fields)] else character()
      if (length(values) == 0 && length(rows) > 0 && length(labels) == 1) {
        last <- rows[[length(rows)]]
        n <- length(last$labels[[1]])
        last$labels[[1]][n] <- paste(last$labels[[1]][n], labels)
        rows[[length(rows)]] <- last
        next
      }
      rows[[length(rows) + 1]] <- tibble::tibble(
        page = page, indent = nchar(line) - nchar(sub("^\\s+", "", line)),
        labels = list(labels), values = list(values)
      )
    }
  }
  do.call(rbind, rows)
}

#' Compare our rows of a table with the rows printed in the report, in order.
#'
#' @param ours A tibble with `label` (the row label as printed) and `values` (a list of
#'   the non-empty printed cells, left to right).
#' @param printed Output of `read_printed_table()`.
#' @param label_of How to get the printed label from a printed row's label fields.
#' @return A tibble with one row per mismatch: `row`, `label`, `ours`, `printed`. No rows
#'   means every row matches, in the same order.
compare_printed_rows <- function(ours, printed, label_of = function(x) utils::tail(x, 1)) {
  n <- max(nrow(ours), nrow(printed))
  rows <- lapply(seq_len(n), function(i) {
    ours_label <- if (i <= nrow(ours)) ours$label[i] else NA_character_
    ours_values <- if (i <= nrow(ours)) paste(ours$values[[i]], collapse = " | ") else NA_character_
    printed_label <- if (i <= nrow(printed)) label_of(printed$labels[[i]]) else NA_character_
    printed_values <- if (i <= nrow(printed)) paste(printed$values[[i]], collapse = " | ") else NA_character_
    if (identical(ours_label, printed_label) && identical(ours_values, printed_values)) {
      return(NULL)
    }
    tibble::tibble(
      row = i, label = dplyr::coalesce(ours_label, printed_label),
      ours = paste(ours_label, "::", ours_values), printed = paste(printed_label, "::", printed_values)
    )
  })
  result <- do.call(rbind, rows)
  if (is.null(result)) {
    result <- tibble::tibble(row = integer(), label = character(), ours = character(), printed = character())
  }
  result
}
