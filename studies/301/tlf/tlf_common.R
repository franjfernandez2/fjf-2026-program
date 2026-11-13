# Common pieces of the tables, figures and listings of FJF-2026-301 (docs/sap/fjf-2026-301-outputs.yml).
#
# A table is a list: id, title, population, columns (header labels, named by column key),
# rows (a data frame with `label`, `indent` and one character column per column key) and
# footnotes. write_table_301() prints it as fixed-width text; a figure is a ggplot saved as
# PNG. Until the database is locked the arms are the dummy assignment (ADR 0027) and every
# output says so.

#' The arms of an analysis set, in column order (FJF-2026 first, placebo last).
#'
#' @param adsl ADSL restricted to the analysis set.
#' @param var "TRT01P" (planned, efficacy) or "TRT01A" (actual, safety).
#' @return A data frame: key ("arm1", "arm2", ...), label, n.
arms_301 <- function(adsl, var) {
  num <- adsl[[paste0(var, "N")]]
  labels <- unique(adsl[[var]][order(-num)])
  data.frame(
    key = paste0("arm", seq_along(labels)), label = labels,
    n = vapply(labels, function(l) sum(adsl[[var]] == l), 0L), row.names = NULL
  )
}

#' Column headers of the arms, with their N, and optionally a total column.
#' @param arms Output of [arms_301()].
#' @param total Whether to add a total column.
arm_columns_301 <- function(arms, total = FALSE) {
  cols <- stats::setNames(sprintf("%s|(N=%d)", arms$label, arms$n), arms$key)
  if (total) cols <- c(cols, total = sprintf("Total|(N=%d)", sum(arms$n)))
  cols
}

#' Rows of a table: a label, its indent and one cell per column key.
#' @param label Row labels.
#' @param cells A named list of character vectors (one per column key), or a data frame.
#' @param indent Indent level of the rows.
table_rows_301 <- function(label, cells = list(), indent = 0L) {
  out <- data.frame(label = label, indent = as.integer(indent), stringsAsFactors = FALSE)
  for (k in names(cells)) out[[k]] <- as.character(cells[[k]])
  out
}

#' A heading row (no cells).
heading_301 <- function(label, indent = 0L) table_rows_301(label, indent = indent)

#' Bind rows of a table, filling missing cells with "".
bind_rows_301 <- function(...) {
  out <- dplyr::bind_rows(data.frame(label = character(), indent = integer()), ...)
  cells <- setdiff(names(out), c("label", "indent"))
  out[cells] <- lapply(out[cells], function(x) ifelse(is.na(x), "", x))
  out
}

#' Count and percentage, "n (x.x%)", "0" when the count is 0.
npct_301 <- function(n, denom) ifelse(denom == 0, "0", fmt_n_pct(as.integer(n), denom, digits = 1))

#' Summary statistics rows of a continuous variable by column.
#'
#' Means and medians carry one more decimal than the data, the SD two more.
#' @param x Values.
#' @param col Column key of each value.
#' @param keys Column keys, in order.
#' @param digits Decimals of the data.
#' @param indent Indent of the rows.
continuous_rows_301 <- function(x, col, keys, digits, indent = 1L) {
  stats_of <- function(v) {
    v <- v[!is.na(v)]
    if (length(v) == 0) {
      return(c(n = "0", mean = "", median = "", range = ""))
    }
    sd <- if (length(v) > 1) fmt_num(stats::sd(v), digits + 2) else "-"
    c(
      n = as.character(length(v)),
      mean = sprintf("%s (%s)", fmt_num(mean(v), digits + 1), sd),
      median = fmt_num(stats::median(v), digits + 1),
      range = sprintf("%s, %s", fmt_num(min(v), digits), fmt_num(max(v), digits))
    )
  }
  by_col <- lapply(keys, function(k) stats_of(x[col == k]))
  cells <- stats::setNames(lapply(seq_along(keys), function(i) by_col[[i]]), keys)
  table_rows_301(c("n", "Mean (SD)", "Median", "Min, max"), cells, indent)
}

#' Count rows of a categorical variable by column, as n (%) of each column's N.
#' @param x Values.
#' @param col Column key of each value.
#' @param keys Column keys.
#' @param denom Named denominators (one per key).
#' @param levels Levels in order, with their printed labels as names (default: sorted values).
#' @param indent Indent of the rows.
categorical_rows_301 <- function(x, col, keys, denom, levels = NULL, indent = 1L) {
  if (is.null(levels)) levels <- stats::setNames(sort(unique(x[!is.na(x)])), sort(unique(x[!is.na(x)])))
  cells <- stats::setNames(lapply(keys, function(k) {
    vapply(levels, function(l) npct_301(sum(x == l & col == k, na.rm = TRUE), denom[[k]]), "")
  }), keys)
  rows <- table_rows_301(names(levels), cells, indent)
  if (any(is.na(x))) {
    miss <- stats::setNames(lapply(keys, function(k) as.character(sum(is.na(x) & col == k))), keys)
    rows <- rbind(rows, table_rows_301("Missing", miss, indent))
  }
  rows
}

#' Column key of each record, from its arm label.
#' @param arm Arm labels of the records.
#' @param arms Output of [arms_301()].
arm_key_301 <- function(arm, arms) arms$key[match(arm, arms$label)]

#' Estimate with its confidence interval, "e (l, u)".
est_ci_301 <- function(est, lower, upper, digits) {
  sprintf("%s (%s, %s)", fmt_num(est, digits), fmt_num(lower, digits), fmt_num(upper, digits))
}

#' A p-value as printed: four decimals, "<0.0001" below that.
p_301 <- function(p) ifelse(is.na(p), "", ifelse(p < 0.0001, "<0.0001", fmt_p(p, 4)))

#' A table.
#' @param id Output id (e.g. "T-14.1.1").
#' @param outputs The outputs specification ([read_outputs()]); gives the title and population.
#' @param columns Named header labels; "|" splits a header over two lines.
#' @param rows Output of [table_rows_301()] / [bind_rows_301()].
#' @param footnotes Footnotes.
tlf_table_301 <- function(id, outputs, columns, rows, footnotes = character()) {
  spec <- output_spec_301(outputs, id)
  if (nrow(rows) == 0) rows <- table_rows_301("No records")
  missing <- setdiff(names(columns), names(rows))
  for (k in missing) rows[[k]] <- ""
  list(
    id = id, study = outputs$study, title = spec$title, population = spec$population, program = spec$program,
    columns = columns, rows = rows[, c("label", "indent", names(columns))], footnotes = footnotes
  )
}

#' The specification of one output.
output_spec_301 <- function(outputs, id) {
  spec <- purrr::keep(outputs$outputs, function(o) o$id == id)
  if (length(spec) != 1) stop("no single output ", id, " in the outputs specification")
  spec[[1]]
}

#' Printed name of an analysis set.
population_label_301 <- function(population) {
  c(FAS = "Full analysis set", SAF = "Safety set", PPS = "Per-protocol set")[[population]]
}

#' The footnote every output carries while the arms are the dummy assignment.
blinding_note_301 <- function(locked) {
  if (locked) {
    return(character())
  }
  "BLINDED: treatment arms are the dummy assignment of the blinded data review (ADR 0027); not study results."
}

#' Print a table as fixed-width text.
#'
#' @param table Output of [tlf_table_301()].
#' @param path File to write.
#' @param locked Whether the database is locked (otherwise the blinding note is added).
#' @return The path.
write_table_301 <- function(table, path, locked) {
  labels <- paste0(strrep("  ", table$rows$indent), table$rows$label)
  label_width <- max(nchar(labels), 30)
  heads <- strsplit(table$columns, "|", fixed = TRUE)
  depth <- max(lengths(heads))
  heads <- lapply(heads, function(h) c(rep("", depth - length(h)), h))
  keys <- names(table$columns)
  widths <- vapply(seq_along(keys), function(i) {
    max(nchar(c(heads[[i]], table$rows[[keys[i]]])), 6) + 2
  }, 0)
  pad_left <- function(x, w) formatC(x, width = w)
  line <- function(label, cells) {
    paste0(formatC(label, width = -label_width), paste(mapply(pad_left, cells, widths), collapse = ""))
  }
  total <- label_width + sum(widths)
  rule <- strrep("-", total)
  header <- vapply(seq_len(depth), function(d) line("", vapply(heads, `[`, "", d)), "")
  body <- vapply(seq_len(nrow(table$rows)), function(r) {
    line(labels[r], vapply(keys, function(k) table$rows[[k]][r], ""))
  }, "")
  text <- c(
    sprintf("%s%s%s", table$study, strrep(" ", max(1, total - nchar(table$study) - nchar(table$id))), table$id),
    sprintf("Table %s: %s", sub("^T-", "", table$id), table$title),
    sprintf("Population: %s", population_label_301(table$population)),
    rule, header, rule, body, rule,
    table$footnotes, blinding_note_301(locked),
    sprintf("Program: %s", table$program)
  )
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  writeLines(sub("\\s+$", "", text), path)
  path
}

#' Save a figure as PNG, with its id, title, population and footnotes.
#'
#' @param plot A ggplot.
#' @param id Output id.
#' @param outputs The outputs specification.
#' @param path File to write.
#' @param locked Whether the database is locked.
#' @param footnotes Footnotes.
#' @return The path.
save_figure_301 <- function(plot, id, outputs, path, locked, footnotes = character()) {
  spec <- output_spec_301(outputs, id)
  caption <- paste(c(
    footnotes, blinding_note_301(locked),
    sprintf("Population: %s. Program: %s", population_label_301(spec$population), spec$program)
  ), collapse = "\n")
  plot <- plot + ggplot2::labs(
    title = sprintf("Figure %s: %s", sub("^F-", "", id), spec$title), caption = caption
  ) + ggplot2::theme_bw(base_size = 10) +
    ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 0), legend.position = "bottom")
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  ggplot2::ggsave(path, plot, width = 9, height = 6, dpi = 150)
  path
}

#' File name of an output, e.g. "t_14_1_1.txt".
output_file_301 <- function(id, dir) {
  ext <- if (startsWith(id, "F-")) "png" else "txt"
  file.path(dir, sprintf("%s.%s", tolower(gsub("[-.]", "_", id)), ext))
}

#' Printed name of a strategy after withdrawal.
strategy_label_301 <- function(strategy) {
  unname(c(
    JR = "jump to reference", CR = "copy reference", MAR = "missing at random", MMRM = "MMRM"
  )[strategy])
}
