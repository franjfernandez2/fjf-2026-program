# Clinical study reports (studies/<study>/csr.qmd). The report reads one pipeline target,
# csr_<study>_data, and embeds the tables and figures the pipeline made, so its numbers are
# the outputs' numbers. While a study is blinded (ADR 0027, ADR 0032) the arms are the dummy
# assignment: the report says so on its first page and in every results chapter, and the
# discussion waits for the unblinding. After an authorised unblinding the pipeline rebuilds
# the outputs and renders the report again, with no change to its text.

#' A table of the pipeline as a Markdown pipe table, with its footnotes.
#'
#' @param table A table ([tlf_table_301()]).
#' @return Markdown text, to print with `results: asis`.
csr_table <- function(table) {
  esc <- function(x) gsub("|", "\\|", x, fixed = TRUE)
  indent <- vapply(table$rows$indent, function(i) strrep("&nbsp;&nbsp;&nbsp;", i), "")
  heading <- table$rows$indent == 0 & rowSums(table$rows[names(table$columns)] != "") == 0
  label <- paste0(indent, ifelse(heading, paste0("**", esc(table$rows$label), "**"), esc(table$rows$label)))
  heads <- vapply(strsplit(table$columns, "|", fixed = TRUE), function(h) paste(h[nzchar(h)], collapse = "<br>"), "")
  cells <- vapply(names(table$columns), function(k) esc(table$rows[[k]]), character(nrow(table$rows)))
  if (is.null(dim(cells))) cells <- matrix(cells, nrow = nrow(table$rows))
  line <- function(x) paste0("| ", paste(x, collapse = " | "), " |")
  body <- vapply(seq_len(nrow(table$rows)), function(r) line(c(label[r], cells[r, ])), "")
  c(
    sprintf("**Table %s: %s** (%s)", sub("^T-", "", table$id), table$title, population_label_301(table$population)),
    "",
    line(c("", heads)),
    line(c(":---", rep("---:", length(heads)))),
    body,
    "",
    if (length(table$footnotes) > 0) paste0("<small>", paste(esc(table$footnotes), collapse = "<br>"), "</small>"),
    ""
  )
}

#' A figure of the pipeline as Markdown.
#'
#' @param id Output id (e.g. "F-14.2.1").
#' @param files The paths the pipeline wrote (tlf_<study>_files).
#' @param root Project root, to make the path absolute.
#' @return Markdown text.
csr_figure <- function(id, files, root) {
  path <- files[basename(files) == basename(output_file_301(id, "."))]
  if (length(path) != 1) stop("the pipeline wrote no single file for ", id)
  c(sprintf("![](%s){width=100%%}", file.path(root, path)), "")
}

#' The blinding status of a study as the report prints it.
#'
#' @param unblinded Whether the pipeline used the real assignment (unblinded_<study>).
#' @return A list: `blinded`, `banner` (first-page callout) and `results_note` (callout at the
#'   start of each results chapter).
csr_blinding <- function(unblinded) {
  if (isTRUE(unblinded)) {
    return(list(blinded = FALSE, banner = character(), results_note = character()))
  }
  callout <- function(title, text) {
    c(sprintf("::: {.callout-warning title=\"%s\"}", title), text, ":::", "")
  }
  list(
    blinded = TRUE,
    banner = callout(
      "BLINDED REPORT",
      paste(
        "The database is locked but the study is not unblinded (ADR 0032). Every result by arm",
        "uses the dummy assignment of the blinded data review (ADR 0027): the arms are the real",
        "ones shuffled within stratum. No number by arm in this report is a study result."
      )
    ),
    results_note = callout(
      "Dummy arms",
      "The arms below are the dummy assignment. These numbers check the programs; they are not study results."
    )
  )
}

#' The discussion chapter: written by the study team after the unblinding.
#'
#' @inheritParams csr_blinding
#' @return Markdown text.
csr_discussion <- function(unblinded) {
  if (isTRUE(unblinded)) {
    text <- paste(
      "The study is unblinded. The discussion and conclusions are written by the study team from",
      "the results above, and any analysis not in the SAP is labelled post hoc."
    )
  } else {
    text <- paste(
      "**Pending the unblinding.** The discussion and the conclusions are written only after the",
      "reviewer authorises the unblinding (ADR 0032) and the results above use the real arms."
    )
  }
  c(text, "")
}

#' Second-programming summary of a study: one row per level.
#'
#' @param sdtm,adam,tlf The comparison targets (sdtm_, adam_, tlf_<study>_qc_comparison).
#' @return A data frame: level, units, compared, clean.
csr_qc_summary <- function(sdtm, adam, tlf) {
  data.frame(
    level = c("SDTM", "ADaM", "Tables"),
    units = c(
      sprintf("%d domains", nrow(sdtm)), sprintf("%d datasets", nrow(adam)), sprintf("%d cells", nrow(tlf$cells))
    ),
    compared = c(
      sprintf("%s records", format(sum(sdtm$rows), big.mark = ",")),
      sprintf("%s records", format(sum(adam$rows), big.mark = ",")),
      sprintf("%d tables", length(unique(tlf$cells$id)))
    ),
    clean = c(all(sdtm$clean), all(adam$clean), isTRUE(tlf$clean))
  )
}

#' A data frame as a Markdown pipe table.
#'
#' @param d A data frame of character or numeric columns.
#' @param header Column headers (default: the names).
#' @return Markdown text.
csr_markdown <- function(d, header = names(d)) {
  line <- function(x) paste0("| ", paste(gsub("|", "\\|", x, fixed = TRUE), collapse = " | "), " |")
  c(line(header), line(rep("---", ncol(d))), apply(d, 1, function(r) line(as.character(r))), "")
}

#' Render a report with Quarto into an output folder.
#'
#' @param qmd The report source.
#' @param out_dir Output folder (created if needed).
#' @param data The report's data target; passed so that the report is rendered again when it
#'   changes (the report reads it from the pipeline store).
#' @return The path of the HTML file.
render_csr <- function(qmd, out_dir, data) {
  force(data)
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  out <- file.path(out_dir, sub("[.]qmd$", ".html", basename(qmd)))
  unlink(out)
  log <- system2("quarto", c("render", qmd, "--output-dir", normalizePath(out_dir)), stdout = TRUE, stderr = TRUE)
  if (!file.exists(out)) stop("quarto did not render ", qmd, ":\n", paste(utils::tail(log, 20), collapse = "\n"))
  out
}
