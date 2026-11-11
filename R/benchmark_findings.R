# Gate 1 of the benchmark: every difference the pipeline finds must be explained in the
# discrepancy log (studies/benchmark/discrepancy-log.csv). These functions collect all the
# findings in one table and link each one to the log rows that explain it, so that the
# check is run, not read.

#' Read the benchmark's discrepancy log.
#' @param path Path of the log (CSV).
read_discrepancy_log <- function(path) {
  utils::read.csv(path, colClasses = "character")
}

#' Find the discrepancy-log rows that explain each finding.
#'
#' A log row explains a finding when it is `explained`, its `source` matches
#' `source_pattern`, it names the finding's dataset, and it names the finding's variable.
#' A finding about whole records (no variable) is explained by a row that names one of
#' the comparison's key variables, since a different key value makes two unmatched
#' records. A finding with no dataset, or no variable and no keys, is matched on the
#' source alone (e.g. a printed table row).
#' @param dataset,variable Character vectors, one element per finding; NA where unknown.
#' @param log Output of `read_discrepancy_log()`.
#' @param source_pattern Regular expression for the log's `source` column.
#' @param keys Key variables of the comparison.
#' @return A character vector: the ids of the explaining rows, separated by "; ", or "".
link_to_log <- function(dataset, variable, log, source_pattern, keys = character()) {
  log <- log[grepl(source_pattern, log$source) & log$status == "explained", , drop = FALSE]
  fields <- function(x) trimws(strsplit(x, ";", fixed = TRUE)[[1]])
  vapply(seq_along(dataset), function(i) {
    wanted <- if (is.na(variable[i])) keys else variable[i]
    hits <- vapply(seq_len(nrow(log)), function(j) {
      dataset_ok <- is.na(dataset[i]) || dataset[i] %in% fields(log$dataset[j])
      variable_ok <- length(wanted) == 0 || any(wanted %in% fields(log$variable[j]))
      dataset_ok && variable_ok
    }, logical(1))
    paste(log$id[hits], collapse = "; ")
  }, character(1))
}

#' Collect every finding of the benchmark and link it to the discrepancy log.
#'
#' @param adam Named list of `compare_with_reference()` results, one per dataset.
#' @param keys Named list of the key variables of each comparison.
#' @param xpt Output of `compare_xpt_with_official()`.
#' @param spec Rows of `check_against_spec()` for our datasets.
#' @param csr Named list of `compare_printed_rows()` results, named by table number.
#' @param qc Comparison of the independent ADSL with ours; any finding is unexplained.
#' @param log Output of `read_discrepancy_log()`.
#' @return A tibble with `comparison`, `dataset`, `item`, `issue`, `n` and `explained_by`
#'   ("" when no log row explains the finding).
collect_benchmark_findings <- function(adam, keys, xpt, spec, csr, qc, log) {
  empty <- tibble::tibble(
    comparison = character(), dataset = character(), item = character(),
    issue = character(), n = numeric(), explained_by = character()
  )
  official <- function(ds) sprintf("^%s vs official %s", ds, ds)

  adam_rows <- lapply(names(adam), function(ds) {
    f <- adam[[ds]]
    if (nrow(f) == 0) {
      return(empty)
    }
    tibble::tibble(
      comparison = "ADaM vs official", dataset = ds, item = f$variable, issue = f$issue, n = f$n,
      explained_by = link_to_log(rep(ds, nrow(f)), f$variable, log, official(ds), keys[[ds]])
    )
  })
  xpt_rows <- lapply(seq_len(nrow(xpt)), function(i) {
    ds <- xpt$dataset[i]
    tibble::tibble(
      comparison = "XPT vs official", dataset = ds, item = xpt$variable[i], issue = xpt$issue[i], n = xpt$n[i],
      explained_by = link_to_log(ds, xpt$variable[i], log, official(ds), keys[[ds]])
    )
  })
  spec_rows <- if (nrow(spec) == 0) {
    empty
  } else {
    tibble::tibble(
      comparison = "ADaM vs define.xml", dataset = spec$dataset, item = spec$variable,
      issue = paste0(spec$check, ": ", spec$found), n = NA_real_,
      explained_by = link_to_log(spec$dataset, spec$variable, log, "define[.]xml")
    )
  }
  csr_rows <- lapply(names(csr), function(table) {
    f <- csr[[table]]
    if (nrow(f) == 0) {
      return(empty)
    }
    tibble::tibble(
      comparison = "Output vs study report", dataset = paste("Table", table), item = f$label,
      issue = "printed values differ", n = 1,
      explained_by = link_to_log(
        rep(NA_character_, nrow(f)), rep(NA_character_, nrow(f)), log,
        sprintf("^Table %s vs study report", gsub(".", "[.]", table, fixed = TRUE))
      )
    )
  })
  qc_rows <- if (nrow(qc) == 0) {
    empty
  } else {
    tibble::tibble(
      comparison = "QC vs production", dataset = "ADSL", item = qc$variable, issue = qc$issue, n = qc$n,
      explained_by = ""
    )
  }
  dplyr::bind_rows(empty, adam_rows, xpt_rows, spec_rows, csr_rows, qc_rows)
}
