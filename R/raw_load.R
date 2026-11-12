# Loading the raw data files (data/raw/<study>/*.csv) into the database. This is the
# data-management side: it reads files and a manifest, and never the simulator.

#' Read and verify the raw data files of the programme.
#'
#' Every file listed in the manifest must exist, have its hash and row count, and have
#' exactly the columns of the raw-layer specification. Values are read with the types of
#' the specification; empty fields are missing.
#'
#' @param dir Directory with `manifest.csv` and one folder per study.
#' @param spec The result of [read_raw_spec()].
#' @return A named list by study ("201", "301") of named lists of data frames.
read_raw_files <- function(dir, spec) {
  manifest <- utils::read.csv(file.path(dir, "manifest.csv"), colClasses = "character")
  types <- c(text = "character", integer = "integer", numeric = "numeric")
  out <- list()
  for (i in seq_len(nrow(manifest))) {
    f <- file.path(dir, manifest$file[i])
    if (!file.exists(f)) cli::cli_abort("Missing raw file {.file {manifest$file[i]}}.")
    hash <- digest::digest(file = f, algo = "sha256")
    if (!identical(hash, manifest$sha256[i])) {
      cli::cli_abort("Hash of {.file {manifest$file[i]}} does not match the manifest.")
    }
    study <- dirname(manifest$file[i])
    table <- sub("[.]csv$", "", basename(manifest$file[i]))
    t <- purrr::keep(spec$tables, \(x) x$name == table)
    if (length(t) != 1) cli::cli_abort("{.file {manifest$file[i]}} is not a table of the specification.")
    cols <- purrr::map_chr(t[[1]]$columns, "name")
    classes <- types[purrr::map_chr(t[[1]]$columns, "type")]
    d <- utils::read.csv(f,
      colClasses = unname(classes), na.strings = "", encoding = "UTF-8", check.names = FALSE
    )
    if (!identical(names(d), cols)) {
      cli::cli_abort("Columns of {.file {manifest$file[i]}} differ from the specification.")
    }
    if (nrow(d) != as.integer(manifest$rows[i])) cli::cli_abort("Row count of {.file {manifest$file[i]}} differs.")
    out[[study]][[table]] <- d
  }
  out
}

#' Load one study's raw tables into its schema, as the loader role.
#'
#' The load is one transaction and refuses to run if any table of the schema already has
#' rows: raw data are loaded once, and later changes go through audited updates.
#'
#' @param con A connection as `fjf_loader` ([db_connect()] with role "loader").
#' @param schema "raw_201" or "raw_301".
#' @param tables Named list of data frames, as one study of [read_raw_files()].
#' @return The number of rows loaded per table, invisibly.
load_raw_study <- function(con, schema, tables) {
  counts <- vapply(names(tables), function(t) {
    as.integer(DBI::dbGetQuery(con, sprintf("SELECT count(*) AS n FROM %s.%s", schema, t))$n)
  }, integer(1))
  if (any(counts > 0)) cli::cli_abort("{.val {schema}} already has data: raw data are loaded once.")
  DBI::dbWithTransaction(con, {
    for (t in names(tables)) {
      DBI::dbAppendTable(con, DBI::Id(schema = schema, table = t), tables[[t]])
    }
  })
  invisible(vapply(tables, nrow, integer(1)))
}
