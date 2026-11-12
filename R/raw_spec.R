# Specification of the raw data layer (docs/specs/raw-layer.yml) and the DDL written from it.

raw_column_types <- c(text = "text", integer = "integer", numeric = "numeric")

#' Read the raw-layer specification.
#'
#' @param path Path to the YAML file.
#' @return The parsed list.
read_raw_spec <- function(path) yaml::read_yaml(path)

#' Codelists of one study: the shared ones plus its arms and visit codes.
#'
#' Unscheduled visits are coded "UNS".
#'
#' @param spec The result of [read_raw_spec()].
#' @param protocol The result of [read_protocol()].
#' @return A named list of character vectors.
raw_codelists <- function(spec, protocol) {
  c(
    lapply(spec$codelists, as.character),
    list(arm = purrr::map_chr(protocol$arms, "code"), visit = c(protocol$visits$code, "UNS"))
  )
}

#' The columns of the specification as one table.
#'
#' @param spec The result of [read_raw_spec()].
#' @return A tibble: table, column, type, nullable, codelist, description.
raw_columns <- function(spec) {
  purrr::map_dfr(spec$tables, function(t) {
    purrr::map_dfr(t$columns, function(c) {
      tibble::tibble(
        table = t$name, column = c$name, type = c$type, nullable = isTRUE(c$nullable),
        codelist = c$codelist %||% NA_character_, description = c$description %||% NA_character_
      )
    })
  })
}

#' Check the raw-layer specification.
#'
#' Tables and columns are unique snake_case names; every table starts with record_id and
#' subject_id, both required; types are known; every codelist is defined; the natural key
#' names existing columns; and every protocol assessment that produces data has a table.
#'
#' @param spec The result of [read_raw_spec()].
#' @param protocols A list of results of [read_protocol()], one per study.
#' @return A character vector of problems; empty when consistent.
check_raw_spec <- function(spec, protocols) {
  found <- new.env()
  found$problems <- character()
  add <- function(cond, msg) if (isTRUE(cond)) found$problems <- c(found$problems, msg)
  tables <- purrr::map_chr(spec$tables, "name")
  add(anyDuplicated(tables) > 0, "duplicated table names")
  cols <- raw_columns(spec)
  snake <- "^[a-z][a-z0-9_]*$"
  add(any(!grepl(snake, c(tables, cols$column))), "table and column names must be snake_case")
  add(any(!cols$type %in% names(raw_column_types)), "unknown column type")
  lists <- c(names(spec$codelists), "arm", "visit")
  unknown <- setdiff(stats::na.omit(cols$codelist), lists)
  add(length(unknown) > 0, paste("undefined codelist:", paste(unknown, collapse = ", ")))
  for (t in spec$tables) {
    names_t <- purrr::map_chr(t$columns, "name")
    add(anyDuplicated(names_t) > 0, sprintf("%s: duplicated columns", t$name))
    add(
      !identical(utils::head(names_t, 2), c("record_id", "subject_id")),
      sprintf("%s: must start with record_id and subject_id", t$name)
    )
    required <- purrr::map_lgl(t$columns[seq_len(min(2, length(t$columns)))], \(c) !isTRUE(c$nullable))
    add(!all(required), sprintf("%s: record_id and subject_id must be required", t$name))
    add(
      length(t$natural_key) == 0 || !all(unlist(t$natural_key) %in% names_t),
      sprintf("%s: natural key must name its columns", t$name)
    )
  }
  # Assessments that produce data, and the table that holds them.
  holds <- c(
    consent = "enrolment", eligibility = "enrolment", demog = "demography", medhist = "demography",
    hba1c = "lab_results", fpg = "lab_results", labs = "lab_results", egfr = "lab_results",
    vitals = "vital_signs", weight = "vital_signs", height = "demography", ae = "adverse_events",
    hypo = "hypoglycaemia", conmed = "concomitant_meds", dispense = "exposure", accountability = "exposure"
  )
  for (p in protocols) {
    used <- unique(unlist(p$visits$assessments))
    needed <- setdiff(used, "ecg")
    add(any(!needed %in% names(holds)), sprintf("%s: assessment with no raw table", p$study))
    add(any(!holds[intersect(needed, names(holds))] %in% tables), sprintf("%s: a raw table is missing", p$study))
  }
  found$problems
}

#' DDL of the raw tables of one schema.
#'
#' Tables are created empty by the administrator, so the audit triggers are attached by the
#' event trigger of db/init/03-audit.sql and the privileges by the default privileges of
#' db/init/02-schemas.sql. A CHECK constraint enforces each codelist, so a value outside it
#' is refused at loading, as an EDC edit check would.
#'
#' @param spec The result of [read_raw_spec()].
#' @param schema Schema name, for example "raw_301".
#' @param protocol The result of [read_protocol()] of that study, for its arms and visits.
#' @return One SQL string.
raw_ddl <- function(spec, schema, protocol) {
  lists <- raw_codelists(spec, protocol)
  quote <- function(x) paste0("'", gsub("'", "''", x, fixed = TRUE), "'")
  one <- function(t) {
    lines <- purrr::map_chr(t$columns, function(c) {
      def <- sprintf("  %-24s %s%s", c$name, raw_column_types[[c$type]], if (isTRUE(c$nullable)) "" else " NOT NULL")
      if (!is.null(c$codelist)) {
        def <- paste0(def, sprintf(" CHECK (%s IN (%s))", c$name, paste(quote(lists[[c$codelist]]), collapse = ", ")))
      }
      def
    })
    lines <- c(lines, "  PRIMARY KEY (record_id)")
    comments <- c(
      sprintf("COMMENT ON TABLE %s.%s IS %s;", schema, t$name, quote(t$label)),
      purrr::map_chr(t$columns, function(c) {
        sprintf("COMMENT ON COLUMN %s.%s.%s IS %s;", schema, t$name, c$name, quote(c$description))
      })
    )
    paste0(
      sprintf("CREATE TABLE %s.%s (\n", schema, t$name), paste(lines, collapse = ",\n"), "\n);\n",
      paste(comments, collapse = "\n"), "\n"
    )
  }
  paste(purrr::map_chr(spec$tables, one), collapse = "\n")
}

#' The DDL file of the raw layer, for both studies.
#'
#' @param spec The result of [read_raw_spec()].
#' @param protocols A named list of results of [read_protocol()], named by schema.
#' @return One SQL string, as written to db/init/04-raw-tables.sql.
raw_ddl_file <- function(spec, protocols) {
  header <- c(
    "-- Raw data tables of the FJF-2026 studies. Runs once, as the administrator, when the",
    "-- database volume is first initialised (after 03-audit.sql, so the audit triggers attach).",
    "--",
    "-- GENERATED from docs/specs/raw-layer.yml by tools/write-raw-ddl.R. Do not edit by hand:",
    "-- a test fails if this file and the specification disagree.",
    sprintf("-- Specification version %s.", spec$version),
    ""
  )
  body <- purrr::imap_chr(protocols, \(p, schema) paste0("-- ", p$study, "\n", raw_ddl(spec, schema, p)))
  paste0(paste(header, collapse = "\n"), "\n", paste(body, collapse = "\n"))
}
