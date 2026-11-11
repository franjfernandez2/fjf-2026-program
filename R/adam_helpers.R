# Helpers shared by the ADaM programs: reading SDTM, looking up codelists in the
# specification, applying the specification's attributes and comparing with a reference.

#' Read SDTM domains from the downloaded pilot files.
#'
#' Blank strings become NA (admiral treats NA, not "", as missing).
#' @param paths Paths returned by `fetch_pilot_files()`.
#' @param domains Lower-case domain names, e.g. `c("dm", "ex")`.
#' @return A named list of tibbles.
read_sdtm <- function(paths, domains) {
  out <- lapply(domains, function(domain) {
    data <- haven::read_xpt(pilot_file(paths, sprintf("sdtm/%s.xpt", domain)))
    admiral::convert_blanks_to_na(data)
  })
  stats::setNames(out, domains)
}

#' Translate between the codes and decodes of a codelist in the specification.
#'
#' Stops if a value is not in the codelist, so a new or misspelt value cannot pass
#' silently as NA.
#' @param spec Output of `read_define_v1()`.
#' @param codelist Codelist id, e.g. `"ARMN"`.
#' @param values Values to translate; NA stays NA.
#' @param from `"decode"` (return the code) or `"code"` (return the decode).
#' @return A character vector.
codelist_lookup <- function(spec, codelist, values, from = c("decode", "code")) {
  from <- match.arg(from)
  to <- if (from == "decode") "code" else "decode"
  cl <- spec$codelists[spec$codelists$code_id == codelist, ]
  if (nrow(cl) == 0) stop("Codelist ", codelist, " is not in the specification.", call. = FALSE)
  unknown <- setdiff(stats::na.omit(unique(values)), cl[[from]])
  if (length(unknown) > 0) {
    stop("Not in codelist ", codelist, ": ", paste(unknown, collapse = ", "), call. = FALSE)
  }
  cl[[to]][match(values, cl[[from]])]
}

#' Round as SAS's ROUND function does: halves away from zero.
#'
#' R's `round()` follows IEC 60559 and works on the binary value, so `round(56.25, 1)` is
#' 56.2. SAS gives 56.3, and so do the pilot's official datasets (discrepancy D-002).
#' The small offset absorbs the binary error of values such as 56.25.
#' @param x Numeric vector.
#' @param digits Decimal places.
round_sas <- function(x, digits = 0) {
  scale <- 10^digits
  sign(x) * floor(abs(x) * scale + 0.5 + sqrt(.Machine$double.eps)) / scale
}

#' Round a variable to the decimal places the specification gives it.
#'
#' In Define-XML 1.0, SignificantDigits is the number of decimal places of a float.
#' @param x Numeric vector.
#' @param spec Output of `read_define_v1()`.
#' @param dataset,variable Where `x` belongs in the specification.
round_to_spec <- function(x, spec, dataset, variable) {
  digits <- spec$variables$sig_dig[spec$variables$dataset == dataset & spec$variables$variable == variable]
  if (length(digits) != 1 || is.na(digits)) {
    stop(dataset, ".", variable, " has no SignificantDigits in the specification.", call. = FALSE)
  }
  round_sas(x, digits)
}

#' Give a derived dataset the variables, order, labels and types of its specification.
#'
#' Variables the specification does not list are dropped; a listed variable that was not
#' derived is an error. Integer and float variables become doubles, as in SAS transport
#' files; dates stay `Date`.
#' @param data The derived dataset.
#' @param spec Output of `read_define_v1()`.
#' @param dataset Dataset name in the specification, e.g. `"ADSL"`.
#' @return `data` with the specification's attributes.
apply_spec_attributes <- function(data, spec, dataset) {
  vars <- spec$variables[spec$variables$dataset == dataset, ]
  vars <- vars[order(vars$order), ]
  missing <- setdiff(vars$variable, names(data))
  if (length(missing) > 0) {
    stop(dataset, " is missing specified variables: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  data <- data[vars$variable]
  for (i in seq_len(nrow(vars))) {
    var <- vars$variable[i]
    x <- data[[var]]
    if (vars$type[i] %in% c("integer", "float") && !inherits(x, "Date")) x <- as.double(x)
    if (vars$type[i] %in% c("text", "datetime")) x <- as.character(x)
    attr(x, "label") <- vars$label[i]
    data[[var]] <- x
  }
  data
}

#' Compare a derived dataset with a reference dataset.
#'
#' Uses `diffdf` on values only: labels and SAS formats are checked separately by
#' `check_against_spec()`. Numbers within `tolerance` count as equal. A SAS transport file
#' cannot hold a missing character value, so blanks in the reference are read as NA,
#' the convention our datasets follow until they are written to XPT.
#' @param ours The derived dataset.
#' @param reference The reference dataset (e.g. the pilot's official ADaM).
#' @param keys Key variables that identify a record.
#' @param tolerance Absolute tolerance for numeric values.
#' @return A tibble with one row per finding: `variable`, `issue`, `n` and up to three
#'   `examples` as "key: reference -> ours". No rows means the datasets agree.
compare_with_reference <- function(ours, reference, keys, tolerance = 1e-8) {
  strip <- function(data) {
    data <- as.data.frame(data)
    for (var in names(data)) attributes(data[[var]]) <- attributes(data[[var]])["class"]
    data
  }
  res <- diffdf::diffdf(
    base = strip(admiral::convert_blanks_to_na(reference)), compare = strip(ours), keys = keys,
    tolerance = tolerance, suppress_warnings = TRUE
  )
  finding <- function(variable, issue, n = NA_integer_, examples = "") {
    if (length(variable) == 0) {
      return(NULL)
    }
    tibble::tibble(variable = variable, issue = issue, n = as.integer(n), examples = examples)
  }
  value_findings <- lapply(grep("^VarDiff_", names(res), value = TRUE), function(name) {
    diff <- res[[name]]
    key <- do.call(paste, c(diff[keys], sep = "/"))
    shown <- utils::head(sprintf("%s: %s -> %s", key, format_value(diff$BASE), format_value(diff$COMPARE)), 3)
    finding(sub("^VarDiff_", "", name), "different values", nrow(diff), paste(shown, collapse = "; "))
  })
  findings <- c(
    list(
      if (!is.null(res$ExtRowsBase)) finding(NA_character_, "record only in reference", nrow(res$ExtRowsBase)),
      if (!is.null(res$ExtRowsComp)) finding(NA_character_, "record only in ours", nrow(res$ExtRowsComp)),
      finding(res$ExtColsBase$COLUMNS, "variable only in reference"),
      finding(res$ExtColsComp$COLUMNS, "variable only in ours"),
      finding(res$ClassDiffs$VARIABLE, "different class")
    ),
    value_findings
  )
  result <- do.call(rbind, findings)
  if (is.null(result)) {
    result <- tibble::tibble(variable = character(), issue = character(), n = integer(), examples = character())
  }
  result
}

# diffdf stores BASE and COMPARE as list columns; show NA and dates readably.
format_value <- function(x) {
  vapply(x, function(v) {
    if (length(v) == 0 || is.na(v)) {
      return("NA")
    }
    if (inherits(v, "Date")) format(v) else as.character(v)
  }, character(1))
}
