# Checks a dataset against its specification (read from define.xml by read_define_v1()).
# Returns one row per finding; no rows means the dataset matches its specification.
#
# Used twice: on the official pilot ADaM (are the pilot's own spec and data consistent?)
# and, from day 3 on, on our derived ADaM datasets.

check_against_spec <- function(data, spec, dataset) {
  vars <- spec$variables[spec$variables$dataset == dataset, ]
  if (nrow(vars) == 0) cli::cli_abort("{.val {dataset}} is not in the specification.")

  finding <- function(variable, check, expected, found) {
    if (length(variable) == 0) {
      return(NULL)
    }
    data.frame(
      dataset = dataset, variable = variable, check = check,
      expected = as.character(expected), found = as.character(found)
    )
  }

  in_both <- intersect(vars$variable, names(data))
  spec_of <- vars[match(in_both, vars$variable), ]

  data_label <- vapply(in_both, function(v) {
    label <- attr(data[[v]], "label")
    if (is.null(label)) NA_character_ else label
  }, character(1))
  # Dates are stored as numbers (SAS dates), so they count as numeric.
  data_numeric <- vapply(in_both, function(v) is.numeric(data[[v]]) || inherits(data[[v]], "Date"), logical(1))
  spec_numeric <- spec_of$type %in% c("integer", "float")
  wrong_label <- !is.na(spec_of$label) & (is.na(data_label) | data_label != spec_of$label)
  wrong_type <- data_numeric != spec_numeric

  in_order <- vars$variable[vars$variable %in% names(data)]
  order_found <- names(data)[names(data) %in% vars$variable]

  findings <- list(
    finding(setdiff(vars$variable, names(data)), "missing variable", "present", "absent"),
    finding(setdiff(names(data), vars$variable), "unexpected variable", "absent", "present"),
    finding(in_both[wrong_label], "label", spec_of$label[wrong_label], data_label[wrong_label]),
    finding(
      in_both[wrong_type], "type", spec_of$type[wrong_type],
      ifelse(data_numeric[wrong_type], "numeric", "character")
    ),
    if (!identical(in_order, order_found)) {
      finding("(all)", "variable order", paste(in_order, collapse = " "), paste(order_found, collapse = " "))
    }
  )

  if ("PARAMCD" %in% names(data)) {
    vlm <- spec$value_level[spec$value_level$dataset == dataset, ]
    if (nrow(vlm) > 0) {
      found <- unique(data$PARAMCD)
      findings <- c(findings, list(
        finding(setdiff(found, vlm$paramcd), "PARAMCD not in value-level metadata", "listed", "unlisted"),
        finding(
          setdiff(unique(vlm$paramcd), found), "PARAMCD in value-level metadata but not in data",
          "present", "absent"
        )
      ))
    }
  }

  result <- do.call(rbind, findings)
  if (is.null(result)) {
    result <- data.frame(
      dataset = character(), variable = character(), check = character(),
      expected = character(), found = character()
    )
  }
  rownames(result) <- NULL
  result
}

# Checks that every analysis result's selection criteria only name parameters that its own
# parameter list declares. The pilot's metadata were written by hand, so they can disagree.
check_analysis_results <- function(spec) {
  displays <- spec$displays
  rows <- lapply(seq_len(nrow(displays)), function(i) {
    selected <- regmatches(displays$selection[i], gregexpr('PARAMCD="[^"]+"', displays$selection[i]))[[1]]
    selected <- unique(gsub('PARAMCD="|"', "", selected))
    declared <- if (is.na(displays$parameters[i])) character() else strsplit(displays$parameters[i], "; ")[[1]]
    undeclared <- setdiff(selected, declared)
    if (length(undeclared) == 0) {
      return(NULL)
    }
    data.frame(
      display_id = displays$display_id[i], result = displays$result[i],
      declared = paste(declared, collapse = "; "), selected = paste(undeclared, collapse = "; ")
    )
  })
  result <- do.call(rbind, rows)
  if (is.null(result)) {
    result <- data.frame(display_id = character(), result = character(), declared = character(), selected = character())
  }
  result
}
