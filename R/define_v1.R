# Reads a Define-XML 1.0 file, the version the CDISC pilot uses, into tidy specification
# tables, and builds a metacore object from them (ADR 0016).
#
# metacore::define_to_metacore() reads only Define-XML 2.0, so it cannot read the pilot's
# define.xml. This reader keeps the specification tied to the official file, instead of
# retyping it by hand.

# The pilot's file pads some values with non-breaking spaces (U+00A0), e.g. the SEX code
# "F\u00a0". SAS ignores trailing blanks, so they are removed here, with ordinary spaces.
trim_text <- function(x) trimws(x, whitespace = "[\\h\\v]")

xml_attr_or_na <- function(nodes, attr, ns) {
  value <- xml2::xml_attr(nodes, attr, ns = ns)
  value <- trim_text(value)
  value[value == ""] <- NA_character_
  value
}

# One row per node, with the attributes in `attrs` (names become column names).
nodes_to_df <- function(nodes, attrs, ns) {
  columns <- lapply(attrs, function(a) xml_attr_or_na(nodes, a, ns))
  names(columns) <- names(attrs)
  as.data.frame(columns, stringsAsFactors = FALSE)
}

read_define_v1 <- function(path) {
  doc <- xml2::read_xml(path)
  ns <- xml2::xml_ns(doc)
  if (!identical(unname(ns["def"]), "http://www.cdisc.org/ns/def/v1.0")) {
    cli::cli_abort("{.file {path}} is not Define-XML 1.0.")
  }
  find <- function(xpath, node = doc) xml2::xml_find_all(node, xpath, ns)

  # Datasets.
  groups <- find("//d1:ItemGroupDef")
  datasets <- nodes_to_df(groups, c(
    dataset = "Name", label = "def:Label", structure = "def:Structure",
    keys = "def:DomainKeys", class = "def:Class"
  ), ns)

  # Dataset variables, in order.
  refs <- lapply(groups, function(group) {
    item_refs <- find("d1:ItemRef", group)
    data.frame(
      dataset = xml2::xml_attr(group, "Name"),
      oid = xml2::xml_attr(item_refs, "ItemOID"),
      order = as.integer(xml2::xml_attr(item_refs, "OrderNumber")),
      mandatory = xml2::xml_attr(item_refs, "Mandatory") == "Yes"
    )
  })
  refs <- do.call(rbind, refs)

  # Item definitions: variables and value-level items share the same element.
  items <- find("//d1:ItemDef")
  item_df <- nodes_to_df(items, c(
    oid = "OID", name = "Name", type = "DataType", length = "Length",
    sig_dig = "SignificantDigits", format = "def:DisplayFormat", origin = "Origin",
    derivation = "Comment", label = "def:Label"
  ), ns)
  item_df$length <- as.integer(item_df$length)
  item_df$sig_dig <- as.integer(item_df$sig_dig)
  item_df$codelist <- vapply(items, function(item) {
    ref <- xml2::xml_find_first(item, "d1:CodeListRef", ns)
    if (inherits(ref, "xml_missing")) NA_character_ else xml2::xml_attr(ref, "CodeListOID")
  }, character(1))
  item_df$valuelist <- vapply(items, function(item) {
    ref <- xml2::xml_find_first(item, "def:ValueListRef", ns)
    if (inherits(ref, "xml_missing")) NA_character_ else xml2::xml_attr(ref, "ValueListOID")
  }, character(1))

  variables <- merge(refs, item_df, by = "oid", all.x = TRUE, sort = FALSE)
  variables$variable <- variables$name
  variables <- variables[order(variables$dataset, variables$order), c(
    "dataset", "variable", "order", "mandatory", "label", "type", "length", "sig_dig",
    "format", "origin", "derivation", "codelist", "valuelist"
  )]

  # Value-level metadata. In Define-XML 1.0 a value-level item's OID is
  # DATASET.VARIABLE.PARAMCD; there is no WhereClause.
  value_lists <- find("//def:ValueListDef")
  value_level <- lapply(value_lists, function(vl) {
    item_refs <- find("d1:ItemRef", vl)
    data.frame(
      valuelist = xml2::xml_attr(vl, "OID"),
      oid = xml2::xml_attr(item_refs, "ItemOID"),
      order = as.integer(xml2::xml_attr(item_refs, "OrderNumber"))
    )
  })
  value_level <- merge(do.call(rbind, value_level), item_df, by = "oid", all.x = TRUE, sort = FALSE)
  parts <- strsplit(value_level$oid, ".", fixed = TRUE)
  value_level$dataset <- vapply(parts, `[`, character(1), 1)
  value_level$variable <- vapply(parts, `[`, character(1), 2)
  value_level$paramcd <- vapply(parts, function(p) paste(p[-(1:2)], collapse = "."), character(1))
  value_level <- value_level[order(value_level$dataset, value_level$variable, value_level$order), c(
    "dataset", "variable", "paramcd", "order", "label", "type", "length", "sig_dig",
    "format", "origin", "derivation", "codelist"
  )]

  # Codelists, one row per code. External codelists (for example MedDRA) have no codes.
  codelists <- lapply(find("//d1:CodeList"), function(cl) {
    codes <- find("d1:CodeListItem", cl)
    if (length(codes) == 0) {
      return(data.frame(
        code_id = xml2::xml_attr(cl, "OID"), name = xml2::xml_attr(cl, "Name"),
        data_type = xml2::xml_attr(cl, "DataType"), code = NA_character_,
        decode = NA_character_, rank = NA_integer_
      ))
    }
    decode <- vapply(codes, function(code) {
      text <- xml2::xml_find_first(code, "d1:Decode/d1:TranslatedText", ns)
      if (inherits(text, "xml_missing")) NA_character_ else trim_text(xml2::xml_text(text))
    }, character(1))
    data.frame(
      code_id = xml2::xml_attr(cl, "OID"),
      name = xml2::xml_attr(cl, "Name"),
      data_type = xml2::xml_attr(cl, "DataType"),
      code = trim_text(xml2::xml_attr(codes, "CodedValue")),
      decode = decode,
      rank = as.integer(xml2::xml_attr(codes, "def:Rank", ns = ns))
    )
  })
  codelists <- do.call(rbind, codelists)

  list(
    datasets = datasets,
    variables = variables,
    value_level = value_level,
    codelists = codelists,
    displays = read_analysis_results(doc, ns)
  )
}

# Analysis results metadata (the draft "adamref" extension used by the pilot): one row per
# analysis result, with the display it belongs to and how it is computed.
read_analysis_results <- function(doc, ns) {
  text_of <- function(node, xpath) {
    found <- xml2::xml_find_all(node, xpath, ns)
    if (length(found) == 0) {
      return(NA_character_)
    }
    paste(trim_text(xml2::xml_text(found)), collapse = "; ")
  }
  attr_of <- function(node, xpath, attr) {
    found <- xml2::xml_find_all(node, xpath, ns)
    if (length(found) == 0) {
      return(NA_character_)
    }
    paste(xml2::xml_attr(found, attr), collapse = "; ")
  }

  results <- xml2::xml_find_all(doc, "//adamref:ResultDisplay/adamref:AnalysisResults", ns)
  rows <- lapply(results, function(result) {
    display <- xml2::xml_parent(result)
    data.frame(
      display_id = xml2::xml_attr(display, "DisplayIdentifier"),
      display_label = xml2::xml_attr(display, "DisplayLabel"),
      result_oid = xml2::xml_attr(result, "OID"),
      result = xml2::xml_attr(result, "ResultIdentifier"),
      reason = xml2::xml_attr(result, "Reason"),
      parameters = attr_of(result, "adamref:ParameterList/adamref:Parameter", "ParamCD"),
      analysis_variables = attr_of(result, "adamref:AnalysisVariable", "ItemOID"),
      datasets = attr_of(result, "adamref:AnalysisDataset/d1:ItemGroupRef", "ItemGroupOID"),
      selection = text_of(result, "adamref:AnalysisDataset/adamref:SelectionCriteria/def:ComputationMethod"),
      documentation = text_of(result, "adamref:Documentation/d1:TranslatedText"),
      documentation_ref = attr_of(result, "adamref:Documentation", "leafID"),
      code = text_of(result, "adamref:ProgrammingCode/def:ComputationMethod")
    )
  })
  do.call(rbind, rows)
}

# Builds a metacore object for the chosen datasets.
define_v1_to_metacore <- function(spec, datasets) {
  ds <- spec$datasets[spec$datasets$dataset %in% datasets, ]
  vars <- spec$variables[spec$variables$dataset %in% datasets, ]
  vlm <- spec$value_level[spec$value_level$dataset %in% datasets, ]

  key_seq <- mapply(function(dataset, variable) {
    keys <- trimws(strsplit(ds$keys[ds$dataset == dataset], ",")[[1]])
    position <- match(variable, keys)
    if (is.na(position)) NA_integer_ else position
  }, vars$dataset, vars$variable, USE.NAMES = FALSE)

  ds_vars <- tibble::tibble(
    dataset = vars$dataset, variable = vars$variable, key_seq = as.integer(key_seq),
    order = vars$order, mandatory = vars$mandatory, core = NA_character_, supp_flag = FALSE
  )

  # A variable defined identically in every dataset gets one row; otherwise one row per
  # dataset, named DATASET.VARIABLE, as metacore expects.
  attrs <- c("label", "type", "length", "format")
  signature <- do.call(paste, c(lapply(vars[attrs], as.character), sep = "\r"))
  shared <- tapply(signature, vars$variable, function(s) length(unique(s)) == 1)
  shared <- unname(as.vector(shared[vars$variable]))
  spec_name <- ifelse(shared, vars$variable, paste(vars$dataset, vars$variable, sep = "."))
  var_spec <- tibble::tibble(
    variable = spec_name, label = vars$label, length = vars$length, type = vars$type,
    common = NA_character_, format = vars$format
  )
  var_spec <- var_spec[!duplicated(var_spec$variable), ]

  derivation_id <- paste(vars$dataset, vars$variable, sep = ".")
  vlm_id <- paste(vlm$dataset, vlm$variable, vlm$paramcd, sep = ".")
  value_spec <- tibble::tibble(
    dataset = c(vars$dataset, vlm$dataset),
    variable = c(vars$variable, vlm$variable),
    where = c(rep(NA_character_, nrow(vars)), sprintf("PARAMCD == '%s'", vlm$paramcd)),
    type = c(vars$type, vlm$type),
    sig_dig = c(vars$sig_dig, vlm$sig_dig),
    code_id = c(vars$codelist, vlm$codelist),
    origin = tolower(c(vars$origin, vlm$origin)),
    derivation_id = c(derivation_id, vlm_id)
  )
  derivations <- tibble::tibble(
    derivation_id = value_spec$derivation_id,
    derivation = c(vars$derivation, vlm$derivation)
  )

  used_codelists <- unique(stats::na.omit(value_spec$code_id))
  cl <- spec$codelists[spec$codelists$code_id %in% used_codelists, ]
  codelist <- lapply(split(cl, cl$code_id), function(x) {
    tibble::tibble(
      code_id = x$code_id[1], name = x$name[1], type = "code_decode",
      codes = list(tibble::tibble(code = x$code, decode = x$decode))
    )
  })
  codelist <- do.call(rbind, codelist)

  metacore::metacore(
    ds_spec = tibble::tibble(dataset = ds$dataset, structure = ds$structure, label = ds$label),
    ds_vars = ds_vars, var_spec = var_spec, value_spec = value_spec,
    derivations = derivations, codelist = codelist, verbose = "silent"
  )
}
