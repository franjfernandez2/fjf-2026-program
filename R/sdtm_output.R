# Writing SDTM datasets to their submission formats: SAS transport (XPT v5) with the
# specification's labels and types, Parquet for analysis, and the define.xml 2.1 that
# describes them. Shared by the studies; the specification is a docs/specs/sdtm-*.yml file.

#' Domain names of an SDTM specification, in order.
#'
#' @param spec The SDTM specification.
#' @return Character vector.
sdtm_domains <- function(spec) purrr::map_chr(spec$domains, "name")

#' xportr metadata of one domain: types and labels from the specification, lengths from the
#' data (the longest value, at least 1), as the FDA's guidance on variable lengths asks.
#'
#' @param data The domain's dataset.
#' @param spec The SDTM specification.
#' @param domain Domain name.
#' @return A data frame with the columns xportr expects.
sdtm_xpt_metadata <- function(data, spec, domain) {
  vars <- sdtm_variables(spec, domain)
  names <- purrr::map_chr(vars, "name")
  type <- purrr::map_chr(vars, "type")
  length <- vapply(names, \(v) {
    if (type[match(v, names)] == "text") max(1L, nchar(data[[v]]), na.rm = TRUE) else 8L
  }, integer(1))
  data.frame(
    dataset = domain, variable = names, label = purrr::map_chr(vars, "label"),
    type = c(text = "character", integer = "integer", float = "float")[type], length = unname(length),
    order = seq_along(names)
  )
}

#' Write one SDTM domain to XPT and Parquet.
#'
#' @param data The domain's dataset.
#' @param spec The SDTM specification.
#' @param domain Domain name.
#' @param dir Folder to write to.
#' @return The paths of the two files, for a file target.
write_sdtm_domain <- function(data, spec, domain, dir) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  xpt <- file.path(dir, paste0(tolower(domain), ".xpt"))
  parquet <- file.path(dir, paste0(tolower(domain), ".parquet"))
  meta <- sdtm_xpt_metadata(data, spec, domain)
  label <- purrr::keep(spec$domains, \(d) d$name == domain)[[1]]$label
  data |>
    xportr::xportr_metadata(meta, domain = domain, verbose = "stop") |>
    xportr::xportr_type() |>
    xportr::xportr_length() |>
    xportr::xportr_label() |>
    xportr::xportr_order() |>
    xportr::xportr_df_label(data.frame(dataset = domain, label = label)) |>
    xportr::xportr_write(xpt)
  arrow::write_parquet(data, parquet)
  c(xpt, parquet)
}

# SDTMIG classes of the domains used here.
sdtm_class <- c(
  DM = "SPECIAL PURPOSE", SV = "SPECIAL PURPOSE", DS = "EVENTS", AE = "EVENTS", CE = "EVENTS",
  EX = "INTERVENTIONS", CM = "INTERVENTIONS", DA = "FINDINGS", LB = "FINDINGS", VS = "FINDINGS",
  SUPPAE = "RELATIONSHIP", SUPPCE = "RELATIONSHIP", SUPPDM = "RELATIONSHIP", IE = "FINDINGS", SE = "SPECIAL PURPOSE",
  TA = "TRIAL DESIGN", TE = "TRIAL DESIGN", TV = "TRIAL DESIGN", TI = "TRIAL DESIGN", TS = "TRIAL DESIGN"
)

# Variables that SDTMIG 3.4 marks as required in the domains used here.
sdtm_required <- function(name, domain) {
  prefix <- if (startsWith(domain, "SUPP")) "" else domain
  fixed <- c(
    "STUDYID", "DOMAIN", "USUBJID", "SUBJID", "SITEID", "SEX", "RDOMAIN", "QNAM",
    "QLABEL", "QVAL", "QORIG", "VISITNUM"
  )
  name %in% c(fixed, paste0(prefix, c("SEQ", "TESTCD", "TEST", "TERM", "DECOD", "TRT", "CAT")[
    c(
      TRUE, TRUE, TRUE, domain %in% c("AE", "CE", "DS"), domain %in% c("AE", "DS"), domain %in% c("EX", "CM"),
      domain == "DS"
    )
  ]))
}

#' Define-XML 2.1 data type of a variable.
#'
#' @param x The variable's values.
#' @param type The specification type.
#' @param name Variable name.
#' @return "text", "integer", "float", "date" or "partialDate".
define_data_type <- function(x, type, name) {
  if (!grepl("DTC$", name)) {
    return(type)
  }
  x <- x[!is.na(x)]
  if (length(x) > 0 && all(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", x))) "date" else "partialDate"
}

#' Write the define.xml 2.1 of an SDTM package.
#'
#' One ItemGroupDef per domain, one ItemDef per variable with its origin, a MethodDef for each
#' derived variable (its source text), and a leaf to each XPT file. The project's coding
#' dictionary has no codes and controlled terminology is not attached as codelists; the
#' reviewer's guide says so.
#'
#' @param datasets Named list of the SDTM datasets.
#' @param spec The SDTM specification.
#' @param path File to write.
#' @param created Creation time of the file, ISO 8601.
#' @return `path`, for a file target.
write_sdtm_define <- function(datasets, spec, path, created) {
  ns <- c(
    xmlns = "http://www.cdisc.org/ns/odm/v1.3", "xmlns:def" = "http://www.cdisc.org/ns/def/v2.1",
    "xmlns:xlink" = "http://www.w3.org/1999/xlink"
  )
  doc <- xml2::xml_new_root("ODM", .version = "1.0", .encoding = "UTF-8")
  xml2::xml_set_attrs(doc, c(ns,
    ODMVersion = "1.3.2", FileType = "Snapshot", FileOID = paste0("DEFINE.", spec$study, ".SDTM"),
    CreationDateTime = created, Originator = "FJF-2026 programme"
  ))
  text <- function(node, tag, value) {
    xml2::xml_add_child(xml2::xml_add_child(node, tag), "TranslatedText", value, "xml:lang" = "en")
  }
  study <- xml2::xml_add_child(doc, "Study", OID = paste0("STUDY.", spec$study))
  gv <- xml2::xml_add_child(study, "GlobalVariables")
  xml2::xml_add_child(gv, "StudyName", spec$study)
  xml2::xml_add_child(gv, "StudyDescription", paste("SDTM tabulation datasets of", spec$study))
  xml2::xml_add_child(gv, "ProtocolName", spec$study)
  mdv <- xml2::xml_add_child(study, "MetaDataVersion",
    OID = paste0("MDV.", spec$study, ".SDTMIG.", spec$sdtmig), Name = paste(spec$study, "SDTM"),
    "def:DefineVersion" = "2.1.0"
  )
  std <- xml2::xml_add_child(mdv, "def:Standards")
  xml2::xml_add_child(std, "def:Standard",
    OID = "STD.SDTMIG", Name = "SDTMIG", Type = "IG", Version = spec$sdtmig, Status = "Final"
  )
  if (!is.null(spec$ct)) {
    xml2::xml_add_child(std, "def:Standard",
      OID = "STD.CT.SDTM", Name = "CDISC/NCI", Type = "CT", PublishingSet = "SDTM", Version = spec$ct,
      Status = "Final"
    )
  }
  items <- list()
  methods <- list()
  for (d in spec$domains) {
    data <- datasets[[d$name]]
    keys <- unlist(d$keys)
    ig <- xml2::xml_add_child(mdv, "ItemGroupDef",
      OID = paste0("IG.", d$name), Name = d$name, SASDatasetName = d$name, Repeating = "Yes",
      IsReferenceData = "No", Purpose = "Tabulation", "def:Structure" = d$structure,
      "def:StandardOID" = "STD.SDTMIG", "def:ArchiveLocationID" = paste0("LF.", d$name),
      Domain = if (startsWith(d$name, "SUPP")) sub("^SUPP", "", d$name) else d$name
    )
    if (d$name == "DM") xml2::xml_set_attr(ig, "Repeating", "No")
    if (sdtm_class[[d$name]] == "TRIAL DESIGN") xml2::xml_set_attr(ig, "IsReferenceData", "Yes")
    text(ig, "Description", d$label)
    for (i in seq_along(d$variables)) {
      v <- d$variables[[i]]
      oid <- paste0("IT.", d$name, ".", v$name)
      ref <- xml2::xml_add_child(ig, "ItemRef",
        ItemOID = oid, OrderNumber = i, Mandatory = if (sdtm_required(v$name, d$name)) "Yes" else "No"
      )
      if (v$name %in% keys) xml2::xml_set_attr(ref, "KeySequence", match(v$name, keys))
      if (v$origin == "Derived") {
        xml2::xml_set_attr(ref, "MethodOID", paste0("MT.", d$name, ".", v$name))
        methods[[length(methods) + 1]] <- list(
          oid = paste0("MT.", d$name, ".", v$name), name = paste("Derivation of", d$name, v$name),
          text = if (is.null(v$source)) v$label else v$source
        )
      }
      items[[length(items) + 1]] <- list(domain = d$name, var = v, oid = oid, values = data[[v$name]])
    }
    xml2::xml_add_child(ig, "def:Class", Name = sdtm_class[[d$name]])
    leaf <- xml2::xml_add_child(ig, "def:leaf", ID = paste0("LF.", d$name), "xlink:href" = paste0(
      tolower(d$name),
      ".xpt"
    ))
    xml2::xml_add_child(leaf, "def:title", paste0(tolower(d$name), ".xpt"))
  }
  for (it in items) {
    type <- define_data_type(it$values, it$var$type, it$var$name)
    def <- xml2::xml_add_child(mdv, "ItemDef",
      OID = it$oid, Name = it$var$name, SASFieldName = it$var$name, DataType = type
    )
    if (type %in% c("text", "integer", "float")) {
      len <- if (type == "text") max(1L, nchar(it$values), na.rm = TRUE) else 8L
      xml2::xml_set_attr(def, "Length", len)
    }
    text(def, "Description", it$var$label)
    source <- switch(it$var$origin,
      Collected = "Investigator",
      "Sponsor"
    )
    origin <- xml2::xml_add_child(def, "def:Origin", Type = it$var$origin, Source = source)
    if (it$var$origin == "Collected" && !is.null(it$var$source)) text(origin, "Description", it$var$source)
  }
  for (m in methods) {
    md <- xml2::xml_add_child(mdv, "MethodDef", OID = m$oid, Name = m$name, Type = "Computation")
    text(md, "Description", m$text)
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  xml2::write_xml(doc, path)
  path
}

#' Write one ADaM dataset to XPT and Parquet, with the specification's labels and types.
#' Dates are numeric in XPT, with the DATE9. format.
#'
#' @param data The dataset.
#' @param spec The ADaM specification (its `datasets` section).
#' @param dataset Dataset name.
#' @param dir Folder to write to.
#' @return The paths of the two files, for a file target.
write_adam_dataset <- function(data, spec, dataset, dir) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  d <- purrr::keep(spec$datasets, \(x) x$name == dataset)[[1]]
  names <- purrr::map_chr(d$variables, "name")
  type <- purrr::map_chr(d$variables, "type")
  length <- vapply(seq_along(names), \(i) {
    if (type[i] == "text") max(1L, nchar(data[[names[i]]]), na.rm = TRUE) else 8L
  }, integer(1))
  meta <- data.frame(
    dataset = dataset, variable = names, label = purrr::map_chr(d$variables, "label"),
    type = c(text = "character", integer = "integer", float = "float", date = "numeric")[type],
    length = length, order = seq_along(names), format = ifelse(type == "date", "DATE9.", NA)
  )
  xpt <- file.path(dir, paste0(tolower(dataset), ".xpt"))
  parquet <- file.path(dir, paste0(tolower(dataset), ".parquet"))
  data |>
    xportr::xportr_metadata(meta, domain = dataset, verbose = "stop") |>
    xportr::xportr_type() |>
    xportr::xportr_length() |>
    xportr::xportr_label() |>
    xportr::xportr_order() |>
    xportr::xportr_format() |>
    xportr::xportr_df_label(data.frame(dataset = dataset, label = d$label)) |>
    xportr::xportr_write(xpt)
  arrow::write_parquet(data, parquet)
  c(xpt, parquet)
}
