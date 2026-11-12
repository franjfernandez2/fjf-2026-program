# define.xml 2.1 of the ADaM package of a study.

#' Write the define.xml 2.1 of an ADaM package.
#'
#' One ItemGroupDef per dataset with its ADaM class, one ItemDef per variable with its origin
#' (Derived and Assigned from the sponsor; Predecessor with the source variable), a MethodDef
#' for each derived variable (its rule from the specification) and a leaf to each XPT file.
#' Dates are SAS numeric dates: integer with the DATE9. display format. Analysis results
#' metadata and value-level metadata are not included; the reviewer's guide says so.
#'
#' @param datasets Named list of the ADaM datasets.
#' @param spec The ADaM specification.
#' @param path File to write.
#' @param created Creation time of the file, ISO 8601.
#' @return `path`, for a file target.
write_adam_define <- function(datasets, spec, path, created) {
  ns <- c(
    xmlns = "http://www.cdisc.org/ns/odm/v1.3", "xmlns:def" = "http://www.cdisc.org/ns/def/v2.1",
    "xmlns:xlink" = "http://www.w3.org/1999/xlink"
  )
  doc <- xml2::xml_new_root("ODM", .version = "1.0", .encoding = "UTF-8")
  xml2::xml_set_attrs(doc, c(ns,
    ODMVersion = "1.3.2", FileType = "Snapshot", FileOID = paste0("DEFINE.", spec$study, ".ADAM"),
    CreationDateTime = created, Originator = "FJF-2026 programme"
  ))
  text <- function(node, tag, value) {
    xml2::xml_add_child(xml2::xml_add_child(node, tag), "TranslatedText", value, "xml:lang" = "en")
  }
  study <- xml2::xml_add_child(doc, "Study", OID = paste0("STUDY.", spec$study))
  gv <- xml2::xml_add_child(study, "GlobalVariables")
  xml2::xml_add_child(gv, "StudyName", spec$study)
  xml2::xml_add_child(gv, "StudyDescription", paste("ADaM analysis datasets of", spec$study))
  xml2::xml_add_child(gv, "ProtocolName", spec$study)
  mdv <- xml2::xml_add_child(study, "MetaDataVersion",
    OID = paste0("MDV.", spec$study, ".ADAMIG.", spec$adamig), Name = paste(spec$study, "ADaM"),
    "def:DefineVersion" = "2.1.0"
  )
  std <- xml2::xml_add_child(mdv, "def:Standards")
  xml2::xml_add_child(std, "def:Standard",
    OID = "STD.ADAMIG", Name = "ADaMIG", Type = "IG", Version = spec$adamig, Status = "Final"
  )
  required <- c("STUDYID", "USUBJID", "PARAMCD", "PARAM", "AVAL", "SUBJID", "SITEID", "TRT01P")
  items <- list()
  methods <- list()
  for (d in spec$datasets) {
    data <- datasets[[d$name]]
    keys <- unlist(d$keys)
    ig <- xml2::xml_add_child(mdv, "ItemGroupDef",
      OID = paste0("IG.", d$name), Name = d$name, SASDatasetName = d$name,
      Repeating = if (d$name == "ADSL") "No" else "Yes", IsReferenceData = "No", Purpose = "Analysis",
      "def:Structure" = d$structure, "def:StandardOID" = "STD.ADAMIG", "def:ArchiveLocationID" = paste0("LF.", d$name)
    )
    text(ig, "Description", d$label)
    for (i in seq_along(d$variables)) {
      v <- d$variables[[i]]
      oid <- paste0("IT.", d$name, ".", v$name)
      ref <- xml2::xml_add_child(ig, "ItemRef",
        ItemOID = oid, OrderNumber = i, Mandatory = if (v$name %in% required) "Yes" else "No"
      )
      if (v$name %in% keys) xml2::xml_set_attr(ref, "KeySequence", match(v$name, keys))
      if (v$origin == "Derived") {
        xml2::xml_set_attr(ref, "MethodOID", paste0("MT.", d$name, ".", v$name))
        methods[[length(methods) + 1]] <- list(
          oid = paste0("MT.", d$name, ".", v$name), name = paste("Derivation of", d$name, v$name),
          text = if (is.null(v$source)) v$label else v$source
        )
      }
      items[[length(items) + 1]] <- list(var = v, oid = oid, values = data[[v$name]])
    }
    xml2::xml_add_child(ig, "def:Class", Name = d$class)
    leaf <- xml2::xml_add_child(ig, "def:leaf", ID = paste0("LF.", d$name), "xlink:href" = paste0(
      tolower(d$name), ".xpt"
    ))
    xml2::xml_add_child(leaf, "def:title", paste0(tolower(d$name), ".xpt"))
  }
  for (it in items) {
    date <- it$var$type == "date"
    type <- if (date) "integer" else it$var$type
    def <- xml2::xml_add_child(mdv, "ItemDef",
      OID = it$oid, Name = it$var$name, SASFieldName = it$var$name, DataType = type,
      Length = if (type == "text") max(1L, nchar(it$values), na.rm = TRUE) else 8L
    )
    if (date) xml2::xml_set_attr(def, "def:DisplayFormat", "DATE9.")
    text(def, "Description", it$var$label)
    origin <- if (it$var$origin == "Predecessor") {
      xml2::xml_add_child(def, "def:Origin", Type = "Predecessor")
    } else {
      xml2::xml_add_child(def, "def:Origin", Type = it$var$origin, Source = "Sponsor")
    }
    if (it$var$origin == "Predecessor" && !is.null(it$var$source)) text(origin, "Description", it$var$source)
  }
  for (m in methods) {
    md <- xml2::xml_add_child(mdv, "MethodDef", OID = m$oid, Name = m$name, Type = "Computation")
    text(md, "Description", m$text)
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  xml2::write_xml(doc, path)
  path
}
