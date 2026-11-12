# Raw tables in the layout of docs/specs/raw-layer.yml.

#' A date as the CRF shows it: DD-MON-YYYY, English month, upper case.
#'
#' @param x A Date vector.
#' @return Character; NA stays NA.
crf_date <- function(x) {
  out <- sprintf(
    "%02d-%s-%04d", as.integer(format(x, "%d")), toupper(month.abb[as.integer(format(x, "%m"))]),
    as.integer(format(x, "%Y"))
  )
  out[is.na(x)] <- NA_character_
  out
}

#' Put a table in the column order of the specification, with record ids.
#'
#' Columns the specification lists and the data lack are added as missing; columns the
#' data have and the specification does not are an error.
#'
#' @param data A data frame.
#' @param spec The raw-layer specification.
#' @param table Table name.
#' @return A tibble with exactly the specified columns.
as_raw_table <- function(data, spec, table) {
  t <- purrr::keep(spec$tables, \(x) x$name == table)[[1]]
  cols <- purrr::map_chr(t$columns, "name")
  types <- purrr::map_chr(t$columns, "type")
  extra <- setdiff(names(data), cols)
  if (length(extra) > 0) stop(sprintf("%s: columns not in the specification: %s", table, paste(extra, collapse = ", ")))
  data$record_id <- seq_len(nrow(data))
  for (j in seq_along(cols)) {
    if (!cols[j] %in% names(data)) data[[cols[j]]] <- NA
    data[[cols[j]]] <- switch(types[j],
      text = as.character(data[[cols[j]]]),
      integer = as.integer(data[[cols[j]]]),
      numeric = as.numeric(data[[cols[j]]])
    )
  }
  tibble::as_tibble(data[cols])
}

#' The efficacy-side raw tables of a study.
#'
#' @param design The result of [read_design()].
#' @param study "201" or "301".
#' @param screened,pt,visits From [simulate_patients()] and [simulate_visits()].
#' @return A named list of raw tables.
efficacy_tables <- function(design, study, screened, pt, visits) {
  spec <- design$raw_spec
  p <- design$protocols[[study]]
  st <- study_settings[[study]]
  sh <- shared_settings
  yn <- function(x) ifelse(x, "Y", "N")
  done <- visits[visits$done, ]

  enrolment <- data.frame(
    subject_id = screened$subject_id, site_id = screened$site_id,
    consent_date = crf_date(screened$consent_date), screening_date = crf_date(screened$screening_date),
    eligible = yn(screened$eligible), failed_criterion = screened$failed_criterion
  )
  randomisation <- data.frame(
    subject_id = pt$subject_id, randomisation_number = pt$randomisation_number,
    randomisation_date = crf_date(pt$randomisation_date), stratum = pt$stratum, arm_code = pt$arm_code
  )
  demography <- data.frame(
    subject_id = pt$subject_id, birth_date = crf_date(pt$birth_date), sex = pt$sex, race = pt$race,
    ethnicity = pt$ethnicity, height_cm = pt$height_cm, diabetes_diagnosis_date = crf_date(pt$diagnosis_date),
    metformin_daily_mg = pt$metformin_mg
  )
  vis <- data.frame(
    subject_id = visits$subject_id, visit_code = visits$code,
    visit_date = ifelse(visits$done, crf_date(visits$date), NA_character_),
    visit_status = ifelse(visits$done, "DONE", "NOT DONE")
  )
  hb <- done[done$has_hba1c, ]
  gl <- done[done$has_fpg, ]
  labs <- rbind(
    data.frame(
      subject_id = hb$subject_id, visit_code = hb$code, collection_date = crf_date(hb$date), test_code = "HBA1C",
      result = sprintf("%.1f", hb$hba1c), unit = "%", ref_low = 4.0, ref_high = 5.6, fasting = NA_character_,
      .date = hb$date
    ),
    data.frame(
      subject_id = gl$subject_id, visit_code = gl$code, collection_date = crf_date(gl$date), test_code = "GLUC",
      result = sprintf("%d", as.integer(gl$fpg)), unit = "mg/dL", ref_low = 70, ref_high = 99, fasting = "Y",
      .date = gl$date
    )
  )
  # Kept as rows with their dates: the safety laboratory values are added in simulate_study().
  vs <- done[done$has_vitals | done$has_weight, ]
  vit <- sh$vitals
  nvs <- nrow(vs)
  vital_signs <- data.frame(
    subject_id = vs$subject_id, visit_code = vs$code, measure_date = crf_date(vs$date),
    sysbp = round(stats::rnorm(nvs, vit$sysbp[1], vit$sysbp[2])),
    diabp = round(stats::rnorm(nvs, vit$diabp[1], vit$diabp[2])),
    pulse = round(stats::rnorm(nvs, vit$pulse[1], vit$pulse[2])),
    weight = ifelse(vs$has_weight, vs$weight, NA), weight_unit = ifelse(vs$has_weight, "kg", NA)
  )
  vital_signs[!vs$has_vitals, c("sysbp", "diabp", "pulse")] <- NA

  # Exposure: a kit at each dispensing visit while on treatment, returned at the next
  # dispensing or accountability visit.
  kit <- done[done$has_dispense | done$has_accountability, ]
  kit <- kit[order(kit$pt, kit$date), ]
  kit$next_date <- stats::ave(as.numeric(kit$date), kit$pt, FUN = \(x) c(x[-1], NA))
  kit$next_week <- stats::ave(kit$week, kit$pt, FUN = \(x) c(x[-1], NA))
  kit <- kit[kit$has_dispense & kit$t < pt$disc_week[kit$pt], ]
  last_dose <- as.numeric(pt$last_dose_date[kit$pt])
  ret <- kit$next_date
  taken_to <- pmin(ifelse(is.na(ret), last_dose, ret), last_dose)
  days <- pmax(0, taken_to - as.numeric(kit$date))
  planned <- ifelse(is.na(kit$next_week), 4, kit$next_week - kit$week) * 7 + 7
  dispensed <- st$tablets_per_day * planned
  missed_doses <- stats::rbinom(nrow(kit), pmax(0, round(st$tablets_per_day * days)), sh$missed_dose_share)
  returned <- pmax(0, dispensed - st$tablets_per_day * days + missed_doses)
  exposure <- data.frame(
    subject_id = kit$subject_id, visit_code = kit$code, dispense_date = crf_date(kit$date),
    return_date = crf_date(as.Date(ret, origin = "1970-01-01")),
    tablets_dispensed = dispensed, tablets_returned = ifelse(is.na(ret), NA, round(returned))
  )

  # Disposition.
  eot <- p$treatment_weeks
  completed_treatment <- !is.finite(pt$disc_week) | pt$disc_week >= eot
  withdrawn <- is.finite(pt$withdrawal_week)
  fu <- visits[visits$week %in% max(protocol_visits(p)$week) & visits$done, ]
  fu_date <- fu$date[match(seq_len(nrow(pt)), fu$pt)]
  fu_date[is.na(fu_date)] <- (pt$randomisation_date + 7 * max(protocol_visits(p)$week))[is.na(fu_date)]
  disposition <- rbind(
    data.frame(
      subject_id = pt$subject_id, event = "TREATMENT",
      status = ifelse(completed_treatment, "COMPLETED", "DISCONTINUED"),
      reason = ifelse(completed_treatment, NA, pt$disc_reason), event_date = crf_date(pt$last_dose_date)
    ),
    data.frame(
      subject_id = pt$subject_id, event = "STUDY", status = ifelse(withdrawn, "DISCONTINUED", "COMPLETED"),
      reason = ifelse(withdrawn, pt$withdrawal_reason, NA),
      event_date = crf_date(ifelse(withdrawn, pt$withdrawal_date, fu_date) |> as.Date(origin = "1970-01-01"))
    )
  )
  disposition <- disposition[order(disposition$subject_id), ]

  # Concomitant medication: metformin for everyone; the rescue drug for rescued patients.
  rescued <- which(!is.na(pt$rescue_start_date))
  cm <- rbind(
    data.frame(
      subject_id = pt$subject_id, cm_seq = 1L, drug_name = "METFORMIN", indication = "TYPE 2 DIABETES",
      daily_dose = paste(pt$metformin_mg, "MG"), start_date = crf_date(pt$diagnosis_date + 30), end_date = NA,
      rescue = "N"
    ),
    data.frame(
      subject_id = pt$subject_id[rescued], cm_seq = rep(2L, length(rescued)),
      drug_name = sample(sh$rescue_drugs, length(rescued), replace = TRUE),
      indication = rep("HYPERGLYCAEMIA (RESCUE)", length(rescued)), daily_dose = rep(NA, length(rescued)),
      start_date = crf_date(pt$rescue_start_date[rescued]), end_date = rep(NA, length(rescued)),
      rescue = rep("Y", length(rescued))
    )
  )
  cm <- cm[order(cm$subject_id, cm$cm_seq), ]

  list(
    enrolment = as_raw_table(enrolment, spec, "enrolment"),
    randomisation = as_raw_table(randomisation, spec, "randomisation"),
    demography = as_raw_table(demography, spec, "demography"),
    visits = as_raw_table(vis, spec, "visits"),
    lab_rows = labs,
    vital_signs = as_raw_table(vital_signs, spec, "vital_signs"),
    exposure = as_raw_table(exposure, spec, "exposure"),
    disposition = as_raw_table(disposition, spec, "disposition"),
    concomitant_meds = as_raw_table(cm, spec, "concomitant_meds")
  )
}
