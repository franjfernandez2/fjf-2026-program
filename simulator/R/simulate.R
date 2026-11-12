# One study's raw data, from the design files, a truth and a seed.

#' Simulate the raw data of a study.
#'
#' The patients' random numbers come from their own stream of the seed (2 for FJF-2026-201,
#' 3 for FJF-2026-301), separate from the stream of the truth, so the truth and each study
#' can be regenerated on their own.
#'
#' @param design The result of [read_design()].
#' @param truth An `fjf_truth`, from [draw_truth()] or [planning_truth()].
#' @param study "201" or "301".
#' @param n_per_arm Patients per arm.
#' @param seed A public seed, or the result of [read_sealed_seed()].
#' Safety data are drawn after all the efficacy data, from the same stream.
#'
#' @param keep_latent Also return the patient-level values no real study records (true
#'   event times, residuals). For the simulator's own tests only; never written to disk.
#' @return A named list of raw tables, as in docs/specs/raw-layer.yml; with `keep_latent`,
#'   a list of `tables` and `latent`.
simulate_study <- function(design, truth, study, n_per_arm, seed, keep_latent = FALSE) {
  study <- as.character(study)
  stopifnot(study %in% names(study_settings), inherits(truth, "fjf_truth"))
  stream <- c(`201` = 2L, `301` = 3L)[[study]]
  out <- withr::with_seed(seed_for(seed, stream), {
    s <- simulate_patients(design, truth, study, n_per_arm)
    v <- simulate_visits(design, truth, study, s$patients)
    tables <- efficacy_tables(design, study, s$screened, v$patients, v$visits)
    cm <- tables$concomitant_meds[tables$concomitant_meds$rescue == "Y", ]
    rescue_drug <- cm$drug_name[match(v$patients$subject_id, cm$subject_id)]
    sf <- simulate_safety(design, truth, study, v$patients, v$visits, rescue_drug)
    spec <- design$raw_spec
    labs <- rbind(tables$lab_rows, sf$labs)
    labs <- labs[order(labs$subject_id, labs$.date, labs$test_code), ]
    labs$.date <- NULL
    tables$lab_rows <- NULL
    tables$lab_results <- as_raw_table(labs, spec, "lab_results")
    tables$adverse_events <- as_raw_table(sf$adverse_events, spec, "adverse_events")
    tables$hypoglycaemia <- as_raw_table(sf$hypoglycaemia, spec, "hypoglycaemia")
    tables <- tables[purrr::map_chr(spec$tables, "name")]
    list(tables = tables, latent = list(patients = v$patients, visits = v$visits, safety = sf$latent))
  })
  if (keep_latent) out else out$tables
}
