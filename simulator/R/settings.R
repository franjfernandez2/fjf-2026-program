# Operational settings of each study: sites, recruitment, screening and the distributions of
# characteristics that no outcome depends on. They are public judgements, not design
# parameters, so they are not in the assumptions file and are not part of the sealed truth.

study_settings <- list(
  `201` = list(
    sites = 20, first_screening = "2027-02-01", recruitment_days = 240,
    tablets_per_day = 2
  ),
  `301` = list(
    sites = 32, first_screening = "2028-03-06", recruitment_days = 300,
    tablets_per_day = 1
  )
)

shared_settings <- list(
  # Screening: besides HbA1c out of range, a share fails another criterion.
  other_screen_failure = 0.12,
  age = c(mean = 57, sd = 10),
  female_share = 0.45,
  race = c(
    "WHITE" = 0.70, "ASIAN" = 0.12, "BLACK OR AFRICAN AMERICAN" = 0.10, "OTHER" = 0.05,
    "AMERICAN INDIAN OR ALASKA NATIVE" = 0.02, "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER" = 0.01
  ),
  ethnicity = c("NOT HISPANIC OR LATINO" = 0.78, "HISPANIC OR LATINO" = 0.20, "NOT REPORTED" = 0.02),
  height_cm = list(M = c(176, 7), F = c(162, 6.5)),
  weight_kg = c(mean = 87, sd = 17),
  diabetes_years = c(shape = 2, scale = 3.5),
  metformin_mg = c(`1000` = 0.10, `1500` = 0.35, `2000` = 0.40, `2550` = 0.15),
  vitals = list(sysbp = c(132, 14), diabp = c(80, 9), pulse = c(74, 10)),
  # Baseline HbA1c differs from the screening value by measurement noise.
  baseline_noise_sd = 0.25,
  # Fasting plasma glucose from HbA1c (mg/dL): an ADAG-like line, shifted to fasting values.
  fpg = c(slope = 28.7, intercept = -66.7, sd = 18),
  missing_at_visit = 0.03,
  washout_weeks = 12,
  treatment_discontinuation_reasons = c(
    "ADVERSE EVENT" = 0.40, "WITHDRAWAL BY SUBJECT" = 0.25, "LACK OF EFFICACY" = 0.15,
    "PHYSICIAN DECISION" = 0.10, "OTHER" = 0.10
  ),
  study_withdrawal_reasons = c("WITHDRAWAL BY SUBJECT" = 0.6, "LOST TO FOLLOW-UP" = 0.3, "OTHER" = 0.1),
  rescue_drugs = c("GLICLAZIDE", "INSULIN GLARGINE", "EMPAGLIFLOZIN", "PIOGLITAZONE"),
  missed_dose_share = 0.05
)
