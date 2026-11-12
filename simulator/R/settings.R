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

# Safety: adverse event terms as an investigator might write them (coded in F3), and the
# laboratory model. Terms are common words, not a coding dictionary.
safety_settings <- list(
  background_terms = list(
    "HEADACHE" = c("Headache", "HEADACHE", "headache", "Tension headache"),
    "NASOPHARYNGITIS" = c("Nasopharyngitis", "Common cold", "COLD"),
    "UPPER RESPIRATORY TRACT INFECTION" = c("Upper respiratory tract infection", "URTI", "Chest cold"),
    "BACK PAIN" = c("Back pain", "LOWER BACK PAIN", "Backache"),
    "ARTHRALGIA" = c("Arthralgia", "Joint pain", "Knee pain"),
    "URINARY TRACT INFECTION" = c("Urinary tract infection", "UTI", "Cystitis"),
    "HYPERTENSION" = c("Hypertension", "High blood pressure", "WORSENING HYPERTENSION"),
    "DIZZINESS" = c("Dizziness", "Dizzy", "DIZZINESS"),
    "COUGH" = c("Cough", "COUGH", "Dry cough"),
    "INFLUENZA" = c("Influenza", "Flu", "FLU-LIKE ILLNESS"),
    "NAUSEA" = c("Nausea", "NAUSEA", "Feeling sick"),
    "DIARRHOEA" = c("Diarrhoea", "Diarrhea", "LOOSE STOOLS")
  ),
  background_weights = c(12, 10, 9, 7, 6, 5, 5, 4, 4, 4, 2, 2),
  gi_terms = list(
    "NAUSEA" = c("Nausea", "NAUSEA", "Feeling sick", "Nausea after tablets"),
    "DIARRHOEA" = c("Diarrhoea", "Diarrhea", "LOOSE STOOLS"),
    "VOMITING" = c("Vomiting", "VOMITING", "Vomited"),
    "ABDOMINAL PAIN" = c("Abdominal pain", "Stomach ache", "UPPER ABDOMINAL PAIN")
  ),
  serious_terms = list(
    "PNEUMONIA" = c("Pneumonia", "PNEUMONIA", "Community acquired pneumonia"),
    "MYOCARDIAL INFARCTION" = c("Myocardial infarction", "Heart attack", "NSTEMI"),
    "CELLULITIS" = c("Cellulitis", "Cellulitis left leg"),
    "ATRIAL FIBRILLATION" = c("Atrial fibrillation", "AF", "New onset atrial fibrillation"),
    "OSTEOARTHRITIS" = c("Osteoarthritis", "OA knee, admitted for knee replacement")
  ),
  hepatic_terms = c("ALT increased", "Liver enzymes elevated", "Raised transaminases"),
  hypo_terms = c("Hypoglycaemia", "Hypoglycemia", "Low blood sugar"),
  extra_events = 0.7,
  severity = c(MILD = 0.60, MODERATE = 0.30, SEVERE = 0.10),
  related_gi = 0.70,
  related_other = 0.15,
  ongoing = 0.08,
  interrupted = 0.03,
  alt_placebo = 0.005,
  hypo_level3 = 0.002,
  # Hypoglycaemia after rescue with a sulfonylurea or insulin.
  hypo_after_rescue = 0.30,
  labs = list(
    ALT = list(unit = "U/L", ref = c(7, 56), log_mean = log(24), patient_sd = 0.35, visit_sd = 0.20),
    AST = list(unit = "U/L", ref = c(10, 40), log_mean = log(21), patient_sd = 0.30, visit_sd = 0.15),
    BILI = list(unit = "mg/dL", ref = c(0.1, 1.2), log_mean = log(0.6), patient_sd = 0.30, visit_sd = 0.15)
  ),
  creatinine = list(
    M = log(0.95), F = log(0.75), patient_sd = 0.15, visit_sd = 0.06,
    ref_M = c(0.7, 1.3), ref_F = c(0.5, 1.1)
  )
)
