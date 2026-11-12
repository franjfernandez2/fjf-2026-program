-- Raw data tables of the FJF-2026 studies. Runs once, as the administrator, when the
-- database volume is first initialised (after 03-audit.sql, so the audit triggers attach).
--
-- GENERATED from docs/specs/raw-layer.yml by tools/write-raw-ddl.R. Do not edit by hand:
-- a test fails if this file and the specification disagree.
-- Specification version 0.1.

-- FJF-2026-201
CREATE TABLE raw_201.enrolment (
  record_id                integer NOT NULL,
  subject_id               text NOT NULL,
  site_id                  text NOT NULL,
  consent_date             text,
  screening_date           text,
  eligible                 text CHECK (eligible IN ('Y', 'N')),
  failed_criterion         text,
  PRIMARY KEY (record_id)
);
COMMENT ON TABLE raw_201.enrolment IS 'Screening and eligibility (one record per screened patient)';
COMMENT ON COLUMN raw_201.enrolment.record_id IS 'EDC record identifier';
COMMENT ON COLUMN raw_201.enrolment.subject_id IS 'Subject number: site and sequence, e.g. 301-012-0007';
COMMENT ON COLUMN raw_201.enrolment.site_id IS 'Site number';
COMMENT ON COLUMN raw_201.enrolment.consent_date IS 'Date of informed consent (DD-MON-YYYY)';
COMMENT ON COLUMN raw_201.enrolment.screening_date IS 'Date of the screening visit';
COMMENT ON COLUMN raw_201.enrolment.eligible IS 'All inclusion and no exclusion criteria met';
COMMENT ON COLUMN raw_201.enrolment.failed_criterion IS 'First criterion not met, for screen failures';

CREATE TABLE raw_201.randomisation (
  record_id                integer NOT NULL,
  subject_id               text NOT NULL,
  randomisation_number     text NOT NULL,
  randomisation_date       text NOT NULL,
  stratum                  text NOT NULL CHECK (stratum IN ('<=8.5', '>8.5')),
  arm_code                 text NOT NULL CHECK (arm_code IN ('PBO', 'FJF5', 'FJF10', 'FJF20', 'FJF40')),
  PRIMARY KEY (record_id)
);
COMMENT ON TABLE raw_201.randomisation IS 'Randomisation system transfer (one record per randomised patient)';
COMMENT ON COLUMN raw_201.randomisation.record_id IS 'Transfer record identifier';
COMMENT ON COLUMN raw_201.randomisation.subject_id IS 'Subject number';
COMMENT ON COLUMN raw_201.randomisation.randomisation_number IS 'Randomisation number from the list';
COMMENT ON COLUMN raw_201.randomisation.randomisation_date IS 'Date of randomisation';
COMMENT ON COLUMN raw_201.randomisation.stratum IS 'Screening HbA1c stratum as entered in the randomisation system';
COMMENT ON COLUMN raw_201.randomisation.arm_code IS 'Assigned arm (released at unblinding)';

CREATE TABLE raw_201.demography (
  record_id                integer NOT NULL,
  subject_id               text NOT NULL,
  birth_date               text,
  sex                      text CHECK (sex IN ('M', 'F')),
  race                     text CHECK (race IN ('WHITE', 'BLACK OR AFRICAN AMERICAN', 'ASIAN', 'AMERICAN INDIAN OR ALASKA NATIVE', 'NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER', 'OTHER')),
  ethnicity                text CHECK (ethnicity IN ('HISPANIC OR LATINO', 'NOT HISPANIC OR LATINO', 'NOT REPORTED')),
  height_cm                numeric,
  diabetes_diagnosis_date  text,
  metformin_daily_mg       integer,
  PRIMARY KEY (record_id)
);
COMMENT ON TABLE raw_201.demography IS 'Demography CRF';
COMMENT ON COLUMN raw_201.demography.record_id IS 'EDC record identifier';
COMMENT ON COLUMN raw_201.demography.subject_id IS 'Subject number';
COMMENT ON COLUMN raw_201.demography.birth_date IS 'Date of birth; year only or partial dates allowed';
COMMENT ON COLUMN raw_201.demography.sex IS 'Sex';
COMMENT ON COLUMN raw_201.demography.race IS 'Race';
COMMENT ON COLUMN raw_201.demography.ethnicity IS 'Ethnicity';
COMMENT ON COLUMN raw_201.demography.height_cm IS 'Height at screening (cm)';
COMMENT ON COLUMN raw_201.demography.diabetes_diagnosis_date IS 'Date of diagnosis of type 2 diabetes; partial dates allowed';
COMMENT ON COLUMN raw_201.demography.metformin_daily_mg IS 'Metformin dose at screening (mg/day)';

CREATE TABLE raw_201.visits (
  record_id                integer NOT NULL,
  subject_id               text NOT NULL,
  visit_code               text NOT NULL CHECK (visit_code IN ('V1', 'V2', 'V3', 'V4', 'V5', 'V6', 'V7', 'UNS')),
  visit_date               text,
  visit_status             text NOT NULL CHECK (visit_status IN ('DONE', 'NOT DONE')),
  PRIMARY KEY (record_id)
);
COMMENT ON TABLE raw_201.visits IS 'Visit dates, one record per scheduled or unscheduled visit';
COMMENT ON COLUMN raw_201.visits.record_id IS 'EDC record identifier';
COMMENT ON COLUMN raw_201.visits.subject_id IS 'Subject number';
COMMENT ON COLUMN raw_201.visits.visit_code IS 'Protocol visit code, or UNS for an unscheduled visit';
COMMENT ON COLUMN raw_201.visits.visit_date IS 'Date of the visit';
COMMENT ON COLUMN raw_201.visits.visit_status IS 'Whether the visit took place';

CREATE TABLE raw_201.lab_results (
  record_id                integer NOT NULL,
  subject_id               text NOT NULL,
  visit_code               text NOT NULL CHECK (visit_code IN ('V1', 'V2', 'V3', 'V4', 'V5', 'V6', 'V7', 'UNS')),
  collection_date          text,
  test_code                text NOT NULL CHECK (test_code IN ('HBA1C', 'GLUC', 'ALT', 'AST', 'BILI', 'CREAT', 'EGFR')),
  result                   text,
  unit                     text,
  ref_low                  numeric,
  ref_high                 numeric,
  fasting                  text CHECK (fasting IN ('Y', 'N')),
  PRIMARY KEY (record_id)
);
COMMENT ON TABLE raw_201.lab_results IS 'Central laboratory transfer';
COMMENT ON COLUMN raw_201.lab_results.record_id IS 'Transfer record identifier';
COMMENT ON COLUMN raw_201.lab_results.subject_id IS 'Subject number';
COMMENT ON COLUMN raw_201.lab_results.visit_code IS 'Visit at which the sample was taken';
COMMENT ON COLUMN raw_201.lab_results.collection_date IS 'Date of sample collection';
COMMENT ON COLUMN raw_201.lab_results.test_code IS 'Laboratory test';
COMMENT ON COLUMN raw_201.lab_results.result IS 'Result as reported, possibly with a qualifier such as "<"';
COMMENT ON COLUMN raw_201.lab_results.unit IS 'Unit of the result as reported';
COMMENT ON COLUMN raw_201.lab_results.ref_low IS 'Lower limit of the reference range';
COMMENT ON COLUMN raw_201.lab_results.ref_high IS 'Upper limit of the reference range';
COMMENT ON COLUMN raw_201.lab_results.fasting IS 'Patient fasting at collection';

CREATE TABLE raw_201.vital_signs (
  record_id                integer NOT NULL,
  subject_id               text NOT NULL,
  visit_code               text NOT NULL CHECK (visit_code IN ('V1', 'V2', 'V3', 'V4', 'V5', 'V6', 'V7', 'UNS')),
  measure_date             text,
  sysbp                    integer,
  diabp                    integer,
  pulse                    integer,
  weight                   numeric,
  weight_unit              text CHECK (weight_unit IN ('kg', 'lb')),
  PRIMARY KEY (record_id)
);
COMMENT ON TABLE raw_201.vital_signs IS 'Vital signs and weight CRF';
COMMENT ON COLUMN raw_201.vital_signs.record_id IS 'EDC record identifier';
COMMENT ON COLUMN raw_201.vital_signs.subject_id IS 'Subject number';
COMMENT ON COLUMN raw_201.vital_signs.visit_code IS 'Visit';
COMMENT ON COLUMN raw_201.vital_signs.measure_date IS 'Date of measurement';
COMMENT ON COLUMN raw_201.vital_signs.sysbp IS 'Systolic blood pressure (mmHg)';
COMMENT ON COLUMN raw_201.vital_signs.diabp IS 'Diastolic blood pressure (mmHg)';
COMMENT ON COLUMN raw_201.vital_signs.pulse IS 'Heart rate (beats/min)';
COMMENT ON COLUMN raw_201.vital_signs.weight IS 'Body weight, in weight_unit';
COMMENT ON COLUMN raw_201.vital_signs.weight_unit IS 'Unit of weight as entered';

CREATE TABLE raw_201.exposure (
  record_id                integer NOT NULL,
  subject_id               text NOT NULL,
  visit_code               text NOT NULL CHECK (visit_code IN ('V1', 'V2', 'V3', 'V4', 'V5', 'V6', 'V7', 'UNS')),
  dispense_date            text,
  return_date              text,
  tablets_dispensed        integer,
  tablets_returned         integer,
  PRIMARY KEY (record_id)
);
COMMENT ON TABLE raw_201.exposure IS 'Study drug dispensing and accountability';
COMMENT ON COLUMN raw_201.exposure.record_id IS 'EDC record identifier';
COMMENT ON COLUMN raw_201.exposure.subject_id IS 'Subject number';
COMMENT ON COLUMN raw_201.exposure.visit_code IS 'Visit at which the kit was dispensed';
COMMENT ON COLUMN raw_201.exposure.dispense_date IS 'Date the kit was dispensed';
COMMENT ON COLUMN raw_201.exposure.return_date IS 'Date the kit was returned';
COMMENT ON COLUMN raw_201.exposure.tablets_dispensed IS 'Tablets dispensed';
COMMENT ON COLUMN raw_201.exposure.tablets_returned IS 'Tablets returned';

CREATE TABLE raw_201.disposition (
  record_id                integer NOT NULL,
  subject_id               text NOT NULL,
  event                    text NOT NULL CHECK (event IN ('TREATMENT', 'STUDY')),
  status                   text NOT NULL CHECK (status IN ('COMPLETED', 'DISCONTINUED')),
  reason                   text CHECK (reason IN ('ADVERSE EVENT', 'LACK OF EFFICACY', 'WITHDRAWAL BY SUBJECT', 'LOST TO FOLLOW-UP', 'PHYSICIAN DECISION', 'PROTOCOL DEVIATION', 'DEATH', 'OTHER')),
  event_date               text,
  PRIMARY KEY (record_id)
);
COMMENT ON TABLE raw_201.disposition IS 'End of treatment and end of study';
COMMENT ON COLUMN raw_201.disposition.record_id IS 'EDC record identifier';
COMMENT ON COLUMN raw_201.disposition.subject_id IS 'Subject number';
COMMENT ON COLUMN raw_201.disposition.event IS 'End of study treatment or end of study';
COMMENT ON COLUMN raw_201.disposition.status IS 'Completed or discontinued';
COMMENT ON COLUMN raw_201.disposition.reason IS 'Reason for discontinuation';
COMMENT ON COLUMN raw_201.disposition.event_date IS 'Date of last dose, or of the last study contact';

CREATE TABLE raw_201.adverse_events (
  record_id                integer NOT NULL,
  subject_id               text NOT NULL,
  ae_seq                   integer NOT NULL,
  verbatim_term            text NOT NULL,
  start_date               text,
  end_date                 text,
  severity                 text CHECK (severity IN ('MILD', 'MODERATE', 'SEVERE')),
  serious                  text CHECK (serious IN ('Y', 'N')),
  related                  text CHECK (related IN ('Y', 'N')),
  action_taken             text CHECK (action_taken IN ('DOSE NOT CHANGED', 'DRUG INTERRUPTED', 'DRUG WITHDRAWN', 'NOT APPLICABLE')),
  outcome                  text CHECK (outcome IN ('RECOVERED', 'RECOVERING', 'NOT RECOVERED', 'RECOVERED WITH SEQUELAE', 'FATAL', 'UNKNOWN')),
  PRIMARY KEY (record_id)
);
COMMENT ON TABLE raw_201.adverse_events IS 'Adverse events CRF (verbatim terms, coded in F3)';
COMMENT ON COLUMN raw_201.adverse_events.record_id IS 'EDC record identifier';
COMMENT ON COLUMN raw_201.adverse_events.subject_id IS 'Subject number';
COMMENT ON COLUMN raw_201.adverse_events.ae_seq IS 'Sequence number of the event within the patient';
COMMENT ON COLUMN raw_201.adverse_events.verbatim_term IS 'Event as written by the investigator';
COMMENT ON COLUMN raw_201.adverse_events.start_date IS 'Start date; partial dates allowed';
COMMENT ON COLUMN raw_201.adverse_events.end_date IS 'End date; empty if ongoing';
COMMENT ON COLUMN raw_201.adverse_events.severity IS 'Maximum severity';
COMMENT ON COLUMN raw_201.adverse_events.serious IS 'Serious adverse event';
COMMENT ON COLUMN raw_201.adverse_events.related IS 'Related to study drug in the investigator''s opinion';
COMMENT ON COLUMN raw_201.adverse_events.action_taken IS 'Action taken with study drug';
COMMENT ON COLUMN raw_201.adverse_events.outcome IS 'Outcome';

CREATE TABLE raw_201.concomitant_meds (
  record_id                integer NOT NULL,
  subject_id               text NOT NULL,
  cm_seq                   integer NOT NULL,
  drug_name                text NOT NULL,
  indication               text,
  daily_dose               text,
  start_date               text,
  end_date                 text,
  rescue                   text CHECK (rescue IN ('Y', 'N')),
  PRIMARY KEY (record_id)
);
COMMENT ON TABLE raw_201.concomitant_meds IS 'Prior and concomitant medication CRF, including rescue';
COMMENT ON COLUMN raw_201.concomitant_meds.record_id IS 'EDC record identifier';
COMMENT ON COLUMN raw_201.concomitant_meds.subject_id IS 'Subject number';
COMMENT ON COLUMN raw_201.concomitant_meds.cm_seq IS 'Sequence number of the medication within the patient';
COMMENT ON COLUMN raw_201.concomitant_meds.drug_name IS 'Medication as written by the investigator';
COMMENT ON COLUMN raw_201.concomitant_meds.indication IS 'Reason for the medication';
COMMENT ON COLUMN raw_201.concomitant_meds.daily_dose IS 'Daily dose with unit, as written';
COMMENT ON COLUMN raw_201.concomitant_meds.start_date IS 'Start date; partial dates allowed';
COMMENT ON COLUMN raw_201.concomitant_meds.end_date IS 'End date; empty if ongoing';
COMMENT ON COLUMN raw_201.concomitant_meds.rescue IS 'Started as rescue medication under the protocol rule';

CREATE TABLE raw_201.hypoglycaemia (
  record_id                integer NOT NULL,
  subject_id               text NOT NULL,
  event_seq                integer NOT NULL,
  event_date               text,
  glucose_mg_dl            integer,
  level                    text CHECK (level IN ('1', '2', '3')),
  assistance               text CHECK (assistance IN ('Y', 'N')),
  PRIMARY KEY (record_id)
);
COMMENT ON TABLE raw_201.hypoglycaemia IS 'Hypoglycaemia CRF (from the glucose meter and diary)';
COMMENT ON COLUMN raw_201.hypoglycaemia.record_id IS 'EDC record identifier';
COMMENT ON COLUMN raw_201.hypoglycaemia.subject_id IS 'Subject number';
COMMENT ON COLUMN raw_201.hypoglycaemia.event_seq IS 'Sequence number of the episode within the patient';
COMMENT ON COLUMN raw_201.hypoglycaemia.event_date IS 'Date of the episode';
COMMENT ON COLUMN raw_201.hypoglycaemia.glucose_mg_dl IS 'Lowest glucose measured (mg/dL)';
COMMENT ON COLUMN raw_201.hypoglycaemia.level IS 'Level 1 (54 to 69 mg/dL), 2 (below 54) or 3 (needed assistance)';
COMMENT ON COLUMN raw_201.hypoglycaemia.assistance IS 'Needed the help of another person';

-- FJF-2026-301
CREATE TABLE raw_301.enrolment (
  record_id                integer NOT NULL,
  subject_id               text NOT NULL,
  site_id                  text NOT NULL,
  consent_date             text,
  screening_date           text,
  eligible                 text CHECK (eligible IN ('Y', 'N')),
  failed_criterion         text,
  PRIMARY KEY (record_id)
);
COMMENT ON TABLE raw_301.enrolment IS 'Screening and eligibility (one record per screened patient)';
COMMENT ON COLUMN raw_301.enrolment.record_id IS 'EDC record identifier';
COMMENT ON COLUMN raw_301.enrolment.subject_id IS 'Subject number: site and sequence, e.g. 301-012-0007';
COMMENT ON COLUMN raw_301.enrolment.site_id IS 'Site number';
COMMENT ON COLUMN raw_301.enrolment.consent_date IS 'Date of informed consent (DD-MON-YYYY)';
COMMENT ON COLUMN raw_301.enrolment.screening_date IS 'Date of the screening visit';
COMMENT ON COLUMN raw_301.enrolment.eligible IS 'All inclusion and no exclusion criteria met';
COMMENT ON COLUMN raw_301.enrolment.failed_criterion IS 'First criterion not met, for screen failures';

CREATE TABLE raw_301.randomisation (
  record_id                integer NOT NULL,
  subject_id               text NOT NULL,
  randomisation_number     text NOT NULL,
  randomisation_date       text NOT NULL,
  stratum                  text NOT NULL CHECK (stratum IN ('<=8.5', '>8.5')),
  arm_code                 text NOT NULL CHECK (arm_code IN ('PBO', 'FJF20')),
  PRIMARY KEY (record_id)
);
COMMENT ON TABLE raw_301.randomisation IS 'Randomisation system transfer (one record per randomised patient)';
COMMENT ON COLUMN raw_301.randomisation.record_id IS 'Transfer record identifier';
COMMENT ON COLUMN raw_301.randomisation.subject_id IS 'Subject number';
COMMENT ON COLUMN raw_301.randomisation.randomisation_number IS 'Randomisation number from the list';
COMMENT ON COLUMN raw_301.randomisation.randomisation_date IS 'Date of randomisation';
COMMENT ON COLUMN raw_301.randomisation.stratum IS 'Screening HbA1c stratum as entered in the randomisation system';
COMMENT ON COLUMN raw_301.randomisation.arm_code IS 'Assigned arm (released at unblinding)';

CREATE TABLE raw_301.demography (
  record_id                integer NOT NULL,
  subject_id               text NOT NULL,
  birth_date               text,
  sex                      text CHECK (sex IN ('M', 'F')),
  race                     text CHECK (race IN ('WHITE', 'BLACK OR AFRICAN AMERICAN', 'ASIAN', 'AMERICAN INDIAN OR ALASKA NATIVE', 'NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER', 'OTHER')),
  ethnicity                text CHECK (ethnicity IN ('HISPANIC OR LATINO', 'NOT HISPANIC OR LATINO', 'NOT REPORTED')),
  height_cm                numeric,
  diabetes_diagnosis_date  text,
  metformin_daily_mg       integer,
  PRIMARY KEY (record_id)
);
COMMENT ON TABLE raw_301.demography IS 'Demography CRF';
COMMENT ON COLUMN raw_301.demography.record_id IS 'EDC record identifier';
COMMENT ON COLUMN raw_301.demography.subject_id IS 'Subject number';
COMMENT ON COLUMN raw_301.demography.birth_date IS 'Date of birth; year only or partial dates allowed';
COMMENT ON COLUMN raw_301.demography.sex IS 'Sex';
COMMENT ON COLUMN raw_301.demography.race IS 'Race';
COMMENT ON COLUMN raw_301.demography.ethnicity IS 'Ethnicity';
COMMENT ON COLUMN raw_301.demography.height_cm IS 'Height at screening (cm)';
COMMENT ON COLUMN raw_301.demography.diabetes_diagnosis_date IS 'Date of diagnosis of type 2 diabetes; partial dates allowed';
COMMENT ON COLUMN raw_301.demography.metformin_daily_mg IS 'Metformin dose at screening (mg/day)';

CREATE TABLE raw_301.visits (
  record_id                integer NOT NULL,
  subject_id               text NOT NULL,
  visit_code               text NOT NULL CHECK (visit_code IN ('V1', 'V2', 'V3', 'V4', 'V5', 'V6', 'V7', 'V8', 'V9', 'V10', 'UNS')),
  visit_date               text,
  visit_status             text NOT NULL CHECK (visit_status IN ('DONE', 'NOT DONE')),
  PRIMARY KEY (record_id)
);
COMMENT ON TABLE raw_301.visits IS 'Visit dates, one record per scheduled or unscheduled visit';
COMMENT ON COLUMN raw_301.visits.record_id IS 'EDC record identifier';
COMMENT ON COLUMN raw_301.visits.subject_id IS 'Subject number';
COMMENT ON COLUMN raw_301.visits.visit_code IS 'Protocol visit code, or UNS for an unscheduled visit';
COMMENT ON COLUMN raw_301.visits.visit_date IS 'Date of the visit';
COMMENT ON COLUMN raw_301.visits.visit_status IS 'Whether the visit took place';

CREATE TABLE raw_301.lab_results (
  record_id                integer NOT NULL,
  subject_id               text NOT NULL,
  visit_code               text NOT NULL CHECK (visit_code IN ('V1', 'V2', 'V3', 'V4', 'V5', 'V6', 'V7', 'V8', 'V9', 'V10', 'UNS')),
  collection_date          text,
  test_code                text NOT NULL CHECK (test_code IN ('HBA1C', 'GLUC', 'ALT', 'AST', 'BILI', 'CREAT', 'EGFR')),
  result                   text,
  unit                     text,
  ref_low                  numeric,
  ref_high                 numeric,
  fasting                  text CHECK (fasting IN ('Y', 'N')),
  PRIMARY KEY (record_id)
);
COMMENT ON TABLE raw_301.lab_results IS 'Central laboratory transfer';
COMMENT ON COLUMN raw_301.lab_results.record_id IS 'Transfer record identifier';
COMMENT ON COLUMN raw_301.lab_results.subject_id IS 'Subject number';
COMMENT ON COLUMN raw_301.lab_results.visit_code IS 'Visit at which the sample was taken';
COMMENT ON COLUMN raw_301.lab_results.collection_date IS 'Date of sample collection';
COMMENT ON COLUMN raw_301.lab_results.test_code IS 'Laboratory test';
COMMENT ON COLUMN raw_301.lab_results.result IS 'Result as reported, possibly with a qualifier such as "<"';
COMMENT ON COLUMN raw_301.lab_results.unit IS 'Unit of the result as reported';
COMMENT ON COLUMN raw_301.lab_results.ref_low IS 'Lower limit of the reference range';
COMMENT ON COLUMN raw_301.lab_results.ref_high IS 'Upper limit of the reference range';
COMMENT ON COLUMN raw_301.lab_results.fasting IS 'Patient fasting at collection';

CREATE TABLE raw_301.vital_signs (
  record_id                integer NOT NULL,
  subject_id               text NOT NULL,
  visit_code               text NOT NULL CHECK (visit_code IN ('V1', 'V2', 'V3', 'V4', 'V5', 'V6', 'V7', 'V8', 'V9', 'V10', 'UNS')),
  measure_date             text,
  sysbp                    integer,
  diabp                    integer,
  pulse                    integer,
  weight                   numeric,
  weight_unit              text CHECK (weight_unit IN ('kg', 'lb')),
  PRIMARY KEY (record_id)
);
COMMENT ON TABLE raw_301.vital_signs IS 'Vital signs and weight CRF';
COMMENT ON COLUMN raw_301.vital_signs.record_id IS 'EDC record identifier';
COMMENT ON COLUMN raw_301.vital_signs.subject_id IS 'Subject number';
COMMENT ON COLUMN raw_301.vital_signs.visit_code IS 'Visit';
COMMENT ON COLUMN raw_301.vital_signs.measure_date IS 'Date of measurement';
COMMENT ON COLUMN raw_301.vital_signs.sysbp IS 'Systolic blood pressure (mmHg)';
COMMENT ON COLUMN raw_301.vital_signs.diabp IS 'Diastolic blood pressure (mmHg)';
COMMENT ON COLUMN raw_301.vital_signs.pulse IS 'Heart rate (beats/min)';
COMMENT ON COLUMN raw_301.vital_signs.weight IS 'Body weight, in weight_unit';
COMMENT ON COLUMN raw_301.vital_signs.weight_unit IS 'Unit of weight as entered';

CREATE TABLE raw_301.exposure (
  record_id                integer NOT NULL,
  subject_id               text NOT NULL,
  visit_code               text NOT NULL CHECK (visit_code IN ('V1', 'V2', 'V3', 'V4', 'V5', 'V6', 'V7', 'V8', 'V9', 'V10', 'UNS')),
  dispense_date            text,
  return_date              text,
  tablets_dispensed        integer,
  tablets_returned         integer,
  PRIMARY KEY (record_id)
);
COMMENT ON TABLE raw_301.exposure IS 'Study drug dispensing and accountability';
COMMENT ON COLUMN raw_301.exposure.record_id IS 'EDC record identifier';
COMMENT ON COLUMN raw_301.exposure.subject_id IS 'Subject number';
COMMENT ON COLUMN raw_301.exposure.visit_code IS 'Visit at which the kit was dispensed';
COMMENT ON COLUMN raw_301.exposure.dispense_date IS 'Date the kit was dispensed';
COMMENT ON COLUMN raw_301.exposure.return_date IS 'Date the kit was returned';
COMMENT ON COLUMN raw_301.exposure.tablets_dispensed IS 'Tablets dispensed';
COMMENT ON COLUMN raw_301.exposure.tablets_returned IS 'Tablets returned';

CREATE TABLE raw_301.disposition (
  record_id                integer NOT NULL,
  subject_id               text NOT NULL,
  event                    text NOT NULL CHECK (event IN ('TREATMENT', 'STUDY')),
  status                   text NOT NULL CHECK (status IN ('COMPLETED', 'DISCONTINUED')),
  reason                   text CHECK (reason IN ('ADVERSE EVENT', 'LACK OF EFFICACY', 'WITHDRAWAL BY SUBJECT', 'LOST TO FOLLOW-UP', 'PHYSICIAN DECISION', 'PROTOCOL DEVIATION', 'DEATH', 'OTHER')),
  event_date               text,
  PRIMARY KEY (record_id)
);
COMMENT ON TABLE raw_301.disposition IS 'End of treatment and end of study';
COMMENT ON COLUMN raw_301.disposition.record_id IS 'EDC record identifier';
COMMENT ON COLUMN raw_301.disposition.subject_id IS 'Subject number';
COMMENT ON COLUMN raw_301.disposition.event IS 'End of study treatment or end of study';
COMMENT ON COLUMN raw_301.disposition.status IS 'Completed or discontinued';
COMMENT ON COLUMN raw_301.disposition.reason IS 'Reason for discontinuation';
COMMENT ON COLUMN raw_301.disposition.event_date IS 'Date of last dose, or of the last study contact';

CREATE TABLE raw_301.adverse_events (
  record_id                integer NOT NULL,
  subject_id               text NOT NULL,
  ae_seq                   integer NOT NULL,
  verbatim_term            text NOT NULL,
  start_date               text,
  end_date                 text,
  severity                 text CHECK (severity IN ('MILD', 'MODERATE', 'SEVERE')),
  serious                  text CHECK (serious IN ('Y', 'N')),
  related                  text CHECK (related IN ('Y', 'N')),
  action_taken             text CHECK (action_taken IN ('DOSE NOT CHANGED', 'DRUG INTERRUPTED', 'DRUG WITHDRAWN', 'NOT APPLICABLE')),
  outcome                  text CHECK (outcome IN ('RECOVERED', 'RECOVERING', 'NOT RECOVERED', 'RECOVERED WITH SEQUELAE', 'FATAL', 'UNKNOWN')),
  PRIMARY KEY (record_id)
);
COMMENT ON TABLE raw_301.adverse_events IS 'Adverse events CRF (verbatim terms, coded in F3)';
COMMENT ON COLUMN raw_301.adverse_events.record_id IS 'EDC record identifier';
COMMENT ON COLUMN raw_301.adverse_events.subject_id IS 'Subject number';
COMMENT ON COLUMN raw_301.adverse_events.ae_seq IS 'Sequence number of the event within the patient';
COMMENT ON COLUMN raw_301.adverse_events.verbatim_term IS 'Event as written by the investigator';
COMMENT ON COLUMN raw_301.adverse_events.start_date IS 'Start date; partial dates allowed';
COMMENT ON COLUMN raw_301.adverse_events.end_date IS 'End date; empty if ongoing';
COMMENT ON COLUMN raw_301.adverse_events.severity IS 'Maximum severity';
COMMENT ON COLUMN raw_301.adverse_events.serious IS 'Serious adverse event';
COMMENT ON COLUMN raw_301.adverse_events.related IS 'Related to study drug in the investigator''s opinion';
COMMENT ON COLUMN raw_301.adverse_events.action_taken IS 'Action taken with study drug';
COMMENT ON COLUMN raw_301.adverse_events.outcome IS 'Outcome';

CREATE TABLE raw_301.concomitant_meds (
  record_id                integer NOT NULL,
  subject_id               text NOT NULL,
  cm_seq                   integer NOT NULL,
  drug_name                text NOT NULL,
  indication               text,
  daily_dose               text,
  start_date               text,
  end_date                 text,
  rescue                   text CHECK (rescue IN ('Y', 'N')),
  PRIMARY KEY (record_id)
);
COMMENT ON TABLE raw_301.concomitant_meds IS 'Prior and concomitant medication CRF, including rescue';
COMMENT ON COLUMN raw_301.concomitant_meds.record_id IS 'EDC record identifier';
COMMENT ON COLUMN raw_301.concomitant_meds.subject_id IS 'Subject number';
COMMENT ON COLUMN raw_301.concomitant_meds.cm_seq IS 'Sequence number of the medication within the patient';
COMMENT ON COLUMN raw_301.concomitant_meds.drug_name IS 'Medication as written by the investigator';
COMMENT ON COLUMN raw_301.concomitant_meds.indication IS 'Reason for the medication';
COMMENT ON COLUMN raw_301.concomitant_meds.daily_dose IS 'Daily dose with unit, as written';
COMMENT ON COLUMN raw_301.concomitant_meds.start_date IS 'Start date; partial dates allowed';
COMMENT ON COLUMN raw_301.concomitant_meds.end_date IS 'End date; empty if ongoing';
COMMENT ON COLUMN raw_301.concomitant_meds.rescue IS 'Started as rescue medication under the protocol rule';

CREATE TABLE raw_301.hypoglycaemia (
  record_id                integer NOT NULL,
  subject_id               text NOT NULL,
  event_seq                integer NOT NULL,
  event_date               text,
  glucose_mg_dl            integer,
  level                    text CHECK (level IN ('1', '2', '3')),
  assistance               text CHECK (assistance IN ('Y', 'N')),
  PRIMARY KEY (record_id)
);
COMMENT ON TABLE raw_301.hypoglycaemia IS 'Hypoglycaemia CRF (from the glucose meter and diary)';
COMMENT ON COLUMN raw_301.hypoglycaemia.record_id IS 'EDC record identifier';
COMMENT ON COLUMN raw_301.hypoglycaemia.subject_id IS 'Subject number';
COMMENT ON COLUMN raw_301.hypoglycaemia.event_seq IS 'Sequence number of the episode within the patient';
COMMENT ON COLUMN raw_301.hypoglycaemia.event_date IS 'Date of the episode';
COMMENT ON COLUMN raw_301.hypoglycaemia.glucose_mg_dl IS 'Lowest glucose measured (mg/dL)';
COMMENT ON COLUMN raw_301.hypoglycaemia.level IS 'Level 1 (54 to 69 mg/dL), 2 (below 54) or 3 (needed assistance)';
COMMENT ON COLUMN raw_301.hypoglycaemia.assistance IS 'Needed the help of another person';

