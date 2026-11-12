# Conformance of the SDTM of FJF-2026-301

The SDTM package (target `sdtm_301_files`, with `sdtm_301_define`) was validated with
Pinnacle 21 Community, engine 2304.3, configuration *SDTM-IG 3.4 (FDA)*, CDISC SDTM
terminology 2024-03-29, by [`tools/run-p21-sdtm.sh`](../../tools/run-p21-sdtm.sh). The report
stays local (`studies/301/output/p21`, ignored by git). The data were blinded: the arms are
the dummy assignment of [ADR 0027](../../docs/adr/0027-blinded-data-review-and-lock.md).

CDISC CORE was also run (day 11; see *CDISC CORE* below).

## What changed after the first run

The first run had one reject (no TS dataset) and found real problems, which were fixed in the
programs, not explained away:

- Trial design domains (TA, TE, TV, TI, TS) and SE were missing; EPOCH was missing everywhere.
- RFPENDTC was the end-of-study page date, but some visits, samples and events come later. It
  is now the last date of contact.
- EXENDTC could fall after the last dose. Each bottle now ends the day before its return, and
  never after the last dose.
- eGFR used the CRF code EGFR, which CDISC terminology uses for *Epidermal Growth Factor
  Receptor*. It is now GFRE, with the CDISC unit `mL/min/1.73 m2`.
- Screen failures had a DSSCAT outside the codelist; DS and CE keys were not unique; DM lacked
  DTHDTC and ACTARMUD; AE lacked AEENRTPT for events with no end date; variable order in CM,
  LB and TV did not follow the IG.
- IE (criteria not met by screen failures) was added from the collected criterion text.

## Issues that remain, and why

| Domain | Rule | Issue | Found | Explanation |
| --- | --- | --- | ---: | --- |
| AE | SD0009 | No seriousness criterion when AESER = Y | 19 | The CRF collects *serious* but not the criteria (hospitalisation, death, ...). Not invented. |
| AE | SD0057 | MedDRA codes and HLT/HLGT not found | 8 | The project's coding dictionary has the MedDRA structure but no MedDRA codes or HLT/HLGT: MedDRA needs a licence. |
| AE | SD1097 | No treatment-emergent flag | 400 | Treatment emergence is derived in ADaM (ADAE.TRTEMFL), from the SAP's definition, so it is defined once. |
| AE | SD1333 | Recovered, but no end date | 4 | The four events whose end was before their start (CR08): the end date as collected is in SUPPAE.AEENRAW. |
| CE | SD1039 | CECAT redundant with CETERM | 45 | CECAT is needed for CESCAT (the ADA level). The CRF page records only hypoglycaemia. |
| CE | SD1201 | Duplicate records | 2 | Two hypoglycaemic events on the same day for one subject, with different CESPID. Both happened. |
| CM | SD1078 | CMENDTC empty in every record | 1 | No medication was recorded as stopped. Kept, as the CRF field exists. |
| CM | SD1339 | EPOCH missing with a start date | 268 | Background metformin started before informed consent, outside every element of SE. |
| DM | SD0002 | COUNTRY null | 374 | The CRF does not collect the country of the site, and none is invented. |
| DM | SD0002 | SEX null | 74 | Screen failures have no demography page in this CRF. |
| DM | SD1149 | Expected variable empty | 3 | DTHDTC and DTHFL (no deaths) and ACTARMUD (no unplanned arm). |
| DS | SD1274 | DSTERM is OTHER | 5 | The CRF's reason list has OTHER without a free-text field. |
| EX | SD1076 | VISITNUM, VISIT added | 2 | The dispensing visit, kept to link EX with DA. |
| LB | CT2002 | LBORRESU not in the unit codelist | 1172 | The eGFR unit as collected (`mL/min/1.73m2`); LBSTRESU uses the CDISC term. |
| LB | CT2002 | LBSTRESC not in its codelist | 10808 | Every numeric result is flagged. The codelist is for character results; this engine checks all of them. |
| TS | SD1076 | TSVAL1 added | 1 | The title is longer than 200 characters, so its end is in TSVAL1, as the IG says. |
| TS | SD1312, SD2240–SD2266 | No pharmacologic class, no SNOMED/UNII codes | 7 | FJF-2026 is fictional: it has no UNII or class. Indication and treatments are given as text. FCNTRY is not collected. |

No issue was found in the define.xml.

## Double programming

An independent second programming in base R ([`qc/301/sdtm_qc.R`](../../qc/301/sdtm_qc.R))
rebuilds DM, SE, DS, IE, EX, CM, AE, LB, VS and CE from the raw CSV files, without the
production programs or their helpers. The target `sdtm_301_qc_comparison` compares each domain
with `diffdf`, and `sdtm_301_qc_check` stops the pipeline if any differs. All ten match.

The first comparison found one difference, in the QC program: it read year-only birth dates
(`UNK-UNK-1964`) as invalid. SV, DA, the SUPP-- datasets and the trial design domains are not
double programmed: they copy or reshape one raw column, or the protocol file, and their tests
check them. Both programs have the same author, which limits their independence (F3 risk
register).

# Conformance of the ADaM of FJF-2026-301

The ADaM package (targets `adam_301_files` and `adam_301_define`, folder `data/adam/301`) was
validated with Pinnacle 21 Community, engine 2304.3, configuration *ADaM-IG 1.3 (FDA)*, CDISC
ADaM terminology 2024-03-29, by [`tools/run-p21-adam-study.sh`](../../tools/run-p21-adam-study.sh). The
data were still blinded.

## What changed after the first run

- Record-level flags (ONTRTFL, PRERESFL, TRTEMFL) were Y/N. ADaMIG wants Y or null, so they
  are now Y or empty. The analysis input (`adeff_analysis_301()`) turns empty back into N
  for the F2 models.
- Three labels differed from ADaMIG: AWHI (*Analysis Window Ending Timepoint*), STARTDT
  (*Time-to-Event Origin Date for Subject*) and STRATAR (*Strata Used for Randomization*).
- Before that run, the names were aligned with the frozen outputs specification
  (`docs/sap/fjf-2026-301-outputs.yml`): STRATAR instead of STRAT1R, RANDFL in ADSL,
  CRIT1/CRIT1FL in ADEFF. A test checks that every `AD*.VAR` named there exists.

## Issues that remain, and why

| Dataset | Rule | Issue | Found | Explanation |
| --- | --- | --- | ---: | --- |
| ADAE | AD0047 | Required variable AEBDSYCD (and the other MedDRA codes) not present | 1 | The project's dictionary has no MedDRA codes: MedDRA needs a licence (as SD0057 in SDTM). |
| GLOBAL | AD1024-AD1026 | Traceability rules not executed (no DM, AE, EX) | 3 | Giving P21 the SDTM folder makes it validate the SDTM domains as ADaM. Traceability is checked by the second programming instead (below). |

## Second programming

`qc/301/adam_qc.R` derives the analysis-critical variables again from the SDTM XPT files. It
uses base R and none of the production helpers:
- ADSL: dates, compliance, baselines, PPROTFL;
- ADEFF: windows, the selected values, BASE/CHG, flags, CRIT1FL;
- ADTTE: both parameters;
- ADAE: start-date imputation and TRTEMFL;
- ADHYPO;
- ADLB and ADVS: baseline, change, flags, WORSTFL.

`compare_adam_301()` compares the two with diffdf, and the target `adam_301_qc_check` stops
the pipeline on any difference. On the blinded data all seven datasets agree on the first
run: 300 + 2,400 + 600 + 400 + 45 + 10,808 + 9,848 records.

# CDISC CORE

CDISC CORE v1.0.0 (`core-mac-apple-silicon.zip` from the official release of
`cdisc-org/cdisc-rules-engine`, SHA-256 checked against the release, kept outside the
repository) was run with its published rules by
[`tools/run-core-study.sh`](../../tools/run-core-study.sh). Both define.xml files now name the
CDISC terminology version in `def:Standards`. CORE takes it from there when the define is
Define-XML 2.1, and P21 still finds no define issue.

- **SDTMIG 3.4**: 562 rules. 347 passed, 146 did not apply, and 42 failed inside CORE
  ("rule execution error"; CORE's own problem, not the data's). 27 reported issues, after the
  SE fix below; the first run had 28.
- **ADaMIG 1.3**: CORE v1.0.0 has no published ADaM rules ("No rules were selected"). Only
  draft rules exist, which the F1 benchmark showed are not a conformance check. ADaM rests on
  P21 and the second programming.

## SDTM issues CORE reports

| Domain | Rule | Records | Explanation |
| --- | --- | ---: | --- |
| all | CORE-000510 | 1 per domain | "Split dataset name is not 3 or 4 characters": no dataset is split. A false positive. |
| SUPPAE, SUPPCE | CORE-000206 | 2, 68 | IDVARVAL not found in the parent. Every IDVARVAL matches its --SEQ (checked), so this is a false positive. |
| SUPPDM | CORE-002056, CORE-002084 | 600 | "Trial design datasets missing", "Missing Define-XML". Both are present; a false positive on a SUPP-- dataset. |
| SV | CORE-000544 | 2,991 | SVSEQ not unique. SV has no --SEQ in SDTMIG 3.4. |
| AE | CORE-000024, CORE-000334 | 400, 1 | No MedDRA codes (licence), as P21 SD0057. |
| AE | CORE-000266 | 19 | No seriousness criteria, as P21 SD0009. |
| AE | CORE-000659 | 4 | As P21 SD1333 (CR08 events, end date in SUPPAE). |
| AE | CORE-002034 | 400 | No AETRTEM: treatment emergence is derived once, in ADAE.TRTEMFL. |
| AE, CM, DA | CORE-000701 | 17, 303, 15 | EPOCH empty: partial dates (AE), background metformin started before consent (CM), records outside every element. |
| CE | CORE-000196 | 45 | As P21 SD1039. |
| CM | CORE-002006, CORE-002077 | 2, 1 | CMENDTC/CMENDY empty, as P21 SD1078. |
| DA | CORE-000774 | 15 | Bottles not returned (DASTAT = NOT DONE): the CRF has no reason field. |
| DM | CORE-000356, CORE-002030 | 448, 3 | Screen failures have no demography page (P21 SD0002); no deaths (SD1149). |
| DS | CORE-000161 | 5 | As P21 SD1274. |
| EX | CORE-000757 | 1,611 | EXDECOD absent: the treatment has no dictionary code (fictional drug), and there is no FA. |
| IE, TA, TV, SUPP-- | CORE-002006, CORE-002030 | 1 each | Expected or permissible variable present and empty (IEDY of screening failures, TATRANS, TV.ARMCD, QEVAL, SUPPDM IDVAR). Allowed by the IG. |
| TS | CORE-000152, -000483, -000550, -000728, -000735, -000852, -001082 | 1-3 | TSVAL1 (title over 200 characters), and no UNII or MED-RT for the fictional drug, as in P21. |

## Fixed after the first run

- **SE boundaries (CORE-000352, 594 records).** Each element ended the day before the next
  started. SDTMIG asks that an element's SEENDTC equal the next element's SESTDTC, so now:
  - screening ends, and treatment starts, on the day of the first dose;
  - treatment ends, and follow-up starts, on the day of the last dose.

  Records on those shared days take the treatment EPOCH, as before. EPOCH is unchanged in
  every domain (checked record by record against the previous build), and both second
  programmings still agree.

- **Last dose after the last bottle (CORE-000238, 129 patients).** The disposition page's
  last-dose date was 1-8 days after the end of the last returned bottle (return date minus
  1). The reviewer, acting as data manager, decided before the lock that the disposition
  date stands. The new cleaning rule CR10 extends the last bottle to it, and each case is in
  the data-issues listing. RFXENDTC now equals the last EXENDTC for all 300 dosed patients.
  Median compliance goes from 95% to 94%; the per-protocol set is unchanged (214).

After both fixes: 348 rules pass and 26 report issues.
