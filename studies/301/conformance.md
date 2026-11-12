# Conformance of the SDTM of FJF-2026-301

The SDTM package (target `sdtm_301_files`, with `sdtm_301_define`) was validated with
Pinnacle 21 Community, engine 2304.3, configuration *SDTM-IG 3.4 (FDA)*, CDISC SDTM
terminology 2024-03-29, by [`tools/run-p21-sdtm.sh`](../../tools/run-p21-sdtm.sh). The report
stays local (`studies/301/output/p21`, ignored by git). The data were blinded: the arms are
the dummy assignment of [ADR 0027](../../docs/adr/0027-blinded-data-review-and-lock.md).

CDISC CORE was not run on this package: its engine is a third-party download that is not on
this machine, and it is left for day 9, before the lock.

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
