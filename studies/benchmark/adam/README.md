# Benchmark ADaM programs

Each program derives one ADaM dataset of the CDISC pilot from its SDTM, with [admiral](https://pharmaverse.github.io/admiral/). The specification is the pilot's ADaM `define.xml`, read by `read_define_v1()` ([ADR 0016](../../../docs/adr/0016-specifications-from-define-xml.md)). The analysis plan is inside the pilot's study report, pp. 418–468.

| Dataset | Program | Target | Result against the official dataset |
| --- | --- | --- | --- |
| ADSL | [`adsl.R`](adsl.R) | `adsl`, `adsl_spec_findings`, `adsl_vs_official` | 254 subjects, 48 variables: one difference, [D-004](../discrepancy-log.csv) |
| ADAE | planned | | |
| ADTTE | planned | | |
| ADQSADAS | planned | | |

The official datasets are used only for the comparison. No value is copied from them. Each difference has a row in the [discrepancy log](../discrepancy-log.csv).

## How the comparison works

1. `derive_adsl()` derives the dataset. `apply_spec_attributes()` then gives it the specification's variables, order, labels and types.
2. `check_against_spec()` checks it against the specification:
   - variables, order, labels and types;
   - codelist values;
   - parameters.
3. `compare_with_reference()` compares the values with the official dataset, record by record, with `diffdf`. Blank character values in the official XPT count as missing.

## Readings of the specification

The specification is sometimes silent or ambiguous. Where the reading taken here gives the same result as the official ADSL, it is recorded below and not in the discrepancy log.

| Variable | The specification says | Reading taken |
| --- | --- | --- |
| (population) | ITTFL is `Y` if ARMCD is not blank | ADSL holds randomised subjects only. The 52 screen failures have ARMCD `Scrnfail` and no ADSL record. |
| TRT01PN | numeric code of the randomised dose | The code of codelist ARMN (0, 54, 81). It is looked up from the specification, not typed in the program. |
| CUMDOSE | the arms are `ARMN=0 or 1` and `ARMN=2` | The arms by position: placebo, low dose, high dose. The codelist codes them 0, 54 and 81. |
| CUMDOSE | the dosing intervals of the high dose | An interval counts as completed if the visit that ends it has a date on or before TRTEDT. |
| TRTSDT, VISIT1DT and visit dates | `SV.SVSTDTC` at a visit | Some subjects have two SV records for one visit number; the earliest counts. |
| COMP8FL, COMP16FL, COMP24FL | `ENDDT >= date of visit` | ENDDT is not an ADSL variable. The end of study, RFENDT, is used. |
| DCREASCD | a grouping of DCDECOD | Each DSDECOD maps to the DISCREAS term of the same meaning. The exception is PROTOCOL VIOLATION with the term PROTOCOL ENTRY CRITERIA NOT MET, which maps to `I/E Not Met`. |
| VISNUMEN | `DSTERM='PROTCOL COMPLETED'` | This is a typing error. The visit of the disposition event is used, and visit 13 counts as 12. |
| DURDIS | months between VISIT1DT and DISONSDT | `admiral::derive_vars_duration()` in months, counting both days. This equals (days + 1) / 30.4375. |
| SITEGR1 | SAP 7.1: pool sites with fewer than 3 patients in any group | The pooled site 900 has at least 3 patients per group. The SAP's further step, adding the next smallest site, is not needed. `pool_sites()` stops if it ever would be. |
