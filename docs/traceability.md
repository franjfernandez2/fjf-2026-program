# Traceability matrix

[`traceability-matrix.csv`](traceability-matrix.csv) links every table, listing and figure to the plan that asked for it and the code that produced and checked it. A reviewer can start from any number in a report and follow it back to a line of the SAP.

The first row is an example and is removed when the first real output is added.

| Column | Content | Example |
| --- | --- | --- |
| `output_id` | Output number, as in the report | `T-14.2.1` |
| `study` | Study number | `FJF-2026-301` |
| `type` | `table`, `listing` or `figure` | `table` |
| `title` | Title as printed | Change from baseline in HbA1c at week 26 |
| `sap_section` | SAP section that specifies it | `9.2` |
| `population` | Analysis set | `FAS` |
| `adam_datasets` | ADaM datasets read, separated by `;` | `ADSL; ADLB` |
| `key_variables` | Parameters, visits and variables used | `PARAMCD=HBA1C; AVISITN=26; CHG` |
| `program` | Primary program | `studies/301/tlf/t_14_2_1.R` |
| `qc_program` | Independent QC program | `qc/301/t_14_2_1_qc.R` |
| `qc_status` | `not started`, `in progress`, `match` or `mismatch` | `match` |
| `shell_id` | Table shell in `docs/shells/` | `T-14.2.1` |

An output is final only when `qc_status` is `match`.
