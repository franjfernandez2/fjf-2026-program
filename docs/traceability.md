# Traceability matrix

[`traceability-matrix.csv`](traceability-matrix.csv) links every table, listing and figure to the plan that asked for it and the code that produced and checked it. A reviewer can start from any number in a report and follow it back to a line of the SAP.

The matrix is written by the pipeline (target `traceability_matrix_file`) from the outputs specifications of the studies and the comparisons of their second programming; the benchmark rows are kept as they are. `qc_status` is `match` only where a second programming compared the output and found no difference, and `not started` where there is none (none at present: figures are compared on the data they plot, written next to the PNG): the matrix does not claim a check that was not made.

The first rows are the four outputs of the benchmark (phase F1). Their "shell" is the published table in the pilot's study report, and their SAP sections are those of the pilot's own SAP, printed inside the same report. The programs are added from day 6 of F1. Until then the paths are planned, not written.

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
| `qc_program` | Independent QC program; for the benchmark, the program that compares the output with the published table | `qc/301/t_14_2_1_qc.R` |
| `qc_status` | `not started`, `in progress`, `match`, `explained` or `mismatch` | `match` |
| `shell_id` | Table shell in `docs/shells/`; for the benchmark, the pages of the published table in the pilot's report | `T-14.2.1` |

An output is final only when `qc_status` is `match`, or `explained`: every remaining difference has an explained row in the study's discrepancy log. A benchmark output can be `explained` when the difference is the reference's error.
