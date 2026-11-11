# Benchmark: CDISC pilot replication (phase F1)

This study checks the pipeline against a study whose answer is already published: the CDISC SDTM/ADaM Pilot Project, CDISCPILOT01. The pilot's data are de-identified legacy data from a real trial, not simulated ([ADR 0015](../../docs/adr/0015-third-party-pilot-data.md)).

| Step | Where | Status |
| --- | --- | --- |
| Download and verify the pilot files | `R/pilot_data.R`, targets `pilot_*` | Done (PR 6) |
| Specifications from the pilot's `define.xml` | `R/define_v1.R`, `R/spec_checks.R`, targets `adam_spec`, `benchmark_metacore` | Done (PR 7) |
| ADSL from SDTM, compared with the official dataset | [`adam/`](adam/README.md), targets `adsl*` | Done (PR 8) |
| ADAE from SDTM, compared with the official dataset | `adam/`, targets `adae*` | Done (PR 9) |
| ADTTE and ADQSADAS, compared with the official datasets | `adam/`, targets `adtte*`, `adqsadas*` | Done (PR 10) |
| Independent QC of ADSL | `qc/benchmark/`, targets `adsl_qc*` | Done (PR 9) |
| Tables 14-2.01 and 14-5.01 compared with the study report | `tlf/`, `compare/`, targets `t_14_*` | Done (PR 11) |
| Table 14-3.01 and Figure 14-1 compared with the study report | `tlf/`, `compare/` | Planned |

## Discrepancy log

[`discrepancy-log.csv`](discrepancy-log.csv) records every difference found between the pilot's own files, or between our results and the pilot's. A difference is never hidden by copying the official value. Each one gets a cause and a decision.

| Column | Content |
| --- | --- |
| `id` | `D-` and a number, never reused |
| `date` | When it was found |
| `source` | Which file or comparison showed it |
| `dataset`, `variable` | Where it is |
| `finding` | What differs, with the values |
| `evidence` | The target, test or page that shows it |
| `cause` | Why it differs, as far as it can be established |
| `decision` | What this project does about it |
| `status` | `open` (cause not yet established) or `explained` |

Gate 1 is not passed while any row is `open`.
