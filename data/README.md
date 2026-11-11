# Data

Every dataset in this folder is **generated or downloaded by the pipeline**. Nothing here is versioned except the notes, the manifests and the empty folders.

The FJF-2026 data are **simulated**. The only exception is `external/`, which holds the public CDISC pilot used by the benchmark: de-identified legacy data from a real trial, which this project downloads and never modifies ([`external/README.md`](external/README.md)).

| Folder | Layer | Format | Produced by |
| --- | --- | --- | --- |
| `external/` | Third-party data: the CDISC pilot (SDTM, official ADaM, `define.xml`, study report) | SAS XPT v5, XML, PDF | Downloaded and verified by the pipeline |
| `raw/` | EDC-like raw data, with realistic collection errors | PostgreSQL (schemas `raw_201`, `raw_301`) and exports | `simulator/` |
| `sdtm/` | SDTM domains | Parquet and SAS XPT v5 | `studies/*/sdtm` programs |
| `adam/` | ADaM datasets and `define.xml` | Parquet and SAS XPT v5 | `studies/*/adam` programs |

The parameters used to simulate the data (the "sealed truth") live in `simulator/truth/`. That folder is never committed and analysis code never reads it. It is opened once, after each study's analysis is final, to check whether the methods recovered the truth.

No identifiable patient data is used anywhere in this repository.
