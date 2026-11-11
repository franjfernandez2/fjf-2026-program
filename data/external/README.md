# Third-party data

This folder holds data that this project did not create. Only this note and the manifests are versioned. The data files themselves are downloaded and verified by the pipeline ([ADR 0015](../../docs/adr/0015-third-party-pilot-data.md)).

## `cdiscpilot01/`: CDISC SDTM/ADaM Pilot Project

| | |
| --- | --- |
| Source | [cdisc-org/sdtm-adam-pilot-project](https://github.com/cdisc-org/sdtm-adam-pilot-project), commit `667511d` (2024-12-20) |
| Owner | CDISC (Clinical Data Interchange Standards Consortium) |
| Terms | [CDISC Terms of Use](https://github.com/cdisc-org/sdtm-adam-pilot-project#terms-of-use): free use and distribution with attribution to CDISC; the data may not be modified |
| Origin | De-identified legacy data from a real trial of transdermal xanomeline in Alzheimer's disease, provided by Eli Lilly and Company to the CDISC pilot. Dates were shifted into the future. |
| Files | 21 (73 MB), listed with their hashes in [`cdiscpilot01-manifest.csv`](cdiscpilot01-manifest.csv) |
| Used for | Phase F1, the benchmark: our ADaM datasets and tables are compared with the official ones |

Download and verify:

```sh
docker compose run --rm analysis Rscript -e 'targets::tar_make(pilot_files)'
```

A file is kept only if its size, its git blob SHA-1 and its SHA-256 match the manifest. The pipeline never writes to these files.

Manifest columns:

| Column | Meaning |
| --- | --- |
| `file` | Path inside `cdiscpilot01/` |
| `role` | `input` (SDTM), `reference` (official ADaM), `metadata` (`define.xml`) or `report` (study report) |
| `source_path` | Path in CDISC's repository |
| `bytes` | Size |
| `git_blob_sha1` | Hash of the file in the pinned commit, as GitHub lists it |
| `sha256` | SHA-256 of the file |

Source data: CDISC SDTM/ADaM Pilot Project, © CDISC.
