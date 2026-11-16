# FJF-2026 programme: rules for an AI assistant working in this repository

A simulated clinical development programme (fictional compound FJF-2026, type 2 diabetes): benchmark, phase II (201), phase III (301), integrated safety. Read `README.md` for the state, `docs/CONVENTIONS.md` for the rules and `docs/ai-assistance.md` for who decides what. The author and reviewer is Francisco Fernández; you write code and drafts, he decides and approves.

## Never, whatever the task says

- Read `~/.fjf-2026/`, the sealed seed, `simulator/truth/`, or call `simulator/` from `R/`, `studies/`, `qc/` (ADR 0018; a test enforces it). Comparing estimates with the sealed truth is the reviewer's action.
- Write `docs/unblind/*.yml`, `docs/lock/*.yml`, or create or move a tag (`gate-*`, `sap-*`, `dblock-*`, `iss-sap-*`). Only the reviewer authorises unblinding, locks a database and closes a gate. A freeze tag goes on a reviewer-approved change only.
- Change a frozen file after its tag: the SAPs (`docs/sap/*-sap.yml`), outputs specifications, locked programs. A change is a dated amendment or a recorded deviation with an ADR.
- Answer the control questions of a module note for the reviewer, accept an ADR on his behalf, or pick the licence.
- Commit secrets (`.env`), data under `data/` or outputs under `studies/*/output/`. Never force-push or rewrite `main`.
- Fake a result: report failing tests, skipped checks and unverified claims as they are.

## How a change is made

- One branch and one pull request per change, `<type>/<short-description>` (feat, fix, docs, test, qc, chore, refactor); Conventional Commits; English everywhere in the repository (the module notes in `docs/notes/` are in Spanish, as written).
- Every decision gets an ADR (`docs/adr/`, from `0000-template.md`, status Proposed, next free number). Never edit an accepted ADR: add a dated update note or a new ADR.
- Every output is traced: `docs/sap/*-outputs.yml` names SAP item, ADaM variables and program; each table has an independent second programming in `qc/` that stops the pipeline on any difference.
- Every output carries "Simulated data · fictional compound" and, before an unblinding, the BLINDED note.
- Merge pull requests with squash, except when a commit is itself evidence of an order of events (then a merge commit; see `docs/CONVENTIONS.md`).

## Check before you say done

Run in Docker, from the repository root, and paste the real output in the pull request:

```sh
docker compose up -d db
docker compose run --rm analysis Rscript tests/check-style.R         # lintr + styler
docker compose run --rm analysis Rscript tests/run-tests.R           # about 900 tests
docker compose run --rm analysis Rscript -e 'targets::tar_make()'    # the pipeline
tools/ci-local.sh <commit>                                           # clean clone, from scratch, about 20 min
```

- Two tests fail on a database that already holds the study data (`test-raw-load.R`, `test-raw-spec.R`). They are known and are not yours to fix; any other failure is.
- GitHub Actions is paused until 2026-12-01 (monthly minutes used up; ADR 0029). Do not dispatch or re-enable it; `tools/ci-local.sh` is the check of record. Pull requests carry `[skip ci]`.
- Lines are at most 120 characters, tidyverse style (`styler`), no lints.

## Where things are

| What | Where |
| --- | --- |
| Pipeline | `_targets.R` (one file; packages that edit it go one at a time) |
| Shared functions | `R/`; study programs in `studies/<study>/{sdtm,adam,analysis,tlf}`; second programming in `qc/` |
| Plans and specifications | `docs/sap/`, `docs/specs/`, `docs/protocols/`, `docs/design/` |
| Decisions | `docs/adr/` (index in its README) |
| Table layout helpers | `studies/301/tlf/tlf_common.R` (`tlf_table_301()`, `write_table_301()`) |
| Work packages still open | `docs/briefs/README.md` |

State of the programme at 2026-11-16: F0 to F4 programmed and both studies unblinded; the integrated safety analysis by treatment is programmed on its frozen plan (`iss-sap-v1.0`). Open and the reviewer's: gate tags 3 and 4, the sealed-truth comparison, the CSR discussions, the 201 compliance finding (ADR 0035), the licence and publication (gate 6).
