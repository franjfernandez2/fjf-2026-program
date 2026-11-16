# Working conventions

These rules apply to every change in the repository. They exist so that each result can be traced, reviewed and rebuilt.

## Language

Code, comments, documents, commits and pull requests are written in English. The public website pages are bilingual (English and Spanish).

## Branches and pull requests

- `main` always holds reviewed work. Nothing is committed to it directly.
- Each piece of work lives on its own branch, named `<type>/<short-description>`, for example `feat/adsl-derivation` or `chore/repo-skeleton`.
- Each branch is merged through a pull request using the template in `.github/pull_request_template.md`.
- A pull request is merged only after:
  1. continuous integration passes (from phase F0 day 4 onwards);
  2. Francisco Fernández approves it in writing in the pull request.
- On GitHub's free plan, private repositories cannot enforce branch protection. Rule 2 is therefore a project rule, and the pull request history is its evidence.
- Pull requests are merged with **squash and merge**, so that `main` has one commit per reviewed change.
- Exception: when a commit is itself the evidence of an order of events, the pull request is merged with a **merge commit**, so that the commit and its date stay on `main`. So far: pull request 54 (the integrated safety plan, 164da63, written before the unblinded results) and pull request 55 (the reviewer's unblinding authorisation, 77910f1).

## Commits

Commit messages follow [Conventional Commits](https://www.conventionalcommits.org/):

| Type | Use |
| --- | --- |
| `feat` | New functionality: a derivation, a model, a table |
| `fix` | Correction of wrong behaviour |
| `docs` | Documents only: SAP, protocol, notes, README |
| `test` | Tests only |
| `qc` | Independent second programming and comparisons |
| `chore` | Tooling, configuration and environment |
| `refactor` | Code change that does not change results |

A scope may name the study: `feat(301): MMRM primary analysis`.

## Tags

| Tag | Marks |
| --- | --- |
| `gate-0` … `gate-6` | Closure of each phase, after its gate criteria are met |
| `sap-201-v1.0`, `sap-301-v1.0` | The frozen SAP of each study, before any data are generated |
| `iss-sap-v1.0` | The frozen plan of the integrated safety analysis |
| `dblock-201`, `dblock-301` | The database lock of each study |

A tag is never moved or deleted. If a frozen document must change, the change is a new, dated amendment.

## Naming

| Item | Convention | Example |
| --- | --- | --- |
| Studies | `FJF-2026-<number>` | `FJF-2026-301` |
| Folders and files | lower case, hyphens or underscores, no spaces | `studies/301/adam/adsl.R` |
| R functions | `snake_case` verbs | `derive_baseline_hba1c()` |
| SDTM and ADaM datasets and variables | CDISC names, upper case | `ADSL`, `CHG`, `AVISITN` |
| Decision records | `docs/adr/NNNN-short-title.md` | `0006-raw-data-database.md` |

## Simulated data

Every table, figure, report and web page carries the label "Simulated data · fictional compound". The sealed simulation truth (`simulator/truth/`) is never committed and never read by analysis code.

## Secrets

No password, token or key is ever committed. Local credentials live in `.env`, which is ignored by git; `.env.example` documents the variables. Continuous integration uses GitHub secrets.
