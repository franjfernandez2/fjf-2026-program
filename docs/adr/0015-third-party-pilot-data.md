# ADR 0015: Third-party data for the benchmark: the CDISC pilot, downloaded and verified

- **Status:** Accepted
- **Date:** 2026-11-11
- **Decided by:** Francisco Fernández, who read and accepted CDISC's Terms of Use and approved the download

## Context

The benchmark (phase F1) checks the pipeline against a study whose answer is already published: the CDISC SDTM/ADaM Pilot Project, CDISCPILOT01. Its original package holds the SDTM domains, the official ADaM datasets, both `define.xml` files and the clinical study report with the published tables.

The data are de-identified legacy data from a real trial, provided by Eli Lilly and Company to the pilot project. Dates were shifted into the future, so the report gives 2012–2015 as the study period. They are not simulated.

The repository has no open-source licence. CDISC's Terms of Use allow use and free distribution with attribution to CDISC. They forbid modifying the data and presenting its origin in a misleading way.

## Decision

- **Source:** [cdisc-org/sdtm-adam-pilot-project](https://github.com/cdisc-org/sdtm-adam-pilot-project), pinned at commit `667511d4b183871d74392ba691c935c38d431d39` (2024-12-20). GitHub has verified that the `cdisc-org` organisation controls the `cdisc.org` domain.
- **Files:** only the 21 the benchmark needs (73 MB), listed in `data/external/cdiscpilot01-manifest.csv`:
  - 14 SDTM domains as input;
  - 4 official ADaM datasets as the reference answer;
  - the 2 `define.xml` files;
  - the study report.
- **Not redistributed:** the files never enter git. The pipeline downloads them into `data/external/cdiscpilot01/`, which git ignores.
- **Verified:** a file is kept only if its size, its git blob SHA-1 (the hash GitHub records for it in the pinned commit) and its SHA-256 all match the manifest. A file that fails is discarded. A local file that was altered is replaced by a verified copy.
- **Cross-checked once against a second source** on 2026-11-11, using PHUSE's copy in `phuse-org/phuse-scripts`:
  - 20 of the 21 files are byte-identical.
  - The SDTM `define.xml` differs only in line endings: PHUSE's copy uses CRLF.
  - CDISC's copy of that file is unchanged since its first commit in 2019.
- **Not modified:** the pipeline reads the files and never writes to them. Our ADaM datasets are new files derived from the SDTM.
- **Attribution:** the README, the benchmark report and every benchmark output name CDISC as the source. They also say the data are de-identified legacy trial data.
- **Not labelled as simulated:** benchmark outputs never carry the "Simulated data · fictional compound" notice used for FJF-2026. That notice would misstate their origin.

## Alternatives considered

- **`pharmaversesdtm` and `pharmaverseadam`:**
  - Convenient, because they are already CRAN packages.
  - But their datasets are modified versions of the pilot.
  - They do not include the official ADaM or the study report, so there is no known answer to compare with.
- **Committing the files to the repository:**
  - Simpler.
  - But it redistributes CDISC's data.
  - It adds 73 MB to every clone, and it would make the repository's future licence ambiguous for those files.
- **PHUSE's copy as the primary source:**
  - The repository is MIT-licensed, but that licence does not change CDISC's terms on the data.
  - CDISC's own repository is the original.
- **Downloading from the default branch:** simpler, but the files could change without notice.

## Consequences

- The first run needs internet access and downloads 73 MB. Continuous integration caches the files, keyed on the manifest.
- Changing the pinned commit or the file list means editing the manifest and running `tools/build-pilot-manifest.R`, which re-checks every file against the commit's tree.
- `haven` (and its dependencies) is added to `renv.lock`, which grows from 88 to 95 packages.
- The statement "no real patient data is used anywhere in this repository" is no longer true. It is replaced by an exact description of each data source.
