# ADR 0012: Reproducible environment: pinned image, dated package snapshot, renv

- **Status:** Accepted
- **Date:** 2026-11-11
- **Decided by:** Francisco Fernández

## Context

Every result must be rebuilt identically on another machine and at a later date. That depends on three layers: the operating system and R, the system libraries, and the R packages. The author works on Apple Silicon (arm64); continuous integration runs on GitHub's Linux runners (amd64).

## Decision

- **Operating system and R:** `rocker/r-ver:4.6.1` (Ubuntu 24.04), pinned by its multi-architecture digest, so the same tag cannot silently change.
- **System libraries:** installed in the Dockerfile from the list returned by the Posit Package Manager system-requirements API.
- **R packages:** `renv.lock`, restored from the Posit Package Manager CRAN snapshot of **2026-11-10**. Every package therefore comes from the same frozen day of CRAN, as a pre-built Linux binary.
- The image build fails unless every locked package is installed at its locked version.
- Packages are copied into the image's library rather than symlinked to the renv cache, so the cache can be deleted to keep the image small.
- The project folder is mounted at run time; only a change to `renv.lock` requires rebuilding the image.

## Alternatives considered

- renv alone, without Docker: fixes R packages but not R itself or system libraries.
- Docker alone, without renv: the image fixes everything, but the package list is not reviewable in a pull request and cannot be restored outside Docker.
- The unpinned `rocker/r-ver:4.6.1` tag: simpler, but the publisher can update it.
- Live CRAN instead of a dated snapshot: package versions would drift between builds.

## Consequences

- Updating a package is a deliberate act: change the snapshot date or the package, rebuild, review the `renv.lock` diff in a pull request.
- `renv` infers `dbplyr` as a dependency when `dplyr` and a database backend are used together; it is included explicitly.
- Opening R directly on the Mac inside this folder activates renv for the Mac. The supported way to run the project is the Docker image.
