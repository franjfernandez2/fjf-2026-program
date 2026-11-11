# ADR 0014: Documents with Quarto, PDF through Typst

- **Status:** Accepted
- **Date:** 2026-11-11
- **Decided by:** Francisco Fernández

## Context

Protocols, SAPs, reports and explanatory notes must be produced from the repository, with results computed in the same pinned environment, as HTML for reading and PDF for sharing.

## Decision

- Quarto **1.11.9** is installed in the analysis image from its official release package, for amd64 and arm64.
- PDFs are produced with **Typst**, which ships with Quarto, so no LaTeX distribution is needed.
- Four templates live in `docs/templates/`: protocol synopsis, SAP (ICH E9 / E9(R1) structure), abridged CSR (ICH E3 structure) and explanatory note. Every HTML and PDF output carries the "Simulated data · fictional compound" notice.
- The locked R library is made visible to every R process in the image, because Quarto starts R from the document's folder, where renv is not activated.
- Inside a Quarto project, `here::here()` stops at the folder holding `_quarto.yml`; documents locate the repository root by `renv.lock` instead.
- Continuous integration renders all templates and notes on every pull request.

## Alternatives considered

- R Markdown with LaTeX: mature, but needs a LaTeX installation of several hundred megabytes and is slower.
- Word documents: common in industry, but not reproducible from code or reviewable as text.

## Consequences

The R package `rmarkdown` (and its dependencies) is added to `renv.lock`, which grows from 75 to 88 packages.
