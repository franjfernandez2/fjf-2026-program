# ADR 0038: Licence of the repository

- **Status:** Accepted (decision delegated by the reviewer to the assistant in chat, 2026-11-17)
- **Date:** 2026-11-17
- **Decided by:** the assistant, on the reviewer's delegation; the reviewer owns the copyright

## Context

The repository is to be made public at gate 6 as a portfolio of clinical biostatistics work, so that employers and
peers can read, run and judge it. It holds code, documents and simulated data, and does not hold third-party data
(the CDISC pilot data are downloaded and not redistributed, ADR 0015).

## Decision

- **Code: MIT.** The most common permissive licence in open-source data science, short and well understood by
  employers, with no obligations beyond keeping the notice. It also covers the simulator package.
- **Documents and simulated data: CC BY 4.0.** The usual choice for documents and data, allowing reuse with
  attribution, which suits a portfolio.
- The copyright is Francisco Fernández's, 2026. The fictional nature of the compound and the data is stated in the
  README and in `LICENSE-DOCS.md`.

## Alternatives considered

- **Apache 2.0 for the code.** Adds a patent grant that matters little for analysis code.
- **GPL.** Would deter reuse in companies, against the aim of the portfolio.
- **All rights reserved.** Public but not reusable; it would also keep the work from being forked and run.
- **One licence for everything (CC0, MIT).** CC BY asks for attribution on documents and data, which MIT does not
  suit as well for non-code works.

## Consequences

- Anyone may reuse the work with attribution. No warranty (as the licences say).
- The licence of the CDISC data is not changed by this one.
