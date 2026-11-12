# ADR 0018: Sealed truth and the data-generating model

- **Status:** Proposed
- **Date:** 2026-11-12
- **Decided by:** Francisco Fernández

## Context

Studies FJF-2026-201 and FJF-2026-301 are analysed on data that this project simulates. The analyst and the author of the simulator are the same assistant, which creates a risk: an analysis can be tuned, even without intending it, towards an answer its author already knows. The dossier names this circularity as a main risk.

The response is a sealed truth. The parameters that generate the data stay unknown on the analysis side until every analysis is final. Then they are unsealed, and the estimates are compared with them. That comparison is only convincing if the truth really was unknown.

## Decision

**Two sets of values.** Every parameter of the data-generating model is in [`docs/design/assumptions.yml`](../design/assumptions.yml) with two values:

- a *planning value*, public, used by the protocols and the sample size;
- a *truth range*, public, inside which the true value lies.

Both are justified from published trials ([design assumptions](../design/assumptions.qmd)). The planning value need not be the true one. In real programmes it seldom is.

**The truth is drawn from a secret seed.** The simulator draws each true value inside its range from a single seed, together with every patient-level random number. Patients, dropouts, rescue, adverse events and the data-collection errors all follow from that seed. The seed is the only secret: with it, anyone can regenerate the truth and the data exactly.

- The seed is a 10-digit number from the operating system's random source (`/dev/urandom`).
- It lives in `~/.fjf-2026/truth-seed`, outside the repository, readable only by the user's account.
- The reviewer keeps a copy in a password manager.
- The seed is never printed, logged, committed or written into any other file.
- The truth it implies is never written to disk before the unblinding. The simulator recomputes it from the seed when it runs.

**Generation happens once, after the SAPs are frozen.** The target that generates the study data reads the seed from that file. It refuses to run unless the tags `sap-301-v1.0` and `sap-201-v1.0` are in the history. Its output is the raw data. The raw data are exported and committed, so the pipeline and continuous integration load them without the seed.

**The analysis cannot reach the simulator or the seed.** The simulator is an R package in `simulator/`. Code under `R/`, `studies/` and `qc/` must not load it, read the seed file or call the truth functions, and a test scans the code for that. The simulator is developed and tested with public test seeds only.

**Unblinding.** When the analyses of a study are final and tagged, the seed is used to regenerate the truth. The estimates are then compared with it, and the comparison is published together with the seed.

**A limit, declared.** The reviewer asked the assistant to generate the seed (2026-11-12). The assistant generated it with a command that wrote it straight to the file and showed nothing, so the assistant does not know it. The file is on a disk the assistant can read, though. The protection therefore rests on a rule and on a test, not on an impossibility. A seed known only to the reviewer, typed in at generation time, would be stronger. This limit is stated in the README and in [docs/ai-assistance.md](../ai-assistance.md).

## Alternatives considered

- **A seed known only to the reviewer, typed in at generation.** The strongest option, and the one the plan recommended. The reviewer preferred the assistant to generate it. The design is the same, so the reviewer can still switch: replace the file's content before the generation, without telling the assistant.
- **Fixed true parameters chosen by the assistant.** Simpler, but the analyst would know the answer. This is exactly the circularity that the sealed truth exists to prevent.
- **An encrypted truth file.** Equivalent to a secret seed, with more parts: a key to manage and a file to protect. The seed already makes everything reproducible.
- **Committing the truth after generation, hidden in a branch.** Anyone with access to the repository, the assistant included, could read it.

## Consequences

- The planning values are public, as they would be in a real protocol. The truth is not.
- The raw data are committed (an exception to "generated data are not versioned"), because continuous integration has no seed. The data are simulated and contain no personal information.
- The simulator's own tests use public seeds: with many patients it must recover its own parameters. This checks the simulator without revealing the sealed truth.
- If the seed is lost, the data stay usable, but the unblinding becomes impossible. That is why the reviewer keeps a copy.
- If the seed is seen on the analysis side before the unblinding, it is replaced before the generation, or, after the generation, the leak is recorded and the unblinding loses its value as evidence.
