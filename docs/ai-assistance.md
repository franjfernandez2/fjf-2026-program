# How AI assistance is used in this project

This project is built by one person working with an AI assistant (Claude, by Anthropic). This page says plainly who does what, so that any reader can judge the work on its merits.

## Who does what

| Activity | Francisco Fernández | AI assistant |
| --- | --- | --- |
| Choice of indication, design and scope | Decides | Proposes options and trade-offs |
| Estimands, analysis methods, SAP content | Decides and approves | Drafts and explains |
| Code (simulator, SDTM, ADaM, analyses, TLFs) | Reviews and approves every change | Writes |
| Independent QC programming | Reviews and approves | Writes, separately from the primary code |
| Clinical interpretation (rescue rules, safety signals) | Leads | Supports |
| Reports and website text | Approves | Drafts |

## Controls that make the review real

- **Every change arrives as a pull request.** Nothing reaches `main` without Francisco's written approval. On a private repository in GitHub's free plan this cannot be enforced by branch protection, so it is a rule of the project, visible in the pull request history.
- **Every module has an explanatory note and control questions.** A gate is not passed until the questions are answered without notes.
- **Decisions are recorded.** Each decision has an ADR saying what was chosen, what was rejected and why.
- **Results are checked, not trusted.** Double programming, the public CDISC benchmark and the sealed simulation truth test the code independently of who wrote it.

## The sealed truth and its limit

The simulated studies are analysed against a sealed truth ([ADR 0018](adr/0018-sealed-truth-and-data-generating-model.md)): the parameters that generate the data come from a secret seed, and the analysis side must not know them until every analysis is final.

At Francisco's request, the assistant generated that seed. It used a command that wrote the number straight to a private file outside the repository and displayed nothing, so the assistant does not know it. Francisco keeps a copy. The file is on a disk the assistant can read, though, so the protection rests on a rule and on a test that keeps the analysis code away from the seed and the simulator, not on an impossibility. A seed known only to Francisco would have been stronger, and Francisco chose that option at the gate-2 review (2026-11-12): before the study data are generated, Francisco replaces the seed with a new one and does not tell the assistant.

## What this means for a reader

The code was not typed by hand. The design, the decisions and the responsibility for every result are Francisco's, and Francisco can explain each of them.
