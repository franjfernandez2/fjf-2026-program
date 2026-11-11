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

## What this means for a reader

The code was not typed by hand. The design, the decisions and the responsibility for every result are Francisco's, and Francisco can explain each of them.
