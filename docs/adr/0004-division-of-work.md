# ADR 0004: Division of work: the AI assistant programs, the author reviews and decides

- **Status:** Accepted
- **Date:** 2026-11-11
- **Decided by:** Francisco Fernández

## Context

The project is built by one person with an AI assistant. The way work is split determines both speed and what the author can later defend.

## Decision

The AI assistant writes the code. Francisco Fernández makes every design decision, reviews every pull request and approves it in writing. Each module ships with an explanatory note and control questions; no gate is passed until they are answered without notes.

## Alternatives considered

The author programs and the assistant supervises: slower, deeper learning. Split by module: infrastructure by the assistant, statistics by the author.

## Consequences

Faster delivery, with a higher risk that the work exceeds what the author can explain. The explanatory notes, control questions and written approvals are therefore mandatory, not optional. The split is declared publicly (ADR 0010).
