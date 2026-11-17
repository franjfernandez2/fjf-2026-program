#!/usr/bin/env bash
# The recovery report: estimates against the sealed truth (ADR 0018). The reviewer's action: run it
# only when the analyses are final, from the repository root, after tar_make() with the real arms.
#   tools/recovery-report.sh
# The seed file (default ~/.fjf-2026/truth-seed, or FJF_TRUTH_SEED_FILE) is mounted read-only.
# It writes docs/recovery/recovery-report.md; commit it yourself, with the seed if you decide to publish it.
set -euo pipefail
cd "$(git rev-parse --show-toplevel)"
seed_file="${FJF_TRUTH_SEED_FILE:-$HOME/.fjf-2026/truth-seed}"
if [ ! -f "$seed_file" ]; then
  echo "No seed file at the expected place." >&2
  exit 1
fi
docker compose up -d db
docker compose run --rm -T -v "$seed_file:/run/secrets/truth-seed:ro" analysis Rscript tools/recovery-report.R
