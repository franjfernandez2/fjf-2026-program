#!/usr/bin/env bash
# Generates the study data from the sealed seed: once, after the SAPs are frozen (ADR 0018).
#   tools/generate-study-data.sh
# Run it from a clean checkout that contains the tags sap-301-v1.0 and sap-201-v1.0. The seed
# file (default ~/.fjf-2026/truth-seed, or FJF_TRUTH_SEED_FILE) is mounted read-only into the
# container; nothing prints it. Afterwards: commit data/raw, then run tools/load-raw-data.R.
set -euo pipefail
cd "$(git rev-parse --show-toplevel)"

for tag in sap-301-v1.0 sap-201-v1.0; do
  if ! git rev-parse -q --verify "refs/tags/$tag" >/dev/null; then
    echo "The tag $tag does not exist: the SAPs must be frozen first." >&2
    exit 1
  fi
  if ! git merge-base --is-ancestor "$tag" HEAD; then
    echo "This checkout does not contain $tag." >&2
    exit 1
  fi
done
if [ -n "$(git status --porcelain)" ]; then
  echo "The working tree has changes: generate from a clean checkout." >&2
  exit 1
fi
if [ -e data/raw/manifest.csv ]; then
  echo "data/raw already holds the study data: they are generated once." >&2
  exit 1
fi
seed_file="${FJF_TRUTH_SEED_FILE:-$HOME/.fjf-2026/truth-seed}"
if [ ! -f "$seed_file" ]; then
  echo "No seed file at the expected place." >&2
  exit 1
fi

docker compose run --rm -T -e FJF_TAGS_CHECKED=1 \
  -v "$seed_file:/run/secrets/truth-seed:ro" \
  analysis Rscript tools/generate-study-data.R
echo "Generated at $(git rev-parse --short HEAD). Commit data/raw, then load it with tools/load-raw-data.R."
