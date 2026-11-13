#!/usr/bin/env bash
# Runs the steps of the CI workflow (.github/workflows/ci.yaml) on this machine, on a clean
# copy of a commit and from scratch, like the weekly CI run (ADR 0029: while the Actions
# minutes are used up, this is the check of record before a milestone).
#
#   tools/ci-local.sh [<commit>] [--no-docs]
#
# The copy is a fresh clone of the commit (default HEAD; uncommitted changes are not in it),
# in a temporary folder, with throwaway database credentials and its own Compose project and
# database volume, so the working copy, its pipeline store and its database are not touched.
# The CDISC pilot files are copied from the working copy when present (the CI cache), else
# the pipeline downloads and verifies them. Everything is removed at the end.
#
# The pipeline uses the real treatment arms only on a commit with both a database lock and an
# unblinding authorisation (ADR 0032); otherwise the data stay blinded.
set -euo pipefail

ref=HEAD
docs=1
for a in "$@"; do
  case "$a" in
    --no-docs) docs=0 ;;
    -*) echo "unknown option: $a"; exit 2 ;;
    *) ref=$a ;;
  esac
done
repo=$(cd "$(dirname "$0")/.." && pwd)
commit=$(git -C "$repo" rev-parse --verify "$ref^{commit}")
work=$(mktemp -d "${TMPDIR:-/tmp}/fjf-ci.XXXXXX")
project="fjf-ci-$(basename "$work" | tr -dc 'a-z0-9' | tr 'A-Z' 'a-z')"
log="$work.log"

cleanup() {
  (cd "$work" 2>/dev/null && docker compose -p "$project" -f compose.yaml -f "$work.override.yaml" down -v >/dev/null 2>&1) || true
  rm -rf "$work" "$work.override.yaml"
}
trap cleanup EXIT

echo "Local CI of $commit in $work (log: $log)"
git clone -q --no-checkout "$repo" "$work"
git -C "$work" checkout -q "$commit"

umask 077
{
  echo "POSTGRES_DB=fjf2026"
  echo "POSTGRES_USER=fjf_admin"
  echo "POSTGRES_PASSWORD=$(openssl rand -hex 16)"
  echo "FJF_LOADER_PASSWORD=$(openssl rand -hex 16)"
  echo "FJF_READER_PASSWORD=$(openssl rand -hex 16)"
} > "$work/.env"
umask 022

if [ -d "$repo/data/external/cdiscpilot01" ]; then
  mkdir -p "$work/data/external"
  cp -R "$repo/data/external/cdiscpilot01" "$work/data/external/"
fi

# The working copy's database publishes port 55432; this one publishes none.
cat > "$work.override.yaml" <<'YAML'
services:
  db:
    ports: !reset []
YAML

cd "$work"
dc() { docker compose -p "$project" -f compose.yaml -f "$work.override.yaml" "$@"; }
step() {
  local name=$1
  shift
  printf '%-40s' "$name"
  local start=$SECONDS
  if "$@" >>"$log" 2>&1; then
    echo "ok ($((SECONDS - start)) s)"
  else
    echo "FAILED (see $log)"
    exit 1
  fi
}

step "Build the analysis image" dc build analysis
step "Start the clinical database" dc up -d --wait db
step "Lockfile in sync with the code" dc run --rm -T --no-deps analysis Rscript -e 'stopifnot(renv::status()$synchronized)'
step "Lint and format checks" dc run --rm -T analysis Rscript tests/check-style.R
step "Test suite" dc run --rm -T analysis Rscript tests/run-tests.R
step "Pipeline from scratch" dc run --rm -T analysis Rscript -e 'targets::tar_make(reporter = "timestamp")'
step "Test suite on the pipeline's results" dc run --rm -T analysis Rscript tests/run-tests.R
if [ "$docs" = 1 ]; then
  step "Render the documents" dc run --rm -T --no-deps analysis bash -c \
    'cd docs/templates && quarto render && cd ../notes && quarto render && cd ../design && quarto render && cd ../protocols && quarto render && cd ../sap && quarto render && cd ../specs && quarto render && cd ../../studies/benchmark/report && quarto render'
fi
echo "Local CI passed on $commit."
