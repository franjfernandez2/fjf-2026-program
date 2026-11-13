#!/usr/bin/env bash
# Locks the database of a study (ADR 0027): writes docs/lock/dblock-<study>.yml, commits it
# and tags the commit dblock-<study>. From then on the pipeline uses the real treatment
# assignment, and programs, specifications and cleaning rules are frozen by the tag.
#
# Only the reviewer runs it. It refuses unless:
#   - the working tree is clean, on main, and main equals origin/main;
#   - the study has no lock file and no lock tag yet;
#   - the SDTM and ADaM second programmings of the study agree with production
#     (targets sdtm_<study>_qc_check and adam_<study>_qc_check, rebuilt here).
# It does not push: the reviewer pushes the commit and the tag.
#
#   tools/lock-database.sh <study> [--dry-run]
#     --dry-run              run every check and print the lock file, change nothing
#     --no-pipeline-check    skip the second-programming check (only for this tool's own test)
set -euo pipefail

study=${1:?usage: tools/lock-database.sh <study> [--dry-run]}
case "$study" in ''|*[!0-9]*) echo "study must be numeric, e.g. 301"; exit 2 ;; esac
shift
dry=0
pipeline=1
for a in "$@"; do
  case "$a" in
    --dry-run) dry=1 ;;
    --no-pipeline-check) pipeline=0 ;;
    *) echo "unknown option: $a"; exit 2 ;;
  esac
done
repo=$(cd "$(dirname "$0")/.." && pwd)
cd "$repo"
lock="docs/lock/dblock-$study.yml"
tag="dblock-$study"
fail() { echo "Not locked: $*"; exit 1; }

[ -z "$(git status --porcelain)" ] || fail "the working tree has changes"
[ "$(git rev-parse --abbrev-ref HEAD)" = "main" ] || fail "not on main"
git fetch -q origin main
[ "$(git rev-parse HEAD)" = "$(git rev-parse origin/main)" ] || fail "main differs from origin/main"
[ ! -e "$lock" ] || fail "$lock already exists"
! git rev-parse -q --verify "refs/tags/$tag" >/dev/null || fail "tag $tag already exists"

if [ "$pipeline" = 1 ]; then
  echo "Checking the second programmings of study $study..."
  docker compose run --rm -T analysis Rscript -e "
    s <- '$study'
    targets::tar_make(names = c(sdtm_${study}_qc_check, adam_${study}_qc_check), reporter = 'silent')
    q <- c(targets::tar_read_raw(paste0('sdtm_', s, '_qc_comparison'))\$clean,
           targets::tar_read_raw(paste0('adam_', s, '_qc_comparison'))\$clean)
    if (!all(q)) stop('a second programming differs')
  " >/dev/null || fail "the second programmings do not agree with production, or the pipeline failed"
fi

frozen=$(git rev-parse HEAD)
content="# Database lock of FJF-2026-$study (ADR 0027). Written by tools/lock-database.sh.
study: \"$study\"
status: locked
locked_at: \"$(date -u +%Y-%m-%dT%H:%M:%SZ)\"
locked_by: \"$(git config user.name)\"
frozen_commit: \"$frozen\"
decisions: docs/adr/0028-pre-lock-analysis-decisions-301.md"

if [ "$dry" = 1 ]; then
  echo "All checks passed. Dry run: $lock would be:"
  echo "$content"
  exit 0
fi

mkdir -p docs/lock
printf '%s\n' "$content" > "$lock"
git add "$lock"
git commit -q -m "Database lock of FJF-2026-$study" -m "Programs, specifications and cleaning rules are frozen at $frozen (ADR 0027)."
git tag -a "$tag" -m "Database lock of FJF-2026-$study"
echo "Locked: $lock committed and tagged $tag. Push both:"
echo "  git push origin main && git push origin $tag"
