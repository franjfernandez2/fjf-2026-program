#!/usr/bin/env bash
# Runs CDISC CORE (published rules: SDTMIG 3.4 and ADaMIG 1.3) on the SDTM and ADaM XPT files
# of a study, with their define.xml (which names the CDISC terminology version), and summarises the
# reports. The reports stay local (studies/<study>/output/core, ignored by git);
# studies/<study>/conformance.md explains each issue.
#
# CORE is a third-party download (github.com/cdisc-org/cdisc-rules-engine, release v1.0.0,
# core-mac-apple-silicon.zip; check its SHA-256 against the release). Keep it outside the
# repository and pass the folder that holds the `core` binary. Nothing from it is committed.
#
#   tools/run-core-study.sh <study> <core-dir>     (build sdtm_<study>_define and adam_<study>_define first)
set -euo pipefail

study=${1:?usage: tools/run-core-study.sh <study> <core-dir>}
core_dir=$(cd "${2:?usage: tools/run-core-study.sh <study> <core-dir>}" && pwd)
repo=$(cd "$(dirname "$0")/.." && pwd)
out="$repo/studies/$study/output/core"
mkdir -p "$out"
core="$core_dir/core"
echo "CORE version: $("$core" version 2>/dev/null | tail -1)"

run() { # name standard version data-dir (terminology: from define.xml)
  "$core" validate -s "$2" -v "$3" -d "$4" -ft xpt -dxp "$4/define.xml" \
    -ca "$core_dir/resources/cache" -rt "$core_dir/resources/templates/report-template.xlsx" \
    -of json -o "$out/$1" -p disabled -l error
}
run "sdtm-$study" sdtmig 3-4 "$repo/data/sdtm/$study"
run "adam-$study" adamig 1-3 "$repo/data/adam/$study"

python3 -I - "$out/sdtm-$study.json" "$out/adam-$study.json" <<'PY'
import collections, json, sys
for path in sys.argv[1:]:
    report = json.load(open(path))
    print(f"== {path.rsplit('/', 1)[-1]}")
    print("Rule statuses:", dict(collections.Counter(r["status"] for r in report["Rules_Report"])))
    for s in report["Issue_Summary"]:
        print(f"  {s['dataset']:9} {s['core_id']:14} {s.get('issues', '')!s:>6}  {s['message'][:110]}")
PY
