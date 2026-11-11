#!/usr/bin/env bash
# Runs CDISC CORE with the draft ADaM rules on our ADaM XPT files and on the official ones,
# and summarises both reports. See studies/benchmark/conformance.md for what was run and why
# the result is not a conformance result.
#
# CORE and the rules are third-party downloads: keep each in its own directory outside the
# repository and pass their paths here. Nothing from them is committed.
#
#   tools/run-core-adam.sh <core-dir> <rules-dir> <output-dir>
#
#   <core-dir>    unzipped core-mac-apple-silicon.zip of CORE v1.0.0 (holds the `core` binary)
#   <rules-dir>   the rule.yml files of cdisc-open-rules Unpublished/ADAMIG, one per rule, flat
#   <output-dir>  where the JSON reports go (outside the repository, or under studies/*/output/)
set -euo pipefail

if [ "$#" -ne 3 ]; then
  sed -n '2,13p' "$0"
  exit 1
fi
core_dir=$(cd "$1" && pwd)
rules_dir=$(cd "$2" && pwd)
mkdir -p "$3"
out_dir=$(cd "$3" && pwd)
repo=$(cd "$(dirname "$0")/.." && pwd)

core="$core_dir/core"
echo "CORE version: $("$core" version)"
echo "Draft rules: $(find "$rules_dir" -maxdepth 1 -name '*.yml' | wc -l | tr -d ' ')"

run() {
  "$core" validate -s adamig -v 1-0 -d "$2" -ft xpt -lr "$rules_dir" \
    -ca "$core_dir/resources/cache" -rt "$core_dir/resources/templates/report-template.xlsx" \
    -of json -o "$out_dir/$1" -p disabled
}
run ours "$repo/data/adam/cdiscpilot01"
run official "$repo/data/external/cdiscpilot01/adam"

python3 -I - "$out_dir/ours.json" "$out_dir/official.json" <<'EOF'
import collections, json, sys
for path in sys.argv[1:]:
    report = json.load(open(path))
    print(f"== {path.rsplit('/', 1)[-1]}")
    print("Rule statuses:", dict(collections.Counter(r["status"] for r in report["Rules_Report"])))
    for s in report["Issue_Summary"]:
        if "execution error" not in s["message"]:
            print(f"  {s['dataset']:9} {s['core_id']:24} {s['message']}")
EOF
