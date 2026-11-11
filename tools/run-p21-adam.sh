#!/usr/bin/env bash
# Runs Pinnacle 21 Community's validator (ADaM-IG 1.0, FDA engine) on our ADaM XPT files and
# on the official ones, with the pilot's SDTM (for the traceability rules) and its ADaM
# define.xml, and prints both issue summaries. See studies/benchmark/conformance.md.
#
# Needs Pinnacle 21 Community installed and set up once from its window (that downloads the
# engines into ~/Documents/Pinnacle 21 Community). Its logs are kept local: the client's own
# logging configuration would also send them to Pinnacle 21.
#
#   tools/run-p21-adam.sh [output-dir]     (default: studies/benchmark/output/p21, ignored by git)
set -euo pipefail

repo=$(cd "$(dirname "$0")/.." && pwd)
out=${1:-$repo/studies/benchmark/output/p21}
mkdir -p "$out"
out=$(cd "$out" && pwd)

app="/Applications/Pinnacle 21 Community.app/Contents/Resources/app.asar.unpacked/components"
engines="$HOME/Documents/Pinnacle 21 Community/configs"
pilot="$repo/data/external/cdiscpilot01"

logback="$out/logback-local.xml"
cat > "$logback" <<'XML'
<configuration>
  <appender name="STDOUT" class="ch.qos.logback.core.ConsoleAppender">
    <encoder><pattern>%d{HH:mm:ss} %-5level %logger{20} - %msg%n</pattern></encoder>
  </appender>
  <root level="INFO"><appender-ref ref="STDOUT"/></root>
</configuration>
XML

run() {
  "$app/java/bin/java" -Dlogback.configurationFile="$logback" -jar "$app/lib/p21-client-1.0.8.jar" \
    --engine.folder="$engines/lib" --engine.version=2204.1 --filter=FDA \
    --standard=ADaM --standard.version=1.0 --config="$engines/2204.1/ADaM-IG 1.0 (FDA).xml" \
    --cdisc.ct.adam.version=2024-03-29 \
    --source.adam="$2" --source.sdtm="$pilot/sdtm" --source.define="$pilot/adam/define.xml" \
    --report="$out/$1-full.xlsx" > "$out/$1-full.log" 2>&1 || true
  # The client exits with a non-zero status when its installation check warns (an old Mac
  # build and a legacy engine; see conformance.md), even after a complete validation. The
  # log says whether the validation itself completed.
  grep -q "Process completed" "$out/$1-full.log" || { echo "Validation of $1 failed: see $out/$1-full.log"; exit 1; }
}
run ours "$repo/data/adam/cdiscpilot01"
run official "$pilot/adam"

# The issue summaries side by side, read in the project's R environment.
cd "$repo"
docker compose run --rm -T --no-deps analysis Rscript -e '
summary_of <- function(path) {
  x <- suppressMessages(readxl::read_excel(path, sheet = "Issue Summary", col_names = FALSE, skip = 4))
  source <- x[[1]]
  for (i in seq_along(source)[-1]) if (is.na(source[i])) source[i] <- source[i - 1]
  data.frame(dataset = source, rule = x[[2]], message = x[[3]], found = x[[5]])[!is.na(x[[2]]), ]
}
dir <- "studies/benchmark/output/p21"
both <- merge(
  summary_of(file.path(dir, "ours-full.xlsx")), summary_of(file.path(dir, "official-full.xlsx")),
  by = c("dataset", "rule", "message"), all = TRUE, suffixes = c("_ours", "_official")
)
print(both, right = FALSE)
cat("Issues only in ours:", sum(is.na(both$found_official) | both$found_ours != both$found_official), "\n")
'
