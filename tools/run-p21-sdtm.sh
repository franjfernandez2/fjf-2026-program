#!/usr/bin/env bash
# Runs Pinnacle 21 Community's validator (SDTM-IG 3.4, FDA engine 2304.3) on the SDTM XPT
# files of a study, with their define.xml, and prints the issue summary. The report stays
# local (studies/<study>/output/p21, ignored by git); studies/<study>/conformance.md explains
# each issue.
#
# Needs Pinnacle 21 Community installed and set up once from its window. Its logs are kept
# local: the client's own logging configuration would also send them to Pinnacle 21.
#
#   tools/run-p21-sdtm.sh <study>     (for example 301; build the target sdtm_<study>_define first)
set -euo pipefail

study=${1:?usage: tools/run-p21-sdtm.sh <study>}
repo=$(cd "$(dirname "$0")/.." && pwd)
data="$repo/data/sdtm/$study"
out="$repo/studies/$study/output/p21"
mkdir -p "$out"

app="/Applications/Pinnacle 21 Community.app/Contents/Resources/app.asar.unpacked/components"
engines="$HOME/Documents/Pinnacle 21 Community/configs"
ct=${P21_SDTM_CT:-2024-03-29}

logback="$out/logback-local.xml"
cat > "$logback" <<'XML'
<configuration>
  <appender name="STDOUT" class="ch.qos.logback.core.ConsoleAppender">
    <encoder><pattern>%d{HH:mm:ss} %-5level %logger{20} - %msg%n</pattern></encoder>
  </appender>
  <root level="INFO"><appender-ref ref="STDOUT"/></root>
</configuration>
XML

"$app/java/bin/java" -Dlogback.configurationFile="$logback" -jar "$app/lib/p21-client-1.0.8.jar" \
  --engine.folder="$engines/lib" --engine.version=2304.3 --filter=FDA \
  --standard=SDTM --standard.version=3.4 --config="$engines/2304.3/SDTM-IG 3.4 (FDA).xml" \
  --cdisc.ct.sdtm.version="$ct" --source.sdtm="$data" --source.define="$data/define.xml" \
  --report="$out/sdtm-$study.xlsx" > "$out/sdtm-$study.log" 2>&1 || true
# The client can exit non-zero after a complete validation when its installation check warns
# (see studies/benchmark/conformance.md); the log says whether the validation completed.
grep -q "Process completed" "$out/sdtm-$study.log" || { echo "Validation failed: see $out/sdtm-$study.log"; exit 1; }

cd "$repo"
docker compose run --rm -T --no-deps analysis Rscript -e '
args <- commandArgs(trailingOnly = TRUE)
x <- suppressMessages(readxl::read_excel(args[1], sheet = "Issue Summary", col_names = FALSE, skip = 4))
source <- x[[1]]
for (i in seq_along(source)[-1]) if (is.na(source[i])) source[i] <- source[i - 1]
s <- data.frame(dataset = source, rule = x[[2]], message = x[[3]], severity = x[[4]], found = x[[5]])
print(s[!is.na(s$rule), ], right = FALSE, row.names = FALSE)
' "studies/$study/output/p21/sdtm-$study.xlsx"
