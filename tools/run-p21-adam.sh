#!/usr/bin/env bash
# Runs Pinnacle 21 Community's validator (ADaM-IG 1.3, FDA engine 2304.3) on the ADaM XPT
# files of a study, with their define.xml, and prints the issue summary. The ADaM-to-SDTM
# traceability rules need SDTM given as ADaM input, which P21 then validates as ADaM, so they
# are left out (AD1024-AD1026); the second programming checks traceability instead. The report stays
# local (studies/<study>/output/p21, ignored by git); studies/<study>/conformance.md explains
# each issue.
#
# Needs Pinnacle 21 Community installed and set up once from its window. Its logs are kept
# local: the client's own logging configuration would also send them to Pinnacle 21.
#
#   tools/run-p21-adam.sh <study>     (for example 301; build the target adam_<study>_define first)
set -euo pipefail

study=${1:?usage: tools/run-p21-adam.sh <study>}
repo=$(cd "$(dirname "$0")/.." && pwd)
data="$repo/data/adam/$study"
out="$repo/studies/$study/output/p21"
mkdir -p "$out"

app="/Applications/Pinnacle 21 Community.app/Contents/Resources/app.asar.unpacked/components"
engines="$HOME/Documents/Pinnacle 21 Community/configs"
ct=${P21_ADAM_CT:-2024-03-29}

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
  --standard=ADaM --standard.version=1.3 --config="$engines/2304.3/ADaM-IG 1.3 (FDA).xml" \
  --cdisc.ct.adam.version="$ct" --source.adam="$data" --source.define="$data/define.xml" \
  --report="$out/adam-$study.xlsx" > "$out/adam-$study.log" 2>&1 || true
# The client can exit non-zero after a complete validation when its installation check warns
# (see studies/benchmark/conformance.md); the log says whether the validation completed.
grep -q "Process completed" "$out/adam-$study.log" || { echo "Validation failed: see $out/adam-$study.log"; exit 1; }

cd "$repo"
docker compose run --rm -T --no-deps analysis Rscript -e '
args <- commandArgs(trailingOnly = TRUE)
x <- suppressMessages(readxl::read_excel(args[1], sheet = "Issue Summary", col_names = FALSE, skip = 4))
source <- x[[1]]
for (i in seq_along(source)[-1]) if (is.na(source[i])) source[i] <- source[i - 1]
s <- data.frame(dataset = source, rule = x[[2]], message = x[[3]], severity = x[[4]], found = x[[5]])
print(s[!is.na(s$rule), ], right = FALSE, row.names = FALSE)
' "studies/$study/output/p21/adam-$study.xlsx"
