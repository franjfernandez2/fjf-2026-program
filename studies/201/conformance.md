# Conformance of the SDTM of FJF-2026-201

The SDTM of FJF-2026-201 is made by the programs of FJF-2026-301 with this study's protocol
and specification ([ADR 0030](../../docs/adr/0030-study-201-reuses-301-programs.md)). The
package (target `sdtm_201_files`, with `sdtm_201_define`) was checked the same way as
FJF-2026-301's, on blinded data (dummy arms, public seed 201201;
[ADR 0027](../../docs/adr/0027-blinded-data-review-and-lock.md)). The reports stay local
(`studies/201/output/`, ignored by git).

## Second programming

The independent SDTM programming of FJF-2026-301 (`qc/301/sdtm_qc.R`), run on this study's raw
data, agrees with production in all ten domains it covers (DM, SE, DS, IE, EX, CM, AE, LB, VS,
CE): 0 differences.

## Pinnacle 21 Community

Engine 2304.3, *SDTM-IG 3.4 (FDA)*, CDISC SDTM terminology 2024-03-29, by
[`tools/run-p21-sdtm.sh`](../../tools/run-p21-sdtm.sh) `201`. It reports 19 rules, all among
the 20 that FJF-2026-301 reports and explains in
[its conformance notes](../301/conformance.md); no rule is new. (SD1201 is not reported here.)

## CDISC CORE

v1.0.0, SDTMIG 3.4 published rules, terminology from define.xml: 348 rules pass and 26 report
issues, the same 26 rules as FJF-2026-301, explained in [its conformance notes](../301/conformance.md).
