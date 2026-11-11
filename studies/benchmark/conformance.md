# Conformance check of the ADaM files

**Result (2026-11-11):**

- **Pinnacle 21 Community** validated our four XPT files against ADaM-IG 1.0, then the four official files with the same settings. The two reports are identical, rule by rule and count by count: no issue is ours alone. Every issue has its cause in the pilot's define.xml, which our datasets follow by design (ADR 0016). Each one is recorded and explained in the discrepancy log (D-014 to D-018).
- **CDISC CORE** could not check ADaM data. Its published engine has no ADaM rules, and its draft ADaM rules don't run (second part of this note).

## Pinnacle 21 Community

### What was run

| Item | Version | Check |
| --- | --- | --- |
| Pinnacle 21 Community for Mac (Certara) | 4.1.0 (May 2024), the latest Mac build; 4.2.0 exists only for Windows | App signed with Developer ID "Pinnacle 21 LLC" (56C9Z22DN3) and notarized by Apple; installer SHA-256 `c01a42fa687d579848ab4280bae4f05fc2d48d246675fe4a277e01ee04a6e220` |
| Validator | command-line client `p21-client-1.0.8.jar`, shipped with the app | Same validator the app's window uses |
| Engine and rules | FDA 2204.1 ("FDA Legacy"), configuration `ADaM-IG 1.0 (FDA)` | The current FDA engine (2304.3) no longer covers ADaM-IG 1.0 |
| Controlled terminology | CDISC ADaM CT 2024-03-29 | The latest one the build has |
| Inputs | our four XPT files (target `adam_xpt`), then the four official ones; the pilot's SDTM (for the traceability rules) and its ADaM define.xml in both runs | |

The app was set up from its window without a Pinnacle ID. Creating one asks for a work email, and the validator does not need it. The client's own logging configuration also sends its logs to Pinnacle 21, so the runs use a local-only logging configuration instead.

To reproduce the runs, use [`tools/run-p21-adam.sh`](../../tools/run-p21-adam.sh). The reports go to `studies/benchmark/output/p21/`, which git ignores.

**Caveats the report itself prints.** The report warns of "Incompatible engine used" and "out of date version of the P21 Community CLI". Both follow from the constraints above: a legacy engine is the only FDA engine with ADaM-IG 1.0 rules, and 4.1.0 is the latest Mac build. Both runs share the same caveats, so the comparison between ours and the official files is fair. The absolute list of issues might differ slightly with a current engine.

### What came out

| Rule | Datasets | Issue | Ours | Official | Cause | Log |
| --- | --- | --- | --- | --- | --- | --- |
| SD0037 | all four | Values of SEX and of the Y/blank flags not in their codelists | same counts | same counts | The define.xml codes are padded with a non-breaking space (`'M<U+00A0>'`, `'Y<U+00A0>'`), so no value matches exactly | D-014 |
| SD0037 | ADSL | BMIBLGR1 = '>=30' not in BMICAT (28 subjects) | 28 | 28 | The codelist says '>30' | D-015 (as D-003) |
| AD0269 | ADAE | TRTEMFL = 'N' (65 records); ADaM-IG allows Y or null | 65 | 65 | The define.xml derivation sets 'N' | D-016 |
| AD0018 | ADQSADAS, ADTTE | Labels of ABLFL, CHG and PARAM differ from the standard | 3 | 3 | The define.xml labels (CHG is labelled "Baseline Value") | D-017 |
| AD0320 | ADSL | Non-standard dataset label | 1 | 1 | The define.xml label | D-018 |

Other lines of the report are not findings about the datasets:

- **SD0061:** the define.xml describes six more ADaM datasets that the benchmark does not build.
- **AD9999:** the SDTM domains given for the traceability rules are not themselves validated against an ADaM configuration.

The traceability rules against DM, AE and EX (AD1024 to AD1026) ran and reported nothing.

## CDISC CORE

### What was run

| Item | Version | Check |
| --- | --- | --- |
| CDISC CORE engine (`cdisc-org/cdisc-rules-engine`, MIT) | v1.0.0, 2026-11-03, `core-mac-apple-silicon.zip` | SHA-256 `8c5b274d24def4bffb983f2a835121f79854a0028f605a49e336fdaca5d87361`, as published with the release |
| Draft ADaM rules (`cdisc-org/cdisc-open-rules`, MIT, folder `Unpublished/ADAMIG`) | commit `6d7a59429f8910f665bd4bf2e99fee44e9ed5b9c`, 191 rules | Repository tarball SHA-256 `cd779b02fbe620ae212edd59b2796eeaca9bfe068dd3c142071e88de0e1ce3bd` |
| Standard | ADaMIG 1.0, the pilot's version (`-s adamig -v 1-0`) | |
| Data | Our four XPT files (target `adam_xpt`) and the four official ones | |

Neither download is committed. To reproduce the run, unzip both outside the repository and use [`tools/run-core-adam.sh`](../../tools/run-core-adam.sh).

### What came out

1. **The published engine has no ADaM rules.** Its cache holds 1,114 rules, all SDTMIG, SENDIG, TIG or USDM. With only the cache, `core validate -s adamig -v 1-0` stops with "No rules were selected for this standard/version". The only ADaM rules CDISC has made public are drafts (`Unpublished/ADAMIG`); none is under `Published/`.
2. **The draft rules run, but they don't read the data.**
   - Of 190 rules evaluated, 175 end in an execution error: "'SDTMDatasetMetadata' object has no attribute 'data'" or "Rule contains invalid operator".
   - 9 are skipped and 3 pass.
   - 3 report issues in every dataset: STUDYID not present (AD0088), USUBJID not present (AD0089), and a variable name that doesn't start with a letter (AD0014). All three are false: every dataset has STUDYID and USUBJID, and every variable name starts with a letter.
3. **The report is identical for our files and the official ones.** The same rules end in the same statuses, with the same issues.
4. **Control.** Rule AD0088 was run alone on its own conformant test data: `positive/01`, an ADSL with STUDYID, USUBJID and COMPLFN, converted to XPT. It still reported "STUDYID is not present". So the problem lies with the draft rules and engine v1.0.0, not with our files or the way they were run. The engine version the rules repository pins (`42028faf`) is v1.0.0 plus a README change, so it would not behave differently.

The 3 passing rules are AD0013 (names of up to 8 characters), AD0016 (labels of up to 40 characters) and AD0144 (PARAMCD starts with a letter). Because of item 4 they are not taken as evidence either.

The CORE run adds no row to the discrepancy log, because it produced no usable finding. It should be run again when ADaM rules appear under `Published/` in `cdisc-open-rules`, or in the engine's cache.

## Other conformance evidence

- `check_against_spec` checks variables, types, lengths, labels and codelists against the pilot's define.xml (targets `*_spec_findings`).
- xportr writes the XPT files from the same specification (target `adam_xpt`).
- `compare_xpt_with_official` compares them with the official files (target `adam_xpt_vs_official`).
