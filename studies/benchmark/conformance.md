# Conformance check of the ADaM files with CDISC CORE

**Result: not a conformance result.** On 2026-11-11, CDISC CORE could not check ADaM data. The published engine has no ADaM rules. The draft ADaM rules don't run on it: they report variables as missing even when they are present, including on the rules' own test data. Conformance of our ADaM files rests, for now, on the checks listed at the end of this note.

## What was run

| Item | Version | Check |
| --- | --- | --- |
| CDISC CORE engine (`cdisc-org/cdisc-rules-engine`, MIT) | v1.0.0, 2026-11-03, `core-mac-apple-silicon.zip` | SHA-256 `8c5b274d24def4bffb983f2a835121f79854a0028f605a49e336fdaca5d87361`, as published with the release |
| Draft ADaM rules (`cdisc-org/cdisc-open-rules`, MIT, folder `Unpublished/ADAMIG`) | commit `6d7a59429f8910f665bd4bf2e99fee44e9ed5b9c`, 191 rules | Repository tarball SHA-256 `cd779b02fbe620ae212edd59b2796eeaca9bfe068dd3c142071e88de0e1ce3bd` |
| Standard | ADaMIG 1.0, the pilot's version (`-s adamig -v 1-0`) | |
| Data | Our four XPT files (target `adam_xpt`) and the four official ones | |

Neither download is committed. To reproduce the run, unzip both outside the repository and use [`tools/run-core-adam.sh`](../../tools/run-core-adam.sh).

## What came out

1. **The published engine has no ADaM rules.** Its cache holds 1,114 rules, all SDTMIG, SENDIG, TIG or USDM. With only the cache, `core validate -s adamig -v 1-0` stops with "No rules were selected for this standard/version". The only ADaM rules CDISC has made public are drafts (`Unpublished/ADAMIG`); none is under `Published/`.
2. **The draft rules run, but they don't read the data.** Of 190 rules evaluated, 175 end in an execution error ("'SDTMDatasetMetadata' object has no attribute 'data'" or "Rule contains invalid operator"). 9 are skipped, 3 pass, and 3 report issues in every dataset: STUDYID not present (AD0088), USUBJID not present (AD0089), and a variable name that doesn't start with a letter (AD0014). All three are false: every dataset has STUDYID and USUBJID, and every variable name starts with a letter.
3. **The report is identical for our files and the official ones.** The same rules end in the same statuses, with the same issues.
4. **Control.** Rule AD0088 was run alone on its own conformant test data: `positive/01`, an ADSL with STUDYID, USUBJID and COMPLFN, converted to XPT. It still reported "STUDYID is not present". So the problem lies with the draft rules and engine v1.0.0, not with our files or the way they were run. The engine version the rules repository pins (`42028faf`) is v1.0.0 plus a README change, so it would not behave differently.

The 3 passing rules are AD0013 (names of up to 8 characters), AD0016 (labels of up to 40 characters) and AD0144 (PARAMCD starts with a letter). Because of item 4 they are not taken as evidence either. Our own checks cover the same three points.

## What this project does about it

- **No row in the discrepancy log.** These are not differences between our results and the pilot's; the tool produced no usable finding.
- **Conformance evidence for F1:**
  - `check_against_spec` checks variables, types, lengths, labels and codelists against the pilot's define.xml (targets `*_spec_findings`).
  - xportr writes the XPT files from the same specification (target `adam_xpt`).
  - `compare_xpt_with_official` compares them with the official files (target `adam_xpt_vs_official`).
- **Next run:** run CORE again when ADaM rules appear under `Published/` in `cdisc-open-rules`, or in the engine's cache. Pinnacle 21 Community is the alternative. It needs an account and a local install, so the user would run it.
