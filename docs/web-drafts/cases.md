# Web case pages: drafts (es, en)

**Draft for the reviewer's approval.** Nothing here is published and the website was not touched
(P5 of `docs/briefs/README.md`). A case may go on the site only when its criteria hold; the status
is stated per case below. Every page carries the label: **Simulated data · fictional compound** /
**Datos simulados · compuesto ficticio**.

Publication criteria: local CI from scratch green; double programming matching; validator issues
explained; simulated-data label; the reviewer can explain every decision (the control questions
of `docs/notes/` answered by him). The last one is not met for F3 and F4 until he answers them.

## Case 1 · Benchmark: replicating a public study

**en.** I replicated the ADaM analysis datasets and the main tables of the public CDISC pilot study
(CDISCPILOT01) with an R pipeline, checked each table with an independent second programming and ran
Pinnacle 21. The aim was to show that the pipeline reproduces published results before it touches
anything invented. Public data, not simulated.
*Stack: R, targets, renv, Docker, PostgreSQL, Pinnacle 21, CDISC CORE.*
*Status: criteria met except the reviewer's explanation of every decision (F0 and F1 notes).*

**es.** Repliqué los conjuntos ADaM y las tablas principales del estudio piloto público de CDISC
(CDISCPILOT01) con un pipeline en R, revisé cada tabla con una segunda programación independiente y
pasé Pinnacle 21. El objetivo era demostrar que el pipeline reproduce resultados publicados antes de
tocar nada inventado. Datos públicos, no simulados.

## Case 2 · FJF-2026-301: a phase III study from raw data to report

**en.** A simulated 26-week, placebo-controlled phase III study of a fictional add-on to metformin.
From EDC-like raw data (partial dates, duplicates, out-of-window visits) to SDTM, ADaM, an analysis
under an ICH E9(R1) estimand (MMRM and reference-based multiple imputation, jump to reference, with
sensitivity and tipping-point analyses), tables and figures, and a study report. The plan was frozen with
a tag before any data existed; programs were written on a dummy assignment and the database locked
before the unblinding was authorised. Every table, listing and figure has a second programming that stops
the pipeline on any difference.
*Stack: R, mmrm, rbmi, survival, Quarto, define.xml, Pinnacle 21, CDISC CORE.*

**es.** Un estudio de fase III simulado de 26 semanas, controlado con placebo, de un fármaco ficticio
añadido a metformina. Desde datos crudos tipo EDC (fechas parciales, duplicados, visitas fuera de
ventana) hasta SDTM, ADaM, un análisis bajo un estimando ICH E9(R1) (MMRM e imputación múltiple basada
en referencia, salto a la referencia, con análisis de sensibilidad y de punto de inflexión), tablas y
figuras, e informe de estudio. El plan se congeló con una etiqueta antes de que existieran datos; los
programas se escribieron sobre un reparto falso y la base se cerró antes de autorizar el desenmascarado.
Cada tabla, listado y figura tiene una segunda programación que detiene el pipeline ante cualquier
diferencia.

## Case 3 · FJF-2026-201: dose finding and integrated safety

**en.** A simulated 12-week phase II study (placebo and four doses) analysed with MCP-Mod to choose
the dose for phase III, with an exploratory clinical utility index, and an integrated safety analysis of both
studies whose plan was committed before the unblinded results were read. A blinded pooled review came
first, with the studies as columns and never an arm.
*Stack: R, DoseFinding, MASS, Mantel-Haenszel and exposure-adjusted rates.*

**es.** Un estudio de fase II simulado de 12 semanas (placebo y cuatro dosis) analizado con MCP-Mod
para elegir la dosis de fase III, con un índice de utilidad clínica exploratorio, y un análisis de
seguridad integrado de ambos estudios cuyo plan se registró antes de leer los resultados
desenmascarados. Antes hubo una revisión ciega agrupada, con los estudios como columnas y nunca un brazo.

## Case 4 · E2: how much do missing-data methods lie?

**en.** A pre-registered simulation study (plan committed before any result): bias, coverage and power
of MMRM and of reference-based imputation when dropout depends on the outcome, in 10 scenarios with 150
trials each. It found that missing-at-random analyses overstate benefit when only the active arm drops out
because of the outcome, and that jump to reference costs power when dropout is random. One of my
predictions was not met and the report says so.
*Stack: R, mmrm, rbmi, parallel simulation with public seeds.*

**es.** Un estudio de simulación con plan registrado antes de cualquier resultado: sesgo, cobertura y
potencia del MMRM y de la imputación basada en referencia cuando el abandono depende del resultado, en
10 escenarios de 150 ensayos. Los análisis con datos perdidos al azar sobreestiman el beneficio cuando
solo abandona por el resultado el brazo activo, y el salto a la referencia cuesta potencia cuando el
abandono es aleatorio. Una de mis predicciones no se cumplió y el informe lo dice.

## Not claimed

No comparison with the sealed truth has been made (reviewer's action), so no page states that the
analyses recover the true effects. The CSR discussions are not written.
