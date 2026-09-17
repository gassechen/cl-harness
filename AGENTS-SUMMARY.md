# Summary
## Objective
- Implementar la arquitectura de **2 motores LISA en cl-harness** (aprobada por el usuario, "vamos"): `*turn-engine*` (WM intra-turno, default activa) + `*mem-engine*` (long-term persistido), como cerebro simbólico (loop-detection declarativa, promoción turn→mem, renderizado `long_term_memory:` en el contexto, persistencia round-trip). Misión transversal: **verificar el harness** (genérico, no arreglar la app del LAB; probar con "dile que arregle los problemas" sin pasarle la lista). Comunicar en español, respuestas cortas.

## Important Details
- Directivas usuario: "TU TRABAJO ES VERIFICAR QUE EL HARNESS FUNCIONE"; respuestas cortas en español; usuario impaciente previamente.
- **LISA moderno**: engine activo = `lisa::*active-engine*` (preamble.lisp) ligado por `lisa:with-inference-engine`; `clear` = clear-system-environment → **reemplaza engine y borra reglas** (por eso cl-harness usa `reset-*` que conserva reglas); `run` devuelve contador; `defrule` compila en el engine activo en runtime; `lisa::rule-name` → SYMBOLS calificados MAYÚSCULAS tipo `INITIAL-CONTEXT.DETECT-READ-TRIPLE` (en tests: `(symbol-name (lisa::rule-name r))`); los CLOs de `deftemplate harness-fact` son instancias de la clase LISA `FACT` (print como `#<HARNESS-FACT>`, pero `type-of` = FACT → `typep 'harness-fact` = NIL: usar `fact-type-of`/`fact-slot`, nunca `typep`); `retrieve` devuelve bindings `(#<HARNESS-FACT> ...)` → `mapcar #'first`.
- **Saliencia**: detect-command-loop / detect-read-triple = 10 > dedup-keep-newest 0 > prune-superseded-by-epoch -40 > prune-expired -50 → en un `(run)` las reglas de loop disparan ANTES de dedup/prune.
- **Reglas de loop requieren**: 3 hechos del MISMO turno, timestamps **estrictamente crecientes** (`tsa<tsb<tsc` o bindings idénticos rompen la red beta → run=0), y para command-exec `:exit-code` NO nulo + `:output` + `not :timed-out`.
- **Mem-engine**: durable = command-exec con `real-error-p` (no timeout, con output) o file-write; conversación NO durable; archivo global `dumps/longterm-mem.lisp` (no por session); `boot-memory` = reset-turn + reset-mem + load-mem-engine + run mem.
- Config test-bed: openrouter/deepseek-v4-flash, iterations 30, timeout 30, conserve_errors true, fact_ttl_turns 30, max_facts_per_type 20; binary instalado como `cl-harness` (renombrado) en `/home/mtk/TEST-PRUEBAS/cl-harness-test-proyect/`; base dir = cwd (o CL_HARNESS_DIR).
- ASDF/quicklisp: los scripts de test deben cargar vía `(load "/tmp/opencode/load-harness.lisp")` (= setup.lisp + `ql:quickload :cl-harness`), NUNCA asdf a secas (cl-ppcre solo es resoluble por quicklisp; asdf puro falla al recompilar rules.lisp).
- Caché FASL a purgar al cambiar código: `rm -rf ~/.cache/common-lisp/sbcl-2.4.1-linux-x64/home/mtk/quicklisp/local-projects/cl-harness/`.

## Work State
### Completed
- **Prototipo 2 motores validado** (engine-prototype.lisp): aislamiento, loop declarativo, promoción, goal/not/retract.
- **Integración escrita y compilando**:
  - `engines.lisp` (NUEVO): install-dual-engines (en load, setea `*active-engine*` + `*active-context*` al turn), `with-turn-engine`/`with-mem-engine`, `reset-turn-engine`/`reset-mem-engine`, `durable-type-p`, `collect-harness-facts` (vía retrieve, no `typep`), `mem-engine-facts`, `promote-durable-facts` (ahora itera `(with-turn-engine (collect-harness-facts))` — antes `(collect-active-facts)` sin wrapper, funcionaba igual pero se envolvió para robustez), `boot-memory`.
  - `rules.lisp`: `read-triple-p` + regla `detect-read-triple` (salience 10), `turn-loop-facts`, `read-loop-warning-text`, `command-loop-warning-text`, `tool-loop-warning` reescrita (hace `(run)`), regla `mem-dedup-durable` (toplevel, `with-mem-engine`).
  - `context.lisp`: `mem-context-block` + append en `build-yaml-context` (fix paréntesis).
  - `persist.lisp`: `mem-session-facts-path` (global), `dump-mem-facts-to-lisp`, `load-mem-engine`, `clear-mem-persistence`; save/restore ampliados.
  - `repl.lisp`: `:clear`→reset-turn-engine, clear-all, process-turn llama promote-durable-facts, run-one-shot/start-harness → boot-memory.
  - `packages.lisp` exports nuevos; asd inserta `(:file "engines")`.
- **Bugs reales encontrados y corregidos durante la verificación**:
  1. **Paren deficit en rules.lisp**: la cláusula final `(t nil)))))))` (línea 493) cerraba MAL – el `(defun tool-loop-warning ...)` quedaba SIN cerrar y "tragaba" el bloque `(with-mem-engine (defrule mem-dedup-durable ...))` → `tool-loop-warning` DEVOLVÍA `#<RULE MEM-DEDUP-DURABLE>` (última forma del defun), y la regla mem nunca se registraba a toplevel (`get-rule-list` mem = NIL). Fix en DOS puntos: `(t nil))))))))` (añadir `)`) + línea 508 `(retract ?older)))` (quitar `)`). Verificado por `read`-loop desde el archivo en runtime (dbg9).
  2. `tool-loop-facts` UNDEFINED en runtime en un run = fasl stale (al recompilar con quicklisp desapareció; NO era bug de código).
- **Verificación completa: 23/23 PASS** (dual-verify.lisp): aislamiento de motores, reglas por engine, read-loop dispara mid-turn, alerta 1/turno, sin falsos positivos, command-loop, promoción durable (2 conservados, conversación no), bloque long_term_memory en contexto, dump/restore/boot-memory, clear-mem-persistence (engine+file).
- **Prueba E2E real EXITOSA** (12:52): `./cl-harness "dile que arregle los problemas"` → el modelo encontró el test `test_accent_changes_sustain` fallando, editó `workspace/synth/engine.py` (note_on escala sustain cuando accent), corrió `pytest` → 32/32 PASS, reportó. Sin aborts de loop-detector. Herramientas: 1 edit_file, 4 exec_command, 3 read_file; 5 LLM-iterations; 29147 real-prompt-tokens. Memoria larga persistida (`dumps/longterm-mem.lisp` con los 2 command-exec durable reales del turno 1). Métricas en `metrics/session-3998649166-metrics.json`.

### Active
- Nada pendiente de la arquitectura dual-engine.

### Blocked
- Ninguno.

## Next Move
- Opcional/proximas mejoras (no solicitadas): distinguir "shell quoting error" de errores reales del proyecto en la conservación de command-exec (los 2 durables actuales son artefactos de shell quoting, no bugs del código del LAB); podar archivos basura del test-bed (`=320`, `_tmp_script.py`); ejecutar 1 run más para ver `long_term_memory` alimentando el contexto del turno siguiente. Reporte al usuario en español ya entregado.

## Relevant Files
- `/home/mtk/quicklisp/local-projects/cl-harness/src/engines.lisp`: 2 motores, promoción, boot, collect-harness-facts.
- `/home/mtk/quicklisp/local-projects/cl-harness/src/rules.lisp`: reglas de loop, tool-loop-warning, mem-dedup-durable (toplevel tras fix de paréntesis).
- `/home/mtk/quicklisp/local-projects/cl-harness/src/context.lisp`: mem-context-block, collect-active-facts (correcto: devuelve instancias harness-fact).
- `/home/mtk/quicklisp/local-projects/cl-harness/src/persist.lisp`, `repl.lisp`, `packages.lisp`, `cl-harness.asd`.
- `/tmp/opencode/dual-verify.lisp`: suite 23 checks (todo PASS tras fixes; test read-loop usa timestamps crecientes; sección promoción resetea turn primero).
- `/tmp/opencode/load-harness.lisp`: loader standard (quicklisp).
- `/tmp/opencode/dbg1..dbg17.lisp`: diagnóstico (dbg9 = dump del defun real; dbg12 = reglas por engine).
- `/home/mtk/TEST-PRUEBAS/cl-harness-test-proyect/`: binario `cl-harness`, config.json, dumps/longterm-mem.lisp, metrics.
- `/home/mtk/quicklisp/local-projects/Lisa/src/core/preamble.lisp`, `rete.lisp`, `language.lisp`: API LISA.