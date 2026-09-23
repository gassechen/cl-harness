# README_EXPERIMENT.md — Batch contra el peor entorno real (freellm)

> **Fecha:** 2026-09-23 · **Setup:** i5, 16 GB RAM, sin GPU
> **Artifacts:** [`EXPERIMENTS/batch-freellm-2026-09-23/`](EXPERIMENTS/batch-freellm-2026-09-23/)

## 1. Tesis en prueba

cl-harness separa **quién decide** de **quién ejecuta**:

- El LLM **escribe** — texto y una secuencia de intenciones (S-expressions),
  como si perforara tarjetas en un batch a lo COBOL de 1950. No tiene poder de
  ejecución.
- La red **Rete (LISA)** arbitra — las intenciones entran como `factos`,
  las reglas `execute-intention-*` deciden si emitir o abortar, y los frenos de
  emergencia (`batch-emergency-brake`, `cancel-intentions-on-abort`, salience
  20) corren *antes*.
- El **SO ejecuta** — `exec-command`, `read-file`, `write-file`, `edit-file`,
  todas re-asertando hechos y alimentando pruning/loop-detection sin cambios.

**Hipótesis del experimento:** si el sistema funciona contra *lo peor* —modelos
random, conexiones pésimas, latencias de decenas de segundos, cambios de modelo
entre turnos— entonces, mejorando el entorno, solo puede mejorar.

## 2. Setup del experimento

Proyecto de prueba: `todo.py` + `test_todo.py` (unittest) + `cli.py` (Todo CLI).

Config relevante (`config.json`):

```json
{
  "llm_provider": "batch",
  "llm_model":  "auto",
  "llm_endpoint": "http://localhost:3001/v1/chat/completions",
  "max_context_facts": 30,
  "fact_ttl_seconds": 600,
  "max_facts_per_type": 15,
  "max_context_chars": 20000
}
```

System prompt: modo `<batch_execution_mode>` (ver
`EXPERIMENTS/.../project/system-prompt.md`) con límites de exploración: **máx.
4 lecturas o 4 comandos** antes de responder.

Prompt del usuario: *"informa estado del proyecto y los pasos a seguir"*.

## 3. Log real del proxy (freellm, `llm_model: auto`)

Como el proveedor es un proxy de modelos gratuitos con `auto`, el harness
recibió **un modelo distinto en casi cada llamada**, con latencias que fueron de
372 ms a 120 096 ms, timeouts y errores:

| Hora | Modelo | Provider | Estado | Iter | In | Out | Latencia |
|------|--------|----------|--------|------|----|-----|----------|
| 09:10:01 | `nvidia/ising-calibration-1.5-31b` | nvidia | success | 2 | 2.8K | 946 | 98 095 ms |
| 09:08:24 | `nvidia/nemotron-3-super-120b-a12b:free` | openrouter | **error** | — | 2.2K | 0 | 990 ms |
| 09:08:23 | `openai/gpt-oss-20b` | nvidia | success | 1 | 1.4K | 443 | 33 483 ms |
| 09:07:49 | `voxtral-small-latest` | mistral | success | 1 | 1.2K | 393 | 3 118 ms |
| 09:07:46 | `openai/gpt-oss-safeguard-20b` | groq | success | 1 | 923 | 606 | 1 226 ms |
| 09:03:03 | `openai/gpt-oss-safeguard-20b` | groq | success | 1 | 2.6K | 1.7K | 2 659 ms |
| 09:02:33 | `command-r-08-2024` | cohere | success | 1 | 2.5K | 53 | 1 913 ms |
| 08:59:50 | `voxtral-small-latest` | mistral | success | 1 | 1.9K | 389 | 2 912 ms |
| 08:59:47 | `nvidia/nemotron-3-nano-...-reasoning` | nvidia | **canceled** | 2 | 0 | 0 | **120 096 ms** |
| 08:58:02 | `command-a-plus-05-2026` | cohere | **error** | — | 1.6K | 0 | 15 010 ms |
| 08:56:28 | `nvidia/nemotron-3-super-120b-a12b:free` | openrouter | success | 1 | 1.8K | 980 | 12 272 ms |
| 08:56:16 | `mistral-code-latest` | mistral | success | 2 | 1.8K | 25 | 60 930 ms |
| 08:56:15 | `nvidia/nemotron-3.5-lightning:free` | openrouter | **error** | — | 1.6K | 0 | 60 006 ms |
| 08:55:15 | `codestral-latest` | mistral | success | 1 | 1.8K | 25 | 867 ms |
| 08:55:14 | `mistral-code-latest` | mistral | success | 1 | 1.3K | 25 | 1 036 ms |
| 08:55:13 | `command-a-plus-05-2026` | cohere | success | 1 | 897 | 1.3K | 5 674 ms |
| 08:55:07 | `mistral-code-latest` | mistral | success | 1 | 990 | 10 | 716 ms |
| 08:55:06 | `gemini-2.5-flash` | google | success | 2 | 1.0K | 11 | 40 481 ms |
| 08:54:26 | `groq/compound` | groq | **error** | — | 903 | 0 | 372 ms |
| 08:43:59 | `nvidia/nemotron-3-ultra-550b-a55b` | nvidia | **error** | 3 | 4.5K | 0 | 47 694 ms |
| 08:43:57 | `google/gemma-4-31b-it:free` | openrouter | **error** | — | 4.5K | 0 | 46 618 ms |
| 08:43:56 | `gemini-3.7-flash` | google | **error** | — | 4.5K | — | — |

Modelos reales que tocaron este turno (el `auto` rotó incluso *dentro* del
mismo turno): `gpt-oss-20b`, `ising-calibration-1.5-31b`, `voxtral-small`,
`command-r`, `gpt-oss-safeguard`, entre otros.

## 4. Qué pasó (turno 1, sesión `session-3999154036`)

Secuencia de hechos registrada (`dumps/session-3999154036-facts.lisp`, ordenado
por timestamp):

1. `user-input` — *"informa estado del proyecto y los pasos a seguir"*.
2. Exploración: `ls -R .` (exit 0), intentos de leer `README.md` (no existía
   aún) y `src/main.py` (no existe) → fallos registrados como facts, no fatales.
3. **El LLM escribió el README.md que faltaba** (691 bytes) y editó
   `system-prompt.md` (aplicado, 34 chars → 528).
4. Intentó editar `todo.py` y `test_todo.py` **sin haberlos leído (blind
   write)** → ambas fallaron limpio: `applied: false, reason: "old_string not
   found"`. **Nada se corrompió.**
5. Segunda pasada del bucle batch (re-llamada, el timestamp ~40 s después):
   re-leyó `todo.py`, `system-prompt.md`, `config.json`, `test_todo.py`.
6. Emitió el informe final (texto plano coherente) y asertó `batch-complete` →
   el turno cerró solo.

**Veredicto del turno:** con un modelo desconocido (rotando), latencias de
hasta 98 s, un round-trip cancelado a los 120 s y errores de provider, el
harness ejecutó un plan, sobrevivió a sus propios errores de edición y entregó
un informe útil. **No colapsó.**

## 5. Métricas (sesión 3999154036)

```json
{"turns":1,"context-tokens":29,"naive-tokens":41,"reduction-pct":29.27,
 "real-prompt-tokens":2756,"llm-iterations":0,"tools":false}
```

- Contexto curado: 116 chars / ~29 tokens; línea base naive: 161 chars / 41.
- Uso real del proveedor: **2756 prompt tokens** acumulados (la re-llamada del
  bucle reenvía la pila creciente, exactamente como advierte el caveat de §11
  del README).

## 6. Hallazgos (nuevos, no documentados)

1. **Auto-recuperación de blind-writes, sin regla, por construcción.**
   El modelo editó archivos sin leerlos y el sistema no colapsó porque
   `edit-file` es quirúrgico: `old_string not found` = no toca nada y registra
   el hecho. La regla `detect-blind-writes` existe pero **sigue sin estar
   conectada** (README §7): hoy la protección es la atomicidad de `edit-file`,
   no el freno. → **Refuerzo lógico próximo: conectar `detect-blind-writes` a
   una regla que aserte `batch-abort`.**

2. **`batch-emergency-brake` no disparó — y no tenía que disparar.**
   Solo se activa con `command-exec` con error real; aquí el único comando fue
   `ls` (exit 0) y los `file-edit` fallidos **no son evidential**
   (`evidential-type-p` no los incluye). El comportamiento fue correcto, pero
   delimita el alcance del freno: un plan que se equivoca *leyendo* no lo
   corta; solo un comando fallido real lo corta.

3. **Métricas ciegas al modo batch.** `llm-iterations: 0` y `tools: false` con
   un turno que **sí** hizo dos pasadas (2 llamadas LLM). El contador de
   iteraciones y el flag de tools están implementados para el tool-loop HTTP,
   no para el bucle Rete del batch. → El contador debe sumar las re-llamadas
   del modo batch.

4. **Modelo distinto por llamada dentro del mismo turno: irrelevante.**
   El plan lo escribió un modelo y la re-lectura + informe final otro. Funcionó
   porque nada depende de la memoria del modelo: el estado vive en los facts y
   Rete reconstruye el contexto.

5. **La latencia de 98 s se pagó una vez, no N veces.** En tool-use esa
   latencia se multiplica por cada iteración (12 × 98 s = 20 min/turno). El
   batch la absorbe como costo fijo de planificación.

## 7. Conclusión

El experimento no demuestra que el modelo fuera bueno — **demuestra que el
harness no necesita que el modelo sea bueno**. La separación *el LLM escribe,
las reglas disponen, el SO ejecuta* aguantó el peor caso real completo. Con
solo mejorar el vecindario (conexión, estabilidad, calidad del modelo) el mismo
pipeline solo puede mejorar.

> Método de validación: "se hace ingeniería para las peores condiciones. Si
> funciona ahí, mejorando el entorno solo puede mejorar."

Siguientes refuerzos sugeridos (ver §6): conectar `detect-blind-writes`,
contar iteraciones del modo batch en métricas, y evaluar el freno de emergencia
frente a errores de edición, no solo de comando.