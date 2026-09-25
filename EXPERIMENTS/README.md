# Experimentos — cl-harness

Bitácora de experimentos de validación del harness. Cada subdirectorio
`EXPERIMENTS/<nombre>/` contiene los artefactos disponibles del experimento
(config, métricas, sessions, dumps y proyecto de prueba, según corresponda) y
esta raíz indexa cada experimento con su resultado.

## Índice

| Experimento | Fecha | Setup | Veredicto |
|-------------|-------|-------|-----------|
| [`batch-freellm-2026-09-23`](./batch-freellm-2026-09-23/) | 2026-09-23 | Modo batch, proveedor freellm (modelos random, latencias 0.4–120 s), sin GPU | No colapsó; validó la división plan/ejecución y expuso hallazgos (blind-writes, métricas ciega a batch) |
| [`batch-freellm-auto-2026-09-25`](./batch-freellm-auto-2026-09-25/) | 2026-09-25 | Modo batch JSON ToolUse, `llm_model: "auto"`, proxy local, rotación de modelos | 5 llamadas, 9/9 tests, sin `BATCH_JSON_INVALID`; recuperó un `502` transitorio |