# Experimentos — cl-harness

Bitácora de experimentos de validación del harness. Cada subdirectorio
`EXPERIMENTS/<nombre>/` contiene los artefactos crudos (config, dumps, metrics,
sessions, proyecto de prueba) y esta raíz indexa cada experimento con su
resultado.

## Índice

| Experimento | Fecha | Setup | Veredicto |
|-------------|-------|-------|-----------|
| [`batch-freellm-2026-09-23`](./batch-freellm-2026-09-23/) | 2026-09-23 | Modo batch, proveedor freellm (modelos random, latencias 0.4–120 s), sin GPU | No colapsó; validó la división plan/ejecución y expuso hallazgos (blind-writes, métricas ciega a batch) |