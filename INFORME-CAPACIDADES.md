# Informe de capacidades — cl-harness v0.1.0

> **Fecha:** 2026-09-25  
> **Alcance:** verificado contra el código de `src/` (13 archivos, 3 820 líneas) y
> contra las corridas reales documentadas en [`EXPERIMENTS/`](./EXPERIMENTS/).
> **Método:** cada capacidad apunta a su implementación `archivo:línea`. Las
> limitaciones se declaran por **ausencia de código** (búsqueda de callers, de
> opciones del sistema y de tests), no por suposición.

## 1. Resumen ejecutivo

cl-harness es una **capa de gestión de contexto para agentes LLM**: mantiene los
hechos de una sesión en un motor Rete (Lisa), los poda con reglas explícitas, los
selecciona por relevancia estructural y los renderiza como un bloque YAML con
presupuesto de caracteres. Sobre esa memoria ejecuta acciones POSIX
(`read_file`, `write_file`, `edit_file`, `exec_command`) que pueden planificarse
por JSON batch o por tools nativas del proveedor.

Lo que **no** es:

- no es un framework de agente autónomo: no planifica, no decide objetivos ni
  reintenta tareas por su cuenta;
- no tiene sandbox: ejecuta comandos y escribe archivos con los permisos del
  usuario que lo lanza;
- tiene 52 tests automatizados y offline (`./run-tests.sh`), pero no hay CI;
- su memoria es un **working set podado**, no un historial: lo viejo se olvida a
  propósito y hay que volver a leerlo.

## 2. Mapa de módulos

| Archivo | Líneas | Responsabilidad |
|---|---:|---|
| `src/packages.lisp` | 68 | API pública (46 símbolos exportados) |
| `src/config.lisp` | 159 | Lectura de `config.json` y getters de límites |
| `src/engines.lisp` | 107 | Dos motores Lisa: working memory y memoria durable |
| `src/facts.lisp` | 140 | Plantillas de hechos, contadores de turno, todos y epochs |
| `src/rules.lisp` | 625 | Poda por reglas, deduplicación, loops, ejecución de intenciones |
| `src/context.lisp` | 451 | Relevancia, selección, render YAML |
| `src/metrics.lisp` | 178 | Métricas por turno, baseline `naive`, export JSON |
| `src/actions.lisp` | 242 | Acciones POSIX y sus hechos |
| `src/background.lisp` | 121 | Relanzamiento de comandos como daemons |
| `src/llm.lisp` | 1 077 | Proveedores, tools, streaming, modo batch JSON |
| `src/persist.lisp` | 207 | Dumps, memoria durable en disco, core image |
| `src/repl.lisp` | 356 | Turno, REPL, comandos, un modo one-shot |
| `src/main.lisp` | 62 | Entry points, handlers de señales |

## 3. Lo que sí hace

### 3.1 Memoria de trabajo y memoria durable

- Dos motores Rete aislados: `*turn-engine*` (working memory del turno) y
  `*mem-engine*` (conocimiento durable). Los hechos de uno son invisibles al otro
  (`src/engines.lisp:28`).
- Al final de cada turno se **promueven** solo los hechos durables: errores reales
  de comandos y escrituras de archivos (`src/engines.lisp:83`,
  `src/engines.lisp:67`).
- La memoria durable se persiste en `dumps/longterm-mem.lisp` y se recarga
  sola al arrancar (`src/engines.lisp:101`, `src/persist.lisp:19`).
- Deduplicación durable last-write-wins en el motor de memoria
  (`src/rules.lisp:514`).

### 3.2 Poda de contexto

- **TTL por turnos y por reloj**, solo para hechos re-derivables
  (`src/rules.lisp:95`, `src/rules.lisp:119`). Los hechos de conversación
  (`user-input`, `llm-response`) nunca expiran por TTL.
- **Conservación de errores reales**: un comando fallido con evidencia en la
  salida no expira y suma 400 puntos de relevancia; los sondeos benignos que
  fallan sin salida no se conservan (`src/rules.lisp:83`, `src/context.lisp:173`).
- **Deduplicación last-write-wins** por clave estructural (comando, ruta, familia
  de loop), conservando ambas versiones si el contenido cambió
  (`src/rules.lisp:44`, `src/rules.lisp:126`).
- **Topes por tipo** aplicados en cada render: se retractan los hechos viejos de
  cada tipo por encima de `max_facts_per_type` (`src/context.lisp:432`).
- **Presupuesto de caracteres** y **truncado de payload** con cabeza y cola, para
  que un archivo enorme no ahogue el contexto (`src/context.lisp:135`,
  `src/context.lisp:190`).

### 3.3 Selección por relevancia

Puntaje estructural y semántico (`src/context.lisp:158`):

| Criterio | Peso | Fuente |
|---|---:|---|
| Proximidad al turno actual | 500 − 60·distancia | decaimiento exponencial por turno |
| Garantía de conversación | 300 | `user-input` y `llm-response` |
| Error real | 400 | `real-error-p` |
| Repetición de la misma clave | 40·frecuencia (tope 200) | reintentos = señal de importancia |
| Match de keywords del prompt | 450 ruta / 400 comando / 80·contenido | `src/context.lisp:113` |
| Causalidad (`parent-id`) | 50 | encadenamiento de acciones |

El contenido de un hecho seleccionado nunca se degrada: la poda es de estructura,
no de texto.

### 3.4 Render del contexto

`build-yaml-context` corre las reglas, aplica topes, selecciona y renderiza
(`src/context.lisp:428`). Secciones del YAML:

| Sección | Origen |
|---|---|
| `context.session` | id de sesión |
| `context.epoch` | checkpoint, si existe |
| `context.goals` | `agent-todo`, si existen |
| `context.warnings` | hechos `tool-loop` |
| `context.prior_turns` | turnos cerrados, agrupados por causalidad |
| `context.current_turn` | eventos del turno en curso |
| `long_term_memory` | bloque aparte con el motor de memoria (`src/context.lisp:403`) |

Los archivos leídos y las salidas de comando se truncan al renderizar, no al
leer (`src/context.lisp:253`).

### 3.5 Transportes de ejecución

`call-llm` despacha por `llm_provider` (`src/llm.lisp:1061`):

| `llm_provider` | Ruta | Streaming | Notas |
|---|---|---|---|
| `batch` | JSON `{"tool_calls":[...], "response":""}` con `response_format: json_object` | no | Compatible con modelos sin ToolUse nativo |
| `anthropic` | API Messages | no | |
| `gemini` | `generateContent` | no | clave en query string |
| `ollama` | `/api/generate` local | no | |
| `openrouter`, `groq`, OpenAI o cualquier compatible | `/chat/completions` | no (ruta estática) | con tools nativas |
| por defecto | `call-llm-with-tools` | **sí** | tools nativas + SSE |

Modo batch: reintento acotado (2 intentos) ante JSON o protocolo inválido, con
mensaje correctivo al modelo (`src/llm.lisp:1002`), y validación del lote completo
antes de asertar intenciones, sin ejecución parcial (`src/llm.lisp:1027`).

### 3.6 Herramientas

Cuatro herramientas, definidas una vez y reutilizadas por ambas rutas
(`src/llm.lisp:82`, `src/llm.lisp:171`):

| Herramienta | Qué hace | Registra |
|---|---|---|
| `read_file` | lee texto UTF-8 con fallback byte a byte | `file-read` |
| `write_file` | crea archivos; **rechaza** existentes de más de 2 000 bytes | `file-write` |
| `edit_file` | reemplaza el primer snippet exacto | `file-edit` (con `applied` y motivo) |
| `exec_command` | shell, exit code real, stderr fusionado, timeout | `command-exec` |

Detalles operativos: se ignoran prefijos `cd ABS &&`, los comandos corren en el
directorio base, y un comando que excede el timeout se relanza genéricamente como
daemon con logfile (`src/actions.lisp:46`, `src/actions.lisp:63`,
`src/actions.lisp:70`, `src/background.lisp:108`).

### 3.7 Salvaguardas

- **Lectura antes de escribir**: `detect-blind-writes` aborta el lote si una
  intención de escritura no tiene una lectura previa en el mismo turno, y una regla
  de saliencia 20 retracta las intenciones restantes
  (`src/rules.lisp:570`, `src/rules.lisp:606`).
- **Detección de loops**, en dos capas:
  - declarativa: tres comandos fallidos del mismo turno con prefijo común
    (`src/rules.lisp:295`) o tres lecturas del mismo archivo
    (`src/rules.lisp:327`), que derivan un hecho `tool-loop`;
  - imperativa: fallback que cuenta lecturas y sondeos mixtos
    (`src/rules.lisp:389`, `src/rules.lisp:343`).
  - El aviso se inyecta una vez por turno y corta el bucle
    (`src/rules.lisp:458`).
- **Freno de emergencia**: un error real en el turno retracta intenciones
  pendientes (`src/rules.lisp:616`).
- **Persistencia en señales**: `SIGTERM`/`SIGINT` guardan métricas y sesión antes
  de morir (`src/main.lisp:16`, `src/main.lisp:23`).

### 3.8 Persistencia y recuperación

- Facts a `.lisp`, sesión a `.json`, métricas a `.json`, con payloads acotados al
  guardar (`src/persist.lisp:24`, `src/persist.lisp:144`).
- `restore-session` recarga hechos desde un dump, y los contadores de turno, todos
  y epochs se restauran con ellos (`src/persist.lisp:152`).
- `create-session-dump` produce un **core image ejecutable** con la sesión viva
  dentro, que reanuda el REPL al ejecutarse (`src/persist.lisp:188`).

### 3.9 Métricas

- Por turno: tokens de contexto, tokens `naive`, tokens reales del proveedor,
  iteraciones, ruta de llamada, y estadísticas por herramienta
  (`src/metrics.lisp:45`).
- Totales con `reduction_pct` = cuánto ahorra el YAML frente a `naive`
  (`src/metrics.lisp:92`).
- El baseline `naive` se captura **antes** de la poda, que es lo que hace válida
  la comparación (`src/metrics.lisp:35`).
- Export JSON por sesión (`src/metrics.lisp:159`) y tabla legible en `:metrics`.

### 3.10 Configuración

20+ knobs en `config.json`, todos leídos por getters de `src/config.lisp`:
límites de facts, TTL (turnos y reloj), topes por tipo, presupuesto de
caracteres, truncado de payload, timeout de herramientas, relanzamiento en
background, conservación de errores, iteraciones máximas de tool-use, tokens
máximos, streaming, modelo, endpoint y clave.

### 3.11 Operación

- `./run.sh` (TUI con `rlwrap`) y `./run.sh run "mensaje"` (un turno).
- `./build.sh` produce `bin/cl-harness` standalone, que resuelve `config.json`,
  `dumps/`, `sessions/` y `metrics/` contra el directorio de ejecución, y acepta
  `CL_HARNESS_DIR`, `CL_HARNESS_CONFIG` y `CL_HARNESS_PROMPT`.
- Comandos del REPL: `:context`, `:metrics`, `:facts`, `:rules`, `:save`,
  `:restore`, `:dump`, `:clear`, `:clearall`, `:model`, `:provider`, `:debug`.

## 4. Lo que NO hace

### 4.1 Verificación y calidad

- **Ya hay suite automatizada (52 casos, toda en verde).** Vive en
  `tests/harness.lisp`, expuesta como el sistema `"cl-harness/tests"` en
  `cl-harness.asd` y ejecutable con `./run-tests.sh` (o `./run-tests.sh metrics`
  para filtrar por nombre). Los casos se registran a mano en `*test-cases*`
  porque el registro interno de Rove no sobrevive a la carga desde FASL.
  Cubre ambos motores, reglas por engine, detección de loops, promoción
  durable, render de contexto, parser batch y normalización ToolUse, dedup/TTL,
  topes por tipo, guardas de escritura, métricas, y —nuevo— comportamiento:
  topes del bucle batch y política de reintentos HTTP.
- **El suite es offline por contrato:** durante la corrida `dexador:post` se
  sustituye por una señal, así que una fuga de red falla ruidosa en vez de
  gastar una llamada real (esto ya pasó: un `flet` sobre `call-llm` no intercepta
  la llamada global de `process-turn` y el test se-wasaba a la API de verdad).
- **No hay CI**, ni lint, ni typecheck. La compilación sigue emitiendo
  style-warnings, todos del tipo "variable definida pero no usada" (gensyms que
  genera la macro `lisa:retrieve`, más `*pids*` y el parámetro `facts` de
  `relevance-score`). Ver §7.
- **No verifica que el modelo haya hecho lo que pidió.** El harness ejecuta y
  registra; no comprueba el resultado de la tarea.

### 4.2 Red y resiliencia

- **El reintento de transporte y el backoff ya existen** (`call-with-retry` +
  `dexador-post-with-retry`): reintenta `429`/`5xx` y errores de transporte con
  backoff exponencial y jitter, hasta `llm_http_attempts` (default 3). Un `4xx`
  que no sea `429` sube de inmediato. Cubierto por 4 tests con condiciones
  sintéticas, sin sockets.
- `read-timeout` (default 120 s, `llm_http_read_timeout`) está en las rutas
  OpenAI-compatible; las rutas por proveedor propio no lo definen.
- **El bucle batch ya tiene topes configurables**: `batch_max_iterations`
  (default 8) y `batch_repeat_tolerance` (default 1, corta cuando el modelo
  repite el mismo lote exacto). Ya no hay tope duro de 100 rondas. Cubierto por
  2 tests de comportamiento.
- La afirmación del experimento previo sobre "recuperar un `502`" se corrigió
  en `EXPERIMENTS/batch-freellm-auto-2026-09-25/README.md`: el log no registra
  el mecanismo de recuperación y en esa corrida todavía no había reintento de
  `5xx`, así que no era evidencia de recuperación automática.

### 4.3 Seguridad y aislamiento

- **No hay sandbox.** `exec_command` corre cualquier comando con los permisos del
  usuario; no hay allowlist de comandos ni de rutas.
- **`write_file` y `edit_file` aceptan rutas absolutas** fuera del directorio de
  trabajo (`src/actions.lisp:32`).
- **El guard de `old_string` vacío solo existe en el validador batch**
  (`src/llm.lisp:960`) y en el prompt del protocolo. En la ruta de tools nativas
  y en la acción `edit-file` no hay guarda: un `old_string` vacío hace
  `(search "" ...)` devolver 0 y **prepende** el texto en silencio
  (`src/actions.lisp:169`).
- **No hay defenses contra prompt injection** vía contenido de archivos: lo que se
  lee entra al YAML como hechos y el modelo decide si lo obedece.
- `config.json` y los logs pueden contener la clave si el modelo los lee; el
  harness no los redacta al persistir.

### 4.4 Memoria y planificación

- **Los todos y los epochs están implementados pero no cableados.**
  `add-todo`, `complete-todo`, `set-todo-status` y `create-epoch`
  (`src/facts.lisp:57`, `src/facts.lisp:123`) **no tienen ningún caller** en
  `src/`: no hay herramienta, ni comando de REPL, ni regla que los invoque.
- En consecuencia, la regla `prune-superseded-by-epoch` (`src/rules.lisp:136`)
  nunca se activa en la práctica y el bloque `goals:` del YAML siempre sale
  vacío.
- **La memoria de trabajo no sobrevive al proceso.** Solo sobreviven la memoria
  durable y los core images. Por eso `run-one-shot` reinicia sesión y métricas en
  cada ejecución (`src/repl.lisp:272`).
- **No hay memoria semántica ni vectorial**: la relevancia es estructural más
  coincidencia de keywords.

### 4.5 Métricas y observabilidad

- **No se persiste `response.model`**: hay que leerlo de la respuesta del
  proveedor. Con `llm_model: "auto"` las columnas de tokens reales mezclan
  tokenizadores.
- `context-tokens` y `naive-tokens` son heurísticos (1 token ≈ 4 caracteres).
- `prompt-tokens` es acumulativo por turno sobre las iteraciones del tool loop:
  no es comparable turno a turno como tokens de contexto.
- **No hay cálculo de costo**, ni de tokens por tipo de hecho, ni por modelo.
- **No hay logging estructurado**: la salida es `format t` a stdout; el contexto
  solo se vuelca a `debug.yaml` cuando `*debug-mode*` está activo
  (`src/context.lisp:447`).

### 4.6 Operabilidad y alcance

- **Un solo agente, un solo turno a la vez.** No hay concurrencia ni múltiples
  agentes; los únicos threads son los daemons de `background.lisp`.
- **No hay control de presupuesto de tokens ni de gasto por sesión**: el único
  límite es de caracteres del bloque de contexto.
- `max_raw_chars` tiene getter en `src/config.lisp:92` y **no se usa en ningún
  sitio**: configuración muerta.
- **No hay versionado de esquema** de `metrics/*.json` ni de `sessions/*.json`.
- **No hay streaming** en batch, anthropic, gemini, ollama ni en la ruta
  OpenAI-compatible estática: solo en `call-llm-with-tools`
  (`src/llm.lisp:809`).
- **Sin dependencias de plataforma**: el harness es POSIX + SBCL. `build.sh`
  asume un SBCL sin `:SB-CORE-COMPRESSION` (`src/persist.lisp:192`).

## 5. Límites con evidencia de campo

| Límite | Evidencia |
|---|---|
| El modelo puede atascarse en un bucle de herramientas | 14 rondas en el turno 6 de `cl-harness-growth-attempt1`; el tope batch es 100 |
| El modelo devuelve respuestas vacías o duplica trabajo | 3 de 5 turnos sin cambio útil en el intento 1; `gcd` duplicado en el intento 2 |
| Un `old_string` desactualizado rompe la edición | `edit_file` con `applied: false, old_string not found` en el intento 1 |
| La reducción de tokens depende de la configuración | `-35,90%` en proyecto chico vs `+53,91%` en 8 turnos ([`EXPERIMENTS/`](./EXPERIMENTS/)) |
| La memoria olvida a propósito | En 8 turnos quedaron 40 hechos = `max_facts_per_type` (8) × 5 tipos |

## 6. Matriz de verificación

| Capacidad | Evidencia | Cómo se comprobó |
|---|---|---|
| Poda y render YAML | 8 turnos, 40 hechos finales, YAML estable en ~7 k chars | `EXPERIMENTS/context-growth-8turns-2026-09-25/` |
| Lectura antes de escribir | abort de lote en intención sin lectura previa | regla `cancel-intentions-on-abort` |
| Deteccion de loops | aviso `[HARNESS LOOP DETECTOR]` y corte del turno | `cl-harness-growth-attempt1/run.log` |
| Batch JSON con modelo sin tools | 5 llamadas, 9/9 tests, sin `BATCH_JSON_INVALID` | `EXPERIMENTS/batch-freellm-auto-2026-09-25/` |
| Persistencia ante SIGTERM | métricas y sesión flushed al detener la corrida | `session-3999339038-metrics.json` |
| Carga del sistema | `LOAD_OK` con Quicklisp | `ql:quickload :cl-harness` |
| Suite offline | 52/52 PASS, `RESULTADO: OK` | `./run-tests.sh` |
| Retry HTTP | 4 casos: 503 reintenta, 429 respeta el tope, 400 no reintenta, transporte reintenta | `./run-tests.sh config/retry` |
| Topes batch | 2 casos: corta por `:iterations` y por `:batch-repeated` antes del tope | `./run-tests.sh "batch/"` |

## 7. Deuda técnica priorizada

**P0 — corrección (cerrado)**

1. ~~Guard de `old_string` vacío~~ → ahora está en `edit-file`
   (`src/actions.lisp`), no solo en el validador batch.
2. ~~Tope duro de 100 rondas en el bucle batch~~ → `batch_max_iterations` +
   `batch_repeat_tolerance`, con tests de comportamiento.
3. ~~Reintento con backoff para 429/5xx~~ → `call-with-retry`, con 4 tests
   offline.

**P1 — verificación y observabilidad (cerrado)**

4. ~~Sistema de tests~~ → `tests/harness.lisp` + sistema `"cl-harness/tests"` +
   `./run-tests.sh`, 52 casos en verde y guardián offline.
5. ~~Persistir `response.model`~~ → `*llm-response-model*` y
   `*llm-call-log*` por llamada, incluidos en `metrics-to-json`.
6. ~~Separar `prompt-tokens` por iteración~~ → `*llm-call-log*` acumula el uso
   por request; el total por turno es la suma.

**Pendiente que sigue abierto (fuera del alcance de este lote)**

7. `build-openai-compat-messages-with-tools` recibe `system-prompt` y no lo usa:
   hoy los dos callers (``stream-llm-with-tools`` y `call-llm-with-tools/static``)
   precargan el mensaje de sistema, así que el prompt llega, pero si alguien
   llama al builder con `prior-messages` vacío lo pierde. Compila con
   style-warning por eso.
8. `restore-facts` / `restore-mem-facts` están llamadas desde `persist.lisp` pero
   no existen: `:restore` no restaura nada en runtime (style-warning
   "undefined function").
9. Style-warnings por variables no usadas: `*pids*` (`src/background.lisp`) y el
   parámetro `facts` de `relevance-score` (`src/context.lisp`). El resto son
   gensyms que genera `lisa:retrieve`.

**P2 — functionality**

10. Cablear todos y epochs: herramienta o comando de REPL, y una política que cree
    epochs automáticamente (por ejemplo, cada N turnos o al superar un umbral de
    hechos).
11. Presupuesto de tokens por sesión, con corte explícito. **No se implementó
    ninguna tabla de precios**: el harness no estima costo, sólo tokens
    estimados y tokens reportados por el proveedor.
12. Borrar `max_raw_chars` o documentarlo como reservado.
