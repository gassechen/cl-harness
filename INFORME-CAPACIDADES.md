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
selecciona por relevancia estructural y los renderiza como un programa COBOL con
presupuesto de caracteres. Sobre esa memoria ejecuta acciones POSIX
(`read_file`, `write_file`, `edit_file`, `exec_command`) que pueden planificarse
por JSON batch o por tools nativas del proveedor.

Lo que **no** es:

- no es un framework de agente autónomo: no planifica, no decide objetivos ni
  reintenta tareas por su cuenta;
- no tiene sandbox: ejecuta comandos y escribe archivos con los permisos del
  usuario que lo lanza;
- tiene una suite automatizada y offline (`./run-tests.sh`, 108 casos) que CI corre
  en cada push y PR (`.github/workflows/tests.yml`);
- su memoria es un **working set podado**, no un historial: lo viejo se olvida a
  propósito y hay que volver a leerlo.

## 2. Mapa de módulos

| Archivo | Líneas | Responsabilidad |
|---|---:|---|
| `src/packages.lisp` | 68 | API pública (46 símbolos exportados) |
| `src/config.lisp` | 193 | Lectura de `config.json` y getters de límites |
| `src/engines.lisp` | 107 | Dos motores Lisa: working memory y memoria durable |
| `src/facts.lisp` | 309 | Plantillas de hechos, contadores de turno, todos y epochs |
| `src/rules.lisp` | 726 | Poda por reglas, deduplicación, loops, ejecución de intenciones |
| `src/context.lisp` | 883 | Relevancia, selección, render del programa COBOL |
| `src/metrics.lisp` | 221 | Métricas por turno, baseline `naive`, export JSON |
| `src/actions.lisp` | 308 | Acciones POSIX y sus hechos |
| `src/background.lisp` | 124 | Relanzamiento de comandos como daemons |
| `src/llm.lisp` | 1 270 | Proveedores, tools, streaming, modo batch JSON |
| `src/persist.lisp` | 317 | Dumps, memoria durable en disco, core image |
| `src/repl.lisp` | 564 | Turno, REPL, comandos, un modo one-shot |
| `src/main.lisp` | 59 | Entry points, handlers de señales |

## 3. Lo que sí hace

### 3.1 Memoria de trabajo y memoria durable

- Dos motores Rete aislados: `*turn-engine*` (working memory del turno) y
  `*mem-engine*` (conocimiento durable). Los hechos de uno son invisibles al otro
  (`src/engines.lisp:28`).
- El `.cob` se genera desde la red de largo plazo, así que los hechos se **promueven**
  al principio de cada ronda y no al final del turno. Cruzan tres cosas: qué es verdad
  ahora (lecturas con su contenido, escrituras y edits, aplicados o no), qué pasó con lo
  que pidió (plan y veredicto) y qué se le pidió y qué contestó; más los `command-exec`
  que son errores reales. El andamiaje intraturno (`intention`, `batch-abort`,
  `tool-loop`) no cruza (`src/engines.lisp:67`).
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
  cada tipo por encima de `max_facts_per_type` (`src/context.lisp:836`).
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

`build-context` corre las reglas, aplica topes, selecciona y renderiza
(`src/context.lisp:836`). Emite **un programa COBOL**, no un YAML. Bloques, en
orden de aparición:

| Bloque | Origen | ¿Siempre? |
|---|---|---|
| `IDENTIFICATION DIVISION.` + `PROGRAM-ID.` | id de sesión | sí |
| `EPOCH.` / `BASELINE-TURN.` / `SUMMARY.` | checkpoint | solo si hay epoch |
| `PROCEDURE DIVISION.` | `batch-plan`: un bloque por paso, con su `VERDICT` al lado | sí, vacía si no hay plan |
| `DATA DIVISION.` + `GOAL ABIERTO. N` | `agent-todo` **sin completar** | sí, con `GOAL ABIERTO. 0` si no hay ninguno |
| `WARNING DIVISION.` | hechos `tool-loop` | solo si hay loops |
| `TURN n.` | turnos, agrupados por `turn-id` (cerrados y en curso) | por turno con hechos |
| `MEMORY DIVISION.` | lo consolidado del motor de memoria, dentro del programa | solo si hay hechos durables |
| `GOBACK.` | cierre | sí |

`PROCEDURE` y `DATA` se emiten aunque estén vacías a propósito: un programa sin
`PROCEDURE DIVISION` no es un programa vacío, es un programa mal formado, y un
modelo que lee "hoy no hay PROCEDURE" aprende que el formato cambia de forma
(`src/context.lisp:745`). En cambio `MEMORY DIVISION` sí se omite cuando no hay
nada durable (`src/context.lisp:800`), porque una división vacía de memoria
sería ruido: "no recuerdo nada" ya lo dice la ausencia.

La gramática completa —incluido por qué `PROCEDURE` y `DATA` se emiten aunque
estén vacías y cómo se identifica un paso por `(turno, ronda, número)`— está en
[`PROTOCOLO.md`](./PROTOCOLO.md) §2.3.

Los archivos leídos y las salidas de comando se truncan al renderizar, no al
leer (`src/context.lisp:171`).

### 3.5 Transportes de ejecución

`call-llm` despacha por `llm_provider` (`src/llm.lisp:1254`):

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
- Totales con `reduction_pct` = cuánto ahorra el contexto curado frente a `naive`
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

- **Ya hay suite automatizada (108 casos, toda en verde), y CI.** Vive en
  `tests/harness.lisp`, expuesta como el sistema `"cl-harness/tests"` en
  `cl-harness.asd` y ejecutable con `./run-tests.sh` (o `./run-tests.sh metrics`
  para filtrar por nombre). Los casos se registran a mano en `*test-cases*`
  porque el registro interno de Rove no sobrevive a la carga desde FASL.
  Cubre ambos motores, reglas por engine, detección de loops, promoción
  durable, render de contexto, parser batch y normalización ToolUse, dedup/TTL,
  topes por tipo, guardas de escritura, métricas, comportamiento de los topes
  del bucle batch, política de reintentos HTTP, **el path de transporte**
  (constructores de mensajes y orden de tools) y **escritura atómica**.
- **El suite es offline por contrato:** durante la corrida `dexador:post` se
  sustituye por una señal, así que una fuga de red falla ruidosa en vez de
  gastar una llamada real (esto ya pasó: un `flet` sobre `call-llm` no intercepta
  la llamada global de `process-turn` y el test se-wasaba a la API de verdad).
- **El CI no necesita ninguna clave de API** por lo anterior. `run-tests.sh`
  instala Quicklisp en un temporal si no encuentra `~/quicklisp/setup.lisp`, de
  modo que el runner es autocontenido.
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
  (`src/llm.lisp:1129`) y en el prompt del protocolo. En la ruta de tools nativas
  y en la acción `edit-file` no hay guarda: un `old_string` vacío hace
  `(search "" ...)` devolver 0 y **prepende** el texto en silencio
  (`src/actions.lisp:171`).
- **No hay defenses contra prompt injection** vía contenido de archivos: lo que se
  lee entra al contexto como hechos y el modelo decide si lo obedezca.
- `config.json` y los logs pueden contener la clave si el modelo los lee; el
  harness no los redacta al persistir.

### 4.4 Memoria y planificación

- **Los goals están a medio cablear, y cada pieza en un sitio distinto.**
  `add-todo` (`src/facts.lisp:178`) lo invoca la regla `auto-create-todo-on-action`
  (`src/rules.lisp:655`), que crea un goal `in-progress` por cada intención del
  plan; `complete-todo` (`src/facts.lisp:210`) lo invoca `reconcile-todos`
  (`src/context.lisp:417`), que `build-context` corre en cada turno y que cierra
  un goal solo si su verbo tiene evidencia 1:1; y `create-epoch`
  (`src/facts.lisp:292`) lo llama la política automática de `process-turn` cada 5
  turnos (`src/repl.lisp:401`). Lo que **no** tiene caller es `set-todo-status`
  (`src/facts.lisp:230`): no hay herramienta ni comando de REPL que lo use, así
  que un goal solo pasa por `pending` → `in-progress` → `completed`, y no hay
  forma de reabrirlo o de marcarlo `failed` a mano.
- Las reglas Rete de auto-completado de goals están **comentadas**, no activas
  (`src/rules.lisp:672`): el cierre pasó a ser imperativo dentro de
  `reconcile-todos`, porque cuatro reglas que cerraban goals se solapaban entre
  sí y con el path de evidencia.
- La regla `prune-superseded-by-epoch` (`src/rules.lisp:136`) puede llegar a
  dispararse, y solo cuando el epoch automático ha consolidationado de verdad.
- **La memoria de trabajo no sobrevive al proceso.** Solo sobreviven la memoria
  durable y los core images. Por eso `run-one-shot` reinicia sesión y métricas en
  cada ejecución (`src/repl.lisp:482`).
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
  solo se vuelca a `debug/debug-<ts>.cob` cuando `*debug-mode*` está activo
  (`src/context.lisp:874`); esos ficheros no se podan nunca.

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
| Poda y render (el formato era YAML en esa corrida) | 8 turnos, 40 hechos finales, contexto estable en ~7 k chars | `EXPERIMENTS/context-growth-8turns-2026-09-25/` |
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
   `./run-tests.sh`, 96 casos / 335 checks en verde y guardián offline.
5. ~~Persistir `response.model`~~ → `*llm-response-model*` y
   `*llm-call-log*` por llamada, incluidos en `metrics-to-json`.
6. ~~Separar `prompt-tokens` por iteración~~ → `*llm-call-log*` acumula el uso
   por request; el total por turno es la suma.

**Pendiente que sigue abierto (fuera del alcance de este lote)**

7. ~~`build-openai-compat-messages-with-tools` recibe `system-prompt` y no lo
   usa~~ → **Arreglado, y era un P0.** El diagnóstico de este informe era
   incorrecto: no era un parámetro sin usar sino un **error de aridad en tiempo
   de ejecución**. La función estaba definida con **3** parámetros
   (`context user-message prior-messages`) y sus **dos** callers le pasaban
   **4** (`system-prompt context user-message messages`) — `src/llm.lisp:598` y
   `:740`. Verificado ejecutando la llamada: `Too many arguments`.

   El alcance era toda la ruta por defecto: `build-openai-compat-messages-with-tools`
   es lo que despacha `call-llm-with-tools`, el `else` de `call-llm`, o sea
   **todo** proveedor openai-compatible (openrouter, groq, openai) tanto en
   streaming como estática. Solo sobrevivía el modo `batch`, que es lo que
   tenía el `config.json` del repo — por eso 96 tests en verde no lo detectaron.

   Ni los 96 tests ni este informe lo cazaron porque **el mock estaba un nivel
   demasiado arriba**: `with-mocked-call-llm` sustituye `call-llm` *entera*, así
   que el builder nunca se ejecutaba. Ahora hay 8 tests nuevos
   (`transport/el-builder-*`, `transport/el-path-estatico-*`) que lo llaman
   directamente, y un CI.

8. ~~`restore-facts` / `restore-mem-facts` no existen~~ → **Falsa alarma de este
   informe.** Ambas son `defun` emitidos *dentro* del propio dump
   (`src/persist.lisp:54` y `:74`) y `restore-session` las invoca con `fboundp`
   guardado (`src/persist.lisp:248`, `:91`). La restauración funciona: es
   exactamente el fix de PROTOCOLO §4.6. Este informe describía el bug
   *antes* de arreglarlo.
9. ~~Style-warnings por variables no usadas~~ → El de `*pids*`
   (`src/background.lisp`) y el parámetro `facts` de `relevance-score`
   (`src/context.lisp`) siguen, y son inocuos. El que **no** era inocuo era
   `system-prompt` en `call-llm-with-tools/static`: una variable libre ausente de
   la lambda list, que en Common Lisp no es un valor por defecto sino una
   variable especial sin ligar — el turno moría con
   `Variable SYSTEM-PROMPT is unbound` al construir el primer mensaje. Arreglado
   en el mismo lote que el punto 7.

**Correcciones posteriores (2026-10-01)**

- **Escrituras atómicas.** `write-file` y `edit-file` escribían con
  `:if-exists :supersede` directo sobre el destino: una muerte a mitad del
  `write-string` dejaba el fichero del usuario **truncado a cero**. Ahora van por
  `write-file-atomically` (temporal hermano + `rename(2)`), que es lo que hace
  el write atómico real. Cubierto por 3 tests.
- **Orden de tools determinista.** `stream-execute-tool-calls` iteraba
  `tool-buf` con `maphash`, cuyo orden no está especificado por el estándar:
  un lote de N tool-calls se ejecutaba en un orden distinto en cada corrida, y
  un `read_file` seguido de un `write_file` podían invertirse. Ahora ordena por
  índice, que es el único orden que el modelo significado.
- **`promote-durable-facts` antes de `save-session`.** El orden inverso dejaba
  `dumps/longterm-mem.lisp` un turno por detrás de los hechos del turno que
  acababa de cerrar; con `run-one-shot` (un turno) se perdía la promoción
  entera. Ahora se promueve y luego se vuelca.
- **Guarda de tipo en `write-file`.** La validación de que `content` es string
  vivía solo en el normalizador batch, así que la ruta de tools nativas podía
  pasar un `hash-table` y `regex-replace-all` señalaba a mitad de la acción. La
  guarda vive ahora en la acción, igual que la del `old_string` vacío.
- **CI.** `.github/workflows/tests.yml` corre `./run-tests.sh` en cada push y PR.
  El suite es offline por contrato, así que **no necesita ninguna clave de API**.
  `run-tests.sh` instala Quicklisp en un temporal si no encuentra
  `~/quicklisp/setup.lisp`, de modo que es autocontenido.
- **El punto 25 (52 tests) está obsoleto**: son 108.

**P2 — functionality**

10. Cablear todos y epochs: herramienta o comando de REPL, y una política que cree
    epochs automáticamente (por ejemplo, cada N turnos o al superar un umbral de
    hechos).
11. Presupuesto de tokens por sesión, con corte explícito. **No se implementó
    ninguna tabla de precios**: el harness no estima costo, sólo tokens
    estimados y tokens reportados por el proveedor.
12. Borrar `max_raw_chars` o documentarlo como reservado.
