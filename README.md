# cl-harness — Harness de Gestión de Contexto para LLMs

**cl-harness** es un harness (armazón/andamiaje) de gestión de contexto para
asistentes de código basados en LLM, escrito en Common Lisp. Combina **dos
motores de producciones Rete** (la librería [Lisa](https://github.com/ldotlisa/Lisa))
con llamadas a proveedores de LLM (OpenRouter, Anthropic, OpenAI, Gemini, Groq,
Ollama) —y un **modo batch** que ejecuta S-expressions preplanificadas contra
las reglas del motor Rete— para decidir, *por turno*, qué hechos técnicos se
envían al modelo y cuáles se descartan — y para conservar, entre sesiones, un
**segundo motor de memoria a largo plazo**.

Su tesis central es:

> El contexto no se redacta, se *podría*. El costo y el ruido se controlan no
> resumiendo mejor, sino decidiendo estructuralmente qué hechos son lo
> suficientemente recientes, repetidos, causales o relevantes como para merecer
> el presupuesto de tokens.

La arquitectura tiene dos memorias simbólicas separadas:

- un **motor de turno** (`*turn-engine*`) que es la memoria de trabajo
  intra-turno (resultados de herramientas, conversación, loops, objetivos), y
- un **motor de memoria** (`*mem-engine*`) que es la memoria a largo plazo:
  conocimiento durable del proyecto que **sobrevive a la sesión y al proceso** y
  se inyecta en el contexto de cada turno.

**Palabras clave:** memoria de sesión, memoria a largo plazo, dos motores
Rete/Lisa, pruning de contexto, detección de loops, tool-use, ejecución en
background, modo batch (intenciones preplanificadas), persistencia, métricas de
reducción.

## ¿Qué es este proyecto?

*cl-harness* es un *REPL* interactivo y una librería ASDF. Como REPL, recibe
instrucciones del usuario, mantiene un estado estructurado (hechos en la memoria
de trabajo del motor de turno), construye un bloque de contexto en YAML —que
incluye la memoria a largo plazo—, lo envía a un LLM con herramientas
disponibles, ejecuta las herramientas que el modelo pida, y registra el
resultado todo en la memoria de hechos.

Ejemplo de sesión:

```
╔══════════════════════════════════════════╗
║  cl-harness — Context Management Harness  ║
║  Lisa/Rete + LLM · v0.1.0                 ║
╚══════════════════════════════════════════╝
Session: session-1893450568
LLM:     openrouter (deepseek/deepseek-v4-flash)
TTL:     1800 sec · Max facts: 50 · Per-type: 20
Type :help for commands, :quit to exit.

user> levantá el servicio y verificá que responda
assistant> [diagnostica, ejecuta tools — exec/read/edit — y verifica el endpoint]
```

## ¿Por qué existe? (Motivación)

Un harness de LLM "ingenuo" acumula todo lo que ocurre y lo pega verbatim en el
prompt. A medida que la sesión crece, esto presenta varios problemas:

1. **Costo y latencia**: el costo del prompt crece con el contexto; a partir de
   cierto tamaño, el retorno marginal es negativo.
2. **Ruido y deriva**: hechos viejos e irrelevantes compiten por atención con
   los actuales y degradan la precisión.
3. **Olvido real**: sin persistencia, un reinicio del proceso pierde todo el
   estado de la sesión; peor aún, pierde lo aprendido sobre el proyecto.
4. **Falta de causalidad**: un contexto plano de "toda la salida" no distingue
   qué tarea generó qué resultado.

La respuesta de *cl-harness* es gestionar el contexto como una *base de
conocimiento temporal y causal*, con dos niveles de memoria:

- **Hechos** atómicos tipados (comandos ejecutados, archivos leídos/escritos/
  editados, conversación, errores, loops, objetivos/todos, epochs).
- **Metadatos causales** por turno (*turn-id*, *parent-id*) que registran en qué
  punto de la conversación se produjo cada hecho.
- **Reglas Rete** que podan por expiración y deduplicación, y que **detectan
  loops** de herramientas.
- **Promoción a memoria larga**: al cerrar cada turno, los hechos *durables*
  (errores reales activos, escrituras) se proyectan al motor de memoria y se
  persisten en disco.
- **Selección por scoring** estructural+semántico dentro de un presupuesto.
- **Métricas** que comparan el contexto curado contra una línea base y contra el
  uso real reportado por el proveedor.

El resultado es una memoria de trabajo *gestionada*: caduca lo redundante,
protege lo que no se puede volver a derivar (conversación, errores activos),
señala al modelo cuando entra en un loop improductivo, y conserva entre sesiones
el conocimiento durable del proyecto.

## Arquitectura

```
                    ┌──────────────────────────────────────────────┐
                    │                  REPL (repl.lisp)            │
                    │  user>  ·  comandos :  ·  process-turn       │
                    └─────────────────┬────────────────────────────┘
                                      │
                    ┌─────────────────▼────────────────────────────┐
                    │           process-turn (por turno)           │
                    │  1. incf *turn-counter*                      │
                    │  2. assert hechos (user-input, tools, ...)   │
                    │  3. snapshot línea base "naive"              │
                    │  4. build-yaml-context → YAML (+ long_term)  │
                    │  5. call-llm (tool-use + loop detector /     │
                    │     batch: parser de S-expressions)          │
                    │  6. (batch) bucle (run)+re-llamada hasta     │
                    │     batch-complete                           │
                    │  7. assert respuesta + métricas              │
                    │  8. promote-durable-facts → *mem-engine*     │
                    └───────┬──────────────────────┬───────────────┘
                            │                      │
        ┌───────────────────▼────┐      ┌──────────▼───────────────┐
        │ Motor Rete de TURNO    │      │ Llamadas HTTP (llm.lisp) │
        │ (rules.lisp / engines) │      │ openrouter / openai      │
        │  prune-expired (TTL)   │      │ anthropic / gemini       │
        │  dedup-keep-newest     │      │ groq / ollama / batch    │
        │  prune-superseded-…    │      │  loop tool-use +         │
        │  detect-command-loop   │      │  detección de loops      │
        │  detect-read-triple    │      │  (tool-loop-warning)     │
        │  execute-intention-*   │      │  batch: clean-llm-text + │
        │  batch-emergency-brake │      │  parse-llm-batch-to-     │
        │  cancel-intentions-…   │      │  intentions (S-expr)     │
        │ context.lisp:          │      └──────────────────────────┘
        │  caps por tipo         │
        │  scoring + selección   │      ┌──────────────────────────┐
        │  render YAML           │      │ Background (background)  │
        └──────────┬─────────────┘      │ daemon genérico por      │
                   │  promote durables   │ timeout + logfile        │
        ┌──────────▼────────────────────┴──────────────────────────┐
        │ Motor Rete de MEMORIA (*mem-engine*)                     │
        │  mem-dedup-durable  ·  hechos durables (errores/writes)  │
        │  render long_term_memory:  en cada turno                 │
        └──────────┬───────────────────────────────────────────────┘
                   │
        ┌──────────▼─────────────────────────────────────────────┐
        │ Persistencia (persist.lisp)                            │
        │  dumps/<id>-facts.lisp · sessions/<id>.json            │
        │  dumps/longterm-mem.lisp · metrics/<id>-metrics.json   │
        │  dumps/<id>.core (ELF standalone) · bg/*.log           │
        └────────────────────────────────────────────────────────┘
```

### Dos motores LISA (el cerebro simbólico)

LISA ejecuta **un motor activo a la vez** (`lisa::*active-engine*`, ligado por
`lisa:with-inference-engine`). Cada `(lisa:make-inference-engine)` es un `rete`
completo: su propia tabla de hechos, red de reglas, contextos y foco. **Los
hechos de un motor son invisibles para el otro.** `engines.lisp` mantiene dos:

| Motor          | Rol                         | Contenido                                                    | Ciclo de vida |
|----------------|-----------------------------|-------------------------------------------------------------|---------------|
| `*turn-engine*`| memoria de trabajo          | conversación, tool results, loops, goals, epochs, TTL/dedup | se vacía al iniciar cada proceso/sesión |
| `*mem-engine*` | memoria a largo plazo       | errores reales activos, escrituras hechas al proyecto       | persiste en `dumps/longterm-mem.lisp` |

`install-dual-engines` crea ambos y deja `*turn-engine*` como **motor activo por
defecto**: todo el resto del código (`assert`/`retract`/`retrieve`/`run`) sigue
apuntando a la memoria de trabajo sin cambios. Las reglas que aplican a la
memoria larga se compilan explícitamente bajo `(with-mem-engine ...)`.

API: `with-turn-engine`, `with-mem-engine`, `reset-turn-engine`,
`reset-mem-engine`, `promote-durable-facts`, `mem-engine-facts`, `boot-memory`.

> **Nota técnica Lisa:** `lisa:clear` = `clear-system-environment` *reemplaza el
> engine y borra las reglas*. Por eso cl-harness nunca usa `clear`: usa
> `reset-*`/`lisa:reset`, que retractan los hechos conservando las reglas.

### Módulos

| Archivo             | Responsabilidad                                                            |
|---------------------|----------------------------------------------------------------------------|
| `packages.lisp`     | Definición del paquete `:cl-harness`                                        |
| `config.lisp`       | Carga de `config.json` + valores por defecto + accesores                    |
| `engines.lisp`      | **Los dos motores LISA**, promoción turn→mem, `boot-memory`, resets         |
| `facts.lisp`        | Templates Lisa (`harness-fact`, `context-slot`, `agent-todo`, `context-epoch`), contadores |
| `rules.lisp`        | Reglas Rete (TTL, dedup, epoch, loops, **intenciones batch**, frenos de emergencia) + helpers imperativos |
| `context.lisp`      | Render YAML, bloque `long_term_memory`, scoring y selección por presupuesto |
| `metrics.lisp`      | Métricas por turno: contexto curado vs naive vs uso real; stats por tool    |
| `actions.lisp`      | Primitivas POSIX: `exec-command`, `read-file`, `write-file`, `edit-file` → hechos |
| `background.lisp`   | Daemonización genérica de comandos que exceden el timeout (Bordeaux Threads) |
| `llm.lisp`          | Clientes de proveedores + loop de tool-use + streaming + loop detector + **parser batch** |
| `persist.lisp`      | Dump/restore (sesión + memoria larga + JSON/metrics) e imagen ejecutable    |
| `repl.lisp`         | Loop REPL, comandos `:`, `process-turn`, `start-harness`                    |
| `main.lisp`         | Punto de entrada `standalone-toplevel`, handlers de señal, prompt por env   |

Dependencias ASDF: `lisa`, `dexador`, `com.inuoe.jzon`, `bordeaux-threads`,
`cl-ppcre`, `uiop` (véase `cl-harness.asd`). `actions.lisp` y `llm.lisp` usan
`babel` (dependencia transitiva de `dexador`; conviene declararla explícitamente).

## Cómo funciona

### 1. Ingestión de hechos

Todo lo que ocurre se registra como un hecho. El template base es:

```lisp
(deftemplate harness-fact ()
  (slot fact-type)
  (slot timestamp)
  (slot data))
```

Donde `data` es un plist con el payload y los metadatos causales *dentro* del
plist (no como slots del template), de modo que persistencia, dedup y render
YAML no necesitan cambios si se agrega metadato:

```lisp
(assert (harness-fact (fact-type "command-exec")
                      (timestamp (get-universal-time))
                      (data (list :command cmd :output out :exit-code 0
                                  :turn-id n :parent-id n))))
```

**Tipos de hecho**

| Tipo             | Semántica                                  | ¿Caduca? (TTL)      | ¿Durable? |
|------------------|--------------------------------------------|---------------------|-----------|
| `user-input`     | Mensaje del usuario                        | Nunca               | No        |
| `llm-response`   | Respuesta del LLM                          | Nunca               | No        |
| `command-exec`   | Resultado de un comando (exit+output)      | Sí, salvo error real | Sí, si error real |
| `file-read`      | Contenido de un archivo leído              | Sí                  | No        |
| `file-write`     | Archivo creado/escrito (bytes)             | Sí                  | **Sí**    |
| `file-edit`      | Edición quirúrgica aplicada (o fallida)    | No (no es evidential) | No        |
| `tool-loop`      | Loop de herramientas detectado por regla   | Sí                  | No        |
| `intention`      | Acción batch pendiente (read/write/edit/exec) | No (ciclo pending→retract) | No |
| `batch-abort`    | Aborto del batch (freno de emergencia)     | No                  | No        |
| `batch-complete` | Marca de fin de batch (el LLM no pidió más acciones) | No        | No        |
| `agent-todo`     | Objetivo/todo (backward-chaining)          | No (ciclo pending→completed) | No |
| `context-epoch`  | Checkpoint de consolidación                | 1 activo (se reemplaza) | No    |

Los hechos evocables (evidentials: `command-exec`, `file-read`, `file-write`,
`tool-loop`) son *re-derivables*: si caducan, el agente puede volver a
ejecutarlos. La conversación no es re-derivable y por eso nunca caduca por TTL.
Los `agent-todo`/`context-epoch` existen como plantillas y se renderizan en el
YAML, pero el harness no los crea automáticamente (uso experimental). Los
`intention` son insumos del *modo batch* (§7) y se retractan tras ejecutarse o
si un freno de emergencia los cancela.

### 2. Reglas Rete — podado

Las reglas de podado se compilan en el **motor de turno**. Dos llevan *salience*
negativo para correr después del resto:

- **`prune-expired`** (salience -50): retracta hechos evidenciales que cumplen
  `fact-expired-p`:
  - comandos con *error real* (exit ≠ 0 **con** evidencia en el output, o fallo
    de lanzamiento) se conservan como contexto de error activo — si
    `conserve_errors` está activo (default true); un exit ≠ 0 sin output
    (p. ej. `cat missing 2>/dev/null`) o un comando *timed-out* no se conserva;
  - decaimiento lógico por turnos: caducan si pasaron más de `fact_ttl_turns`
    turnos desde su *turn-id*;
  - decaimiento por reloj si `fact_ttl_seconds > 0`.
- **`dedup-keep-newest`** (salience 0): si dos hechos del mismo tipo tienen la
  misma clave estructural (comando, ruta) *y* el mismo output/exit (o mismo
  contenido en file-read), el más viejo es redundante y se retracta. Si el
  output cambió, ambos se conservan (refleja mutación de estado). Los
  `tool-loop` deduplican por *familia*.
- **`prune-superseded-by-epoch`** (salience -40): si hay un *epoch* activo con
  *baseline-seq*, retracta los evidenciales previos al baseline (experimental).

Además, `build-yaml-context` aplica *caps imperativos por tipo*
(`retract-oldest-of-type`) para conversación y evidenciales, protegiendo
siempre a los comandos fallidos.

### 3. Reglas Rete — detección de loops de herramientas

Un modelo débil se traba re-issueando el MISMO comando fallido con variantes de
quoting, o releyendo el mismo archivo una y otra vez, quemando las iteraciones
de tool-use. Se detecta de dos formas que comparten predicados y convergen en un
hecho derivado `tool-loop`:

- **`detect-command-loop`** (salience 10): tres hechos `command-exec` **del
  mismo turno**, estrictamente crecientes en tiempo, **fallidos** (no timeout) y
  cuyos comandos normalizados comparten un prefijo común, asertan un hecho
  `tool-loop` con la *familia* (prefijo), los comandos y `:count 3`.
- **`detect-read-triple`** (salience 10): tres hechos `file-read` **del mismo
  turno** sobre el mismo *basename* asertan un `tool-loop` con `:kind
  "read-loop"`.

`tool-loop-warning` (en `rules.lisp`, llamada desde `llm.lisp`) hace `(run)`
tras cada lote de herramientas —para que las reglas evalúen la evidencia
fresca— y, si existe un `tool-loop` del turno actual (o si el *fallback*
imperativo detecta un patrón mixto), **aborta el turno** devolviendo un texto
claro que le dice al modelo que pare, lea el error real y cambie de enfoque. La
alerta se emite **una vez por turno** (`*loop-alerted-turn*`).

Parámetros calibrados en campo:

| Parámetro                  | Default | Significado |
|----------------------------|---------|-------------|
| `*loop-min-count*`         | 3       | Comandos fallidos de la misma familia en un turno. |
| `*read-loop-min-count*`    | 4       | Toques del mismo archivo (file-read o comando que lo menciona). Más alto que el de comandos **a propósito**: levantar/verificar un servicio re-importa el mismo módulo varias veces de forma legítima. |
| `*loop-prefix-min-chars*`  | 20      | Prefijo mínimo compartido para considerar dos comandos "el mismo intento". |
| `*loop-prefix-ratio*`      | 0.6     | Prefijo mínimo como fracción del comando más corto. |

El *fallback* imperativo de `tool-loop-warning` (cuando las reglas no matchean
un patrón mixto) corre `detect-read-loop` (file-read o comandos que mencionan
el mismo *basename*) y `detect-tool-loop` (misma familia de comandos fallidos),
usando `mentioned-files-in-command` para extraer rutas de un comando — este
último **ignora tokens puramente numéricos** (IPs como `0.0.0.0`, versiones
como `2.4.1`), que no son archivos. (Nota: existe `content-probe-command-p`
que distingue un *probe de contenido* de una ejecución/compilación del mismo
archivo, pero hoy no está conectado a la detección; no cuenta como re-lectura.)

### 4. Memoria a largo plazo (promoción y persistencia)

Al final de cada turno, `process-turn` llama a `promote-durable-facts`:

1. Recorre los hechos del motor de turno y, para cada uno **durable**
   (`durable-type-p`), lo re-aserta en el motor de memoria.
2. Corre `(run)` en el motor de memoria (aplica `mem-dedup-durable`, que deja
   única la versión más nueva de cada clave).
3. Re-aplica caps por tipo (`retract-oldest-of-type` sobre `command-exec` y
   `file-write`) para que la memoria quede acotada.

`durable-type-p` = **command-exec con error real** (`real-error-p`: exit ≠ 0,
con evidencia, no timeout) **o** `file-write` (una acción realmente tomada sobre
el proyecto). La conversación **no** es durable.

`boot-memory` (al arrancar un proceso/sesión nueva) resetea ambos motores, carga
`dumps/longterm-mem.lisp` en el motor de memoria y corre sus reglas. El bloque
`long_term_memory:` se agrega al YAML de **cada** turno (ver §5).

### 5. Selección por scoring y render YAML

Tras el podado, `select-relevant-facts` puntúa cada hecho restante dentro de un
presupuesto (`max_context_chars`) y un tope (`max_context_facts`). El scoring
tiene seis términos:

1. *Proximidad al turno actual*: decaimiento exponencial según |turn-id − now|.
2. *Garantía de conversación*: `user-input` y `llm-response` reciben bonificación
   fija (300).
3. *Priorización de errores*: un comando fallido suma 400.
4. *Repetición*: una clave de dedup vista varias veces suma hasta 200.
5. *Coincidencia semántica*: contra palabras clave extraídas del prompt del
   usuario (stopwords en ES/EN), 450/400 para ruta/comando.
6. *Dependencia causal*: un *parent-id* presente suma 50.

La idea central del diseño sigue vigente: **Rete decide estructura** (tiempo,
repetición, dependencia) y **deja el significado al LLM**. La selección *nunca
degrada* el payload de un hecho que se selecciona: se emite íntegro, salvo un
recorte de cabeza/cola con aviso explícito para payloads gigantes
(`truncate-payload`, `max_fact_payload_chars`).

`grouped-context-string` renderiza los hechos agrupados por *turn-id*, y
`build-yaml-context` **agrega al final** el bloque de memoria larga:

```yaml
context:
  session: session-1893450568
  turns:
    3:
      exec: ls -la -> exit 0
        output: total 40 ...
      user: "¿qué SO corre?"
    4:
      assistant: "El sistema es Linux 6.18.36 ..."
long_term_memory:
  - error: pytest -q -> exit 1
    evidence: "FAILED tests/test_synth.py::test_accent_changes_sustain ..."
  - action: wrote /proyecto/synth/engine.py (8421 bytes)
```

Se aplica escaping YAML (`escape-yaml`) para valores con caracteres especiales.

### 6. Loop de tool-use

Los proveedores OpenAI-compatibles usan streaming + tool-use, con hasta
`llm_max_tool_iterations` iteraciones (default 12, configurable): si el modelo
responde con `tool_calls`, el harness ejecuta la herramienta, adjunta el
resultado como mensaje de rol `tool` y re-llama. Las herramientas expuestas son:

- `exec_command` (comando shell): delega en `exec-command` → registra hecho.
- `read_file` (leer archivo): delega en `read-file` → registra hecho.
- `write_file` (crear archivo): delega en `write-file`. **Guard:** si el destino
  ya existe con contenido significativo (> 2000 bytes), se **rechaza** y se
  indica usar `edit_file` (evita que modelos débiles re-emitan archivos grandes
  y los trunquen).
- `edit_file` (edición quirúrgica): delega en `edit-file`; reemplaza la primera
  ocurrencia exacta de `old_string` por `new_string`. Es la vía principal para
  modificar archivos existentes (cuesta pocos tokens y no reescribe todo).

De este modo *toda* salida observable que el modelo ve pasa también por la
memoria de hechos y alimenta el pruning de turnos futuros.

### 7. Modo batch (intenciones en Rete)

Con `llm_provider: "batch"` no hay tool-use HTTP: `call-llm` pide al modelo que
planifique de antemano y entregue **todas** sus acciones en una sola respuesta
como S-expressions Lisp puras. El flujo:

1. `call-llm` apenda el bloque `<batch_execution_mode>` al system prompt, llama
   al proveedor OpenAI-compat con herramientas **desactivadas** y
   `parse-llm-batch-to-intentions` escanea la respuesta:
   - `clean-llm-text` normaliza el texto (quita code fences, tags XML sueltos,
     arregla `read-file`/`exec-command` sin paréntesis);
   - cada S-expression válida (`(read-file "…")`, `(exec-command "…")`,
     `(write-file "…" "…")`, `(edit-file "…" "…" "…")`) se aserta como un hecho
     `intention` con `:action`, args y `:status :pending`;
   - si no se parsea ninguna S-expression, se aserta `batch-complete` (la
     respuesta era el texto final del modelo).
2. `process-turn`, en modo batch, entra en un bucle Rete-controlado: dispara
   `(run)`, reconstruye el contexto y re-llama al LLM hasta que existe
   `batch-complete` (o se agota un safety-net de 100 rondas). Cada `(run)` hace
   que las reglas `execute-intention-read/write/edit/command` llamen a las
   funciones POSIX originales (`read-file`, `write-file`, `edit-file`,
   `exec-command`) y retracten la intención (las funciones re-asertan los
   hechos `file-read`/`file-write`/`command-exec`/… de siempre, que alimentan
   pruning y loop-detection sin cambios).
3. **Frenos de emergencia** (salience 20, corren antes que las de ejecución):
   - `batch-emergency-brake`: si en el turno actual existe un `command-exec` con
     *error real*, retracta todas las `intention` `:pending` — no se sigue
     escribiendo sobre un proyecto fallido;
   - `cancel-intentions-on-abort`: si existe un hecho `batch-abort`, retracta
     todas las intenciones restantes.

`detect-blind-writes` (llamada desde `process-turn` en cada ronda del bucle
batch) detecta una intención `write-file` **o `edit-file`** sin lectura previa
del archivo en el turno y aserta `batch-abort`; la regla
`cancel-intentions-on-abort` (salience 20) la consume retractando todas las
intenciones restantes. Es la protección activa contra reescribir a ciegas
archivos que el modelo nunca leyó.

### 8. Streaming en vivo (opencode-style)

Con `llm_stream: true` (por defecto) la respuesta no llega entera:
`stream-llm-with-tools` consume los *deltas* SSE a medida que el proveedor los
emite y los imprime en vivo, con

- el **razonamiento** del modelo (p. ej. `delta.reasoning` en DeepSeek) —visible
  progresivamente, pero no se acumula en el contexto—,
- el **texto final** (`delta.content`),
- **banners de herramienta** antes/durante la ejecución:

```
assistant>
The user is asking how many lines are in src/context.lisp. ...
 ⚙ exec_command {"command": "wc -l src/context.lisp"}
  ✓ 74 chars received

El archivo `src/context.lisp` tiene **369 líneas**.
```

El canal de salida de deltas es `(llm-stream-out)`, que usa `*stream-output*` si
está definido o el `*standard-output*` actual (evita streams obsoletos al
arrancar desde una imagen SBCL guardada). El resultado de cada herramienta
inyectado de vuelta pasa por `truncate-tool-result`: con `max_tool_result_chars`
(default 8000) los outputs gigantes se recortan a cabeza+cola marcando el corte
con `[TRUNCATED]: original X chars…`. El output *completo* sigue viviendo en la
memoria de hechos; solo la copia que re-llama al modelo se acota. `assistant>`
se imprime una sola vez por turno (guard `*llm-streamed*`).

### 9. Ejecución en background (daemons)

Un comando que excede `tool_timeout_seconds` (p. ej. levantar un servidor web) no
debe bloquear el turno. `exec-command` detecta el timeout y, si
`background_on_timeout` está activo (default), lo **relanza detached** vía
`start-background`: un hilo (Bordeaux Threads) lanza el proceso con
`uiop:launch-program`, lo registra en un registro global con su PID/estado/log y
sigue vivo **fuera del harness**. El modelo recibe un mensaje que le exige
continuar el flujo: esperar, leer el log, verificar el proceso y recién entonces
terminar. La detección es **genérica**: ninguna heurística por nombre de servidor
o stack — cualquier comando que siga vivo tras el timeout recibe este trato.

API: `start-background`, `background-status`, `stop-background`,
`shutdown-background-processes`. Los logfiles se guardan en `bg/bg-N.log` bajo
el directorio base.

### 10. Proveedores

`call-llm` despacha por `llm_provider`:

| Proveedor     | Función                          | Tool-use |
|---------------|----------------------------------|----------|
| `anthropic`   | `call-anthropic`                 | No (texto) |
| `ollama`      | `call-ollama` (local)            | No       |
| `gemini`      | `call-gemini` (generateContent)  | Texto (backend) |
| openai-compat | `call-llm-with-tools` (streaming) | Sí (`llm_max_tool_iterations`) |
| `openrouter`  | openai-compat                    | Sí       |
| `groq`        | openai-compat                    | Sí       |
| `batch`       | openai-compat sin tools + parser de S-expressions → intenciones en Rete (§7) | No (batch preplanificado) |

Configuración vía `config.json` o variables de entorno (`LLM_PROVIDER`,
`LLM_MODEL`, `LLM_ENDPOINT` vía `config.lisp`; la API key por env es
`ANTHROPIC_API_KEY` / `OPENAI_API_KEY` / `GOOGLE_API_KEY`, según el proveedor).
El proveedor por defecto es
`anthropic` con `claude-sonnet-4-20250514`; el proyecto se validó con OpenRouter
+ DeepSeek.

### 11. Métricas (medir antes de optimizar)

Cada turno registra (`record-context-metrics`):

- *Contexto curado*: caracteres/tokens de lo que realmente se envió (heurística
  ~4 caracteres/token).
- *Línea base naive*: snapshot de `naive-context-string` *antes* de que corran
  las reglas de podado.
- *Uso real*: `prompt_tokens`/`completion_tokens` **acumulados** a lo largo de
  todas las iteraciones del tool-loop (cada request reenvía la pila de
  mensajes creciente), reportados por el proveedor — la única fuente de verdad.
- *Per-tool*: `record-tool-call` guarda por ejecución el nombre, chars crudos y
  chars re-inyectados (post-truncación); el resumen agrega `chars_raw`,
  `chars_sent` y % ahorrado por herramienta.

`metrics-summary` imprime la tabla por turno, el total con *reduction_pct* y la
tabla por herramienta; `metrics-to-json` vuelca todo a
`metrics/<id>-metrics.json`.

> **Caveat honesto:** `reduction_pct` compara el contexto curado contra el
> snapshot naive **previo al podado**, pero el contexto final además incluye el
> bloque `long_term_memory:` (que el naive no tiene). Por eso el porcentaje
> puede ser **negativo** y no debe leerse como "ahorro real". La métrica de
> ahorro real sigue siendo el uso reportado por el proveedor. El costo real lo
> domina el **tool-loop** (iteraciones × pila), no el estimador.

### 12. Persistencia

- **`save-session`** escribe:
  - `dumps/<id>-facts.lisp`: formas Lisp que re-ejecutan los `assert` con
    `timestamp (get-universal-time)` (los timestamps se *refrescan* al salvar);
    también persiste los contadores monótonos;
  - `dumps/longterm-mem.lisp`: **memoria larga** (global, no por sesión);
  - `sessions/<id>.json`: estado interoperable;
  - `metrics/<id>-metrics.json`: métricas.
- **`create-session-dump`** (`:dump`) genera un *ejecutable ELF standalone* con
  `sb-ext:save-lisp-and-die :executable t`: la sesión vive *dentro* de la imagen
  (memoria de trabajo de Lisa + hechos), con `refresh-fact-timestamps` antes de
  salvar y un restart `abort` en el toplevel.
- **`restore-session`** (`:restore ID`) carga un `dumps/<id>-facts.lisp` y
  también la memoria larga.

## Configuración

El archivo `config.json` en el directorio base (secreto de API **no** incluido
en el repo; debe inyectarse por archivo o variable de entorno):

```json
{
  "llm_provider": "openrouter",
  "llm_api_key": "<secreto — nunca documentar>",
  "llm_model": "deepseek/deepseek-v4-flash",
  "llm_endpoint": "https://openrouter.ai/api/v1/chat/completions",
  "llm_max_tool_iterations": 30,
  "max_context_facts": 80,
  "fact_ttl_seconds": 0,
  "fact_ttl_turns": 30,
  "max_facts_per_type": 20,
  "max_context_chars": 30000,
  "tool_timeout_seconds": 30,
  "background_on_timeout": true,
  "conserve_errors": true,
  "max_tool_result_chars": 8000,
  "llm_stream": true
}
```

Las claves de entorno que respeta el código son `LLM_PROVIDER`, `LLM_MODEL`,
`LLM_ENDPOINT` (vía `config.lisp`) y `ANTHROPIC_API_KEY` / `OPENAI_API_KEY` /
`GOOGLE_API_KEY` como fallback de `llm_api_key` (no existe una `LLM_API_KEY`
propia): el valor del `config.json` tiene prioridad sobre la variable.

### Claves y valores por defecto

| Clave                    | Default  | Significado                                          |
|--------------------------|----------|------------------------------------------------------|
| `llm_provider`           | `anthropic` | Proveedor de LLM (openrouter/groq/gemini/ollama/batch…)  |
| `llm_api_key`            | env       | Key del proveedor                                    |
| `llm_model`              | `claude-sonnet-4-20250514` | Modelo                        |
| `llm_endpoint`           | autodetect | Endpoint override                                   |
| `llm_max_tokens`         | 4000      | Tope de respuesta                                    |
| `llm_max_tool_iterations`| 12        | Máx. iteraciones de tool-use por turno               |
| `llm_stream`             | true      | Streaming en vivo de deltas + banners de tools (§8)  |
| `max_context_facts`      | 50        | Tope de hechos seleccionables por turno              |
| `fact_ttl_seconds`       | 1800      | TTL de reloj para evidenciales (0 = off)             |
| `fact_ttl_turns`         | 10        | TTL lógico por turnos                                |
| `max_facts_per_type`     | 20        | Cap por tipo de hecho                                |
| `max_fact_payload_chars` | 2500      | Recorte de payloads gigantes                         |
| `max_context_chars`      | 12000     | Presupuesto del bloque de contexto                   |
| `tool_timeout_seconds`   | 30        | Timeout de `exec_command` (0/false = off)            |
| `background_on_timeout`  | true      | Relanzar detached lo que excede el timeout           |
| `conserve_errors`        | true      | Errores reales se conservan (TTL-exentos + prioridad) |
| `max_tool_result_chars`  | 8000      | Recorte del tool-result inyectado (0/false = off)    |
| `sessions_dir`/`dumps_dir`/`metrics_dir` | subdirs del base dir | Dónde persistir |

## Comandos del REPL

| Comando                | Acción                                             |
|------------------------|---------------------------------------------------|
| `:help`                | Ayuda                                             |
| `:quit`                | Salva sesión y sale                               |
| `:debug`               | Alterna trazas de depuración                      |
| `:facts`               | Lista hechos activos del motor de turno           |
| `:rules`               | Lista reglas cargadas                             |
| `:save`                | Dump Lisp + memoria larga + JSON + metrics        |
| `:restore ID`          | Restaura sesión (y memoria larga)                 |
| `:exec CMD`            | Ejecuta comando shell                             |
| `:read PATH`           | Lee archivo                                       |
| `:write PATH`          | Escribe archivo (pide contenido)                  |
| `:dump`                | Crea imagen ejecutable con el estado actual       |
| `:clear`               | Vacía la **memoria de turno** (conserva reglas y memoria larga) |
| `:clearall`            | Reset completo: ambos motores + memoria en disco + contadores + métricas + session id nuevo |
| `:context`             | Muestra el YAML curado (+ `long_term_memory`) que se enviaría hoy |
| `:metrics`             | Muestra tabla de reducción por turno              |
| `:model NAME` / `:provider P` | Cambian modelo/proveedor en caliente       |

## Detalles técnicos clave (y trampas del DSL Lisa)

Durante la validación se corrigieron bugs reales, hoy documentados como
conocimiento necesario del código:

1. **Salience no es `declare`**: en Lisa la saliencia va en el *lambda-list* de
   la regla: `(defrule name (:salience -50) ...)`. La forma
   `(declare (salience -40))` es inválida y las reglas jamás disparan.
2. **Symbol desnudo = literal**: en un patrón de `assert`, `(id id)` guarda el
   símbolo literal `ID`, no el valor de la variable. Para computar un valor hace
   falta una expresión no trivial, p. ej. `(id (identity id))`.
3. **Restore y TTL**: al restaurar sesiones viejas, el primer `run` purgaba los
   evidenciales (timestamps antiguos ante un TTL por reloj activo). Se resuelve
   refrescando timestamps en el dump.
4. **Literal-symbol en retrieve**: patrones como `(id id)` en `retrieve` no
   matchean nunca; se resuelve filtrando en Lisp puro (`find-todo-by-id`).
5. **Contadores monótonicos entre rebotes**: el dump persiste
   `*turn-counter*`/`*todo-counter*`/`*epoch-counter*`; al reanudar la imagen o
   tras `:restore` se continúan, de modo que los `turn-id` no colisionan.
6. **`typep` no sirve sobre hechos**: las instancias de `harness-fact` son de la
   clase LISA `FACT` (`(typep f 'harness-fact)` = NIL). Usar `fact-type-of` /
   `fact-slot` / `fact-data-of`, y `retrieve` + `(mapcar #'first …)`.
7. **Paréntesis que "traga" un `defrule`**: un `defun` mal cerrado puede
   consumir el bloque `(with-mem-engine (defrule …))` siguiente, dejando la
   regla sin registrar (y el `defun` devolviendo el objeto regla). Verificar con
   `:rules` / `get-rule-list` que la regla existe.

Otros hechos operativos verificados:

- `exec-command` usa `uiop:run-program` con captura del exit-code real y stderr
  mergeado al output; marca `:timed-out` (124/137) y `:launch-failed` (exit -1).
- `read-file-contents` lee por caracteres (no dimensiona el buffer por
  `file-length` en bytes), evitando relleno `#\Null` en UTF-8 multibyte; hay
  fallback *lossy* a bytes para archivos no-UTF-8.
- `strip-cd-prefix` elimina un `cd ABSOLUTO &&` inicial redundante (el harness ya
  corre dentro del directorio base).
- El **prompt de una corrida** en el binario standalone (y solo ahí) se pasa por
  la variable de entorno `CL_HARNESS_PROMPT` (no por argv), para que un
  `pkill -f` que emita el propio modelo no matchee al proceso del harness.
  (`run.sh run` en cambio lo toma por argv y lo re-exporta como
  `CL_HARNESS_RUN`.)
- El harness instala handlers de **SIGTERM/SIGINT** que vuelcan métricas y
  sesión antes de salir (exit 130), para no perder el audit trail si algo lo mata.
- `:compression`/`:purify` no aplican en esta build de SBCL (2.4.1).

## Seguridad (por diseño)

La seguridad de cl-harness **no es responsabilidad del harness**: se delega por
completo al sistema operativo. Cada agente corre bajo **un usuario Linux
dedicado** (con su grupo, su home y sus buzones), provisionado externamente
mediante Ansible. El OS define el perímetro: qué usuario puede tocar qué
archivos, qué procesos ejecuta y en qué entornos trabaja.

Por eso cl-harness **no implementa seguridad "soft" propia**: ni pedidos de
aprobación por herramienta, ni políticas de proveedor, ni sandbox interno. El
harness confía en que el usuario del SO bajo el que corre *ya es* el control de
acceso.

## Qué NO es / qué NO hace

cl-harness es deliberadamente reducido. Resuelve la gestión de contexto y
memoria de una sesión de LLM (hechos, podado, loops, memoria larga, persistencia)
y delega el resto al sistema operativo y a la infraestructura.

| Capacidad (opencode)               | En cl-harness | Por qué |
|------------------------------------|---------------|---------|
| Multi-agente / subagentes (`task`) | NO | Un solo harness por proceso y por usuario Linux. |
| Plan mode / build mode             | NO | Cada turno va directo: contexto → LLM → herramientas. |
| Permisos por herramienta           | NO | La frontera de confianza es el hard user del SO. |
| Políticas de proveedor             | NO | Un proveedor por config, conmutable en caliente. |
| Herramientas amplias (MCP, GitHub, LSP, web) | NO | Solo `exec_command`, `read_file`, `write_file`, `edit_file`. |
| Undo/redo + git diffs              | NO | No gestiona el working tree; solo ejecuta lo que el LLM elige. |
| Compartir conversaciones           | NO | Sin sharing ni enlaces públicos. |
| Workspace multi-proyecto           | NO | Una sesión activa por proceso; las viejas se retoman con restore/reboot. |
| Event sourcing / transcripts chat  | NO | La memoria es hechos en Rete, no un log de eventos formateado. |
| Caching de respuestas              | NO | El contexto se reconstruye por turno (el pruning Rete es la optimización). |
| Cost-tracking por turno            | NO | Solo métricas de contexto + el uso real que reporta el proveedor. |
| SDK / servidor / plugins           | NO | Librería + REPL + binario standalone. |

## Casos de uso

**Depuración e inspección con memoria de proyecto**
Investigación de archivos, ejecución de comandos encadenados, retención del error
activo, y retoma de la tarea entre reinicios —con los errores reales y las
escrituras del proyecto conservados en memoria larga.

**Levantar y verificar servicios**
El modelo diagnostica, arranca un servidor (daemonizado automáticamente al
exceder el timeout), lee su log, y verifica el endpoint antes de dar por cerrada
la tarea.

**Automatización de tareas largas**
Sesiones de múltiples turnos donde el LLM debe recordar que "ya leyó X", "detectó
N líneas", "quedó pendiente limpiar-tmp", sin re-descubrir hechos.

**Validación de pruning y presupuesto de contexto**
Banco de pruebas para comparar contexto curado vs naive y medir el uso real
reportado por el proveedor.

**Demostración de Rete/Lisa aplicado a sistemas LLM**
Ejemplo funcional de forward-chaining con dos motores (trabajo + memoria larga),
TTL, dedup, detección declarativa de loops, integrado con tool-use real.

**Imagen standalone portable**
Binario ELF que retoma una sesión congelada sin instalación de SBCL ni
dependencias.

## Limitaciones conocidas

- Los proveedores no-OpenAI y Anthropic devuelven texto plano sin tool-use.
- `reduction_pct` puede ser negativo y **no** representa el ahorro real (ver
  caveat en §11); el uso del proveedor es la verdad de tierra.
- La selección por presupuesto usa heurística de caracteres/tokens (~4 chars).
- `agent-todo`/`context-epoch` existen pero no se crean automáticamente
  (experimental).
- No implementa caching ni cost-tracking por turno (decisión de diseño).

## Validación realizada

- *Offline (0 llamadas API)*: suite de verificación de los dos motores (23/23
  PASS): aislamiento entre motores, reglas por engine (incl. `mem-dedup-durable`
  en el motor de memoria), detección de loops, sin falsos positivos, promoción
  durable (se conservan errores/escrituras, no la conversación), bloque
  `long_term_memory` en el contexto, dump/restore/boot-memory.
- *En vivo (OpenRouter/DeepSeek)*:
  - arreglar un test que fallaba → `pytest` 32/32 PASS;
  - **levantar un servicio** uvicorn y **verificarlo** por HTTP (`/api/state`
    devolvió el JSON esperado);
  - **arreglar un `TemplateNotFound`** de Jinja y relanzar → `GET /` = 200;
  - memoria larga usada entre corridas (el modelo citó traces de fallos previos)
    y persistida en `dumps/longterm-mem.lisp` (errores reales + escrituras).

## Cómo ejecutar

Arranque directo con SBCL + Quicklisp (el proyecto se encuentra vía
`~/quicklisp/local-projects/`):

- **Modo TUI interactivo**:

  ```shell
  ./run.sh
  ```

  En un TTY, `run.sh` usa `rlwrap` para edición de línea e historial.

- **Modo no interactivo, un turno** (como `opencode run "..."`): procesa el
  mensaje, ejecuta herramientas, imprime la respuesta (streameada), salva la
  sesión y sale. Exit 0 = OK, 1 = fallo de LLM/loop:

  ```shell
  ./run.sh run "verifica que el servicio responda"
  ```

  `run.sh run` toma el mensaje **por argv** (lo exporta a `CL_HARNESS_RUN`
  internamente y falla con exit 2 si está vacío). `CL_HARNESS_PROMPT` solo lo
  lee el binario standalone (`bin/cl-harness`), no `run.sh`:

  ```shell
  ./run.sh run "verifica que el servicio responda"
  ```

- **Config/dir base alternativos** vía entorno:

  ```shell
  CL_HARNESS_CONFIG=/ruta/config.json CL_HARNESS_DIR=/ruta/proyecto ./run.sh run "mensaje"
  ```

`run.sh` equivale a:

```shell
sbcl --noinform --non-interactive \
     --eval '(ql:quickload :cl-harness)' \
     --eval '(cl-harness:start-harness)'        # TUI
     --eval '(cl-harness:run-one-shot "...")'   # one-shot
```

La imagen ejecutable (`./build.sh` → `bin/cl-harness`, o `dumps/<id>.core`)
acepta los mismos dos modos sobre la sesión que lleva en memoria; su one-shot
sí se alimenta por entorno (para que el prompt no quede en el argv y un
`pkill -f` del modelo no mate al proceso del harness):

```shell
bin/cl-harness                                   # TUI
CL_HARNESS_PROMPT="verifica bla" bin/cl-harness  # one-shot (recomendado)
bin/cl-harness "verifica bla"                    # one-shot (argv, fallback)
```

## Referencias

- Lisa / Rete: `quicklisp/local-projects/Lisa` (motor de producciones).
- SBCL: `sb-ext:save-lisp-and-die`, `:executable t`, restart `abort`,
  `sb-sys:enable-interrupt`.
- Inspiración: *session_context_epoch* y *goals/todos* de OpenCode; el patrón
  *Means-Ends Analysis* de `examples/mab.lisp` de Lisa.
- Ideario: "Phase 0 — measure before optimizing" (`metrics.lisp`).

## Licencia

MIT (véase `cl-harness.asd`).
