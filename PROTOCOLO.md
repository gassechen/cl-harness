# PROTOCOLO.md — protocolo interno del harness

> Estado: **borrador**. Describe el contrato de comunicación entre las tres capas.
> No es una especificación externa: no hay versión, ni compatibilidad, ni terceros.
> Su único validador es la suite de tests.

## 1. Tesis

El harness no es un agente con herramientas. Es una **colaboración neurosimbólica**
entre tres partes con dominios disjuntos, y el fallo de diseño más común es pedirle
a una de ellas que acts fuera del suyo.

| Capa | Instancia | Autoridad | Prohibición |
|---|---|---|---|
| **Estadística** | LLM | Semántica, pane estratégico, elegir la siguiente transición | Declarar un resultado. No es fuente de verdad |
| **Simbólica** | Lisa + Rete + rules | Goals (bananas), recuperación, contabilización | Generar significado. No inventa intención |
| **Determinista** | POSIX, FS, exit codes | La verdad: bytes y códigos de salida | Entender intención |

La regla que atraviesa las tres:

> **La información sube y baja, pero cada capa solo escribe en su propio registro.**
> Intención baja: LLM → simbólico → SO.
> Verdad sube: SO → simbólico → LLM.
> Nadie salta el medio.

## 2. Los cinco mensajes

Cerrados, direccionales y asimétricos. Ninguno es una petición: el `VERDICT` es un
estado, el `PLAN` es una propuesta. El LLM nunca queda bloqueado esperando.

| Mensaje | Dirección | Emisor | Representación actual | Note |
|---|---|---|---|---|
| `PLAN` | LLM → harness | LLM | JSON ToolUse, `tool_calls[]` | Ordenado, 1..N |
| `NOTE` | LLM → harness | LLM | — **no existe** | Intención/salto del modelo |
| `VERDICT` | harness → LLM | harness | — **no existe** | Único emisor de applied/failed |
| `GOAL` | harness → LLM | harness | hecho `agent-todo` | Estado derivado de `VERDICT` |
| `DONE` | LLM → harness | LLM | `"tool_calls":[]` | **Propuesta**, no hecho |

### 2.1 `PLAN`

El LLM propone N pasos ordenados. Formato actual: JSON ToolUse, validado por
`normalize-batch-tool-calls` (`src/llm.lisp:1129`). **No cambiar** — ya está en la
distribución de entrenamiento del modelo y ya tiene validación de esquema.

Estados operacionales por objetivo (ruta), que es la unidad: un turno con tres
ficheros son tres máquinas en paralelo, no una secuencia.

```
A  READ     ¿existe?           -> ABSENT | PRESENT
B  CREATE   si ABSENT          -> CREATED | FAILED(refused: exists)
C  EDIT     si PRESENT         -> EDITED  | FAILED(motivo)
D  modo     insert|append|replace|delete   (sub-tipo de C)
E  (guardar) implícito en la escritura
   COMMAND  fuera de la cadena -> COMMAND(exit=N)
```

**Estados terminales:** `CREATED`, `EDITED`, `FAILED`, `SKIPPED`.
No existe estado "no podemos modelarlo": si el harness se niega a intentar un
paso (p. ej. `old_string` vacío), el veredicto es `FAILED(motivo)`. La capa
operacional no tiene salida "no puedo modelarlo".

### 2.2 `NOTE`

El LLM puede anotar, nunca afirmar. Se renderiza en campo aparte y **no cuenta como
evidencia para cerrar un goal**. Cubre lo que el harness no puede saber: "me salto
esto porque Y", "esto es una precondición", "voy a comprobar X".

### 2.3 `VERDICT`

Lo escribe **solo** el harness, proyectado de los hechos de resultado. Se lleva la
identidad del paso (`:turn-id`, `:round`, `:step`) y un motivo acotado. Los tres campos son
la clave del `JOIN` con el `PLAN`, y los tres se heredan de la **intención**, no del reloj:
ver §4.8 y §4.9.

`VERDICT` no viaja suelto: se renderiza **dentro de la tarjeta `PLAN`**, en la
división que el modelo no controla. Es decir, el mismo formato sirve para las dos
direcciones, y la separación entre "lo que el LLM pidió" y "lo que la máquina
respondió" es literal.

**Formato: medio-CBOL, en el contexto ENTERO.** No es decoración, y no es solo la
tarjeta del plan: todo lo que el modelo lee cada turno es un único programa. Salida
real de `build-context` para un turno con un paso aplicado, uno rechazado, un comando
ejecutado y un fichero leído:

```
IDENTIFICATION DIVISION.
PROGRAM-ID. session-demo.
PROCEDURE DIVISION.
    01.  T3 WRITE-FILE  math_utils.py
        STATE = APPLIED
    02.  T3 EDIT-FILE  nope.py
        STATE = FAILED
        REASON = old_string not found
DATA DIVISION.
GOAL ABIERTO. 1
    todo-2  Edit nope.py
        STATUS = pending
        PRIORITY = normal
TURN 3.
    USER.
        TEXT = hazlo
    WRITE-FILE  math_utils.py
        STATE = APPLIED
        BYTES = 2048
    EXEC-COMMAND.
        COMMAND = pytest -q
        EXIT-CODE = 0
        OUTPUT =
            1 passed
    READ-FILE  app.py
        STATE = APPLIED
        CONTENTS =
            import os
            print(os.getcwd())
GOBACK.
```

`IDENTIFICATION DIVISION` y `PROCEDURE DIVISION` los emitía antes solo la tarjeta; el
YAML era el que los envolvía. Ahora las cuatro divisiones (`IDENTIFICATION`,
`PROCEDURE`, `DATA`, `GOBACK`) son el contexto, y la tarjeta del plan es su
`PROCEDURE DIVISION`. No hay dos formatos anidados.

Por qué COBOL y no el logfmt que este documento describía antes. Tres razones, en
orden de peso:

1. **El formato es el mecanismo, pero el mecanismo no es una frontera contra el
   modelo.** Hay que decirlo claro porque una versión anterior de este documento
   afirmaba lo contrario: el modelo **no escribe** en el contexto. El contexto lo
   construye el harness entero, a partir de hechos; lo único que el modelo emite
   son `tool_calls` en JSON, y eso lo valida `parse-llm-batch-to-intentions`. Así
   que "el modelo podría declarar su propio éxito en `DATA DIVISION`" es falso: no
   tiene por dónde. La separación por divisiones es **disciplina de lectura**, no
   un cortafuegos — y la defensa real está en el parser, no en el formato.

   Lo que el formato sí compra, y es lo que sostiene el cambio: un solo dialecto en
   vez de dos anidados, `STATE` como vocabulario cerrado
   (`APPLIED | FAILED | PENDING`), y el veredicto **pegado al paso** que juzga. En
   YAML plano y en logfmt el estado y la acción caían en la misma clave y el paso
   no se distinguía de un turno a otro; en la tarjeta, `01.` con su `STATE` al lado
   se lee como lo que es, un paso y su resultado.

2. **Números de paso en columna fija, con el turno al lado.** `01.`, `02.`, ...
   El orden es legible y ordenable sin parsear, que es de lo que se trata. El
   `T3` delante **no es adorno**: la numeración de COBOL se reinicia por turno, así que
   `02.` del turno 1 y `02.` del turno 2 son el mismo número y turnos distintos. Sin
   marcarlo, la tarjeta de una sesión de dos turnos tiene dos `01.` y dos `02.` y no hay
   forma de saber a qué turno pertenece cada uno — que es justo lo que el formato tiene
   que evitar.

3. **El vocabulario cerrado ya no necesita justificarse en prosa.** `STATE` es
   `APPLIED | FAILED | PENDING`. `REASON` es el único texto libre, y viene del
   stderr del SO o del rechazo de la acción.

`GOAL ABIERTO. N` no es decoración: pone las bananas pendientes del LLM en el mismo
sitio donde ve su plan, que es donde tiene sentido que las mire. Cierra cuando `N`
llega a 0, y solo entonces.

`PROGRAM-ID` es el id de sesión, no del turno: la tarjeta sobrevive a los turnos, que
es justo lo que exige I4.

#### La gramática, que es lo que hay que saber para no malinterpretarlo

Lo que emite `build-context`, en orden. Cuatro espacios por nivel de sangría, y un
valor de varias líneas pone `CLAVE =` a solas y sigue en las líneas siguientes con 12
espacios.

| Bloque | Cuándo aparece | Qué lleva |
|---|---|---|
| `IDENTIFICATION DIVISION.` | siempre | `PROGRAM-ID. <id de sesión>.` |
| `EPOCH.` / `BASELINE-TURN.` / `SUMMARY.` | si se consolidó un epoch | Va **dentro** de la identificación: es la identidad del programa, no una sección más |
| `PROCEDURE DIVISION.` | **siempre**, aunque no haya plan | Un bloque por paso del `batch-plan`: `01.  T3 WRITE-FILE  math_utils.py`, su `STATE = …` y su `REASON` si lo hay |
| `DATA DIVISION.` | siempre | `GOAL ABIERTO. N` y las bananas, con `STATUS`, `PRIORITY` y `PARENT` |
| `WARNING DIVISION.` | solo si `collect-tool-loops` detecta reintentos | `LOOP.` y qué dejar de hacer |
| `TURN n.` | uno por turno, del más viejo al actual | `USER.` con su `TEXT`, los hechos de resultado y los veredictos sin tarjeta |
| `MEMORY DIVISION.` | si hay memoria durable | Lo consolidado; va **dentro** del programa, antes del `GOBACK` |
| `GOBACK.` | siempre | Cierre |

Cinco detalles que el ejemplo de arriba no enseña:

- **`PROCEDURE DIVISION` y `DATA DIVISION` se emiten aunque estén vacías.** Un programa
  sin división de procedimiento no es un programa vacío: es un programa *mal formado*,
  y un modelo que lee «aquí no hay `PROCEDURE`» aprende que la forma del formato cambia
  según el turno. Una sección vacía se lee como lo que es: no me pidió nada.
- **`R<n>` solo sale cuando la ronda es mayor que 1** (§4.9). Con una sola respuesta del
  LLM por turno no hay nada que desambiguar, y la etiqueta no lleva ruido.
- **Un veredicto sin paso en la tarjeta aparece como `STEP 01` dentro de su `TURN`**, no
  en la `PROCEDURE`. Pasa cuando el tope de `batch-plan` ya se llevó el plan viejo y
  dejó los veredictos atrás: es el único sitio donde el modelo se entera de que algo
  falló. El que **sí** tiene paso no se repite, o diríamos la misma verdad dos veces.
- **`GOAL ABIERTO. N` cuenta los goals sin completar**, no los que existen. `N` en cero
  es lo que habilita `DONE`.
- **`MEMORY DIVISION` va dentro del programa a propósito.** Fuera del `GOBACK` no sería
  parte de este turno, sería un anexo pegado detrás.

Con `*debug-mode*` activo (`:debug` en el REPL), cada `build-context` vuelca el programa
**tal cual se le envió al modelo** a `debug/debug-<unix-time>.cob`, bajo `harness-base-dir`
—que prioriza `CL_HARNESS_DIR` sobre `*base-dir*` sobre el cwd—, creando la carpeta si no
existe. Es la misma cadena, no una reimpresión: comparar el fichero con lo que el modelo
vio no admite medias tintas.

Fijado por `protocol/el-plan-se-lee-como-un-programa-cobol`, que comprueba las tres
divisiones y que el veredicto de la máquina sobrevive al viaje de ida y vuelta.

### 2.4 `GOAL`

Estado de las bananas. El goal nombra verbo + objetivo; **cierra cuando la máquina
del objetivo llega a estado terminal**, no por coincidencia de texto.

| Verbo | Evidencia válida (1:1) |
|---|---|
| `Read` | `file-read` |
| `Write` | `file-write` con `:applied` |
| `Edit` | `file-edit` con `:applied t` |
| `Run` | `command-exec` con `:exit-code 0` |

### 2.5 `DONE`

`"tool_calls":[]` significa "propongo que se acabó". El harness **lo valida**:
¿están todos los goals cerrados? Si no, `DONE` es una mentira y el harness responde
con cuáles siguen abiertos. Mismo caso que §2.1: `DONE` es propuesta, `VERDICT` es hecho.

## 3. Invariantes

Verificables, y **escritos para que la suite los afirme sin LLM en el bucle**.

| # | Invariante | Estado actual |
|---|---|---|
| I1 | Todo `PLAN` emite exactamente un `VERDICT` por paso antes del siguiente turno | **CUMPLIDO** — `assert-step-verdict` (`src/facts.lisp:82`) ata `:step` a cada resultado; las 4 reglas `execute-intention-*` lo envuelven. Fijado por `protocol/el-plan-sobrevive-a-la-ejecucion` y `protocol/el-verdict-llega-al-modelo-con-su-paso` |
| I2 | El harness emite `VERDICT` incluso cuando se niega a intentar el paso | **CUMPLIDO** — la regla `edit` ya no pre-valida: delega en `edit-file`, cuyo guard aserta `(file-edit :applied nil :reason …)`. Un solo sitio con la regla. Fijado por `protocol/un-paso-rechazado-deja-veredicto` |
| I3 | `GOAL` cierra solo con `VERDICT`, nunca con `NOTE` ni por coincidencia de texto | **Correcto y fijado** — `todo-evidence-types` (`src/context.lisp:372`) es 1:1 estricto; `protocol/un-comando-nunca-cierra-un-goal-de-fichero` lo protege |
| I4 | Ningún `PLAN` se borra antes de que su resultado esté escrito | **CUMPLIDO** — el `(retract ?f)` va *después* del `assert-step-verdict` en las 4 reglas, y además `assert-batch-intention` aserta un `batch-plan` que nadie retracta. El plan sobrevive a su propia ejecución |
| I5 | `DONE` se valida contra el estado antes de aceptarse | **CUMPLIDO** — `parse-llm-batch-to-intentions` consulta `open-goal-tasks` y devuelve el rechazo por la vía del error de parseo, para que el modelo reciba en el turno siguiente cuáles quedan. Fijado por `protocol/done-se-valida-contra-los-goals-abiertos` |
| I6 | Un campo tiene un solo nombre en todo el sistema | **CUMPLIDO** — un solo nombre, `:reason`, en hechos y valores devueltos. Fijado por `protocol/un-rechazo-tiene-un-solo-nombre` |

**Nota sobre I1.** Cumplirlo a medias no sirve. El primer fix emito veredictos y
el test de conteo pasó, pero el contexto **no los renderizaba**: el modelo
seguía sin enterarse. Por eso el segundo test
(`el-verdict-llega-al-modelo-con-su-paso`) afirma sobre el texto renderizado, no
sobre el conteo de hechos.

## 4. Deuda que el protocolo destapa

No es deuda de formato, es deuda de mensaje:

1. ~~**No existe `VERDICT`.**~~ Resuelto: `verdict` es un hecho de primera clase.
   Y ya existe también `batch-plan`, que es el otro lado del JOIN. Un hecho `batch-plan`
   por paso —`:step`, `:action`, `:target`, `:turn-id`, `:round`— que **no se retracta** al
   ejecutarse, a diferencia de `intention`, que sí lo hace para que la regla no dispare en
   bucle. El contexto lo renderiza como la `PROCEDURE DIVISION` del programa (§2.3): cada
   paso con su veredicto al lado, y `PENDING` para lo que aún no se ha ejecutado.

   El `plan:` de abajo se conserva porque explica el problema que la tarjeta resolvió,
   no porque sea la forma actual de nada. Ya no hay `plan:`: hay `PROCEDURE DIVISION`.

   ```yaml
   # ANTES (YAML plano, eliminado en d2e45f0)
   plan:
     - step: 1
       action: READ-FILE
       target: uno.txt
       verdict: applied
     - step: 2
       action: EDIT-FILE
       target: uno.txt
       verdict: failed
       reason: old_string not found in /tmp/.../uno.txt
   ```

   Dos cosas que solo se ven al mirar el YAML: `step` y `verdict` están **en la misma
   entrada**, o sea en la misma clave que el modelo controla, y `step` no distingue un
   turno de otro. Lo primero lo arregla la separación por divisiones; lo segundo, el
   `T<n>` de la etiqueta.

   `pending` es lo que lo hace útil: convierte el contexto en un **estado de la máquina**
   y no en un parte. Antes el modelo veía resultados sueltos —"el paso 2 falló"— sin
   haber pedido un paso 2.

   En el `batch-plan` se guarda deliberadamente **solo** paso, acción, objetivo, turno y
   ronda: no la intención entera. `:content` de un `write_file` puede ser el fichero
   completo, y volarlo al contexto en cada paso es vaciarle el portapapeles al modelo. Lo
   único que el modelo no sabe de su propio plan es el estado, y el estado es justo lo que
   el harness le devuelve. `:turn-id` y `:round` no son contexto: son la mitad de la clave
   del JOIN (§4.9), y sin ellos el `STATE` que se pinta al lado del paso puede ser el de
   otro.

   El renderer **tolera hechos basura a propósito**: un `batch-plan` sin `:step`, o con
   un `:step` que no es entero, se salta. Perder el estado de los pasos buenos por un
   dato sucio en uno sería la peor forma de perderlo, porque además es silenciosa. Una
   mutación que quita ese filtro tumba el turno entero, y el test lo detecta.
2. ~~**`:parent-id` es un nombre que miente.**~~ Resuelto: eliminado de los hechos
   resultado, donde duplicaba `:turn-id` bajo otro nombre.

   Y no era solo cosmético. `select-relevant-facts` paga `+score-causal+` (50 puntos)
   a cualquier hecho que tenga `:parent-id`, para premiar la cadena causal. Como
   *todos* los resultados lo llevaban, la bonificación era para todos: 50 puntos de
   relleno que no separaban un caso de otro. Una señal que no discrimina no es una
   señal, es ruido con nombre de heurística.

   La tentación era apuntar `:parent-id` a la intención que originó el resultado. Se
   descartó: el enlace paso↔resultado ya existe y es el `:step` del `verdict`, que es
   donde debe estar. Añadir un segundo enlace al mismo hecho habría creado justo el
   problema que se quería cerrar.

   `:parent-id` queda en `agent-todo`, donde significa algo de verdad: la jerarquía de
   goals (`"root"` por defecto). Fijado por `protocol/un-nombre-no-puede-decir-dos-cosas`.

3. ~~**`batch-complete` es un nombre que miente.**~~ Resuelto: renombrado a `plan-done`,
   y `batch-complete-p` a `plan-done-p`.

   Con I5 ya no era tan falso — solo se aserta si no queda ningún goal abierto — pero el
   nombre seguía mintiendo, y un nombre que miente se lleva por delante a quien lo lea
   después, que es el siguiente que toque esto.

   Renombrar sin borrar el nombre viejo deja dos vocabularios conviviendo, que es el
   problema original con más pasos. El test afirma que `batch-complete` ya no lo emite
   nadie, no solo que `plan-done` existe. Fijado por `protocol/plan-done-y-nada-mas`.
4. ~~**El aviso de truncación miente para `command-exec`.**~~ Resuelto. Decía
   *"Use read_file or grep to inspect specifics"*, pero el stderr de un comando no vive
   en ningún fichero localizable: `read_file` no tiene a qué aplicarse. Ahora el aviso
   dice lo único que es verdad — repetir el comando más estrecho, o pasarlo por
   `head`/`tail`/`grep`. Un aviso que sugiere una herramienta que no puede funcionar es
   peor que no dar ninguno, porque gasta un turno del modelo en intentarlo.
5. **Un rechazo sin nombre acaba colándose en el campo de al lado.** Esto no se
   vio hasta arreglar I6. `read-file` metía *"ERROR: File not found"* dentro de
   `:contents`, así que un hecho `file-read` fallido era indistinguible de una
   lectura buena — y `todo-satisfied-p` solo miraba `:applied` para `:edit`, con
   lo que **leer un fichero inexistente cerraba el goal de lectura**. El modelo
   recibía `Read app.py` completado sin haber leído nada.

   El escarabajo tenía dos patas, y una era Landau:

   - `edit-file` sobre fichero ausente y `write-file` sobre fichero existente
     grande hacían `return-from` **sin asertar nada**. El paso se intentaba, se
     rechazaba, y no constaba en ninguna parte.
   - Añadir esos hechos (justo lo que exige I2) habría hecho que **todo rechazo
     cerrara el goal que precisamente no cumplió**, porque `todo-satisfied-p`
     seguía aceptando cualquier hecho cuya ruta cuadrara.

   Por eso I2 e I6 no eran independientes: arreglar I2 a secas cambiaba un fallo
   por otro. Ahora `todo-satisfied-p` exige `:applied t` para **todo** verbo, y
   hay tests que fijan las tres legs (el rechazo no cierra, el rechazo no
   desaparece, el acierto sí cierra).

6. **Restaurar una sesión nunca funcionó, y se rompía en silencio.** Tres capas, y
   ninguna daba un error visible:
   - `persist.lisp` tenía **dos** `defun restore-facts`. El que usa `restore-session`
     lo emite el propio serializador *dentro* del dump; el segundo, al final del
     fichero, lo sobreescribía y leía `(getf f :type)` / `(getf f :ts)`, un formato
     que **ningún serializador escribe**. El resultado: `Session ... restored.` en
     pantalla y cero hechos. Solo quedaba un `WARNING: redefining ...` que nadie leía.
   - `collect-facts-of-type` usaba `mapcar` con `when` en vez de `filter`, así que
     devolvía una lista de la longitud de la entrada con `nil` en los huecos.
     `length` contaba los huecos: 2 hechos de los que 1 era `file-read` daban
     longitud 2. Nadie se quejaba porque casi siempre se filtraba después.
   - Las claves de los datos restaurados llegan en **MAYÚSCULAS** (`:CONTENTS`), no
     en minúsculas como las escribe el código. Con eso, cada `data-get` del programa
     ve `nil` sobre un hecho viejo. Un hecho que parece *vacío* es peor que uno que
     parece *roto*, porque no se queja nada.

   Y el más caro de todos, invisible por construcción: el saneado usaba `eql` para
   comparar strings. `eql` sobre dos strings compara **identidad del objeto**, no
   contenido, así que la comparación nunca era cierta y el arreglo no hacía nada —
   sin error, sin aviso, sin forma de saber que había fallado.

   Resuelto: `data-get-ci` acepta las dos formas de clave, `collect-facts-of-type`
   filtra de verdad, el `defun` duplicado está borrado, y `restore-session` **avisa**
   de la deriva en vez de cargarla callado. Rechazar el dump entero no era opción
   (perdería sesiones válidas); lo que no puede ser es cargar en silencio. Fijado por
   `persistence/un-viaje-de-ida-y-vuelta-no-cambia-los-hechos` y
   `persistence/un-dump-viejo-no-resucita-el-bug-de-i2`.

7. **Un test que ata la variable equivocada pasa verde y ensucia el proyecto.**
   `protocol/el-plan-se-lee-como-un-programa-cobol` hacía `(let ((*base-dir* dir)) ...)`.
   Ese símbolo **no está importado** en el paquete de pruebas, así que ligaba
   `cl-harness/tests::*base-dir*`, que no lee nadie: el test escribía sus ficheros en
   la raíz del repo y pasaba. Ahora los directorios se atan por los tres caminos que
   existen — `dumps_dir`, `sessions_dir` y `metrics_dir` en la config, y el base dir
   con el símbolo del paquete bueno — porque `harness-base-dir` da prioridad al env
   `CL_HARNESS_DIR` sobre `*base-dir*`, y atar solo uno de los dos deja que el
   entorno de quien lanza decida dónde acaba la sesión.

8. **El número de paso no es un identificador, y la tarjeta lo trataba como si lo fuera.**
   `render-plan-card` emparejaba plan y veredicto solo por `:step`. Pero la numeración se
   reinicia en cada turno, así que `02.` del turno 1 y `02.` del turno 2 son el mismo
   número y turnos distintos. Como los dos planes suelen tener el mismo número de pasos —
   que es lo normal, no una casualidad — el emparejamiento cruzado no era un caso raro:
   era **cualquier sesión de dos turnos**. Una escritura que había funcionado aparecía como
   `FAILED`, con el motivo de un fallo que no era suyo, y el modelo leía que su operación
   se había roto.

   Y el bug de verdad estaba un nivel más abajo, en `assert-step-verdict`, que sellaba el
   veredicto con `(current-turn-id)`: **el turno en curso, no el de la intención**. Las
   reglas de ejecución disparan en cuanto la intención entra, así que cualquier intention
   ejecutada en un turno distinto del suyo — reintento, cola, cancelación tardía — quedaba
   mal sellada. Arreglado solo el JOIN, el cruce seguía ahí, solo que repartido distinto.
   Por eso el turno va en el propio hecho: `:turn-id (data-get data :turn-id)`.

   Fijado por `protocol/dos-turnos-no-comparten-paso`, que avanza `*turn-counter*` a mano
   porque el turno lo incrementa `process-turn` y no `run` — un test que encadena dos
   `parse` + `run` sin avanzar el contador está probando dos veces el mismo turno, y todo
   lo que afirma sería verdad por casualidad, no por el código.

9. **El número de paso se reinicia por ronda, y eso era el mismo bug un nivel más abajo.**
   El turno no es la única unidad que reinicia la numeración: un turno de usuario tiene
   **varias respuestas del LLM**, y cada una propone su lote con los pasos `1..N`. Un turno
   puede tener un `01. WRITE`, un `01. EXEC` y un `01. READ`, y los tres son el mismo par
   `(turn-id, step)`. Con la clave de §8 el `01. EXEC` cogía el veredicto del `01. WRITE`
   — el primero que aparecía — y salía `APPLIED` aunque hubiera fallado.

   El daño era silencioso porque en la sesión que lo destapó todos los pasos eran `APPLIED`:
   el cruce no se ve cuando los dos lados coinciden.

   La clave del JOIN es ahora **`(turn-id, round, step)`**. `assert-batch-plan` y
   `assert-batch-intention` sellan `:round` con `current-batch-round`; `assert-step-verdict`
   lo **hereda de la intención** —no de `current-batch-round`—, porque releer el valor global
   reintroduciría exactamente el fallo de §8 en la versión de la ronda. `process-turn` fija
   la ronda: `1` al abrir el turno y `(1+ i)` antes de cada llamada al LLM dentro del bucle.
   Y la **baja al terminar el turno**: subida y no bajada, se escapaba a la siguiente sesión.

   La bajada va en un `unwind-protect`, no como un `setf` al final del cuerpo. El cuerpo de
   un turno hace trabajo de verdad a mitad —`run`, las reglas de Rete, `build-context`,
   `save-session`— y cualquiera de esos puede morir. Con el reinicio al final, una excepción
   se saltaba por encima y `*batch-round*` se quedaba en la última ronda. No es cosmético: la
   ronda se graba **dentro** del hecho (`:round`) y forma parte de la clave del JOIN, así que
   el valor fugado sella los pasos del turno siguiente con una ronda que no es suya y sus
   veredictos dejan de encajar. El fallo se vería dos turnos después, cuando el modelo pregunta
   por qué el paso 1 no tiene veredicto. El `setf` de **entrada** no sobra: son dos defensas
   distintas —esta da entrada a cualquier turno, el `unwind-protect` da salida a este turno—.

   Fijado por `protocol/un-turno-que-revienta-no-deja-la-ronda-puesta`, que inyecta un fallo en
   `build-context` en la cuarta llamada (con la ronda ya en 2) y mide dos cosas: que la ronda
   sale a 1, y que el plan del turno siguiente se sella en la ronda 1. Anulando el cleanup del
   `unwind-protect`, el test falla con la ronda en 3 y el plan siguiente en la ronda 3.

   En la tarjeta la ronda solo aparece cuando es `> 1` (`01. T1 R2 EDIT-FILE …`): con una
   sola respuesta por turno no hay nada que desambiguar y la etiqueta no se ensucia. La
   ausencia de `:round` se normaliza a `1`, para que un plan escrito a mano o un dump de
   otra versión empareje con la ronda 1 en vez de quedarse sin veredicto.

   Fijado por `protocol/dos-rondas-en-un-turno-no-comparten-paso`: un turno, dos rondas, un
   paso `1` que se aplica y un paso `1` que falla. Sin la ronda en la clave, el segundo sale
   `STATE = APPLIED` con el motivo del primero, y el fallo desaparece de la tarjeta.

## 5. Tests que fijan el protocolo

Sin red, sin LLM, sobre el harness. La suite entera son **91 tests / 305 checks** y se
corre con `./run-tests.sh` (código 0 si pasa, 1 si falla, 2 si el sistema no carga). De
esos, los que **fijan un invariante del protocolo** son los de abajo; los demás cubren
parseo, acciones, goals, métricas y configuración.

- `protocol/el-plan-sobrevive-a-la-ejecucion` — `PLAN` de 3 pasos → los 3 tienen veredicto. (I1)
- `protocol/un-paso-rechazado-deja-veredicto` — `edit` con `old_string` vacío → aparece
  como edit no aplicado y enlazado a su paso. (I2)
- `protocol/done-se-valida-contra-los-goals-abiertos` — `DONE` con goals abiertos → el
  harness lo rechaza y nombra los abiertos. (I5)
- `protocol/un-comando-nunca-cierra-un-goal-de-fichero` — `command-exec` nunca cierra un
  goal de fichero. (I3)
- `protocol/el-verdict-llega-al-modelo-con-su-paso` — el veredicto se renderiza, con su
  `:step`, distinguiendo `applied` de `failed` y llevando el motivo.
- `protocol/un-rechazo-tiene-un-solo-nombre` — un rechazo usa `:reason` y solo `:reason`,
  con el mismo texto en el hecho y en el valor devuelto. (I6)
- `protocol/ningun-rechazo-desaparece` — editar un fichero ausente y escribir a ciegas
  dejan hecho, lo dejan como `:applied nil`, con motivo, y no se renderizan como
  `wrote`. (I2)
- `protocol/un-hecho-rechazado-no-descarga-un-goal` — un hecho `:applied nil` no cierra
  el goal, para ningún verbo, y el camino feliz sigue cerrando. (I3)
- `protocol/el-plan-sobrevive-como-plan` — el `batch-plan` no se retracta al ejecutarse y
  se renderiza con el veredicto de cada paso al lado, incluido `pending`. (I1/I4)
- `protocol/un-paso-malformado-no-arrastra-a-los-demas` — un `batch-plan` roto se salta
  sin tumbar el render ni perder los pasos buenos.
- `protocol/el-plan-se-lee-como-un-programa-cobol` — la tarjeta tiene las tres divisiones,
  el `veredicto` de la máquina sobrevive al viaje, y los ficheros del test no salen del
  directorio temporal. (§2.3)
- `context/el-contexto-no-es-yaml` — ninguna clave del YAML viejo sobrevive en el
  contexto, y el programa entero se lee como un solo dialecto. (§2.3)
- `context/goal-abierto-no-cuenta-los-completados` — `GOAL ABIERTO. N` sale de los goals
  **sin completar**, no de los que existen: un goal cerrado bajo un rótulo `ABIERTO`.
  (§2.3)
- `context/render-shows-file-write-before-its-command` — dos hechos del mismo segundo se
  ordenan con su clave de orden, no por timestamp: un comando salía por encima del
  `write` que lo había producido.
- `persistence/un-viaje-de-ida-y-vuelta-no-cambia-los-hechos` — siete tipos de hechos y
  sus datos sobreviven a `save` → motor nuevo → `restore`, y el motivo de un rechazo sigue
  a su hecho.
- `persistence/un-dump-viejo-no-resucita-el-bug-de-i2` — un dump con la forma de antes de
  I2 carga entero (perder la sesión sería peor), avisa de lo que trae de otra versión, y
  no devuelve `"ERROR: File not found"` disfrazado de `:contents`.
- `protocol/dos-turnos-no-comparten-paso` — el paso 2 del turno 1 y el del turno 2 se
  emparejan con su propio veredicto. Aquí falló, y no por poco: una escritura que había
   funcionado aparecía como `FAILED` con el motivo de un fallo ajeno. (§2.3)
- `protocol/dos-rondas-en-un-turno-no-comparten-paso` — dos respuestas del LLM en el mismo
  turno, las dos con un paso `1`: cada una conserva su veredicto. El fallo que falla es el
   segundo paso, que se ejecutó con `STATE = APPLIED` y el motivo de otro. (§2.3)
- `protocol/un-turno-que-revienta-no-deja-la-ronda-puesta` — un turno que muere por excepción
  a mitad no deja `*batch-round*` puesta, y el turno siguiente se sella en la ronda 1. La
  precondición se comprueba: si el fallo inyectado no llegara a dispararse, el test pasa por
  la rama que no importa y no mide nada. (§4.9)

- `protocol/plan-done-y-nada-mas` — `DONE` sin goals abiertos se acepta, y nadie emite
  ya el nombre viejo.

## 5b. Una nota sobre cómo se cazan estos fallos

Cada fix de invariante se comprobó **mutando el código a propósito** y viendo que el
test se pone rojo. Un test verde que no puede fallar no prueba nada, y en un sistema
donde el LLM es el otro extremo del cable, un test que no muerde es peor que no
tenerlo: da confianza falsa.

Las dos mutaciones que se hicieron:

- Cambiar la comparación de `:verdict` por un test truthy (`:applied` y `:failed` son
  ambos truthy) → rojo en la aserción exacta.
- Quitar la exigencia de `:applied t` en `todo-satisfied-p` → rojo en las dos legs
  de goal, y verde en la del acierto, que es justo lo que un test de una sola
  aserción no habría detectado.

## 6. Decisiones pendientes

- **Lado de escritura del modelo:** el contexto ya es medio-CBOL entero (§2.3),
  pero la salida del modelo sigue siendo JSON `tool_calls`. No se le pide escribir
  COBOL: no se le puede exigir un dialecto para el que no fue entrenado, y el parser
  ya valida JSON. Decidir si algún día se acepta COBOL de entrada —y con qué parser—
  queda abierto; hoy no hace falta.
- **`fact-to-yaml` en las métricas:** `src/metrics.lisp` sigue serializando en YAML.
  No es transporte al modelo, es un volcado de telemetría, así que se queda como está
  hasta que haya una razón para tocarlo.
- **Orden de trabajo:** resuelto por los hechos, no por decreto. Los tests se
  escribieron primero y el documento salió de ellos; el orden contrario ya había
  producido antes un §2.3 entero que describía un formato que nadie implementó.
- **`restore-session` sigue ejecutando el dump con `load`.** El dump es código Lisp: un
  `(assert (harness-fact …))` que se vuelve a evaluar. Hoy se audita **después** de
  cargar, así que un dump manipulado ejecuta código antes de que exista el aviso. La
  auditoría de verdad va **antes**: leer el fichero, buscar las formas viejas
  (`:parent-id`, `:contents` con `ERROR: File not found`) y avisar antes de ejecutar
  nada. Cuesta un parser y no un `load`; está pendiente, no resuelto.
- **`debug/` crece sin podar.** Cada `build-context` con `*debug-mode*` escribe un
  `debug-<timestamp>.cob` y nadie los borra: una sesión larga con depuración activa deja
  un fichero por turno. El timestamp completo ordena bien, pero no hay tope ni limpieza.
  Decidir si `debug/` se poda por antigüedad o si lo borra `:clear`.
- **Un veredicto huérfano se renderiza como `STEP NN` sin turno ni ronda.** Solo ocurre
  cuando el tope de `batch-plan` retiró el plan y dejó los veredictos, así que es la ruta
  rara; pero dos veredictos huérfanos del mismo turno y distinta ronda con el mismo
  número de paso salen indistinguibles. Llevar la ronda también ahí es barato; no está
  hecho porque el caso no se ha visto en una sesión real.
