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
| `GOAL` | harness → LLM | harness | `agent-todo` en YAML | Estado derivado de `VERDICT` |
| `DONE` | LLM → harness | LLM | `"tool_calls":[]` | **Propuesta**, no hecho |

### 2.1 `PLAN`

El LLM propone N pasos ordenados. Formato actual: JSON ToolUse, validado por
`normalize-batch-tool-calls` (`src/llm.lisp:1167`). **No cambiar** — ya está en la
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
operacional no tiene Recognize限.

### 2.2 `NOTE`

El LLM puede anotar, nunca afirmar. Se renderiza en campo aparte y **no cuenta como
evidencia para cerrar un goal**. Cubre lo que el harness no puede saber: "me salto
esto porque Y", "esto es una precondición", "voy a comprobar X".

### 2.3 `VERDICT`

Lo escribe **solo** el harness, proyectado de los hechos de resultado. Se lleva la
identidad del paso (`:step`) y un motivo acotado.

```
step=2/3 target=math_utils.py state=EDITED verdict=applied reason=-
step=3/4 target=test_math_u.py state=EDIT verdict=failed reason=anchor not found at line 12
```

`reason` es el único campo de texto arbitrario (viene del stderr del SO). Los otros
cuatro son tokens. Formato: **logfmt**, un registro por línea, con el vocabulario
cerrado de §2.1. Un registro malformado se descarta sin arrastrar a los demás.

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
| I1 | Todo `PLAN` emite exactamente un `VERDICT` por paso antes del siguiente turno | **CUMPLIDO** — `assert-step-verdict` (`src/facts.lisp:56`) ata `:step` a cada resultado; las 4 reglas `execute-intention-*` lo envuelven. Fijado por `protocol/el-plan-sobrevive-a-la-ejecucion` y `protocol/el-verdict-llega-al-modelo-con-su-paso` |
| I2 | El harness emite `VERDICT` incluso cuando se niega a intentar el paso | **CUMPLIDO** — la regla `edit` ya no pre-valida: delega en `edit-file`, cuyo guard aserta `(file-edit :applied nil :reason …)`. Un solo sitio con la regla. Fijado por `protocol/un-paso-rechazado-deja-veredicto` |
| I3 | `GOAL` cierra solo con `VERDICT`, nunca con `NOTE` ni por coincidencia de texto | **Correcto y fijado** — `todo-evidence-types` (`src/context.lisp:334`) es 1:1 estricto; `protocol/un-comando-nunca-cierra-un-goal-de-fichero` lo protege |
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
   por paso —`:step`, `:action`, `:target`— que **no se retracta** al ejecutarse, a
   diferencia de `intention`, que sí lo hace para que la regla no dispare en bucle. El
   contexto lleva ahora un bloque `plan:` donde cada paso aparece con su veredicto al
   lado, y `pending` para lo que aún no se ha ejecutado.

   ```yaml
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

   `pending` es lo que lo hace útil: convierte el contexto en un **estado de la máquina**
   y no en un parte. Antes el modelo veía resultados sueltos —"el paso 2 falló"— sin
   haber pedido un paso 2.

   En el `batch-plan` se guarda deliberadamente **solo** paso, acción y objetivo, no la
   intención entera: `:content` de un `write_file` puede ser el fichero completo, y
   volarlo al contexto en cada paso es vaciarle el portapapeles al modelo. Lo único que
   el modelo no sabe de su propio plan es el estado, y el estado es justo lo que el
   harness le devuelve.

   El renderer **tolera hechos basura a propósito**: un `batch-plan` sin `:step`, o con
   un `:step` que no es entero, se salta. Perder el estado de los pasos buenos por un
   dato sucio en uno sería la peor forma de perderlo, porque además es silenciosa. Una
   mutación que quita ese filtro tumba el turno entero, y el test lo detecta.
2. **`:parent-id` es un nombre que miente.** En los hechos resultado vale
   `(current-turn-id)` — lo mismo que `:turn-id` (`src/actions.lisp:159`). Debería
   apuntar a la intención, o eliminarse.
3. **`batch-complete` es un nombre que miente.** Significa "el modelo mandó
   `tool_calls` vacío", no "el trabajo está hecho". Debería ser `plan-done`.
4. **El aviso de truncación miente para `command-exec`.** Dice *"Use read_file or
   grep to inspect specifics"* (`src/context.lisp:177`), pero no se puede hacer
   `read_file` del stderr de un comando: no vive en un fichero localizable. El único
   recupero es re-ejecutar más estrecho.
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

## 5. Tests que fijan el protocolo

Sin red, sin LLM, sobre el harness. Los cinco primeros están escritos y verdes:

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

- **Codificación del alambre:** logfmt (§2.3) frente a TSV posicional con cabecera.
  El YAML actual se queda como **vista humana**, no como transporte.
- **Orden de trabajo:** ¿tests primero (el documento emerge de ellos) o documento
  primero (los tests lo implementan)?
