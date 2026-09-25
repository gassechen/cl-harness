# Crecimiento de contexto en 8 turnos: Rete/YAML frente a `naive`

> **Fecha:** 2026-09-25  
> **Origen de la corrida:** `/home/mtk/TEST-PRUEBAS/cl-harness-growth-v3-2026-09-25`  
> **Setup:** modo `batch`, endpoint OpenAI-compatible local, `llm_model: "nvidia/nemotron-3-ultra-550b-a55b"` (modelo fijo para que los tokens reales sean comparables entre turnos)  
> **Clave:** redactada; se suministra por configuración/entorno  
> **Sesión:** [`session-3999340228`](./metrics/session-3999340228-metrics.json)

## 1. Objetivo

Responder una pregunta concreta: **qué pasa con los tokens y con el YAML cuando el
proyecto crece turno a turno dentro de la misma sesión.**

El experimento anterior
([`batch-freellm-auto-2026-09-25`](../batch-freellm-auto-2026-09-25/)) midió una
sola vuelta con un proyecto chico y obtuvo `-35,90%`: el overhead de serializar
los hechos superaba al baseline `naive`. Eso no permite distinguir entre dos
causas opuestas:

- el YAML es siempre más caro que `naive`, o
- el YAML es más caro solo mientras el proyecto es chico y cheaper cuando crece.

Agregar funcionalidades turno a turno en una misma sesión es lo que separa ambas.

## 2. Método

### 2.1. Una sola sesión acumulativa

El binario standalone resuelve `CL_HARNESS_PROMPT` con `run-one-shot`, que hace
`boot-memory` + `new-session-id` + `metrics-reset` en cada proceso
(`src/repl.lisp:272`): invocar el binario por turno produciría N sesiones
independientes y una curva sin sentido. Para acumular estado se usa el REPL
interactivo (`start-harness`, `src/repl.lisp:305`) alimentado por un archivo de
entradas: una línea por turno, y al final `:context`, `:metrics` y `:quit`.

```shell
./cl-harness < prompts.txt > run.log 2>&1
```

Todas las métricas viven en un único archivo y las 8 filas comparten sesión.

### 2.2. Crecimiento determinista del proyecto

Dos intentos previos con edición a cargo del modelo no fueron confiables
(detalle en la sección 7). Para que la variable medida fuera el contexto y no la
habilidad del modelo, cada etapa del proyecto se aplica con un script idempotente
([`project/stage.py`](./project/stage.py)) que el agente ejecuta con
`exec_command`, y luego verifica con `read_file` y `python3 -m unittest -v`.

El crecimiento del proyecto es real y verificable:

| | Inicial | Final |
|---|---:|---:|
| Bytes de código | 2 794 | 8 080 |
| Módulos de código | 1 (`math_utils`) | 2 (`math_utils`, `text_utils`) |
| Funciones | 3 | 10 |
| Tests | 9 | 26 |

Etapas: `gcd` → `is_perfect_square` → `sum_range` → `prime_factors` → módulo
`text_utils` con 3 funciones → `main.py` integrando todo → verificación final.
El proyecto final está en [`project/`](./project/) y corre `26/26` tests.

## 3. Configuración

[`config.json`](./config.json) sanitizado. Los límites se ajustaron respecto del
test-bed original para que la curva fuera observable en 8 turnos:

| Clave | Test-bed previo | Esta corrida | Motivo |
|---|---:|---:|---|
| `fact_ttl_seconds` | 120 | 7 200 | Con 2 min de TTL de pared la curva se reiniciaba a mitad de la corrida |
| `fact_ttl_turns` | 5 | 20 | 8 turnos con TTL de 5 expulsaría los hechos de la etapa 1 |
| `max_facts_per_type` | 5 | 8 | Permite 8 hechos por tipo; es el límite que termina mandando |
| `max_fact_payload_chars` | 1 000 | 1 200 | Margen para contenidos de archivo |
| `max_context_chars` | 8 000 | 10 000 | Presupuesto del bloque renderizado |
| `max_context_facts` | 30 | 60 | Techo global, no fue el factor limitante |
| `llm_max_tool_iterations` | 12 | 8 | Acota los bucles de herramientas por turno |
| `llm_model` | `auto` | `nvidia/nemotron-3-ultra-550b-a55b` | Modelo fijo: `real-prompt-tokens` comparable entre turnos |

## 4. Curva por turno

Tokens heurísticos del harness (1 token ≈ 4 caracteres). `reduccion_pct` positivo
significa que el YAML curado ocupa menos tokens que el snapshot `naive` previo a
la poda. Fuente: [`growth-table.json`](./growth-table.json).

| Turno | `context-tokens` | `naive-tokens` | Reducción | `prompt-tokens` real | Iteraciones |
|---:|---:|---:|---:|---:|---:|
| 1 | 82 | 83 | +1,20% | 1 761 | 2 |
| 2 | 956 | 1 173 | +18,50% | 2 800 | 3 |
| 3 | 1 732 | 2 036 | +14,93% | 3 151 | 2 |
| 4 | 1 929 | 2 572 | +25,00% | 3 295 | 3 |
| 5 | 1 925 | 4 153 | +53,65% | 3 292 | 2 |
| 6 | 1 720 | 4 819 | +64,31% | 2 938 | 3 |
| 7 | 1 761 | 5 509 | +68,03% | 2 795 | 2 |
| 8 | 1 729 | 5 330 | +67,56% | 2 807 | 2 |
| **Total** | **11 834** | **25 675** | **+53,91%** | 22 839 | 19 |

Las dos curvas se separan justo cuando el proyecto empieza a crecer:

- `naive` crece de forma casi monótona (83 → 5 330 tokens) porque acumula todos
  los hechos sin podar: cada lectura de archivo, cada salida de comando y cada
  respuesta quedan para siempre.
- El YAML curado **satura** desde el turno 4 en la banda 1 720–1 930 tokens. No
  crece con el proyecto: se queda en lo que las reglas de Relevance y los límites
  dejan pasar.
- `prompt-tokens` real del proveedor tampoco crece: 1 761 en el turno 1 y ~2 800
  desde el turno 4, aunque el `naive` en ese tramo se duplica. Es la señal
  práctica: **el costo por turno deja de crecer aunque el proyecto acumule
  historial**.

## 5. Qué le pasó al YAML

Instantáneas del bloque de contexto tomadas con `:context` después de los turnos 2, 4,
6 y 8 ([`yaml/`](./yaml/)):

| Etapa | Turno | Caracteres | Líneas |
|---|---:|---:|---:|
| 1 | 2 | 6 927 | 176 |
| 2 | 4 | 7 612 | 204 |
| 3 | 6 | 7 598 | 178 |
| 4 | 8 | 6 547 | 143 |

El YAML **no crece de forma monótona**: sube hasta ~7,6 k y después baja. La
causa es identificable en las reglas, no en el formato:

- El límite que manda es `max_facts_per_type` (8), no `max_context_chars`
  (10 000). El máximo observado fueron 7 612 caracteres, muy por debajo del
  presupuesto de caracteres.
- Al final de la sesión había exactamente **40 hechos = 5 tipos × 8**: `user-input`,
  `llm-response`, `file-read`, `command-exec` y `batch-complete`, 8 cada uno. Es el
  tope por tipo, no un corte de caracteres.
- La etapa 4 (6 547) es la más chica porque la expulsión por tipo se llevó los
  hechos `file-read` más antiguos, que eran los de mayor payload (contenidos
  completos de archivos). El YAML intercambia hechos viejos por hechos recientes,
  y los archivos grandes son lo primero que sale.

Es decir: el YAML es un **working set acotado por reglas**, no un historial. Eso es
deseable para el costo, y significa que el detalle viejo se pierde del contexto y
hay que recuperarlo con `read_file` (de ahí la regla de no repetir acciones ya
presentes en el contexto).

## 6. Lectura combinada con el experimento anterior

| Corrida | Proyecto | Turnos | `context` | `naive` | Reducción total |
|---|---|---:|---:|---:|---:|
| `session-3999154036` | chico | 1 | 29 | 41 | +29,27% |
| `session-3999080576` | chico | 1 | 39 | 50 | +22,00% |
| `session-3999334880` | chico | 1 | 265 | 195 | -35,90% |
| `session-3999340228` | creciente | 8 | 11 834 | 25 675 | **+53,91%** |

El signo de la reducción **cambia con el tamaño de la base de hechos**: en las
corridas de un solo turno sobre proyecto chico el YAML paga overhead de
serialización y puede ser más caro que `naive`; en una sesión que acumula trabajo
el YAML se queda acotado y `naive` sigue creciendo. Por eso la cifra anterior
("-35,90%") no era una propiedad de Rete, sino de una corrida corta.

## 7. Intentos previos: por qué el crecimiento quedó en scripts

Dos corridas anteriores, conservadas en el test-bed, 덜 muestra que el cuello de
botella era el modelo y no el harness:

- `/home/mtk/TEST-PRUEBAS/cl-harness-growth-attempt1` (`session-3999339038`):
  5 turnos, `79 → 2 099` de contexto contra `80 → 4 132` naive (+34,59% total). El
  turno 2 funcionó (editó dos archivos y corrió los tests); el turno 3 devolvió
  respuesta vacía; el turno 4 aplicó un `edit_file` sobre `test_math_utils.py` y
  rechazó el de `math_utils.py` con `old_string not found` porque el snippet
  venía de una versión anterior del archivo; el turno 6 entró en bucle de
  herramientas y llegó a 14 rondas hasta que se detuvo con `SIGTERM` (el handler
  persistió métricas y sesión).
- `/home/mtk/TEST-PRUEBAS/cl-harness-growth-attempt2` (`session-3999339973`):
  con un `system-prompt.md` que exigía leer antes de editar, el modelo duplicó
  `gcd` en vez de agregar `is_perfect_square` y no landingó los tests. 4 turnos,
  +19,49% total.

Los dos intentos son evidencia de que las salvaguardas funcionan (escrituras a
ciegas rechazadas, bucles acotados, métricas persistidas al recibir `SIGTERM`) y de
que, para medir el comportamiento del contexto, conviene separar el crecimiento del
proyecto de la calidad del modelo.

## 8. Límites de esta medición

- `context-tokens` y `naive-tokens` son heurísticos (1 token ≈ 4 caracteres). La
  proporción entre ambos es la que importa, no el valor absoluto.
- `prompt-tokens` es **acumulativo por turno**: suma las iteraciones del tool loop
  de ese turno e incluye system prompt y mensaje de usuario, así que no es
  comparable turno a turno como "tokens de contexto". Se usa aquí como indicador de
  saturación, no como medida fina.
- Una sola corrida, un solo modelo, un proyecto: sin repeticiones no hay barras de
  error. La forma de la curva es la observación; las cifras son de `n=1`.
- El techo observado depende de la configuración de la sección 3. Con
  `max_facts_per_type` o `max_context_chars` más altos, el YAML crecería hasta
  esos límites y `naive` también: la separación se mantiene, pero los valores
  absolutos cambian.
- El crecimiento lo aplicaron scripts idempotentes, no el modelo. El contexto, las
  facts y el tool loop sí son los reales del harness.

## 9. Artefactos

- [`growth-table.json`](./growth-table.json): curva por turno, totales, tamaños de
  YAML y proyecto inicial/final.
- [`metrics/session-3999340228-metrics.json`](./metrics/session-3999340228-metrics.json):
  métricas del harness, 8 eventos.
- [`sessions/session-3999340228.json`](./sessions/session-3999340228.json): hechos
  de la sesión (40 al final).
- [`yaml/etapa-1.yaml`](./yaml/etapa-1.yaml) … [`yaml/etapa-4.yaml`](./yaml/etapa-4.yaml):
  instantáneas del bloque de contexto.
- [`project/`](./project/): proyecto final (2 794 → 8 080 bytes, 26 tests) y
  [`stage.py`](./project/stage.py) con las etapas.
- [`prompts.txt`](./prompts.txt), [`system-prompt.md`](./system-prompt.md),
  [`run.log`](./run.log) y [`config.json`](./config.json): reproducción.

## 10. Reproducir la verificación del proyecto

Desde [`project/`](./project/):

```shell
python3 -m unittest -v   # Ran 26 tests -> OK
python3 main.py
```

Para repetir la corrida completa hay que usar un binario compilado con el código
actual, clonar el `config.json` del test-bed con la clave, dejar
`stage.py`, `prompts.txt` y `system-prompt.md` en el directorio de trabajo y
ejecutar `./cl-harness < prompts.txt > run.log 2>&1` desde un directorio limpio.

## 11. Conclusión

El experimento confirma la hipótesis condicional y agrega la dimensión que faltaba:
**la reducción de Rete/YAML crece con el tamaño de la base de hechos**. En una
sesión que acumula ocho etapas, el contexto curado se estabiliza en ~1,7–1,9 k
tokens mientras `naive` pasa de 83 a 5 330; la separación pasa de +1,20% a +68,03%
y el total de la sesión queda en +53,91%. El límite que produce la saturación es
`max_facts_per_type`, con 40 hechos finales, y el YAML se estabiliza en un working
set de ~7 k caracteres.

Esto no es propiedad del modo batch ni de ToolUse: es el comportamiento de
`build-yaml-context` frente a `naive-context-string` con la misma tarea. Lo que
cambia con el transporte es únicamente cómo se planifican las acciones.
