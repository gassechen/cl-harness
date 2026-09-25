# Batch JSON sin ToolUse nativo con `auto` y rotación de modelos

> **Fecha:** 2026-09-25  
> **Origen de la corrida:** `/home/mtk/TEST-PRUEBAS/cl-harness-yaml-debug`  
> **Setup:** modo `batch`, endpoint OpenAI-compatible local, `llm_model: "auto"`  
> **Clave:** redactada; se suministla por configuración/entorno

## 1. Objetivo

Validar el modo batch después de migrar el parser a JSON compatible con ToolUse.
La prueba de estrés no dependía de la calidad de un modelo concreto: usó `auto`,
por lo que el proveedor pudo cambiar de modelo entre llamadas del mismo turno.

El experimento tiene dos objetivos independientes:

1. **Compatibilidad:** permitir que un modelo sin ToolUse nativo exprese un plan
   JSON y que las reglas de Rete lo ejecuten sin depender de una API de tools.
2. **Eficiencia de contexto:** medir si el YAML generado por Rete reduce los
   tokens enviados frente al contexto `naive`, independientemente de si la
   acción se planificó mediante batch o ToolUse nativo. Esto es una hipótesis
   condicional: depende de los hechos sobrevivientes, las reglas de poda y el
   tamaño de la tarea.

La corrida actual permite observar ambos objetivos. Se comprobaron además:

- `response_format: {"type":"json_object"}`;
- normalización de ToolUse canónico y flatten (`gpt-oss`);
- argumentos JSON serializados como string;
- reintento acotado ante una respuesta inválida;
- validación atómica del lote;
- lectura previa antes de editar o escribir;
- ejecución de lecturas, escrituras y comandos;
- respuesta final con `tool_calls: []`.

## 2. Configuración

La configuración sanitizada está en [`config.json`](./config.json). El endpoint
se mantiene local porque forma parte del entorno de la corrida:

```json
{
  "llm_provider": "batch",
  "llm_model": "auto",
  "llm_endpoint": "http://localhost:3001/v1/chat/completions"
}
```

El protocolo canónico esperado por el harness es:

```json
{
  "tool_calls": [
    {
      "id": "call-1",
      "type": "function",
      "function": {
        "name": "read_file",
        "arguments": {"path": "math_utils.py"}
      }
    }
  ],
  "response": ""
}
```

Una respuesta final usa `{"tool_calls":[],"response":"..."}`. El lote se valida
completo antes de asertar intenciones; un error no produce intenciones parciales.

## 3. Prompt de la prueba

Se pidió crear un mini proyecto Python en el directorio actual:

> Creá un mini proyecto Python con `math_utils.py` (`factorial`, `is_prime`,
> `fibonacci`), `test_math_utils.py` con casos normales y borde, y `main.py` con
> ejemplos. Ejecutar `python3 -m unittest test_math_utils.py`, corregir si falla,
> ejecutar `python3 main.py` y reportar funciones, cantidad de tests y output.

La implementación y las reglas relevantes están en
[`src/llm.lisp`](../../src/llm.lisp) y [`README.md`](../../README.md#7-modo-batch-intenciones-json-en-rete).

## 4. Resultado

| Medida | Resultado |
|---|---:|
| Llamadas al endpoint en el turno | 5 |
| Modelo `nvidia/nemotron-3-ultra-550b-a55b` | 4 |
| Modelo `meta/muse-glimmer-30b` | 1 |
| Tests Python | **9/9** |
| Respuesta final | Coherente, sin `BATCH_JSON_INVALID` |
| `session-id` de la corrida | `session-3999334880` |

La salida verificable de `main.py` fue:

```text
Factorial of 5 is 120
Is 29 a prime? Yes
The 10th Fibonacci number is 55
```

En una repetición del stress test se produjo un `502` transitorio del endpoint;
el flujo batch lo recuperó y volvió a cerrar el turno correctamente.

### 4.1. Rete/YAML frente a `naive`

Las métricas del harness separan el contexto que Rete renderiza como YAML del
snapshot `naive` previo a la poda. `reduction_pct` positivo significa que el
YAML ocupa menos tokens; uno negativo significa que el overhead de serializar
los hechos supera al baseline.

| Corrida | `context-tokens` (Rete) | `naive-tokens` | `reduction_pct` | Lectura |
|---|---:|---:|---:|---|
| [`session-3999154036`](../batch-freellm-2026-09-23/metrics/session-3999154036-metrics.json) | 29 | 41 | **+29,27%** | YAML más compacto |
| [`session-3999080576`](../../metrics/session-3999080576-metrics.json) | 39 | 50 | **+22,00%** | YAML más compacto |
| [`session-3999334880`](./metrics/session-3999334880-metrics.json) | 265 | 195 | **-35,90%** | YAML más grande |

La corrida larga de este experimento no muestra ahorro: el prompt y los hechos
dominaron más que la poda. Esto confirma que Rete puede reducir el contexto, pero
que la reducción es condicional y no automática. `real-prompt-tokens` mide el
consumo real del proveedor y es una métrica distinta de esta comparación.

Esta medición no depende de batch ni de ToolUse: para una comparación justa solo
se compara `build-yaml-context` (Rete) contra `naive-context-string` con la misma
tarea. Si se desea además medir el costo total del proveedor, hay que fijar el
mismo `response.model` y comparar los tokens acumulados de ambas ramas.

### 4.2. Fidelidad al cambiar de modelo

El mismo estado serializado permite que un modelo distinto reconstruya el
contexto sin cargar todos los archivos de texto plano ni depender de la memoria
de la conversación. Esa propiedad se probó para la tarea del experimento; no
significa que el YAML sea ilimitadamente completo, porque Rete sigue aplicando
podas, TTL, límites por tipo y truncado de payloads.

## 5. Routing de modelos

El resumen de routing está en [`model-routing.json`](./model-routing.json). El
harness no fija el modelo efectivo: con `auto` se debe leer `response.model` de
cada respuesta del proveedor. El mismo turno puede rotar entre proveedores sin
que el estado dependa de la memoria implícita del modelo.

## 6. Artefactos

- [`project/`](./project/): archivos generados por el modelo y ejecutados durante
  la prueba.
- [`metrics/session-3999334880-metrics.json`](./metrics/session-3999334880-metrics.json):
  métricas de la corrida principal; `llm-iterations: 5`.
- [`config.json`](./config.json): configuración de referencia sin credenciales.
- [`model-routing.json`](./model-routing.json): conteo de modelos efectivos.

Las sesiones y dumps completos se conservaron en el test-bed de origen para no
duplicar artefactos de ejecución; el resumen y las métricas relevantes quedan
versionados aquí.

## 7. Reproducir la verificación del proyecto

Desde [`project/`](./project/):

```shell
python3 -m unittest test_math_utils.py
python3 main.py
```

Resultados esperados: `Ran 9 tests` y las tres líneas de salida indicadas en la
sección 4. Para repetir la llamada al LLM hay que usar un binario compilado con
el código actual, proporcionar la clave por el mecanismo habitual y ejecutar el
prompt desde un directorio de trabajo limpio.

## 8. Conclusión

El harness mantuvo la separación entre planificación y ejecución cuando `auto`
cambió de modelo dentro del mismo turno. La corrida demuestra compatibilidad
con la ruta JSON sin ToolUse nativo y que el estado YAML permite reconstruir el
contexto. La medición de Rete muestra un ahorro potencial, pero condicional:
`+29,27%` y `+22,00%` en corridas cortas y `-35,90%` en la corrida larga. No
se debe atribuir esa diferencia al modo batch; depende de la poda, del formato
y del volumen de hechos.
