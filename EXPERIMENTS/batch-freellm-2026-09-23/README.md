Este archivo contiene los resultados crudos del experimento documentado en
README_EXPERIMENT.md (raíz del repo).

Origen: /home/mtk/TEST-PRUEBAS/cl-harness-test1
Fecha: 2026-09-23
Provider: batch (freellm proxy en http://localhost:3001/v1/chat/completions,
llm_model "auto", API key redactada en config.json)

Contenido:
- project/   : proyecto de prueba (todo.py, test_todo.py, cli.py) + el
               README.md y system-prompt.md que el LLM escribió/editó
- sessions/  : dump JSON interoperable por sesión
- metrics/   : métricas de contexto (curado vs naive vs uso real)
- dumps/     : dumps Lisp restaurables (hechos de turno + memoria larga)