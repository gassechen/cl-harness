(in-package :cl-harness)

;;; ============================================
;;; Fact templates for Lisa/Rete engine
;;; ============================================

(deftemplate harness-fact ()
  (slot fact-type)
  (slot timestamp)
  (slot data))

(deftemplate context-slot ()
  (slot fact-type)
  (slot count))

;;; ============================================
;;; Causal metadata (Phase 2)
;;; ============================================
;;; Every fact carries :turn-id INSIDE its data plist (not a template slot) so
;;; persistence, dedup and YAML rendering need no change. Linkage is captured at
;;; ingestion, never derived by rules.
;;;
;;; :parent-id NO se pone en los hechos resultado. Antes valia lo mismo que
;;; :turn-id, y un campo que repite otro bajo otro nombre no es un campo: es una
;;; segunda puerta por la que colarse. Peor: select-relevant-facts paga +50 por
;;; tenerlo, asi que la bonificacion causal era para todos los resultados y no
;;; discriminaba nada. Ahora :parent-id queda solo en AGENT-TODO, donde significa
;;; de verdad la jerarquia de goals, y la senal causal vuelve a senal.

(defparameter *turn-counter* 0
  "Monotonic turn number; incremented at the start of each process-turn.")

(defparameter *batch-round* 1
  "Ronda (respuesta del LLM) dentro del turno actual.

   Un turno de usuario puede tener VARIAS respuestas del LLM: process-turn
   itera hasta que el plan termina. Cada respuesta propone su propio lote con
   los pasos numerados 1..N, asi que :step se reinicia y `(turn-id, step)` NO
   identifica un paso dentro de un turno: el paso 2 del lote 1 y el paso 2 del
   lote 3 son el mismo par. El JOIN plan<->veredicto usaba solo esos dos
   campos, asi que emparejaba por el PRIMER veredicto con ese numero y le
   pegaba al paso el motivo de otro.

   La ronda se suma a la clave: `(turn-id, round, step)`. La fija process-turn
   antes de cada llamada al LLM; los tests que no la tocan trabajan en la
   ronda 1.")

(defun current-turn-id ()
  "Turn context for facts asserted between turns (0 = pre-session setup)."
  *turn-counter*)

(defun current-batch-round ()
  "Ronda del turno en curso. 1 si nadie la ha fijado (p. ej. en un test)."
  (or *batch-round* 1))

(defun reset-turn-counter ()
  (setf *turn-counter* 0))

;;; ============================================
;;; VERDICT — PROTOCOLO.md §2.3
;;; ============================================
;;; El UNICO mensaje que dice si un paso funciono, y lo escribe solo el harness.
;;; Existe porque el ejecutor hacia (RETRACT ?f) sobre la intencion nada mas
;;; ejecutarla: el resultado se guardaba y el plan se borraba, asi que tras
;;; ejecutar no quedaba forma de saber cual de los N pasos fallo. Ese era el
;;; invariante I1 del protocolo, hoy cubierto por una prueba.

(defun step-applied-p (result)
  "T si la accion REALIZO lo pedido, NIL si fallo.

   Unica fuente de verdad: el plist que devolvio la accion, que ya consulto al
   SO. Sin heuristicas, sin segundo opinion. Un rechazo (write_file sobre
   fichero existente grande, old_string vacio, no encontrado, fichero ausente)
   es un fallo aunque el SO no opine, y se llama SIEMPRE :reason (I6: un solo
   nombre). Para un comando manda Unix: funciona o falla."
  (and result
       (not (getf result :refused))
       (not (getf result :reason))
       (let ((exit (getf result :exit-code)))
         (if exit (zerop exit) t))))

(defun round-verdict-counts (turn round)
  "Cuantos pasos de este TURNO y esta RONDA quedaron APPLIED y cuantos CANCELLED.

   Es la medida de si la ronda AVANZO. Sin ella, un turno que se pasa entero
   proponiendo lo mismo que el harness ya veto no se distingue de uno que esta
   trabajando: los dos llegan al tope de iteraciones y los dos se reportan como
   'se agoto batch_max_iterations', que dice cuanto duro y no si sirvio."
  (let ((applied 0)
        (cancelled 0))
    (dolist (d (collect-facts-of-type (collect-active-facts) "verdict"))
      (when (and (eql (data-get d :turn-id) turn)
                 (eql (data-get d :round) round))
        (case (data-get d :verdict)
          (:applied (incf applied))
          (:cancelled (incf cancelled)))))
    (values applied cancelled)))

(defun assert-step-cancelled (data why &optional contents)
  "Un paso que NO se ejecuta porque otro paso ya lo cubrio, dicho como
   veredicto.

   CONTENTS es lo que el paso anterior YA HABIA PRODUCIDO, y se adjunta al
   veredicto cuando existe. No es un adorno: es la diferencia entre un no y
   una respuesta.

   El caso real que lo motivó: un turno que lee math_utils.py en la ronda
   1, falla un test en la ronda 2, y en las rondas 4 a 7 vuelve a pedir esa
   lectura. Cada vez PREVENT-DUPLICATE-READ la cancelo con 'el archivo ya se
   leyo en este turno' y RETRACTO la intencion, sin escribir nada. Cinco
   rondas muertas: el harness tenia el fichero entero en un hecho file-read y
   le estaba diciendo que no en vez de enseñarselo. El modelo pedia la lectura
   porque no la tenia delante; un no no lo convence, un no repetido cinco veces
   solo gasta el turno.

   El contenido entra recortado por MAX-TOOL-RESULT-CHARS, el mismo tope que
   gobierna lo que un read_file normal devuelve al modelo: la cancelacion no
   puede ser mas generosa que la lectura que sustituye, o el 'atajo' seria la
   via de meter contexto sin presupuesto.

   Hermanastro de ASSERT-STEP-VERDICT, con la misma identidad de paso, y por el
   mismo motivo: el estado de la maquina es un JOIN entre el plan y el
   veredicto.

   Existia un cuarto caso que no cabia en los tres estados. Cuando una lectura
   o un edit se piden por segunda vez en el mismo turno,
   PREVENT-DUPLICATE-READ y PREVENT-DUPLICATE-EDIT retractan la intencion y no
   escribian NADA. El paso se quedaba sin veredicto, y RENDER-PLAN-PROCEDURE
   caia al fallback 'PENDING': la tarjeta decia que el trabajo seguia pendiente
   cuando ya estaba hecho, y el REASON culpaba a la poda ('puede haberse
   podado') cuando lo que hubo fue una cancelacion deliberada. Tres renders
   seguidos de la misma sesion quedaron con un GOAL ABIERTO sobre una tarea ya
   terminada.

   Con :CANCELLED el paso llega a un estado terminal y PENDING vuelve a
   significar una sola cosa: la intencion sigue viva, en la cola, va a
   ocurrir. El render NO hubo que cambiar: RENDER-PLAN-PROCEDURE ya traducía
   cualquier keyword de :verdict con (symbol-name ...), y :CANCELLED sale solo.
   Lo que faltaba era el hecho."
  (assert (harness-fact
           (fact-type "verdict")
           (timestamp (get-universal-time))
           (data (list :step (data-get data :step)
                       :action (data-get data :action)
                       :target (or (data-get data :path)
                                   (data-get data :command))
                       :verdict :cancelled
                       ;; El turno y la ronda son los de la INTENCION, no los
                       ;; del reloj: la regla dispara en cuanto entra la
                       ;; intencion, asi que un intention del turno 1 que se
                       ;; cancela en el turno 2 se sellaria con el turno
                       ;; equivocado y su tarjeta se emparejaria con el paso
                       ;; equivocado de otro turno. Mismo criterio que en
                       ;; ASSERT-STEP-VERDICT.
                       :turn-id (or (data-get data :turn-id)
                                    (current-turn-id))
                       :round (or (data-get data :round)
                                  (current-batch-round))
                       :reason why
                       ;; El contenido que el paso cancelado pretendia
                       ;; conseguir, cuando ya lo tenemos. Es lo que convierte
                       ;; 'no repetition' en 'aqui tienes, no hace falta
                       ;; pedirlo otra vez'. Recortado con el mismo tope que
                       ;; aplica a un read_file de verdad.
                       :contents (when (and (stringp contents)
                                             (plusp (length contents)))
                                    (truncate-tool-result contents))))))
  ;; Y CIERRA LA META, porque un veto tambien es una RESPUESTA.
  ;;
  ;; AUTO-CREATE-TODO-ON-ACTION tiene salience 20 y los vetos tienen 15, asi que
  ;; la meta se abre ANTES de que el veto decida. La intencion sigue viva cuando
  ;; la mira la regla de veto, y por eso crea su meta igual. Y como el veto no es
  ;; una ejecucion --no hay :applied-- esa meta no la cerraba nadie nunca.
  ;;
  ;; En una corrida real: el turno 6 ejecuto `python3 -m unittest test_all.py`
  ;; (APPLIED, y esa meta se cerro), y en las rondas 2 y 3 propuso lo mismo otra
  ;; vez. Las dos se vetaron como duplicado, las dos abrieron meta, y el .cob
  ;; final cerro con GOAL ABIERTO. 1 -- con la tarea HECHA y los tests en verde.
  ;; Como N en cero es lo que habilita DONE, esa meta era una sesion que no
  ;; podia terminar.
  ;;
  ;; NO es cerrar las metas por la fuerza: el paso CANCELLED tiene veredicto, es
  ;; terminal, y la meta existia para recordar que habia algo que hacer. Lo que
  ;; ya no esta pendiente es RECORDARLO: la razon del veto esta en la tarjeta,
  ;; con su texto, y lo que se ha losto es solo el contador.
  (close-todos-for-vetoed-step
   (data-get data :action)
   (or (data-get data :path) (data-get data :command))))

(defun todo-verb-of-action (action)
  "El verbo de meta que corresponde a la ACCION de una intencion."
  (case action
    (:read-file :read)
    (:write-file :write)
    (:edit-file :edit)
    ((:exec-command :command) :run)
    (t nil)))

(defun close-todos-for-applied-step (action target)
  "Cierra las metas in-progress que ACCION sobre TARGET acaba de dischargear.

   Solo APPLIED. Un paso FALLIDO deja la meta abierta a proposito: el usuario
   pidio una cosa, no se hizo, y GOAL ABIERTO tiene que seguir diciendo eso.
   Cerrar tambien las fallidas seria cambiar el significado del contador para
   que pareciera mejor, que es justo lo que no debe hacer un contador."
  (let ((verb (todo-verb-of-action action)))
    (when (and verb (stringp target) (plusp (length target)))
      (dolist (todo (collect-active-todos))
        (let ((task (or (get-slot-value todo 'task) "")))
          (when (and (string= (or (get-slot-value todo 'status) "") "in-progress")
                     (eql verb (todo-verb task))
                     (let ((arg (todo-argument task verb)))
                       (if (eql verb :run)
                           (goal-command-match-p arg target)
                           (goal-path-match-p arg target)))))
            (complete-todo (get-slot-value todo 'id)))))))

(defun close-todos-for-vetoed-step (action target)
  "Cierra la meta in-progress que ACCION sobre TARGET abrio y nadie cerro.

   Solo si la meta nombra lo mismo que el paso. Un veto puede venir de una
   regla que ni siquiera ha mirado el target --una lectura rancia, por ejemplo,
   que se cancela por el estado del fichero y no por lo que se pidio--, y en
   ese caso la meta sigue siendo real: el modelo todavia no tiene lo que
   pedia, y cerrar su meta seria mentir sobre lo que falta."
  (let ((verb (todo-verb-of-action action)))
    (when (and verb (stringp target) (plusp (length target)))
      (dolist (todo (collect-active-todos))
        (let* ((task (or (get-slot-value todo 'task) ""))
               (todo-verb-now (todo-verb task))
               (arg (and todo-verb-now (todo-argument task todo-verb-now))))
          (when (and (string= (or (get-slot-value todo 'status) "")
                                "in-progress")
                     (eql todo-verb-now verb)
                     (if (eql verb :run)
                         (goal-command-match-p arg target)
                         (goal-path-match-p arg target)))
            (complete-todo (get-slot-value todo 'id))))))))

(defun assert-step-verdict (data result)
  "PROTOCOLO §2.3. DATA es la data de la intencion (trae :step del PLAN);
   RESULT es lo que devolvio la accion. El veredicto lleva la identidad del
   paso para que el estado de la maquina sea un JOIN entre el plan y esto."
  (let* ((applied (step-applied-p result))
         ;; El objetivo se NORMALIZA igual que en ASSERT-BATCH-PLAN, y por el
         ;; mismo motivo: si el plan dice una cosa y el veredicto otra, la
         ;; tarjeta muestra DOS comandos para el mismo paso y el modelo no sabe
         ;; cual se ejecuto. En una corrida real el paso 3 aparecia como
         ;;
         ;;     EXEC-COMMAND.  COMMAND = python -m unittest test_math_utils
         ;;     STEP 03.  EXEC-COMMAND  cd /home/mtk/... && python -m unittest
         ;;
         ;; El plan ya lo normalizaba; el veredicto se habia quedado con el
         ;; texto crudo, o sea la mitad del arreglo.
         ;;
         ;; Solo para :exec-command: una RUTA con espacios o con un 'cd' en el
         ;; nombre no se toca. Mismo criterio que el plan.
         (target (let ((raw (or (data-get data :path)
                                (data-get data :command))))
                   (if (and (stringp raw)
                            (eq (data-get data :action) :exec-command))
                       (strip-cd-prefix raw)
                       raw)))
         (why (or (getf result :reason)
                  ;; :refused llega como T, no como texto; convertirlo a texto
                  ;; evita que el YAML muestre un T desnudo como si fuera el
                  ;; motivo. El texto del motivo real va en :reason.
                  (when (getf result :refused)
                    (format nil "write_file refused: ~A"
                            (getf result :reason)))
                  (unless applied
                    (format nil "~A devolvio exit ~A"
                            (data-get data :command) (getf result :exit-code))))))
    (assert (harness-fact
             (fact-type "verdict")
             (timestamp (get-universal-time))
             (data (append (list :step (data-get data :step)
                                :action (data-get data :action)
                                :target target
                                :verdict (if applied :applied :failed)
                                ;; El turno del VERDICTO es el turno de la
                                ;; INTENCION, no el que corre ahora. current-turn-id
                                ;; mentia: las reglas de ejecucion disparan en
                                ;; cuanto el intention entra, asi que un intention
                                ;; del turno 1 que se ejecuta en el turno 2
                                ;; (reintento, cola, cancelacion tardia) quedaba
                                ;; sellado con el turno equivocado. Como el
                                ;; numero de paso se reinicia cada turno, un
                                ;; veredicto mal sellado se emparejaba con el
                                ;; paso equivocado de otro turno, y la tarjeta
                                ;; de COBOL ensenaba un FAILED donde hubo un
                                ;; APPLIED.
                                ;;
                                ;; El JOIN es por (turn-id, round, step): la
                                ;; RONDA hace falta ademas del turno porque un
                                ;; turno tiene varias respuestas del LLM y cada
                                ;; una renumera desde 1. Con solo (turn-id, step)
                                ;; el paso 2 de una ronda se llevaba el veredicto
                                ;; del paso 2 de otra. La ronda se hereda de la
                                ;; intencion para no volver a mentir.
                                :turn-id (or (data-get data :turn-id)
                                             (current-turn-id))
                                :round (or (data-get data :round)
                                           (current-batch-round)))
                           (when why (list :reason why))))))
  ;; Y CIERRA LA META EN EL MOMENTO, no cuando alguien la busque mas tarde.
  ;;
  ;; Reconciliar metas es buscar la EVIDENCIA de que se hicieron, y esa
  ;; búsqueda tiene dos relojes encima que la pueden borrar antes de que
  ;; llegue: PRUNE-EXPIRED (fact_ttl_seconds = 120 en la configuracion de
  ;; produccion) y el cap por tipo. En una corrida real quedaron ABIERTAS
  ;; cuatro metas que ya estaban hechas:
  ;;
  ;;   todo-9  Run: ls -la ...        (se ejecuto en el turno 3, exit 0)
  ;;   todo-25 Run: ls -la ...        (otra vez, el turno 4)
  ;;   todo-30 Run: python3 -m unittest ...
  ;;
  ;; Las tres son metas de COMANDO, y no por casualidad: un comando que sale
  ;; bien no es durable --el harness lo decided asi, y con acierto, porque no
  ;; cambia nada que el modelo no vea-- asi que su unico registro durable es
  ;; el veredicto, y el veredicto tambien se capa. La meta se quedaba esperando
  ;; una prueba que el harness habia tirado.
  ;;
  ;; Como GOAL ABIERTO. N en cero es lo que habilita DONE, cuatro metas
  ;; filtradas son una sesion que no puede terminar: por mucho que este todo
  ;; hecho, el modelo ve un contador que no baja y la sesion no cierra nunca.
  ;;
  ;; Aqui no hay carrera: el veredicto se aserta en el mismo instante en que la
  ;; accion ocurre, con el codigo de salida delante. Si se aplico, se aplico.
  (when applied
    (close-todos-for-applied-step (data-get data :action) target))))


;;; ============================================
;;; Goal & Todo Templates (Backward Chaining / Planning)
;;; Inspired by Lisa's mab.lisp (Means-Ends Analysis)
;;; and OpenCode's todo and session_context_epoch
;;; ============================================

(deftemplate agent-todo ()
  (slot id)
  (slot task)
  (slot status)      ; "pending", "in-progress", "completed", "failed"
  (slot priority)    ; "high", "normal", "low"
  (slot parent-id)   ; parent goal ID for hierarchical sub-goals
  (slot turn-id)
  (slot timestamp))

(deftemplate context-epoch ()
  (slot id)
  (slot baseline-seq)
  (slot summary)
  (slot timestamp))

(defparameter *todo-counter* 0)
(defparameter *epoch-counter* 0)

(defun todo-id-number (id)
  "Numeric suffix of a todo id (\"todo-7\" -> 7), or 0 when unparsable.
   The id counter is monotonic, so this is a total order that does not depend
   on timestamp resolution."
  (if (and (stringp id) (>= (length id) 5) (string= "todo-" id :end2 5))
      (or (ignore-errors (parse-integer id :start 5)) 0)
      0))

(defun open-todo-p (todo)
  "True when TODO still needs work (pending or in-progress)."
  (let ((st (get-slot-value todo 'status)))
    (and (stringp st)
         (not (string-equal st "completed"))
         (not (string-equal st "failed")))))

(defun find-open-todo (task &optional (turn (current-turn-id)))
  "An unfinished TODO with TASK raised in TURN, if any."
  (find-if (lambda (cand)
             (and (string= (get-slot-value cand 'task) task)
                  (open-todo-p cand)
                  (or (null turn) (eql (get-slot-value cand 'turn-id) turn))))
           (mapcar #'first (retrieve (?t) (?t (agent-todo))))))

(defun add-todo (task &key (priority "normal") (parent-id nil) (status "pending"))
  "Assert a new goal/todo into Rete.

   Within one turn, an identical unfinished goal is reused instead of
   duplicated. Without this, asking for the same read twice in a turn -- which
   the batcher does routinely -- appends a second copy of the same goal until
   the per-type cap evicts the older, real ones."
  (let ((existing (and (not (string-equal status "completed"))
                       (find-open-todo task))))
    (if existing
        (get-slot-value existing 'id)
        (progn
          (incf *todo-counter*)
          (let ((id (format nil "todo-~A" *todo-counter*))
                (now (get-universal-time))
                (turn (current-turn-id)))
            (assert (agent-todo (id (identity id))
                                (task (identity task))
                                (status (identity status))
                                (priority (identity priority))
                                (parent-id (or parent-id "root"))
                                (turn-id (identity turn))
                                (timestamp (identity now))))
            id)))))

(defun find-todo-by-id (id)
  "Locate an agent-todo fact by id (plain Lisp filter; avoids DSL literal-symbol pitfalls)."
  (let ((matches (retrieve (?t) (?t (agent-todo)))))
    (find-if (lambda (f)
               (string= (get-slot-value f 'id) id))
             (mapcar #'first matches))))

(defun complete-todo (id &optional (result-summary nil))
  "Mark an agent-todo as completed."
  (let ((fact (find-todo-by-id id)))
    (when fact
      (let ((current-task (get-slot-value fact 'task))
            (priority (get-slot-value fact 'priority))
            (parent (get-slot-value fact 'parent-id))
            (turn (get-slot-value fact 'turn-id)))
        (retract fact)
        (assert (agent-todo (id (identity id))
                            (task (if result-summary
                                      (format nil "~A [Completado: ~A]" current-task result-summary)
                                      current-task))
                            (status "completed")
                            (priority (identity priority))
                            (parent-id (identity parent))
                            (turn-id (identity turn))
                            (timestamp (get-universal-time)))))
      t)))

(defun set-todo-status (id new-status)
  "Update status of an agent-todo."
  (let ((fact (find-todo-by-id id)))
    (when fact
      (let ((task (get-slot-value fact 'task))
            (priority (get-slot-value fact 'priority))
            (parent (get-slot-value fact 'parent-id))
            (turn (get-slot-value fact 'turn-id)))
        (retract fact)
        (assert (agent-todo (id (identity id))
                            (task (identity task))
                            (status (identity new-status))
                            (priority (identity priority))
                            (parent-id (identity parent))
                            (turn-id (identity turn))
                            (timestamp (get-universal-time)))))
      t)))

(defun collect-active-todos ()
  "Retrieve all agent-todos from Rete in creation order.

   Keyed on the monotonic id counter, not on timestamp: goals raised inside
   the same second share a timestamp and CL:SORT is not stable, so a
   timestamp key let equal entries land in arbitrary order -- the goal list
   came out as todo-2, todo-1, todo-4, todo-3.

   The key has to pull the id OUT of the fact. Handing the fact itself to
   TODO-ID-NUMBER made every key 0 (it only accepts a string), so the sort
   was a no-op and the list kept whatever order RETRIEVE returned."
  (let* ((matches (retrieve (?t) (?t (agent-todo))))
         (facts (mapcar #'first matches)))
    (sort (remove nil facts) #'<
          :key (lambda (f) (todo-id-number (get-slot-value f 'id))))))


(defun collect-facts-of-type (facts type)
  "Los hechos de TYPE dentro de FACTS, como datos, en orden de llegada.

   FILTER, no MAPCAR: la version anterior usaba (mapcar (lambda (f) (when ...)
   ...) facts), que devuelve una lista de la MISMA LONGITUD que FACTS con
   NILs en las posiciones que no casan. No se notaba porque casi siempre se
   filtraba despues con REMOVE-IF-NOT, pero (length ...) sobre el resultado
   contaba tambien los huecos: 2 hechos de los que 1 es file-read daban
   longitud 2, y un consumidor razonable leia que habia 2 lecturas. Un
   selector que devuelve huecos es un selector roto por la forma de la
   llamada, no por la del codigo."
  (loop for f in facts
        when (string= type (fact-type-of f))
          collect (fact-data-of f)))


(defun open-goal-tasks ()
  "Descripciones de los goals que siguen sin completarse, en orden de creacion.

   PROTOCOLO I5: un DONE del LLM es una PROPUESTA de fin de turno, no un
   hecho. Esto es contra lo que se valida antes de aceptarlo, para que el
   harness no cierre la sesion mientras quedan bananas sin recoger."
  (mapcar (lambda (f) (or (get-slot-value f 'task) "?"))
          (remove-if (lambda (f)
                       (string-equal (or (get-slot-value f 'status) "") "completed"))
                     (collect-active-todos))))

(defun create-epoch (summary &optional (baseline-seq *turn-counter*))
  "Establish a context epoch (checkpoint) consolidating history up to baseline-seq."
  (incf *epoch-counter*)
  (let ((id (format nil "epoch-~A" *epoch-counter*))
        (now (get-universal-time)))
    ;; Retract prior epoch
    (dolist (m (retrieve (?e) (?e (context-epoch))))
      (retract (first m)))
    (assert (context-epoch (id (identity id))
                           (baseline-seq (identity baseline-seq))
                           (summary (identity summary))
                           (timestamp (identity now))))
    id))

(defun current-epoch ()
  "Retrieve the active context epoch, if one exists."
  (let ((matches (retrieve (?e) (?e (context-epoch)))))
    (and matches (caar matches))))
