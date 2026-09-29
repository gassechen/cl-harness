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
;;; Every fact carries :turn-id / :parent-id INSIDE its data plist (not a
;;; template slot) so persistence, dedup and YAML rendering need no change.
;;; Linkage is captured at ingestion, never derived by rules.

(defparameter *turn-counter* 0
  "Monotonic turn number; incremented at the start of each process-turn.")

(defun current-turn-id ()
  "Turn context for facts asserted between turns (0 = pre-session setup)."
  *turn-counter*)

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

(defun assert-step-verdict (data result)
  "PROTOCOLO §2.3. DATA es la data de la intencion (trae :step del PLAN);
   RESULT es lo que devolvio la accion. El veredicto lleva la identidad del
   paso para que el estado de la maquina sea un JOIN entre el plan y esto."
  (let* ((applied (step-applied-p result))
         (target (or (data-get data :path) (data-get data :command)))
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
                                :turn-id (current-turn-id))
                           (when why (list :reason why))))))))

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
  "Los hechos de TYPE dentro de FACTS, como datos, en orden de llegada."
  (mapcar (lambda (f)
            (when (string= type (fact-type-of f))
              (fact-data-of f)))
          facts))


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
