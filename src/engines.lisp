(in-package :cl-harness)

;;; ============================================
;;; Dual Lisa inference engines (symbolic brain)
;;; ============================================
;;; LISA runs one active engine at a time (`lisa::*active-engine*`, bound by
;;; `lisa:with-inference-engine`). Each `(lisa:make-inference-engine)` is a
;;; full `rete`: its own fact-table, rule network, contexts and focus stack.
;;; Facts asserted into one engine are INVISIBLE to the other.
;;;
;;; We keep TWO engines:
;;;   * *turn-engine*  — intra-turn working memory (tool results, loops, goals,
;;;                      conversation). It is the DEFAULT active engine, so the
;;;                      whole existing code base (assert/retract/retrieve/run)
;;;                      keeps targeting it unchanged.
;;;   * *mem-engine*   — long-term memory (durable knowledge: real errors,
;;;                      actions taken). Promoted from the turn engine at the
;;;                      end of each turn, persisted to dumps/longterm-mem.lisp,
;;;                      and re-exported into the YAML context of every turn.
;;; ============================================

(defparameter *turn-engine* nil
  "Intra-turn working memory engine (default active engine).")

(defparameter *mem-engine* nil
  "Cross-turn long-term memory engine (persisted).")

(defun install-dual-engines ()
  "Create the turn and mem inference engines and make *turn-engine* the
   default active engine. Load-time DEFRULE forms (and every runtime
   assert/retract/retrieve/run without an explicit with-* wrapper) therefore
   target the turn engine. Must run before rules.lisp is loaded."
  (setf *turn-engine* (lisa:make-inference-engine))
  (setf *mem-engine* (lisa:make-inference-engine))
  (setf lisa::*active-engine* *turn-engine*)
  ;; The active context must point into the NEW active engine, otherwise
  ;; assertions land in the boot-time engine's context.
  (setf lisa::*active-context* (lisa::initial-context *turn-engine*))
  (values *turn-engine* *mem-engine*))

(install-dual-engines)

(defmacro with-turn-engine (&body body)
  "Evaluate BODY with the intra-turn working memory engine active."
  `(lisa:with-inference-engine (*turn-engine*) ,@body))

(defmacro with-mem-engine (&body body)
  "Evaluate BODY with the long-term memory engine active."
  `(lisa:with-inference-engine (*mem-engine*) ,@body))

(defun collect-harness-facts ()
  "All harness-fact instances currently living in the ACTIVE engine."
  (mapcar #'first (retrieve (?f) (?f (harness-fact)))))

(defun reset-turn-engine ()
  "Retract all facts from the turn engine, keeping its rules intact."
  (with-turn-engine (lisa:reset)))

(defun reset-mem-engine ()
  "Retract all facts from the long-term memory engine, keeping its rules intact."
  (with-mem-engine (lisa:reset)))

;;; ============================================
;;; Long-term memory policy
;;; ============================================

(defun durable-type-p (type data)
  "TRUE when a fact is worth keeping in long-term memory:
   - command-exec facts that are REAL active errors (non-zero exit WITH
     evidence, not timed out, when conserve_errors is on);
   - file-write facts that were APPLIED (actions actually taken on the project);
   - file-edit facts that were APPLIED (the file changed after its write).

   Both file types ask for :applied, and that is the whole difference con la
   version anterior, que aceptaba CUALQUIER file-write. Ahi hay dos cosas:

   - Un EDIT que no se aplico no cambio ningun archivo, asi que no es durable.
     Sin esta puerta el unico rastro durable de un edit era... ninguno: el
     archivo se congelo en el tamano de su escritura inicial. El caso real de
     una sesion de 4 turnos: WROTE. math_utils.py BYTES = 252 con un archivo
     de 560 en disco, y los dos edits que lo llevaron ahi -- con sus chars
     replacement y anadidos -- guardados en un hecho que la memoria nunca
     miraba. La verdad ESTABA en los facts.

   - Un WRITE RECHAZado no es una escritura. WRITE-FILE con :refused t afirma
     :applied nil, y sin mirar ese campo salia en la memoria como
     'WROTE. nope.py / BYTES = ' con el tamano VACIO: la memoria decia que
     habia escrito un archivo que no escribio, y un archivo que no existe no
     se puede rechazar por no existir, asi que era el unico hueco. Falla en los
     dos pasos, no en uno."
  (or (and (string= type "command-exec")
           (real-error-p type data))
      (and (string= type "file-write")
           (data-get data :applied))
      (and (string= type "file-edit")
           (data-get data :applied))))

(defun mem-engine-facts ()
  "All harness-facts currently living in the long-term memory engine."
  (with-mem-engine
    (remove-if-not (lambda (f)
                     (durable-type-p (fact-type-of f) (fact-data-of f)))
                   (collect-harness-facts))))

(defun promote-durable-facts ()
  "Project durable turn-engine facts into long-term memory, run the mem
   engine's rules, then re-apply per-type caps so memory stays bounded."
  (dolist (f (unless (null *mem-engine*)
               (with-turn-engine (collect-harness-facts))))
    (let ((type (fact-type-of f))
          (data (fact-data-of f)))
      (when (and (durable-type-p type data) *mem-engine*)
        (with-mem-engine
          ;; El timestamp es EL DEL HECHO, no el de ahora. Con
          ;; (get-universal-time) todos los hechos promovidos en la misma
          ;; llamada quedaban con la MISMA edad, y el cap por tipo --que saca
          ;; los mas viejos-- no tenia con que ordenar: se quedaba un
          ;; subconjunto arbitrario. Con siete edits y un tope de cinco se
          ;; guardaban a0 a a3 y se perdian los tres mas recientes, que es
          ;; justo al reves de lo que un tope debe hacer.
          ;;
          ;; La edad de un hecho durable es la que tiene. Re-sellarlo al
          ;; promoverlo borraba la unica distincion que el tope y la
          ;; MEMORY DIVISION tienen para ordenar, y los dos dependian de ella
          ;; sin poder notarlo: el tope sacaba los que no debia, y los hechos
          ;; promoted en un mismo instante quedaban en orden arbitrario.
          (assert (harness-fact (fact-type (identity type))
                                (timestamp (identity (fact-timestamp-of f)))
                                (data (identity data))))))))
  (unless (null *mem-engine*)
    (with-mem-engine
      (run)
      (retract-oldest-of-type "command-exec" (max-facts-per-type))
      (retract-oldest-of-type "file-write" (max-facts-per-type))
      ;; El cap de FILE-EDIT va con los otros dos, y por la misma razon: si
      ;; DURABLE-TYPE-P lo deja pasar y no se capa, la memoria de largo plazo
      ;; crece sin tope. Ahi esta el archivo, la escritura y cada edit, y un
      ;; turno que edita 20 veces deja 20 hechos mas para siempre.
      (retract-oldest-of-type "file-edit" (max-facts-per-type)))))

(defun boot-memory ()
  "Fresh-process boot: reset both engines, load persisted long-term memory
   from disk into the mem engine, and run its rules."
  (reset-turn-engine)
  (reset-mem-engine)
  (load-mem-engine)
  (with-mem-engine (run))
  t)