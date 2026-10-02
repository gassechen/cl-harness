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
  "TRUE when a fact crosses from the intraturn network to the long-term one.

   THE MEMORY DIVISION is not a log of what mattered; it is the whole input of
   the next round. Everything the model needs in order to stay coherent
   between states has to cross, and nothing else has to.

   So the test is NOT 'is this important'. It is: 'without this, can the model
   keep working'. And there are exactly three things it needs.

   ONE. WHAT IS TRUE NOW. The files: what was written, what was edited, and --
   the one that gets forgotten -- WHAT WAS READ, with its contents. Drop the
   CONTENTS of a read and the model cannot plan its next step; it has to guess
   what is in the file, and it guessed wrong four times in a row in a real run.
   A read is not durable in the sense of 'matters forever'; it is durable in the
   sense of 'this is how the model finds out what the world looks like'.

   TWO. WHAT HAPPENED TO WHAT IT ASKED FOR. batch-plan and verdict. The plan is
   what the model proposed and the verdict is what the machine answered. That
   pair is the loop closing: without it the next target has nothing to compare
   against, and the model reinvents the session from scratch every round.

   THREE. WHAT IS STILL MISSING. Not the goals themselves -- those live in
   their own slots -- but user-input, so the model knows what it was asked, and
   llm-response, so it knows what it answered.

   THE FILES CROSS WHETHER THEY WERE APPLIED OR NOT, and this reverses an older
   decision. A refusal is a fact about the world: THIS DID NOT HAPPEN, AND HERE
   IS WHY. Gating on :applied kept exactly the failures out of memory, and a
   failure is the one thing the model must not forget -- it is the whole point
   of what it is supposed to do differently next round. What it must never see
   is a refused write painted as a done one, and that is the RENDERER's job
   and not the promotion's: STATE = FAILED with REASON and REFUSED = TRUE, and
   never WROTE.

   Same for commands, and here the gate stays: only the real errors cross. A
   command that worked changed nothing the model cannot see for itself, and one
   that failed is a wall it is going to hit again.

   NOT promoted, because they are intraturn scaffolding and would be noise in
   every future context: intention (an intention that never ran is a draft, not
   a fact), batch-abort, tool-loop, and the derived loop warnings."
  (or (and (string= type "command-exec")
           (real-error-p type data))
      (member type '("file-write" "file-edit" "file-read" "batch-plan"
                     "verdict" "user-input" "llm-response")
              :test #'string=)))

(defun mem-engine-facts ()
  "The long-term memory as the .cob needs it: durable facts, in causal order.

   This is the whole input of the next round, so the order is not cosmetic.
   COLLECT-HARNESS-FACTS returns whatever RETRIEVE handed back; sorted by
   FACT-ORDER-BEFORE-P it is the order in which things happened, which is the
   order in which they have to be read."
  (with-mem-engine
    (sort (remove-if-not (lambda (f)
                           (durable-type-p (fact-type-of f) (fact-data-of f)))
                         (collect-harness-facts))
          #'fact-order-before-p :key #'fact-order-key)))

(defun promote-durable-facts ()
  "Project durable turn-engine facts into long-term memory, run the mem
   engine's rules, then re-apply per-type caps so memory stays bounded."
  ;; EN ORDEN CAUSAL, y no en el orden de recuperacion de Rete.
  ;;
  ;; COLLECT-HARNESS-FACTS devuelve lo que RETRIEVE entregue, que no es un orden:
  ;; el motor de turno lo usa tal cual y por eso la DATA DIVISION, que si pasa
  ;; por COLLECT-ACTIVE-FACTS, si sale bien. Aqui no habia nadie que pusiera las
  ;; cosas en su sitio.
  ;;
  ;; Y no es cosmetico: FACT-ORDER-KEY desempata por LISA-FACT-ID, y el id de un
  ;; hecho de la MEMORIA es el que le dio el motor de memoria al insertarlo, o
  ;; sea EL ORDEN DE PROMOCION. Promoviendo en orden arbitrario, el id deja de
  ;; medir causalidad y pasa a medir azar, y con el la MEMORY DIVISION entero:
  ;; dos escrituras del mismo segundo salian al reves, con la mas antigua al
  ;; final leyendose como el estado actual. En la prueba, 'WROTE. p2.py' caia
  ;; antes que 'WROTE. p1.py'.
  ;;
  ;; PROMOVER ES IDEMPOTENTE, y no por una guarda nuestra sino por la IDENTIDAD
  ;; DEL HECHO de LISA: dos hechos con el mismo tipo, timestamp y datos son el
  ;; mismo hecho, y la red no los guarda dos veces. Se llama una vez por RONDA y
  ;; el motor de turno no se vacia entre rondas --el user-input del turno sigue
  ;; ahi en la ronda 3, y el batch-plan y el verdict de la ronda 1 tambien--, asi
  ;; que sin esa propiedad la ronda 2 los volveria a copiar y el .cob los
  ;; renderizaria DOS VECES.
  ;;
  ;; Se apoya en el timestamp DEL HECHO para que la identidad sea la correcta: dos
  ;; escrituras del mismo fichero dentro del mismo segundo comparten reloj y se
  ;; distinguen por los datos, y dos rondas copie la misma escritura
  ;; coinciden en los tres campos y son la misma.
  (dolist (f (unless (null *mem-engine*)
               (with-turn-engine
                 (sort (copy-list (collect-harness-facts))
                       #'fact-order-before-p :key #'fact-order-key))))
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
      ;; Los topes de la memoria de largo plazo, aqui y no en BUILD-CONTEXT:
      ;; ya no es BUILD-CONTENT quien la poda, y dejar los topes ahi los
      ;; aplicaba al motor equivocado. PRUNE-LONG-TERM-MEMORY es el unico sitio
      ;; que decide que se queda, para que las dos redes no puedan discordar
      ;; sobre el mismo conjunto.
      (prune-long-term-memory))))

(defun boot-memory ()
  "Fresh-process boot: reset both engines, load persisted long-term memory
   from disk into the mem engine, and run its rules."
  (reset-turn-engine)
  (reset-mem-engine)
  (load-mem-engine)
  (with-mem-engine (run))
  t)