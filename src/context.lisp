(in-package :cl-harness)

;;; ============================================
;;; Context builder — salida medio-COBOL
;;; ============================================

;;; Defined with DEFVAR in repl.lisp (loaded later); declared here so the debug
;;; traces below compile without "undefined variable" warnings.
(defvar *debug-mode* nil)


(defun escape-yaml (s)
  "Escape a string for YAML scalar values."
  (if (or (null s) (string= s ""))
      "\"\""
      (let* ((first-char (elt s 0))
             (needs-quotes (or (find #\: s)
                              (find #\# s)
                              (find #\{ s)
                              (find #\} s)
                              (find #\[ s)
                              (find #\] s)
                              (find #\, s)
                              (find #\| s)
                              (find #\> s)
                              (find #\* s)
                              (find #\? s)
                              (find #\- s :end 1)
                              (digit-char-p first-char)
                              (string= (string-downcase s) "true")
                              (string= (string-downcase s) "false")
                              (string= (string-downcase s) "yes")
                              (string= (string-downcase s) "no")
                              (string= (string-downcase s) "null")
                              (string= (string-downcase s) "nil")
                              (find #\Newline s))))
        (if needs-quotes
            (format nil "~S" s)
            s))))

(defun yaml-value (key value)
  "Format a single YAML key-value pair."
  (etypecase value
    (null (format nil "~A: null" key))
    (string (format nil "~A: ~A" key (escape-yaml value)))
    (number (format nil "~A: ~A" key value))
    (boolean (format nil "~A: ~A" key (if value "true" "false")))))

(defun fact-slot (fact slot)
  "Read a slot value from a lisa fact instance."
  (get-slot-value fact slot))

(defun fact-to-yaml (fact)
  "Convert a harness-fact instance to a YAML mapping string."
  (let* ((type (fact-slot fact 'fact-type))
         (ts   (fact-slot fact 'timestamp))
         (data (fact-slot fact 'data))
         (head (list (format nil "- type: ~A" (escape-yaml type))
                     (format nil "  timestamp: ~A" ts)))
         (tail (when (listp data)
                 (loop for (k v) on data by #'cddr
                       for key-name = (if (symbolp k) (string-downcase (symbol-name k)) (princ-to-string k))
                       collect (format nil "  ~A: ~A"
                                       (escape-yaml key-name)
                                       (if (stringp v) (escape-yaml v) (princ-to-string v)))))))
    (format nil "~{~A~^~%~}" (append head tail))))

(defun lisa-fact-id (f)
  "LISA's per-engine monotonic insertion counter for F, or NIL if unavailable.
   Assigned in assert-fact-aux, so within a single engine it is TRUE insertion
   order -- exactly what get-universal-time cannot give us at 1-second
   resolution. Read defensively: it is another package's internal accessor."
  (ignore-errors (lisa::fact-id f)))

(defun fact-order-key (f)
  "(timestamp . insertion-order), a total order over the facts of ONE engine.

   Sorting on timestamp alone handed CL:SORT no tiebreaker, so two actions
   inside the same second came out in arbitrary order and the rendered context
   could show a command running BEFORE the file write it consumed. Every
   context query reads a single engine, so the ids are comparable; facts
   without one fall back to 0 rather than dropping out of the timeline."
  (let ((ts (or (fact-slot f 'timestamp) 0))
        (id (lisa-fact-id f)))
    (cons ts (if (integerp id) id 0))))

(defun fact-order-before-p (a b)
  "CL:SORT wants a PREDICATE, and #'<> only accepts numbers, so the (ts . id)
   pair of FACT-ORDER-KEY cannot be compared with it. Timestamp first,
   insertion order as the tiebreaker."
  (let ((ta (car a)) (tb (car b)))
    (or (< ta tb)
        (and (= ta tb) (< (cdr a) (cdr b))))))

(defun collect-active-facts ()
  "Retrieve all current harness facts from Rete, in causal order.
   Timestamp first, insertion order as tiebreaker (see FACT-ORDER-KEY)."
  (let* ((result (retrieve (?f) (?f (harness-fact))))
         (facts (mapcar #'first result)))
    (sort (remove nil facts) #'fact-order-before-p
          :key #'fact-order-key)))

;;; ============================================================
;;; Structural relevance (Phase 3)
;;; ============================================================
;;; The harness never degrades fact content: every fact that is selected
;;; is emitted with its payload INTACT. Selection is purely structural:
;;; - recency (newer timestamp = higher score)
;;; - closeness to the current turn (small |turn-id - now| = higher)
;;; - repetition (a dedup-key seen more times = more relevant; re-reading
;;;   or re-running the same thing signals it matters)
;;; - causality (facts with a parent-id = a sub-goal of another, so it sits in
;;;   a chain worth keeping). Solo goals lo llevan: ver la nota de parent-id en
;;;   facts.lisp para por que los resultados dejaron de llevarlo.
;;;
;;; This keeps Rete deciding structure (time, repetition, dependency) and
;;; leaves meaning to the LLM. Nothing here is a rule — it is a scoring
;;; function called when assembling the context block.

(defun fact-timestamp-of (f)
  (fact-slot f 'timestamp))

(defun fact-data-of (f)
  (fact-slot f 'data))

(defun fact-type-of (f)
  (fact-slot f 'fact-type))

(defun fact-raw-chars (f)
  "Estimated character footprint of a fact in the budget."
  (let* ((data (fact-data-of f))
         (raw-len (length (or (data-get data :contents)
                              (data-get data :output)
                              (data-get data :text)
                              ""))))
    (min raw-len (max-fact-payload-chars))))

(defun collect-tool-loops ()
  "Retrieve all tool-loop warning facts from Rete, newest first. Rendered as a
   top-level warnings block (like epochs/goals) so they are always visible
   regardless of the relevance budget."
  (nreverse
   (sort (loop for f in (mapcar #'first
                                 (retrieve (?l) (?l (harness-fact))))
             when (string= (fact-type-of f) "tool-loop")
               collect f)
         #'fact-order-before-p :key #'fact-order-key)))

(defun tokenize-query (text)
  "Extract lowercase alphanumeric keywords from query, filtering short tokens and common stopwords."
  (when (and text (stringp text))
    (let ((words (cl-ppcre:split "[^a-zA-Z0-9_.-]+" (string-downcase text)))
          (stopwords '("the" "and" "for" "with" "that" "this" "from" "what" "show" "tell"
                       "que" "los" "las" "del" "por" "para" "con" "una" "uno" "hay" "como"
                       "cual" "donde" "pero" "este" "esta" "bien" "hace" "quien")))
      (remove-if (lambda (w)
                   (or (< (length w) 3)
                       (member w stopwords :test #'string=)))
                 words))))

(defun keyword-match-score (keywords text)
  "Count occurrences of keywords in text."
  (if (or (null keywords) (null text) (not (stringp text)) (string= text ""))
      0
      (let ((target (string-downcase text))
            (count 0))
        (dolist (kw keywords count)
          (when (search kw target :test #'char=)
            (incf count))))))

(defun truncate-payload (text &optional (max-chars (max-fact-payload-chars)))
  "If TEXT exceeds MAX-CHARS, retain head and tail with an explicit omission notice.
   This prevents a single massive file from starving the rest of the context budget.

   El aviso NO sugiere read_file. Antes decia \"Use read_file or grep to inspect
   specifics\", y para el caso que mas lo necesitaba -- el stderr de un comando
   truncado -- read_file no tiene a que aplicarse: ese texto no vive en ningun
   fichero localizable, solo en la salida del proceso que ya termino. Un aviso
   que manda al modelo a una herramienta que no puede funcionar le cuesta un
   turno entero y no le devuelve nada. Ahora solo dice lo que es verdad: repetir
   el comando mas estrecho, o pasarlo por head/tail/grep, que si se puede
   ejecutar."
  (if (or (null text) (<= (length text) max-chars))
      text
      (let* ((head-len (floor (* max-chars 0.6)))
             (tail-len (floor (* max-chars 0.3)))
             (omitted (- (length text) head-len tail-len)))
        (format nil "~A~%... [~A characters omitted. Re-run narrower, or pipe the same command through head/tail/grep] ...~%~A"
                (subseq text 0 head-len)
                omitted
                (subseq text (- (length text) tail-len))))))


(defun cobol-block (text &optional (indent "            "))
  "Texto multi-linea como bloque COBOL: la clave, dos puntos, y el texto debajo.

   El bloque va DEBAJO de la clave y no dentro de comillas, porque un
   contenido de fichero puede traer comillas, barras invertidas y saltos de
   linea. Escaparlo a un escalar de una linea exigiria inventar una sintaxis
   de escape, y cualquier escape que no se deshaga exacto es una forma de
   corromper el dato. Ahi se lee igual que en la terminal: lo que se metio
   esta.

   Acepta cualquier cosa y la vuelve texto: los hechos sucios no deben poder
   tirar el render (ver PLAN-STEPS-OF y el resto del renderer)."
  (with-output-to-string (s)
    (format s "~%")
    (dolist (line (cl-ppcre:split "\\n" (if (stringp text) text
                                            (princ-to-string (or text "")))))
      (format s "~A~A~%" indent line))))


(defun cobol-value (s)
  "Un valor de una linea, o NIL si no hay nada que poner.

   El dato va de seguido. No se envuelve entre comillas porque un valor con
   comillas dentro -- que es la norma en contenido de codigo -- tendria que
   escaparse, y un escape se deshace peor que la comilla. La ambiguedad
   real (un valor que empieza por 'REASON = ') es teoria: estos valores los
   escribe la maquina, no el modelo.

   Acepta cualquier cosa y la vuelve texto, por la misma razon que COBOL-BLOCK."
  (when s
    (let ((text (if (stringp s) s (princ-to-string s))))
      (when (plusp (length (string-trim '(#\Space #\Tab #\Return #\Newline) text)))
        text))))


(defun count-repetitions (facts)
  "Frequency of each dedup key among FACTS -> hash key->count."
  (let ((h (make-hash-table :test #'equal)))
    (dolist (f facts)
      (let* ((type (fact-type-of f))
             (key (dedup-key type (fact-data-of f))))
        (when key
          (incf (gethash key h 0)))))
    h))


;; ============================================
;; Context Scoring Weights (Phase 3)
;; ============================================
(defconstant +score-base-recency+ 500 "Puntaje base por ser reciente.")
(defconstant +score-recency-decay+ 60 "Cuánto pierde por cada turno de distancia.")
(defconstant +score-conversation+ 300 "Garantía de preservar el hilo de diálogo.")
(defconstant +score-real-error+ 400 "Priorización de errores reales (contexto crítico).")
(defconstant +score-rep-max+ 200 "Tope de puntaje por repetición.")
(defconstant +score-rep-multiplier+ 40 "Multiplicador por cada repetición.")
(defconstant +score-keyword-path+ 450 "Bonus si el path del archivo matchea la query.")
(defconstant +score-keyword-cmd+ 400 "Bonus si el comando matchea la query.")
(defconstant +score-keyword-content+ 80 "Bonus por keyword en el contenido (max 4).")
(defconstant +score-causal+ 50 "Bonus por un parent-id real (un goal dentro de otro).")

(defun relevance-score (f now-turn rep-counts &optional (query-keywords nil))
  "Structural and semantic relevance of fact F, higher = more important."
  (let* ((data (fact-data-of f))
         (type (fact-type-of f))
         (turn (or (data-get data :turn-id) 0))
         (path (data-get data :path))
         (cmd (data-get data :command))
         (content (or (data-get data :contents) (data-get data :output) (data-get data :text) "")))
    (+ ;; 1. Turn proximity (exponential logical decay, NOT clock seconds)
       (let ((turn-dist (abs (- turn now-turn))))
         (max 0 (- +score-base-recency+ (* +score-recency-decay+ turn-dist))))
       ;; 2. Conversation guarantee
       (if (conversation-type-p type) +score-conversation+ 0)
       ;; 3. Error prioritization
       (if (and (conserve-errors-p)
                (real-error-p type data))
           +score-real-error+
           0)
       ;; 4. Frequency of repetition
       (let ((key (dedup-key type data)))
         (if key
             (min +score-rep-max+ (* +score-rep-multiplier+ (gethash key rep-counts 0)))
             0))
       ;; 5. Semantic keyword matching against user prompt
       (if query-keywords
           (+ (if (and path (plusp (keyword-match-score query-keywords path))) +score-keyword-path+ 0)
              (if (and cmd (plusp (keyword-match-score query-keywords cmd))) +score-keyword-cmd+ 0)
              (* +score-keyword-content+ (min 4 (keyword-match-score query-keywords content))))
           0)
       ;; 6. Causal dependency
       (if (data-get data :parent-id) +score-causal+ 0))))


(defun select-relevant-facts (facts now-turn
                              &key (budget (max-context-chars))
                                   (max-facts (max-context-facts))
                                   (query nil))
  "Pick the most structurally and semantically relevant FACTS that fit in BUDGET."
  (let* ((keywords (tokenize-query query))
         (rep (count-repetitions facts))
         (scored (mapcar (lambda (f)
                           (cons (relevance-score f now-turn rep keywords) f))
                         facts))
         (sorted (sort scored #'> :key #'car))
         (selected '())
         (used 0))
    (when *debug-mode*
      (format t "~&[DEBUG select-relevant-facts] total-facts=~A budget=~A max-facts=~A keywords=~A~%"
              (length facts) budget max-facts keywords))
    (dolist (cell sorted)
      (let* ((f (cdr cell))
             (score (car cell))
             (type (fact-type-of f))
             (key (dedup-key type (fact-data-of f)))
             (cost (+ (fact-raw-chars f) 200)))
        (when *debug-mode*
          (format t "~&[DEBUG select-relevant-facts] score=~6,F type=~A key=~A cost=~A selected=~A used=~A~%"
                  score type key cost (length selected) used))
        (when (and (< (length selected) max-facts)
                   (<= (+ used cost) budget))
          (push f selected)
          (incf used cost)
          (when *debug-mode*
            (format t "~&[DEBUG select-relevant-facts]   -> SELECTED~%")))
        (when *debug-mode*
          (format t "~&[DEBUG select-relevant-facts]   -> REJECTED (budget/max-facts exceeded)~%"))))
    (when *debug-mode*
      (format t "~&[DEBUG select-relevant-facts] final-selected=~A total-used=~A~%" (length selected) used))
    (nreverse selected)))



;;; ============================================================
;;; Goal reconciliation
;;; ============================================================
;;; Goals used to be closed by four Rete rules (auto-complete-todo-on-read
;;; and friends). They are disabled because their right-hand sides call
;;; set-todo-status, which runs retrieve() -- and per the note at the top of
;;; rules.lisp, a rule body must not re-enter the engine. Nothing replaced
;;; them, so every goal stayed "in-progress" for the whole session and the
;;; model saw a full todo list on the last turn of a finished task.
;;;
;;; Closing is a plain pass over the facts instead, run from
;;; build-context next to the per-type caps, which is the mechanism this
;;; file already uses for every other bound.

(defun todo-verb (task)
  "The verb of goal TASK: :read, :write, :edit or :run. NIL if unrecognised."
  (cond ((and (>= (length task) 5) (string= "Read " task :end2 5)) :read)
        ((and (>= (length task) 6) (string= "Write " task :end2 6)) :write)
        ((and (>= (length task) 5) (string= "Edit " task :end2 5)) :edit)
        ((and (>= (length task) 5) (string= "Run: " task :end2 5)) :run)
        (t nil)))

(defun todo-argument (task verb)
  "The path or command named by goal TASK, given its VERB."
  (let ((prefix (if (eql verb :write) 6 5)))
    (when (>= (length task) prefix)
      (string-trim '(#\Space) (subseq task prefix)))))

(defun goal-path-match-p (goal-arg fact-path)
  "True when FACT-PATH is the file GOAL-ARG names. Goals hold the path the
   model asked for; facts hold the resolved absolute one, so compare basenames
   and accept an exact match as well."
  (and (stringp goal-arg) (stringp fact-path) (plusp (length goal-arg))
       (or (string= goal-arg fact-path)
           (string= (path-basename goal-arg) (path-basename fact-path)))))

(defun goal-command-match-p (goal-arg fact-command)
  "True when FACT-COMMAND is the run GOAL-ARG asked for. The batcher often
   appends a target path to the command, so accept either string containing
   the other after normalisation."
  (and (stringp goal-arg) (stringp fact-command)
       (let ((a (normalize-command goal-arg))
             (b (normalize-command fact-command)))
         (and (plusp (length a))
              (or (search a b) (search b a))))))

(defun todo-evidence-types (verb)
  "Fact types that can discharge a goal with VERB."
  (case verb
    (:read (list "file-read"))
    (:write (list "file-write"))
    (:edit (list "file-edit"))
    (:run (list "command-exec"))
    (t nil)))

(defun todo-satisfied-p (todo facts)
  "True when one of FACTS already carries out TODO.

     Evidence must be at least as new as the goal, so a result left over from
     an earlier turn cannot silently close a goal that was just raised.

     A failed action does not count, for ANY verb, not just :edit. It used to
     check :applied only on the :edit branch, which was fine while only
     file-edit could be asserted in a failed state -- but that stopped being
     true: refusing a blind write and reading a missing file both now assert
     their fact with :applied nil, precisely so the rejection is visible. With
     the check still on :edit only, ADDING that visibility would have made
     every rejection close the very goal it failed to do. Hence one rule for
     every verb: the evidence has to say it was applied.

     Evidence from the SAME turn as the goal is accepted whatever its
     timestamp. The cut by time exists so a result left over from an earlier
     turn cannot silently close a goal that was just raised, and that is the
     only thing it defends against: a fact from the goal's own turn is not a
     leftover. Without this exception a repeated read inside one turn is
     cancelled by PREVENT-DUPLICATE-READ, which never runs, and the goal it
     raised can never be discharged -- the only file-read that could close it
     is the earlier one. The card then shows GOAL ABIERTO over work that is
     already done, and it re-raised the goal on every later render, which is
     what three consecutive renders of one session did. It was also clock
     dependent: with the read and the goal inside the same second the
     timestamp check passed by accident and the goal closed, so the defect
     appeared and disappeared with how fast the turn ran."
  (let* ((task (or (get-slot-value todo 'task) ""))
         (verb (todo-verb task))
         (arg (and verb (todo-argument task verb)))
         (types (and verb (todo-evidence-types verb)))
         (since (or (get-slot-value todo 'timestamp) 0))
         (turn (get-slot-value todo 'turn-id)))
    (and arg types
         (some (lambda (f)
                 (and (or (>= (fact-timestamp-of f) since)
                          (eql (data-get (fact-data-of f) :turn-id) turn))
                      (member (fact-type-of f) types :test #'string=)
                      (if (eql verb :run)
                          (goal-command-match-p arg (data-get (fact-data-of f) :command))
                          (and (data-get (fact-data-of f) :applied)
                               (goal-path-match-p arg (data-get (fact-data-of f) :path))))))
               facts))))

(defun reconcile-todos (facts)
  "Close every in-progress goal that FACTS already discharged. Returns how
   many were closed."
  (let ((closed 0))
    (dolist (todo (collect-active-todos))
      (when (and (string= (or (get-slot-value todo 'status) "") "in-progress")
                 (todo-satisfied-p todo facts))
        (complete-todo (get-slot-value todo 'id))
        (incf closed)))
    closed))

(defun prune-finished-todos ()
  "Drop the oldest COMPLETED goals once they exceed the per-type cap.

   Deliberately never touches an unfinished goal. Pruning by age alone
   retracted goals whose work was already done, so between two prompts of the
   same turn the goal list silently changed -- the model saw todo-1 and
   todo-2 disappear without any of them being reported done."
  (let* ((todos (collect-active-todos))
         (max-todos (max-facts-per-type))
         (finished (remove-if-not
                    (lambda (f) (string= (or (get-slot-value f 'status) "") "completed"))
                    todos)))
    (when (> (length finished) max-todos)
      (let ((oldest (sort (copy-list finished) #'< :key #'todo-id-number)))
        (dotimes (i (- (length oldest) max-todos))
          (retract (nth i oldest)))))))

(defun cobol-step-label (n)
  "El paso N con la numeracion de COBOL: 01, 02, ... 12.

   Un entero a secas ('3') no dice nada. El numero de dos digitos con punto es
   lo que hace que esto se lea como un programa y no como un log, y de paso da
   ancho de columna fijo, que es lo que hace el formato legible de un vistazo."
  (format nil "~2,'0D." n))

(defun plan-steps-of (facts)
  "Los batch-plan con :step entero y positivo.

   Tolera hechos basura: un batch-plan sin :step, o con un :step que no es
   entero, se SALTA. Perder el estado de los pasos buenos por un dato sucio en
   uno seria la peor forma de perderlo, porque ademas seria silenciosa."
  (remove nil
          (mapcar (lambda (d)
                    (let ((n (data-get d :step)))
                      (when (and (integerp n) (> n 0)) d)))
                  (collect-facts-of-type facts "batch-plan"))))


(defun render-plan-card (facts)
  "El PLAN como programa COBOL suelto: pasos y veredictos.

   La tarjeta suelta se usa en los tests y para inspeccionarla sola. El contexto
   completo NO la incrusta: reutiliza RENDER-PLAN-PROCEDURE y pone su propia
   cabecera, para no anidar un programa dentro de otro.

   Lo que el formato compra -- y conviene no exagerarlo:

     - Un solo dialecto. Antes el plan era COBOL y el resto YAML: dos formatos
       anidados donde el que mandaba era el viejo. Aqui todo es el mismo idioma.

     - El veredicto va PEGADO al paso que juzga (`01.` con su `STATE` debajo), y
       `STATE` es un vocabulario cerrado: APPLIED | FAILED | PENDING. En el YAML
       plano el estado y la accion caian en la misma clave.

     - `IDENTIFICATION DIVISION` lleva de quien es la tarjeta. Sin eso, un
       veredicto de un turno viejo es indistinguible de uno recien emitido, y el
       modelo reintenta cosas que ya se ejecutaron.

     - `DATA DIVISION` lleva GOAL ABIERTO, la cuenta que I5 ya hacia calcular y
       que vivia escondida dentro de open-goal-tasks.

   OJO: esto NO es una frontera de seguridad. El modelo no escribe en el
   contexto -- lo construye el harness entero --, asi que la separacion de
   divisiones es disciplina de lectura, no un cortafuegos. La validacion de lo
   que el modelo emite vive en parse-llm-batch-to-intentions.

   PENDING es lo que la hace estado de maquina y no parte de prensa: lo que no
   se ha ejecutado se ve.

   Tolera hechos basura, como el renderer anterior: un batch-plan sin :step, o
   con un :step que no es entero, se SALTA (ver PLAN-STEPS-OF)."
  (let* ((steps (plan-steps-of facts))
         (verdicts (collect-facts-of-type facts "verdict"))
         (intentions (collect-facts-of-type facts "intention")))
    (when steps
      (let ((order (sort steps #'< :key (lambda (d) (data-get d :step)))))
        (with-output-to-string (s)
          (format s "~&IDENTIFICATION DIVISION.~%")
          (format s "PROGRAM-ID. ~A.~%" (or *session-id* "SESSION"))
          (render-plan-procedure s order verdicts intentions)
          (format s "~%DATA DIVISION.~%")
          (let ((open (open-goal-tasks)))
            (format s "~%GOAL ABIERTO. ~D~%"
                    (length open))
            (dolist (g open)
              (format s "    ~A~%" g)))
          (format s "~%GOBACK.~%"))))))


(defun intention-statuses (intentions)
  "Identidad de cada paso (PLAN-STEP-IDENTITY) -> el :status de su intencion.

   Antes devolvia un hash de identidades PENDIENTES a T, y eso no reachaba para
   distinguir las dos formas de no tener veredicto: una intencion que sigue
   :pending (el paso esta en cola, no hay que tocarlo) y una intencion ya
   :done (el paso se lanzo y su veredicto no esta, no se puede repetir a
   ciegas). Son opuestas para el modelo y con un hash de booleanos salian como
   la misma cosa.

   Se guarda el STATUS, no un T, justamente para poder decir las dos."
  (let ((live (make-hash-table :test #'equal)))
    (dolist (d intentions)
      (setf (gethash (plan-step-identity d) live) (data-get d :status)))
    live))

(defun abort-reason-for-step (identity)
  "Why this step never ran: the batch-abort that killed its turn/round, or NIL.

   Un paso sin veredicto tiene dos causas que para el modelo son opuestas: se
   podo, o el turno ABORTO. Lo segundo no es una perdida, es una decision del
   harness, y se comporta distinto: lo podado se puede repetir, lo abortado hay
   que replantearlo. Decirle 'puede haberse podado' de un turno que el propio
   harness mato deja al modelo sin manera de decidir, que es justo lo que un
   motivo de PENDING tiene que evitar.

   Se empareja por la IDENTIDAD COMPLETA (step round turn), no por el numero de
   paso: el paso se renumera cada ronda, asi que el 02. de la ronda 2 y el 02.
   de la ronda 5 del mismo turno son pasos distintos. Y no basta con el turno:
   un turno abortado en su ultima ronda no invalida los pasos de las rondas
   anteriores, que si se ejecutaron y ya tienen su veredicto."
  (let ((reason nil))
    (dolist (f (mapcar #'first (retrieve (?f) (?f (harness-fact)))))
      (let ((d (fact-data-of f)))
        (when (and (string= (fact-type-of f) "batch-abort")
                   (member identity (data-get d :steps) :test #'equal))
          (setf reason (data-get d :reason)))))
    reason))


(defun cap-batch-plans ()
  "Tope de batch-plan, y los veredictos de los planes que se van SE VAN con el.

   La proteccion de RETRACT-OLDEST-OF-TYPE va en un solo sentido: un veredicto
   cuya tarjeta sigue viva no se capa. Lo que no existia era el otro. Cuando la
   TARJETA se va por su propio tope, sus veredictos se quedan, y entonces el
   veredicto huerfano se renderiza en el flujo igual que uno con tarjeta: la
   misma accion sale dos veces, como linea suelta y como 'STEP nn.', con la
   misma razon y en distinto orden. El modelo no puede saber si le contaron un
   hecho o dos, y planifica en torno a un historial que no se deja leer.

   Se hace AQUI, en el momento de retirar la tarjeta, y no despues buscando
   huerfanos: un veredicto sin batch-plan puede ser un veredicto de un plan que
   NUNCA se aserto -- una tarjeta hecha a mano, un test -- y a ese no hay que
   tocarlo. Retirar 'los que no tienen tarjeta' confunde los dos casos y se
   come respuestas que el modelo si necesita. Solo se va el veredicto CUANDO se
   va SU tarjeta, y se emparejan por PLAN-STEP-IDENTITY, la misma clave que usa
   el render, para que no se separen por un criterio distinto."
  (let* ((type "batch-plan")
         (keep (max-facts-per-type))
         (all (sort (remove-if-not (lambda (f)
                                     (string= (get-slot-value f 'fact-type) type))
                                   (mapcar #'first (retrieve (?f) (?f (harness-fact)))))
                    #'fact-order-before-p :key #'fact-order-key))
         ;; Se quedan los KEEP MAS NUEVOS: victimas son los de delante del
         ;; último, o sea (BUTLAST all KEEP). Con (NTHCDR KEEP all) seria al
         ;; revés -- se.capaba lo reciente y se guardaba lo viejo, que es justo
         ;; lo contrario de lo que hace RETRACT-OLDEST-OF-TYPE y lo que hace
         ;; inservible el tope: la tarjeta que se queda era la de un plan que el
         ;; modelo ya no puede repetir.
         (victims (when (> (length all) keep)
                    (butlast all keep))))
    (dolist (f victims)
      (let ((gone (plan-step-identity (fact-data-of f))))
        (retract f)
        (dolist (v (mapcar #'first (retrieve (?f) (?f (harness-fact)))))
          (when (and (string= (fact-type-of v) "verdict")
                     (equal gone (plan-step-identity (fact-data-of v))))
            (retract v)))))))

(defun superseded-round (turn round order)
  "La ronda que SUSTITUYO a la de este paso, o NIL si el plan sigue vigente.

   Cuarto estado, y el que mas dañaba: un plan que el PROPIO MODELO cambio por
   otro en una ronda posterior. No se podo nada, no aborto nada, y no se ejecuto:
   el modelo RESPONDIO OTRO LOTE. Antes caia en el 'puede haberse podado', que es mentira en las tres partes, y el modelo lo leia como 'perdi mi
   plan' -- con lo que su unica salida era volver a planificar. Asi se cerraba
   el bucle: cada ronda de mas dejaba un plan muerto en pantalla, y el modelo
   los veia a todos como pendientes por lo que replanificaba el mismo trabajo.

   Se deduce de los propios batch-plan del turno: si hay un paso de una ronda
   MAYOR, el plan de esta ya no es el vigente. No hace falta ningun hecho nuevo,
   y no puede quedar desfasado del estado, que es lo que le pasaria a un campo
   que se escribiera al planificar y no se limpiara al re-planificar.

   Devuelve la ronda INMEDIATAMENTE POSTERIOR, no la mayor: entre la 2 y la 4 no
   fue la 4 la que cambio el lote de la 2, sino la 3. Decir 'lo cambio por otro en
   la ronda 4' manda al modelo a mirar un lote que tampoco es el suyo."
  (let (newer)
    (dolist (d order)
      (let ((r (or (data-get d :round) 1)))
        (when (and (eql (data-get d :turn-id) turn) (> r round))
          (setf newer (if (null newer) r (min newer r))))))
    newer))

(defun pending-reason (identity pending)
  "El porque de un PENDING sin veredicto, o NIL si no hay nada que decir.

   Estos estados se renderizaban IGUALES, y no son lo mismo:

     - ANOTADO, SIN EJECUTAR: la intencion sigue :pending. El paso esta en la
       cola de Rete; todavia no ha ocurrido. Es lo mas comun en una ronda en
       vuelo, y el modelo no tiene que hacer nada: ya esta en marcha.

     - SIN VEREDICTO REGISTRADO: no hay intencion pendiente ni veredicto. O la
       intencion ya se ejecuto y su veredicto se perdio por la poda, o el plan
       se aserto a mano sin intention. NO se afirma cual de los dos: no hay
       forma de saberlo desde aqui, y un PENDING sin motivo es exactamente el
       estado que hace que el modelo no sepa si reintentar.

   La distincion es de RENDER, no de poda. La poda ya protege los veredictos de
   las tarjetas vivas (ver BUILD-CONTEXT); lo que faltaba era que cuando aun
   asi no hay veredicto, la tarjeta lo dijera.

   Y hay un TERCER caso, que va PRIMERO porque es el unico del trio del que no
   se debe reintentar igual: el turno ABORTO. No se podo nada -- el harness
   dejo de preguntar a proposito (ver ABORT-REASON-FOR-STEP). Decirle 'puede
   haberse podado' de un turno que el propio harness mato deja al modelo sin
   manera de decidir, que es justo lo que un motivo de PENDING evita."
  (let ((aborted (abort-reason-for-step identity)))
    (cond
      (aborted
       (format nil "EL TURNO ABORTO ANTES DE EJECUTAR ESTE PASO: ~A. No se perdio ni se podo; no lo reintentes igual, replantearlo."
               aborted))
      ((gethash identity pending)
       "ANOTADO, SIN EJECUTAR: la intencion sigue pendiente; este paso esta en cola")
      (t
       "SIN VEREDICTO REGISTRADO: no hay intencion pendiente ni veredicto; puede haberse podado"))))
(defun render-plan-procedure (s order verdicts intentions)
  "La PROCEDURE DIVISION de la tarjeta: los pasos y su veredicto.

   Vive suelto para que el programa COMPLETO del contexto la reutilice. La
   tarjeta sola es un programa entero (IDENTIFICATION...GOBACK), pero cuando
   va incrustada seria un programa dentro de otro: dos cabeceras y dos
   GOBACK, y no se sabria de quien es cada parte.

   Por eso no lleva PROGRAM-ID ni GOBACK: los pone quien envuelve.

   Los VERDICTS y las INTENTIONS se pasan como argumento, no se vuelven a leer
   de la base de hechos. Recolectarlos aqui otra vez haria que la tarjeta leyera
   el estado global por su cuenta, y si el contexto se esta construyendo con
   una lista filtrada --GROUPED-CONTEXT-STRING construye con SELECTED-- la
   tarjeta se saltaria el filtro y su PENDING daria un motivo de un paso que
   quizas ni esta en este programa.

   PENDING lleva siempre su REASON. Ver PENDING-REASON: un PENDING a secas
   mezcla 'en cola' con 'veredicto perdido', y el modelo no puede decidir."
  (format s "~%PROCEDURE DIVISION.~%")
  (let ((statuses (intention-statuses intentions)))
    (dolist (d order)
      (let* ((n (data-get d :step))
             (turn (data-get d :turn-id))
             ;; La ronda del plan. Normalizada a 1: un plan sin :round (hecho a
             ;; mano, o de una version anterior) es la ronda 1, no "otra".
             (round (or (data-get d :round) 1))
             ;; EMPAREJAR POR (turn-id, round, step), NO POR step SOLO.
             ;;
             ;; El numero de paso se reinicia cada TURNO y cada RONDA. Dos casos
             ;; reales, no teoricos:
             ;;
             ;;  - Turnos: 02. del turno 1 y 02. del turno 2 son el mismo numero
             ;;    y turnos distintos. Emparejando solo por :step, el paso 2 del
             ;;    turno 2 se llevaba el veredicto del turno 1.
             ;;
             ;;  - Rondas: un turno tiene varias respuestas del LLM y cada una
             ;;    renumera desde 1. Ese mismo turno puede tener un 01. WRITE, un
             ;;    01. EXEC y un 01. READ, y con solo (turn-id, step) los tres
             ;;    cogian el PRIMER veredicto step 1 -- el del write -- y el EXEC
             ;;    salia APPLIED aunque hubiera fallado.
             ;;
             ;; :turn-id o :round a NIL solo emparejan entre si (via la
             ;; normalizacion), para no cruzarlos con los que si los tienen.
             (v (find-if (lambda (vd)
                           (and (eql (data-get vd :step) n)
                                (eql (data-get vd :turn-id) turn)
                                (eql (or (data-get vd :round) 1) round)))
                         verdicts))
             (verdict (data-get v :verdict))
             ;; OJO: :APPLIED y :FAILED son keywords y los dos son truthy. Hay
             ;; que COMPARAR para decidir cual es cual; con un test de verdad,
             ;; todo saldria 'applied'.
             (state (if v
                        (string-downcase
                          (symbol-name (if (keywordp verdict) verdict :failed)))
                        "pending"))
             (identity (plan-step-identity d)))
        ;; El turno va en la etiqueta, porque la numeracion de COBOL se REINICIA
        ;; por turno y sola no distingue 02. del turno 1 de 02. del turno 2. La
        ;; RONDA se anade solo cuando es >1 -- en el caso comun de una sola
        ;; respuesta por turno no hay nada que desambiguar y la etiqueta no se
        ;; ensucia.
        (format s "    ~A  T~A~@[ R~A~] ~A  ~A~%"
                (cobol-step-label n)
                (or turn "-")
                (when (> round 1) round)
                (cobol-verb (or (data-get d :action) "?"))
                (or (data-get d :target) "-"))
        ;; PENDING cuando no hay veredicto: lo que el modelo pidio y todavia no
        ;; ha ocurrido. NO se omite la linea. Omitirla hacia que 'no ejecutado'
        ;; fuera indistinguible de 'no me lo dijeron', y el modelo no puede
        ;; reintentar lo que cree que no existe.
        (format s "        STATE = ~A~%"
                (string-upcase state))
        ;; El REASON sale del veredicto si lo hay, y si no, de PENDING-REASON.
        ;; Sin esto ultimo, los PENDING serian todos iguales y el modelo no
        ;; podria distinguir un paso en cola de uno cuyo veredicto se perdio.
        (let ((why (or (data-get v :reason)
                       (unless v
                         ;; ORDEN: aborted > sustituido > en cola > podado.
                         ;; PENDING-REASON resuelve los tres ultimos, pero su
                         ;; ultima rama es un 'puede haberse podado' que SIEMPRE
                         ;; devuelve algo: consultarlo antes dejaria al estado
                         ;; SUSTITUIDO sin poder aparecer nunca.
                         (let ((aborted (abort-reason-for-step identity))
                               (newer (superseded-round turn round order)))
                           (cond
                             (aborted
                              (format nil "EL TURNO ABORTO ANTES DE EJECUTAR ESTE PASO: ~A. No se perdio ni se podo; no lo reintentes igual, replantearlo."
                                      aborted))
                             (newer
                              (format nil "SUSTITUIDO, NO PENDIENTE: pediste este lote en la ronda ~D y tu siguiente respuesta lo cambio por otro en la ronda ~D. No se ejecuto, pero tampoco se perdio: no lo reintentes, el trabajo vigente es el del lote de la ronda ~D."
                                      round newer newer))
                             ((eql (gethash identity statuses) :pending)
                              "ANOTADO, SIN EJECUTAR: la intencion sigue pendiente; este paso esta en cola")
                             ((gethash identity statuses)
                              "EJECUTADO SIN VEREDICTO: la intencion se marco hecha pero no hay veredicto, asi que no se puede saber si ocurrio. No lo repitas igual: verificalo o replantealo")
                             (t
                              "LOTE VIGENTE, SIN EJECUTAR: este paso es de tu ultima respuesta y todavia no se ha ejecutado; no hace falta repetirlo")))))))
          (when why
            (format s "        REASON = ~A~%" why)))
        ;; El contenido que un paso cancelado YA TENIA. Sin esto el veredicto
        ;; dice 'no lo repitas' y el modelo, al no ver el fichero, lo repite:
        ;; el veto sin el contenido es un callejon sin salida que el modelo no
        ;; puede resolver solo. Con el, la cancelacion se lee como una entrega.
        (let ((contents (data-get v :contents)))
          (when (and (stringp contents) (plusp (length contents)))
            (format s "        CONTENTS =~%~A~%" contents)))))))




(defun cobol-verb (action)
  "La accion tal cual la escribio el modelo, enmayusculada.

   Sin traducir. READ-FILE se queda READ-FILE: el mismo token que el modelo
   escribio, para que pueda comparar lo que pidio con lo que se ejecuto. Una
   tabla de sinonimos aqui seria otra fuente de verdad mas que puede
   contradecir al modelo, y su memoria no tiene por que pasar por ella."
  (string-upcase (princ-to-string action)))



(defun render-fact (s type data)
  "Un hecho como su bloque COBOL. Los internos no se renderizan: no son
   contexto, son andamiaje.

   El verbo va en la etiqueta, no en una clave. 'read:', 'wrote:',
   'read_failed:' eran tres claves distintas para un solo tipo de hecho
   (file-read), y el que decidia cual era el que decidia el resultado: un
   file-read con :applied nil se imprimia como read_failed:, o sea que el
   ESTADO viajaba dentro del NOMBRE. Ahora el nombre es siempre el mismo
   (READ-FILE) y el estado va en STATE, que es lo unico que se lee."
  (cond ((string= type "user-input")
         (format s "    USER.~%")
         (format s "        TEXT = ~A~%" (cobol-value (or (data-get data :text) ""))))
        ((string= type "llm-response")
         (format s "    ASSISTANT.~%")
         (format s "        TEXT = ~A~%" (cobol-value (or (data-get data :text) ""))))
        ((string= type "command-exec")
         (format s "    EXEC-COMMAND.~%")
         (format s "        COMMAND = ~A~%" (cobol-value (or (data-get data :command) "")))
         (format s "        EXIT-CODE = ~A~%" (or (data-get data :exit-code) ""))
         (format s "        OUTPUT = ~A~%"
                 (cobol-block (truncate-payload (or (data-get data :output) "")))))
        ((string= type "file-read")
         (format s "    READ-FILE  ~A~%" (cobol-value (or (data-get data :path) "")))
         ;; I6: el estado viaja en STATE, no en el nombre del bloque. Con el
         ;; motivo dentro de :contents se imprimia el texto del error
         ;; como si fuera el contenido del fichero, y el modelo creia
         ;; haber leido algo.
         (format s "        STATE = ~A~%" (if (data-get data :applied) "APPLIED" "FAILED"))
         (if (data-get data :applied)
             (format s "        CONTENTS = ~A~%"
                     (cobol-block (truncate-payload (or (data-get data :contents) ""))))
             (when (data-get data :reason)
               (format s "        REASON = ~A~%" (data-get data :reason)))))
        ((string= type "file-write")
         (format s "    WRITE-FILE  ~A~%" (cobol-value (or (data-get data :path) "")))
         (format s "        STATE = ~A~%" (if (data-get data :applied) "APPLIED" "FAILED"))
         (when (data-get data :bytes)
           (format s "        BYTES = ~A~%" (data-get data :bytes)))
         (when (data-get data :reason)
           (format s "        REASON = ~A~%" (data-get data :reason)))
         (when (data-get data :refused)
           (format s "        REFUSED = TRUE~%")))
        ((string= type "file-edit")
         (format s "    EDIT-FILE  ~A~%" (cobol-value (or (data-get data :path) "")))
         (format s "        STATE = ~A~%" (if (data-get data :applied) "APPLIED" "FAILED"))
         ;; I6: una sola clave, :reason. Leer :reason y :error a la vez era
         ;; el sintoma de que el rechazo no tenia nombre unico.
         (let ((why (data-get data :reason)))
           (when why
             (format s "        REASON = ~A~%" why)))
         (when (data-get data :matches)
           (format s "        MATCHES = ~A~%" (data-get data :matches))))
        ;; batch-plan NO se renderiza aqui: vive en PROCEDURE DIVISION, con su
        ;; veredicto al lado (ver RENDER-PLAN-CARD). Los hechos de control
        ;; tampoco: son andamiaje, no contexto.
        ((or (string= type "tool-loop")
             (string= type "plan-done")
             (string= type "batch-abort")
             (string= type "intention")
             (string= type "batch-plan"))
         nil)
        ;; Un veredicto HUERFANO -- sin batch-plan que lo muestre en la
        ;; tarjeta -- si se renderiza. Si el paso existe, la tarjeta ya lleva
        ;; el STATE al lado del paso, y repetirlo aqui seria la misma verdad
        ;; dos veces en el mismo turno. Si el paso se fue (el cap de
        ;; batch-plan retrae los viejos y deja los verdicts), este es el unico
        ;; sitio donde el modelo se entera de que fallo.
        ((string= type "verdict")
         (format s "    STEP ~A  ~A  ~A~%"
                 (cobol-step-label (or (data-get data :step) 0))
                 (cobol-verb (or (data-get data :action) "?"))
                 (or (data-get data :target) "-"))
         (format s "        STATE = ~A~%"
                 (string-upcase (symbol-name (or (data-get data :verdict) :failed))))
         (let ((why (data-get data :reason)))
           (when why
             (format s "        REASON = ~A~%" why))))
        (t nil)))


(defun plan-step-identity (data)
  "La identidad de un paso: (step, round, turn-id). Es la MISMA que empareja la
   tarjeta con su veredicto, y se comparte con el render y con la poda para que
   las dos cosas no se separen por un cambio de criterio.

   CONS con nil al final, no LIST: en este paquete LIST es la de LISA y
   devuelve un patron. Y el nil no es cosmetico: (cons a (cons b c)) es una
   lista PUNTEADA, y una lista punteada no se puede comparar con <."
  (cons (data-get data :step)
        (cons (or (data-get data :round) 1)
              (cons (data-get data :turn-id) nil))))

(defun plan-card-order-key (d)
  "Clave de orden de las tarjetas de la PROCEDURE: la LINEA DE TIEMPO, que es
   (turno, ronda, paso).

   El paso va el ULTIMO a proposito, aunque parezca lo contrario: un numero de
   paso solo significa algo DENTRO de su turno, porque la numeracion de COBOL
   se reinicia en cada uno. Si el paso fuese la clave principal, las tarjetas
   saldrian agrupadas por numero a traves de los turnos --
   01. T1 / 01. T2 / 01. T3 / 02. T1 -- y el modelo leeria un solo turno con
   numeros repetidos en vez de tres turnos con su propia numeracion.

   Antes solo se ordenaba por :step, y ademas el orden le llegaba ya revuelto:
   SELECT-RELEVANT-FACTS ordena por puntuacion, y la puntuacion DECAE con la
   distancia al turno actual, asi que el turno nuevo llegaba primero. Como el
   SORT de SBCL es estable, cada numero de paso conservaba ese ranking y las
   tarjetas del turno anterior salian detras de las del actual. En una corrida
   real el paso 3 de la ronda 5 salia antes que el paso 2 de la ronda 6. No era
   un barajado aleatorio: era un error DETERMINISTA que ademas parecian
   intencionado, y el modelo no tenia forma de saber que faltaba tiempo.

   La compara PLAN-CARD-BEFORE-P, no #'<: < solo admite reales y una clave de
   mas de un campo es una lista."
  (cons (or (data-get d :turn-id) 0)
        (cons (or (data-get d :round) 1)
              (cons (or (data-get d :step) 0) nil))))

(defun plan-card-before-p (a b)
  "CL:SORT con #:CUR lesser... no: con #'< solo admite REALES, y una lista no es
   un real, asi que la clave no puede ser una lista: hay que comparar campo a
   campo. Por eso PLAN-CARD-ORDER-KEY no se pasa directamente a #'<."
  (let ((ka (plan-card-order-key a))
        (kb (plan-card-order-key b)))
    (or (< (first ka) (first kb))
        (and (= (first ka) (first kb))
             (or (< (second ka) (second kb))
                 (and (= (second ka) (second kb))
                      (< (third ka) (third kb))))))))

(defun live-plan-step-identities (&optional (facts (collect-active-facts)))
  "Las identidades de las tarjetas batch-plan que siguen en pantalla.
   COLLECT-FACTS-OF-TYPE devuelve datos, no hechos: no hay que volver a
   developed el 'data' encima."
  (let ((live (make-hash-table :test #'equal)))
    (dolist (d (collect-facts-of-type facts "batch-plan"))
      (setf (gethash (plan-step-identity d) live) t))
    live))

(defun prune-long-term-memory ()
  "Los topes de la memoria de largo plazo, aplicados al motor de memoria.

   Va aqui, y no en BUILD-CONTENT, porque la memoria de largo plazo ya no se
   poda como efecto secundario de construir el .cob: se poda porque se poda.
   Y va aqui, y no repartido, porque la tarjeta y su veredicto tienen que caer
   por la MISMA ventana y en ese orden, y eso solo se puede garantizar si los
   dos topes se aplican juntos y sobre el mismo conjunto. Con un tope en cada
   sitio, cada uno recorta por su cuenta y se desalinean solos -- que es como
   una tarjeta se queda sin veredicto y sale con STATE = PENDING."
  (retract-oldest-of-type "user-input" (max-facts-per-type))
  (retract-oldest-of-type "llm-response" (max-facts-per-type))
  (retract-oldest-of-type "command-exec" (max-facts-per-type))
  (retract-oldest-of-type "file-read" (max-facts-per-type))
  (retract-oldest-of-type "file-write" (max-facts-per-type))
  (retract-oldest-of-type "file-edit" (max-facts-per-type))
  ;; El protocolo tambien crece: un batch-plan y un verdict POR PASO.
  (cap-batch-plans)
  ;; Y al reves: un veredicto CUYA TARJETA SE FUE no puede quedarse. La proteccion
  ;; de abajo solo va en un sentido -- protege al veredicto cuya tarjeta sigue
  ;; viva -- y por eso un veredicto sin tarjeta sobrevivia a la poda de la tarjeta.
  ;;
  ;; Un veredicto huerfano se renderiza en el flujo como 'STEP nn.', o sea como
  ;; si fuera un paso que el modelo llego a pedir, cuando su plan hace rato que no
  ;; esta en pantalla. Y se duplica: la linea suelta del hecho (READ-FILE, FAILED,
  ;; REASON = ...) y la tarjeta (STEP 01. READ-FILE, FAILED, REASON = ...) salen
  ;; las dos, con la misma razon y en distinto orden. El modelo no puede saber si
  ;; le contaron dos veces el mismo hecho o dos hechos, y planifica en torno a un
  ;; historial que no se deja leer.
  (let ((live (live-plan-step-identities (collect-harness-facts))))
    (retract-oldest-of-type "verdict" (max-facts-per-type)
                            (lambda (f)
                              (gethash (plan-step-identity (fact-data-of f)) live)))))

(defun grouped-context-string (facts now-turn &optional memory)
  "Los hechos del turno, agrupados por :turn-id, como PROGRAMA COBOL.

   Antes esto era un bloque YAML con claves 'context:', 'prior_turns:',
   'current_turn:', y la tarjeta COBOL incrustada dentro -- dos formatos
   anidados, con la tarjeta de Boundaries en medio. El que mandaba era el
   viejo, y el modelo leia un 'read:' junto a un '01. WRITE-FILE' sin saber
   que eran el mismo idioma.

   Ahora es un unico programa. La jerarquia la ponen las divisiones y la
   sangria, no los dos puntos; y el dato va de seguido, sin comillas ni
   escapes, porque un escape que no se deshace exacto es una forma de
   corromper el contenido de un fichero."
  (let ((groups (make-hash-table :test #'eql))
        (epoch (current-epoch))
        ;; GOAL ABIERTO son los que siguen SIN completar. collect-active-todos
        ;; devuelve tambien los completados que aun no se han retraido (el cap
        ;; los poda por cantidad, no por estado), y contarlos como abiertos
        ;; pintaba un goal cerrado bajo el rotulo 'ABIERTO'. La cuenta tiene
        ;; que salir de la misma definicion que usa el protocolo para decidir
        ;; si cierra la sesion.
        (todos (remove-if (lambda (td)
                            (string-equal (or (get-slot-value td 'status) "") "completed"))
                          (collect-active-todos)))
        (plan-keys (make-hash-table :test #'equal)))
    (dolist (d (plan-steps-of facts))
      ;; Misma clave que usa el render para emparejar el veredicto: si las dos
      ;; cosas calcularan la identidad por su cuenta, un cambio de criterio en
      ;; una separaria de la otra en silencio.
      (setf (gethash (plan-step-identity d) plan-keys)
            t))
    (dolist (f facts)
      (let* ((type (fact-type-of f))
             (data (fact-data-of f))
             (turn (or (data-get data :turn-id) 0)))
        ;; Un veredicto cuyo paso YA esta en la tarjeta no se repite en el
        ;; flujo: la tarjeta lo lleva al lado del paso, y repetirlo seria la
        ;; misma verdad dos veces. El que no tiene paso -- porque el cap de
        ;; batch-plan lo retiro -- se cae al flujo para no perderse.
        (when (or (not (string= type "verdict"))
                  (not (gethash (plan-step-identity data) plan-keys)))
          ;; Carry the full ORDER KEY, not the bare timestamp: two events in the
          ;; same second used to be re-sorted here with no tiebreaker, which is how
          ;; a command still came out above the write it read.
          ;; NOTE: built with CONS, not LIST -- in this package LIST is LISA's, and
          ;; calling it here silently produced a pattern instead of a list.
          (push (cons (fact-order-key f) (cons type data)) (gethash turn groups)))))
    (maphash (lambda (turn cells)
               (setf (gethash turn groups)
                     (sort cells #'fact-order-before-p :key #'car)))
             groups)

    (with-output-to-string (s)
      (format s "IDENTIFICATION DIVISION.~%")
      (format s "PROGRAM-ID. ~A.~%" (or *session-id* "SESSION"))

      ;; El epoch, si lo hay, va aqui y no en su propio bloque: es parte de
      ;; la identidad del programa, no una seccion mas.
      (when epoch
        (format s "EPOCH. ~A~%" (get-slot-value epoch 'id))
        (format s "BASELINE-TURN. ~A~%" (get-slot-value epoch 'baseline-seq))
        (when (get-slot-value epoch 'summary)
          (format s "SUMMARY. ~A~%" (get-slot-value epoch 'summary))))

      ;; PROCEDURE DIVISION va SIEMPRE, aunque no haya plan. Un programa sin
      ;; division de procedimiento no es un programa vacio: es un programa
      ;; MAL FORMADO, y el modelo que lee 'no hay PROCEDURE' aprende que el
      ;; formato cambia de forma. Una seccion vacia se lee: no me pidio nada.
      (let* ((ordered (sort (plan-steps-of facts) #'plan-card-before-p))
             (verdicts (collect-facts-of-type facts "verdict"))
             (intentions (collect-facts-of-type facts "intention")))
        (render-plan-procedure s ordered verdicts intentions))

      ;; DATA DIVISION con los goals abiertos. La tarjeta suelta lleva
      ;; 'GOAL ABIERTO. N' con las tareas peladas; el programa completo lleva
      ;; ademas STATUS, PRIORITY y PARENT, que es lo que la tarjeta no cabe.
      (format s "~%DATA DIVISION.~%")
      (format s "~%GOAL ABIERTO. ~D~%" (length todos))
      (dolist (td todos)
        (let ((id (get-slot-value td 'id))
              (task (get-slot-value td 'task))
              (status (get-slot-value td 'status))
              (priority (get-slot-value td 'priority))
              (parent (get-slot-value td 'parent-id)))
          (format s "    ~A  ~A~%" (or id "-") (cobol-value task))
          (when status
            (format s "        STATUS = ~A~%" (cobol-value status)))
          (when priority
            (format s "        PRIORITY = ~A~%" priority))
          (unless (or (null parent) (string= parent "root"))
            (format s "        PARENT = ~A~%" parent))))

      ;; Los avisos. En YAML era 'warnings:' con una lista de '- loop: ...'.
      (let ((loops (collect-tool-loops)))
        (when loops
          (format s "~%WARNING DIVISION.~%")
          (dolist (l loops)
            (let* ((d (fact-data-of l))
                   (family (or (data-get d :family) ""))
                   (count (or (data-get d :count) 3)))
              (format s "    LOOP. ~A failing commands are the same retried attempt (family ~S).~%"
                      count family)
              (format s "        STOP re-running variants; change approach.~%")))))

      ;; Los turnos, del mas viejo al actual. La agrupacion por :turn-id se
      ;; mantiene porque es informacion, no formato: saber que dos hechos son
      ;; del mismo turno es parte del estado.
      (dolist (turn (sort (loop for k being the hash-keys of groups collect k) #'<))
        (format s "~%TURN ~A.~%" turn)
        (dolist (cell (gethash turn groups))
          (render-fact s (cadr cell) (cddr cell))))

      ;; La memoria va DENTRO del programa, antes del cierre. Fuera del
      ;; GOBACK no seria parte de este turno, seria un anexo.
      (when (and memory (plusp (length memory)))
        (format s "~A" memory))

      (format s "~%GOBACK.~%"))))


(defun mem-context-block ()
  "La MEMORY DIVISION: lo durable de la *mem-engine*, dentro del programa.

   Antes se CONCATENABA fuera del programa, con su propia clave
   'long_term_memory:'. Eso tenia dos problemas: el programa cerraba con
   GOBACK y la memoria aparecia DESPUES del cierre, o sea fuera del programa;
   y un bloque YAML colgando de un programa COBOL, que es exactamente el
   mezcladito que este cambio viene a quitar.

   Es independiente de la memoria de turno y de su TTL/dedup/cap: eso lo hace
   la mem-engine antes de llegar aqui.

   RECOGE EL TURNO EN CURSO TAMBIEN, y no solo los anteriores. La memoria es
   la proyeccion durable, y promover antes de construir es lo que hace que
   tenga lo que acaba de pasar; si esperase al final del turno, el modelo
   estaria leyendo el .cob del turno anterior mientras este ocurre.

   Que un hecho salga tambien en el bloque de su turno no es una copia por
   descuido: son dos preguntas distintas. El bloque de turno dice QUE PASO EN
   ESE TURNO --con STATE, REASON y REFUSED--, y la memoria dice COMO ESTA EL
   MUNDO --con BYTES, REPLACED y NEW, que son cosas que el bloque de turno no
   lleva--. Lo que no puede pasar es que las dos digan cosas DISTINTAS del
   mismo hecho, y no pueden: las dos leen la misma red."
  (let ((facts (mem-engine-facts)))
    (when facts
      (with-output-to-string (s)
        (format s "~%MEMORY DIVISION.~%")
        ;; EN ORDEN CRONOLOGICO, como la DATA DIVISION de arriba.
        ;;
        ;; Antes era (SORT ... fact-order-before-p :key fact-order-key) seguido
        ;; de un NREVERSE, o sea descendente: la memoria durable se leia al
        ;; reves que los turnos. Y sus entradas no llevan etiqueta de turno, asi
        ;; que el modelo no tenia forma de saber cual era la vigente. En una
        ;; corrida real la ultima linea que se leia era
        ;;
        ;;     WROTE. math_utils.py
        ;;         BYTES = 294
        ;;
        ;; sobre un fichero de 587 bytes: el RESUMEN MAS ANTIGUO presented como
        ;; si fuera el estado actual. Sin orden ni turno, tres afirmaciones
        ;; paralelas se leen como la misma, y la que gana es la que esta ultima.
        (dolist (f (sort facts #'fact-order-before-p :key #'fact-order-key))
          (let* ((type (fact-type-of f))
                 (data (fact-data-of f)))
            (cond ((string= type "command-exec")
                   ;; Un comando que salio mal es lo que hay que recordar: un
                   ;; comando que fue bien no merece linea. La asimetria es
                   ;; deliberada, y por eso esto no es ni 'command:' ni 'exec:'.
                   (format s "    ERROR. ~A~%"
                           (cobol-value (or (data-get data :command) "")))
                   (format s "        EXIT-CODE = ~A~%"
                           (or (data-get data :exit-code) ""))
                   (format s "        EVIDENCE = ~A~%"
                           (cobol-block (truncate-payload
                                         (or (data-get data :output) "")))))
;; LO QUE EL MODELO YA LEYO, y por que ahora viaja aqui.
                  ;;
                  ;; FILE-READ ya era durable (ENGINES.LISP lo lista) pero esta
                  ;; division no lo imprimia: de una lectura solo salia su
                  ;; carpeta en la DATA DIVISION, y como los hechos se podan con
                  ;; MAX-FACTS-PER-TYPE, al turno siguiente el modelo se
                  ;; encontraba con 'BYTES = 8951' y nada mas. Veia el tamano de
                  ;; un fichero que no tenia delante.
                  ;;
                  ;; Con eso el modelo tiene que releer, y releer cuesta una
                  ;; llamada entera para recibir algo que el harness ya tiene en
                  ;; la mano. Y peor: entre la lectura y la siguiente respuesta el
                  ;; contenido vuelve a quedarse fuera, asi que el turno N+1
                  ;; vuelve a empezar sin el fichero. Medido en cuatro corridas:
                  ;; el modelo escribio, leyo, vio el error exacto de una linea,
                  ;; y aun asi reescribio el fichero entero porque en el momento
                  ;; de arreglarlo ya no tenia el contenido delante.
                  ;;
                  ;; La razon de fondo no es de economia sino de geometria del
                  ;; contexto: con el contenido a la vista, el modelo puede
                  ;; ENFRENTAR lo que tiene con lo que le pasa --una linea con un
                  ;; corchete de mas, un assert que no cuadra-- y decidir el
                  ;; cambio minimo. Sin el, solo le queda reemitir, que es
                  ;; precisamente lo que hacen bien cuando les funciona.
                  ;;
                  ;; SIN TRUNCAR, a proposito. Un fichero a medias --cortado por
                  ;; el presupuesto a la mitad de una linea-- es peor que no
                  ;; verlo: el modelo razona sobre codigo que no existe y
                  ;; Arregla un fichero que no es el suyo. Si el presupuesto
                  ;; aprieta, el recorte tiene que caer entero, fuera del bloque,
                  ;; no por dentro.
                  ((string= type "file-read")
                   (if (data-get data :applied)
                       (progn
                         (format s "    READ. ~A~%" (cobol-value (or (data-get data :path) "")))
                         (format s "        CONTENTS =~%~A~%"
                                 (cobol-block (or (data-get data :contents) ""))))
                       (progn
                         (format s "    READ-FAILED. ~A~%" (cobol-value (or (data-get data :path) "")))
                         (when (data-get data :reason)
                           (format s "        REASON = ~A~%" (data-get data :reason))))))
                  ((string= type "file-write")
                  (if (data-get data :applied)
                      (progn
                        (format s "    WROTE. ~A~%" (cobol-value (or (data-get data :path) "")))
                        (format s "        BYTES = ~A~%" (or (data-get data :bytes) "")))
                      ;; Un rechazo que sale como WROTE es la MENTIRA mas
                      ;; caro que puede decir esta division: dice que el archivo
                      ;; existe con un tamano que no tiene. El motivo sale con
                      ;; el, porque un rechazo sin motivo es un rechazo que el
                      ;; modelo no puede evitar repetir.
                      (progn
                        (format s "    REFUSED. ~A~%" (cobol-value (or (data-get data :path) "")))
                        (format s "        REQUEST = WRITE-FILE~%")
                        (when (data-get data :reason)
                          (format s "        REASON = ~A~%" (data-get data :reason))))))
                 ((string= type "file-edit")
                  (if (data-get data :applied)
                      (progn
                        (format s "    EDITED. ~A~%" (cobol-value (or (data-get data :path) "")))
                        ;; SIN BYTES, A PROPOSITO. El edit no lleva tamano, y la
                        ;; tentacion de computarlo --leer el archivo, restar lo
                        ;; viejo, sumar lo nuevo-- seria inventar un dato que el
                        ;; harness no midio. Lo que se sabe de verdad es cuanto
                        ;; salio y cuanto entro, y con eso el modelo puede
                        ;; reconstruir la diferencia. La escritura que este edit
                        ;; modifica sigue en la division con SU tamano.
                        (format s "        REPLACED = ~A~%"
                                (or (data-get data :replaced-chars) ""))
                        (format s "        NEW = ~A~%"
                                (or (data-get data :new-chars) "")))
                      ;; El mismo motivo que en la escritura: un edit que no se
                      ;; aplico no cambio el archivo, y decirlo como EDITED es
                      ;; inventarse un cambio.
                      (progn
                        (format s "    REFUSED. ~A~%" (cobol-value (or (data-get data :path) "")))
                        (format s "        REQUEST = EDIT-FILE~%")
                        (when (data-get data :reason)
                          (format s "        REASON = ~A~%" (data-get data :reason))))))
                  (t nil))))))))


(defun build-context (user-message)
  "El contexto completo como programa COBOL: poda, selecciona y renderiza
   los hechos mas relevantes estructural y semanticamente."
  (run)
  ;; PROMUEVE Y LUEGO LEE. El .cob se genera desde la red de largo plazo, y una
  ;; red de largo plazo que no tiene lo que acaba de pasar no sabe lo que
  ;; acaba de pasar. Mientras la promocion vivia en el llamador, este funcion
  ;; podia leer una memoria vacia y devolver un programa en blanco: no habia
  ;; forma de distinguir "no se ha promovido" de "no hay nada".
  ;;
  ;; Y es idempotente, porque promover dos veces el mismo hecho no lo duplica,
  ;; asi que el llamador puede promover antes de Construiry no pasa nada.
  (promote-durable-facts)
  (retract-oldest-of-type "user-input" (max-facts-per-type))
  (retract-oldest-of-type "llm-response" (max-facts-per-type))
  (retract-oldest-of-type "command-exec" (max-facts-per-type))
  (retract-oldest-of-type "file-read" (max-facts-per-type))
  (retract-oldest-of-type "file-write" (max-facts-per-type))
  ;; El protocolo tambien crece: un batch-plan y un verdict POR PASO. Sin tope,
  ;; un turno de 20 pasos deja 40 hechos que se acumulan para siempre y el
  ;; contexto se infla sin que nadie lo note. Con tope, lo que se va es el plan
  ;; viejo, que es justo lo que el modelo ya no puede reintentar.
  (retract-oldest-of-type "file-edit" (max-facts-per-type))
  ;; La tarjeta y su veredicto se podan por la MISMA ventana, y en ese orden:
  ;; primero se queda la tarjeta, y despues se capan los veredictos PROTEGIENDO
  ;; los de las tarjetas que siguen en pantalla. Si no, los dos topes van por
  ;; separado sobre conjuntos distintos y se desalinean solos, que es como una
  ;; tarjeta se queda sin veredicto y sale con STATE = PENDING.
  (cap-batch-plans)
  ;; Y al reves: un veredicto CUYA TARJETA SE FUE no puede quedarse. La proteccion
  ;; de abajo solo va en un sentido -- protege al veredicto cuya tarjeta sigue
  ;; viva -- y por eso un veredicto sin tarjeta sobrevivia a la poda de la tarjeta.
  ;;
  ;; Un veredicto huerfano se renderiza en el flujo como 'STEP nn.', o sea como
  ;; si fuera un paso que el modelo llego a pedir, cuando su plan hace rato que no
  ;; esta en pantalla. Y se duplica: la linea suelta del hecho (READ-FILE, FAILED,
  ;; REASON = ...) y la tarjeta (STEP 01. READ-FILE, FAILED, REASON = ...) salen
  ;; las dos, con la misma razon y en distinto orden. El modelo no puede saber si
  ;; le contours dos veces el mismo hecho o dos hechos, y planifica en torno a un
  ;; historial que no se deja leer.
  (let ((live (live-plan-step-identities)))
    (retract-oldest-of-type "verdict" (max-facts-per-type)
                            (lambda (f)
                              (gethash (plan-step-identity (fact-data-of f)) live))))
  ;; --- GOALS: close what the batch already did, THEN bound the history ---
  ;; Order matters. Reconciling first means the cap evicts finished goals
  ;; instead of unfinished ones, so the goals the model still has to act on
  ;; are the ones that survive.
  ;;
  ;; DE LA MEMORIA DE LARGO PLAZO, y no del motor de turno. El .cob se genera
  ;; desde alli, y una meta se cierra con la EVIDENCIA de que se hizo -- esa
  ;; evidencia tiene que salir del mismo sitio del que sale el resto del
  ;; programa, o las dos mitades pueden discrepar.
  ;;
  ;; Y discreparon. En una corrida real: la meta 'Write utils.py' se abrio en
  ;; el turno 2, la escritura se aplico y quedo durable, y en el turno 5 la
  ;; meta seguia ABIERTA. La razon es el TTL: fact_ttl_seconds = 120, y entre
  ;; el turno 2 y el 5 habian pasado doce minutos, asi que PRUNE-EXPIRED ya
  ;; habia retractado el file-write del motor de turno. Reconciliando contra
  ;; el motor de turno, la evidencia de una meta de hace dos minutos ya no
  ;; existia y la meta no se cerraba nunca.
  ;;
  ;; No era un detalle cosmético: GOAL ABIERTO. N en cero es lo que habilita
  ;; DONE. Una meta que no se cierra nunca es una sesion que no puede terminar.
  (reconcile-todos (mem-engine-facts))
  (prune-finished-todos)

  (let* ((all-facts (mem-engine-facts))
         (now-turn (if all-facts
                       (or (data-get (fact-data-of (car (last all-facts))) :turn-id)
                           0)
                       0)))
    (when *debug-mode*
      (format t "~&[DEBUG build-context] mem-facts=~A now-turn=~A~%" (length all-facts) now-turn))
    (let ((selected (select-relevant-facts all-facts now-turn :query user-message)))
      ;; La memoria se calcula ANTES de renderizar, porque ahora es una
      ;; division del programa y no una cola que se le pega por detras.
      (let* ((mem (mem-context-block))
             (cobol (grouped-context-string selected now-turn mem)))
        ;; DUMP AUTOMÁTICO AQUÍ:
        (when *debug-mode*
          (let* ((ts (get-universal-time))
                 ;; Armamos la ruta completa: /harness-base/debug/debug-123.cob
                 (full-path (merge-pathnames
                             (format nil "debug/debug-~A.cob" ts)
                             (harness-base-dir))))
            ;; Le pasamos el archivo: SBCL crea la carpeta 'debug' si no existe
            (ensure-directories-exist full-path)
            (with-open-file (s full-path
                               :direction :output :if-exists :supersede)
              (write-string cobol s))))
        cobol))))
