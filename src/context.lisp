(in-package :cl-harness)

;;; ============================================
;;; Context builder — YAML output
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
   inside the same second came out in arbitrary order and the rendered YAML
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
   This prevents a single massive file from starving the rest of the context budget."
  (if (or (null text) (<= (length text) max-chars))
      text
      (let* ((head-len (floor (* max-chars 0.6)))
             (tail-len (floor (* max-chars 0.3)))
             (omitted (- (length text) head-len tail-len)))
        (format nil "~A~%... [~A characters omitted. Use read_file or grep to inspect specifics] ...~%~A"
                (subseq text 0 head-len)
                omitted
                (subseq text (- (length text) tail-len))))))


(defun yaml-block (text &optional (indent "          "))
  "Formatea un texto multi-línea como un block scalar de YAML (usando |)."
  (with-output-to-string (s)
    (format s "|~%")
    (dolist (line (cl-ppcre:split "\\n" (or text "")))
      (format s "~A~A~%" indent line))))


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
;;; build-yaml-context next to the per-type caps, which is the mechanism this
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
     every verb: the evidence has to say it was applied."
  (let* ((task (or (get-slot-value todo 'task) ""))
         (verb (todo-verb task))
         (arg (and verb (todo-argument task verb)))
         (types (and verb (todo-evidence-types verb)))
         (since (or (get-slot-value todo 'timestamp) 0)))
    (and arg types
         (some (lambda (f)
                 (and (>= (fact-timestamp-of f) since)
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

(defun render-plan-card (facts)
  "El PLAN como tarjeta COBOL: lo que el LLM pidio y lo que la maquina respondio.

   NO es decoracion. El formato ES el mecanismo de seguridad del protocolo:

     - La seccion que el modelo controla (sus pasos) y la que la maquina
       controla (los veredictos) estan SEPARADAS por division. Un modelo que
       quisiera declarar su propio exito tendria que escribir en la division
       de la maquina, que es la que el harness rellena. En el YAML plano el
       veredicto viajaba en la misma clave que la accion, al lado: nada
       impedia que el modelo se lo escribiera solo.

     - IDENTITY DIVISION lleva de quien es la tarjeta. Sin eso, un veredicto
       de un turno viejo es indistinguible de uno recien emitido, y el modelo
       reintenta cosas que ya se完之后.

     - DATA DIVISION lleva GOAL ABIERTO, la cuenta que I5 ya hacia calcular y
       que hasta ahora vivia escondida dentro de open-goal-tasks. Aqui el
       modelo ve sus bananas en el mismo sitio donde ve su plan, que es donde
       tiene sentido que mire antes de decidir el proximo turno.

   PENDING es lo que la hace estado de maquina y no parte de prensa: lo que
   no se ha ejecutado se ve.

   Tolera hechos basura, como el renderer anterior: un batch-plan sin :step, o
   con un :step que no es entero, se SALTA. Perder el estado de los pasos
   buenos por un dato sucio en uno seria la peor forma de perderlo, porque
   ademas seria silenciosa."
  (let* ((steps (remove nil
                        (mapcar (lambda (d)
                                  (let ((n (data-get d :step)))
                                    (when (and (integerp n) (> n 0)) d)))
                                (collect-facts-of-type facts "batch-plan"))))
         (verdicts (collect-facts-of-type facts "verdict")))
    (when steps
      (let ((order (sort steps #'< :key (lambda (d) (data-get d :step)))))
        (with-output-to-string (s)
          (format s "~&IDENTIFICATION DIVISION.~%")
          (format s "PROGRAM-ID. ~A.~%" (or *session-id* "SESSION"))
          (format s "~%PROCEDURE DIVISION.~%")
          (dolist (d order)
            (let* ((n (data-get d :step))
                   (v (find-if (lambda (vd) (eql (data-get vd :step) n)) verdicts))
                   (verdict (data-get v :verdict))
                   ;; OJO: :APPLIED y :FAILED son keywords y los dos son
                   ;; truthy. Hay que COMPARAR para decidir cual es cual; con un
                   ;; test de verdad, todo saldria 'applied'.
                   (state (if v
                              (string-downcase
                                (symbol-name (if (keywordp verdict) verdict :failed)))
                              "pending")))
              (format s "    ~A  ~A  ~A~%"
                      (cobol-step-label n)
                      (cobol-verb (or (data-get d :action) "?"))
                      (or (data-get d :target) "-"))
              (format s "        STATE = ~A~%"
                      (string-upcase state))
              (let ((why (data-get v :reason)))
                (when why
                  (format s "        REASON = ~A~%" why)))))
          (format s "~%DATA DIVISION.~%")
          (let ((open (open-goal-tasks)))
            (format s "~%GOAL ABIERTO. ~D~%"
                    (length open))
            (dolist (g open)
              (format s "    ~A~%" g)))
          (format s "~%GOBACK.~%"))))))


(defun cobol-verb (action)
  "La accion tal cual la escribio el modelo, enmayusculada.

   Sin traducir. READ-FILE se queda READ-FILE: el mismo token que el modelo
   escribio, para que pueda comparar lo que pidio con lo que se ejecuto. Una
   tabla de sinonimos aqui seria otra fuente de verdad mas que puede
   contradecir al modelo, y su memoria no tiene por que pasar por ella."
  (string-upcase (princ-to-string action)))



(defun grouped-context-string (facts now-turn)
  "Render facts grouped by causal :turn-id, separating past from current turn."
  (let ((groups (make-hash-table :test #'eql))
        (epoch (current-epoch))
        (todos (collect-active-todos)))
    (dolist (f facts)
      (let* ((type (fact-type-of f))
             (data (fact-data-of f))
             (turn (or (data-get data :turn-id) 0)))
        ;; Carry the full ORDER KEY, not the bare timestamp: two events in the
        ;; same second used to be re-sorted here with no tiebreaker, which is how
        ;; a command still came out above the write it read.
        ;; NOTE: built with CONS, not LIST -- in this package LIST is LISA's, and
        ;; calling it here silently produced a pattern instead of a list.
        (push (cons (fact-order-key f) (cons type data)) (gethash turn groups))))
    (maphash (lambda (turn cells)
               (setf (gethash turn groups)
                     (sort cells #'fact-order-before-p :key #'car)))
             groups)


        (flet ((render-fact (s type data)
             "Helper local para renderizar un hecho en YAML."
             (cond ((string= type "user-input")
                    (format s "      user: ~A~%" (escape-yaml (or (data-get data :text) ""))))
                   ((string= type "llm-response")
                    (format s "      assistant: ~A~%" (escape-yaml (or (data-get data :text) ""))))
                   ((string= type "command-exec")
                    (format s "      exec:~%")
                    (format s "        command: ~A~%" (escape-yaml (or (data-get data :command) "")))
                    (format s "        exit_code: ~A~%" (or (data-get data :exit-code) ""))
                    (format s "        output: ~A~%" (yaml-block (truncate-payload (or (data-get data :output) "")) "          ")))
                   ((string= type "file-read")
                    ;; I6: una lectura fallida se renderiza COMO FALLIDA. Con
                    ;; el motivo dentro de :contents se imprimia un bloque de
                    ;; texto que parecia el contenido del fichero.
                    (if (data-get data :applied)
                        (format s "      read:~%")
                        (format s "      read_failed:~%"))
                    (format s "        path: ~A~%" (escape-yaml (or (data-get data :path) "")))
                    (if (data-get data :applied)
                        (format s "        contents: ~A~%" (yaml-block (truncate-payload (or (data-get data :contents) "")) "          "))
                        (when (data-get data :reason)
                          (format s "        reason: ~A~%" (escape-yaml (data-get data :reason))))))
                   ((string= type "file-write")
                    (if (data-get data :applied)
                        (format s "      wrote:~%")
                        (format s "      write_refused:~%"))
                    (format s "        path: ~A~%" (escape-yaml (or (data-get data :path) "")))
                    (when (data-get data :bytes)
                      (format s "        bytes: ~A~%" (data-get data :bytes)))
                    ;; I6: :error ya no existe en los hechos; el motivo es
                    ;; :reason, como en file-edit y en verdict.
                    (when (data-get data :reason)
                      (format s "        reason: ~A~%" (escape-yaml (data-get data :reason))))
                    (when (data-get data :refused)
                      (format s "        refused: true~%")))

		   ((string= type "file-edit")
                    (format s "      edit:~%")
                    (format s "        path: ~A~%" (escape-yaml (or (data-get data :path) "")))
                    (format s "        applied: ~A~%" (if (data-get data :applied) "true" "false"))
                    ;; I6: una sola clave, :reason. Leer :reason y :error a la
                    ;; vez era el sintoma de que el rechazo no tenia nombre unico.
                    (let ((why (data-get data :reason)))
                      (when why
                        (format s "        reason: ~A~%" (escape-yaml why))))
                    (when (data-get data :matches)
                      (format s "        matches: ~A~%" (data-get data :matches))))
;; PROTOCOLO §2.3: el VERDICT es lo unico que el LLM puede leer como verdad
;; sobre lo que paso. :step es lo que lo enlaza con el plan que el propio
;; modelo probo -- sin el, el modelo ve "edit fallo" y no sabe cual de sus N
;; pasos fue, que es justo lo que necesita para decidir el siguiente.
		   ((string= type "verdict")
                     (format s "      verdict:~%")
                     (format s "        step: ~A~%" (or (data-get data :step) "?"))
                     (format s "        action: ~A~%" (or (data-get data :action) "?"))
                     (format s "        target: ~A~%" (escape-yaml (or (data-get data :target) "")))
                     ;; OJO: :verdict es el keyword :applied o :failed, y en
                     ;; Lisp los DOS son truthy. Un (if (data-get ...)) dira
                     ;; "applied" siempre; hay que COMPARAR el simbolo.
                     (format s "        result: ~A~%"
                             (let ((v (data-get data :verdict)))
                               (string-downcase (symbol-name (if (keywordp v) v :failed)))))
                     (let ((why (data-get data :reason)))
                       (when why
                         (format s "        reason: ~A~%" (escape-yaml why)))))
		   
                    ;; Suprimimos hechos de control interno que no son contexto.
                    ;; batch-plan NO va aqui: se renderiza en su propio bloque
                    ;; (ver PLAN-BLOCK), que lo junta con los veredictos.
                    ((or (string= type "tool-loop")
                         (string= type "plan-done")
                         (string= type "batch-abort")
                         (string= type "intention")
                         (string= type "batch-plan")
                         (string= type "verdict"))
                     nil)
                    (t nil))))

	  
      (with-output-to-string (s)
        (format s "context:~%")
        (format s "  session: ~A~%" (escape-yaml *session-id*))
        
        ;; Render epoch baseline snapshot if present
        (when epoch
          (format s "  epoch:~%")
          (format s "    id: ~A~%" (escape-yaml (get-slot-value epoch 'id)))
          (format s "    baseline_turn: ~A~%" (get-slot-value epoch 'baseline-seq))
          (format s "    summary: ~A~%" (escape-yaml (get-slot-value epoch 'summary))))

        ;; PROTOCOLO: el plan del turno como TARJETA COBOL, antes de los goals.
        ;; Primero porque es lo que acaba de hacer el modelo y lo que decide su
        ;; siguiente turno; los goals son el marco de fondo.
        ;;
        ;; Va dentro del bloque context: y en indented, y no suelto arriba. La
        ;; tarjeta es texto plano con su propio formato, y la clave del sistema
        ;; es que cada bloque tenga un renderizador con el suyo. Mezclarla aqui
        ;; seria el camino corto a un formato que no se puede parsear de vuelta.
        (let ((plan (render-plan-card facts)))
          (when plan
            (format s "~A" plan)))
        
        ;; Render goals / todos
        (when todos
          (format s "  goals:~%")
          (dolist (td todos)
            (let ((id (get-slot-value td 'id))
                  (task (get-slot-value td 'task))
                  (status (get-slot-value td 'status))
                  (priority (get-slot-value td 'priority))
                  (parent (get-slot-value td 'parent-id)))
              (format s "    - id: ~A~%" (escape-yaml id))
              (format s "      task: ~A~%" (escape-yaml task))
              (format s "      status: ~A~%" (escape-yaml status))
              (format s "      priority: ~A~%" (escape-yaml priority))
              (unless (or (null parent) (string= parent "root"))
                (format s "      parent_goal: ~A~%" (escape-yaml parent))))))
        
        ;; Render loop warnings
        (let ((loops (collect-tool-loops)))
          (when loops
            (format s "  warnings:~%")
            (dolist (l loops)
              (let* ((d (fact-data-of l))
                     (family (or (data-get d :family) ""))
                     (count (or (data-get d :count) 3)))
                (format s "    - loop: ~A failing commands are the same retried attempt (family ~S). STOP re-running variants — change approach.~%"
                        count family)))))
        
        ;; Render PAST TURNS (Historia cerrada)
        (format s "  prior_turns:~%")
        (dolist (turn (sort (loop for k being the hash-keys of groups collect k) #'<))
          (when (< turn now-turn)
            (format s "    ~A:~%" turn)
            (dolist (cell (gethash turn groups))
              (render-fact s (cadr cell) (cddr cell)))))
        
        ;; Render CURRENT TURN (Lo que está pasando ahora)
        (when (> now-turn 0)
          (format s "  current_turn:~%")
          (format s "    number: ~A~%" now-turn)
          (format s "    events:~%")
          (dolist (cell (gethash now-turn groups))
            (render-fact s (cadr cell) (cddr cell))))))))


(defun mem-context-block ()
  "Render long-term memory (durable facts in the *mem-engine*) as a YAML
   section appended to every turn's context. Independent of the turn working
   memory and its TTL/dedup/cap pruning."
  (let ((facts (mem-engine-facts)))
    (when facts
      (with-output-to-string (s)
        (format s "long_term_memory:~%")
        (dolist (f (nreverse
                     (sort facts #'fact-order-before-p :key #'fact-order-key)))
          (let* ((type (fact-type-of f))
                 (data (fact-data-of f)))
            (cond ((string= type "command-exec")
                   (format s "  - error: ~A -> exit ~A~%"
                           (escape-yaml (or (data-get data :command) ""))
                           (or (data-get data :exit-code) ""))
                   (format s "    evidence: ~A~%"
                           (escape-yaml (truncate-payload
                                         (or (data-get data :output) "")))))
                  ((string= type "file-write")
                   (format s "  - action: wrote ~A (~A bytes)~%"
                           (escape-yaml (or (data-get data :path) ""))
                           (or (data-get data :bytes) "")))
                  (t nil))))))))


(defun build-yaml-context (user-message)
  "Build the full YAML context block: run pruning rules, then select the
   most structurally and semantically relevant facts."
  (run)
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
  (retract-oldest-of-type "batch-plan" (max-facts-per-type))
  (retract-oldest-of-type "verdict" (max-facts-per-type))
  ;; --- GOALS: close what the batch already did, THEN bound the history ---
  ;; Order matters. Reconciling first means the cap evicts finished goals
  ;; instead of unfinished ones, so the goals the model still has to act on
  ;; are the ones that survive.
  (reconcile-todos (collect-active-facts))
  (prune-finished-todos)

  (let* ((all-facts (collect-active-facts))
         (now-turn (if all-facts
                       (or (data-get (fact-data-of (car (last all-facts))) :turn-id)
                           0)
                       0)))
    (when *debug-mode*
      (format t "~&[DEBUG build-yaml-context] active-facts=~A now-turn=~A~%" (length all-facts) now-turn))
    (let ((selected (select-relevant-facts all-facts now-turn :query user-message)))
      (let ((yaml (grouped-context-string selected now-turn)))
        ;; DUMP AUTOMÁTICO AQUÍ:
        (when *debug-mode*
          (let* ((ts (get-universal-time))
                 ;; Armamos la ruta completa: /harness-base/debug/debug-123.yaml
                 (full-path (merge-pathnames 
                             (format nil "debug/debug-~A.yaml" ts) 
                             (harness-base-dir))))
            ;; Le pasamos el archivo: SBCL crea la carpeta 'debug' si no existe
            (ensure-directories-exist full-path)
            (with-open-file (s full-path
                               :direction :output :if-exists :supersede)
              (write-string yaml s))))
        (let ((mem (mem-context-block)))
          (if mem (format nil "~A~%~A" yaml mem) yaml))))))
