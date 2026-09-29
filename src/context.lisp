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
         (verdicts (collect-facts-of-type facts "verdict")))
    (when steps
      (let ((order (sort steps #'< :key (lambda (d) (data-get d :step)))))
        (with-output-to-string (s)
          (format s "~&IDENTIFICATION DIVISION.~%")
          (format s "PROGRAM-ID. ~A.~%" (or *session-id* "SESSION"))
          (render-plan-procedure s order verdicts)
          (format s "~%DATA DIVISION.~%")
          (let ((open (open-goal-tasks)))
            (format s "~%GOAL ABIERTO. ~D~%"
                    (length open))
            (dolist (g open)
              (format s "    ~A~%" g)))
          (format s "~%GOBACK.~%"))))))


(defun render-plan-procedure (s order verdicts)
  "La PROCEDURE DIVISION de la tarjeta: los pasos y su veredicto.

   Vive suelto para que el programa COMPLETO del contexto la reutilice. La
   tarjeta sola es un programa entero (IDENTIFICATION...GOBACK), pero cuando
   va incrustada seria un programa dentro de otro: dos cabeceras y dos
   GOBACK, y no se sabria de quien es cada parte.

   Por eso no lleva PROGRAM-ID ni GOBACK: los pone quien envuelve.

   Los VERDICTS se pasan como argumento, no se vuelven a leer de la base de
   hechos. Recolectarlos aqui otra vez haria que la tarjeta leyera el estado
   global por su cuenta, y si el contexto se esta construyendo con una lista
   filtrada, la tarjeta se saltaria el filtro."
  (format s "~%PROCEDURE DIVISION.~%")
  (dolist (d order)
    (let* ((n (data-get d :step))
           (turn (data-get d :turn-id))
           ;; EMPAREJAR POR (turn-id, step), NO POR step SOLO.
           ;;
           ;; El numero de paso se reinicia cada turno: 02. del turno 1 y 02.
           ;; del turno 2 son el mismo numero y turnos distintos. Emparejando
           ;; solo por :step, el paso 2 del turno 2 se llevaba el veredicto del
           ;; turno 1 -- y como el plan de ambos turnos suele tener el MISMO
           ;; numero de pasos, eso no es un caso raro: es cualquier sesion de
           ;; dos turnos. La tarjeta se ve perfecta, con STATE rellenado, solo
           ;; que con el dato de otro turno. Un paso que se escribio bien
           ;; aparecia como FAILED, con el motivo de un fallo que no era suyo.
           ;;
           ;; :turn-id a NIL (un plan sin turno) solo empareja con veredictos
           ;; sin turno, para no cruzarlos con los que si lo tienen.
           (v (find-if (lambda (vd)
                         (and (eql (data-get vd :step) n)
                              (eql (data-get vd :turn-id) turn)))
                       verdicts))
           (verdict (data-get v :verdict))
           ;; OJO: :APPLIED y :FAILED son keywords y los dos son truthy. Hay
           ;; que COMPARAR para decidir cual es cual; con un test de verdad,
           ;; todo saldria 'applied'.
           (state (if v
                      (string-downcase
                        (symbol-name (if (keywordp verdict) verdict :failed)))
                      "pending")))
      ;; El turno va en la etiqueta, porque la numeracion de COBOL se REINICIA
      ;; por turno y sola no distingue 02. del turno 1 de 02. del turno 2. Sin
      ;; esto la tarjeta tiene dos 01. y dos 02. y no hay forma de saber cuales
      ;; pertenecen a cual, que es justo lo que el formato tiene que evitar.
      (format s "    ~A  T~A ~A  ~A~%"
              (cobol-step-label n)
              (or turn "-")
              (cobol-verb (or (data-get d :action) "?"))
              (or (data-get d :target) "-"))
      ;; PENDING cuando no hay veredicto: lo que el modelo pidio y todavia no
      ;; ha ocurrido. NO se omite la linea. Omitirla hacia que 'no ejecutado'
      ;; fuera indistinguible de 'no me lo dijeron', y el modelo no puede
      ;; reintentar lo que cree que no existe.
      (format s "        STATE = ~A~%"
              (string-upcase state))
      (let ((why (data-get v :reason)))
        (when why
          (format s "        REASON = ~A~%" why))))))




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
      (setf (gethash (cons (data-get d :step) (data-get d :turn-id)) plan-keys) t))
    (dolist (f facts)
      (let* ((type (fact-type-of f))
             (data (fact-data-of f))
             (turn (or (data-get data :turn-id) 0)))
        ;; Un veredicto cuyo paso YA esta en la tarjeta no se repite en el
        ;; flujo: la tarjeta lo lleva al lado del paso, y repetirlo seria la
        ;; misma verdad dos veces. El que no tiene paso -- porque el cap de
        ;; batch-plan lo retiro -- se cae al flujo para no perderse.
        (when (or (not (string= type "verdict"))
                  (not (gethash (cons (data-get data :step) (data-get data :turn-id))
                                plan-keys)))
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
      (let* ((ordered (sort (plan-steps-of facts) #'< :key (lambda (d) (data-get d :step))))
             (verdicts (collect-facts-of-type facts "verdict")))
        (render-plan-procedure s ordered verdicts))

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
   la mem-engine antes de llegar aqui."
  (let ((facts (mem-engine-facts)))
    (when facts
      (with-output-to-string (s)
        (format s "~%MEMORY DIVISION.~%")
        (dolist (f (nreverse
                     (sort facts #'fact-order-before-p :key #'fact-order-key)))
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
                  ((string= type "file-write")
                   (format s "    WROTE. ~A~%" (cobol-value (or (data-get data :path) "")))
                   (format s "        BYTES = ~A~%" (or (data-get data :bytes) "")))
                  (t nil))))))))


(defun build-context (user-message)
  "El contexto completo como programa COBOL: poda, selecciona y renderiza
   los hechos mas relevantes estructural y semanticamente."
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
      (format t "~&[DEBUG build-context] active-facts=~A now-turn=~A~%" (length all-facts) now-turn))
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
