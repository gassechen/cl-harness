(in-package :cl-harness)

;;; ============================================
;;; Metrics — Phase 0: measure before optimizing
;;; ============================================
;;;
;;; The purpose of this module is to make every future rule change
;;; *measurable*: we compare the CURATED context we send to the LLM
;;; against a NAIVE baseline (all facts verbatim, no pruning/caps),
;;; and we also record the REAL usage reported by the provider.
;;;
;;; Token estimates are heuristic (1 token ~= 4 chars). The real
;;; provider usage (when the LLM answers) is ground truth.

;;; These live in llm.lisp (loaded AFTER this file) and in repl.lisp, so they
;;; are declared here too to keep the compiler quiet and to make it explicit
;;; that metrics.lisp depends on them. DEFVAR does not reset an existing value;
;;; the DEFVAR of the owning file still runs later and initialises it to NIL.
(defvar *debug-mode* nil)
(defvar *last-llm-usage* nil)
(defvar *llm-call-log* nil)
(defvar *llm-response-model* nil)

(defparameter *metrics-events* nil
  "List of per-turn metric records (plists).")

(defparameter *metrics-tool-calls* nil
  "Per-call tool execution records (:name :chars-raw :chars-sent), reset at
   the end of each turn (see record-context-metrics).")

(defparameter *metrics-llm-calls* nil
  "Per-API-call token records for the turn in progress, snapshotted by
   record-context-metrics. Mirrors cl-harness::*llm-call-log*.")

(defun token-estimate (text)
  "Heuristic token count: ~4 chars per token. Good enough for ratios."
  (ceiling (length text) 4))

(defun record-llm-call (record)
  "Record one provider API call for per-call token accounting. RECORD is a
   plist (:prompt N :completion N :model \"m\" :endpoint \"url\")."
  (push record *metrics-llm-calls*))

(defun record-tool-call (name raw-text sent-text)
  "Record one tool execution for per-turn metrics. RAW-TEXT is the full
   result, SENT-TEXT the version actually injected into the message pile
   (after truncation), so we can measure the intra-turn savings."
  (push (list :name name
              :chars-raw (length raw-text)
              :chars-sent (length sent-text))
        *metrics-tool-calls*))

(defun naive-context-string ()
  "NAIVE baseline: every fact verbatim, no TTL, no caps.
   This is what an unmanaged harness would send to the LLM."
  (with-output-to-string (s)
    (format s "context:~%")
    (format s "  session: ~A~%" (escape-yaml *session-id*))
    (format s "  facts:~%")
    (dolist (f (collect-active-facts))
      (format s "~A~%" (fact-to-yaml f)))))

(defun record-context-metrics (context-str user-str
                               &key (naive-str nil) real-prompt real-completion
                                    llm-iterations llm-path (llm-calls nil)
                                    (response-model nil))
  "Record one turn: curated context vs naive baseline vs real API usage.
   NAIVE-STR must be a snapshot captured BEFORE pruning rules ran, so the
   reduction reflects facts the rules actually removed (a naive captured
   after run() would be identical to the curated set). The turn's tool-call
   and per-API-call stats are snapshotted and reset here.
   REAL-PROMPT/REAL-COMPLETION are CUMULATIVE usage across the whole tool
   loop (each iteration re-sends the message pile), and LLM-ITERATIONS counts
   the API requests made this turn.
   LLM-CALLS is the per-request breakdown (:prompt :completion :model), which
   is what makes a runaway loop visible instead of a single opaque total.
   RESPONSE-MODEL is the model the provider reported for this turn."
  (let ((naive (or naive-str (naive-context-string)))
        (calls (or llm-calls (reverse cl-harness::*llm-call-log*)))
        (failures (reverse cl-harness::*llm-failed-calls*)))
    (push (list :turn (1+ (length *metrics-events*))
                :user-chars (length user-str)
                :context-chars (length context-str)
                :context-tokens (token-estimate context-str)
                :naive-chars (length naive)
                :naive-tokens (token-estimate naive)
                :prompt-tokens real-prompt
                :completion-tokens real-completion
                :llm-iterations llm-iterations
                :llm-path llm-path
                :response-model (or response-model cl-harness::*llm-response-model*)
                :llm-calls calls
                :llm-failures failures
                :tools (reverse *metrics-tool-calls*))
          *metrics-events*)
    (setf *metrics-tool-calls* nil)
    (setf *metrics-llm-calls* nil)
    (setf cl-harness::*llm-call-log* nil)
    (setf cl-harness::*llm-failed-calls* nil)))

(defun metrics-reset ()
  "Clear recorded metrics."
  (setf *metrics-events* nil)
  (setf *metrics-tool-calls* nil)
  (setf *metrics-llm-calls* nil)
  (setf cl-harness::*llm-call-log* nil)
  (setf cl-harness::*llm-failed-calls* nil))

(defun metrics-tool-totals ()
  "Aggregate per-tool statistics over all recorded turns.
   Returns an alist of (name . (:count N :chars-raw R :chars-sent S))."
  (let ((agg (make-hash-table :test 'equal)))
    (dolist (e *metrics-events*)
      (dolist (c (getf e :tools))
        (let* ((name (or (getf c :name) "?"))
               (acc (gethash name agg (list :count 0 :chars-raw 0 :chars-sent 0))))
          (setf (gethash name agg)
                (list :count (1+ (getf acc :count))
                      :chars-raw (+ (getf acc :chars-raw) (getf c :chars-raw))
                      :chars-sent (+ (getf acc :chars-sent) (getf c :chars-sent)))))))
    (let ((res '()))
      (maphash (lambda (k v) (push (cons k v) res)) agg)
      (sort res #'string< :key #'car))))

(defun call-record-models (records)
  "Los :model de una lista de registros de llamada o de fallo, sin repetir el
   mismo registro. RECORDS son plists (:model \"m\" ...)."
  (loop for r in records
        for m = (getf r :model)
        when m collect m))

(defun metrics-models (evs)
  "TODOS los modelos que se usaron en la sesion, sin repetir y ordenados.

   EVS son los eventos de turno, en orden cronologico. Se barren TRES fuentes
   porque cada una se pierde por su cuenta:

     1. :response-model -- lo que reporto el proveedor en el ULTIMO exito del
        turno. Es UNO por turno: con enrutado por carga, un turno da cinco
        modelos distintos y aqui solo salia el ultimo de todos.
     2. :llm-calls -- el desglose por peticion, que si lleva un :model en cada
        una.
     3. :llm-failures -- los intentos que fallaron. Un modelo que solo falla
        tambien se uso y tambien se paga, asi que dejarlo fuera daria la misma
        lista incompleta por el otro lado.

   Antes solo se miraba la 1, y por eso la sesion decia 4 modelos cuando el
   panel de OpenRouter decia 5: el mismo hecho, dos numeros.

   OJO con escribir esto como (loop ... do (loop ... collect m)): el DO evalua
   el loop interno y se TIRA su valor, asi que la lista se pierde entera y solo
   queda lo que aporte la fuente 1. MAPCAN si encadena.

   Y OJO con REMOVE-DUPLICATES: sin :TEST usa EQL, y EQL sobre dos strings con
   el mismo texto pero distinta identidad da NIL, asi que el mismo modelo
   aparecia dos veces en la lista ('gamma, gamma') porque cada registro trae su
   propia cadena. Con :TEST STRING= se compara por texto, que es lo que quiere
   decir 'modelos distintos'."
  (sort
   (remove-duplicates
    (append (loop for e in evs
                  for m = (getf e :response-model)
                  when m collect m)
            (mapcan (lambda (e) (call-record-models (getf e :llm-calls)))
                    evs)
            (mapcan (lambda (e) (call-record-models (getf e :llm-failures)))
                    evs))
    :test #'string=)
   #'string<))

(defun metrics-totals ()
  "Aggregate statistics over all recorded turns."
  (let* ((evs (reverse *metrics-events*))
         (ctx (reduce (lambda (a e) (+ a (getf e :context-tokens)))
                      evs :initial-value 0))
         (nve (reduce (lambda (a e) (+ a (getf e :naive-tokens)))
                      evs :initial-value 0))
         (real (remove-if-not (lambda (e) (getf e :prompt-tokens)) evs))
         (real-ctx (reduce (lambda (a e) (+ a (getf e :prompt-tokens)))
                           real :initial-value 0))
         (real-comp (reduce (lambda (a e) (+ a (or (getf e :completion-tokens) 0)))
                            real :initial-value 0))
         (ok-calls (reduce (lambda (a e) (+ a (length (getf e :llm-calls))))
                           evs :initial-value 0))
         (failures (reduce (lambda (a e) (+ a (length (getf e :llm-failures))))
                           evs :initial-value 0))
         (api-calls (+ ok-calls failures))
         (models (metrics-models evs))
         (iters (reduce (lambda (a e) (+ a (or (getf e :llm-iterations) 0)))
                        evs :initial-value 0))
         (turns (length evs)))
    (list :turns turns
          :context-tokens ctx
          :naive-tokens nve
          :reduction-pct (if (zerop nve)
                             0.0
                             (* 100.0 (/ (- nve ctx) nve)))
          :real-prompt-tokens (and (plusp (length real)) real-ctx)
          :real-completion-tokens (and (plusp (length real)) real-comp)
          :real-turns (length real)
          :api-calls api-calls
          :api-ok ok-calls
          :api-failures failures
          :models models
          :llm-iterations iters)))

(defun metrics-summary (&optional (stream *standard-output*))
  "Print a readable per-turn table and totals to STREAM (stdout por defecto).
   The 'iter' column is the number
   of API requests the tool loop made; 'prompt_tok' is the CUMULATIVE prompt
   usage across all iterations of the turn (the real cost of the message pile).

   STREAM es un PARAMETRO a proposito, no un (format t ...) suelto: el
   WITH-OUTPUT-TO-STRING de SBCL no re-liga *STANDARD-OUTPUT* (usa WRITE sobre la
   variable de la macro), asi que (FORMAT T ...) dentro de el se escapa a la
   consola y la cadena sale vacia. Con STREAM el resumen se puede capturar y
   testear, que es como se encontraron los tres fallos de esta seccion."
  (let ((evs (reverse *metrics-events*)))
    (format stream "~&=== Session metrics ~A ===~%" *session-id*)
    (format stream "~&~A~%" (format nil "~4A ~4A ~7A ~7A ~9A ~7A ~9A ~13A"
                              "turn" "iter" "ctx_tok" "naive_tok" "prompt_tok" "ctx_chr" "naive_ch" "llm_path"))
    (dolist (e evs)
      (format stream "~&~4A ~4A ~7A ~7A ~9A ~7A ~9A ~13A~%"
              (getf e :turn)
              (or (getf e :llm-iterations) "-")
              (getf e :context-tokens)
              (getf e :naive-tokens)
              (or (getf e :prompt-tokens) "-")
              (getf e :context-chars)
              (getf e :naive-chars)
              (or (getf e :llm-path) "-")))
    (let ((tot (metrics-totals)))
      (format stream "~&~A~%" (format nil "~4A ~4A ~7A ~7A ~9A ~7A ~9A ~13A"
                                "TOT"
                                (or (getf tot :llm-iterations) "-")
                                (getf tot :context-tokens)
                                (getf tot :naive-tokens)
                                (or (getf tot :real-prompt-tokens) "-")
                                "" "" ""))
      (format stream "~&Reduction (curated vs naive): ~,1F%~%" (getf tot :reduction-pct))
      (format stream "~&Turns (curated): ~A · Turns (real usage): ~A · API calls: ~A~%"
              (getf tot :turns) (getf tot :real-turns) (getf tot :api-calls))
      (format stream "~&API calls: ~A ok + ~A fallidos = ~A intentos~%"
              (getf tot :api-ok) (getf tot :api-failures) (getf tot :api-calls))
      (when (getf tot :models)
        (format stream "~&Models: ~{~A~^, ~}~%" (getf tot :models)))
      (dolist (e (reverse *metrics-events*))
        (let* ((f (getf e :llm-failures))
               (n (length f))
               ;; YA VIENEN en orden cronologico: RECORD-CONTEXT-METRICS
               ;; invierte *LLM-FAILED-CALLS*, que se apila por delante. No se
               ;; reordena, porque lo que interesa es WHEN fallo cada cosa, no
               ;; agrupar por codigo. Los repetidos se colapsan.
               (msgs (remove-duplicates
                      (mapcar (lambda (r) (or (getf r :error) "?")) f))))
          ;; (when n) NO vale: 0 es verdadero en Common Lisp, asi que un turno
          ;; sin fallos imprimia '0 intentos fallido:' con la lista vacia. Y
          ;; ~:[~;s~] imprime la rama THEN cuando el argumento es NO-NIL, o sea
          ;; que la 's' va con (not (= n 1)) y no con (= n 1).
          (when (plusp n)
            (format stream "~&  turno ~A: ~D intento~P fallido~:[~;s~]: ~{~A~^, ~}~%"
                    (getf e :turn) n n (not (= n 1)) msgs))))
      (format stream "~&Real completion tokens: ~A~%" (getf tot :real-completion-tokens)))
    (let ((tools (metrics-tool-totals)))
      (when tools
        (format stream "~&~%Tools:~%")
        (format stream "~&~12A ~6A ~11A ~11A ~8A~%"
                "tool" "calls" "chars_raw" "chars_sent" "saved%")
        (dolist (entry tools)
          (let* ((cc (cdr entry))
                 (raw (getf cc :chars-raw))
                 (sent (getf cc :chars-sent)))
            (format stream "~&~12A ~6A ~11A ~11A ~7,1F~%"
                    (car entry) (getf cc :count) raw sent
                    (if (zerop raw)
                        0.0
                        (* 100.0 (/ (- raw sent) raw))))))))))

(defun metrics-to-json (&optional (path nil))
  "Write metrics totals + per-turn events + per-tool stats to a JSON file."
  (ensure-directories-exist (metrics-dir))
  (let* ((path (or path
                   (merge-pathnames
                    (format nil "~A-metrics.json" *session-id*)
                    (metrics-dir))))
         (tools (metrics-tool-totals))
         (data (list :session-id *session-id*
                     :generated (get-universal-time)
                     :totals (metrics-totals)
                     :tools (loop for (name . cc) in tools
                                 collect (list :name name
                                               :count (getf cc :count)
                                               :chars-raw (getf cc :chars-raw)
                                               :chars-sent (getf cc :chars-sent)))
                     :events (reverse *metrics-events*)))
         (json (com.inuoe.jzon:stringify (convert-to-json-map data))))
    (with-open-file (s path :direction :output :if-exists :supersede)
      (write-string json s))
    (format t "~&Metrics written to ~A~%" path)))