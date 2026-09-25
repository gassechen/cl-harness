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
        (calls (or llm-calls (reverse cl-harness::*llm-call-log*))))
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
                :tools (reverse *metrics-tool-calls*))
          *metrics-events*)
    (setf *metrics-tool-calls* nil)
    (setf *metrics-llm-calls* nil)
    (setf cl-harness::*llm-call-log* nil)))

(defun metrics-reset ()
  "Clear recorded metrics."
  (setf *metrics-events* nil)
  (setf *metrics-tool-calls* nil)
  (setf *metrics-llm-calls* nil)
  (setf cl-harness::*llm-call-log* nil))

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
         (api-calls (reduce (lambda (a e) (+ a (length (getf e :llm-calls))))
                            evs :initial-value 0))
         (models (remove-duplicates
                  (loop for e in evs
                        for m = (getf e :response-model)
                        when m collect m)))
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
          :models models
          :llm-iterations iters)))

(defun metrics-summary ()
  "Print a readable per-turn table and totals. The 'iter' column is the number
   of API requests the tool loop made; 'prompt_tok' is the CUMULATIVE prompt
   usage across all iterations of the turn (the real cost of the message pile)."
  (let ((evs (reverse *metrics-events*)))
    (format t "~&=== Session metrics ~A ===~%" *session-id*)
    (format t "~&~A~%" (format nil "~4A ~4A ~7A ~7A ~9A ~7A ~9A ~13A"
                              "turn" "iter" "ctx_tok" "naive_tok" "prompt_tok" "ctx_chr" "naive_ch" "llm_path"))
    (dolist (e evs)
      (format t "~&~4A ~4A ~7A ~7A ~9A ~7A ~9A ~13A~%"
              (getf e :turn)
              (or (getf e :llm-iterations) "-")
              (getf e :context-tokens)
              (getf e :naive-tokens)
              (or (getf e :prompt-tokens) "-")
              (getf e :context-chars)
              (getf e :naive-chars)
              (or (getf e :llm-path) "-")))
    (let ((tot (metrics-totals)))
      (format t "~&~A~%" (format nil "~4A ~4A ~7A ~7A ~9A ~7A ~9A ~13A"
                                "TOT"
                                (or (getf tot :llm-iterations) "-")
                                (getf tot :context-tokens)
                                (getf tot :naive-tokens)
                                (or (getf tot :real-prompt-tokens) "-")
                                "" "" ""))
      (format t "~&Reduction (curated vs naive): ~,1F%~%" (getf tot :reduction-pct))
      (format t "~&Turns (curated): ~A · Turns (real usage): ~A · API calls: ~A~%"
              (getf tot :turns) (getf tot :real-turns) (getf tot :api-calls))
      (when (getf tot :models)
        (format t "~&Models: ~{~A~^, ~}~%" (getf tot :models)))
      (format t "~&Real completion tokens: ~A~%" (getf tot :real-completion-tokens)))
    (let ((tools (metrics-tool-totals)))
      (when tools
        (format t "~&~%Tools:~%")
        (format t "~&~12A ~6A ~11A ~11A ~8A~%"
                "tool" "calls" "chars_raw" "chars_sent" "saved%")
        (dolist (entry tools)
          (let* ((cc (cdr entry))
                 (raw (getf cc :chars-raw))
                 (sent (getf cc :chars-sent)))
            (format t "~&~12A ~6A ~11A ~11A ~7,1F~%"
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