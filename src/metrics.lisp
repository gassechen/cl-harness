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

(defparameter *metrics-events* nil
  "List of per-turn metric records (plists).")

(defun token-estimate (text)
  "Heuristic token count: ~4 chars per token. Good enough for ratios."
  (ceiling (length text) 4))

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
                               &key (naive-str nil) real-prompt real-completion)
  "Record one turn: curated context vs naive baseline vs real API usage.
   NAIVE-STR must be a snapshot captured BEFORE pruning rules ran, so the
   reduction reflects facts the rules actually removed (a naive captured
   after run() would be identical to the curated set)."
  (let ((naive (or naive-str (naive-context-string))))
    (push (list :turn (1+ (length *metrics-events*))
                :user-chars (length user-str)
                :context-chars (length context-str)
                :context-tokens (token-estimate context-str)
                :naive-chars (length naive)
                :naive-tokens (token-estimate naive)
                :prompt-tokens real-prompt
                :completion-tokens real-completion)
          *metrics-events*)))

(defun metrics-reset ()
  "Clear recorded metrics."
  (setf *metrics-events* nil))

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
         (turns (length evs)))
    (list :turns turns
          :context-tokens ctx
          :naive-tokens nve
          :reduction-pct (if (zerop nve)
                             0.0
                             (* 100.0 (/ (- nve ctx) nve)))
          :real-prompt-tokens (and (plusp (length real)) real-ctx)
          :real-turns (length real))))

(defun metrics-summary ()
  "Print a readable per-turn table and totals."
  (let ((evs (reverse *metrics-events*)))
    (format t "~&=== Session metrics ~A ===~%" *session-id*)
    (format t "~&~A~%" (format nil "~4A ~7A ~7A ~6A ~8A ~8A"
                              "turn" "ctx_tok" "naive_tok" "prompt" "ctx_chr" "naive_ch"))
    (dolist (e evs)
      (format t "~&~4A ~7A ~7A ~6A ~8A ~8A~%"
              (getf e :turn)
              (getf e :context-tokens)
              (getf e :naive-tokens)
              (or (getf e :prompt-tokens) "-")
              (getf e :context-chars)
              (getf e :naive-chars)))
    (let ((tot (metrics-totals)))
      (format t "~&~A~%" (format nil "~4A ~7A ~7A ~6A ~8A ~8A"
                                "TOT" (getf tot :context-tokens)
                                (getf tot :naive-tokens)
                                (or (getf tot :real-prompt-tokens) "-")
                                "" ""))
      (format t "~&Reduction (curated vs naive): ~,1F%~%" (getf tot :reduction-pct))
      (format t "~&Turns (curated): ~A · Turns (real usage): ~A~%"
              (getf tot :turns) (getf tot :real-turns)))))

(defun metrics-to-json (&optional (path nil))
  "Write metrics totals + per-turn events to a JSON file."
  (ensure-directories-exist (metrics-dir))
  (let* ((path (or path
                   (merge-pathnames
                    (format nil "~A-metrics.json" *session-id*)
                    (metrics-dir))))
         (data (list :session-id *session-id*
                     :generated (get-universal-time)
                     :totals (metrics-totals)
                     :events (reverse *metrics-events*)))
         (json (com.inuoe.jzon:stringify (convert-to-json-map data))))
    (with-open-file (s path :direction :output :if-exists :supersede)
      (write-string json s))
    (format t "~&Metrics written to ~A~%" path)))