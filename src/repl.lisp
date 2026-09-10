(in-package :cl-harness)

;;; ============================================
;;; Main REPL loop
;;; ============================================

(defparameter *running* nil)
(defparameter *debug-mode* nil
  "When T, emit detailed debug traces for exec-command, context selection, and LLM calls.")
(defparameter *system-prompt-path*
  (merge-pathnames "system-prompt.txt"
                   (asdf:system-source-directory :cl-harness)))

(defun load-system-prompt ()
  "Load the system prompt from file."
  (if (probe-file *system-prompt-path*)
      (with-open-file (s *system-prompt-path* :direction :input)
        (let ((buf (make-string (file-length s))))
          (read-sequence buf s)
          buf))
      "You are a helpful coding assistant. You have access to the user's
working context via structured facts. Help them with their task."))

(defun print-welcome ()
  (format t "~&╔══════════════════════════════════════════╗~%")
  (format t "║  cl-harness — Context Management Harness  ║~%")
  (format t "║  Lisa/Rete + LLM · v0.1.0                 ║~%")
  (format t "╚══════════════════════════════════════════╝~%")
  (format t "~&Session: ~A~%" *session-id*)
  (format t "~&LLM:     ~A (~A)~%" (llm-provider) (llm-model))
  (format t "~&TTL:     ~A sec · Max facts: ~A · Per-type: ~A~%"
          (fact-ttl-seconds) (max-context-facts) (max-facts-per-type))
  (format t "~&Type :help for commands, :quit to exit.~%~%"))

(defun toggle-debug (&optional (new-state nil))
  "Toggle or set debug mode."
  (setf *debug-mode* (if new-state new-state (not *debug-mode*)))
  (format t "~&Debug mode: ~A~%" *debug-mode*))

(defun print-help ()
  (format t "~&Commands:~%")
  (format t "  :help        Show this help~%")
  (format t "  :quit        Exit harness~%")
  (format t "  :debug       Toggle debug mode~%")
  (format t "  :facts       Show active facts in Rete~%")
  (format t "  :rules       Show loaded rules~%")
  (format t "  :save        Save session to disk~%")
  (format t "  :restore ID  Restore a previous session~%")
  (format t "  :exec CMD    Execute a shell command~%")
  (format t "  :read PATH   Read a file~%")
  (format t "  :write PATH  Write text to a file (prompts for content)~%")
  (format t "  :dump        Create SBCL core image~%")
  (format t "  :clear       Clear all facts~%")
  (format t "  :context     Show current YAML context~%")
  (format t "  :metrics     Show per-turn token metrics (Phase 0)~%")
  (format t "  :model NAME  Switch LLM model~%")
  (format t "  :provider P  Switch LLM provider~%")
  (format t "~%"))

(defun handle-command (input)
  "Handle meta-commands (starting with :). Returns T to continue, NIL to quit."
  (let* ((trimmed (string-trim " " input))
         (parts (uiop:split-string trimmed :separator '(#\Space)))
         (cmd (string-downcase (or (first parts) ""))))
    (cond
      ((string= cmd ":quit")
       (save-session)
       (format t "~&Session saved. Goodbye.~%")
       nil)

       ((string= cmd ":help")
        (print-help)
        t)

       ((string= cmd ":debug")
        (toggle-debug)
        t)

      ((string= cmd ":facts")
       (let ((facts (collect-active-facts)))
         (format t "~&Active facts (~A):~%" (length facts))
         (dolist (f facts)
           (format t "  [~A] ~A ts=~A~%"
                   (fact-slot f 'fact-type)
                   (fact-slot f 'data)
                   (fact-slot f 'timestamp))))
       t)

      ((string= cmd ":rules")
       (format t "~&Loaded rules:~%")
       (dolist (r (rules))
         (format t "  ~A~%" (rule-name r)))
       t)

      ((string= cmd ":save")
       (save-session)
       t)

      ((string= cmd ":restore")
       (let ((sid (second parts)))
         (if sid
             (restore-session sid)
             (format t "~&Usage: :restore SESSION-ID~%")))
       t)

       ((string= cmd ":exec")
        (let ((shell-cmd (subseq input (1+ (position #\Space input)))))
          (format t "~&> ~A~%" shell-cmd)
          (let ((result (exec-command shell-cmd)))
            (format t "~A~%" (getf result :output))
            (format t "(exit: ~A)~%" (getf result :exit-code))))
        t)

      ((string= cmd ":read")
       (let ((path (second parts)))
         (if path
             (let ((result (read-file path)))
               (format t "~A~%" (getf result :contents)))
             (format t "~&Usage: :read FILEPATH~%")))
       t)

      ((string= cmd ":write")
       (let ((path (second parts)))
         (if path
             (progn
               (format t "~&Enter content (end with ~A on its own line):~%" "\\.")
               (let ((lines '()))
                 (loop for line = (read-line *standard-input* nil nil)
                       while (and line (not (string= (string-trim '(#\Space #\Tab) line) ".")))
                       do (push line lines))
                 (let ((content (format nil "~{~A~^~%~}" (nreverse lines))))
                   (write-file path content)
                   (format t "~&Written ~A bytes to ~A~%" (length content) path))))
             (format t "~&Usage: :write FILEPATH~%")))
       t)

      ((string= cmd ":dump")
       (create-session-dump)
       t)

      ((string= cmd ":clear")
       (clear)
       (format t "~&Facts cleared.~%")
       t)

      ((string= cmd ":context")
       (format t "~A" (build-yaml-context ""))
       t)

      ((string= cmd ":metrics")
       (metrics-summary)
       t)

      ((string= cmd ":model")
       (let ((model (second parts)))
         (if model
             (progn
               (setf (gethash "llm_model" *config*) model)
               (format t "~&Model set to: ~A~%" model))
             (format t "~&Usage: :model MODEL-NAME~%")))
       t)

      ((string= cmd ":provider")
       (let ((prov (second parts)))
         (if prov
             (progn
               (setf (gethash "llm_provider" *config*) prov)
               (format t "~&Provider set to: ~A~%" prov))
             (format t "~&Usage: :provider PROVIDER~%")))
       t)

      (t
       (format t "~&Unknown command: ~A~%" cmd)
       t))))

(defun process-turn (user-message system-prompt)
  "Single turn: build context → call LLM → register response.
    Records per-turn context metrics (Phase 0) and tags every fact with
    the causal :turn-id / :parent-id metadata (Phase 2)."
  (incf *turn-counter*)
  (let* ((turn *turn-counter*)
         ;; Register user input as fact, tagged with its causal turn id
         (naive (progn
                  (assert (harness-fact (fact-type "user-input")
                                       (timestamp (get-universal-time))
                                       (data (list :text user-message
                                                   :turn-id turn))))
                  ;; Snapshot the NAIVE baseline BEFORE pruning rules run,
                  ;; so the metrics show the reduction rules really produce.
                  (naive-context-string)))
         (context (build-yaml-context user-message))
         (response (handler-case
                        (progn
                          (setf *last-llm-usage* nil)
                          (call-llm system-prompt context user-message))
                      (error (e) (format nil "[LLM ERROR] ~A" e)))))
    (record-context-metrics context user-message
                            :naive-str naive
                            :real-prompt (getf *last-llm-usage* :prompt)
                            :real-completion (getf *last-llm-usage* :completion))
    ;; Register LLM response as fact (skip error markers)
    (when (and response (not (search "[LLM ERROR]" response :test #'char=)))
      (assert (harness-fact (fact-type "llm-response")
                           (timestamp (get-universal-time))
                           (data (list :text response
                                       :turn-id turn
                                       :parent-id turn)))))
    (when *debug-mode*
      (format t "~&[DEBUG process-turn] turn=~A user=~A context-len=~A~%" turn user-message (length context))
      (format t "~&[DEBUG process-turn] response=~A~%" response))
    response))

(defun run-one-shot (user-message &optional (reset-engine t))
  "Non-interactive mode (opencode-run style): process a single turn and exit.
   RESET-ENGINE T starts from an empty working memory (fresh process via
   run.sh); NIL continues the session already living in a saved core image."
  (load-config)
  (when reset-engine
    (reset))
  (metrics-reset)
  (let* ((system-prompt (load-system-prompt))
         (response (or (process-turn user-message system-prompt)
                       "[LLM ERROR] empty response")))
    (format t "~&assistant> ~A~%" response)
    (save-session)
    (finish-output)
    (sb-ext:exit :code (if (or (search "[LLM ERROR]" response :test #'char=)
                               (search "[LLM API ERROR]" response :test #'char=)
                               (search "[LLM JSON ERROR]" response :test #'char=))
                           1
                           0))))

(defun start-harness (&optional (reset-engine t))
  "Start the interactive REPL loop.
   When RESET-ENGINE is NIL the current Rete state is kept (used when
   booting from a saved SBCL core image)."
  (load-config)
  (when reset-engine
    (reset))
  (metrics-reset)
  (reset-turn-counter)
  (print-welcome)
  (setf *running* t)
  (let ((system-prompt (load-system-prompt)))
    (loop while *running*
          do (format t "~&user> ")
             (force-output *standard-input*)
             (let ((input (read-line *standard-input* nil nil)))
               (cond
                 ((null input)
                  (setf *running* nil))
                 ((string= (string-trim " " input) "")
                  nil)
                 ((char= (char input 0) #\:)
                  (setf *running* (handle-command input)))
                 (t
                  (let ((response (process-turn input system-prompt)))
                    (format t "~&~%assistant> ~A~%~%" response)))))))
  (format t "~&Harness stopped.~%"))

(defun stop-harness ()
  "Stop the harness."
  (setf *running* nil))
