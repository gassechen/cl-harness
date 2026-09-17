(in-package :cl-harness)

(defparameter *config* nil)
(defparameter *session-id*
  (format nil "session-~A" (get-universal-time)))

(defun new-session-id ()
  "Generate a FRESH session id. Called at every fresh start (reset-engine T)
   so an image built earlier does not keep reusing a stale id and overwriting
   dumps/sessions/metrics between independent runs."
  (setf *session-id* (format nil "session-~A" (get-universal-time))))

(defparameter *base-dir* nil
  "Base directory that owns config.json, sessions/, dumps/, metrics/ and
   system-prompt.txt. NIL = use the directory where the harness runs.")

(defun harness-base-dir ()
  "Directory for the harness artifacts. Priority:
   CL_HARNESS_DIR env var > *base-dir* > process current working directory."
  (or *base-dir*
      (let ((env (uiop:getenv "CL_HARNESS_DIR")))
        (if (and env (plusp (length env)))
            (uiop:ensure-directory-pathname env)
            (uiop:ensure-directory-pathname (uiop:getcwd))))))

(defparameter *config-file* nil
  "Path to config.json. NIL = default to config.json in the harness run directory.")

(defun load-config (&optional (path (or *config-file*
                                        (merge-pathnames "config.json"
                                                         (harness-base-dir)))))
  (if (probe-file path)
      (with-open-file (s path :direction :input)
        (setf *config* (com.inuoe.jzon:parse s)))
      (setf *config*
            (make-hash-table :test 'equal))))

(defun config-value (key &optional (default nil))
  (if *config*
      (gethash key *config* default)
      default))

(defun config-or-false (key)
  "Returns (values VALUE PRESENT). Distinguishes an ABSENT key (return the
   caller's default) from an explicitly JSON-false value (jzon parses to NIL),
   so getters can honor `\"key\": false` instead of silently falling back."
  (if *config*
      (gethash key *config*)
      (values nil nil)))

(defun llm-provider ()
  (or (config-value "llm_provider")
      (uiop:getenv "LLM_PROVIDER")
      "anthropic"))

(defun llm-api-key ()
  (or (config-value "llm_api_key")
      (uiop:getenv "ANTHROPIC_API_KEY")
      (uiop:getenv "OPENAI_API_KEY")
      (uiop:getenv "GOOGLE_API_KEY")
      ""))

(defun llm-model ()
  (or (config-value "llm_model")
      (uiop:getenv "LLM_MODEL")
      "claude-sonnet-4-20250514"))

(defun llm-endpoint ()
  (or (config-value "llm_endpoint")
      (uiop:getenv "LLM_ENDPOINT")
      nil))

(defun max-context-facts ()
  (or (config-value "max_context_facts") 50))

(defun fact-ttl-seconds ()
  "Wall-clock TTL in seconds. Defaults to 1800 (30 mins). Set to 0 to disable."
  (or (config-value "fact_ttl_seconds") 1800))

(defun fact-ttl-turns ()
  "Logical turn distance TTL. Facts older than this many turns expire."
  (or (config-value "fact_ttl_turns") 10))

(defun max-facts-per-type ()
  (or (config-value "max_facts_per_type") 20))

(defun max-fact-payload-chars ()
  "Maximum characters allowed for a single fact's payload before clipping,
   preventing a single large file from starving the rest of the context budget."
  (or (config-value "max_fact_payload_chars") 2500))

(defun max-raw-chars ()
  (or (config-value "max_raw_chars") 500))

(defun tool-timeout-seconds ()
  "Timeout (seconds) for exec_command tool calls. 0 disables. Default 30.
   Prevents a long-running command (e.g. starting a server) from hanging
   the turn forever. `\"tool_timeout_seconds\": false` also disables it."
  (multiple-value-bind (v present) (config-or-false "tool_timeout_seconds")
    (cond ((and present (null v)) 0)
          ((null v) 30)
          (t v))))

(defun background-on-timeout-p ()
  "Whether a command that exceeds tool_timeout_seconds is relaunched DETACHED
   and treated as a daemon (default T). Fully generic: no hardcoded server
   names or language stacks — any command that keeps running past the timeout
   (web server, worker, tail -f) gets this treatment so the turn is not
   blocked and the process stays usable. Disable with
   \"background_on_timeout\": false to keep plain timeout reporting."
  (multiple-value-bind (v present) (config-or-false "background_on_timeout")
    (cond ((and present (null v)) nil)
          (t t))))

(defun conserve-errors-p ()
  "Whether failed commands count as ACTIVE ERROR context: TTL-exempt (never
   expire) and boosted in relevance scoring. Default T. When disabled
   (`\"conserve_errors\": false`), failed commands expire like any other
   evidential."
  (multiple-value-bind (v present) (config-or-false "conserve_errors")
    (cond ((and present (null v)) nil)
          (t t))))

(defun max-tool-result-chars ()
  "Max chars kept from a tool result when injected back into the intra-turn
   message pile. Larger outputs are truncated with a marker so the model still
   sees the beginning. 0 (or JSON false) disables truncation. Default 8000."
  (multiple-value-bind (v present) (config-or-false "max_tool_result_chars")
    (cond ((and present (null v)) 0)
          ((null v) 8000)
          (t v))))

(defun max-context-chars ()
  "Budget for the rendered context block. Facts are selected by
   structural and semantic relevance until this budget is consumed."
  (or (config-value "max_context_chars") 12000))

(defun llm-max-tokens ()
  (or (config-value "llm_max_tokens") 4000))

(defun llm-max-tool-iterations ()
  "Max tool-use iterations per turn before giving up. Configurable so long
   multi-step tasks (write + verify) can finish inside a single turn."
  (or (config-value "llm_max_tool_iterations") 12))

(defun sessions-dir ()
  (or (config-value "sessions_dir")
      (merge-pathnames "sessions/"
                       (harness-base-dir))))

(defun dumps-dir ()
  (or (config-value "dumps_dir")
      (merge-pathnames "dumps/"
                       (harness-base-dir))))

(defun metrics-dir ()
  (or (config-value "metrics_dir")
      (merge-pathnames "metrics/"
                       (harness-base-dir))))
