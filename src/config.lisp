(in-package :cl-harness)

(defparameter *config* nil)
(defparameter *session-id*
  (format nil "session-~A" (get-universal-time)))

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

(defun max-context-chars ()
  "Budget for the rendered context block. Facts are selected by
   structural and semantic relevance until this budget is consumed."
  (or (config-value "max_context_chars") 12000))

(defun llm-max-tokens ()
  (or (config-value "llm_max_tokens") 4000))

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
