(in-package :cl-harness)

;;; ============================================
;;; LLM API communication via dexador
;;; ============================================

(defparameter *last-llm-usage* nil
  "Usage (:prompt N :completion N) of the last successful LLM call.")

(defun capture-usage (resp-obj &optional (prompt-key "prompt_tokens")
                                   (completion-key "completion_tokens"))
  "Read usage metrics from a parsed API response, if present."
  (let ((usage (or (gethash "usage" resp-obj)
                   (gethash "usageMetadata" resp-obj))))
    (setf *last-llm-usage*
          (and usage
               (list :prompt (or (gethash prompt-key usage)
                                 (gethash "promptTokenCount" usage))
                     :completion (or (gethash completion-key usage)
                                     (gethash "candidatesTokenCount" usage)))))))

(defun decode-response (raw)
  "Safely decode HTTP response body (string or octets) to a character string."
  (cond
    ((stringp raw) raw)
    ((typep raw '(vector (unsigned-byte 8)))
     (babel:octets-to-string raw :encoding :utf-8))
    ((vectorp raw)
     (map 'string #'code-char raw))
    (t (format nil "~A" raw))))

(defun openrouter-p ()
  (string-equal (llm-provider) "openrouter"))

(defun groq-p ()
  (string-equal (llm-provider) "groq"))

(defun gemini-p ()
  (string-equal (llm-provider) "gemini"))

(defun openai-compat-endpoint ()
  (or (llm-endpoint)
      (cond
        ((openrouter-p) "https://openrouter.ai/api/v1/chat/completions")
        ((groq-p) "https://api.groq.com/openai/v1/chat/completions")
        (t "https://api.openai.com/v1/chat/completions"))))

(defun openai-compat-headers ()
  (let ((headers (list (cons "Authorization" (format nil "Bearer ~A" (llm-api-key)))
                       (cons "Content-Type" "application/json"))))
    (when (openrouter-p)
      (push (cons "HTTP-Referer" "https://github.com/cl-harness") headers)
      (push (cons "X-Title" "cl-harness") headers))
    headers))

(defun gemini-endpoint ()
  (or (llm-endpoint)
      (format nil "https://generativelanguage.googleapis.com/v1beta/models/~A:generateContent?key=~A"
              (llm-model) (llm-api-key))))

(defun gemini-headers ()
  (list (cons "Content-Type" "application/json")))

(defun tool-definitions ()
  "Return available tools in OpenAI-compatible format."
  (let ((tools '()))
    ;; exec_command tool
    (let* ((cmd-prop (make-hash-table :test 'equal))
           (properties (make-hash-table :test 'equal))
           (params (make-hash-table :test 'equal))
           (func (make-hash-table :test 'equal))
           (fn (make-hash-table :test 'equal)))
      (setf (gethash "type" cmd-prop) "string")
      (setf (gethash "description" cmd-prop) "The shell command to execute")
      (setf (gethash "command" properties) cmd-prop)
      (setf (gethash "type" params) "object")
      (setf (gethash "properties" params) properties)
      (setf (gethash "required" params) (list "command"))
      (setf (gethash "name" func) "exec_command")
      (setf (gethash "description" func) "Execute a shell command in the working directory. Use this to run ls, grep, cat, etc.")
      (setf (gethash "parameters" func) params)
      (setf (gethash "type" fn) "function")
      (setf (gethash "function" fn) func)
      (push fn tools))
    ;; read_file tool
    (let* ((path-prop (make-hash-table :test 'equal))
           (properties (make-hash-table :test 'equal))
           (params (make-hash-table :test 'equal))
           (func (make-hash-table :test 'equal))
           (fn (make-hash-table :test 'equal)))
      (setf (gethash "type" path-prop) "string")
      (setf (gethash "description" path-prop) "Absolute or relative path to the file")
      (setf (gethash "path" properties) path-prop)
      (setf (gethash "type" params) "object")
      (setf (gethash "properties" params) properties)
      (setf (gethash "required" params) (list "path"))
      (setf (gethash "name" func) "read_file")
      (setf (gethash "description" func) "Read the contents of a file. Use this to inspect source code, configs, etc.")
      (setf (gethash "parameters" func) params)
      (setf (gethash "type" fn) "function")
      (setf (gethash "function" fn) func)
      (push fn tools))
    (nreverse tools)))

(defun execute-tool (tool-name arguments)
  "Execute a tool by name with the given arguments hash table. Returns a result string."
  (cond
    ((string-equal tool-name "exec_command")
     (let ((cmd (gethash "command" arguments)))
       (if cmd
           (let ((result (exec-command cmd)))
             (format nil "Command: ~A~%Exit code: ~A~%Output:~%~A"
                     cmd (getf result :exit-code) (getf result :output)))
           "Error: missing command argument")))
    ((string-equal tool-name "read_file")
     (let ((path (gethash "path" arguments)))
       (if path
           (let ((result (read-file path)))
             (format nil "File: ~A~%Contents:~%~A"
                     path (getf result :contents)))
           "Error: missing path argument")))
    (t (format nil "Unknown tool: ~A" tool-name))))

(defun build-openai-compat-messages-with-tools (system-prompt context user-message prior-messages)
  "Build OpenAI-compatible Chat Completions JSON body with tools."
  (let ((msg (make-hash-table :test 'equal)))
    (setf (gethash "model" msg) (llm-model))
    (setf (gethash "max_tokens" msg) (llm-max-tokens))
    (setf (gethash "tools" msg) (tool-definitions))
    (let ((messages (copy-list prior-messages)))
      (when (or (null messages) (not (gethash "role" (car (last messages)))))
        (push (make-hash-table :test 'equal) messages)
        (setf (gethash "role" (car messages)) "user")
        (setf (gethash "content" (car messages))
              (format nil "~A~%~%~A~%~%~A"
                      "=== CONTEXT (structured facts, Rete-pruned) ==="
                      context
                      user-message)))
      (setf (gethash "messages" msg) messages))
    msg))

(defun call-llm-with-tools (system-prompt context user-message)
  "Call an OpenAI-compatible provider with tool definitions. If the model
   requests tools, execute them and loop until we get a text response or
   reach the max iterations."
  (let ((messages (list (make-hash-table :test 'equal)
                        (make-hash-table :test 'equal))))
    (setf (gethash "role" (first messages)) "system")
    (setf (gethash "content" (first messages)) system-prompt)
    (setf (gethash "role" (second messages)) "user")
    (setf (gethash "content" (second messages))
          (format nil "~A~%~%~A~%~%~A"
                  "=== CONTEXT (structured facts, Rete-pruned) ==="
                  context
                  user-message))
    (dotimes (iteration 5)
      (let* ((body (build-openai-compat-messages-with-tools system-prompt context user-message messages))
             (json-body (com.inuoe.jzon:stringify body))
             (endpoint (openai-compat-endpoint))
             (headers (openai-compat-headers))
             (raw-response (handler-case
                               (dexador:request endpoint :method :post :headers headers :content json-body :force-binary t)
                             (error (e)
                               (when *debug-mode*
                                 (format t "~&[DEBUG llm] ERROR: ~A~%" e)
                                  (let ((raw (or (ignore-errors (slot-value e 'dexador::response-body))
                                                 (ignore-errors (slot-value e 'response-body)))))
                                    (when raw
                                      (format t "~&[DEBUG llm] error-body=~A~%"
                                              (decode-response raw)))))
                                (return-from call-llm-with-tools (format nil "[LLM ERROR] ~A" e)))))
             (response (decode-response raw-response))
             (resp-obj (handler-case (com.inuoe.jzon:parse response)
                         (error (e)
                           (return-from call-llm-with-tools (format nil "[LLM JSON ERROR] ~A" e)))))
             (err-obj (when (hash-table-p resp-obj) (gethash "error" resp-obj))))
        (when *debug-mode*
          (format t "~&[DEBUG llm] iteration=~A endpoint=~A~%" (1+ iteration) endpoint)
          (format t "~&[DEBUG llm] body=~A~%" json-body)
          (format t "~&[DEBUG llm] response=~A~%" response))
        (when err-obj
          (let ((err-msg (or (and (hash-table-p err-obj) (gethash "message" err-obj)) response)))
            (when *debug-mode*
              (format t "~&[DEBUG llm] API ERROR: ~A~%" err-msg))
            (return-from call-llm-with-tools (format nil "[LLM API ERROR] ~A" err-msg))))
        (capture-usage resp-obj)
        (let* ((choices (when (hash-table-p resp-obj) (gethash "choices" resp-obj)))
               (msg (when (and choices (plusp (length choices)))
                      (gethash "message" (elt choices 0))))
               (tool-calls (when msg (gethash "tool_calls" msg))))
(if tool-calls
               (progn
                 (setf messages (append messages (list msg)))
                 (dolist (tc (if (listp tool-calls)
                                 tool-calls
                                 (coerce tool-calls 'list)))
                   (let* ((fn (gethash "function" tc))
                          (tool-name (gethash "name" fn))
                          (args-raw (gethash "arguments" fn))
                          (tool-call-id (gethash "id" tc)))
                    (when *debug-mode*
                      (format t "~&[DEBUG llm] tool_call id=~A name=~A args=~A~%"
                              tool-call-id tool-name args-raw))
                    (let* ((args-obj (handler-case (com.inuoe.jzon:parse args-raw)
                                       (error () (make-hash-table :test 'equal))))
                           (result-text (execute-tool tool-name args-obj))
                           (tool-result-msg (make-hash-table :test 'equal)))
                      (when *debug-mode*
                        (format t "~&[DEBUG llm] tool_result=~A~%" result-text))
                      (setf (gethash "role" tool-result-msg) "tool")
                      (setf (gethash "content" tool-result-msg) result-text)
                      (setf (gethash "tool_call_id" tool-result-msg) tool-call-id)
                      (setf messages (append messages (list tool-result-msg)))))))
              (return-from call-llm-with-tools (when msg (gethash "content" msg)))))))))



(defun build-anthropic-messages (system-prompt context user-message)
  "Build Anthropic Messages API JSON body."
  (let ((msg (make-hash-table :test 'equal)))
    (setf (gethash "model" msg) (llm-model))
    (setf (gethash "max_tokens" msg) (llm-max-tokens))
    (setf (gethash "system" msg) system-prompt)
    (let ((messages (list (make-hash-table :test 'equal))))
      (setf (gethash "role" (first messages)) "user")
      (setf (gethash "content" (first messages))
            (format nil "~A~%~%~A~%~%~A"
                    "=== CONTEXT (structured facts, Rete-pruned) ==="
                    context
                    user-message))
      (setf (gethash "messages" msg) messages))
    msg))

(defun build-openai-compat-messages (system-prompt context user-message)
  "Build OpenAI-compatible Chat Completions JSON body."
  (let ((msg (make-hash-table :test 'equal)))
    (setf (gethash "model" msg) (llm-model))
    (setf (gethash "max_tokens" msg) (llm-max-tokens))
    (let ((messages (list (make-hash-table :test 'equal)
                          (make-hash-table :test 'equal))))
      (setf (gethash "role" (first messages)) "system")
      (setf (gethash "content" (first messages)) system-prompt)
      (setf (gethash "role" (second messages)) "user")
      (setf (gethash "content" (second messages))
            (format nil "~A~%~%~A"
                    context
                    user-message))
      (setf (gethash "messages" msg) messages))
    msg))

(defun anthropic-endpoint ()
  (or (llm-endpoint)
      "https://api.anthropic.com/v1/messages"))

(defun anthropic-headers ()
  (list (cons "x-api-key" (llm-api-key))
        (cons "anthropic-version" "2023-06-01")
        (cons "content-type" "application/json")))

(defun call-anthropic (system-prompt context user-message)
  "Call Anthropic Messages API, return assistant text."
  (let* ((body (build-anthropic-messages system-prompt context user-message))
         (json-body (com.inuoe.jzon:stringify body))
         (response (dexador:request (anthropic-endpoint)
                                         :method :post
                                         :headers (anthropic-headers)
                                         :content json-body
                                         :force-binary t))
         (resp-obj (com.inuoe.jzon:parse response)))
    (capture-usage resp-obj "input_tokens" "output_tokens")
    (let ((content (gethash "content" resp-obj)))
      (when (and content (plusp (length content)))
        (let ((block (elt content 0)))
          (gethash "text" block))))))

(defun call-openai-compat (system-prompt context user-message)
  "Call OpenAI-compatible API (OpenAI, OpenRouter, etc.)."
  (let* ((body (build-openai-compat-messages system-prompt context user-message))
         (json-body (com.inuoe.jzon:stringify body))
         (endpoint (openai-compat-endpoint))
         (headers (openai-compat-headers)))
    (when *debug-mode*
      (format t "~&[DEBUG llm] endpoint=~A~%" endpoint)
      (format t "~&[DEBUG llm] headers=~A~%" headers)
      (format t "~&[DEBUG llm] body=~A~%" json-body))
    (handler-case
        (let* ((raw-response (dexador:request endpoint
                                              :method :post
                                              :headers headers
                                              :content json-body
                                              :force-binary t))
               (response (if (stringp raw-response)
                             raw-response
                             (coerce raw-response 'string)))
               (resp-obj (com.inuoe.jzon:parse response)))
          (when *debug-mode*
            (format t "~&[DEBUG llm] response=~A~%" response))
          (capture-usage resp-obj)
          (let ((choices (gethash "choices" resp-obj)))
            (when (and choices (plusp (length choices)))
              (let ((msg (gethash "message" (elt choices 0))))
                (gethash "content" msg)))))
      (error (e)
        (format t "~&[DEBUG llm] ERROR class=~A msg=~A~%" (class-name (class-of e)) e)
        (let ((raw (or (ignore-errors (slot-value e 'dexador::response-body))
                       (ignore-errors (slot-value e 'response-body)))))
          (when raw
            (format t "~&[DEBUG llm] error-body-type=~A len=~A~%" (type-of raw) (length raw))
            (format t "~&[DEBUG llm] error-body=~A~%"
                    (if (stringp raw) raw (coerce raw 'string)))))
        (format nil "[LLM ERROR] ~A" e)))))

(defun call-ollama (system-prompt context user-message)
  "Call local Ollama API."
  (let* ((endpoint (or (llm-endpoint) "http://localhost:11434/api/generate"))
         (body (make-hash-table :test 'equal)))
    (setf (gethash "model" body) (llm-model))
    (setf (gethash "prompt" body)
          (format nil "~A~%~%~A~%~%~A"
                  system-prompt context user-message))
    (setf (gethash "stream" body) nil)
    (let* ((json-body (com.inuoe.jzon:stringify body))
           (raw-response (dexador:request endpoint
                                               :method :post
                                               :headers '(("Content-Type" . "application/json"))
                                               :content json-body
                                               :force-binary t))
           (response (if (stringp raw-response)
                         raw-response
                         (coerce raw-response 'string)))
           (resp-obj (com.inuoe.jzon:parse response)))
      (when *debug-mode*
        (format t "~&[DEBUG llm] ollama response=~A~%" response))
      (gethash "response" resp-obj))))

(defun call-gemini (system-prompt context user-message)
  "Call Google Gemini API, return assistant text."
  (let* ((body (make-hash-table :test 'equal))
         (contents (list (make-hash-table :test 'equal)))
         (user-parts (list (make-hash-table :test 'equal))))
    (setf (gethash "role" (first contents)) "user")
    (setf (gethash "parts" (first contents)) user-parts)
    (setf (gethash "text" (first user-parts))
          (format nil "~A~%~%~A~%~%~A"
                  "=== CONTEXT (structured facts, Rete-pruned) ==="
                  context
                  user-message))
    (setf (gethash "contents" body) contents)
    (when (and system-prompt (plusp (length system-prompt)))
      (let ((sys-inst (make-hash-table :test 'equal))
            (sys-parts (list (make-hash-table :test 'equal))))
        (setf (gethash "parts" sys-inst) sys-parts)
        (setf (gethash "text" (first sys-parts)) system-prompt)
        (setf (gethash "systemInstruction" body) sys-inst)))
    (let ((gen-config (make-hash-table :test 'equal)))
      (setf (gethash "maxOutputTokens" gen-config) (llm-max-tokens))
      (setf (gethash "generationConfig" body) gen-config))
    (let* ((json-body (com.inuoe.jzon:stringify body))
           (endpoint (gemini-endpoint))
           (headers (gemini-headers)))
      (when *debug-mode*
        (format t "~&[DEBUG llm] gemini endpoint=~A~%" endpoint)
        (format t "~&[DEBUG llm] gemini body=~A~%" json-body))
      (handler-case
          (let* ((raw-response (dexador:request endpoint
                                                :method :post
                                                :headers headers
                                                :content json-body))
                 (response (if (stringp raw-response)
                               raw-response
                               (coerce raw-response 'string)))
               (resp-obj (com.inuoe.jzon:parse response)))
            (when *debug-mode*
              (format t "~&[DEBUG llm] gemini response=~A~%" response))
            (capture-usage resp-obj)
            (let ((candidates (gethash "candidates" resp-obj)))
              (when (and candidates (plusp (length candidates)))
                (let ((parts (gethash "parts" (gethash "content" (elt candidates 0)))))
                  (when (and parts (plusp (length parts)))
                    (gethash "text" (elt parts 0)))))))
        (error (e)
          (format t "~&[DEBUG llm] gemini ERROR class=~A msg=~A~%" (class-name (class-of e)) e)
          (format nil "[LLM ERROR] ~A" e))))))

(defun call-llm (system-prompt context user-message)
  "Dispatch to the configured LLM provider.
    OpenAI-compatible and Gemini providers use a tool-use loop so the model
    can request exec/read/write actions. Other providers return plain text."
  (let ((provider (llm-provider)))
    (cond
      ((string-equal provider "anthropic")
       (call-anthropic system-prompt context user-message))
      ((string-equal provider "ollama")
       (call-ollama system-prompt context user-message))
      ((gemini-p)
       (call-gemini system-prompt context user-message))
      (t (call-llm-with-tools system-prompt context user-message)))))
