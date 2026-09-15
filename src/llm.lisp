(in-package :cl-harness)

;;; ============================================
;;; LLM API communication via dexador
;;; ============================================

(defparameter *last-llm-usage* nil
  "Usage (:prompt N :completion N) of the last successful LLM call.")

(defun llm-stream-p ()
  "Whether to stream LLM responses to stdout as they arrive (opencode-style
   live output with tool-use activity banners). Enabled by default; disable
   with \"llm_stream\": false in config.json."
  (let ((v (config-value "llm_stream")))
    (cond ((eq v :false) nil)
          ((null v) t)
          (t (not (eq v :false))))))

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

;;; ============================================
;;; SSE streaming (opencode-style live output)
;;; ============================================

(defparameter *stream-output* nil
  "When non-nil, redirect live LLM deltas and tool banners to this stream.
   When NIL (the default), output goes to the CURRENT *standard-output*,
   which is safe even inside saved SBCL core images.")

(defun llm-stream-out ()
  "Resolve the live output stream.  Uses *STREAM-OUTPUT* if explicitly
   set, otherwise falls back to the dynamically-bound *STANDARD-OUTPUT*."
  (or *stream-output* *standard-output*))

(defparameter *llm-streamed* nil
  "Set to T when the last LLM call streamed its output.")

(defun parse-sse-data (line)
  "Extract and parse the JSON payload of an SSE 'data: ...' line.
   Returns the parsed object, or NIL for [DONE]/blank/skip lines."
  (when (and line (plusp (length line)))
    (let ((line (string-trim '(#\Space #\Tab #\Return) line)))
      (when (>= (length line) 5)
        (let ((payload (subseq line 5)))
          (handler-case
              (if (or (string= payload "[DONE]") (string= payload ""))
                  nil
                  (com.inuoe.jzon:parse payload))
            (error () nil)))))))

(defun sse-read-line (stream)
  "Read one SSE line from a stream. Uses READ-LINE (works with dexador
   DECODING-STREAM returned by :want-stream t without force-binary). Returns
   :EOF at end of stream."
  (read-line stream nil :eof))

(defun stream-content-delta (text)
  "Print a streamed content delta and flush (live output)."
  (when (and text (plusp (length text)))
    (write-string text (llm-stream-out))
    (finish-output (llm-stream-out))))

(defun stream-tool-banner (name args-raw)
  "Print a live banner when the model requests a tool."
  (let* ((args (string-trim '(#\Space #\Newline #\Tab) args-raw))
         (preview (subseq args 0 (min 80 (length args)))))
    (format (llm-stream-out) "~& ⚙ ~A ~A~%" name preview)))

(defun stream-tool-result (result-text)
  "Print a live banner when a tool execution finishes."
  (format (llm-stream-out) "  ✓ ~A chars received~%" (length result-text)))

(defun stream-process-chunk (chunk content-buf tool-buf)
  "Apply one SSE chunk to the content accumulator and tool-call buffer.
   Returns the finish reason (or NIL). Streaming deltas may arrive in
   'content' (final answer) and 'reasoning' (model thinking); both are shown
   live but only 'content' is kept in CONTENT-BUF."
  (let ((choices (and (hash-table-p chunk) (gethash "choices" chunk)))
        (finish nil))
    (when (hash-table-p chunk)
      (capture-usage chunk))
    (when (and choices (plusp (length choices)))
      (let* ((choice (elt choices 0))
             (delta (and (hash-table-p choice) (gethash "delta" choice))))
        (when (hash-table-p delta)
          (let ((text (gethash "content" delta))
                (reasoning (gethash "reasoning" delta)))
            (when *debug-mode*
              (format t "~&[DEBUG chunk] content=~S reasoning=~S~%"
                      (and (stringp text) (subseq text 0 (min 40 (length text))))
                      (and (stringp reasoning) (subseq reasoning 0 (min 40 (length reasoning))))))
            (when (and text (stringp text) (plusp (length text)))
              (write-string text content-buf)
              (stream-content-delta text))
            (when (and reasoning (stringp reasoning) (plusp (length reasoning)))
              (stream-content-delta reasoning)))
          (dolist (tc (let ((tcs (gethash "tool_calls" delta)))
                        (if (listp tcs) tcs (coerce tcs 'list))))
            (when (hash-table-p tc)
              (let* ((index (or (gethash "index" tc) 0))
                     (acc (or (gethash index tool-buf)
                              (setf (gethash index tool-buf)
                                    (list :id nil :name nil :args ""))))
                     (fn (gethash "function" tc)))
                (when (gethash "id" tc)
                  (setf (getf acc :id) (gethash "id" tc)))
                (when (hash-table-p fn)
                  (when (gethash "name" fn)
                    (setf (getf acc :name) (gethash "name" fn)))
                  (let ((args (gethash "arguments" fn)))
                    (when (and args (stringp args))
                      (setf (getf acc :args)
                            (concatenate 'string (getf acc :args) args)))))))))
        (setf finish (and (hash-table-p choice) (gethash "finish_reason" choice)))))
    finish))

(defun stream-tool-call-to-json (acc-tool)
  "Convert an accumulated tool-call plist to an OpenAI tool_calls hash."
  (let ((tc (make-hash-table :test 'equal))
        (fn (make-hash-table :test 'equal)))
    (setf (gethash "id" tc) (or (getf acc-tool :id) ""))
    (setf (gethash "type" tc) "function")
    (setf (gethash "name" fn) (or (getf acc-tool :name) ""))
    (setf (gethash "arguments" fn) (getf acc-tool :args))
    (setf (gethash "function" tc) fn)
    tc))

(defun stream-execute-tool-calls (messages tool-buf)
  "Execute all accumulated tool calls, print live banners, append assistant +
   tool messages. Returns the appended MESSAGES list (unchanged when no tools)."
  (if (zerop (hash-table-count tool-buf))
      messages
      (progn
        (let ((a-msg (make-hash-table :test 'equal))
              (tcs '()))
          (setf (gethash "role" a-msg) "assistant")
          (setf (gethash "content" a-msg) 'null)
          (maphash (lambda (index acc)
                     (declare (ignore index))
                     (push (stream-tool-call-to-json acc) tcs))
                   tool-buf)
          (setf (gethash "tool_calls" a-msg) tcs)
          (setf messages (append messages (list a-msg))))
        (maphash (lambda (index acc)
                   (declare (ignore index))
                   (let* ((name (or (getf acc :name) ""))
                          (args-raw (getf acc :args)))
                     (stream-tool-banner name args-raw)
                     (let* ((args-obj (handler-case
                                          (com.inuoe.jzon:parse args-raw)
                                        (error () (make-hash-table :test 'equal))))
                            (result-text (execute-tool name args-obj))
                            (tool-msg (make-hash-table :test 'equal)))
                       (stream-tool-result result-text)
                       (setf (gethash "role" tool-msg) "tool")
                       (setf (gethash "content" tool-msg) result-text)
                       (setf (gethash "tool_call_id" tool-msg) (getf acc :id))
                       (setf messages (append messages (list tool-msg))))))
                 tool-buf)
        messages)))

(defun stream-llm-with-tools (system-prompt context user-message)
  "Stream an OpenAI-compatible tool call until the model gives a text answer.
   Falls back to the non-streaming static path on transport errors."
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
    (setf *llm-streamed* t)
    (dotimes (iteration 5)
      (let* ((body (build-openai-compat-messages-with-tools
                    system-prompt context user-message messages))
             (json-body (progn
                          (setf (gethash "stream" body) t)
                          (com.inuoe.jzon:stringify body)))
             (endpoint (openai-compat-endpoint))
             (headers (openai-compat-headers)))
        (when *debug-mode*
          (format t "~&[DEBUG stream] iteration=~A endpoint=~A body=~A~%"
                  (1+ iteration) endpoint json-body))
        (let ((stream
                (handler-case
                    (dexador:request endpoint
                                     :method :post
                                     :headers headers
                                     :content json-body
                                     :want-stream t)
                  (error (e)
                    (when *debug-mode*
                      (format t "~&[DEBUG stream] transport error: ~A~%" e))
                    (return-from stream-llm-with-tools
                      (call-llm-with-tools/static
                       system-prompt context user-message))))))
          (let ((content-buf (make-string-output-stream))
                (tool-buf (make-hash-table :test #'eql))
                (api-error nil))
            (handler-case
                  (loop for line = (sse-read-line stream)
                        while (and line (not (eq line :eof)))
                        for chunk = (parse-sse-data line)
                        do (when *debug-mode*
                             (format t "~&[DEBUG sse] line=~A~%" line))
                        when chunk do
                          (if (gethash "error" chunk)
                              (setf api-error
                                    (gethash "message"
                                             (if (hash-table-p (gethash "error" chunk))
                                                 (gethash "error" chunk)
                                                 chunk)))
                              (stream-process-chunk chunk content-buf tool-buf)))
                (error (e)
                  (when *debug-mode*
                    (format t "~&[DEBUG stream] read error: ~A~%" e))))
            (when api-error
              (setf *llm-streamed* nil)
              (return-from stream-llm-with-tools
                (format nil "[LLM API ERROR] ~A" api-error)))
            (if (zerop (hash-table-count tool-buf))
                (let ((text (get-output-stream-string content-buf)))
                  (when (plusp (length text))
                    (write-char #\Newline (llm-stream-out)))
                  (finish-output (llm-stream-out))
                  (return-from stream-llm-with-tools text))
                (progn
                  (setf messages
                        (stream-execute-tool-calls messages tool-buf))
                  (write-char #\Newline (llm-stream-out))
                  (finish-output (llm-stream-out))))))))
    (error "stream-llm-with-tools: max iterations")
    nil))

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

(defun call-llm-with-tools/static (system-prompt context user-message)
  "Non-streaming OpenAI-compatible provider call with tool definitions. If the
   model requests tools, execute them and loop until we get a text response or
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
    (setf *llm-streamed* nil)
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
                               (return-from call-llm-with-tools/static (format nil "[LLM ERROR] ~A" e)))))
             (response (decode-response raw-response))
             (resp-obj (handler-case (com.inuoe.jzon:parse response)
                         (error (e)
                           (return-from call-llm-with-tools/static (format nil "[LLM JSON ERROR] ~A" e)))))
             (err-obj (when (hash-table-p resp-obj) (gethash "error" resp-obj))))
        (when *debug-mode*
          (format t "~&[DEBUG llm] iteration=~A endpoint=~A~%" (1+ iteration) endpoint)
          (format t "~&[DEBUG llm] body=~A~%" json-body)
          (format t "~&[DEBUG llm] response=~A~%" response))
        (when err-obj
          (let ((err-msg (or (and (hash-table-p err-obj) (gethash "message" err-obj)) response)))
            (when *debug-mode*
              (format t "~&[DEBUG llm] API ERROR: ~A~%" err-msg))
            (return-from call-llm-with-tools/static (format nil "[LLM API ERROR] ~A" err-msg))))
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
              (return-from call-llm-with-tools/static (when msg (gethash "content" msg)))))))))




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

(defun call-llm-with-tools (system-prompt context user-message)
  "Dispatch OpenAI-compatible tool calls: streaming (live output) when
   (llm-stream-p) is active, otherwise the static buffered path."
  (if (llm-stream-p)
      (stream-llm-with-tools system-prompt context user-message)
      (call-llm-with-tools/static system-prompt context user-message)))

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
