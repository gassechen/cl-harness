(in-package :cl-harness)

;;; ============================================
;;; LLM API communication via dexador
;;; ============================================

(defparameter *last-llm-usage* nil
  "Usage (:prompt N :completion N) of the last successful LLM call. For tool
   loops these are CUMULATIVE totals across all API iterations of the turn.")

(defparameter *last-llm-call-info* nil
  "Metadata of the most recent LLM turn call:
   (:iterations N :path PATH) where PATH is one of:
   :final (normal answer) | :tool-loop (aborted by loop detector) |
   :api-error | :exhaustion-partial (iterations exhausted, partial text kept) |
   :exhaustion-error (iterations exhausted, no text).")

(defun llm-stream-p ()
  "Whether to stream LLM responses to stdout as they arrive (opencode-style
   live output with tool-use activity banners). Enabled by default; disable
   with \"llm_stream\": false in config.json."
  (multiple-value-bind (v present) (config-or-false "llm_stream")
    (cond ((and present (null v)) nil)
          (t t))))

(defun capture-usage (resp-obj &optional (prompt-key "prompt_tokens")
                                   (completion-key "completion_tokens"))
  "Read usage metrics from a parsed API response, if present. Only updates
   the global when a usage object exists: streaming chunks that carry no
   usage must NOT overwrite the accumulated value with NIL."
  (let ((usage (or (gethash "usage" resp-obj)
                   (gethash "usageMetadata" resp-obj))))
    (when usage
      (setf *last-llm-usage*
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
    ;; write_file tool
    (let* ((path-prop (make-hash-table :test 'equal))
           (content-prop (make-hash-table :test 'equal))
           (properties (make-hash-table :test 'equal))
           (params (make-hash-table :test 'equal))
           (func (make-hash-table :test 'equal))
           (fn (make-hash-table :test 'equal)))
      (setf (gethash "type" path-prop) "string")
      (setf (gethash "description" path-prop) "Absolute or relative path to the file")
      (setf (gethash "path" properties) path-prop)
      (setf (gethash "type" content-prop) "string")
      (setf (gethash "description" content-prop) "Full file contents to write (new files only; large existing files are refused — use edit_file to modify)")
      (setf (gethash "content" properties) content-prop)
      (setf (gethash "type" params) "object")
      (setf (gethash "properties" params) properties)
      (setf (gethash "required" params) (list "path" "content"))
      (setf (gethash "name" func) "write_file")
      (setf (gethash "description" func) "Create a NEW file with the given content (creates parent dirs). If the target already exists, it is overwritten ONLY when small (<= 2000 bytes); an existing file larger than that is REFUSED — for modifying existing files use edit_file with the exact snippet. Use write_file instead of shell heredocs/sed for new files so the change is recorded as a structured fact.")
      (setf (gethash "parameters" func) params)
      (setf (gethash "type" fn) "function")
      (setf (gethash "function" fn) func)
      (push fn tools))
    ;; edit_file tool
    (let* ((path-prop (make-hash-table :test 'equal))
           (old-prop (make-hash-table :test 'equal))
           (new-prop (make-hash-table :test 'equal))
           (properties (make-hash-table :test 'equal))
           (params (make-hash-table :test 'equal))
           (func (make-hash-table :test 'equal))
           (fn (make-hash-table :test 'equal)))
      (setf (gethash "type" path-prop) "string")
      (setf (gethash "description" path-prop) "Absolute or relative path to the file")
      (setf (gethash "path" properties) path-prop)
      (setf (gethash "type" old-prop) "string")
      (setf (gethash "description" old-prop) "The exact text to find (must be present in the file, ideally unique)")
      (setf (gethash "old_string" properties) old-prop)
      (setf (gethash "type" new-prop) "string")
      (setf (gethash "description" new-prop) "The replacement text")
      (setf (gethash "new_string" properties) new-prop)
      (setf (gethash "type" params) "object")
      (setf (gethash "properties" params) properties)
      (setf (gethash "required" params) (list "path" "old_string" "new_string"))
      (setf (gethash "name" func) "edit_file")
      (setf (gethash "description" func) "Replace a small exact snippet in a file with new text (surgical edit). Unlike write_file, it does not rewrite the whole file, so it works for any size and costs few tokens. Use this as the PRIMARY way to modify existing files; read_file first to get exact old text.")
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
             (if (getf result :backgrounded-p)
                 (format nil "~A" (getf result :message))
                 (format nil "Command: ~A~%Exit code: ~A~%Output:~%~A"
                         cmd (getf result :exit-code) (getf result :output))))
           "Error: missing command argument")))
    ((string-equal tool-name "read_file")
     (let ((path (gethash "path" arguments)))
       (if path
           (let ((result (read-file path)))
             (format nil "File: ~A~%Contents:~%~A"
                     path (getf result :contents)))
           "Error: missing path argument")))
    ((string-equal tool-name "write_file")
     (let ((path (gethash "path" arguments))
           (content (gethash "content" arguments)))
       (if (and path content)
           (let ((result (write-file path content)))
             (if (getf result :refused)
                 (format nil "write_file refused: ~A" (getf result :error))
                 (format nil "Wrote ~A bytes to ~A"
                         (getf result :bytes) (getf result :path))))
           "Error: missing path or content argument")))
    ((string-equal tool-name "edit_file")
     (let ((path (gethash "path" arguments))
           (old-string (gethash "old_string" arguments))
           (new-string (gethash "new_string" arguments)))
       (if (and path old-string new-string)
           (let ((result (edit-file path old-string new-string)))
             (if (getf result :applied)
                 (format nil "Edited ~A: replaced ~A chars with ~A chars (~A match~:P found)"
                         (getf result :path)
                         (getf result :replaced-chars)
                         (getf result :new-chars)
                         (getf result :matches))
                 (format nil "Edit failed: ~A" (getf result :error))))
           "Error: missing path, old_string or new_string argument")))
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

(defun truncate-tool-result (text)
  "Bound a tool result to max-tool-result-chars when it goes back into the
   intra-turn message pile. Keeps the HEAD and the TAIL of the output (with an
   explicit omission notice) so a huge file cannot swamp the prompt AND the
   model can still reach code that lives past the truncation point. The full
   output stays in the Rete fact; only the copy sent to the model is trimmed."
  (let ((max (max-tool-result-chars)))
    (if (and max (plusp max) (> (length text) max))
        (let* ((head-len (floor (* max 0.6)))
               (tail-len (floor (* max 0.4)))
               (omitted (- (length text) head-len tail-len)))
          (format nil "~A~%~%... [TRUNCATED]: original ~A chars, ~A omitted from the middle. Use read_file/grep/sed to inspect a specific section ...~%~A"
                  (subseq text 0 head-len)
                  (length text) omitted
                  (subseq text (- (length text) tail-len))))
        text)))

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
       (raw-text (execute-tool name args-obj))
       (result-text (truncate-tool-result raw-text))
       (tool-msg (make-hash-table :test 'equal)))
  (record-tool-call name raw-text result-text)
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
    (let ((total-text (make-string-output-stream))
          (tot-prompt 0)
          (tot-completion 0))
      (dotimes (iteration (llm-max-tool-iterations))
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
                (setf *last-llm-call-info*
                      (list :iterations (1+ iteration) :path :api-error))
                (return-from stream-llm-with-tools
                  (format nil "[LLM API ERROR] ~A" api-error)))
              ;; Accumulate per-request usage into CUMULATIVE turn totals, so
              ;; metrics reflect what the tool loop really cost (each request
              ;; re-sends the whole growing message pile).
              (let* ((p (getf *last-llm-usage* :prompt))
                     (c (getf *last-llm-usage* :completion)))
                (when (numberp p) (incf tot-prompt p))
                (when (numberp c) (incf tot-completion c)))
              (setf *last-llm-usage*
                    (list :prompt tot-prompt :completion tot-completion))
              (if (zerop (hash-table-count tool-buf))
                  (let ((text (get-output-stream-string content-buf)))
                    (write-string text total-text)
                    (when (plusp (length text))
                      (write-char #\Newline (llm-stream-out)))
                    (finish-output (llm-stream-out))
                    (setf *last-llm-call-info*
                          (list :iterations (1+ iteration) :path :final))
                    (return-from stream-llm-with-tools text))
                  ;; Preserve any text streamed alongside tool calls so a
                  ;; partial answer survives iteration exhaustion.
                  (let ((text (get-output-stream-string content-buf)))
                    (write-string text total-text)
                    (setf messages
                          (stream-execute-tool-calls messages tool-buf))
                    (let ((warning (tool-loop-warning)))
                      (when warning
                        (write-string warning (llm-stream-out))
                        (write-char #\Newline (llm-stream-out))
                        (finish-output (llm-stream-out))
                        (setf *last-llm-call-info*
                              (list :iterations (1+ iteration) :path :tool-loop))
                        (return-from stream-llm-with-tools warning)))
                    (write-char #\Newline (llm-stream-out))
                    (finish-output (llm-stream-out))))))))
      ;; Graceful end: report exhaustion explicitly instead of returning NIL
      ;; silently. If the model STREAMED some content while calling tools, keep
      ;; it but MARK it as partial (it is NOT a complete answer); otherwise it
      ;; is a hard error.
      (let ((text (get-output-stream-string total-text)))
        (if (plusp (length text))
            (progn
              (format (llm-stream-out)
                      "~&[LLM PARCIAL: se agotaron ~A iteraciones de tool-use sin respuesta final; se conserva lo emitido]~%"
                      (llm-max-tool-iterations))
              (finish-output (llm-stream-out))
              (setf *last-llm-call-info*
                    (list :iterations (llm-max-tool-iterations)
                          :path :exhaustion-partial))
              (format nil "[LLM PARCIAL] ~A iteraciones agotadas sin respuesta final; texto parcial:~%~A"
                      (llm-max-tool-iterations) text))
            (progn
              (setf *llm-streamed* nil)
              (setf *last-llm-call-info*
                    (list :iterations (llm-max-tool-iterations)
                          :path :exhaustion-error))
              (format nil "[LLM ERROR] reached ~A tool iterations without a final answer"
                      (llm-max-tool-iterations))))))))

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
                        (make-hash-table :test 'equal)))
        (tot-prompt 0)
        (tot-completion 0))
    (setf (gethash "role" (first messages)) "system")
    (setf (gethash "content" (first messages)) system-prompt)
    (setf (gethash "role" (second messages)) "user")
    (setf (gethash "content" (second messages))
          (format nil "~A~%~%~A~%~%~A"
                  "=== CONTEXT (structured facts, Rete-pruned) ==="
                  context
                  user-message))
(setf *llm-streamed* nil)
    (dotimes (iteration (llm-max-tool-iterations))
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
                               (setf *last-llm-call-info*
                                     (list :iterations (1+ iteration)
                                           :path :transport-error))
                               (return-from call-llm-with-tools/static (format nil "[LLM ERROR] ~A" e)))))
             (response (decode-response raw-response))
             (resp-obj (handler-case (com.inuoe.jzon:parse response)
                         (error (e)
                           (setf *last-llm-call-info*
                                 (list :iterations (1+ iteration)
                                       :path :json-error))
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
            (setf *last-llm-call-info*
                  (list :iterations (1+ iteration) :path :api-error))
            (return-from call-llm-with-tools/static (format nil "[LLM API ERROR] ~A" err-msg))))
        (capture-usage resp-obj)
        (let ((p (getf *last-llm-usage* :prompt))
              (c (getf *last-llm-usage* :completion)))
          (when (numberp p) (incf tot-prompt p))
          (when (numberp c) (incf tot-completion c)))
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
                           (raw-text (execute-tool tool-name args-obj))
                           (result-text (truncate-tool-result raw-text))
                           (tool-result-msg (make-hash-table :test 'equal)))
                      (when *debug-mode*
                        (format t "~&[DEBUG llm] tool_result=~A~%" result-text))
                      (record-tool-call tool-name raw-text result-text)
                      (setf (gethash "role" tool-result-msg) "tool")
                      (setf (gethash "content" tool-result-msg) result-text)
                      (setf (gethash "tool_call_id" tool-result-msg) tool-call-id)
                      (setf messages (append messages (list tool-result-msg))))))
                ;; Stop the turn early when the model is stuck re-running the
                ;; same failing command (imperative companion to the Rete rule).
                (let ((warning (tool-loop-warning)))
                  (when warning
                    (setf *last-llm-usage*
                          (list :prompt tot-prompt :completion tot-completion))
                    (setf *last-llm-call-info*
                          (list :iterations (1+ iteration) :path :tool-loop))
                    (return-from call-llm-with-tools/static warning))))
              (progn
                (setf *last-llm-usage*
                      (list :prompt tot-prompt :completion tot-completion))
                (setf *last-llm-call-info*
                      (list :iterations (1+ iteration) :path :final))
                (return-from call-llm-with-tools/static
                  (when msg (gethash "content" msg)))))))
    ;; Graceful end: report exhaustion instead of returning NIL silently.
    (setf *last-llm-usage*
          (list :prompt tot-prompt :completion tot-completion))
    (setf *last-llm-call-info*
          (list :iterations (llm-max-tool-iterations) :path :exhaustion-error))
    (format nil "[LLM ERROR] reached ~A tool iterations without a final answer"
            (llm-max-tool-iterations)))))




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
					      :read-timeout 120
                                              :force-binary t))
               ;;(response (if (stringp raw-response)
               ;;              raw-response
               ;;              (coerce raw-response 'string)))
	                      (response (decode-response raw-response))
	       
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


;;
;; BATCH MODE
;;
(defun get-batch-instructions ()
  "Instrucciones batch."
  (format nil "~%
<batch_execution_mode>
  <rules>
    <rule>Output ONLY S-expressions. No XML, no markdown, no explanations.</rule>
    <rule>If the context already contains what you need, respond with text and stop.</rule>
    <rule>Do NOT repeat actions already present in the context.</rule>
    <rule>For write-file with multi-line content or content containing quotes, use a heredoc (any variant: &lt;&lt;EOF, &lt;&lt;&lt;EOF, &lt;&lt;'EOF'). The content is written verbatim, no escaping needed.</rule>
  </rules>
  <actions>
    <action>(read-file \"path\")</action>
    <action>(exec-command \"cmd\")</action>
    <action>(write-file \"path\" \"content\")</action>
    <action>(write-file \"path\" &lt;&lt;EOF
content
EOF
)</action>
    <action>(edit-file \"path\" \"old\" \"new\")</action>
  </actions>
</batch_execution_mode>~%"))



;; (defun assert-intention-from-form (form step-id)
;;   "Convierte una S-expression parseada en un hecho de intención en Rete."
;;   (when (consp form)
;;     (let* ((action (first form))
;;            (args (rest form))
;;            (data (case action
;;                    (read-file (list :action :read-file :path (first args)))
;;                    (exec-command (list :action :exec-command :command (first args)))
;;                    (write-file (list :action :write-file :path (first args) :content (second args)))
;;                    (edit-file (list :action :edit-file :path (first args) :old-string (second args) :new-string (third args)))
;;                    (otherwise nil))))
;;       (when data
;; 	(format t "~&[DEBUG parser] Asserting intention: ~A~%" data)
;;         (assert (harness-fact
;;                  (fact-type "intention")
;;                  (timestamp (get-universal-time))
;;                   (data (append data (list :step step-id :status :pending)))))))))

(defun escape-for-lisp (s)
  "Escapa S para que sea válido dentro de un string Lisp."
  (with-output-to-string (out)
    (loop for c across s do
      (case c
        (#\" (write-string "\\\"" out))
        (#\\ (write-string "\\\\" out))
        (t   (write-char c out))))))



(defun preprocess-heredocs (text)
  "Convierte (write-file \"path\" <<DELIM ... DELIM) en un string Lisp.
   Acepta las variantes que el LLM manda sin avisar:
     <<EOF   <<<EOF   <<'EOF'   <<<'EOF'   <<\"EOF\"   <<<\"EOF\"   << EOF"
  (let ((result text)
        (pos 0))
    (loop
      (let ((wf-pos (search "(write-file" result :start2 pos)))
        (when (null wf-pos) (return result))
        (let* ((path-q1 (position #\" result :start wf-pos))
               (path-q2 (and path-q1 (position #\" result :start (1+ path-q1))))
               (hd-base (and path-q2 (search "<<" result :start2 (1+ path-q2)))))
          (cond
            ((null hd-base)
             (setf pos (1+ wf-pos)))
            (t
             (let* ((after-lt (+ hd-base 2))
                    (delim-start
                      (cond
                        ;; <<<EOF  ->  saltear el < extra
                        ((and (< after-lt (length result))
                              (char= (char result after-lt) #\<))
                         (1+ after-lt))
                        ;; <<'EOF' o <<"EOF"  ->  saltear la comilla
                        ((and (< after-lt (length result))
                              (or (char= (char result after-lt) #\')
                                  (char= (char result after-lt) #\")))
                         (1+ after-lt))
                        ;; <<EOF  ->  directo
                        (t after-lt)))
                    (delim-end (or (position #\Newline result :start delim-start)
                                   (length result)))
                    (delim (string-trim '(#\Space #\Tab #\Return #\' #\" #\<)
                                        (subseq result delim-start delim-end)))
                    (content-start (if (< delim-end (length result))
                                       (1+ delim-end)
                                       delim-end))
                    (close-pattern (format nil "~%~A" delim))
                    (close-pos (and (plusp (length delim))
                                    (search close-pattern result :start2 content-start))))
               (cond
                 ((null close-pos)
                  (setf pos (1+ wf-pos)))
                 (t
                  (let* ((content (subseq result content-start close-pos))
                         (escaped (escape-for-lisp content))
                         (before  (subseq result 0 (1+ path-q2)))
                         (after   (subseq result (+ close-pos 1 (length delim))))
                         (new-result (concatenate 'string
                                                  before
                                                  " \"" escaped "\""
                                                  after)))
                    (setf result new-result)
                    (setf pos (+ (1+ path-q2) 3 (length escaped))))))))))))))


(defun fix-c-escapes (text)
  "Convierte secuencias de escape estilo C (\\n, \\t) a sus equivalentes reales.
   Necesario porque el reader de CL interpreta \\n como la letra 'n'."
  (let ((result text))
    (setf result (cl-ppcre:regex-replace-all "\\\\n" result (string #\Newline)))
    (setf result (cl-ppcre:regex-replace-all "\\\\t" result (string #\Tab)))
    result))



(defun clean-llm-text (text)
  "Normaliza el texto del LLM. Traduce tool-use nativo y arregla sintaxis rota."
  (let ((clean text))
    ;; 1. Remover code fences de markdown
    (setf clean (cl-ppcre:regex-replace-all "```[a-zA-Z]*" clean ""))
    ;; 2. Remover tags XML simples (<exec-command>, </batch_execution_mode>)
    (setf clean (cl-ppcre:regex-replace-all "</?[a-zA-Z_\\-]+>" clean ""))
    ;; 3. read-filepath path -> (read-file "path")
    (setf clean (cl-ppcre:regex-replace-all "read-filepath\\s*\"?([^\"\\n<]+)\"?" clean "(read-file \"\\1\")"))
    ;; 4. <exec-command("cmd")> -> (exec-command "cmd")
    (setf clean (cl-ppcre:regex-replace-all "<exec-command\\(\"(.*?)\"\\)>" clean "(exec-command \"\\1\")"))
    ;; 5. read-file sin paréntesis -> (read-file "path")
    (setf clean (cl-ppcre:regex-replace-all "(?<!\\()read-file\\s+\"(.*?)\"" clean "(read-file \"\\1\")"))
    ;; 6. exec-command sin paréntesis -> (exec-command "cmd")
    (setf clean (cl-ppcre:regex-replace-all "(?<!\\()exec-command\\s+\"(.*?)\"" clean "(exec-command \"\\1\")"))
    ;; 7. Tool-use nativo: read_file(path='...') -> (read-file "...")
    (setf clean (cl-ppcre:regex-replace-all "read_file\\(path='([^']+)'\\)" clean "(read-file \"\\1\")"))
    ;; 8. Tool-use nativo: exec_command(command='...') -> (exec-command "...")
    (setf clean (cl-ppcre:regex-replace-all "exec_command\\(command='([^']+)'\\)" clean "(exec-command \"\\1\")"))
    ;; 9. Tool-use nativo: write_file(path='...', content='...') -> (write-file "..." "...")
    (setf clean (cl-ppcre:regex-replace-all "write_file\\(path='([^']+)',\\s*content='([^']*)'\\)" clean "(write-file \"\\1\" \"\\2\")"))
    ;; 10. Limpiar tokens especiales de tool-call (después de traducir, no antes)
    (setf clean (cl-ppcre:regex-replace-all "<\\|tool_call_start\\|>" clean ""))
    (setf clean (cl-ppcre:regex-replace-all "<\\|tool_call_end\\|>" clean ""))
    (setf clean (cl-ppcre:regex-replace-all "<\\|/?tool_calls?\\|>" clean ""))
    (setf clean (cl-ppcre:regex-replace-all "<\\|/?function_calls?\\|>" clean ""))
    ;; 11. Corchetes sueltos de listas tool-call [a, b] -> a b
    (setf clean (cl-ppcre:regex-replace-all "\\[\\s*" clean " "))
    (setf clean (cl-ppcre:regex-replace-all "\\s*\\]" clean " "))
    ;; 12. Comas separadoras entre S-expressions -> espacio
    (setf clean (cl-ppcre:regex-replace-all "\\)\\s*,\\s*\\(" clean ") ("))
    ;; 13. Convertir escapes tipo C en strings simples (antes del heredoc)
    (setf clean (fix-c-escapes clean))
    ;; 14. Extraer heredocs VERBATIM (el contenido no se toca)
    (setf clean (preprocess-heredocs clean))
    clean))



(defun assert-intention-from-form (form step-id)
  "Convierte una S-expression parseada en un hecho de intención en Rete."
  (when (consp form)
    (let* ((action (first form))
           (args (rest form))
           ;; ACÁ FORZAMOS QUE TODO SEA STRING (Anticrash por over-escaping del LLM)
           (str-args (mapcar (lambda (x) (if (stringp x) x (princ-to-string x))) args))
           (data (cond
                   ((string-equal action "read-file")
                    (list :action :read-file :path (first str-args)))
                   ((string-equal action "exec-command")
                    (list :action :exec-command :command (first str-args)))
                   ((string-equal action "write-file")
                    (list :action :write-file :path (first str-args) :content (second str-args)))
                   ((string-equal action "edit-file")
                    (list :action :edit-file :path (first str-args) :old-string (second str-args) :new-string (third str-args)))
                   (t nil))))
      (when data
        (format t "~&[DEBUG parser] Asserting intention: ~A~%" data)
        (assert (harness-fact
                 (fact-type "intention")
                 (timestamp (get-universal-time))
                 (data (append data (list :step step-id :status :pending)))))))))


(defun parse-llm-batch-to-intentions (text)
  "Escanea el texto del LLM, busca S-expressions y las aserta en Rete."
  (let ((parsed-something nil))
    (when (and text (stringp text) (plusp (length text)))
      (let ((clean-text (clean-llm-text text)))
        (handler-case
            (loop with pos = 0
                  for step-id from 1
                  while (< pos (length clean-text))
                  do (multiple-value-bind (form new-pos)
                         (read-from-string clean-text nil :eof :start pos)
                     (when (eq form :eof) (return))
                     (when (assert-intention-from-form form step-id)
                       (setf parsed-something t))
                     (setf pos new-pos)))
          (error () nil))))
    ;; NUEVO: Si no parseó nada, el LLM dio su respuesta final. Le avisamos a Rete.
    (unless parsed-something
      (assert (harness-fact 
               (fact-type "batch-complete")
               (timestamp (get-universal-time))
               (data (list :turn-id (current-turn-id))))))
    parsed-something))



(defun call-llm (system-prompt context user-message)
  "Dispatch to the configured LLM provider."
  (let ((provider (llm-provider)))
    (cond
      ((string-equal provider "anthropic")
       (call-anthropic system-prompt context user-message))
      ((string-equal provider "ollama")
       (call-ollama system-prompt context user-message))
      ((gemini-p)
       (call-gemini system-prompt context user-message))
            ((string-equal provider "batch")
       (let* ((full-prompt (concatenate 'string system-prompt (get-batch-instructions)))
              (llm-text (call-openai-compat full-prompt context user-message)))
         ;; CAMBIÁ EL FORMAT PARA USAR ~S:
         (format t "~&[DEBUG call-llm] Texto recibido: ~S~%" llm-text)
         (parse-llm-batch-to-intentions llm-text)
     llm-text))

      (t 
       (call-llm-with-tools system-prompt context user-message)))))


