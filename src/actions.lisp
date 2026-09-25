(in-package :cl-harness)

;;; ============================================
;;; POSIX actions — read, write, exec
;;; ============================================
;;;
;;; IMPORTANT (Lisa DSL): in assert patterns, a bare *symbol* as a slot
;;; value is treated as a literal constant symbol (e.g. (data result)
;;; stores the symbol RESULT). To store a computed Lisp value, the slot
;;; value MUST be a non-trivial expression (CONS form) so that Lisa
;;; evaluates it: (data (list :command ...)). Lexical variables are
;;; captured in compiled contexts.

(defun read-file-contents (full)
  "Read FULL as a text string. Never NUL-pads: the previous implementation
   sized a buffer with (make-string (file-length s)), and FILE-LENGTH is in
   BYTES while the buffer counts CHARACTERS — any file containing multi-byte
   UTF-8 runs left the buffer tail filled with #\\Null padding, corrupting
   the content served to the model (and any rewrite via edit-file).
   Falls back to a lossy byte decode when the file is not valid UTF-8."
  (handler-case
      (uiop:read-file-string full :external-format :utf-8)
    (error ()
      ;; Raw-byte fallback for non-UTF-8 files. The byte buffer is sized by
      ;; file-length exactly (octets), so there is no char/byte mismatch.
      (with-open-file (s full :direction :input :element-type '(unsigned-byte 8))
        (let ((buf (make-array (file-length s)
                               :element-type '(unsigned-byte 8))))
          (read-sequence buf s)
          (babel:octets-to-string buf :encoding :utf-8 :errorp nil))))))

(defun resolve-path (path)
  "Resolve a RELATIVE PATH against the harness base directory, so the LLM
   can reference project files without absolute paths."
  (let ((pn (pathname path)))
    (if (uiop:absolute-pathname-p pn)
        pn
        (merge-pathnames pn (harness-base-dir)))))

(defun sh-quote (s)
  "Single-quote a string for /bin/sh, escaping embedded single quotes.
   Enables wrapping a whole command with `sh -c '...'` even when the command
   itself contains double quotes (python -c \"...\") or single quotes.
   El reemplazo usa una FUNCIÓN: en la cadena de reemplazo de cl-ppcre, `\\`
   inicia una referencia al grupo, así que pasar \"'\\\\''\" a secas no escapaba
   nada y `(sh-quote \"it's\")` devolvía `'it's's'`, que /bin/sh parte en tres
   palabras."
  (format nil "'~A'"
          (cl-ppcre:regex-replace-all "'" s (lambda (m &rest registers)
                                            (declare (ignore m registers))
                                            "'\\''"))))

(defun strip-cd-prefix (command)
  "Drop a leading `cd ABSOLUTE &&` the model often emits, since the harness
   already runs commands inside the configured working directory. Any
   leading absolute cd is redundant and usually points at a path that does
   not exist in the sandbox; relative cd (e.g. `cd synth`) is preserved."
  (labels ((strip (cmd)
             (multiple-value-bind (m match)
                 (cl-ppcre:scan-to-strings
                  "^[ \t]*cd[ \t]+([^ ;|&]+)[ \t]*(&&)?[ \t]*(.*)$" cmd)
               (if (and m (not (null (aref match 0))))
                   (let ((dir (aref match 0)))
                     (if (uiop:absolute-pathname-p dir)
                         (strip (aref match 2))
                         cmd))
                   cmd))))
    (strip command)))

(defun wrap-with-timeout (command)
  "Prefix COMMAND with `timeout -k 5 N` when tool_timeout_seconds > 0."
  (let ((ttl (tool-timeout-seconds)))
    (if (and ttl (plusp ttl))
        (format nil "timeout -k 5 ~A sh -c ~A" ttl (sh-quote command))
        command)))

(defun exec-command (command &optional (workdir (harness-base-dir)))
  "Execute a shell command, return the real exit-code and combined output
   (stderr merged into stdout). Non-zero exits are captured instead of
   signalled, and execution is bounded by tool_timeout_seconds. If the
   command exceeds the timeout AND background_on_timeout is enabled (the
   default), it is GENERICALLY relaunched detached (start-background) and
   returned as a daemon with a logfile — no server-name heuristics involved.
   A genuine launch failure is stored as exit -1 with the :launch-failed
   marker. Registers the result as a fact in Rete. WORKDIR defaults to the
   harness base directory so commands always run inside the project dir."
  (let* ((command (strip-cd-prefix command))
         (full-cmd (if workdir
                       (format nil "cd ~S && ~A" (namestring workdir) command)
                       command))
         (wrapped (wrap-with-timeout full-cmd)))
    (handler-case
        (let* ((buf (make-string-output-stream))
               (code (handler-case
                         (progn
                           (uiop:run-program wrapped :output buf :error-output :output)
                           0)
                       (uiop:subprocess-error (e)
                         (or (uiop:subprocess-error-code e) -1))))
               (output (get-output-stream-string buf))
               (timed-out (if (and (integerp code) (member code '(124 137))) t nil)))
          (assert (harness-fact (fact-type "command-exec")
                                (timestamp (get-universal-time))
                                (data (list :command command
                                            :output output
                                            :exit-code code
                                            :timed-out timed-out
                                            :turn-id (current-turn-id)
                                            :parent-id (current-turn-id)))))
          (when *debug-mode*
            (format t "~&[DEBUG exec-command] cmd=~A exit=~A output-len=~A~%"
                    command code (length output))
            (format t "~&[DEBUG exec-command] output:~%~A~%" output))
          (if (and timed-out (background-on-timeout-p))
              ;; Generic daemon relaunch: the command kept running past the
              ;; timeout, so hand it to the model as a live background
              ;; process instead of a dead timeout result.
              (let ((bg (start-background command workdir)))
                (list :command command :output output :exit-code code
                      :timed-out t :backgrounded-p t
                      :logfile (getf bg :logfile)
                      :message (getf bg :message)))
              (list :command command :output output :exit-code code
                    :timed-out timed-out)))
      (error (e)
        (let ((output (format nil "ERROR: ~A" e)))
          (assert (harness-fact (fact-type "command-exec")
                                (timestamp (get-universal-time))
                                (data (list :command command
                                            :output output
                                            :exit-code -1
                                            :launch-failed t
                                            :turn-id (current-turn-id)
                                            :parent-id (current-turn-id)))))
          (when *debug-mode*
            (format t "~&[DEBUG exec-command] cmd=~A exit=-1 error=~A~%" command e))
          (list :command command :output output :exit-code -1 :launch-failed t))))))

(defun read-file (path)
  "Read a file, return its contents (relative paths resolve against the
   harness base directory).
   Registers as a fact in Rete."
  (let* ((full (resolve-path path))
         (contents
           (if (uiop:file-exists-p full) 
               (read-file-contents full)
               (format nil "ERROR: File not found: ~A" (namestring full)))))
    (assert (harness-fact (fact-type "file-read")
                          (timestamp (get-universal-time))
                          (data (list :path (namestring full)
                                      :contents contents
                                      :turn-id (current-turn-id)
                                      :parent-id (current-turn-id)))))
    (list :path (namestring full) :contents contents)))

(defun count-occurrences (text needle)
  "Count non-overlapping occurrences of NEEDLE in TEXT using CL's SEARCH."
  (let ((count 0)
        (pos 0))
    (loop
      (setf pos (search needle text :start2 pos))
      (when (null pos) (return count))
      (incf count)
      (incf pos (length needle)))))

(defun edit-file (path old-string new-string)
  "Replace the first exact occurrence of OLD-STRING with NEW-STRING in PATH,
   rewriting the file in place. Pure CL (SEARCH + CONCATENATE), no external
   dependencies, so files of any size survive intact.
   Guard: an EMPTY OLD-STRING is rejected. (SEARCH \"\" ...) returns 0, which
   would silently PREPEND NEW-STRING to the file instead of replacing anything
   — a quiet corruption of the user's source. The guard lives in the ACTION,
   not in the batch validator, so it protects every caller (batch mode, native
   tool-use mode and direct calls).
   Registers as a fact in Rete."
  (let* ((full (resolve-path path)))
    (when (zerop (length old-string))
      (assert (harness-fact (fact-type "file-edit")
                            (timestamp (get-universal-time))
                            (data (list :path (namestring full)
                                        :applied nil
                                        :reason "old_string must not be empty"
                                        :turn-id (current-turn-id)
                                        :parent-id (current-turn-id)))))
      (return-from edit-file
        (list :path (namestring full)
              :applied nil
              :error (format nil "old_string must not be empty: an empty old_string cannot identify what to replace in ~A. Replace a non-empty exact snippet instead (and keep that snippet inside new_string when inserting)."
                             (namestring full)))))
    (let* ((contents (if (probe-file full)
                         (read-file-contents full)
                         (return-from edit-file
                           (list :path (namestring full) :error "File not found"))))
           (pos (search old-string contents)))
      (if (null pos)
          (progn
            (assert (harness-fact (fact-type "file-edit")
                                  (timestamp (get-universal-time))
                                  (data (list :path (namestring full)
                                              :applied nil
                                              :reason "old_string not found"
                                              :turn-id (current-turn-id)
                                              :parent-id (current-turn-id)))))
            (list :path (namestring full) :applied nil
                  :error (format nil "old_string not found in ~A"
                                 (namestring full))))
          (let* ((new-contents
                   (concatenate 'string
                                (subseq contents 0 pos)
                                new-string
                                (subseq contents (+ pos (length old-string)))))
                 (hits (count-occurrences contents old-string)))
            (ensure-directories-exist full)
            (with-open-file (s full :direction :output :if-exists :supersede)
              (write-string new-contents s))
            (assert (harness-fact (fact-type "file-edit")
                                  (timestamp (get-universal-time))
                                  (data (list :path (namestring full)
                                              :applied t
                                              :replaced-chars (length old-string)
                                              :new-chars (length new-string)
                                              :matches hits
                                              :turn-id (current-turn-id)
                                              :parent-id (current-turn-id)))))
            (list :path (namestring full)
                  :applied t
                  :matches hits
                  :replaced-chars (length old-string)
                  :new-chars (length new-string)))))))




(defun write-file (path content)
  "Write content to a file (relative paths resolve against the harness
   base directory). Registers as a fact in Rete.

   Guard: write_file is meant for CREATING files. If the target already
   exists with significant content, we refuse and point the model to
   edit_file — otherwise weak models re-emit entire big files and truncate
   them mid-way, corrupting the workspace."
  ;; ACÁ AGREGAMOS LA LIMPIEZA DE SALTOS DE LÍNEA:
  (let* ((full (resolve-path path))
         (clean-content (cl-ppcre:regex-replace-all "\\\\n" content (string #\Newline)))
         (clean-content (cl-ppcre:regex-replace-all "\\\\t" clean-content (string #\Tab))))
    (when (probe-file full)
      (let ((size (with-open-file (s full) (file-length s))))
        (when (> size 2000)
          (return-from write-file
            (list :path (namestring full)
                  :refused t
                  :error (format nil "~A already exists (~A bytes). write_file is only for NEW files; to modify an existing file use edit_file with the exact snippet to replace."
                                 (namestring full) size))))))
    (ensure-directories-exist full)
    ;; ACÁ USAMOS CLEAN-CONTENT EN VEZ DE CONTENT:
    (with-open-file (s full :direction :output :if-exists :supersede)
      (write-string clean-content s))
    ;; ACÁ TAMBIÉN USAMOS CLEAN-CONTENT:
    (let ((bytes (length clean-content)))
      (assert (harness-fact (fact-type "file-write")
                            (timestamp (get-universal-time))
                            (data (list :path (namestring full)
                                        :bytes bytes
                                        :turn-id (current-turn-id)
                                        :parent-id (current-turn-id)))))
      (list :path (namestring full) :bytes bytes))))

