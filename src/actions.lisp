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

(defun resolve-path (path)
  "Resolve a RELATIVE PATH against the harness base directory, so the LLM
   can reference project files without absolute paths."
  (let ((pn (pathname path)))
    (if (uiop:absolute-pathname-p pn)
        pn
        (merge-pathnames pn (harness-base-dir)))))

(defun exec-command (command &optional (workdir (harness-base-dir)))
  "Execute a shell command, return exit-code and combined output.
   Registers result as a fact in Rete. WORKDIR defaults to the harness
   base directory so commands always run inside the project dir."
  (handler-case
      (let* ((full-cmd (if workdir
                           (format nil "cd ~S && ~A" (namestring workdir) command)
                           command))
             (output (with-output-to-string (s)
                       (uiop:run-program full-cmd :output s :error-output :interactive))))
        (assert (harness-fact (fact-type "command-exec")
                              (timestamp (get-universal-time))
                              (data (list :command command
                                          :output output
                                          :exit-code 0
                                          :turn-id (current-turn-id)
                                          :parent-id (current-turn-id)))))
        (when *debug-mode*
          (format t "~&[DEBUG exec-command] cmd=~A exit=0 output-len=~A~%" command (length output))
          (format t "~&[DEBUG exec-command] output:~%~A~%" output))
        (list :command command :output output :exit-code 0))
    (error (e)
      (let ((output (format nil "ERROR: ~A" e)))
        (assert (harness-fact (fact-type "command-exec")
                              (timestamp (get-universal-time))
                              (data (list :command command
                                          :output output
                                          :exit-code -1
                                          :turn-id (current-turn-id)
                                          :parent-id (current-turn-id)))))
        (when *debug-mode*
          (format t "~&[DEBUG exec-command] cmd=~A exit=-1 error=~A~%" command e))
        (list :command command :output output :exit-code -1)))))

(defun read-file (path)
  "Read a file, return its contents (relative paths resolve against the
   harness base directory).
   Registers as a fact in Rete."
  (let* ((full (resolve-path path))
         (contents
           (if (probe-file full)
               (with-open-file (s full :direction :input)
                 (let ((buf (make-string (file-length s))))
                   (read-sequence buf s)
                   buf))
               (format nil "ERROR: File not found: ~A" (namestring full)))))
    (assert (harness-fact (fact-type "file-read")
                          (timestamp (get-universal-time))
                          (data (list :path (namestring full)
                                      :contents contents
                                      :turn-id (current-turn-id)
                                      :parent-id (current-turn-id)))))
    (list :path (namestring full) :contents contents)))

(defun write-file (path content)
  "Write content to a file (relative paths resolve against the harness
   base directory).
   Registers as a fact in Rete."
  (let ((full (resolve-path path)))
    (ensure-directories-exist full)
    (with-open-file (s full :direction :output :if-exists :supersede)
      (write-string content s))
    (let ((bytes (length content)))
      (assert (harness-fact (fact-type "file-write")
                            (timestamp (get-universal-time))
                            (data (list :path (namestring full)
                                        :bytes bytes
                                        :turn-id (current-turn-id)
                                        :parent-id (current-turn-id)))))
      (list :path (namestring full) :bytes bytes))))
