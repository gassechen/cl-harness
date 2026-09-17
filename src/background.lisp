(in-package :cl-harness)

;;; ============================================
;;; Background process execution (Bordeaux Threads)
;;; ============================================
;;;
;;; Long-running commands (web servers, daemons) must NOT block the harness
;;; turn loop. Detection is GENERIC: no hardcoded server names or stacks —
;;; any command that exceeds tool_timeout_seconds is relaunched detached and
;;; handed back to the model as a daemon with a logfile. State lives in the
;;; global registry so the model can inspect/stop them with exec_command
;;; (tail logfile, kill/pkill).

(defvar *background-processes* (make-hash-table :test #'equal))
(defvar *background-lock* (bt:make-lock "bg-processes"))
(defvar *background-counter* 0)

(defun make-bg-logfile ()
  "Allocate a fresh logfile path for a background process, stored next to the
   rest of the harness artifacts (harness-base-dir) instead of a hardcoded
   /tmp directory shared by every project."
  (ensure-directories-exist
   (merge-pathnames "bg/" (harness-base-dir)))
  (namestring
   (merge-pathnames (format nil "bg-~A.log" (incf *background-counter*))
                    (merge-pathnames "bg/" (harness-base-dir)))))

(defun bg-key (pid)
  "Registry key for a background process (string, for hash table)."
  (format nil "~A" pid))

(defun background-status ()
  "Summarize registered background processes for the model/context."
  (let ((lines '()))
    (bt:with-lock-held (*background-lock*)
      (maphash (lambda (k rec)
                 (push (format nil "~A: status=~A pid=~A cmd=~A log=~A"
                               k (getf rec :status) (getf rec :pid)
                               (getf rec :command) (getf rec :logfile))
                       lines))
               *background-processes*))
    (if lines
        (format nil "Background processes:~%~{~A~%~}" (nreverse lines))
        "No background processes running.")))

(defun stop-background (pid-string)
  "Terminate a registered background process by its PID (string subprocess)."
  (let ((pid (parse-integer pid-string :junk-allowed t)))
    (if pid
        (progn
          (with-open-file (s "/dev/null")
            (uiop:run-program (format nil "kill ~A 2>/dev/null" pid) :output s))
          (bt:with-lock-held (*background-lock*)
            (let ((rec (gethash (bg-key pid) *background-processes*)))
              (when rec
                (setf (getf rec :status) :stopped)
                (setf (getf rec :exit-code) -9))))
          (format nil "Stopped background process ~A." pid))
        "Error: invalid PID.")))

(defun start-background (command &optional (workdir (harness-base-dir)))
  "Launch COMMAND detached from the harness, returning immediately.
   A worker thread watches the process, records state/exit-code in the
   registry, and the process outlives the harness run (daemon)."
  (let* ((logfile (make-bg-logfile))
         (full-cmd (if workdir
                       (format nil "cd ~S && ~A" (namestring workdir)
                               (strip-cd-prefix command))
                       command)))
    (bt:make-thread
      (lambda ()
        (handler-case
            (let ((proc (uiop:launch-program
                         (list "/bin/sh" "-c" full-cmd)
                         :output logfile
                         :error-output logfile
                         :wait nil)))
              (let ((pid (uiop:process-info-pid proc))
                    (key (bg-key (uiop:process-info-pid proc))))
                (bt:with-lock-held (*background-lock*)
                  (setf (gethash key *background-processes*)
                        (list :command command
                              :pid pid
                              :logfile logfile
                              :status :running
                              :proc proc)))
                (let ((code (uiop:wait-process proc)))
                  (bt:with-lock-held (*background-lock*)
                    (let ((rec (gethash key *background-processes*)))
                      (when rec
                        (setf (getf rec :status) :exited)
                        (setf (getf rec :exit-code) code)))))))
          (error (e)
            (bt:with-lock-held (*background-lock*)
              ;; No PID exists on launch failure; key the record by the
              ;; logfile allocated for THIS attempt (stable, unique) instead
              ;; of allocating a second, mismatched key.
              (setf (gethash logfile *background-processes*)
                    (list :command command
                          :logfile logfile
                          :status :launch-failed
                          :error (princ-to-string e)))))))
      :name "cl-harness-bg")
    (list :background t
          :command command
          :logfile logfile
          :message (format nil
                            "[BACKGROUND] The command exceeded the tool timeout, so it was relaunched DETACHED as a daemon (the harness is NOT blocked and the process is now live in the background). This was only the LAUNCH step — your task is NOT finished. You MUST continue the workflow: (1) wait briefly (exec_command \"sleep 2\"), (2) verify the process actually started by reading its log with exec_command (cat ~A) and, if it is expected to expose an endpoint or produce output, probe/interact with it the way its project defines, (3) fix anything in the log that failed. Only end your turn after you have confirmed with a real check that the daemon is alive and working. You can stop it at any time with a kill/pkill via exec_command. Output log: ~A." logfile logfile))))

(defun shutdown-background-processes ()
  "Kill any live background processes (called at harness shutdown when
   configured to do so)."
  (let ((pids '()))
    (bt:with-lock-held (*background-lock*)
      (maphash (lambda (k rec)
                 (declare (ignore k))
                 (when (eq (getf rec :status) :running)
                   (let ((proc (getf rec :proc)))
                     (when proc
                       (handler-case (uiop:terminate-process proc)
                         (error () nil))))))
               *background-processes*))))