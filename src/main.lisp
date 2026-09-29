(in-package :cl-harness)

;;; ============================================
;;; Entry point — (cl-harness:start)
;;; ============================================

(defun main ()
  "Main entry point. Can be called from CLI or REPL."
  (load-config)
  (start-harness))

(defun run-command ()
  "Entry point for the 'run' subcommand."
  (main))

(defun emergency-flush ()
  "Best-effort persistence used from signal handlers: write metrics and save
   the current session so a run killed by an external SIGTERM/SIGINT still
   leaves an audit trail instead of dying silently."
  (ignore-errors (metrics-to-json))
  (ignore-errors (save-session)))

(defun install-signal-handlers ()
  "Catch SIGTERM/SIGINT and flush metrics + session before exiting. A tool
   command issued by the agent (e.g. `pkill -f \"uvicorn app:app\"`) can match
   the harness process itself; without this the process dies mid-turn with no
   metrics written. Exit code 130 mirrors a conventional signal death."
  #+sbcl
  (dolist (sig (list sb-unix:sigterm sb-unix:sigint))
    (sb-sys:enable-interrupt
     sig
     (lambda (&rest args)
       (declare (ignore args))
       (format t "~&~%[SIGNAL] termination requested — flushing metrics/session...~%")
       (finish-output)
       (emergency-flush)
       (sb-ext:exit :code 130)))))


(defun one-shot-message ()
  "Resolve the one-shot prompt without exposing it in the process argv.
   Filtra el flag --debug para que no se meta en el mensaje."
  (let ((env (sb-ext:posix-getenv "CL_HARNESS_PROMPT"))
        (args (remove "--debug" (rest sb-ext:*posix-argv*) :test #'string=)))
    (cond
      ((and env (plusp (length env))) env)
      (args (format nil "~{~A~^ ~}" args))
      (t nil))))

(defun standalone-toplevel ()
  "Toplevel for a standalone executable."
  (install-signal-handlers)
  (with-simple-restart (abort "Exit cl-harness (aborted)")
    (let* ((debug-p (member "--debug" sb-ext:*posix-argv* :test #'string=))
           (message (one-shot-message)))
      (if message
          (run-one-shot message :reset-engine t :debug debug-p)
          (start-harness t)))))

