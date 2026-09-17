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
   Prefers CL_HARNESS_PROMPT (so an agent's `pkill -f <prompt fragment>`
   cannot match this process); falls back to joined argv for compatibility.
   Returns NIL when no message was given (interactive REPL)."
  (let ((env (sb-ext:posix-getenv "CL_HARNESS_PROMPT"))
        (args (rest sb-ext:*posix-argv*)))
    (cond
      ((and env (plusp (length env))) env)
      (args (format nil "~{~A~^ ~}" args))
      (t nil))))

(defun standalone-toplevel ()
  "Toplevel for a standalone executable built WITHOUT a baked session
   (build.sh → bin/cl-harness). Fresh engine at boot: config.json/dumps/
   sessions/metrics resolve against the runtime working directory, so the
   binary can be copied anywhere. One-shot message via CL_HARNESS_PROMPT (or
   argv fallback); otherwise the interactive REPL."
  (install-signal-handlers)
  (with-simple-restart (abort "Exit cl-harness (aborted)")
    (let ((message (one-shot-message)))
      (if message
          (run-one-shot message t)
          (start-harness t)))))
