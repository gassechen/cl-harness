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
