(load "~/quicklisp/setup.lisp")
(ql:quickload :cl-harness)
(in-package :cl-harness)

;;; Phase 0 benchmark — deterministic scenario.
;;; Replays a short "session" and measures, per turn:
;;;   - curated context tokens (what we actually send)
;;;   - naive baseline tokens (all facts verbatim, no pruning/caps)
;;;   - real prompt/completion tokens reported by the provider (if online)
;;; Run: sbcl --script bench/scenario-basic.lisp

(load-config)
(reset)
(metrics-reset)

;; Build a representative working file
(write-file "/tmp/bench-sample.txt"
            (with-output-to-string (s)
              (dotimes (i 400)
                (format s "line ~A alpha beta gamma~%" (1+ i)))))

(defun status () (format t "~%[~A facts so far]~%" (length (collect-active-facts))))

(read-file "/tmp/bench-sample.txt")
(exec-command "ls -la")
(status)

(let ((sp (load-system-prompt)))
  ;; Turn 1 — plain question
  (process-turn "What is in the current directory?" sp)
  ;; Turn 2 — read the sample, then ask about it (first dup read)
  (read-file "/tmp/bench-sample.txt")
  (process-turn "How many lines does bench-sample.txt have?" sp)
  ;; Turn 3 — repeat the read again (second duplicate) + another command
  (read-file "/tmp/bench-sample.txt")
  (exec-command "grep -c gamma /tmp/bench-sample.txt")
  (process-turn "Show me the lines that mention gamma." sp)
  ;; Turn 4 — unrelated command to grow memory
  (exec-command "df -h")
  (process-turn "Reply with exactly: OK" sp))

(format t "~&========================================~%")
(metrics-summary)
(metrics-to-json)
(format t "~&DONE.~%")
(uiop:quit)