(load "~/quicklisp/setup.lisp")
(ql:quickload :cl-harness)
(in-package :cl-harness)

(load-config)
(reset)
(metrics-reset)
(setf *debug-mode* t)

(format t "~&=== DEBUG SESSION ===~%")

;; 1. Ejecutar comando (simula :exec ls -a)
(format t "~&--- Executing :exec ls -a ---~%")
(let ((result (exec-command "ls -a")))
  (format t "~&exit=~A output=~A~%" (getf result :exit-code) (getf result :output)))

;; 2. Turno normal del usuario (simula escribir un mensaje al LLM)
(let ((sp (load-system-prompt)))
  (format t "~&--- Process turn: 'que archivos hay?' ---~%")
  (process-turn "que archivos hay?" sp))

(format t "~&=== END DEBUG SESSION ===~%")
(uiop:quit)
