(load "~/quicklisp/setup.lisp")
(ql:quickload :cl-harness)
(in-package :cl-harness)

;;; Prueba de Google Gemini.
;;; Uso:
;;;   1. Copiá config.gemini.example.json -> config.json
;;;   2. Pone tu API key de Google AI Studio (https://aistudio.google.com)
;;;   3. Ejecutá: sbcl --load run.lisp

(load-config)
(reset)
(metrics-reset)
(setf *debug-mode* t)

(format t "~&=== GEMINI TEST ===~%")
(format t "~&Provider: ~A~%" (llm-provider))
(format t "~&Model: ~A~%" (llm-model))
(format t "~&Endpoint: ~A~%" (gemini-endpoint))

;; Ejecutar un comando para tener contexto
(let ((result (exec-command "ls -la")))
  (format t "~&exec exit=~A~%" (getf result :exit-code)))

;; Turno de usuario
(let ((sp (load-system-prompt)))
  (process-turn "que archivos hay?" sp))

(format t "~&=== END GEMINI TEST ===~%")
(quit)
