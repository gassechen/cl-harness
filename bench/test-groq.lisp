(load "~/quicklisp/setup.lisp")
(ql:quickload :cl-harness)
(in-package :cl-harness)

;;; Prueba de Groq como alternativa a OpenRouter.
;;; Usa config.groq.example.json como configuracion.
;;; Reemplaza TU_GROQ_API_KEY con tu key real de https://console.groq.com

(load-config "/home/mtk/TEST-PRUEBAS/cl-harness/config.groq.example.json")
(reset)
(metrics-reset)
(setf *debug-mode* t)

(format t "~&=== GROQ TEST ===~%")
(format t "~&Provider: ~A~%" (llm-provider))
(format t "~&Model: ~A~%" (llm-model))
(format t "~&Endpoint: ~A~%" (openai-compat-endpoint))

;; Ejecutar un comando para tener contexto
(let ((result (exec-command "ls -la")))
  (format t "~&exec exit=~A~%" (getf result :exit-code)))

;; Turno de usuario
(let ((sp (load-system-prompt)))
  (process-turn "que archivos hay?" sp))

(format t "~&=== END GROQ TEST ===~%")
(quit)
