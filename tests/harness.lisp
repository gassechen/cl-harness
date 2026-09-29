;;;; Pruebas offline de cl-harness con Rove.
;;;;
;;;; Nada aquí toca la red: las rutas HTTP se prueban por su POLÍTICA
;;;; (retryable-http-status-p, backoff) y no por llamadas reales. Cada prueba
;;;; que toca Rete instala motores frescos, de modo que el orden de ejecución no
;;;; cambia el resultado.
(defpackage #:cl-harness/tests
  (:use #:cl)
  (:local-nicknames (#:h #:cl-harness)
                    (#:jzon #:com.inuoe.jzon))
  (:import-from #:rove #:ok #:ng)
  (:import-from #:com.inuoe.jzon #:parse #:stringify)
  (:import-from #:cl-harness
                ;; actions
                #:write-file #:read-file #:exec-command
                ;; engines
                #:*turn-engine* #:*mem-engine*
                #:with-turn-engine #:with-mem-engine
                #:reset-turn-engine #:reset-mem-engine
                #:durable-type-p #:promote-durable-facts #:mem-engine-facts
                #:collect-active-facts #:retract-oldest-of-type #:fact-slot
                ;; metrics
                #:metrics-reset #:build-yaml-context
                ;; config
                #:llm-provider #:llm-endpoint))

(in-package #:cl-harness/tests)

(defparameter *test-cases* nil
  "Lista de (nombre . funcion) en orden de definicion. La rellena DEFTEST.")

(defmacro deftest (name &body body)
  "Como ROVE:DEFTEST, pero ademas guarda el caso en *TEST-CASES* para que el
runner no dependa del registro interno de Rove: con ASDF compilando a fasl ese
registro no sobrevive a la carga, asi que ROVE:RUN-TESTSRespondia 'No test
found'. El HANDLER-CASE convierte cualquier error en un aserto fallido con el
mensaje, en lugar del generico 'Raise an error while testing'."
  (let ((fn (gensym "TEST")) (desc (string-downcase (symbol-name name))))
    `(progn
       (defun ,fn ()
         (handler-case
             (rove/core/test::with-testing-with-options ,desc (:name ',name)
               (handler-case
                   (progn ,@body)
                 (error (condition)
                   (format t ";; ERROR en ~A: ~A~%" ,desc condition)
                   (rove/fail :reason (format nil "~A" condition)))))
           (error (condition)
             (format t ";; ERROR NO CAPTURADO en ~A: ~A~%" ,desc condition))))
       (push (cons ',name #',fn) *test-cases*)
       ',name)))

(defun test-cases (&optional (filter ""))
  "Casos registrados, en orden, filtrados por subcadena del nombre."
  (let ((cases (reverse *test-cases*)))
    (if (plusp (length filter))
        (remove-if-not (lambda (c)
                         (search filter (string-downcase (symbol-name (car c)))))
                       cases)
        cases)))

(defun run-test-cases (filter)
  "Ejecuta los casos (o los que casen con FILTER) con el reporter de Rove.
Devuelve (fallos . total) leyendo las estadísticas de Rove."
  (let ((cases (test-cases filter))
        (failed 0))
    (format t "~&Ejecutando ~D prueba~:P~%" (length cases))
    ;; Dentro de WITH-REPORTER, *STATS* es el reporter: ahí quedan los fallos.
    (rove:with-reporter :spec
      (dolist (case cases)
        (funcall (cdr case)))
      (setf failed
            (length (rove/core/stats:all-failed-assertions rove/core/stats:*stats*))))
    (values failed (length cases))))

(defun main (filter)
  "Punto de entrada de run-tests.sh. Sale con 0 si todo pasa, 1 si falla algo.
   Durante la corrida DEXADOR:POST se sustituye por una señal: el suite es
   offline por contrato y así una fuga de red falla ruidosa en vez de gastar
   una llamada real contra el proveedor."
  (multiple-value-bind (failed total)
      (handler-case
          (let ((original (symbol-function 'dexador::post)))
            (unwind-protect
                 (progn
                   (setf (symbol-function 'dexador::post)
                         (lambda (&rest args)
                           (declare (ignore args))
                           (error "intento de llamada HTTP desde el suite offline")))
                   (run-test-cases filter))
              (setf (symbol-function 'dexador::post) original)))
        (error (e)
          (format t "~&~&ERROR EJECUTANDO EL SUITE: ~A~%" e)
          (values 1 1)))
    (format t "~&~&RESULTADO: ~A~%"
            (if (zerop failed) "OK" (format nil "~D/~D FALLOS" failed total)))
    (sb-ext:exit :code (if (zerop failed) 0 1))))


(defmacro with-mocked-call-llm (response &body body)
  "Ejecuta BODY con H::CALL-LLM sustituido globalmente por una lambda de tres
   argumentos que devuelve RESPONSE (una o mas formas).
   OJO: no sirve un FLET. PROCESS-TURN está compilado aparte, así que su llamada
   a CALL-LLM es una llamada de función global que FLET no puede interceptar; con
   FLET el test se-wasendo a la API real (401 de Anthropic) en vez de usar el
   mock. Hay que parchear SYMBOL-FUNCTION y restaurarlo al salir."
  (let ((original (gensym "ORIGINAL")))
    `(let ((,original (symbol-function 'h::call-llm)))
       (unwind-protect
            (progn
              (setf (symbol-function 'h::call-llm)
                    (lambda (system-prompt context user-message)
                      (declare (ignore system-prompt context user-message))
                      ,@response))
              ,@body)
         (setf (symbol-function 'h::call-llm) ,original)))))

(defmacro with-test-config (pairs &body body)
  "Evaluate BODY with cl-harness::*config* holding PAIRS, a list of (key value)."
  `(let ((h:*config* (make-hash-table :test 'equal)))
     ,@(mapcar (lambda (pair)
                  `(setf (gethash ,(first pair) h:*config*) ,(second pair)))
                pairs)
     ,@body))

(defmacro with-temp-dir ((var &optional (prefix "cl-harness-tests")) &body body)
  "Bind VAR to a fresh empty directory under TMPDIR, deleted afterwards.
   OJO: sin `:if-exists t` — la versión de ASDF de este entorno no acepta esa
   llave en DELETE-DIRECTORY-TREE y el error saltava en el UNWIND-PROTECT, al
   final de cada prueba, dejando además el directorio en /tmp."
  (let ((dir (gensym "DIR"))
        (name (format nil "~A-~D/" prefix (random 1000000))))
    `(let ((,dir (merge-pathnames ,name
                                 (uiop:ensure-directory-pathname
                                  (or (uiop:getenv "TMPDIR") "/tmp/")))))
       (ensure-directories-exist ,dir)
       (unwind-protect
            (let ((,var ,dir)) ,@body)
         (uiop:delete-directory-tree ,dir
                                     :validate t
                                     :if-does-not-exist :ignore)))))

(defun fact-count (type)
  "How many facts of TYPE live in the turn engine right now."
  (with-turn-engine
    (count-if (lambda (tp) (string= tp type))
              (mapcar (lambda (f) (h::fact-type-of f)) (h::collect-harness-facts)))))

(defun read-temp (path)
  (h::read-file-contents path))

(defun jzon-list (value)
  "Coerce VALUE to a list. Jzon devuelve los arrays JSON como vectores y en este
   Lisp CAR/FIRST/MAPCARE no aceptan vectores, hay que pasarlos por LIST."
  (cond ((null value) nil)
        ((listp value) value)
        ((vectorp value) (coerce value 'list))
        (t (list value))))

;;; ---------------------------------------------------------------
;;; Batch JSON: normalización y validación (src/llm.lisp)
;;; ---------------------------------------------------------------

(deftest batch-parsing/valid-batch
  (with-turn-engine
    (reset-turn-engine)
    (multiple-value-bind (ok-p response error)
        (h::parse-llm-batch-to-intentions
         "{\"tool_calls\":[{\"id\":\"1\",\"type\":\"function\",\"function\":{\"name\":\"read_file\",\"arguments\":{\"path\":\"a.lisp\"}}}]}")
      (ok (null error) (format nil "error: ~A" error))
      (ok ok-p "devuelve T tras asertar las intenciones")
      (ng response)
      (ok (= 1 (fact-count "intention"))
          (format nil "tipos: ~S" (mapcar #'h::fact-type-of (h::collect-harness-facts)))))))

(deftest batch-parsing/empty-old-string-rejected
  (with-turn-engine
    (reset-turn-engine)
    (multiple-value-bind (ok-p response error)
        (h::parse-llm-batch-to-intentions
         "{\"tool_calls\":[{\"id\":\"1\",\"type\":\"function\",\"function\":{\"name\":\"edit_file\",\"arguments\":{\"path\":\"a.lisp\",\"old_string\":\"\",\"new_string\":\"x\"}}}]}")
      (ng ok-p "una edición con old_string vacío debe rechazarse")
      (ok (search "old_string" error))
      (ok (= 0 (fact-count "intention")) "no debe asertar intenciones"))))

(deftest batch-parsing/unknown-tool-rejected
  (with-turn-engine
    (reset-turn-engine)
    (multiple-value-bind (ok-p response error)
        (h::parse-llm-batch-to-intentions
         "{\"tool_calls\":[{\"id\":\"1\",\"type\":\"function\",\"function\":{\"name\":\"rm_rf\",\"arguments\":{}}}]}")
      (ng ok-p)
      (ok (search "rm_rf" error)))))

(deftest batch-parsing/missing-arguments-rejected
  (with-turn-engine
    (reset-turn-engine)
    (multiple-value-bind (ok-p response error)
        (h::parse-llm-batch-to-intentions
         "{\"tool_calls\":[{\"id\":\"1\",\"type\":\"function\",\"function\":{\"name\":\"exec_command\",\"arguments\":{}}}]}")
      (ng ok-p)
      (ok (search "command" error)))))

(deftest batch-parsing/invalid-batch-is-atomic
  "Una llamada inválida en medio del lote invalida TODO el lote: las intenciones
   válidas anteriores no deben ejecutarse."
  (with-turn-engine
    (reset-turn-engine)
    (multiple-value-bind (ok-p response error)
        (h::parse-llm-batch-to-intentions
         (concatenate 'string
                      "{\"tool_calls\":["
                      "{\"id\":\"1\",\"type\":\"function\",\"function\":{\"name\":\"read_file\",\"arguments\":{\"path\":\"ok.lisp\"}}},"
                      "{\"id\":\"2\",\"type\":\"function\",\"function\":{\"name\":\"edit_file\",\"arguments\":{\"path\":\"x\",\"old_string\":\"\",\"new_string\":\"y\"}}}]}"))
      (ng ok-p)
      (ok error)
      (ok (= 0 (fact-count "intention"))))))

(deftest batch-parsing/response-only-plan-dones
  (with-turn-engine
    (reset-turn-engine)
    (multiple-value-bind (ok-p response error)
        (h::parse-llm-batch-to-intentions "{\"tool_calls\":[],\"response\":\"listo\"}")
      (ng error)
      (ng ok-p)
      (ok (string= "listo" response))
      (ok (= 1 (fact-count "plan-done"))))))

(deftest batch-parsing/malformed-json-reported
  (with-turn-engine
    (reset-turn-engine)
    (multiple-value-bind (ok-p response error)
        (h::parse-llm-batch-to-intentions "esto no es json")
      (ng ok-p)
      (ng response)
      (ok (search "JSON" error)))))

(deftest batch-parsing/fenced-json-accepted
  "Muchos modelos envuelven el JSON en un bloque ```; hay que quitar las
   vallas. OJO: el payload se arma con `format`/`~%` y no con `\\n`, porque en
   este SBCL los escapes de tipo `\\n` en los literales devuelven la letra `n`."
  (with-turn-engine
    (reset-turn-engine)
    (let ((payload (format nil "```json~%{\"tool_calls\":[{\"id\":\"1\",\"type\":\"function\",\"function\":{\"name\":\"read_file\",\"arguments\":{\"path\":\"a.lisp\"}}}]}~%```")))
      (multiple-value-bind (ok-p response error)
          (h::parse-llm-batch-to-intentions payload)
        (ok (null error) (format nil "error: ~A" error))
        (ok ok-p)))))

(deftest batch-parsing/arguments-as-json-string
  "Muchos modelos serializan arguments como string JSON; debe aceptarse."
  (with-turn-engine
    (reset-turn-engine)
    (multiple-value-bind (ok-p response error)
        (h::parse-llm-batch-to-intentions
         "{\"tool_calls\":[{\"id\":\"1\",\"type\":\"function\",\"function\":{\"name\":\"write_file\",\"arguments\":\"{\\\"path\\\":\\\"a.lisp\\\",\\\"content\\\":\\\"hola\\\"}\"}}]}")
      (ok (null error) (format nil "error: ~A" error))
      (ok ok-p))))

(deftest batch-parsing/fingerprint-ignores-whitespace
  (let ((a "{\"tool_calls\":[{\"id\":\"1\",\"type\":\"function\",\"function\":{\"name\":\"read_file\",\"arguments\":{\"path\":\"a.lisp\"}}}]}")
        (b "{\"tool_calls\": [ { \"id\" : \"1\" , \"type\" : \"function\" , \"function\" : { \"name\" : \"read_file\" , \"arguments\" : { \"path\" : \"a.lisp\" } } } ] }"))
    (ok (eql (h::batch-fingerprint a) (h::batch-fingerprint b))
        "el mismo lote con otro espaciado debe tener la misma huella")))

(deftest batch-parsing/fingerprint-distinguishes-calls
  (let ((a "{\"tool_calls\":[{\"id\":\"1\",\"type\":\"function\",\"function\":{\"name\":\"read_file\",\"arguments\":{\"path\":\"a.lisp\"}}}]}")
        (b "{\"tool_calls\":[{\"id\":\"1\",\"type\":\"function\",\"function\":{\"name\":\"read_file\",\"arguments\":{\"path\":\"b.lisp\"}}}]}")
        (c "{\"tool_calls\":[{\"id\":\"1\",\"type\":\"function\",\"function\":{\"name\":\"read_file\",\"arguments\":{\"path\":\"a.lisp\"}}},{\"id\":\"2\",\"type\":\"function\",\"function\":{\"name\":\"read_file\",\"arguments\":{\"path\":\"c.lisp\"}}}]}"))
    (ng (eql (h::batch-fingerprint a) (h::batch-fingerprint b))
        "rutas distintas = lotes distintos")
    (ng (eql (h::batch-fingerprint a) (h::batch-fingerprint c))
        "un lote con una llamada más no es el mismo lote")))

;;; ---------------------------------------------------------------
;;; Acciones POSIX (src/actions.lisp)
;;; ---------------------------------------------------------------

(deftest actions/edit-file-rejects-empty-old-string
  (with-temp-dir (dir)
    (let* ((path (merge-pathnames "code.lisp" dir))
           (h::*base-dir* dir))
      (with-open-file (s path :direction :output :if-exists :supersede)
        (write-string "(defun a () 1)" s))
      (with-turn-engine
        (reset-turn-engine)
        (let ((result (h::edit-file "code.lisp" "" "(defun b () 2)")))
          (ng (getf result :applied) "un old_string vacío no debe aplicarse")
          ;; I6: el motivo del rechazo se llama :reason en TODO el sistema.
          ;; Antes estos tests afirmaban sobre :error, que era la mitad del
          ;; mismo concepto con dos nombres.
          (ok (getf result :reason))
          (ok (search "old_string" (getf result :reason)))
          (ng (getf result :matches))))
      (ok (string= "(defun a () 1)" (read-temp path))
          "el archivo no debe tocarse"))))

(deftest actions/edit-file-rejects-unknown-snippet
  (with-temp-dir (dir)
    (let* ((path (merge-pathnames "code.lisp" dir))
           (h::*base-dir* dir))
      (with-open-file (s path :direction :output :if-exists :supersede)
        (write-string "(defun a () 1)" s))
      (with-turn-engine
        (reset-turn-engine)
        (let ((result (h::edit-file "code.lisp" "(defun zzz () 9)" "x")))
          (ng (getf result :applied))
          (ok (search "not found" (getf result :reason)))))
      (ok (string= "(defun a () 1)" (read-temp path))))))

(deftest actions/edit-file-replaces-first-occurrence
  (with-temp-dir (dir)
    (let* ((path (merge-pathnames "code.lisp" dir))
           (h::*base-dir* dir))
      (with-open-file (s path :direction :output :if-exists :supersede)
        (write-string "a a a" s))
      (with-turn-engine
        (reset-turn-engine)
        (let ((result (h::edit-file "code.lisp" "a" "b")))
          (ok (getf result :applied))
          (ok (= 3 (getf result :matches)) "debe contar las 3 ocurrencias")))
      (ok (string= "b a a" (read-temp path))
          "sólo se reemplaza la primera ocurrencia"))))

(deftest actions/edit-file-missing-file
  (with-temp-dir (dir)
    (let ((h::*base-dir* dir))
      (with-turn-engine
        (reset-turn-engine)
        (let ((result (h::edit-file "no-existe.lisp" "a" "b")))
          (ng (getf result :applied))
          (ok (search "not found" (getf result :reason))))))))

(deftest actions/write-file-refuses-large-existing
  (with-temp-dir (dir)
    (let* ((path (merge-pathnames "big.lisp" dir))
           (h::*base-dir* dir))
      (with-open-file (s path :direction :output :if-exists :supersede)
        (dotimes (i 3000) (write-string "0123456789" s)))
      (with-turn-engine
        (reset-turn-engine)
        (let ((result (write-file "big.lisp" "nuevo")))
          (ok (getf result :refused) "un archivo existente grande no se sobrescribe")
          (ok (search "edit_file" (getf result :reason)))))
      (ok (> (with-open-file (s path) (file-length s)) 2000)
          "el archivo original no debe truncarse"))))

(deftest actions/write-file-converts-escaped-newlines
  (with-temp-dir (dir)
    (let* ((path (merge-pathnames "nuevo.lisp" dir))
           (h::*base-dir* dir))
      (with-turn-engine
        (reset-turn-engine)
        (write-file "nuevo.lisp" "uno\\ndos\\tcolumna"))
      (ok (string= (format nil "uno~Cdos~Ccolumna" #\Newline #\Tab) (read-temp path))))))

(deftest actions/read-file-missing-reports-error
  (with-temp-dir (dir)
    (let ((h::*base-dir* dir))
      (with-turn-engine
        (reset-turn-engine)
        ;; I6: el motivo ya no se disfraza de contenido. Este test afirmaba
        ;; que :contents contendria "File not found", o sea, que un hecho
        ;; file-read fallido era indistinguible de una lectura buena.
        (let* ((result (read-file "no-existe.lisp"))
               (contents (getf result :contents)))
          (ok (search "File not found" (getf result :reason)))
          (ok (not contents) "y :contents NO lleva el error dentro"))))))

(deftest actions/count-occurrences
  (ok (= 0 (h::count-occurrences "abc" "z")))
  (ok (= 3 (h::count-occurrences "aaa" "a")))
  (ok (= 2 (h::count-occurrences "aaaa" "aa")) "cuenta ocurrencias no solapadas"))

(deftest actions/strip-cd-prefix-drops-absolute-cd
  (ok (string= "ls" (h::strip-cd-prefix "cd /tmp/proyecto && ls")))
  (ok (string= "ls" (h::strip-cd-prefix "  cd /a/b/c && ls"))))

(deftest actions/strip-cd-prefix-keeps-relative-cd
  (ok (string= "cd sub && ls" (h::strip-cd-prefix "cd sub && ls"))
      "un cd relativo se conserva"))

(deftest actions/sh-quote-escapes-inner-quotes
  (ok (string= "'echo \"hola\"'" (h::sh-quote "echo \"hola\"")))
  (ok (string= "'it'\\''s'" (h::sh-quote "it's"))
      "las comillas simples internas se escapan"))

(deftest actions/resolve-path-relative-vs-absolute
  (with-temp-dir (dir)
    (let ((h::*base-dir* dir))
      (ok (uiop:absolute-pathname-p (h::resolve-path "/etc/hosts")))
      (ok (uiop:absolute-pathname-p (h::resolve-path "relativo.lisp"))))))

(deftest actions/exec-command-returns-real-exit-code
  (with-turn-engine
    (reset-turn-engine)
    (let ((ok-result (exec-command "echo hola")))
      (ok (= 0 (getf ok-result :exit-code)))
      (ok (search "hola" (getf ok-result :output))))
    (let ((bad (exec-command "exit 3")))
      (ok (= 3 (getf bad :exit-code)) "el exit code real debe conservarse"))))

;;; ---------------------------------------------------------------
;;; Reglas: errores, TTL, dedup, memoria durable (src/rules.lisp)
;;; ---------------------------------------------------------------

(deftest rules/real-error-detection
  (ok (h::real-error-p "command-exec" (list :exit-code 1 :output "boom"))
      "fallo con salida = error real")
  (ng (h::real-error-p "command-exec" (list :exit-code 0 :output "todo bien")))
  (ng (h::real-error-p "command-exec" (list :exit-code 1 :output "   "))
      "un sondeo benigno sin salida no se conserva")
  (ng (h::real-error-p "command-exec" (list :exit-code 1 :output "x" :timed-out t)))
  (ok (h::real-error-p "command-exec" (list :exit-code -1 :output "x" :launch-failed t))
      "fallo de lanzamiento es error real aunque haya salida")
  (ok (h::real-error-p "command-exec" (list :exit-code -1 :launch-failed t))))

(deftest rules/dedup-key-identity
  (ok (string= "ls" (h::dedup-key "command-exec" (list :command "ls"))))
  (ng (h::dedup-key "llm-response" (list :text "hola"))
      "las facts de conversación no se deduplican")
  (ok (string= "/a.lisp" (h::dedup-key "file-read" (list :path "/a.lisp")))))

(deftest rules/conversation-facts-never-expire
  (let ((h::*turn-counter* 9999))
    (ng (h::fact-expired-p "user-input" 0 (list :text "hola" :turn-id 1))
        "user-input nunca expira")
    (ng (h::fact-expired-p "llm-response" 0 (list :text "hola" :turn-id 1))
        "llm-response nunca expira")))

(deftest rules/evidential-ttl-expires
  (let ((h::*turn-counter* 100))
    ;; Very old wall-clock timestamp: 1 hour ago, default TTL is 30 min.
    (ok (h::fact-expired-p "file-read" (- (get-universal-time) 3600) (list :path "a" :turn-id 100))
        "un hecho evidencial antiguo expira")
    (ng (h::fact-expired-p "file-read" (get-universal-time) (list :path "a" :turn-id 100))
        "un hecho reciente no expira")))

(deftest rules/real-errors-are-ttl-exempt
  (let ((h::*turn-counter* 100))
    (ng (h::fact-expired-p "command-exec" (- (get-universal-time) 3600)
                          (list :command "ls" :exit-code 1 :output "boom" :turn-id 100))
        "un error real se conserva como contexto activo")
    (with-test-config (("conserve_errors" nil))
      (ok (h::fact-expired-p "command-exec" (- (get-universal-time) 3600)
                            (list :command "ls" :exit-code 1 :output "boom" :turn-id 100))
          "con conserve_errors desactivado expira como cualquier otro"))))

(deftest rules/durable-type-policy
  (ok (durable-type-p "command-exec" (list :exit-code 1 :output "boom")))
  (ng (durable-type-p "command-exec" (list :exit-code 0 :output "ok")))
  (ok (durable-type-p "file-write" (list :path "a" :bytes 3)))
  (ng (durable-type-p "file-read" (list :path "a" :contents "x"))
      "leer no es conocimiento durable"))

(deftest rules/promote-durable-facts-to-memory
  (reset-mem-engine)
  (with-turn-engine
    (reset-turn-engine)
    (h::assert (h::harness-fact (h::fact-type "command-exec")
                                (h::timestamp (get-universal-time))
                                (h::data (list :command "ls" :exit-code 1
                                               :output "boom"
                                               :turn-id 1 :parent-id 1))))
    (h::assert (h::harness-fact (h::fact-type "file-read")
                                (h::timestamp (get-universal-time))
                                (h::data (list :path "a" :contents "x"
                                               :turn-id 1 :parent-id 1))))
    (promote-durable-facts)
    (ok (= 1 (length (mem-engine-facts)))
        "sólo el error real llega a la memoria durable")))

(deftest rules/per-type-cap-retracts-oldest
  (with-turn-engine
    (reset-turn-engine)
    (dotimes (i 6)
      (h::assert (h::harness-fact (h::fact-type "file-read")
                                  (h::timestamp (+ (get-universal-time) i))
                                  (h::data (list :path (format nil "archivo-~D.lisp" i)
                                                 :contents (format nil "v~D" i)
                                                 :turn-id 1 :parent-id 1)))))
    (ok (= 6 (fact-count "file-read")))
    (with-test-config (("max_facts_per_type" 2))
      (build-yaml-context "hola"))
    (ok (= 2 (fact-count "file-read")) "el tope por tipo debe retirar los viejos")
    (let ((paths (mapcar (lambda (f) (h::data-get (h::fact-data-of f) :path))
                         (with-turn-engine (h::collect-harness-facts)))))
      (ok (member "archivo-5.lisp" paths :test #'string=) "el más nuevo se conserva")
      (ng (member "archivo-0.lisp" paths :test #'string=)))))

;;; ---------------------------------------------------------------
;;; Contexto (src/context.lisp)
;;; ---------------------------------------------------------------

(deftest context/truncate-payload-keeps-head-and-tail
  (let ((short "hola"))
    (ok (string= short (h::truncate-payload short 100))))
  (let* ((text (make-string 500 :initial-element #\x))
         (truncated (h::truncate-payload text 100)))
    (ng (string= text truncated))
    (ok (search "omitted" truncated) "debe avisar de la omisión")
    (ok (< (length truncated) 500))))

(deftest context/token-estimate
  (ok (= 1 (h::token-estimate "abcd")))
  (ok (= 2 (h::token-estimate "abcde")) "redondea hacia arriba")
  (ok (= 0 (h::token-estimate ""))))

(deftest context/build-yaml-context-renders-session
  (with-temp-dir (dir)
    (let ((h::*base-dir* dir)
          (h:*session-id* "session-test-123"))
      (with-turn-engine
        (reset-turn-engine)
        (h::assert (h::harness-fact (h::fact-type "user-input")
                                    (h::timestamp (get-universal-time))
                                    (h::data (list :text "hola" :turn-id 1))))
        (let ((yaml (build-yaml-context "hola")))
          (ok (search "context:" yaml))
          (ok (search "session-test-123" yaml))
          (ok (search "hola" yaml)))))))

;;;; ---------------------------------------------------------------
;;;; Goals: cierre, deduplicacion, orden y poda
;;;; ---------------------------------------------------------------

(defun todo-slot (todo name)
  "Slot NAME of a goal fact, read with the symbol interned in CL-HARNESS.

   The deftemplates live in CL-HARNESS, so their slot keys are CL-HARNESS
   symbols; a bare 'status here would be CL-HARNESS/TESTS::STATUS and quietly
   read NIL."
  (h::get-slot-value todo (find-symbol (string-upcase name) :cl-harness)))

(defun active-todos ()
  (sort (mapcar #'first (h::retrieve (?t) (?t (h::agent-todo))))
        #'<
        :key (lambda (todo)
               (h::todo-id-number (todo-slot todo "id")))))

(deftest goals/same-turn-duplicate-is-one-goal
  "El mismo objetivo pedido dos veces en un turno es UN goal, no dos: el modelo
   recibe el mismo texto con dos numeros distintos y lo toma por dos tareas."
  (with-turn-engine
    (reset-turn-engine)
    (let ((first-id (h::add-todo "Read src/app.lisp" :status "pending"))
          (second-id (h::add-todo "Read src/app.lisp" :status "in-progress")))
      (ok (= 1 (length (active-todos)))
          (format nil "un solo goal, hay ~D" (length (active-todos))))
      (ok (string= first-id second-id)
          "el segundo pedido devuelve el mismo id, no uno nuevo"))))

(deftest goals/distinct-tasks-are-distinct
  (with-turn-engine
    (reset-turn-engine)
    (h::add-todo "Read src/app.lisp" :status "pending")
    (h::add-todo "Read src/util.lisp" :status "pending")
    (ok (= 2 (length (active-todos))))))

(deftest goals/ordering-is-numeric-not-lexicographic
  "Ordenar por texto daria todo-10 antes que todo-9: el modelo leeria sus goals
   desordenados justo cuando ya hay muchos."
  (ok (= 9 (h::todo-id-number "todo-9")))
  (ok (= 10 (h::todo-id-number "todo-10")))
  (ok (< (h::todo-id-number "todo-9") (h::todo-id-number "todo-10"))))

(deftest goals/evidence-completes-a-matching-goal
  "Escribir el archivo que el goal nombra es la evidencia de que se hizo.

   OJO: el hecho va a mano, asi que tiene que llevar SIEMPRE los mismos campos
   que pone WRITE-FILE de verdad. Al exigir :applied para todo verbo (no solo
   para :edit), este fixture sin :applied dejo de contar como evidencia y el
   test se rompio. Los hechos a mano se desincronizan del codigo en silencio:
   por eso el campo va explicito aqui."
  (with-turn-engine
    (reset-turn-engine)
    (h::add-todo "Write app.py" :status "in-progress")
    (h::assert (h::harness-fact (h::fact-type "file-write")
                                (h::timestamp (get-universal-time))
                                (h::data (list :path "/tmp/proyecto/app.py"
                                               :applied t
                                               :bytes 42
                                               :turn-id (h::current-turn-id)))))
    (h::reconcile-todos (h::collect-active-facts))
    (ok (string= "completed" (todo-slot (first (active-todos)) "status"))
        (format nil "quedo ~A" (todo-slot (first (active-todos)) "status")))))

(deftest goals/unrelated-evidence-leaves-goal-open
  (with-turn-engine
    (reset-turn-engine)
    (h::add-todo "Write app.py" :status "in-progress")
    (h::assert (h::harness-fact (h::fact-type "file-write")
                                (h::timestamp (get-universal-time))
                                (h::data (list :path "/tmp/otro-cosa.txt"
                                               :bytes 7
                                               :turn-id (h::current-turn-id)))))
    (h::reconcile-todos (h::collect-active-facts))
    (ok (string= "in-progress" (todo-slot (first (active-todos)) "status"))
        "escribir otro archivo no cierra un goal que habla de otro")))

(deftest goals/failed-edit-is-not-evidence
  "Un edit con :applied nil NO cuenta: el archivo nunca cambio. Cerrar el goal
   aqui era como el harness reportaba successes que no ocurrieron."
  (with-turn-engine
    (reset-turn-engine)
    (h::add-todo "Edit test.py" :status "in-progress")
    (h::assert (h::harness-fact (h::fact-type "file-edit")
                                (h::timestamp (get-universal-time))
                                (h::data (list :path "/tmp/test.py"
                                               :applied nil
                                               :reason "old_string not found"
                                               :turn-id (h::current-turn-id)))))
    (h::reconcile-todos (h::collect-active-facts))
    (ok (string= "in-progress" (todo-slot (first (active-todos)) "status"))
        "un edit fallido no completa el goal")))

(deftest goals/prune-keeps-open-goals
  "Podar es solo para lo terminado. Un goal abierto que rebase el tope debe
   seguir visible, o el modelo pierde el trabajo que le falta por hacer."
  (with-turn-engine
    (reset-turn-engine)
    (h::add-todo "sigue pendiente" :status "in-progress")
    (h::add-todo "ya estaba hecho" :status "completed")
    (h::add-todo "tambien hecho" :status "completed")
    (with-test-config (("max_facts_per_type" 1))
      (h::prune-finished-todos))
    (let ((tasks (mapcar (lambda (todo) (todo-slot todo "task"))
                         (active-todos))))
      (ok (member "sigue pendiente" tasks :test #'string=)
          "el goal abierto sobrevive a la poda")
      (ok (not (member "ya estaba hecho" tasks :test #'string=))
          "los completed son los que se podan")
      (ok (member "tambien hecho" tasks :test #'string=)
          "se conserva el completed mas reciente")
      (ok (= 2 (length (active-todos)))
          (format nil "quedan 2 goals, no ~D" (length (active-todos)))))))

;;;; ---------------------------------------------------------------
;;;; Orden causal de eventos
;;;; ---------------------------------------------------------------

(deftest context/same-second-events-keep-insertion-order
  "get-universal-time tiene 1 segundo de resolucion: un file-write y un
   command-exec del mismo turno comparten timestamp. Sin desempate, CL:SORT los
   deja en orden arbitrario y el YAML puede ensenar que el comando corrio antes
   de escribir el archivo que consumio."
  (with-turn-engine
    (reset-turn-engine)
    (let ((t0 (get-universal-time)))
      (h::assert (h::harness-fact (h::fact-type "file-write")
                                  (h::timestamp (identity t0))
                                  (h::data (list :path "/tmp/a.txt" :bytes 1 :turn-id 1))))
      (h::assert (h::harness-fact (h::fact-type "command-exec")
                                  (h::timestamp (identity t0))
                                  (h::data (list :command "wc -l a.txt"
                                                 :output "1"
                                                 :exit-code 0
                                                 :turn-id 1)))))
    (let* ((types (mapcar #'h::fact-type-of (h::collect-active-facts))))
      (ok (equal types '("file-write" "command-exec"))
          (format nil "insertion order preservado, salio ~S" types)))))

(deftest context/render-shows-file-write-before-its-command
  "La version observable: el archivo que el comando leyo aparece antes que el
   comando en el contexto que ve el modelo."
  (with-temp-dir (dir)
    (let ((h::*base-dir* dir)
          (h:*session-id* "session-order-1"))
      (with-turn-engine
        (reset-turn-engine)
        (h::add-todo "contar lineas" :status "in-progress")
        (let ((t0 (get-universal-time)))
          (h::assert (h::harness-fact (h::fact-type "file-write")
                                      (h::timestamp (identity t0))
                                      (h::data (list :path (format nil "~A/datos.txt" dir)
                                                     :applied t
                                                     :bytes 5
                                                     :turn-id 1))))
          (h::assert (h::harness-fact (h::fact-type "command-exec")
                                      (h::timestamp (identity t0))
                                      (h::data (list :command "wc -l datos.txt"
                                                     :output "5"
                                                     :exit-code 0
                                                     :turn-id 1)))))
        (let ((yaml (build-yaml-context "cuentalas")))
          ;; Search the RENDERED KEYS, not the path: the goal task also mentions
          ;; datos.txt, so searching the bare path would match the goals block.
          (let ((wrote-at (search "wrote:" yaml))
                (exec-at (search "exec:" yaml)))
            (ok (and wrote-at exec-at (< wrote-at exec-at))
                (format nil "wrote en ~S y exec en ~S" wrote-at exec-at))))))))

;;;; ---------------------------------------------------------------
;;;; Registro de la respuesta del modelo
;;;; ---------------------------------------------------------------

(deftest response/raw-batch-is-not-stored-as-speech
  "Guardar el lote crudo hacia que el modelo, al turno siguiente, leyera sus
   propias tool_calls como si fueran una declaracion previa. Se reemplaza por
   lo que el lote REALMENTE hizo."
  (with-turn-engine
    (reset-turn-engine)
    (let* ((turn (h::current-turn-id))
           (blob "{\"tool_calls\":[{\"name\":\"write_file\"}],\"response\":\"\"}"))
      (ok (h::raw-batch-blob-p blob) "el lote crudo se reconoce como lote")
      (let ((out (h::conversational-response-p blob turn)))
        (ok (null (search "tool_calls" out :test #'char=))
            "el lote crudo no queda como texto de conversacion")
        (ok (plusp (length out))
            "se registra algo, no se pierde el turno")))))

(deftest response/empty-becomes-factual-summary
  "El modelo puede ejecutar el lote entero y dejar 'response' vacio. Antes eso
   se guardaba como \"\": el turno quedaba registrado como un silencio aunque
   hubiera trabajo real hecho."
  (with-turn-engine
    (reset-turn-engine)
    (let* ((turn (h::current-turn-id)))
      (h::assert (h::harness-fact (h::fact-type "file-write")
                                  (h::timestamp (get-universal-time))
                                  (h::data (list :path "/tmp/datos.txt"
                                                 :bytes 12
                                                 :turn-id turn))))
      (let ((out (h::conversational-response-p "" turn)))
        (ok (plusp (length out)) "no se guarda un turno vacio")
        (ok (search "datos.txt" out :test #'char=)
            (format nil "el resumen dice que se escribio, obtuve ~S" out))
        (let ((out2 (h::conversational-response-p "   " turn)))
          (ok (search "datos.txt" out2 :test #'char=)
              "solo espacios tambien cuenta como respuesta vacia"))))))

(deftest response/prose-mentioning-a-tool-is-kept
  "La blacklist anterior buscaba \"read_file\"/\"write_file\" como subcadena, asi
   que una respuesta normal que mencionara una herramienta se destruia. La
   prueba ahora es estructural: solo se descarta lo que PARSEA como lote."
  (with-turn-engine
    (reset-turn-engine)
    (let* ((turn (h::current-turn-id))
           (prose "Voy a usar write_file para crear el modulo, y luego read_file para revisarlo.")
           (out (h::conversational-response-p prose turn)))
      (ok (string= prose out)
          (format nil "la prosa se conserva intacta, obtuve ~S" out)))))

(deftest response/failed-action-is-reported-as-failure
  "El resumen tiene que poder decir que algo FALLO. Un goal que se cierra solo
   porque se intento es peor que no cerrarlo."
  (with-turn-engine
    (reset-turn-engine)
    (let* ((turn (h::current-turn-id)))
      (h::assert (h::harness-fact (h::fact-type "file-edit")
                                  (h::timestamp (get-universal-time))
                                  (h::data (list :path "/tmp/leeme.md"
                                                 :applied nil
                                                 :reason "old_string not found"
                                                 :turn-id turn))))
      (let ((out (h::conversational-response-p "" turn)))
        (ok (search "FAILED" out :test #'char=)
            (format nil "el intento fallido se declara, obtuve ~S" out))
        (ok (search "old_string not found" out :test #'char=)
            (format nil "y dice por que fallo, obtuve ~S" out))))))

;;;; ---------------------------------------------------------------
;;;; Configuración: límites de batch y política de reintentos
;;;; ---------------------------------------------------------------

(deftest config/batch-limits-defaults
  (with-test-config ()
    (ok (= 8 (h::batch-max-iterations))
        "el tope por defecto del bucle batch es 8, no 100")
    (ok (= 1 (h::batch-repeat-tolerance)))
    (ok (= 3 (h::llm-http-attempts)))
    (ok (= 1.0 (h::llm-http-backoff-seconds)))
    (ok (= 120 (h::llm-http-read-timeout)))))

(deftest config/batch-limits-overrides
  (with-test-config (("batch_max_iterations" 3)
                     ("batch_repeat_tolerance" 0)
                     ("llm_http_attempts" 5)
                     ("llm_http_backoff_seconds" 0))
    (ok (= 3 (h::batch-max-iterations)))
    (ok (= 0 (h::batch-repeat-tolerance))
        "0 significa 'abortar en la primera repetición' y 0 es verdadero en CL")
    (ok (= 5 (h::llm-http-attempts)))
    (ok (= 0.0 (h::llm-http-backoff-seconds)))))

(deftest config/http-retry-policy
  (ok (h::retryable-http-status-p 429) "rate limit: reintentar")
  (ok (h::retryable-http-status-p 500))
  (ok (h::retryable-http-status-p 502) "gateway error: reintentar")
  (ok (h::retryable-http-status-p 503))
  (ok (h::retryable-http-status-p 599))
  (ng (h::retryable-http-status-p 400) "petición incorrecta: no reintentar")
  (ng (h::retryable-http-status-p 401))
  (ng (h::retryable-http-status-p 403))
  (ng (h::retryable-http-status-p 404))
  (ng (h::retryable-http-status-p 200))
  (ng (h::retryable-http-status-p nil)))

(deftest config/http-backoff-growth
  (with-test-config (("llm_http_backoff_seconds" 1))
    (let ((d1 (h::retry-delay-seconds 1))
          (d2 (h::retry-delay-seconds 2))
          (d3 (h::retry-delay-seconds 3)))
      (ok (< d1 d2) "el retraso crece")
      (ok (< d2 d3))
      (ok (< d3 40) "pero con techo"))
    (ok (plusp (h::retry-delay-seconds 1)) "nunca cero con backoff activo")))

;;; ---------------------------------------------------------------
;;; Métricas: reducción, herramientas y tokens por llamada
;;; ---------------------------------------------------------------

(deftest metrics/reduction-percentage
  (metrics-reset)
  (with-test-config ()
    ;; 400 chars curated = 100 tokens; 2000 chars naive = 500 tokens.
    (h::record-context-metrics (make-string 400 :initial-element #\a) "hola"
                               :naive-str (make-string 2000 :initial-element #\b)
                               :real-prompt 1000 :real-completion 50
                               :llm-iterations 1 :llm-path :final)
    (let ((totals (h::metrics-totals)))
      (ok (= 1 (getf totals :turns)))
      (ok (= 100 (getf totals :context-tokens)))
      (ok (= 500 (getf totals :naive-tokens)))
      (ok (= 80.0 (getf totals :reduction-pct)) "500→100 es una reducción del 80%")
      (ok (= 1000 (getf totals :real-prompt-tokens)))
      (ok (= 50 (getf totals :real-completion-tokens)))))
  (metrics-reset))

(deftest metrics/tool-totals-aggregate
  (metrics-reset)
  (h::record-tool-call "read_file" "1234567890" "12345")
  (h::record-tool-call "read_file" "1234567890" "12345")
  (h::record-tool-call "exec_command" "1234" "1234")
  (h::record-context-metrics "ctx" "hola" :naive-str "ctx" :llm-iterations 1)
  (let ((totals (h::metrics-totals)))
    ;; OJO: sin `reduce ... :initial-value` — dentro del macro OK de Rove esa
    ;; forma no evalúa a 3 en este entorno, mientras que APPLY sí.
    (ok (= 3 (apply #'+ (mapcar (lambda (e) (getf (cdr e) :count))
                                (h::metrics-tool-totals)))))
        (format nil "3 llamadas de herramientas en total; totals=~S suma=~D"
                (h::metrics-tool-totals)
                (reduce #'+ (mapcar (lambda (e) (getf (cdr e) :count))
                                    (h::metrics-tool-totals))
                        :initial-value 0)))
    (ok (= 2 (getf (cdr (assoc "read_file" (h::metrics-tool-totals) :test #'string=)) :count)))
  (metrics-reset))

(deftest metrics/per-call-token-log
  (metrics-reset)
  (with-test-config ()
    (let ((h::*llm-call-log* nil))
      (h::record-context-metrics "ctx" "hola" :naive-str "ctx"
                                 :real-prompt 300 :real-completion 60
                                 :llm-iterations 3 :llm-path :final)
      (let* ((events h::*metrics-events*)
             (event (first events)))
        (ok (= 3 (getf event :llm-iterations)))
        (ok (null (getf event :llm-calls)) "sin llamadas registradas, la lista va vacía")
        (ok (null h::*llm-call-log*) "el log se reinicia tras cada turno")))))

(deftest metrics/usage-capture-per-provider-shape
  (let ((openai-shape (jzon:parse "{\"model\":\"gpt-x\",\"usage\":{\"prompt_tokens\":10,\"completion_tokens\":4}}"))
        (anthropic-shape (jzon:parse "{\"model\":\"claude-x\",\"usage\":{\"input_tokens\":11,\"output_tokens\":5}}"))
        (gemini-shape (jzon:parse "{\"modelVersion\":\"gemini-x\",\"usageMetadata\":{\"promptTokenCount\":12,\"candidatesTokenCount\":6}}")))
    (let ((h::*last-llm-usage* nil)
          (h::*llm-response-model* nil)
          (h::*llm-call-log* nil))
      (h::capture-usage openai-shape)
      (ok (= 10 (getf h::*last-llm-usage* :prompt)))
      (ok (= 4 (getf h::*last-llm-usage* :completion)))
      (ok (string= "gpt-x" (getf h::*last-llm-usage* :model)))
      (ok (= 1 (length h::*llm-call-log*)))

      ;; OJO: las llaves de CAPTURE-USAGE son &key (no &optional); mezclarlas
      ;; prohibe el &optional y SBCL lo marca como style-warning.
      (h::capture-usage anthropic-shape :prompt-key "input_tokens"
                        :completion-key "output_tokens")
      (ok (= 11 (getf h::*last-llm-usage* :prompt))
          "Anthropic usa input_tokens")
      (ok (= 5 (getf h::*last-llm-usage* :completion)))

      (h::capture-usage gemini-shape)
      (ok (= 12 (getf h::*last-llm-usage* :prompt))
          "Gemini usa promptTokenCount")
      (ok (= 6 (getf h::*last-llm-usage* :completion)))
      (ok (= 3 (length h::*llm-call-log*))
          "una entrada en el log por llamada al proveedor"))))

(deftest metrics/usage-capture-keeps-model
  (let ((h::*last-llm-usage* nil)
        (h::*llm-response-model* nil)
        (h::*llm-call-log* nil))
    ;; Un chunk sin usage NO debe borrar lo ya capturado.
    (h::capture-usage (jzon:parse "{\"model\":\"m1\",\"usage\":{\"prompt_tokens\":7,\"completion_tokens\":2}}"))
    (h::capture-usage (jzon:parse "{\"choices\":[]}"))
    (ok (= 7 (getf h::*last-llm-usage* :prompt))
        "un fragmento sin usage no debe sobrescribir el acumulado")
    (ok (= 1 (length h::*llm-call-log*))
        "y no debe contar como llamada nueva")))

(deftest metrics/streaming-chunks-do-not-log-calls
  (let ((h::*last-llm-usage* nil)
        (h::*llm-response-model* nil)
        (h::*llm-call-log* nil))
    (dotimes (i 5)
      (h::capture-usage (jzon:parse "{\"choices\":[{\"delta\":{\"content\":\"x\"}}]}")
                        :log nil))
    (ok (null h::*llm-call-log*)
        "los fragmentos de streaming no generan entradas de log")))

(deftest metrics/json-export-includes-model-and-calls
  (metrics-reset)
  (with-temp-dir (dir)
    (let ((h::*base-dir* dir)
          (h::*metrics-dir* (merge-pathnames "metrics/" dir))
          (h::*llm-call-log* (list (list :prompt 10 :completion 2
                                          :model "m1" :endpoint "http://x")))
          (h::*llm-response-model* "m1"))
      (h::record-context-metrics "ctx" "hola" :naive-str "ctx"
                                 :real-prompt 10 :real-completion 2
                                 :llm-iterations 1 :llm-path :final)
      (h::metrics-to-json)
      (let* ((path (merge-pathnames (format nil "~A-metrics.json" h:*session-id*)
                                    h::*metrics-dir*))
             (data (jzon:parse (read-temp path)))
             (event (first (jzon-list (gethash "events" data))))
             (totals (gethash "totals" data)))
        (ok event)
        (ok (string= "m1" (gethash "response-model" event))
            "el modelo del proveedor queda en el evento")
        (ok (= 1 (length (jzon-list (gethash "llm-calls" event))))
            "el desglose por llamada queda en el evento")
        (ok (string= "m1" (gethash "model" (first (jzon-list (gethash "llm-calls" event))))))
        (ok (= 1 (gethash "api-calls" totals))))))
  (metrics-reset))

;;; ---------------------------------------------------------------
;;; Comportamiento: tope de rondas del bucle batch
;;; ---------------------------------------------------------------

(defun stuck-batch-json ()
  "Un lote que el modelo puede pedir indefinidamente: una tool_call y nada de
   respuesta de texto, así que el bucle nunca encuentra plan-done."
  "{\"tool_calls\":[{\"id\":\"1\",\"type\":\"function\",\"function\":{\"name\":\"read_file\",\"arguments\":{\"path\":\"a.lisp\"}}}]}")


(deftest batch/iteration-cap-stops-a-stuck-model
  "Comportamiento, no getter: con batch_max_iterations=2 el bucle dispara como
   mucho una ronda de herramientas y para, en vez de pagar N turnos."
  (metrics-reset)
  ;; OJO: la llave es "llm_provider", no "provider": con "provider" el
  ;; (LLM-PROVIDER) caia al valor ambiente y el bucle batch no se activaba.
  (with-test-config (("llm_provider" "batch")
                     ("llm_stream" nil)
                     ("batch_max_iterations" 2)
                     ("batch_repeat_tolerance" 0))
    (with-turn-engine
      (reset-turn-engine)
      (with-temp-dir (dir)
        (let ((h::*base-dir* dir)
              (calls 0))
          (with-mocked-call-llm ((incf calls) (stuck-batch-json))
            (h::process-turn "hola" "eres un asistente"))
          (ok (<= calls 2)
              (format nil "con tope 2 no se pasan de 2 llamadas, se hicieron ~D" calls))
          (ok h::*last-llm-call-info*)
          (ok (eq (getf h::*last-llm-call-info* :path) :iterations)
              (format nil "el corte se reporta como :iterations, fue ~S"
                      (getf h::*last-llm-call-info* :path)))))))
  (metrics-reset))


(deftest batch/repeated-batch-is-aborted-before-the-cap
  "Un modelo que pide EXACTAMENTE el mismo lote dos veces se corta por
   repetición, antes de agotar batch_max_iterations."
  (metrics-reset)
  (with-test-config (("llm_provider" "batch")
                     ("llm_stream" nil)
                     ("batch_max_iterations" 8)
                     ("batch_repeat_tolerance" 1))
    (with-turn-engine
      (reset-turn-engine)
      (with-temp-dir (dir)
        (let ((h::*base-dir* dir)
              (calls 0))
          (with-mocked-call-llm ((incf calls) (stuck-batch-json))
            (h::process-turn "hola" "eres un asistente"))
          (ok (<= calls 4)
              (format nil "corta pronto, no agota las 8 rondas: hizo ~D" calls))
          (ok (eq (getf h::*last-llm-call-info* :path) :batch-repeated)
              (format nil "el corte se reporta como :batch-repeated, fue ~S"
                      (getf h::*last-llm-call-info* :path)))))))
  (metrics-reset))

;;; ---------------------------------------------------------------
;;; Comportamiento: retry HTTP sin tocar la red
;;; ---------------------------------------------------------------

(defun http-failure (status)
  "Condición DEXADOR::HTTP-REQUEST-FAILED sintética con STATUS: el suite no
   abre ningún socket, sólo ejercita la POLÍTICA de reintentos."
  (make-condition 'dexador.error::http-request-failed
                  :status status :body "" :headers nil))


(deftest config/retry-repeats-transient-failures
  (with-test-config (("llm_http_attempts" 3) ("llm_http_backoff_seconds" 0))
    (let* ((calls 0)
           (result (h::call-with-retry
                    (lambda ()
                      (incf calls)
                      (if (< calls 3) (error (http-failure 503)) "ok")))))
      (ok (string= "ok" result) "reintenta hasta que la llamada funciona")
      (ok (= 3 calls) (format nil "hizo 3 intentos, hizo ~D" calls)))))

;;;; ---------------------------------------------------------------
;;;; PROTOCOLO: invariantes I1, I2, I3 e I5 de PROTOCOLO.md
;;;;
;;;; Lo que puede fallar y como debe, escrito como assert. Nada aqui toca la
;;;; red. El test 5 de PROTOCOLO.md (un registro malformado no tumba el resto)
;;;; NO se puede escribir todavia: depende del renderizador de la baraja, que
;;;; aun no existe. Los cuatro de aqui si se pueden.
;;; ---------------------------------------------------------------

(defun step-linked-facts ()
  "Cuantos hechos llevan la identidad del paso en su data.

   Si esto es 0 tras ejecutar, el plan se perdio: el harness ejecuto, aserto el
   resultado correcto, y borro que paso fue. El YAML no puede entonces decir
   cual de los N fallo, que es justo lo que el modelo necesita para elegir el
   siguiente paso. Implementacion-agnostico a proposito: vale igual que el fix
   conserve las intenciones marcadas o que propague :step a los resultados.

  OJO: aquí se usa FACT-DATA-OF y no H::FACT-SLOT. Desde este paquete,
  (H::FACT-SLOT f 'data) devuelve NIL, mientras que FACT-DATA-OF (que el
  fuente define como su equivalente exacto) sí devuelve la plist. Dentro de
  CL-HARNESS el codigo de produccion no sufre esto: BATCH-COMPLETE-P, que usa
  FACT-SLOT sin cualificar, funciona. Es una rareza de este paquete.

   Y se cuentan PASOS DISTINTOS, no hechos. El invariante es 'un veredicto por
   paso', no 'un hecho por paso': con batch-plan en escena hay dos familias de
   hechos que llevan :step (el plan y el veredicto) y contar hechos diria que un
   plan de 3 pasos tiene 6 pasos. El test tiene que medir la invariante, no una
   implementacion."
  (with-turn-engine
    (length (remove nil
                    (remove-duplicates
                     (mapcar (lambda (f) (h::data-get (h::fact-data-of f) :step))
                             (h::collect-harness-facts))
                     :test #'eql)))))


(defun applied-nil-edits ()
  "Cuantos file-edit quedaron registrados como NO aplicados."
  (with-turn-engine
    (count-if (lambda (f)
                (and (string= (h::fact-type-of f) "file-edit")
                     (not (h::data-get (h::fact-slot f 'data) :applied))))
              (h::collect-harness-facts))))



(defun card-section (ctx)
  "El TRAMO de la tarjeta COBOL: desde 'PROCEDURE DIVISION.' hasta 'DATA DIVISION.'.

   Se recorta por los DOS extremos de la seccion, no por sangria. Con el YAML
   plano habia que inferir el final por 'la siguiente clave de nivel 2', una
   regla sobre ESPACIOS que se rompe en cuanto un valor contiene un salto de
   linea. Con la tarjeta los limites los pone el propio formato, y por eso una
   de las Dividendas del cambio es que el recorte ya no tiene que adivinar.

   El motivo de recortarla en vez de buscar en el contexto entero es el mismo
   de antes: SEARCH devuelve un indice, no un texto. Buscar 'APPLIED' en todo
   el contexto podria dar el acierto por el bloque equivocado, y el test
   pasaria sin que la tarjeta tuviera nada que ver."
  (let ((lines (uiop:split-string ctx :separator '(#\Newline))))
    (let ((start (position "PROCEDURE DIVISION." lines :test #'string=))
          (end (position "DATA DIVISION." lines :test #'string=)))
      (if (or (null start) (null end) (<= end start))
          ""
          (with-output-to-string (s)
            (loop for i from start below end
                  do (format s "~A~%" (nth i lines))))))))


(defun batch-plan-facts ()
  "Los hechos batch-plan, en orden de paso."
  (with-turn-engine
    (sort (remove nil
                  (mapcar (lambda (f)
                            (when (string= "batch-plan" (h::fact-type-of f))
                              (h::fact-data-of f)))
                          (h::collect-harness-facts)))
          #'< :key (lambda (d) (or (h::data-get d :step) 0)))))


(deftest protocol/un-nombre-no-puede-decir-dos-cosas
  "I6, segunda vuelta: :parent-id.

   El mismo campo significa dos cosas. En AGENT-TODO es la jerarquia de goals
   (por defecto \"root\"), y eso es real y se usa. En los hechos resultado vale
   (current-turn-id), o sea :turn-id DUPLICADO con otro nombre. Y no era
   inofensivo: SELECT-RELEVANT-FACTS da +score-causal+ (50 puntos) a cualquier
   hecho que tenga :parent-id, para premiar la cadena causal. Como todos los
   hechos resultado lo tenian, la bonificacion era para todos: 50 puntos de
   relleno, sin discriminar nada. Una senal que no separa casos no es una senal.

   Aqui no se inventa un nombre nuevo. El enlace paso-resultado YA existe y es
   el :step del VERDICT, que es donde debe estar. Asi que :parent-id se queda en
   goals, donde significa algo, y la senal causal vuelve a significar algo."
  (with-temp-dir (dir)
    (let ((h::*base-dir* dir))
      (with-turn-engine
        (reset-turn-engine)
        (h::write-file "uno.txt" "a")
        (h::read-file "uno.txt")
        (h::edit-file "uno.txt" "a" "b")
        (h::exec-command "true")
        (let ((liars (remove nil
                             (mapcar (lambda (f)
                                       (let ((d (h::fact-data-of f)))
                                         (and (h::data-get d :parent-id)
                                              (eql (h::data-get d :parent-id)
                                                   (h::data-get d :turn-id))
                                              (h::fact-type-of f))))
                                     (h::collect-harness-facts)))))
          (ok (null liars)
              (format nil "ningun hecho resultado lleva :parent-id copiando :turn-id; hay ~D"
                      (length liars))))
        ;; Y la jerarquia de goals sigue intacta: ese uso es real. AGENT-TODO
        ;; guarda sus campos en SLOTS del DSL, no en un plist :data, asi que
        ;; fact-data-of devuelve NIL ahi y hay que leerlos por slot.
        ;;
        ;; Y el slot se lee con el simbolo CUANTIFICADO: desde este paquete,
        ;; 'parent-id es un simbolo distinto del que el template de Lisa/Rete
        ;; declaro en CL-HARNESS, y get-slot-value devuelve NIL sin avisar. No
        ;; era un fallo de arranque ni una rareza del harness: era un simbolo
        ;; distinto, y eso no se debuggea solo.
        (h::add-todo "Write uno.txt" :status "in-progress")
        (let ((goal (first (h::collect-active-todos))))
          (ok (and goal
                   (string= (h::get-slot-value goal 'h::parent-id) "root"))
              "el :parent-id de un goal sigue siendo su jerarquia, no el turno")
          (ok (not (eql (h::get-slot-value goal 'h::parent-id)
                        (h::get-slot-value goal 'h::turn-id)))
              "y no es su turno: los dos campos ya no dicen lo mismo"))))))


(deftest protocol/el-harness-no-se-vende-como-assistente-general
  "El system prompt NO lo escribe el harness.

   load-system-prompt devolvia, cuando no encontraba el fichero, \"You are a
   helpful coding assistant... Help them with their task\". Un solo default de
   esos, y el modelo se volvia opencode: creia que podia hacer de todo, pedia
   cosas que este harness no tiene, y el fallo se manifestaba DOS turnos tarde,
   como un refusal raro del proveedor, no como un prompt equivocado. La causa
   raiz era invisible porque el default funcionaba.

   Esto no es un opencode: no hay busqueda web, ni edicion multiarchivo
   autonoma. Su alcance es estrecho -- escribir codigo -- y quien lo define es
   quien lo usa, no el codigo.

   El test fija las DOS mitades, que es donde esta el fallo real:
     1. sin fichero, no hay prompt inventado (NIL, no una cadena)
     2. con fichero, se lee el suyo y no se mezcla con doctrina ajena
   Si solo fijara la primera, alguien podria 'arreglar' el aviso devolviendo un
   default mas cauto y la trampa volveria a cierra."
  ;; Por *system-prompt-path* y no por *base-dir*: harness-base-dir da prioridad
  ;; al env CL_HARNESS_DIR sobre *base-dir*, asi que atar solo *base-dir* aqui
  ;; no ataba nada, y el test pasaba o fallaba segun el entorno desde el que se
  ;; lanzara. Los tests que dependan de rutas deben atar la variable con MAYOR
  ;; prioridad, no la mas comoda.
  (with-temp-dir (dir)
    ;; 1. Sin prompt, sin doctrina inventada.
    (let ((h::*system-prompt-path* (merge-pathnames "nada.md" dir)))
      (ok (null (h::load-system-prompt))
          "sin system prompt el harness devuelve NIL, no un 'helpful assistant'")
      ;; Y el aviso lo dice en vez de callarse: el usuario creeria que su
      ;; prompt se cargo cuando en realidad nunca se leyo.
      (ok (search "No hay system prompt"
                  (with-output-to-string (s) (let ((*standard-output* s))
                                               (h::warn-missing-system-prompt))))
          "arrancar sin prompt avisa, con la ruta que busco"))
    ;; 2. Con prompt, es el suyo y solo el suyo. Let independiente del de arriba:
    ;; si las dos mitades comparten binding, un fallo en la primera secuestra a la
    ;; segunda y el diagnostico senala el sitio equivocado -- que es lo que paso.
    (let ((p (merge-pathnames "system-prompt.md" dir)))
      (with-open-file (out p :direction :output :if-exists :supersede)
        (write-string "Eres un programador de Python. Nada mas." out))
      ;; LET y no LET*: los inicializadores de un LET se evaluan EN PARALELO, no
      ;; en orden, asi que (let ((path p) (got (load ...)))) calcula got con el
      ;; binding VIEJO. El error no es de CL: es de lectura. LET* ata uno a uno.
      (let* ((h::*system-prompt-path* p)
             (got (h::load-system-prompt)))
        (ok (and got (search "Python" got))
            "con fichero, el prompt es el del usuario")
        (ok (not (search "coding assistant" (or got "")))
            "y no se le anade doctrina de opencode por debajo")))))


(deftest protocol/el-plan-se-lee-como-un-programa-cobol
  "El PLAN no es YAML: es un programa de COBOL por tarjetas, y se lee asi.

   Este es el test PRIMERO, como toca: fija la forma antes de que exista el
   codigo que la produce. Al reves, el codigo dicta la forma y despues se
   escribe el test que lo consagra -- que es como el formato real acabo siendo
   logfmt en PROTOCOLO.md, un formato que nunca implemento nadie.

   Por que COBOL y no YAML. YAML es lo que consume un LLM moderno sin
   esfuerzo. COBOL es lo que declara el CONTRATO: campos en columna fija,
   jerarquia explicita, y sobre todo una division entre lo que el LLM escribe y
   lo que la maquina responde. En PLAN eso es literal: el modelo perfora la
   tarjeta, y el veredicto lo rellena la maquina. El formato ES el mecanismo.
   En YAML el modelo podia escribir su propio veredicto sin que nada se lo
   impidiera; aqui el veredicto viaja en una seccion que el modelo no controla.

   Y una division mas, que es la que I5 ya necesitaba y no tenia donde
   esconderse: PROCEDURE DIVISION lleva GOAL ABIERTO. El LLM ve sus bananas
   pendientes en el mismo lugar donde ve su plan, que es donde tiene sentido
   que las mire.

   El test comprueba las tres divisiones y que el veredicto de la maquina
   sobrevive al viaje de ida y vuelta."
  (with-temp-dir (dir)
    ;; cl-harness::*base-dir* y no *base-dir*: este simbolo NO esta importado
    ;; en el paquete de pruebas, asi que un (let ((*base-dir* dir)) ...) liga
    ;; CL-HARNESS/TESTS::*BASE-DIR*, que no lee nadie. El test pasaba verde y
    ;; dejaba uno.txt y dos.txt en la raiz del proyecto: los tests que "no
    ;; dependen del disco" si dependian del disco, del equivocado.
    (let ((cl-harness::*base-dir* dir))
      (with-turn-engine
        (reset-turn-engine)
        (h::write-file "uno.txt" "contenido real\n")
        (h::parse-llm-batch-to-intentions
         "{\"tool_calls\":[{\"name\":\"read_file\",\"arguments\":{\"path\":\"uno.txt\"}},{\"name\":\"edit_file\",\"arguments\":{\"path\":\"uno.txt\",\"old_string\":\"no esta\",\"new_string\":\"x\"}},{\"name\":\"write_file\",\"arguments\":{\"path\":\"dos.txt\",\"content\":\"hola\"}}]}")
        (h::run)
        ;; Se pasa la lista CRUDA a proposito: la tarjeta se dibuja desde los
        ;; hechos, no desde el texto ya renderizado, para que el test mida el
        ;; dato y no el formato viejo. Se llama directo en vez de por un helper
        ;; porque un defun suelto aqui no lo ve el cuerpo del deftest, y un
        ;; "unbound" ahi habria hecho depurar el sitio equivocado.
        (let ((card (h::render-plan-card (h::collect-harness-facts))))
          ;; 1. Las tres divisiones, con su nombre exacto. Un formato con las
          ;; divisiones equivocadas no es "casi COBOL", es otro formato.
          (ok (search "IDENTIFICATION DIVISION." card)
              "IDENTIFICATION DIVISION: quien produjo la tarjeta")
          (ok (search "PROCEDURE DIVISION." card)
              "PROCEDURE DIVISION: los pasos y sus veredictos")
          (ok (search "DATA DIVISION." card)
              "DATA DIVISION: el estado de la maquina")

          ;; 2. El paso conserva su identidad a traves del viaje. Sin esto el
          ;; modelo recibe 'algo fallo' y no sabe reintentar.
          (ok (search "01." card) "el paso se numera a la COBOL, con punto")
          (ok (search "READ-FILE" card) "cada paso conserva su ACCION")
          (ok (search "uno.txt" card) "y su OBJETIVO")

          ;; 3. Veredicto por paso, con el que puso la maquina.
          (ok (search "APPLIED" card) "un paso que funciono dice APPLIED")
          (ok (search "FAILED" card) "y uno que fallo dice FAILED")
          ;; 4. PENDING se comprueba mas abajo, en su propio turno: aqui los tres
          ;; pasos se ejecutaron, y buscar PENDING en esta tarjeta solo pasaria
          ;; por el motivo equivocado. Ver la tarjeta final.
          ;; 5. El motivo viaja. Sin el, FAILED no dice nada accionable.
          (ok (search "REASON" card) "y el motivo del rechazo es parte del registro")

          ;; PENDING va en su propia tarjeta, y en un turno propio. Meterlo aqui
          ;; exigia fabricarse un batch-abort a mano, y lo que se ended verificando
          ;; era que yo habia asertado un batch-abort, no que el harness produce
          ;; un PENDING. Este caso sale solo: un plan de un paso que se cancela
          ;; ANTES de ejecutarse, que es exactamente como se queda trabajo
          ;; colgando en la vida real.
          (h::reset-turn-engine)
          (h::assert-batch-intention (list :action :write-file :target "nunca.txt"
                                           :content "x" :step 1) 1)
          ;; Se cancela la intencion, no el plan. Se dispara la REGLA de verdad
          ;; (cancel-intentions-on-abort) en vez de retractar a mano: retract es
          ;; una primitiva de Rete con firma propia y lo que interesa comprobar
          ;; es que el harness produce el PENDING solo, no que se lo sepamos
          ;; fabricar. Si el plan se fuera con la intencion, PENDING seria
          ;; imposible por construccion y la seccion entera no significaria nada.
          (h::assert (h::harness-fact (h::fact-type "batch-abort")
                                      (h::timestamp (get-universal-time))
                                      (h::data (list :reason "Abort de prueba"
                                                     :turn-id 1))))
          (h::run)
          (let ((card2 (h::render-plan-card (h::collect-harness-facts))))
            (ok (search "PENDING" card2)
                "un plan sin ejecutar se declara PENDING, no desaparece")
            (ok (search "01." card2)
                "y conserva su numero de paso aunque no se haya ejecutado")))))))

(defun fact-signature (facts)
  "Una firma comparable de FACTS: tipo + datos relevantes, en orden estable.

   Se mide el DATO, no el tipo. Un hecho que vuelve con el tipo correcto y el
   motivo vacio es un hecho que miente igual: el viaje se respeta cuando el
   contenido coincide, no cuando el nombre coincide.

   Se comparan solo los campos que el dump garantiza. El TIMESTAMP se excluye a
   proposito: el serializador lo re-emite fresco al restaurar, asi que exigirlo
   seria exigir algo que el formato nunca prometio. Los payloads grandes tambien
   fuera del cuerpo de la comparacion por :contents truncado: lo que se prueba
   es que el dato llega, no que el recorte sea identico byte a byte."
  (let ((out '()))
    (dolist (f (sort (copy-list facts)
                     (lambda (a b) (string< (format nil "~A" (h::fact-type-of a))
                                            (format nil "~A" (h::fact-type-of b))))))
      (let ((d (h::fact-data-of f)))
        (push (list (h::fact-type-of f)
                    (h::data-get d :path)
                    (h::data-get d :applied)
                    (h::data-get d :reason)
                    (h::data-get d :step)
                    (h::data-get d :verdict)
                    (h::data-get d :action)
                    (h::data-get d :turn-id))
              out)))
    (nreverse out)))

(deftest persistence/un-viaje-de-ida-y-vuelta-no-cambia-los-hechos
  "Escribir, volcar, restaurar: los hechos tienen que volver IGUALES.

   Este es el invariante que faltaba, y no porgazateorico. El dump de disco es
   CODIGO -- un (assert (harness-fact ...)) que se vuelve a cargar -- y por eso
   un cambio de vocabulario lo rompe en silencio: nadie ve un error, el dump
   carga bien y aparecen hechos con la forma de la version vieja.

   Ya paso. Los batch-plan，随后 el batch-complete se renombro a plan-done y el
   :parent-id desaparecio de los hechos resultado, sin tocar el serializador. Un
   dump de la epoca anterior, restaurado hoy, resucita :PARENT-ID (un campo que
   ya no existe) y hechos tipados con nombres que nadie emite. Carga sin quejarse
   y el estado queda mezclado entre dos versiones del protocolo.

   El test fija el viaje entero en DOS motores distintos, que es como pasa de
   verdad: no basta con recargar en la misma red, porque ahi los hechos siguen
   vivos y el assert parece funcionar."
  ;; dumps_dir por CONFIG y no *base-dir*: harness-base-dir prioriza el env
  ;; CL_HARNESS_DIR sobre *base-dir*, asi que atar solo *base-dir* deja que el
  ;; entorno de quien lance decida donde acaba el dump. Mismo trope que con el
  ;; system prompt: hay que atar lo que GANA, no lo que es comodo. Aqui
  ;; config-value "dumps_dir" es lo que gana, y se ata en memoria con
  ;; with-test-config, sin tocar el entorno de la maquina.
  (with-temp-dir (dir)
    ;; Los TRES directorios, no solo los dos del dump: save-session escribe
    ;; tambien metricas, y metrics-dir cae a harness-base-dir (= el cwd) si no
    ;; se ata. Un solo directorio sin atar hacia que el test escribiera en el
    ;; proyecto real, que es el modo de fallo que se quiere evitar.
    (with-test-config (("dumps_dir" (merge-pathnames "dumps/" dir))
                      ("sessions_dir" (merge-pathnames "sessions/" dir))
                      ("metrics_dir" (merge-pathnames "metrics/" dir)))
      ;; El base-dir TAMBIEN, y no es opcional: (h::run) es lisa:run, el motor
      ;; Rete, y escribe ahi. Sin atarlo no es que el test falle por lo que
      ;; prueba: escribe en el cwd del proyecto. De ahi el error NIL-is-not-a-
      ;; stream que costó tres intentos de adivinar. Se atan las TRES cosas --
      ;; config, base-dir y session-id -- y no solo la que se recuerda.
      (let ((cl-harness::*base-dir* dir) (h:*session-id* "rt-test"))
      ;; --- ida: un turno con de todo, incluidos los casos que rompen ---
      (with-turn-engine
        (h::reset-turn-engine)
        (h::write-file "uno.txt" "contenido real\n")
        (h::edit-file "uno.txt" "contenido real" "editado")
        (h::edit-file "uno.txt" "no esta" "x")            ; FAILED con motivo
        (h::read-file "no-existe.txt")                     ; FAILED, I2
        ;; :path, NO :target. :target es la etiqueta de la tarjeta COBOL y
        ;; nunca llega a ejecutarse: las reglas leen :path. Este test lo
        ;; descobrio al reventar con "NIL is not of type ... FILE-STREAM" --
        ;; write-file recibia NIL. No es un fallo de este test: es que la
        ;; diferencia entre etiqueta y ruta no estaba escrita en ningun sitio,
        ;; y se nota justo cuando alguien usa la API a mano.
        (h::assert-batch-intention (list :action :write-file :path "dos.txt"
                                         :content "hola")
                                   1)
        (h::run)
        (let ((before (fact-signature (h::collect-active-facts)))
              (before-types (sort (mapcar (lambda (f) (h::fact-type-of f))
                                          (h::collect-active-facts))
                                  #'string<)))
          (ok (plusp (length before-types))
              (format nil "el turno tiene hechos que perder; hay ~D tipos"
                      (length before-types)))
          ;; Volcar. Asi lo hace save-session en cada :save.
          (h::save-session)
          (ok (probe-file (h::session-facts-path))
              "el dump se escribe a disco")
          ;; --- vuelta, en un motor NUEVO ---
          (h::reset-turn-engine)
          (ok (= 0 (length (h::collect-active-facts)))
              "el motor nuevo arranca vacio: si no, el test probaria nada")
          (h::restore-session "rt-test")
          (let ((after (fact-signature (h::collect-active-facts)))
                (after-types (sort (mapcar (lambda (f) (h::fact-type-of f))
                                           (h::collect-active-facts))
                                   #'string<)))
            (ok (equal before-types after-types)
                (format nil "los tipos vuelven iguales; antes ~S despues ~S"
                        before-types after-types))
            ;; El contenido, no solo el tipo. Un hecho que vuelve con el tipo
            ;; correcto y el motivo vacio es un hecho que miente igual que antes.
            (ok (equal before after)
                (format nil "los DATOS vuelven intactos; antes ~D hechos, despues ~D;~%  antes=~S~%  despues=~S"
                        (length before) (length after) before after))
            ;; Y explicitamente: el motivo de un rechazo sigue a su hecho. Es el
            ;; dato que I6 puso en su sitio y el que mas se pierde al volcar.
            ;; :test CHAR-EQUAL no sirve: AFTER es una lista de TUPLAS, y el
            ;; test se compara contra la tupla entera, no contra su contenido.
            ;; Con char-equal nunca puede encontrar un string dentro de una
            ;; tupla. La busqueda va sobre la propia tupla: si alguna posicion
            ;; es el motivo, el motivo viajo.
            (ok (some (lambda (entry)
                        (some (lambda (field)
                                (and (stringp field)
                                     (search "old_string not found" field)))
                              entry))
                      after)
                "el motivo del rechazo sobrevive al viaje"))))))))

(deftest persistence/un-dump-viejo-no-resucita-el-bug-de-i2
  "Un dump escrito con la forma anterior no debe colarse de contrabando.

   Cuando un fichero de facts llega por una via que NO es la de la prueba
   anterior -- restaurado a mano, copiado de otra maquina, de un dump de hace
   semanas -- su forma no se ha inventado este codigo. Ese fichero puede traer
   :PARENT-ID en hechos resultado (campo que se elimino), o el error de I2
   escondido dentro de :CONTENTS en vez de en :reason.

   La segunda es la grave: es un hecho que AFIRMA que leyo el contenido de un
   fichero, y su 'contenido' es la cadena 'ERROR: File not found'. Si eso vuelve
   al contexto, el modelo cree que ha leido un fichero que no existe. No es un
   dato viejo, es un dato falso restaurado con toda la autoridad de un hecho.

   Que el dump viejo cargue es inevitable -- no se puede rechazar un fichero
   entero sin perder las sesiones validas. Lo que NO debe pasar es que sobreviva
   en silencio: un aviso mientras se restaura."
  (with-temp-dir (dir)
    ;; Los TRES directorios, no solo los dos del dump: save-session escribe
    ;; tambien metricas, y metrics-dir cae a harness-base-dir (= el cwd) si no
    ;; se ata. Un solo directorio sin atar hacia que el test escribiera en el
    ;; proyecto real, que es el modo de fallo que se quiere evitar.
    (with-test-config (("dumps_dir" (merge-pathnames "dumps/" dir))
                      ("sessions_dir" (merge-pathnames "sessions/" dir))
                      ("metrics_dir" (merge-pathnames "metrics/" dir)))
      (let ((h:*session-id* "viejo"))
      (h::ensure-dirs)
      ;; Escribo a mano un dump con la forma de antes de I2, tal como lo
      ;; produciria el serializador de la epoca pasada.
      (with-open-file (s (h::session-facts-path) :direction :output :if-exists :supersede)
        (format s ";;; Session viejo facts dump~%")
        (format s "(setf *turn-counter* 1)~%")
        (format s "(setf *todo-counter* 0)~%")
        (format s "(setf *epoch-counter* 0)~%")
        (format s "(defun restore-facts ()~%  (progn~%")
        ;; I2 roto: el motivo escondido dentro de :contents.
        (format s "    (assert (harness-fact (fact-type ~S)~%" "file-read")
        (format s "                                 (timestamp (get-universal-time))~%")
        (format s "                                 (data (quote ~S))))~%"
                (list :path "/tmp/no-existe.txt"
                      :contents "ERROR: File not found: /tmp/no-existe.txt"
                      :turn-id 1 :parent-id 1))
        ;; :parent-id residual, campo que ya no existe en el codigo.
        (format s "    (assert (harness-fact (fact-type ~S)~%" "file-write")
        (format s "                                 (timestamp (get-universal-time))~%")
        (format s "                                 (data (quote ~S))))~%"
                (list :path "a.txt" :applied t :bytes 3 :turn-id 1 :parent-id 1))
        (format s "    t))~%"))
      ;; El motor de Rete, la captura de stdout y la restauracion van JUNTOS,
      ;; en el MISMO with-turn-engine que las aserciones. Separarlos hacia que
      ;; la restauracion Happens en un motor y las aserciones mirasen a otro
      ;; vacio -- y de ahi el "WARNED is unbound": no era el aviso, era que el
      ;; let de masNIVEL se evaluaba con un cuerpo que aun no existia.
      ;; FIXME: con-output-to-string solo captura stdout, no el aviso real.
      (let ((warned nil))
        (h::with-turn-engine
          (h::reset-turn-engine)
          (setf warned
                (with-output-to-string (o)
                  (let ((*standard-output* o))
                    (h::restore-session "viejo"))))
          (let ((reads (h::collect-facts-of-type (h::collect-active-facts) "file-read")))
            ;; 1. El hecho entra: no se puede rechazar el fichero entero.
            (ok (= 1 (length reads))
                "el dump viejo se restaura: perder la sesion entera seria peor")
            ;; 2. Pero AVISA. Un restaurado silencioso de datos de otra epoca es
            ;; justo lo que hace imposible sterimar por que fallo.
            (ok (or (search "parent-id" warned)
                    (search "PARENT-ID" warned)
                    (search "restaurado" warned))
                (format nil "y avisa de lo que trae de otra version; aviso=~S" warned))
            ;; 3. Y un hecho que AFIRMA haber leido un fichero inexistente no se
            ;; queda con la mentira puesta como contenido.
            (let ((contents (h::data-get (first reads) :contents)))
              (ok (not (and (stringp contents)
                            (or (search "ERROR: File not found" contents)
                                (search "not found" contents))))
                  (format nil "el motivo del I2 no vuelve disfrazado de :contents; hay ~S"
                          contents))))))))))

(deftest protocol/plan-done-y-nada-mas
  "El nombre del hecho de fin de turno decia una cosa y hacia otra.

   BATCH-COMPLETE significaba 'el modelo mando tool_calls vacio', no 'el trabajo
   esta hecho'. Con I5 ya no es tan falso -- solo se aserta si no queda ningun
   goal abierto -- pero el nombre seguia mintiendo, y un nombre que miente se
   lleva por delante a quien lo lea, que es el siguiente que toque esto.

   Se renombra a plan-done, que es lo que afirma, y el test fija que el nombre
   viejo desaparece: renombrar sin quitar el viejo deja dos vocabularios
   conviviendo, que es el problema original."
  (with-turn-engine
    (reset-turn-engine)
    (multiple-value-bind (parsed response error)
        (h::parse-llm-batch-to-intentions
         "{\"tool_calls\":[],\"response\":\"Listo\"}")
      (ok (null error) "un DONE sin goals abiertos se acepta")
      (ok (= 0 (fact-count "batch-complete"))
          "el nombre viejo 'batch-complete' ya no lo emite nadie")
      (ok (= 1 (fact-count "plan-done"))
          "se llama plan-done, que es lo que significa")
      (ok (h::plan-done-p) "y el predicado se llama igual"))))


(deftest protocol/el-plan-sobrevive-a-la-ejecucion
  "I1: un PLAN de 3 pasos debe dejar 3 pasos recuperables.

   HOY FALLA (0 de 3). La causa es una linea: EXECUTE-INTENTION-WRITE hace
   (RETRACT ?f) sobre la intencion al ejecutarla, y el hecho file-write que
   aserta WRITE-FILE no lleva :step. El resultado se guarda y el plan se
   borra, asi que no queda forma de saber que paso fue el que fallo.

   OJO al arreglarlo: si en vez de retractar se marca :status :done, la regla
   tiene que guardar el (TEST (EQL :PENDING ...)) o se dispara en bucle."
  (with-turn-engine
    (reset-turn-engine)
    (with-temp-dir (dir)
      (let ((h::*base-dir* dir))
        (h::assert-batch-intention (list :action :write-file :path "uno.txt" :content "1") 1)
        (h::assert-batch-intention (list :action :write-file :path "dos.txt" :content "2") 2)
        (h::assert-batch-intention (list :action :write-file :path "tres.txt" :content "3") 3)
        (h::run)
        (ok (= 3 (step-linked-facts))
            (format nil "los 3 pasos deben quedar registrados; quedan ~D"
                    (step-linked-facts)))
        (ok (= 3 (fact-count "file-write"))
            (format nil "y los 3 deben haberse escrito; se escribieron ~D"
                    (fact-count "file-write")))))))


(deftest protocol/un-paso-rechazado-deja-veredicto
  "I2: si el harness se niega a intentar un paso, tambien debe registrar POR QUE.

   HOY FALLA (0 de 1). EXECUTE-INTENTION-EDIT comprueba que old_string no sea
   vacio y, si lo es, escribe un [DEBUG rule] y hace (RETRACT ?f) sin asertar
   nada. El paso desaparece del mundo: el modelo ve su PLAN con un hueco y no
   sabe si se intento, se salto o se perdio. Edit-file SI sabe registrar el
   fallo (:applied nil :reason); la regla se lo salta antes de llamarlo."
  (with-turn-engine
    (reset-turn-engine)
    (with-temp-dir (dir)
      (let ((h::*base-dir* dir))
        (h::write-file "vacio.txt" "contenido real")
        (h::assert-batch-intention
         (list :action :edit-file
               :path "vacio.txt"
               :old-string ""            ; invalido a proposito
               :new-string "x")
         1)
        (h::run)
        (ok (= 1 (applied-nil-edits))
            (format nil "un edit rechazado debe quedar como edit no aplicado; hay ~D"
                    (applied-nil-edits)))
        (ok (= 1 (step-linked-facts))
            (format nil "y debe quedar enlazado a su paso; hay ~D paso(s)"
                    (step-linked-facts)))))))


(defun fact-data-for (type)
  "DATA del primer hecho de TYPE, o NIL."
  (let ((f (find-if (lambda (f) (string= type (h::fact-type-of f)))
                   (h::collect-harness-facts))))
    (and f (h::fact-data-of f))))


(deftest protocol/un-rechazo-tiene-un-solo-nombre
  "I6: 'por que no lo hice' se llamaba :reason en el hecho y :error en el valor
   devuelto, para el MISMO evento. Eso obliga a cada lector a probar los dos:
   los dos renderers hacen (or :reason :error) y ASSERT-STEP-VERDICT idem. No es
   cosmetico: hoy los dos textos ADEMAS son distintos, el hecho guarda
   'old_string not found' y el retorno guarda la explicacion larga con la que
   el modelo podria reintentar bien. Alguien lea solo el hecho y se queda con
   la version inutil.

   Aqui se fija una sola clave, :reason, y con el MISMO texto en hecho y
   retorno. Un solo nombre, un solo texto, un solo sitio donde equivocarse."
  (with-temp-dir (dir)
    (let ((h::*base-dir* dir))
      (with-turn-engine
        (reset-turn-engine)
        ;; Caso 1: old_string vacio.
        (let ((ret (h::edit-file "no-existe-importa.txt" "" "x"))
              (fact (progn (h::edit-file "no-existe-importa.txt" "" "x")
                           (fact-data-for "file-edit"))))
          (ok (and (getf ret :reason) (not (getf ret :error)))
              "el valor devuelto usa :reason, no :error")
          (ok (equal (getf ret :reason) (getf fact :reason))
              (format nil "hecho y retorno cuentan lo MISMO~%     retorno: ~S~%     hecho:   ~S"
                      (getf ret :reason) (getf fact :reason)))
          (ok (and (getf fact :reason) (not (getf fact :error)))
              "el hecho usa :reason, no :error"))
        ;; Caso 2: fichero inexistente.
        (reset-turn-engine)
        (let ((ret (h::edit-file "fantasma.txt" "viejo" "nuevo"))
              (fact (progn (h::edit-file "fantasma.txt" "viejo" "nuevo")
                           (fact-data-for "file-edit"))))
          (ok (and (getf ret :reason) (not (getf ret :error)))
              "un fichero ausente tambien se explica con :reason")
          (ok (and fact (getf fact :reason))
              "y deja hecho: un rechazo sin hecho es un paso desaparecido"))))))


(deftest protocol/ningun-rechazo-desaparece
  "I2, segunda parte: un rechazo tiene que dejar RASTRO aunque no haya nada
   que ejecutar.

   Este test falla HOY y no por el nombre del campo, sino porque EDIT-FILE
   sobre un fichero inexistente hace RETURN-FROM sin asertar hecho, y WRITE-FILE
   sobre un fichero grande se niega sin asertar hecho. El paso se ejecuta, se
   rechaza, y no consta en ninguna parte: el modelo ve su turno sin ningun
   veredicto y no tiene ni idea de que intento algo. Los dos casos devuelven
   :error y :refused, y ya de paso usan el nombre equivocado."
  (with-temp-dir (dir)
    (let ((h::*base-dir* dir))
      ;; Un fichero grande de verdad, para que WRITE-FILE se niegue.
      (with-open-file (s (merge-pathnames "grande.txt" dir) :direction :output
                         :if-exists :supersede)
        (dotimes (_ 400) (write-string "basura de relleno para pasar de 2000 bytes" s)))
      (with-turn-engine
        ;; Rechazo por fichero inexistente.
        (reset-turn-engine)
        (h::edit-file "fantasma.txt" "viejo" "nuevo")
        (ok (fact-data-for "file-edit")
            "editar un fichero ausente deja hecho")
        (ok (and (fact-data-for "file-edit")
                 (eql (getf (fact-data-for "file-edit") :applied) nil))
            "y lo deja como NO aplicado")
        ;; Rechazo por escritura a ciegas.
        (reset-turn-engine)
        (h::write-file "grande.txt" "contenido nuevo")
        (let ((fact (fact-data-for "file-write")))
          (ok fact "escribir sobre un fichero grande deja hecho")
          (ok (and fact (eql (getf fact :applied) nil))
              "y lo deja como NO aplicado")
          (ok (and fact (getf fact :reason))
              "y explica por que, con el mismo nombre de clave que el resto"))
          ;; Y que el rechazo LLEGU al modelo. Si el renderer no distingue un
          ;; hecho fallido de uno bueno, el modelo ve 'wrote:' sobre un fichero
          ;; que no se toco.
          (let ((yaml (build-yaml-context "sigue")))
            (ok (search "write_refused" yaml)
                "una escritura rechazada no se renderiza como 'wrote'")
            (ok (search "already exists" yaml)
                "y el motivo viaja al modelo"))))))


(deftest protocol/un-hecho-rechazado-no-descarga-un-goal
  "I3, y el.previa de I6.

   TODO-SATISFIED-P mira :applied SOLO para :edit. Para :read y :write le da
   igual: cualquier hecho del tipo que cuadre con la ruta descarga el goal. Dos
   consecuencias, y la segunda la introduce este mismo trabajo:

     1. HOY: leer un fichero que no existe aserta un file-read con
        :contents \"ERROR: File not found\" y CIERRA el goal de lectura. El
        modelo recibe 'Read app.py' completado sin haber leido nada.
     2. AL ANADIR los hechos :applied nil de los rechazos (que es justo lo que
        hace que I2 se cumpla), un rechazo pasaria a CERRAR el goal que
        precisamente no cumplio. Arreglar I2 sin esto seria cambiar un fallo
        por otro.

   Asi que la evidencia exige :applied t SIEMPRE, no solo en :edit."
  (with-temp-dir (dir)
    (let ((h::*base-dir* dir))
      (with-turn-engine
        ;; 1) Lectura de un fichero inexistente NO cierra el goal.
        (reset-turn-engine)
        (h::add-todo "Read fantasma.txt" :status "in-progress")
        (h::read-file "fantasma.txt")
        (h::reconcile-todos (h::collect-active-facts))
        (ok (string= "in-progress" (todo-slot (first (active-todos)) "status"))
            "leer un fichero que no existe no cierra el goal de lectura")
        ;; 2) Escritura rechazada por fichero grande NO cierra el goal.
        (reset-turn-engine)
        (with-open-file (s (merge-pathnames "grande.txt" dir) :direction :output
                           :if-exists :supersede)
          (dotimes (_ 400) (write-string "basura de relleno para pasar de 2000 bytes" s)))
        (h::add-todo "Write grande.txt" :status "in-progress")
        (h::write-file "grande.txt" "contenido nuevo")
        (h::reconcile-todos (h::collect-active-facts))
        (ok (string= "in-progress" (todo-slot (first (active-todos)) "status"))
            "una escritura rechazada no cierra el goal de escritura")
        ;; 3) Y el camino feliz sigue cerrando: esto no lo arregla rompiendolo.
        (reset-turn-engine)
        (h::add-todo "Write ok.txt" :status "in-progress")
        (h::write-file "ok.txt" "contenido real")
        (h::reconcile-todos (h::collect-active-facts))
        (ok (string= "completed" (todo-slot (first (active-todos)) "status"))
            (format nil "una escritura correcta SI cierra su goal; quedo ~A"
                    (todo-slot (first (active-todos)) "status")))))))


(deftest protocol/done-se-valida-contra-los-goals-abiertos
  "I5: tool_calls vacio es una PROPUESTA de fin de turno, no un hecho.

   HOY FALLA. PARSE-LLM-BATCH-TO-INTENTIONS, cuando no hay intenciones,
   aserta plan-done y devuelve la respuesta tal cual, sin mirar si queda
   trabajo por hacer. El modelo puede mandar {\"tool_calls\":[],
   \"response\":\"Listo\"} con tres goals abiertos y el harness lo acepta como
   turno completado. Es la fuga de ejecutor en su forma mas desnuda: la
   autoridad de cerrar la sesion es del LLM."
  (with-turn-engine
    (reset-turn-engine)
    (h::add-todo "Write pendiente.txt" :status "in-progress")
    (h::parse-llm-batch-to-intentions
     "{\"tool_calls\":[],\"response\":\"Listo, todo hecho\"}")
    (ok (not (h::plan-done-p))
        "con un goal abierto, DONE no puede aceptarse como completado")
    (ok (= 1 (length (active-todos)))
        "y el goal sigue abierto, no se cerro por decreto del modelo")))


(deftest protocol/un-comando-nunca-cierra-un-goal-de-fichero
  "I3, fijado: un command-exec no es evidencia de escritura.

   Pytest que pasa no significa que el archivo se escribiera. TODO-EVIDENCE-
   TYPES ya es 1-1 estricto, asi que esto HOY PASA; el test existe para que no
   se rompa al construir la maquina de estados, que es cuando va a ser tentador
   abrirlo para que los goals se cierren solos."
  (with-turn-engine
    (reset-turn-engine)
    (h::add-todo "Write app.py" :status "in-progress")
    (h::assert (h::harness-fact (h::fact-type "command-exec")
                                (h::timestamp (get-universal-time))
                                (h::data (list :command "pytest -q"
                                               :exit-code 0
                                               :output "1 passed"
                                               :turn-id (h::current-turn-id)))))
    (h::reconcile-todos (h::collect-active-facts))
    (ok (string= "in-progress" (todo-slot (first (active-todos)) "status"))
        (format nil "un comando no escribe ficheros; quedo ~A"
                (todo-slot (first (active-todos)) "status")))))


(deftest protocol/el-verdict-llega-al-modelo-con-su-paso
  "I1 cerrado de verdad: asertar el veredicto no basta, tiene que LLEGAR.

   El invariante 1 se puede cumplir a medias y seguir siendo inutil: si el
   harness aserta el veredicto pero el contexto no lo renderiza, el modelo
   recibe un turno entero sin enterarse de nada y el estado solo existe para
   el debugger. Este test falla si el VERDICT se deja de renderizar.

   Tambien fija el detalle de que :step viaje. Y que 'result' distinga
   applied de failed: :applied y :failed son keywords y los DOS son truthy,
   asi que un test de verdad sobre el texto renderizado es la unica forma de
   pillar un (if (data-get data :verdict)) que diga 'applied' siempre."
  (with-temp-dir (dir)
    (let ((h::*base-dir* dir) (h::*session-id* "s"))
      (with-turn-engine
        (reset-turn-engine)
        (h::assert-step-verdict (list :step 1 :action :write-file :path "uno.txt")
                                (list :applied t))
        (h::assert-step-verdict (list :step 2 :action :edit-file :path "uno.txt")
                                (list :applied nil
                                      :reason "old_string not found"))
        (let ((yaml (build-yaml-context "sigue")))
          (ok (search "verdict:" yaml) "el veredicto se renderiza en el contexto")
          (ok (search "step: 1" yaml) "el veredicto conserva su numero de paso")
          (ok (search "step: 2" yaml)
              "cada veredicto apunta a SU paso, no a un 'fallo' generico")
          (ok (search "result: applied" yaml) "un paso ok se distingue de uno roto")
          (ok (search "result: failed" yaml)
              "un fallo NO se presenta como applied aunque el keyword sea truthy")
          (ok (search "old_string not found" yaml)
              "el motivo del rechazo viaja al modelo, no solo al debugger"))))))


(deftest protocol/el-plan-sobrevive-como-plan
  "Cierre de I1/I4: el plan no solo se puede recuperar, se LEE.

   El test de I1 cuenta hechos con :step. Eso prueba que la informacion no se
   perdio, pero no que el modelo la pueda usar: hoy el modelo ve una lista de
   veredictos sueltos (paso 2 fallo) sin ver que PIDO en el paso 2. Para
   reintentar bien necesita el par, no dos mitades.

   Asi que se exige un bloque plan: en el que cada paso aparece con su accion, su
   objetivo, y el veredicto del harness al lado, incluyendo 'pending' para lo que
   aun no se ha ejecutado. Pending es lo que hace util el bloque: convierte el
   contexto en un estado de la maquina y no en un parte.

   Y se exige lo que I4 pedia: el batch-plan NO se retracta al ejecutarse. Si
   desaparece con el intention, esto es I1 con otro nombre.

   OJO con como se monta el pending: SELECT-RELEVANT-FACTS llama a (RUN), asi
   que construir el contexto EJECUTA el plan entero. No hay forma de ver un
   paso pendiente asi. El estado real donde un paso queda pending es despues de
   un aborto: CANCEL-INTENTIONS-ON-ABORT retracta las intenciones que quedan y el
   plan se queda sin ejecutarlas. Ahi se comprueba."
  (with-temp-dir (dir)
    (let ((h::*base-dir* dir) (h:*session-id* "s"))
      (with-turn-engine
        (reset-turn-engine)
        (h::write-file "uno.txt" "contenido real")
        (h::assert-batch-intention (list :action :write-file :path "uno.txt"
                                         :content "1") 1)
        (h::assert-batch-intention (list :action :edit-file :path "uno.txt"
                                         :old-string "nada de esto"
                                         :new-string "x") 2)
        (h::assert-batch-intention (list :action :read-file :path "uno.txt") 3)
        (h::assert (h::harness-fact (h::fact-type "verdict")
                                    (h::timestamp (get-universal-time))
                                    (h::data (list :step 1 :action :write-file
                                                   :target "uno.txt"
                                                   :verdict :applied :turn-id 1))))
        (h::assert (h::harness-fact (h::fact-type "verdict")
                                    (h::timestamp (get-universal-time))
                                    (h::data (list :step 2 :action :edit-file
                                                   :target "uno.txt"
                                                   :verdict :failed
                                                   :reason "old_string not found in uno.txt"
                                                   :turn-id 1))))
        ;; Estado post-aborto: el plan del paso 3 sobrevive, su intencion no.
        (h::assert (h::harness-fact (h::fact-type "batch-abort")
                                    (h::timestamp (get-universal-time))
                                    (h::data (list :reason "Blind write attempt" :turn-id 1))))
        (let* ((plans (batch-plan-facts))
               (ctx (build-yaml-context "sigue"))
               (plan (card-section ctx)))
          (ok (= 3 (length plans))
              (format nil "los 3 pasos del plan quedan registrados; hay ~D" (length plans)))
          (ok (and (equal 1 (h::data-get (first plans) :step))
                   (eql :write-file (h::data-get (first plans) :action)))
              "y en orden, con su numero de paso y su accion")
          (ok (search "PROCEDURE DIVISION." ctx)
              "el plan se renderiza como tarjeta COBOL en el contexto")
          (ok (search "01." plan) "el bloque lista los pasos numerados a la COBOL")
          (ok (search "APPLIED" plan) "con el veredicto del harness al lado")
          (ok (search "PENDING" plan)
              "un paso sin ejecutar se ve PENDING: el estado de la maquina, no un parte")
          (ok (search "old_string not found" plan)
              "el motivo del fallo queda unido a SU paso"))))))


(deftest protocol/un-paso-malformado-no-arrastra-a-los-demas
  "El plan se renderiza desde hechos, y un hecho puede llegar roto.

   Un batch-plan sin :step, o con un :step que no es un numero, no puede
   reventar el render entero: perder el estado de los pasos buenos por un dato
   basura en uno es la peor forma de perderlo, porque ademas es silenciosa. Se
   descarta el paso malo y el resto se muestra igual.

   Aqui no se usa assert-batch-intention porque se normaliza y dejaria el dato
   bueno: se aserta el hecho a proposito, como entraria desde disco o de un
   motor Rete con estado viejo."
  (with-temp-dir (dir)
    (let ((h::*base-dir* dir) (h:*session-id* "s"))
      (with-turn-engine
        (reset-turn-engine)
        (dolist (step '(1 2 3))
          (h::assert-batch-intention (list :action :read-file :path "a.txt") step))
        ;; Dos hechos basura intercalados, como si el estado estuviera sucio.
        (h::assert (h::harness-fact (h::fact-type "batch-plan")
                                    (h::timestamp (get-universal-time))
                                    (h::data (list :action :read-file :turn-id 1))))
        (h::assert (h::harness-fact (h::fact-type "batch-plan")
                                    (h::timestamp (get-universal-time))
                                    (h::data (list :step "no-so-numero" :action :read-file
                                                   :turn-id 1))))
        ;; Esto NO debe lanzar: es el fallo que se vigila.
        (let ((ctx (build-yaml-context "sigue")))
          (ok (search "PROCEDURE DIVISION." ctx)
              "los pasos validos se siguen mostrando")
          (ok (search "01." ctx))
          (ok (search "02." ctx))
          (ok (search "03." ctx)
              "un paso sin numero no borra a los demas"))))))


(deftest config/retry-does-not-repeat-client-errors
  (with-test-config (("llm_http_attempts" 3) ("llm_http_backoff_seconds" 0))
    ;; LET* (no LET): el initform de OUTCOME necesita CALLS ya ligado.
    ;; El HANDLER-CASE va FUERA del OK: dentro del macro no se comporta
    ;; (mismo problema que con REDUCE :INITIAL-VALUE).
    (let* ((calls 0)
           (outcome (handler-case
                       (progn (h::call-with-retry
                                (lambda ()
                                  (incf calls)
                                  (error (http-failure 400))))
                              :sin-error)
                      (error () :error))))
       (ok (eq outcome :error)
           "un 400 no se reintenta: la petición está mal, no el servidor")
           (ok (= 1 calls) (format nil "un solo intento, hizo ~D" calls)))))


(deftest config/retry-stops-at-the-attempt-cap
  (with-test-config (("llm_http_attempts" 2) ("llm_http_backoff_seconds" 0))
    (let* ((calls 0)
           (outcome (handler-case
                        (progn (h::call-with-retry
                                 (lambda ()
                                   (incf calls)
                                   (error (http-failure 429))))
                               :sin-error)
                      (error () :error))))
      (ok (eq outcome :error) "agotados los intentos, el error sube")
      (ok (= 2 calls) (format nil "respeta el tope de intentos, hizo ~D" calls)))))


(deftest config/retry-retries-transport-errors
  "Sin status HTTP (DNS, connection reset, timeout) el error también es
   reintentable: son fallos de transporte, no de la petición."
  (with-test-config (("llm_http_attempts" 2) ("llm_http_backoff_seconds" 0))
    (let* ((calls 0)
           (result (h::call-with-retry
                    (lambda ()
                      (incf calls)
                      (if (= calls 1) (error "connection reset") "ok")))))
      (ok (string= "ok" result) "un fallo de transporte también se reintenta")
      (ok (= 2 calls) (format nil "hizo 2 intentos, hizo ~D" calls)))))
