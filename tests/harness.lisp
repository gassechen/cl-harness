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

(deftest batch-parsing/response-only-batch-completes
  (with-turn-engine
    (reset-turn-engine)
    (multiple-value-bind (ok-p response error)
        (h::parse-llm-batch-to-intentions "{\"tool_calls\":[],\"response\":\"listo\"}")
      (ng error)
      (ng ok-p)
      (ok (string= "listo" response))
      (ok (= 1 (fact-count "batch-complete"))))))

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
          (ok (getf result :error))
          (ok (search "old_string" (getf result :error)))
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
          (ok (search "not found" (getf result :error)))))
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
          (ok (search "not found" (getf result :error))))))))

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
          (ok (search "edit_file" (getf result :error)))))
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
        (let ((contents (getf (read-file "no-existe.lisp") :contents)))
          (ok (search "File not found" contents)))))))

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

;;; ---------------------------------------------------------------
;;; Configuración: límites de batch y política de reintentos
;;; ---------------------------------------------------------------

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
   respuesta de texto, así que el bucle nunca encuentra batch-complete."
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
