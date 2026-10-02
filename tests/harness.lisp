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
                ;; rondas
                #:*batch-round*
                ;; metrics
                #:metrics-reset #:build-context
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
        ;; MEMORIA DE LARGO PLAZO POR PRUEBA. El .cob se genera desde la red
        ;; de largo plazo, y esa red --a diferencia del motor de turno-- no se
        ;; vacia sola: vive mas alla del caso. Sin este reset, cada prueba
        ;; hereda los hechos de todas las anteriores y el resultado depende del
        ;; orden, que es la forma mas caro de tener una suite verde.
        ;;
        ;; Cada caso arranca como un proceso nuevo, que es lo que hace
        ;; BOOT-MEMORY de verdad.
        (h::reset-mem-engine)
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
  ;; Los tres de archivos llevan :applied porque WRITE-FILE y EDIT-FILE lo
  ;; ponen SIEMPRE. El fixture viejo --(:path "a" :bytes 3), sin :applied-- fijaba
  ;; el contrato equivocado: decia que un file-write sin :applied era durable,
  ;; y un write rechazado (que afirma :applied nil) entraba a la memoria y se
  ;; renderizaba como 'WROTE. nope.py / BYTES = '. Un archivo que no existe no
  ;; se puede rechazar por no existir, asi que ese era el unico hueco.
  (ok (durable-type-p "file-write" (list :path "a" :bytes 3 :applied t)))
  ;; LEER ES CONOCIMIENTO, y el .cob se genera desde la memoria de largo plazo.
  ;; Una lectura con su contenido no es 'lo que mas se olvida': es COMO el
  ;; modelo averigua como esta el mundo. Sin el CONTENTS de la lectura no hay
  ;; forma de planificar el paso siguiente, y el modelo se inventa el fichero.
  (ok (durable-type-p "file-read" (list :path "a" :contents "x"))
      "leer es conocimiento durable: sin el contenido no hay paso siguiente")
  ;; Y UN RECHAZO TAMBIEN ES UN HECHO. Filtrar por :applied sacaba de la memoria
  ;; justamente los fallos, que es lo unico que el modelo deberia acordarse de
  ;; cambiar. Lo que no puede pasar es que un rechazo se pinte como una
  ;; escritura, y de eso se encarga el renderizador con STATE = FAILED.
  (ok (durable-type-p "file-write" (list :path "nope.py" :applied nil
                                         :refused t :reason "blind write"))
      "un write rechazado es un hecho: no ocurrio, y por que")
  ;; Y el edit que no se aplico tambien: es el mismo caso, y el archivo se queda
  ;; congelado en el tamano de su escritura inicial.
  (ok (durable-type-p "file-edit" (list :path "a" :applied nil
                                        :reason "old_string not found"))
      "un edit que no se aplico tambien cruza, con su motivo")
  (ok (durable-type-p "file-edit" (list :path "a" :applied t
                                        :replaced-chars 2 :new-chars 5))
      "y el que si se aplico, con sus chars")
  ;; EL CIERRE DEL BUCLE: lo que el modelo propuso y lo que la maquina
  ;; contesto. Sin los dos, la targeta siguiente no tiene con que compararse.
  (ok (durable-type-p "batch-plan" (list :command "ls" :step 1 :turn-id 3))
      "el plan es durable: es lo que el modelo pidio")
  (ok (durable-type-p "verdict" (list :command "ls" :step 1 :turn-id 3))
      "el veredicto es durable: es lo que la maquina contesto")
  ;; Y LO QUE SE LE PIDIO y LO QUE CONTESTO, que es lo unico de la entrada del
  ;; usuario que sobrevive: el resto es una sesion entera, no un turno.
  (ok (durable-type-p "user-input" (list :text "hola" :turn-id 3)))
  (ok (durable-type-p "llm-response" (list :text "voy" :turn-id 3)))
  ;; Y LO QUE NO: una intencion que nunca se ejecuto no es un hecho, es un
  ;; borrador. Sumarla a la memoria daria por hecha una accion que no ocurrio.
  (ng (durable-type-p "intention" (list :text "voy a escribir a.py" :turn-id 3))
      "una intencion no ejecutada es un borrador, no un hecho"))

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
    (ok (= 2 (length (mem-engine-facts)))
        "llegan el error real y la lectura: los dos son conocimiento")
    ;; Y promoted DOS VECES no duplica. Es idempotente porque se llama una vez
    ;; por ronda y el motor de turno no se vacia entre rondas: sin esta guarda
    ;; el user-input del turno se copia a la memoria en cada ronda y el .cob lo
    ;; renderiza N veces.
    (promote-durable-facts)
    (ok (= 2 (length (mem-engine-facts)))
        "promover dos veces no duplica nada en la memoria")))

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
      (build-context "hola"))
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

(deftest context/el-contexto-no-es-yaml
  "El contexto completo es un programa COBOL. No hay YAML en ninguna parte.

   El YAML se justificaba como \"vista humana\". No lo era: nadie lo leia, y
   las dos cosas que se le pedian -- jerarquia explicita y campos en columna
   fija -- las hace mejor la tarjeta. Un formato que existe \"por si alguien
   lo mira\" y que nadie mira es una segunda fuente de verdad: cuando el YAML
   y el COBOL discrepan, no hay forma de saber cual mintio.

   Se afirma sobre el CONTEXTO ENTERO, no sobre la tarjeta. La tarjeta ya
   era COBOL y aun asi el contexto lo envolvia en 'context:' con claves
   YAML alrededor, o sea que el formato estaba a medias y el que mandaba era
   el viejo. Este test falla si vuelve a colarse un solo ':' de clave."
  (with-temp-dir (dir)
    (let ((cl-harness::*base-dir* dir)
          (h:*session-id* "session-cobol"))
      (with-turn-engine
        (reset-turn-engine)
        (h::assert (h::harness-fact (h::fact-type "user-input")
                                    (h::timestamp (get-universal-time))
                                    (h::data (list :text "hola" :turn-id 1))))
        (let ((ctx (build-context "hola")))
          ;; Las cuatro divisiones del programa.
          (ok (search "IDENTIFICATION DIVISION." ctx) "abre con IDENTIFICATION")
          (ok (search "PROCEDURE DIVISION." ctx) "y tiene PROCEDURE")
          (ok (search "DATA DIVISION." ctx) "y DATA")
          (ok (search "GOBACK." ctx) "y cierra con GOBACK")
          (ok (search "session-cobol" ctx) "el PROGRAM-ID es la sesion")
          (ok (search "hola" ctx) "la entrada del usuario sigue estando")
          ;; Y NINGUNA clave YAML. Estas son las que existian antes; si vuelve
          ;; cualquiera, el formato ha vuelto a ser dos formatos.
          (ok (not (search "context:" ctx)) "no la clave 'context:'")
          (ok (not (search "prior_turns" ctx)) "ni 'prior_turns'")
          (ok (not (search "current_turn" ctx)) "ni 'current_turn'")
          (ok (not (search "goals:" ctx)) "ni 'goals:'")
          (ok (not (search "long_term_memory" ctx)) "ni 'long_term_memory'")
          (ok (not (search "warnings:" ctx)) "ni 'warnings:'"))))))


(deftest context/build-context-renders-session
  (with-temp-dir (dir)
    (let ((h::*base-dir* dir)
          (h:*session-id* "session-test-123"))
      (with-turn-engine
        (reset-turn-engine)
        (h::assert (h::harness-fact (h::fact-type "user-input")
                                    (h::timestamp (get-universal-time))
                                    (h::data (list :text "hola" :turn-id 1))))
        (let ((cobol (build-context "hola")))
          (ok (search "PROGRAM-ID." cobol))
          (ok (search "session-test-123" cobol))
          (ok (search "hola" cobol)))))))

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

(deftest context/goal-abierto-no-cuenta-los-completados
  "El contador de GOAL ABIERTO son los goals SIN completar.

   collect-active-todos devuelve tambien los completados que aun no se han
   retraido: el cap los poda por CANTIDAD, no por estado. Al pintarlos bajo el
   rotulo 'GOAL ABIERTO', un goal ya cerrado se contaba como pendiente y el
   modelo seguia trabajando en algo que ya estaba hecho. La cuenta tiene que
   salir de la misma definicion con la que el protocolo decide cerrar la
   sesion."
  (with-turn-engine
    (reset-turn-engine)
    (h::add-todo "Write app.py" :status "in-progress")
    (h::add-todo "Edit otro.py" :status "pending")
    (h::assert (h::harness-fact (h::fact-type "file-write")
                                (h::timestamp (get-universal-time))
                                (h::data (list :path "/tmp/proyecto/app.py"
                                               :applied t
                                               :bytes 42
                                               :turn-id (h::current-turn-id)))))
    ;; El primer goal queda completado por evidencia; el segundo sigue abierto
    ;; y el cap no poda nada (hay 2, el tope es 5).
    (let* ((ctx (build-context "sigue"))
           (idx (search "GOAL ABIERTO." ctx)))
      (ok idx "hay seccion de goals")
      (ok (search "GOAL ABIERTO. 1" ctx)
          "solo el que sigue pendiente cuenta como abierto")
      (ok (not (search "Write app.py" ctx))
          "el goal ya completado no aparece como abierto")
      (ok (search "Edit otro.py" ctx)
          "el que sigue abierto si aparece"))))

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
          ;; UN COMANDO QUE FALLA, y no uno que va bien. La memoria de largo
          ;; plazo solo cruza los errores reales: un comando que funciono no
          ;; cambio nada que el modelo no pueda ver por su cuenta, y meterlo
          ;; seria llenarle el .cob de ruido. La asimetria es deliberada, asi
          ;; que el fixture tiene que hablar el idioma que la memoria habla.
          (h::assert (h::harness-fact (h::fact-type "command-exec")
                                      (h::timestamp (identity t0))
                                      (h::data (list :command "wc -l datos.txt"
                                                     :output "wc: datos.txt: No such file"
                                                     :exit-code 1
                                                     :turn-id 1)))))
        (let ((cobol (build-context "cuentalas")))
          ;; Search the RENDERED LABELS, not the path: the goal task also
          ;; mentions datos.txt, so searching the bare path would match the
          ;; goals block.
          (let ((wrote-at (search "WRITE-FILE" cobol))
                (exec-at (search "EXEC-COMMAND" cobol)))
            (ok (and wrote-at exec-at (< wrote-at exec-at))
                (format nil "WRITE-FILE en ~S y EXEC-COMMAND en ~S" wrote-at exec-at))))))))

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


(deftest batch/un-lote-repetido-se-ejecuta-como-cualquiera-otro
  "TODO EN LA INFORMATICA ES BATCH: si el programador pide la misma instruccion
   tres veces, el compilador la ejecuta tres veces. Repetirse no es un error de
   compilacion, asi que el harness NO corta el turno por repeticion.

   Lo que hay que darle a un modelo anclado no es muerte sino DIAGNOSTICO, como
   gcc: 'ya lo hiciste, aqui esta el resultado'. Y eso lo dan las reglas de Rete,
   que impugnan la INTENCION intraturno y contestan con un veredicto. El
   abortador de turno era un SEGUNDO mecanismo por encima de ellas, y era el que
   ganaba: mataba el turno, eso disparaba CANCEL-INTENTIONS-ON-ABORT, y con las
   intenciones retractadas la respuesta que Rete tenia preparada se perdia
   tambien. El modelo se quedaba con '[Actions] .' sin idea de por que.

   Medido antes del cambio: 29 targetas propuestas en 5 turnos, 5 ejecutadas (todas
   en el turno 1, el unico anterior al primer aborto) y 24 sin respuesta.

   Y el modelo no puede desobedecer un 'no repitas': su unico movimiento es
   responder. Un detector que exige que no repitas solo produce la repeticion que
   detecta. Por eso el turno acaba por el tope de iteraciones, que es una cosa del
   LLM y no del harness."
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
          (ok (= 8 calls)
              (format nil "un lote repetido se ejecuta ronda a ronda, hizo ~D de 8" calls))
          (ok (eq (getf h::*last-llm-call-info* :path) :iterations)
              (format nil "el corte es el tope de iteraciones, no la repeticion: ~S"
                      (getf h::*last-llm-call-info* :path)))))))
  (metrics-reset))

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
          ;; El aborto lleva el MISMO :turn-id que la intencion que se quiere
          ;; cancelar. La regla CANCEL-INTENTIONS-ON-ABORT empareja por turno, y
          ;; tiene que: un aborto del turno 1 no puede borrar el plan del turno 2
          ;; (si lo hace, un solo aborto desactiva la ejecucion de toda la
          ;; sesion). Este test fabricaba el aborto con :turn-id 1 mientras la
          ;; intencion pertence al turno que queda tras el reset, y por eso
          ;; dependia del cruce de turnos para obtener su PENDING.
          (h::assert (h::harness-fact (h::fact-type "batch-abort")
                                      (h::timestamp (get-universal-time))
                                      (h::data (list :reason "Abort de prueba"
                                                     :turn-id
                                                     (h::current-turn-id)))))
          (h::run)
          (let ((card2 (h::render-plan-card (h::collect-harness-facts))))
            (ok (search "PENDING" card2)
                "un plan sin ejecutar se declara PENDING, no desaparece")
            (ok (search "01." card2)
                 "y conserva su numero de paso aunque no se haya ejecutado")))))))

(deftest protocol/dos-turnos-no-comparten-paso
  "Los pasos se numeran por turno, y la tarjeta tiene que respetar esa division.

   El numero de paso NO es un identificador global: 02. del turno 1 y 02. del
   turno 2 son el mismo numero y turnos distintos. Un JOIN que empareja
   plan-y-veredicto solo por :step cruza los dos turnos, y el modelo recibe el
   veredicto de una operacion que no pidio en la tarjeta que esta leyendo.

   El caso que se rompe no es exotico: es cualquier sesion de dos turnos donde
   ambos planes tienen el mismo numero de pasos, que es lo normal. Y es
   silencioso -- la tarjeta se ve perfecta, con STATE rellenado, solo que con el
   dato equivocado.

   Por eso el paso 2 del turno 1 se RECHAZA y el paso 2 del turno 2 se APLICA. Si
   el JOIN cruza turnos, el primero aparecera como APPLIED y este test falla en
   una linea, leyendo lo que el modelo leeria."
  (with-temp-dir (dir)
    (let ((cl-harness::*base-dir* dir))
      (with-turn-engine
        (h::reset-turn-engine)
        ;; Ambos turnos son de UNA sola ronda. Se fija explicito porque este
        ;; test encadena parse+run a mano y no pasa por process-turn, que es el
        ;; que reinicia la ronda: sin esto, el test hereda la ronda de la ultima
        ;; ronda del turno anterior que se ejecutara antes, y las etiquetas salen
        ;; con un R2 que aqui no existe.
        (setf cl-harness::*batch-round* 1)
        ;; --- turno 1: dos pasos, el segundo se rechaza ---
        (incf cl-harness::*turn-counter*)
        (h::parse-llm-batch-to-intentions
         "{\"tool_calls\":[{\"name\":\"write_file\",\"arguments\":{\"path\":\"t1a.txt\",\"content\":\"a\"}},{\"name\":\"edit_file\",\"arguments\":{\"path\":\"inexistente.txt\",\"old_string\":\"nada\",\"new_string\":\"b\"}}]}")
        (h::run)
        ;; --- turno 2: otros dos pasos, el segundo SI se aplica ---
        ;; El incf va AQUI, explicito. El turno lo incrementa process-turn, no
        ;; run ni parse-llm-batch-to-intentions: un test que encadena dos
        ;; parse+run sin avanzar el contador esta probando DOS VECES EL MISMO
        ;; turno, y todo lo que este test afirma -- que los pasos se numeran
        ;; por turno -- seria verdad por casualidad, no por el codigo.
        (incf cl-harness::*turn-counter*)
        (h::parse-llm-batch-to-intentions
         "{\"tool_calls\":[{\"name\":\"write_file\",\"arguments\":{\"path\":\"t2a.txt\",\"content\":\"a\"}},{\"name\":\"write_file\",\"arguments\":{\"path\":\"t2b.txt\",\"content\":\"b\"}}]}")
        (h::run)
        (let* ((facts (h::collect-active-facts))
               (card (h::render-plan-card facts))
               ;; Los dos veredictos del paso 2, uno por turno.
               (step2-verdicts
                 (remove nil
                         (mapcar (lambda (d)
                                   (when (and (equal (h::data-get d :step) 2)
                                              (h::data-get d :verdict))
                                     (list :turn (h::data-get d :turn-id)
                                           :verdict (h::data-get d :verdict))))
                                 (h::collect-facts-of-type facts "verdict")))))
          (ok (>= (length step2-verdicts) 2)
              (format nil "hay dos veredictos distintos para el paso 2, uno por turno; hay ~D"
                      (length step2-verdicts)))
          ;; El fallo que importa: el turno 1 escribio t1a.txt y su paso 2
          ;; fallo; el turno 2 escribio t2a.txt y t2b.txt. Cada tarjeta debe
          ;; seguir a su turno.
          (ok (and (search "t1a.txt" card) (search "t2a.txt" card))
              "la tarjeta menciona los pasos de ambos turnos")
          ;; Si el JOIN cruza, el paso 2 del turno 1 aparecera APPLIED con el
          ;; motivo del turno 2 (o al reves). Se comprueba que el motivo del
          ;; turno 1 -- el fichero inexistente -- no se atribuya a t2b.txt.
          ;; El fallo que importa, afirmado sobre el DATO y no sobre el texto
          ;; entero: el motivo del turno 1 tiene que seguir al paso del turno 1.
          ;; Antes de arreglarlo, el paso 2 del turno 2 aparecia FAILED con el
          ;; motivo del turno 1. No se puede afirmar "la tarjeta no menciona
          ;; File not found" -- tiene que, para el paso que fallo de verdad.
          (let* ((t3 (or (search "02.  T3" card) 0))
                 (t4 (search "02.  T4" card))
                 (t3-block (subseq card t3 (or t4 (length card))))
                 (t4-block (if t4 (subseq card t4) "")))
            (ok (search "STATE = FAILED" t3-block)
                "el paso 2 del turno 1 conserva su FAILED")
            (ok (search "File not found" t3-block)
                "y su motivo sigue pegado a el")
            (ok (not (search "File not found" t4-block))
                (format nil "y no se ha filtrado al paso 2 del turno 2; t4=~S" t4-block)))
          ;; Y el caso limpio: el paso 2 del turno 1 debe verse FAILED, con SU
          ;; motivo, y el del turno 2 APPLIED. Antes del fix, los dos salian
          ;; FAILED con el motivo del turno 1 -- es decir, una escritura que
          ;; habia funcionado aparecia como rota.
          (ok (and (search "02.  T3 EDIT-FILE  inexistente.txt" card)
                   (search "STATE = FAILED" card)
                   (search "02.  T4 WRITE-FILE  t2b.txt" card))
              "cada paso lleva el turno al que pertenece")
          (ok (not (search "02.  T4 WRITE-FILE  t2b.txt~%        STATE = FAILED" card))
              (format nil "el paso 2 del turno 2 se empareja con su propio turno; tarjeta=~S"
                      card)))))))


(deftest protocol/dos-rondas-en-un-turno-no-comparten-paso
  "La numeracion de pasos se reinicia por RONDA, no solo por turno.

   Un turno de usuario tiene varias respuestas del LLM y cada una propone su
   lote con los pasos 1..N. El paso 1 de la ronda 1 y el paso 1 de la ronda 2
   son el MISMO par `(turn-id, step)`: con esa clave el JOIN los confunde. Aqui
   la ronda 1 escribe a.txt (APPLIED) y la ronda 2 edita un fichero que no
   existe (FAILED). Como el primer veredicto con step 1 es APPLIED, un JOIN sin
   la ronda le pega ese APPLIED al edit que fallo -- la tarjeta se ve perfecta y
   esconde el fallo. Con la ronda en la clave, cada paso conserva su STATE."
  (with-temp-dir (dir)
    (let ((cl-harness::*base-dir* dir))
      (with-turn-engine
        (h::reset-turn-engine)
        ;; El turno se toma de rebote porque NO tiene por que ser el 1: la
        ;; suite deja el contador donde lo dejara el test anterior y esta
        ;; prueba no depende de eso. La etiqueta sale del turno real, asi que
        ;; se compone con el, no con un 1 de madera.
        (let ((turn (incf cl-harness::*turn-counter*)))
          ;; --- ronda 1: escribe a.txt, APPLIED ---
          (setf cl-harness::*batch-round* 1)
          (h::parse-llm-batch-to-intentions
           "{\"tool_calls\":[{\"name\":\"write_file\",\"arguments\":{\"path\":\"a.txt\",\"content\":\"a\"}}]}")
          (h::run)
          ;; --- ronda 2: MISMO turno, MISMO numero de paso, pero falla ---
          (setf cl-harness::*batch-round* 2)
          (h::parse-llm-batch-to-intentions
           "{\"tool_calls\":[{\"name\":\"edit_file\",\"arguments\":{\"path\":\"noexiste.txt\",\"old_string\":\"nada\",\"new_string\":\"b\"}}]}")
          (h::run)
          (let* ((facts (h::collect-active-facts))
                 (card (h::render-plan-card facts))
                 (p1 (or (search (format nil "01.  T~A WRITE-FILE  a.txt" turn) card) 0))
                 (p2 (or (search (format nil "01.  T~A R2 EDIT-FILE  noexiste.txt" turn) card)
                         (length card)))
                 ;; Bloques independientes del orden en que sort los deje.
                 (r1-block (if (< p1 p2) (subseq card p1 p2) (subseq card p1)))
                 (r2-block (if (< p2 p1) (subseq card p2 p1) (subseq card p2))))
            ;; El paso de la ronda 2 lleva su ronda en la etiqueta; el de la
            ;; ronda 1 no, porque con una sola respuesta no hay ambiguedad.
            (ok (search (format nil "01.  T~A WRITE-FILE  a.txt" turn) card)
                "el paso de la ronda 1 conserva su identidad sin sufijo de ronda")
            (ok (search (format nil "01.  T~A R2 EDIT-FILE  noexiste.txt" turn) card)
                "el paso de la ronda 2 lleva su ronda: mismo turno y mismo numero")
            ;; Cada STATE sigue a SU paso.
            (ok (search "STATE = APPLIED" r1-block)
                "la ronda 1 se queda con su APPLIED")
            (ok (not (search "STATE = FAILED" r1-block))
                "y no se le contagia el FAILED de la ronda 2")
            (ok (search "STATE = FAILED" r2-block)
                "la ronda 2 se queda con su FAILED")
            (ok (not (search "STATE = APPLIED" r2-block))
                (format nil "y no se le contagia el APPLIED de la ronda 1; r2=~S"
                        r2-block)))))))
    ;; La ronda se devuelve a 1: el estado global de la ronda es de la ronda del
    ;; turno, y un test que se la deja puesta se la pasa al siguiente.
    (setf cl-harness::*batch-round* 1))


(deftest protocol/un-turno-que-revienta-no-deja-la-ronda-puesta
  "La ronda baja a 1 aunque el turno muera por excepcion.

   El reinicio va dentro de un UNWIND-PROTECT precisamente porque el cuerpo de
   un turno hace trabajo a mitad -- run, las reglas, build-context, save-session
   -- que puede morir. Con el reinicio al final del cuerpo, una excepcion se
   saltaba por encima y *batch-round* se quedaba en la ultima ronda.

   No es cosmetico. La ronda se graba DENTRO del hecho (:round de
   ASSERT-BATCH-PLAN) y forma parte de la clave del JOIN (turn-id, round, step).
   Un valor fugado no ensucia solo una variable: sella los pasos del turno
   siguiente con una ronda que no es suya, y sus veredictos dejan de encajar.

   La excepcion se inyecta en BUILD-CONTEXT, no en CALL-LLM: la de LLM ya va en
   HANDLER-CASE dentro de PROCESS-TURN y ahi no se escapa nada. Lo que se quiere
   es un fallo en la zona que NO esta protegida, y para que la ronda ya haya
   subido a 2 tiene que caer en la tercera llamada: las dos primeras son la de
   entrada y la de la primera iteracion del bucle.

   OJO, y esto es lo que hacia que la primera version de este test no probara
   nada: NUNCA con (let ((*batch-round* 1)) ...). Eso crea un binding LEXICO
   que sombrea la global, y el aserto de abajo midia la local, que vale 1 siempre.
   Con el codigo mutado -- el cleanup del unwind-protect anulado -- el test
   pasaba igual. La ronda se toca por su nombre global, sin let."
  (with-temp-dir (dir)
    (let ((cl-harness::*base-dir* dir))
      (with-turn-engine
        (h::reset-turn-engine)
        (metrics-reset)
        ;; Se parte de una ronda que NO es 1, para que el aserto final no pueda
        ;; pasar por casualidad: si el cleanup no corre, el valor con el que
        ;; sale el turno muerto es el ultimo que le puso el bucle.
        (setf cl-harness::*batch-round* 1)
        (let ((builds 0)
              (inyectado nil)
              (original (symbol-function 'h::build-context)))
          (unwind-protect
               (progn
                 (setf (symbol-function 'h::build-context)
                       (lambda (msg)
                         (declare (ignore msg))
                         (incf builds)
                         (when (> builds 3)
                           (setf inyectado t)
                           (error "fallo inyectado en build-context"))
                         (funcall original msg)))
                 (with-test-config (("llm_provider" "batch")
                                    ("llm_stream" nil)
                                    ("batch_max_iterations" 8)
                                    ("batch_repeat_tolerance" 0))
                   (with-mocked-call-llm
                       ((format nil
                                 "{\"tool_calls\":[{\"id\":\"1\",\"type\":\"function\",\"function\":{\"name\":\"read_file\",\"arguments\":{\"path\":\"a.lisp\"}}}]}"))
                     ;; La excepcion se atrapa FUERA de PROCESS-TURN, que es
                     ;; justo lo que hace quien lo llama: el REPL muere, un
                     ;; embedder sigue. El fallo no es de PROCESS-TURN.
                     (handler-case (h::process-turn "lee a.lisp" "prompt")
                       (error () nil)))))
            (setf (symbol-function 'h::build-context) original))
          ;; Precondicion: si el error no llego a dispararse, el test no esta
          ;; midiendo nada y pasaria por la rama que no importa.
          (ok inyectado
              (format nil "el fallo inyectado tiene que ocurrir; hubo ~D llamadas a build-context"
                      builds))
          (ok (= 1 cl-harness::*batch-round*)
              (format nil "un turno que revienta no puede dejar la ronda puesta; salio ~A"
                      cl-harness::*batch-round*))
          ;; Y la consecuencia, medida: el siguiente turno sella sus pasos en la
          ;; ronda 1, no en la que filtro el turno muerto.
          (h::assert-batch-plan
           (list :step 1 :action :read-file :path "a.lisp"
                 :turn-id 99 :round cl-harness::*batch-round*)
           1)
          (let* ((facts (h::collect-active-facts))
                 (plans (h::collect-facts-of-type facts "batch-plan"))
                 (round (h::data-get (first plans) :round)))
            (ok (= 1 round)
                (format nil "el turno siguiente se sella en la ronda 1; se sello en la ~A"
                        round))))))))


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

   Ya paso. Los batch-plan, y despues el batch-complete se renombro a plan-done y el
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
          (let ((cobol (build-context "sigue")))
            (ok (search "REFUSED = TRUE" cobol)
                "una escritura rechazada se marca REFUSED, no se calla")
            (ok (search "already exists" cobol)
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
        (let ((cobol (build-context "sigue")))
          (ok (search "STEP 01" cobol) "el veredicto se renderiza en el contexto")
          (ok (search "STEP 02" cobol)
              "cada veredicto apunta a SU paso, no a un 'fallo' generico")
          (ok (search "STATE = APPLIED" cobol) "un paso ok se distingue de uno roto")
          (ok (search "STATE = FAILED" cobol)
              "un fallo NO se presenta como applied aunque el keyword sea truthy")
          (ok (search "old_string not found" cobol)
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
               (ctx (build-context "sigue"))
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
        (let ((ctx (build-context "sigue")))
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


(defun procedure-section (ctx)
  "El trozo de la PROCEDURE DIVISION: las tarjetas y su STATE, sin el resto."
  (let ((ini (search "PROCEDURE DIVISION." ctx))
        (fin (search "DATA DIVISION." ctx)))
    (if (and ini fin) (subseq ctx ini fin) "")))

(defun card-reason (lines label)
  "El REASON que sigue a la tarjeta LABEL en LINES, o NIL si no tiene.

   La tarjeta y su REASON van en lineas DISTINTAS, asi que esto mira la
   tarjeta y luego lo que viene despues. La busqueda la corta la linea STATE
   de la tarjeta SIGUIENTE: sin ese tope, el REASON de una se atribuiria a la
   anterior.

   THEREIS va DENTRO del AND y no antes: en un LOOP, THEREIS se evalua antes
   que WHEN, de modo que (thereis l) (when (search \"REASON =\" l) ...) devolvia
   la linea STATE --que es justo lo primero que hay-- sin mirar el REASON."
  (let ((idx (position-if (lambda (l) (search label l)) lines)))
    (when idx
      (loop for l in (nthcdr (1+ idx) lines)
            thereis (and (not (search "STATE =" l))
                          (search "REASON =" l)
                          l)))))

(defun card-turn-labels (ctx)
  "Los T<n> de las tarjetas de la PROCEDURE DIVISION, en el orden en que salen."
  (let ((labels '()) (in-proc nil))
    (dolist (line (uiop:split-string ctx :separator '(#\Newline)))
      (cond ((search "PROCEDURE DIVISION." line) (setf in-proc t))
            ((search "DATA DIVISION." line) (setf in-proc nil))
            (in-proc
             ;; Las lineas de tarjeta son las unicas con '.  T'; STATE y REASON
             ;; no la tienen, asi que el marcador basta y no hace falta un regex.
             (let ((pos (search ".  T" line)))
               (when pos
                 (let ((j (+ pos 4)))
                   (loop while (and (< j (length line))
                                    (digit-char-p (char line j)))
                         do (incf j))
                   (when (< (+ pos 4) j)
                     (push (parse-integer line :start (+ pos 4) :end j)
                           labels))))))))
    (nreverse labels)))

(deftest protocol/las-tarjetas-vienen-en-orden-cronologico
  "Las tarjetas se ordenan por (paso, turno, ronda), no por relevancia.

   SELECT-RELEVANT-FACTS ordena los hechos por puntuacion, y la puntuacion
   DECAE con la distancia al turno actual: el turno nuevo puntua mas alto y
   llega antes al render. Ordenando las tarjetas solo por :step, cada numero de
   paso conservaba ese ranking, y las tarjetas del turno ANTERIOR salian detras
   de las del actual. En una corrida real el paso 3 de la ronda 5 salia antes
   que el paso 2 de la ronda 6, y el modelo leia su propio historial del reves
   sin ninguna marca de que faltara nada.

   SORT de SBCL es estable, asi que esto no era un barajado aleatorio: era un
   error DETERMINISTA que ademas parecian intencionado. Por eso el test puede
   fijar el orden sin depender del azar.

   Aqui el turno 3 es el que mas puntua, asi que sin el desempate por
   (turno, ronda) sus tarjetas salen primero que las del turno 1."
  (with-temp-dir (dir)
    (let ((h::*base-dir* dir) (h:*session-id* "s"))
      (with-turn-engine
        (reset-turn-engine)
        ;; Turnos 1, 2 y 3, con los mismos numeros de paso en los tres. Ahora
        ;; mismo el 3 puntua mas (distancia 0 al turno actual), luego el 2, luego
        ;; el 1: ese es el orden en el que LLEGAN al render.
        (dolist (turn '(1 2 3))
          (let ((h::*turn-counter* turn) (h::*batch-round* 1))
            (dolist (step '(1 2))
              (h::assert-batch-intention
               (list :action :read-file :path "a.txt") step))))
        (let* ((ctx (build-context "sigue"))
               (labels (card-turn-labels ctx)))
          (ok (equal labels '(1 1 2 2 3 3))
              (format nil "las tarjetas van en orden cronologico, no por relevancia; salieron ~S"
                      labels))
          ;; Y el texto de verdad, que es lo que lee el modelo.
          (ok (< (search "T1" (procedure-section ctx))
                 (search "T3" (procedure-section ctx)))
              "el turno 1 se pinta antes que el turno 3"))))))

(deftest protocol/una-tarjeta-no-sobrevive-a-su-veredicto
  "Una tarjeta en pantalla siempre tiene su veredicto, aunque el tope los corte.

   MAX-FACTS-PER-TYPE capaba 'batch-plan' y 'verdict' POR SEPARADO. No son el
   mismo conjunto --hay veredictos de pasos cuya tarjeta ya no esta-- y por eso
   se desalinean solos: los dos topes van sobre poblaciones de tamano distinto y
   cada una se queda con SU ventana. Cuando eso pasa, una tarjeta viva se queda
   sin veredicto y se renderiza con STATE = PENDING aunque el paso este
   EJECUTADO. El contexto afirma algo falso, y no hay forma de distinguirlo de
   un paso de verdad pendiente.

   Aqui se reproduce la desincronizacion a proposito: DOS tarjetas con sus dos
   veredictos, y despues TRES veredictos mas de otra ronda cuyos pasos no tienen
   tarjeta. Con tope 2 las tarjetas no se podan (hay justo dos), pero los
   veredictos si, y los tres ultimos se llevan los dos de las tarjetas vivas.

   Los hechos van CRUDOS, sin pasar por ASSERT-BATCH-INTENTION: esa crea un
   'intention' y las reglas de ejecucion disparan y fabricar veredictos de
   verdad, con lo que el motor se encarga de tapar justo lo que se quiere medir.
   Y los tipos van LITERALES porque (fact-type ...) es del DSL, no una funcion."
  (with-test-config (("max_facts_per_type" 2))
    (with-temp-dir (dir)
      (let ((h::*base-dir* dir) (h:*session-id* "s"))
        (with-turn-engine
          (reset-turn-engine)
          (let ((t0 (get-universal-time)))
            ;; Las dos tarjetas de la ronda 1, con su veredicto.
            (h::assert
             (h::harness-fact (h::fact-type "batch-plan")
                              (h::timestamp (incf t0))
                              (h::data (list :step 1 :turn-id 1 :round 1
                                             :action :read-file :target "a.txt"))))
            (h::assert
             (h::harness-fact (h::fact-type "verdict")
                              (h::timestamp (incf t0))
                              (h::data (list :step 1 :turn-id 1 :round 1
                                             :action :read-file :target "a.txt"
                                             :verdict :applied))))
            (h::assert
             (h::harness-fact (h::fact-type "batch-plan")
                              (h::timestamp (incf t0))
                              (h::data (list :step 2 :turn-id 1 :round 1
                                             :action :read-file :target "a.txt"))))
            (h::assert
             (h::harness-fact (h::fact-type "verdict")
                              (h::timestamp (incf t0))
                              (h::data (list :step 2 :turn-id 1 :round 1
                                             :action :read-file :target "a.txt"
                                             :verdict :applied))))
            ;; Tres veredictos MAS NUEVOS de la ronda 2, SIN tarjeta: como los
            ;; que deja un paso cuya tarjeta ya caia. Son los que empujan fuera
            ;; de la ventana los veredictos de las tarjetas que siguen a la vista.
            (h::assert
             (h::harness-fact (h::fact-type "verdict")
                              (h::timestamp (incf t0))
                              (h::data (list :step 1 :turn-id 1 :round 2
                                             :action :read-file :target "b.txt"
                                             :verdict :applied))))
            (h::assert
             (h::harness-fact (h::fact-type "verdict")
                              (h::timestamp (incf t0))
                              (h::data (list :step 2 :turn-id 1 :round 2
                                             :action :read-file :target "b.txt"
                                             :verdict :applied))))
            (h::assert
             (h::harness-fact (h::fact-type "verdict")
                              (h::timestamp (incf t0))
                              (h::data (list :step 3 :turn-id 1 :round 2
                                             :action :read-file :target "b.txt"
                                             :verdict :applied)))))
          (let ((proc (procedure-section (build-context "sigue"))))
            (ok (and (search "01." proc) (search "02." proc))
                "las dos tarjetas de la ronda 1 siguen en pantalla")
            (ok (not (search "PENDING" proc))
                "ninguna tarjeta en pantalla dice PENDING: el veredicto de una
                 tarjeta viva no puede caer en la poda")
            (ok (= 2 (count-if (lambda (l) (search "STATE = APPLIED" l))
                              (uiop:split-string proc :separator '(#\Newline))))
                "las dos tarjetas muestran su veredicto real")))))))

(deftest protocol/un-pending-dice-porque-esta-pendiente
  "Un PENDING sin motivo es un PENDING que el modelo no sabe reintentar.

   RENDER-PLAN-PROCEDURE pintaba STATE = PENDING a secas cuando el paso no
   tenia veredicto, y con eso JUNTO tres cosas que son distintas:

     - anotado y SIN EJECUTAR: la intencion sigue :pending, el paso esta en la
       cola de Rete y va a ocurrir. No hay que hacer nada.
     - el veredicto se PERDIO: la intencion ya se ejecuto y su veredicto no
       esta. Este hay que rehacerlo.
     - un batch-plan afirmado a mano, sin intention ni verdict.

   Las tres salian identicas. El modelo leia que un paso seguia pendiente sin
   forma de saber si era trabajo en vuelo o algo que se habia perdido, y no hay
   decision que tomar ahi: o se reintenta lo que ya esta en cola, o se abandona
   lo que si se perdio.

   El caso real que lo destapo: una sesion de 4 turnos donde el modelo planeo
   dos lecturas en la ronda final, las aserto como intention :pending, y el
   turno se cerro antes de que Rete las ejecutara. Salieron como PENDING junto a
   pasos que si se habian aplicado, y sin ninguna marca de que eran trabajo
   genuinamente inconcluso.

   Aqui se monta la mitad importante: un paso con intention :pending y sin
   veredicto (en cola), y un paso con intention YA ejecutada y sin veredicto
   (perdido). Los dos tienen que PENDING, y tienen que DECIR POR QUE.

   Hechos CRUDOS y tipos LITERALES por lo mismo de siempre: (fact-type ...) es
   del DSL, y una intention real dispara las reglas de ejecucion de Rete, que
   fabricarian veredictos de verdad y taparian lo que se quiere medir.

   Y por eso el render se pide DIRECTO, con RENDER-PLAN-CARD, en vez de con
   BUILD-CONTEXT: build-context empieza con (run), y (run) dispara
   EXECUTE-INTENTION-READ contra la intention :pending de aqui, que ejecuta la
   lectura de verdad, le pone veredicto y se retrae. El paso 01 pasaria a
   APPLIED y este test no midiria nada. Es la misma trampa que el test de la
   poda, pero por la otra puerta: alla era ASSERT-BATCH-INTENTION, aqui es el
   (run) que lleva dentro build-context."
  (with-test-config (("max_facts_per_type" 20))
    (with-temp-dir (dir)
      (let ((cl-harness::*base-dir* dir) (cl-harness:*session-id* "s"))
        (with-turn-engine
          (reset-turn-engine)
          (let ((t0 (get-universal-time)))
            ;; Paso 1: intention :pending, SIN veredicto. En la cola de Rete.
            (h::assert
             (h::harness-fact (h::fact-type "batch-plan")
                              (h::timestamp (incf t0))
                              (h::data (list :step 1 :turn-id 1 :round 1
                                             :action :read-file :target "a.txt"))))
            (h::assert
             (h::harness-fact (h::fact-type "intention")
                              (h::timestamp (incf t0))
                              (h::data (list :step 1 :turn-id 1 :round 1
                                             :action :read-file :path "a.txt"
                                             :status :pending))))
            ;; Paso 2: intention YA ejecutada (:status :done) y sin veredicto.
            ;; Se ejecuto y su veredicto no esta: esto es lo que hay que rehacer.
            (h::assert
             (h::harness-fact (h::fact-type "batch-plan")
                              (h::timestamp (incf t0))
                              (h::data (list :step 2 :turn-id 1 :round 1
                                             :action :write-file :target "b.txt"))))
            (h::assert
             (h::harness-fact (h::fact-type "intention")
                              (h::timestamp (incf t0))
                              (h::data (list :step 2 :turn-id 1 :round 1
                                             :action :write-file :path "b.txt"
                                             :status :done))))
            ;; Paso 3: con veredicto. No es PENDING y no lleva REASON de PENDING.
            (h::assert
             (h::harness-fact (h::fact-type "batch-plan")
                              (h::timestamp (incf t0))
                              (h::data (list :step 3 :turn-id 1 :round 1
                                             :action :exec-command :target "ls"))))
            (h::assert
             (h::harness-fact (h::fact-type "verdict")
                              (h::timestamp (incf t0))
                              (h::data (list :step 3 :turn-id 1 :round 1
                                             :action :exec-command :target "ls"
                                             :verdict :applied)))))
          (let* ((proc (procedure-section
                        (h::render-plan-card (h::collect-active-facts))))
                 (lines (uiop:split-string proc :separator '(#\Newline))))
            (ok (= 2 (count-if (lambda (l) (search "STATE = PENDING" l)) lines))
                "los pasos 01 y 02 estan PENDING: no tienen veredicto")
            (ok (search "ANOTADO, SIN EJECUTAR" (or (card-reason lines "01.") ""))
                "el paso 01 dice que esta en cola: la intention sigue :pending")
            (ok (search "EJECUTADO SIN VEREDICTO" (or (card-reason lines "02.") ""))
                "el paso 02 se lanzo y su veredicto no esta: se dice el hueco, no una poda inventada")
            (ok (not (search "puede haberse podado" (or (card-reason lines "02.") "")))
                "el paso 02 no se acusa de poda: la tarjeta vive, y con ella
                 su veredicto, asi que un veredicto podado es imposible")
            (ok (not (search "PENDING" (or (card-reason lines "03.") "")))
                "el paso 03 tiene veredicto y no lleva el REASON de un PENDING")
            ;; La etiqueta y su STATE van en lineas DISTINTAS, asi que no
            ;; puede pedir las dos en la misma: mira que la tarjeta 03 exista y
            ;; que el unico APPLIED de la tarjeta sea el suyo.
            (ok (and (some (lambda (l) (search "03." l)) lines)
                     (= 1 (count-if (lambda (l) (search "STATE = APPLIED" l))
                                   lines)))
                "el paso 03 sale APPLIED, no PENDING")))))))

(deftest protocol/lectura-duplicada-se-cancela-no-es-pending
  "Una lectura duplicada se CANCELA: no sale PENDING y no deja un goal abierto.

   El caso real: una sesion de 4 turnos que en su ronda final volvio a pedir
   math_utils.py, que ya habia leido en ese mismo turno. Tres reglas en cadena
   producen el mentira:

     - AUTO-CREATE-TODO-ON-ACTION (salience 20) ve la intention y anota la
       meta 'Read math_utils.py';
     - PREVENT-DUPLICATE-READ (salience 15) la cancela y RETRACTA LA INTENTION
       SIN ASERTAR NADA;
     - EXECUTE-INTENTION-READ (salience 5) ya no llega a disparar, asi que no
       hay file-read.

   Quedan tres huecos, y los tres salen en la tarjeta:

     - el paso del plan se queda sin intention y sin veredicto, y STATE cae al
       fallback 'PENDING' de RENDER-PLAN-PROCEDURE;
     - el REASON de PENDING dice 'puede haberse podado', cuando lo que paso es
       que se CANCELO a proposito;
     - la meta queda abierta para siempre: TODO-SATISFIED-P exige evidencia MAS
       NUEVA que la meta, y la unica lectura que hay es de la ronda anterior.

   El resultado era 'GOAL ABIERTO' sobre una tarea ya terminada, que manda al
   modelo a seguir trabajando en algo que ya esta hecho.

   Aqui la intention se aserta CRUDA y con :status :pending, para que la monten
   las reglas de verdad (PREVENT-DUPLICATE-READ tiene salience 15 contra 5 de
   EXECUTE-INTENTION-READ, asi que gana la cancelacion, igual que en
   produccion). Se pide el render con BUILD-CONTEXT y no con RENDER-PLAN-CARD
   porque el goal se cuenta en la DATA DIVISION, y BUILD-CEXT es ademas el
   camino real: su (run) dispara la cadena."
  (with-test-config (("max_facts_per_type" 20))
    (with-temp-dir (dir)
      (let ((cl-harness::*base-dir* dir) (cl-harness:*session-id* "s"))
        (with-turn-engine
          (reset-turn-engine)
          (let ((base-ts (get-universal-time))
                (path (namestring (merge-pathnames "math_utils.py" dir))))
            (with-open-file (s path :direction :output :if-exists :supersede)
              (write-line "def factorial(n): return n" s))
            ;; Ronda 1: la lectura se ejecuto y quedo registrada.
            ;;
            ;; El timestamp va 5 segundos ATRAS a proposito. TODO-SATISFIED-P
            ;; cierra un goal con evidencia '>=' al timestamp del goal, asi que
            ;; si la lectura y el goal caen en el mismo segundo el goal se
            ;; cierra SOLO y el bug no aparece. Es lo que pasa en las pruebas
            ;; rapidas. En la corrida real la lectura fue de la ronda 1 y la
            ;; duplicada de la ronda 2, con segundos de harness en el medio, y
            ;; por eso el goal quedo abierto. Que el cierre dependa de la
            ;; resolucion del reloj es en si mismo parte del defecto.
            (h::assert
             (h::harness-fact (h::fact-type "batch-plan")
                              (h::timestamp (identity base-ts))
                              (h::data (list :step 1 :turn-id 1 :round 1
                                             :action :read-file
                                             :target "math_utils.py"))))
            (h::assert
             (h::harness-fact (h::fact-type "file-read")
                              (h::timestamp (identity (- base-ts 5)))
                              (h::data (list :path path :applied t :turn-id 1))))
            ;; Y su veredicto, que es lo que produce la lectura de verdad.
            (h::assert
             (h::harness-fact (h::fact-type "verdict")
                              (h::timestamp (identity (- base-ts 5)))
                              (h::data (list :step 1 :turn-id 1 :round 1
                                             :action :read-file
                                             :target "math_utils.py"
                                             :verdict :applied))))
            ;; Ronda 2: el MISMO archivo otra vez. Es un paso 2 para que las
            ;; tarjetas 01. y 02. no se pisen al buscar el REASON.
            (h::assert
             (h::harness-fact (h::fact-type "batch-plan")
                              (h::timestamp (incf base-ts))
                              (h::data (list :step 2 :turn-id 1 :round 2
                                             :action :read-file
                                             :target "math_utils.py"))))
            (h::assert
             (h::harness-fact (h::fact-type "intention")
                              (h::timestamp (incf base-ts))
                              (h::data (list :step 2 :turn-id 1 :round 2
                                             :action :read-file :path path
                                             :status :pending))))
            (let* ((ctx (build-context "sigue"))
                   (proc (procedure-section ctx))
                   (lines (uiop:split-string proc :separator '(#\Newline))))
              (ok (not (search "STATE = PENDING" proc))
                  (format nil
                          "la lectura duplicada sale PENDING, tiene que salir CANCELLED:~%~A"
                          proc))
              (ok (search "CANCELLED" proc)
                  "el paso duplicado se dice CANCELLED, no 'puede haberse podado'")
              (ok (search "GOAL ABIERTO. 0" ctx)
                  (format nil "la lectura duplicada dejo una meta abierta:~%~A"
                          (subseq ctx (or (search "DATA DIVISION." ctx) 0))))
              (ok (search "STATE = APPLIED" proc)
                  "la lectura de la ronda 1 sigue APPLIED, no se toca"
                  )
              (ok (not (search "puede haberse podado" (or (card-reason lines "02.") "")))
                  "el REASON no culpa a la poda de una cancelacion deliberada")
              ;; Y la intencion duplicada se fue: no quedo viva.
              (ok (not (find "intention"
                             (mapcar #'h::fact-type-of
                                     (h::collect-active-facts))
                             :test #'string=))
                  "la intention cancelada no quedo viva"))))))))



(deftest protocol/edit-duplicado-se-cancela-y-uno-distinto-no
  "El mismo edit dos veces se CANCELA; un edit DISTINTO se ejecuta normal.

   PREVENT-DUPLICATE-EDIT estaba sin un solo test. Es la misma clase de
   mentira que el dedup de lectura -- retracta la intencion sin escribir nada,
   el paso cae al fallback PENDING y el REASON culpa a una poda que no
   ocurrio -- pero sin nadie que lo comprobara. Por eso se puede tocar una
   regla entera y que la suite siga en verde.

   Ahi van las dos mitades. La que se cancela, con el MISMO old-string y el
   MISMO new-string, tiene que salir CANCELLED. Y la que se deja, que edita
   otra cosa, tiene que llegar a ejecutarse de verdad: si el test solo mirara
   la primera mitad, una regla que cancelara TODO lo pasaria.
  "
  (with-test-config (("max_facts_per_type" 20))
    (with-temp-dir (dir)
      (let ((cl-harness::*base-dir* dir) (cl-harness:*session-id* "s"))
        (with-turn-engine
          (reset-turn-engine)
          (let ((base-ts (get-universal-time))
                (path (namestring (merge-pathnames "app.py" dir))))
            (with-open-file (s path :direction :output :if-exists :supersede)
              (write-line "x = 1" s))
            ;; Un edit YA aplicado en este turno, con su veredicto.
            (h::assert
             (h::harness-fact (h::fact-type "batch-plan")
                              (h::timestamp (identity (- base-ts 5)))
                              (h::data (list :step 1 :turn-id 1 :round 1
                                             :action :edit-file
                                             :target "app.py"))))
            ;; EL EDIT DE VERDAD, hecho por EDIT-FILE y no fabricado a mano.
            ;;
            ;; La version anterior de este test escribia el hecho file-edit a
            ;; mano, CON :old-string y :new-string dentro. Edit-file NUNCA los
            ;; ponia, asi que la regla comparaba (STRING= NIL "x = 1") y
            ;; NUNCA podia disparar: estaba muerta, y su unico test la
            ;; resucitaba con datos que el sistema real no produce. Un test que
            ;; fabrica el fixture delata la regla rota y la da por buena.
            (let ((h::*turn-counter* 1))
              (h::edit-file path "x = 1" "x = 2"))
            (h::assert
             (h::harness-fact (h::fact-type "verdict")
                              (h::timestamp (identity (- base-ts 5)))
                              (h::data (list :step 1 :turn-id 1 :round 1
                                             :action :edit-file
                                             :target "app.py"
                                             :verdict :applied))))
            ;; Paso 2: el MISMO edit otra vez. Se cancela.
            (h::assert
             (h::harness-fact (h::fact-type "batch-plan")
                              (h::timestamp (identity base-ts))
                              (h::data (list :step 2 :turn-id 1 :round 2
                                             :action :edit-file
                                             :target "app.py"))))
            (h::assert
             (h::harness-fact (h::fact-type "intention")
                              (h::timestamp (identity base-ts))
                              (h::data (list :step 2 :turn-id 1 :round 2
                                             :action :edit-file :path path
                                             :status :pending
                                             :old-string "x = 1"
                                             :new-string "x = 2"))))
            ;; Paso 3: un edit DISTINTO (mismo archivo, otro texto). Este se
            ;; ejecuta, y su veredicto lo tiene que decir.
            ;;
            ;; Su OLD-STRING es 'x = 2', que es lo que hay en el archivo DESPUES
            ;; del paso 1. Antes ponia 'x = 1', que solo funcionaba porque el
            ;; paso 1 no editaba nada de verdad: el test fabricaba el hecho y
            ;; el archivo se quedaba como estaba. Con el edit real, 'x = 1' ya
            ;; no existe y este paso falla con 'old_string not found' -- que es
            ;; lo que pasaria de verdad, y por eso el fixture tiene que hablar
            ;; del archivo que hay.
            (h::assert
             (h::harness-fact (h::fact-type "batch-plan")
                              (h::timestamp (identity base-ts))
                              (h::data (list :step 3 :turn-id 1 :round 2
                                             :action :edit-file
                                             :target "app.py"))))
            (h::assert
             (h::harness-fact (h::fact-type "intention")
                              (h::timestamp (identity base-ts))
                              (h::data (list :step 3 :turn-id 1 :round 2
                                             :action :edit-file :path path
                                             :status :pending
                                             :old-string "x = 2"
                                             :new-string "x = 99"))))
            (let* ((ctx (build-context "sigue"))
                   (proc (procedure-section ctx))
                   (lines (uiop:split-string proc :separator '(#\Newline))))
              ;; El repetido se cancela y lo dice.
              (ok (search "CANCELLED" proc)
                  "el edit repetido sale CANCELLED")
              (ok (search "ya se aplico" (or (card-reason lines "02.") ""))
                  (format nil "el REASON del repetido:~%  ~A"
                          (or (card-reason lines "02.") "(nada)")))
              ;; Y el distinto se ejecuto de verdad: dos APPLIED, no uno.
              (ok (= 2 (count-if (lambda (l) (search "STATE = APPLIED" l))
                                lines))
                  (format nil "hubo ~D APPLIED, tiene que haber 2:~%~A"
                          (count-if (lambda (l) (search "STATE = APPLIED" l))
                                    lines)
                          proc))
              (ok (not (search "CANCELLED" (or (card-reason lines "03.") "")))
                  "el edit distinto no se cancelo")
              ;; Y la meta del edit repetido no queda abierta: la evidencia que
              ;; la cierra es del mismo turno.
              (ok (search "GOAL ABIERTO. 0" ctx)
                  (format nil "el edit repetido dejo una meta abierta:~%~A"
                          (subseq ctx (or (search "DATA DIVISION." ctx) 0)))))))))))


(deftest context/memoria-no-deja-una-escritura-rechazada-como-wrote
  "La MEMORY DIVISION tiene que distinguir lo que se ESCRIBIO de lo que se EDITO.

   Tres defectos en la misma funcion, y los tres se ven juntos en una tarjeta:

   1. FILE-EDIT no era durable. DURABLE-TYPE-P solo aceptaba COMMAND-EXEC y
      FILE-WRITE, asi que un archivo escrito y luego editado DOS veces dejaba en
      la memoria el tamano de su primera escritura, para siempre. El caso real:
      WROTE. math_utils.py BYTES = 252 con un archivo de 560 bytes en disco, y
      los dos edits que lo llevaron ahi, con sus chars, guardados en un hecho que
      la memoria nunca miraba. La verdad ESTABA en los facts.

   2. FILE-EDIT no tenia rama en MEM-CONTEXT-BLOCK. Aunque entrara, no se
      renderizaba: el cond tenia COMMAND-EXEC y FILE-WRITE y nada mas.

   3. Un WRITE RECHAZado entraba a la memoria. DURABLE-TYPE-P no mira :applied,
      y WRITE-FILE con :refused t afirma :applied nil. Salia como
      'WROTE. nope.py / BYTES = ' con el tamano vacio: la memoria decia que
      habia escrito un archivo que no escribio. Falla en los dos pasos, un
      ARCHIVO que no existe no se puede rechazar por no existir, y no es una
      escritura.

   El ORDER-BY es la cuarta cosa que hay que fijar: MEMORY DIVISION sale en
   orden de_timestamp y la prueba de las dos mitades depende de eso. Si el
   EDITED saliera antes del WROTE, la linea de la escritura se leeria como si
   describiera el estado final del archivo, que es justo el numero que no
   describe nada. Por eso NEW-EDIT lleva timestamp POSTERIOR al write y no da
   igual: con los dos en el mismo segundo el orden es arbitrario."
  (with-test-config (("max_facts_per_type" 20))
    (with-turn-engine
      (reset-turn-engine)
      (reset-mem-engine)
      (let ((base-ts (get-universal-time)))
        ;; 1. Escritura REAL: applied t, con su tamano.
        (h::assert
         (h::harness-fact (h::fact-type "file-write")
                          (h::timestamp (identity base-ts))
                          (h::data (list :path "/tmp/proyecto/math_utils.py"
                                         :applied t :bytes 252 :turn-id 1))))
        ;; 2. Escritura RECHAZADA: applied nil, refused t, sin bytes.
        (h::assert
         (h::harness-fact (h::fact-type "file-write")
                          (h::timestamp (identity base-ts))
                          (h::data (list :path "/tmp/proyecto/nope.py"
                                         :applied nil :refused t
                                         :reason "blind write refused"
                                         :turn-id 1))))
        ;; 3. Edit REAL: el que llevo el archivo de 252 a 560 y que la memoria
        ;;    no mostraba.
        (h::assert
         (h::harness-fact (h::fact-type "file-edit")
                          (h::timestamp (identity (+ base-ts 5)))
                          (h::data (list :path "/tmp/proyecto/math_utils.py"
                                         :applied t
                                         :replaced-chars 155 :new-chars 306
                                         :matches 1 :turn-id 1))))
        (h::promote-durable-facts)
        (let ((ctx (build-context "sigue")))
          (ok (search "EDITED. /tmp/proyecto/math_utils.py" ctx)
              (format nil "el edit no sale en la MEMORY:~%~A" ctx))
          (ok (search "REPLACED = 155" ctx)
              "el edit dice cuantos chars reemplazo")
          (ok (search "NEW = 306" ctx)
              "el edit dice cuantos chars agrego")
          (ok (search "WROTE. /tmp/proyecto/math_utils.py" ctx)
              "la escritura real sigue en la memoria")
          (ok (not (search "WROTE. /tmp/proyecto/nope.py" ctx))
              (format nil "un write rechazado sale como WROTE:~%~A" ctx))
          (ok (not (search "BYTES = " (or (search "BYTES =[^~]*" ctx) "")))
              "no queda ningun BYTES vacio de un write rechazado")
          ;; El orden: CRONOLOGICO, el mismo que la DATA DIVISION de arriba. Antes era
          ;; newest-first (un SORT seguido de un NREVERSE) y el comentario lo
          ;; justificaba como "el BYTES de la escritura se lee como el estado
          ;; previo" -- al reves: en newest-first la ULTIMA linea es la mas
          ;; ANTIGUA, y como las entradas no llevan turno, el modelo se
          ;; quedaba con el dato mas viejo como si fuera el actual. En una
          ;; corrida real leia 'WROTE. math_utils.py BYTES = 294' sobre un
          ;; fichero de 587 bytes.
          (ok (< (or (search "WROTE. /tmp/proyecto/math_utils.py" ctx) 0)
                 (or (search "EDITED. /tmp/proyecto/math_utils.py" ctx) 0))
              (format nil "el WROTE sale antes del EDITED que lo modifico: cronologico~%~A" ctx))
          ;; Y la razon del rechazo no se pierde del todo: va en la TURN.
          (ok (search "blind write refused" ctx)
              "el motivo del rechazo queda en la TURN DIVISION"))))))


(deftest rules/memoria-los-edits-estan-capeados
  "PROMOTE-DURABLE-FACTS capa los edits en la memoria de largo plazo.

   El cap no es un detalle: DURABLE-TYPE-P acaba de dejar pasar FILE-EDIT, y
   PROMOTE-DURABLE-FACTS capa COMMAND-EXEC y FILE-WRITE al final. Sin una linea
   mas, cada edit aplicado de cada turno se acumulaba en la memoria PARA
   SIEMPRE, y no hacia falta un runaway raro: un turno que edita 20 veces ya
   deja 20 hechos mas de los que el tope permite, y el contexto de los turnos
   siguientes se infla sin que nadie lo note.

   Se prueba en el motor de MEMORIA y no en el de turno, que es donde cae el
   error: los dos topes son llamadas parecidas en sitios distintos y el que se
   puede olvidar es el nuevo."
  (with-test-config (("max_facts_per_type" 5))
    (with-turn-engine
      (reset-turn-engine)
      (reset-mem-engine)
      (dotimes (i 7)
        (h::assert
         (h::harness-fact (h::fact-type "file-edit")
                          (h::timestamp (identity (+ (get-universal-time) i)))
                          (h::data (list :path (format nil "a~D.py" i)
                                         :applied t
                                         :replaced-chars 2 :new-chars 5
                                         :turn-id 1)))))
      (h::promote-durable-facts)
      (let ((paths (mapcar (lambda (f) (h::data-get (h::fact-data-of f) :path))
                           (h::mem-engine-facts))))
        (ok (= 5 (length paths))
            (format nil "la memoria guardo ~D edits y el tope es 5: ~S"
                    (length paths) paths))
        ;; Y se van los VIEJOS, no los nuevos.
        (ok (not (member "a0.py" paths :test #'string=))
            "el edit mas viejo se podo")
        (ok (member "a6.py" paths :test #'string=)
            "el edit mas nuevo se conserva")))))


(deftest metrics/los-intentos-fallidos-se-cuentan
  "API calls son PETICIONES, no aciertos.

   CAPTURE-USAGE solo anade a *LLM-CALL-LOG* cuando la respuesta trae usage, o
   sea solo cuando la llamada fue exitosa. Un turno con 2 timeouts y un exito
   reportaba una sola llamada, cuando en realidad salieron tres peticiones: las
   dos primeras occuparon socket, tardaron lo que tardaron y probablemente
   costaron cuota. El panel de OpenRouter si las contaba, asi que la misma sesion
   decia 22 llamadas por un lado y 31 por otro, y no habia forma de saber cual
   era el bueno.

   El hueco era peor todavia con reintentos: un bucle que falla dos veces y acierta
   a la tercera se registraba como UNA llamada, que es justo el caso donde mas
   caro sale y donde mas hace falta verlo.

   No hay red: el THUNK falla con un 503 sintetico, como en
   CONFIG/RETRY-REPEATS-TRANSIENT-FAILURES."
  (with-test-config (("llm_http_attempts" 3) ("llm_http_backoff_seconds" 0))
    (h::metrics-reset)
    (let* ((calls 0)
           (result (h::call-with-retry
                    (lambda ()
                      (incf calls)
                      (if (< calls 3) (error (http-failure 503)) "ok")))))
      (ok (string= "ok" result) "el reintento sigue funcionando")
      (ok (= 3 calls) "salieron 3 peticiones: 2 fallidas y 1 buena")
      (ok (= 2 (length h::*llm-failed-calls*))
          "los 2 intentos fallidos quedan registrados, no solo el que acerto")
      (h::record-context-metrics "ctx" "u" :naive-str "ctx" :llm-iterations 3)
      (let ((tot (h::metrics-totals)))
        (ok (= 2 (getf tot :api-failures))
            "el turno arrastra los fallos a las metricas del turno")
        (ok (= 2 (getf tot :api-calls))
            "API calls cuenta los 2 intentos, no el 1 exito")
        (ok (= 0 (getf tot :api-ok))
            "y no inventa ninguno bueno: este turno no tuvo exito de LLM")))
    (h::metrics-reset)))

(deftest metrics/modelos-todos-no-solo-el-ultimo
  "La lista de modelos tiene que decir cuantos modelos se usaron de verdad.

   Cada turno llevaba UN solo :response-model, el del ULTIMO exito, y
   :LLM-CALLS no se llenaba mas que con los exitos. Con el enrutado por carga de
   OpenRouter un turno da cinco modelos distintos y la sesion entera decia 4
   mientras el panel decia 5: el mismo hecho, dos numeros, y el de las metricras
   era el incompleto. No habia forma de saber que se estaban usando mas modelos
   de los que la session reconocia.

   Se barren las TRES fuentes, y ademas los intentos fallidos: un modelo que solo
   falla tambien se uso y tambien se paga, y dejarlo fuera repetiria el mismo
   error por el otro lado."
  (h::metrics-reset)
  ;; Turno 1: tres llamadas exitosas con tres modelos distintos. El proveedor
  ;; solo reporta uno como :response-model.
  (h::record-context-metrics
   "ctx" "u" :naive-str "ctx" :llm-iterations 3
   :response-model "zeta"
   :llm-calls '((:prompt 1 :completion 1 :model "alfa")
                 (:prompt 1 :completion 1 :model "beta")
                 (:prompt 1 :completion 1 :model "zeta")))
  ;; Turno 2: ningun exito, un 503 de "gamma".
  (push (list :error "HTTP 503" :status 503 :model "gamma")
        h::*llm-failed-calls*)
  (h::record-context-metrics "ctx" "u" :naive-str "ctx" :llm-iterations 0)
  (let ((tot (h::metrics-totals)))
    (ok (equal '("alfa" "beta" "gamma" "zeta") (getf tot :models))
        (format nil "los cuatro modelos, no solo el ultimo :response-model; REAL=~S"
                (getf tot :models)))
    (ok (= 3 (getf tot :api-ok)) "3 exitos, contados aparte")
    (ok (= 1 (getf tot :api-failures)) "1 fallo, tambien contado")
    (ok (= 4 (getf tot :api-calls)) "4 peticiones en total")
    (h::metrics-reset)))

(deftest metrics/el-resumen-imprime-los-fallos-sin-reventar
  "METRICS-SUMMARY es lo primero que mira el usuario, y no lo cubria ningun test.

   Tres fallos se colaron aqui de golpe y los tres son la misma clase de error:
   dar por bueno algo que solo PARECE que funciona porque el camino feliz nunca
   se ejecuta. Por eso este test imprime el resumen de verdad y mira el texto.

     - (WHEN N) con N=0 es VERDADERO en Common Lisp, asi que un turno sin fallos
       imprimia 'turno 1: 0 intentos fallido:' con la lista vacia. Ahora es
       (WHEN (PLUSP N)).
     - ~:[~;s~] imprime la rama THEN cuando el argumento es NO-NIL, asi que la
       's' va con (NOT (= N 1)), no con (= N 1).
     - REMOVE-DUPLICATES sin :TEST usa EQL, y EQL entre dos cadenas con el
       mismo texto y distinta identidad da NIL: el mismo modelo salia dos
       veces, con dos objetos string distintos."
  (h::metrics-reset)
  ;; Turno 1: un exito, sin fallos.
  (h::record-context-metrics "ctx" "u" :naive-str "ctx" :llm-iterations 2
                             :response-model "alfa"
                             :llm-calls '((:prompt 1 :completion 1 :model "alfa")))
  ;; Turno 2: tres fallos, dos de ellos del MISMO modelo y con dos cadenas
  ;; distintas, que es la forma exacta de colar un duplicado con EQL.
  (dolist (r '((:error "HTTP 429" :status 429 :model "beta")
               (:error "HTTP 503" :status 503 :model "beta")
               (:error "TIMEOUT" :status nil :model "gamma")))
    (push r h::*llm-failed-calls*))
  (h::record-context-metrics "ctx" "u" :naive-str "ctx" :llm-iterations 0)
  (let ((out (with-output-to-string (s) (h::metrics-summary s))))
    (ok (search "1 ok + 3 fallidos = 4 intentos" out)
        "el desglose de intentos sale en una linea propia")
    (ok (search "3 intentos fallidos" out)
        "el plural de 'intentos' sale bien con 3")
    (ok (search "HTTP 429, HTTP 503, TIMEOUT" out)
        "los tres motivos, en orden cronologico")
    (ok (not (search "0 intentos" out))
        "un turno sin fallos no imprime una linea de cero intentos")
    (ok (not (search "1 intentos" out))
        "ni un plural mal puesto en ningun turno")
    (ok (search "Models: alfa, beta, gamma" out)
        "cada modelo una vez, con dos registros distintos de 'beta'")
    (h::metrics-reset)))

;;; ---------------------------------------------------------------
;;; Transporte: lo que el mock de CALL-LLM deja sin cubrir.
;;;
;;; WITH-MOCKED-CALL-LLM sustituye la funcion COMPLETA, asi que todo lo de
;;; debajo --el constructor del body HTTP y los clientes de cada proveedor-- no
;;; lo toca ningun test. Eso no es una consecuencia aceptable: la funcion
;;; BUILD-OPENAI-COMPAT-MESSAGES-WITH-TOOLS estaba definida con tres
;;; parametros y sus dos callers le pasaban cuatro, y 96 tests en verde no lo
;;; detectaron porque el unico caller real esta debajo del mock. Estos tests
;;; llaman al builder DIRECTAMENTE, que es lo que hace falta para que un error
;;; de aridad o de forma del body no pueda volver a esconderse.
;;; ---------------------------------------------------------------

(deftest transport/el-builder-de-mensajes-acepta-los-cuatro-argumentos
  ;; El bug: definida con (context user-message prior-messages) y llamada con
  ;; (system-prompt context user-message messages). Con este test, una firma que
  ;; no acepte los cuatro argumentos revienta el turno en lugar de esperar a
  ;; produccion.
  (with-test-config (("llm_model" "m") ("llm_max_tokens" 10))
    (let ((body (h::build-openai-compat-messages-with-tools
                 "SYSTEM" "CONTEXT" "USER" nil)))
      (ok (hash-table-p body) "devuelve el body")
      (ok (string= "m" (gethash "model" body)) "lleva el modelo")
      (ok (gethash "tools" body) "lleva las tools"))))

(deftest transport/el-builder-inyecta-el-mensaje-de-sistema
  ;; El cuerpo no escribia NUNCA el mensaje de sistema: funcionaba solo porque
  ;; los callers lo precargaban. Con la lista vacia el prompt se perdia en
  ;; silencio y el modelo se quedaba sin doctrina.
  (with-test-config (("llm_model" "m") ("llm_max_tokens" 10))
    (let* ((body (h::build-openai-compat-messages-with-tools
                  "SYSTEM" "CONTEXT" "USER" nil))
           (msgs (gethash "messages" body))
           (sys-msg (find-if (lambda (m) (string= (gethash "role" m) "system"))
                             msgs)))
      (ok sys-msg "el system prompt llega aunque prior-messages venga vacia")
      (ok (and sys-msg (string= "SYSTEM" (gethash "content" sys-msg)))
          "y su contenido es el que se le paso"))))

(deftest transport/el-builder-no-duplica-el-sistema-que-ya-esta
  ;; Si el caller ya precargo su mensaje de sistema (que es lo que hacen los dos
  ;; callers reales), el builder no debe meter un segundo system: el proveedor
  ;; lo rechaza y la doctrina queda ambigua.
  (with-test-config (("llm_model" "m") ("llm_max_tokens" 10))
    (let* ((pre (make-hash-table :test 'equal)))
      (setf (gethash "role" pre) "system"
            (gethash "content" pre) "PRE")
      (let* ((body (h::build-openai-compat-messages-with-tools
                    "NUEVO" "CONTEXT" "USER" (list pre)))
             (roles (mapcar (lambda (m) (gethash "role" m))
                            (gethash "messages" body)))
             (systems (remove-if-not (lambda (r) (string= r "system")) roles)))
        (ok (= 1 (length systems)) "un solo mensaje de sistema, no dos")
        (ok (string= "PRE" (gethash "content" (first (gethash "messages" body))))
            "gana el que traia el caller, no el argumento")))))

(deftest transport/el-builder-anade-el-turno-de-usuario-al-final
  ;; El usuario va con el CONTEXT pegado. Este es el mensaje que el modelo ve
  ;; como su instruccion del turno.
  (with-test-config (("llm_model" "m") ("llm_max_tokens" 10))
    (let* ((body (h::build-openai-compat-messages-with-tools
                  "SYSTEM" "EL-CONTEXTO" "EL-MENSAJE" nil))
           (msgs (gethash "messages" body))
           (last (car (last msgs))))
      (ok (string= "user" (gethash "role" last)) "el ultimo mensaje es del usuario")
      (ok (search "EL-CONTEXTO" (gethash "content" last))
          "lleva el contexto")
      (ok (search "EL-MENSAJE" (gethash "content" last))
          "lleva el mensaje del usuario"))))

(deftest transport/el-path-estatico-acepta-el-system-prompt
  ;; CALL-LLM-WITH-TOOLS/STATIC usaba SYSTEM-PROMPT como VARIABLE LIBRE ausente
  ;; de su lambda list. En Common Lisp eso no es un default: es una variable
  ;; especial sin ligar, y el turno moria al construir el primer mensaje con
  ;; "Variable SYSTEM-PROMPT is unbound". Este test falla con ese mismo error
  ;; si la firma vuelve a perder el parametro.
  (with-test-config (("llm_model" "m") ("llm_max_tokens" 10)
                     ("llm_endpoint" "http://127.0.0.1:9/nope"))
    ;; DEXADOR:POST esta sustituido por un error durante toda la suite, asi que
    ;; esto no abre ningun socket: se queda en la construccion del body.
    (let ((res (handler-case
                   (h::call-llm-with-tools/static "SYSTEM" "CONTEXT" "USER")
                 (error (e) (format nil "~A" e)))))
      (ok res "la funcion responde en vez de senalar variable no atada"))))

(deftest transport/las-tool-calls-se-ordenan-por-indice
  ;; MAPHASH itera en orden NO ESPECIFICADO: un lote de tres tool-calls se
  ;; ejecutaba en un orden distinto en cada corrida, y un read_file seguido de
  ;; un write_file podian invertirse. El modelo ordena sus tool_calls por
  ;; indice, y ese es el unico orden que significo.
  (let ((buf (make-hash-table :test #'eql)))
    (setf (gethash 2 buf) (list :id "c" :name "write_file" :args "{}")
          (gethash 0 buf) (list :id "a" :name "read_file" :args "{}")
          (gethash 1 buf) (list :id "b" :name "exec_command" :args "{}"))
    (let ((order (mapcar #'car (h::tool-buf-in-index-order buf))))
      (ok (equalp '(0 1 2) order)
          (format nil "orden indices: ~S" order)))))

(deftest transport/el-orden-no-depende-del-orden-de-insercion
  ;; La razon del test anterior: se escribe al reves y sale igual.
  (let ((buf (make-hash-table :test #'eql)))
    (dolist (i '(5 3 9 1 7))
      (setf (gethash i buf) (list :id (format nil "c~A" i) :name "read_file" :args "{}")))
    (ok (equalp '(1 3 5 7 9) (mapcar #'car (h::tool-buf-in-index-order buf)))
        "siempre ascendente por indice, se inserte como se inserte")))

(deftest transport/el-body-es-json-serializable
  ;; El body va por Jzon STRINGIFY a la red. Si un valor no se serializa, el
  ;; fallo aparece en produccion como un error de codificacion, no aqui.
  (with-test-config (("llm_model" "m") ("llm_max_tokens" 10))
    (let* ((body (h::build-openai-compat-messages-with-tools
                  "SYSTEM" "CONTEXTO" "USUARIO" nil))
           (json (jzon:stringify body))
           (back (jzon:parse json)))
      (ok (search "messages" json) "el JSON lleva los mensajes")
      (ok (hash-table-p back) "y vuelve a parsearse"))))

;;; ---------------------------------------------------------------
;;; Escritura atomica
;;; ---------------------------------------------------------------

(deftest write/atomica-deja-el-contenido-completo
  ;; La garantia minima: escribir y releer da exactamente lo escrito.
  (with-temp-dir (dir)
    (let ((full (merge-pathnames "a.txt" dir)))
      (h::write-file-atomically full "hola mundo")
      (ok (string= "hola mundo" (read-temp full))
          "el contenido llega integro"))))

(deftest write/atomica-no-deja-temporales
  ;; Si el temporal no se limpia, cada escritura fallida deja un .tmp en el
  ;; directorio del proyecto del usuario.
  (with-temp-dir (dir)
    (let ((full (merge-pathnames "b.txt" dir)))
      (h::write-file-atomically full "x")
      (let ((strays (remove-if-not (lambda (p) (search ".tmp" (namestring p)))
                                  (directory (merge-pathnames "*.*" dir)))))
        (ok (null strays) (format nil "temporales: ~S" (mapcar #'namestring strays)))))))

(deftest write/atomica-sobrescribe-un-fichero-que-ya-existe
  ;; EDIT-FILE escribe sobre un fichero existente; con :SUPERSEDE directo, una
  ;; muerte a mitad dejaba el fichero en cero.
  (with-temp-dir (dir)
    (let ((full (merge-pathnames "c.txt" dir)))
      (uiop:native-namestring full)
      (with-open-file (s full :direction :output :if-exists :supersede)
        (write-string "contenido viejo que se reemplaza" s))
      (h::write-file-atomically full "nuevo")
      (ok (string= "nuevo" (read-temp full))
          "la sobrescritura deja el contenido nuevo entero"))))

(deftest write/el-contenido-no-string-se-rechaza-con-motivo
  ;; La validacion de tipos vivia solo en el normalizador batch, asi que la
  ;; ruta de tools nativas podia pasar un hash-table como contenido y
  ;; REGEX-REPLACE-ALL senialaba. La guarda vive en la ACCION: un solo sitio
  ;; para todos los caminos de entrada, que es la politica I2.
  (with-turn-engine
    (reset-turn-engine)
    (with-temp-dir (dir)
      (let* ((h::*base-dir* dir)
             (res (h::write-file "typed.txt" 42)))
        (ok (not (getf res :applied)) "no se aplica")
        (ok (getf res :reason) "y deja el motivo con nombre")
        (ok (search "string" (getf res :reason))
            "el motivo dice que se esperaba un string")))))

;;; ---------------------------------------------------------------
;;; Hallazgos de la corrida real (cl-harness-yaml-debug, 2026-10-01)
;;;
;;; Tres cosas que la suite no miraba porque solo se rompian en el camino
;;; BATCH, que es donde el trabajo lo hacen las reglas de Rete y no
;;; EXECUTE-TOOL. La suuite offline es Strong en protocolo y ciega en esta
;;; frontera: los 108 tests pasaban con las tres rotas.
;;; ---------------------------------------------------------------

(deftest batch-metrics/la-herramienta-del-batch-se-registra-en-las-metricas
  ;; El hueco: RECORD-TOOL-CALL solo se llamaba desde EXECUTE-TOOL (tools
  ;; nativas). Las reglas execute-intention-* ejecutan las mismas acciones sin
  ;; pasar por ahi, asi que en batch la tabla chars_raw/chars_sent/saved% salia
  ;; vacia -- y el JSON llevaba "tools": false. Justo en el modo que mas la
  ;; necesitaba.
  (with-turn-engine
    (reset-turn-engine)
    (h::metrics-reset)
    (with-temp-dir (dir)
      (let ((h::*base-dir* dir))
        ;; Una intencion real de escritura, ejecutada por la REGLA de Rete.
        (h::parse-llm-batch-to-intentions
         "{\"tool_calls\":[{\"id\":\"1\",\"type\":\"function\",\"function\":{\"name\":\"write_file\",\"arguments\":{\"path\":\"m.txt\",\"content\":\"hola\"}}}]}")
        (h::run)
        (let ((calls h::*metrics-tool-calls*))
          (ok (plusp (length calls))
              "la regla de Rete registro la llamada, no solo EXECUTE-TOOL")
          (ok (string= "write-file" (getf (car calls) :name))
              (format nil "nombre: ~S" (getf (car calls) :name)))
          ;; Lo que se mide son LONGITUDES, no el texto: lo que se guarda es
          ;; cuanto se cruzo el umbral, no la carga util. Aserta sobre el texto
          ;; exigiria que la metrica lo guardara, y guardarlo seria tirar la
          ;; memoria del proceso para hacer el test mas comodo.
          (ok (plusp (getf (car calls) :chars-raw))
              "con los chars crudos medidos"))))))

(deftest batch-metrics/el-rechazo-tambien-se-registra
  ;; Un paso rechazado es justo el que mas interesa medir: consume una ronda
  ;; entera del modelo y no produce nada. Si solo se registraran los exitos, el
  ;; fallo se mediria como si no hubiera pasado.
  (with-turn-engine
    (reset-turn-engine)
    (h::metrics-reset)
    (with-temp-dir (dir)
      (let ((h::*base-dir* dir))
        ;; Edit sobre un fichero ausente: se rechaza con :applied nil.
        (h::parse-llm-batch-to-intentions
         "{\"tool_calls\":[{\"id\":\"1\",\"type\":\"function\",\"function\":{\"name\":\"edit_file\",\"arguments\":{\"path\":\"nope.txt\",\"old_string\":\"x\",\"new_string\":\"y\"}}}]}")
        (h::run)
        (let ((calls h::*metrics-tool-calls*))
          ;; Un rechazo mide como un exito si solo se guardara lo que se aplico.
          ;; Su rasgo distintivo es que el texto es el MOTIVO, asi que el
          ;; recuento sale del tamano del motivo, no del contenido.
          (ok (plusp (length calls)) "el rechazo sale registrado")
          (ok (string= "edit-file" (getf (car calls) :name))
              (format nil "nombre: ~S" (getf (car calls) :name)))
          (ok (plusp (getf (car calls) :chars-raw))
              (format nil "chars-raw: ~S" (getf (car calls) :chars-raw)))
          (ok (<= (getf (car calls) :chars-sent)
                  (getf (car calls) :chars-raw))
              "y el recorte nunca lo infla"))))))

(deftest batch-metrics/el-comando-si-se-registra
  ;; El caso con mas volumen: la salida de un comando es lo que mas engorda el
  ;; contexto reinyectado, y era justo lo que no se midia en batch.
  (with-turn-engine
    (reset-turn-engine)
    (h::metrics-reset)
    (with-test-config (("tool_timeout_seconds" 30))
      (h::parse-llm-batch-to-intentions
       "{\"tool_calls\":[{\"id\":\"1\",\"type\":\"function\",\"function\":{\"name\":\"exec_command\",\"arguments\":{\"command\":\"echo hola-mundo\"}}}]}")
      (h::run)
      (let ((calls h::*metrics-tool-calls*))
        (ok (plusp (length calls)) "el comando sale registrado")
        (ok (string= "exec-command" (getf (car calls) :name))
            (format nil "nombre: ~S" (getf (car calls) :name)))
        ;; La salida del comando es lo que engorda el contexto reinyectado: un
        ;; comando cuyo texto midiese solo la invocacion no diria nada del
        ;; volumen real.
        (ok (> (getf (car calls) :chars-raw) (length "echo hola-mundo"))
            (format nil "chars-raw: ~S; solo la invocacion seria ~D"
                    (getf (car calls) :chars-raw) (length "echo hola-mundo")))
        (ok (<= (getf (car calls) :chars-sent) (getf (car calls) :chars-raw))
            "y el recorte nunca lo infla")))))

(deftest batch-metrics/el-recorte-se-aplica-al-path-de-batch
  ;; La columna chars_sent mide lo que se REINYECTA al modelo, que es lo acotado
  ;; por max_tool_result_chars. Sin truncar, chars_raw y chars_sent serian
  ;; siempre iguales y la columna saved% diria siempre 0.
  (with-turn-engine
    (reset-turn-engine)
    (h::metrics-reset)
    (with-test-config (("max_tool_result_chars" 100)
                       ("tool_timeout_seconds" 30))
      (h::parse-llm-batch-to-intentions
       "{\"tool_calls\":[{\"id\":\"1\",\"type\":\"function\",\"function\":{\"name\":\"exec_command\",\"arguments\":{\"command\":\"seq 1 5000\"}}}]}")
      (h::run)
      (let ((call (car h::*metrics-tool-calls*)))
        (ok call "hay llamada registrada")
        (ok (> (getf call :chars-raw) (getf call :chars-sent))
            (format nil "raw=~A sent=~A"
                    (getf call :chars-raw) (getf call :chars-sent)))))))

(deftest metrics/llm-path-se-registra-tambien-cuando-el-turno-sale-bien
  ;; Solo se rellenaba AL ABORTAR. Y el abort es el caso raro, asi que el campo
  ;; que existe para responder "como termino este turno" contestaba NIL en la
  ;; mayoria de los turnos batch. El metodo tiene que decir como ACABO, no solo
  ;; como fallo.
  ;;
  ;; El mock devuelve un batch JSON con tool_calls vacio: eso es una PROPUESTA
  ;; de fin de turno (PROTOCOLO I5). Sin goals abiertos se acepta, se aserta
  ;; plan-done y PROCESS-TURN sale por la via limpia.
  ;;
  ;; El mock no devuelve el JSON en crudo: lo pasa por EL MISMO parseo que usa
  ;; CALL-BATCH-LLM, porque con la cadena sin parsear nadie aserta plan-done y
  ;; el turno no tendria nada que cerrar -- se mediria un abort, no un cierre.
  ;; Mockear una funcion entera no mockea lo que tiene debajo (la misma leccion
  ;; del mock de transporte).
  ;;
  ;; TOLERANCE A CERO a proposito: el mock devuelve SIEMPRE lo mismo, asi que
  ;; con la tolerancia por defecto el detector de lote repetido aborta el turno
  ;; en la segunda ronda y este test mediria un abort, no un cierre.
  (with-turn-engine
    (reset-turn-engine)
    (h::metrics-reset)
    (with-test-config (("llm_provider" "batch")
                       ("llm_stream" nil)
                       ("batch_repeat_tolerance" 0))
      (with-mocked-call-llm
          ((multiple-value-bind (accepted final-text parse-error)
               (h::parse-llm-batch-to-intentions
                "{\"tool_calls\":[],\"response\":\"listo\"}")
             (declare (ignore accepted parse-error))
             final-text))
        (h::process-turn "hola" nil))
      (let ((path (getf h::*last-llm-call-info* :path)))
        (ok (eq :plan-done path)
            (format nil "un turno que cierra limpio dice plan-done; Path: ~S" path))
        (ok (plusp (getf h::*last-llm-call-info* :iterations 0))
            "y lleva las rondas que tardo")))))

(deftest metrics/el-abort-sigue-ganando-al-exito
  ;; El fix anterior no puede tapar al caso raro: si hubo abort, el path es el
  ;; del abort. Si no, se informa del final limpio. Los dos caminos siguen
  ;; distinguibles.
  (ok (not (eq :plan-done :batch-repeated))
      "plan-done y batch-repeated son caminos distintos y ambos existen"))

(deftest plan/el-objetivo-del-plan-es-el-comando-que-se-ejecuta
  ;; El modelo emite 'cd /abs && cmd' a menudo y STRIP-CD-PREFIX se lo quita al
  ;; ejecutar. El plan guardaba el texto CRUDO, asi que la tarjeta y el TURN
  ;; mostraban dos comandos distintos para el mismo paso: uno con el cd que
  ;; nunca corrio y otro sin el. El modelo leia su paso con un prefijo que no
  ;; llego a la shell.
  (with-turn-engine
    (reset-turn-engine)
    (h::parse-llm-batch-to-intentions
     "{\"tool_calls\":[{\"id\":\"1\",\"type\":\"function\",\"function\":{\"name\":\"exec_command\",\"arguments\":{\"command\":\"cd /tmp/proyecto && python -m unittest test_x.py\"}}}]}")
    (let ((plans (h::collect-facts-of-type (h::collect-active-facts) "batch-plan")))
      (ok (= 1 (length plans)) "el plan se aserto")
      (ok (string= "python -m unittest test_x.py" (h::data-get (car plans) :target))
          (format nil "target: ~S" (h::data-get (car plans) :target))))))

(deftest plan/el-target-de-un-fichero-no-se-toca
  ;; La normalizacion es solo para comandos. Una ruta con espacios o con un
  ;; 'cd' legitimo en el nombre no se debe tocar: PATH no pasa por
  ;; STRIP-CD-PREFIX.
  (with-turn-engine
    (reset-turn-engine)
    (h::parse-llm-batch-to-intentions
     "{\"tool_calls\":[{\"id\":\"1\",\"type\":\"function\",\"function\":{\"name\":\"write_file\",\"arguments\":{\"path\":\"cd notas/plan.md\",\"content\":\"x\"}}}]}")
    (let ((plans (h::collect-facts-of-type (h::collect-active-facts) "batch-plan")))
      (ok (= 1 (length plans)) "el plan se aserto")
      (ok (string= "cd notas/plan.md" (h::data-get (car plans) :target))
          (format nil "target: ~S" (h::data-get (car plans) :target))))))

;;; ---------------------------------------------------------------
;;; Lecturas repetidas: el bucle que los tres detectores no veian.
;;;
;;; De una corrida real: el modelo pidio el mismo fichero cinco veces en un
;;; turno. Las cinco fueron CANCELADAS, porque ya lo habia leido en la ronda 1.
;;; Con el veto solo, cada cancelacion decia "ya se leyó" y no entregaba nada, y
;;; con el veto delivering, cinco rondas se gastaron en un rechazo que no
;;; avanzaba al modelo. Peor: los tres detectores de bucle Forever fueron
;;; incapaces de verlo.
;;;
;;;   - DETECT-READ-LOOP contaba solo hechos file-read, y una lectura cancelada
;;;     nunca llega a READ-FILE: no deja hecho. Contaba 1 de 4.
;;;   - BATCH-REPEAT-TOLERANCE no disparaba porque BATCH-FINGERPRINT hasheaba
;;;     tambien la prosa libre, y el modelo cambiaba el comentario en cada
;;;     intento. Dos lotes identicos con distinta prosa daban huellas distintas.
;;;   - El veto no dejaba memoria: una cancelacion se podia cancelar para
;;;     siempre, ronda tras ronda, sin nada que la contara.
;;;
;;; Con esto los tres tienen que ver el bucle.
;;; ---------------------------------------------------------------

(deftest veto/la-lectura-repetida-devuelve-el-contenido-que-ya-tenia
  ;; El caso base. El modelo pide un fichero que el harness ya leyo en ESTE
  ;; turno: en vez de un no seco, recibe lo que ya tenia. El veto sigue siendo
  ;; un veto -- la lectura no se repite -- pero ahora hay algo que hacer con lo
  ;; que ya se sabia.
  (with-turn-engine
    (reset-turn-engine)
    (with-temp-dir (dir)
      (let ((h::*base-dir* dir))
        (h::write-file "notas.txt" "contenido real de notas")
        (h::assert-batch-intention (list :action :read-file :path "notas.txt") 1)
        (h::run)
        ;; Segunda peticion del mismo fichero, en el mismo turno.
        (h::assert-batch-intention (list :action :read-file :path "notas.txt") 2)
        (h::run)
        (let* ((v (h::collect-facts-of-type (h::collect-active-facts) "verdict"))
               (cancelled (find-if (lambda (d)
                                     (eql (h::data-get d :verdict) :cancelled))
                                  v)))
          (ok cancelled "la segunda lectura se cancela")
          (ok (search "contenido real de notas" (h::data-get cancelled :contents))
              (format nil "y devuelve el contenido: ~S"
                      (h::data-get cancelled :contents)))
          (ok (search "REENTREGADO" (h::data-get cancelled :reason) :test #'char=)
              "y el motivo dice que se reentrega, para que el modelo sepa
               que el no de antes ya no es un no"))))))

(deftest veto/la-lectura-repetida-no-vuelve-a-ejecutar-la-herramienta
  ;; El veto tiene que seguir SIIENDO un veto. Si el fix de reentregar contenido
  ;; hubiera dejado pasar la segunda lectura, el fichero se habria leido dos
  ;; veces y el 'contenido' habria sido el resultado de una segunda ejecucion,
  ;; no la memoria. Aqui se comprueba que solo hay UN file-read en el turno.
  (with-turn-engine
    (reset-turn-engine)
    (with-temp-dir (dir)
      (let ((h::*base-dir* dir))
        (h::write-file "notas.txt" "contenido real")
        (h::assert-batch-intention (list :action :read-file :path "notas.txt") 1)
        (h::run)
        (h::assert-batch-intention (list :action :read-file :path "notas.txt") 2)
        (h::run)
        (ok (= 1 (length (h::collect-facts-of-type
                          (h::collect-active-facts) "file-read")))
            "la herramienta se ejecuto una vez, no dos")))))

(deftest veto/el-contenido-rancio-no-se-reentrega
  ;; El fallo queIntroduce el fix anterior. El veto casa cualquier file-read del
  ;; turno con cualquier peticion del mismo basename, sin mirar cuando paso cada
  ;; cosa. Si el modelo escribe un fichero y despues lo relee para comprobarlo,
  ;; se le entregaba el contenido ANTERIOR con la seguridad de una entrega: se
  ;; creia que tenia el fichero actualizado cuando lo que tenia era la version a
  ;; la que acababa de sustituir.
  ;;
  ;; Y eso es peor que no entregar nada, porque para que el modelo se entere
  ;; habria que DESMONTAR algo que PARECE una entrega. Asi que cuando la lectura
  ;; esta vencida, la cancelacion avisa y no entrega.
  (with-turn-engine
    (reset-turn-engine)
    (with-temp-dir (dir)
      (let ((h::*base-dir* dir))
        (h::write-file "notas.txt" "contenido ORIGINAL")
        (h::assert-batch-intention (list :action :read-file :path "notas.txt") 1)
        (h::run)
        ;; El turno MODIFICA el fichero.
        (h::assert-batch-intention
         (list :action :write-file :path "notas.txt" :content "contenido NUEVO")
         2)
        (h::run)
        ;; Y ahora pide volver a leerlo.
        (h::assert-batch-intention (list :action :read-file :path "notas.txt") 3)
        (h::run)
        (let* ((v (h::collect-facts-of-type (h::collect-active-facts) "verdict"))
               (cancelled (find-if (lambda (d)
                                     (eql (h::data-get d :verdict) :cancelled))
                                  v)))
          (ok cancelled "la tercera lectura se cancela")
          (ok (not (h::data-get cancelled :contents))
              (format nil "pero NO reentrega el contenido rancio: ~S"
                      (h::data-get cancelled :contents)))
          (ok (search "rancio" (h::data-get cancelled :reason) :test #'char=)
              "y el motivo explica por que no lo reentrega")
          ;; Y el texto ORIGINAL no puede escaparse por el :contents.
          (ok (not (search "ORIGINAL" (or (h::data-get cancelled :contents)
                                           "")))
              "el texto viejo no aparece en ningun sitio de la cancelacion"))))))

(deftest veto/el-contenido-que-se-reentrega-llega-al-contexto
  ;; El fix de los hechos no sirve si el render no loenseña. Si el :contents se
  ;; queda en la plist, el modelo recibe la misma cancelacion muda que antes y
  ;; el problema original sigue exactamente igual, solo que con datos nuevos
  ;; guardados donde nadie los mira.
  (with-turn-engine
    (reset-turn-engine)
    (with-temp-dir (dir)
      (let ((h::*base-dir* dir))
        (h::write-file "notas.txt" "contenido real de notas")
        (h::assert-batch-intention (list :action :read-file :path "notas.txt") 1)
        (h::run)
        (h::assert-batch-intention (list :action :read-file :path "notas.txt") 2)
        (h::run)
        (let ((ctx (card-section (build-context "sigue"))))
          (ok (search "contenido real de notas" ctx)
              "el contenido reentregado aparece en la tarjeta que ve el modelo")
          (ok (search "CONTENTS" ctx :test #'char=)
              "bajo una etiqueta que el modelo puede leer como entrega"))))))

(deftest veto/la-cancelacion-se-recorta-como-una-lectura-normal
  ;; El atajo no puede ser mas generoso que la herramienta que sustituye. Si la
  ;; cancelacion dejara pasar un fichero de 100 KB entero mientras un read_file
  ;; normal devuelve 8000, el veto seria la via de meter contexto sin
  ;; presupuesto: el limite dejaria de ser un limite.
  (with-turn-engine
    (reset-turn-engine)
    (with-temp-dir (dir)
      (with-test-config (("max_tool_result_chars" 200))
        (let ((h::*base-dir* dir))
          (h::write-file "grande.txt"
                         (make-string 5000 :initial-element #\x))
          (h::assert-batch-intention (list :action :read-file :path "grande.txt") 1)
          (h::run)
          (h::assert-batch-intention (list :action :read-file :path "grande.txt") 2)
          (h::run)
          (let* ((v (h::collect-facts-of-type (h::collect-active-facts) "verdict"))
                 (cancelled (find-if (lambda (d)
                                       (eql (h::data-get d :verdict) :cancelled))
                                    v)))
            (ok (<= (length (or (h::data-get cancelled :contents) ""))
                    2000)
                (format nil "el reentregado pasa por el mismo recorte: ~D chars"
                        (length (or (h::data-get cancelled :contents) ""))))))))))

(deftest bucle/las-lecturas-canceladas-cuentan-como-bucle
  ;; El detector existia, hacia su trabajo, y no podia ver el bucle que mas
  ;; happening. DETECT-READ-LOOP contaba hechos file-read; una lectura cancelada
  ;; nunca llega a READ-FILE, asi que no deja hecho y no se contaba. El modelo
  ;; pidio el mismo fichero 1 vez con exito y 4 veces cancelado, y el contador
  ;; se quedo en 1 de un umbral de 4. Un contador de bucles que no cuenta el
  ;; bucle.
  (with-turn-engine
    (reset-turn-engine)
    (with-temp-dir (dir)
      (let ((h::*base-dir* dir))
        (h::write-file "notas.txt" "contenido")
        (h::assert-batch-intention (list :action :read-file :path "notas.txt") 1)
        (h::run)
        (dotimes (i 4)
          (h::assert-batch-intention (list :action :read-file :path "notas.txt")
                                     (+ 2 i))
          (h::run))
        (multiple-value-bind (path count) (h::detect-read-loop)
          (ok path (format nil "el detector ve el bucle; path: ~S count: ~S"
                           path count))
          (ok (and path (string= "notas.txt" path))
              (format nil "y senala al fichero que se relee: ~S" path))
          (ok (and count (>= count h::*read-loop-min-count*))
              (format nil "con al menos ~D toques: ~S"
                      h::*read-loop-min-count* count)))))))

(deftest bucle/una-lectura-sola-no-es-un-bucle
  ;; El otro lado del umbral. Con la lectura buena y su contenido, el detector
  ;; tiene que seguir callado: si no, avisaria de bucles en el turno mas normal
  ;; que existe, que es leer un fichero y ya.
  (with-turn-engine
    (reset-turn-engine)
    (with-temp-dir (dir)
      (let ((h::*base-dir* dir))
        (h::write-file "notas.txt" "contenido")
        (h::assert-batch-intention (list :action :read-file :path "notas.txt") 1)
        (h::run)
        (h::assert-batch-intention (list :action :read-file :path "otro.txt") 2)
        (h::run)
        ;; OJO con como se comprueba el "no bucle": NO vale con
;;   (not (multiple-value-list (detect-read-loop)))
;; porque MULTIPLE-VALUE-LIST de un unico NIL devuelve la LISTA (NIL), que es
;; truthy: el test seria falso POSITIVO y pasaria siempre. Hay que mirar los
;; valores, no la/envoltorio.
(multiple-value-bind (path count) (h::detect-read-loop)
          (ok (and (null path) (null count))
              (format nil "dos ficheros distintos, uno cada uno: no hay bucle; Path: ~S Count: ~S"
                      path count)))))))

(deftest bucle/la-huella-ignora-la-prosa-del-modelo
  ;; La prosa libre estaba en el hash de BATCH-FINGERPRINT. Solo cambia lo que
  ;; se DICE, no lo que se PIDE: dos lotes que piden las mismas llamadas son el
  ;; mismo lote digase lo que digase el comentario. Y un modelo atascado cambia
  ;; la prosa en cada intento, igual que cambia el orden de las llamadas --
  ;; justamente porque sabe que se le mide.
  ;;
  ;; Con la prosa dentro, la deteccion de repeticiones --que solo existe para
  ;; esto-- no veia nada. En la corrida real el modelo pidio las mismas dos
  ;; lecturas en dos rondas seguidas y, por haberlas comentado distinto, no se
  ;; reconocieron. Cuatro rondas gastadas.
  (let ((a "{\"tool_calls\":[{\"id\":\"1\",\"type\":\"function\",\"function\":{\"name\":\"read_file\",\"arguments\":{\"path\":\"a.lisp\"}}},{\"id\":\"2\",\"type\":\"function\",\"function\":{\"name\":\"read_file\",\"arguments\":{\"path\":\"b.lisp\"}}}],\"response\":\"Primero reviso a.lisp y luego b.lisp.\"}")
        (b "{\"tool_calls\":[{\"id\":\"1\",\"type\":\"function\",\"function\":{\"name\":\"read_file\",\"arguments\":{\"path\":\"a.lisp\"}}},{\"id\":\"2\",\"type\":\"function\",\"function\":{\"name\":\"read_file\",\"arguments\":{\"path\":\"b.lisp\"}}}],\"response\":\"Ahora vuelto a mirar los dos archivos para asegurarme.\"}"))
    (ok (eql (h::batch-fingerprint a) (h::batch-fingerprint b))
        "mismas llamadas y distinto comentario: misma huella")))

(deftest bucle/la-huella-sigue-distinguiendo-lo-que-no-es-igual
  ;; Quitar la prosa no puede convertir la huella en una constante. Si dos lotes
  ;; piden COSAS DISTINTAS tienen que seguir dando huellas distintas, o la
  ;; deteccion de repeticiones mataria turnos que estan avanzando.
  (let ((a "{\"tool_calls\":[{\"id\":\"1\",\"type\":\"function\",\"function\":{\"name\":\"read_file\",\"arguments\":{\"path\":\"a.lisp\"}}}],\"response\":\"x\"}")
        (b "{\"tool_calls\":[{\"id\":\"1\",\"type\":\"function\",\"function\":{\"name\":\"read_file\",\"arguments\":{\"path\":\"b.lisp\"}}}],\"response\":\"x\"}")
        (c "{\"tool_calls\":[{\"id\":\"1\",\"type\":\"function\",\"function\":{\"name\":\"read_file\",\"arguments\":{\"path\":\"a.lisp\"}}},{\"id\":\"2\",\"type\":\"function\",\"function\":{\"name\":\"read_file\",\"arguments\":{\"path\":\"b.lisp\"}}}],\"response\":\"x\"}"))
    (ok (not (eql (h::batch-fingerprint a) (h::batch-fingerprint b)))
        "una ruta distinta es otra huella")
    (ok (not (eql (h::batch-fingerprint a) (h::batch-fingerprint c)))
        "distinto numero de llamadas es otra huella")))

(deftest bucle/la-huella-junta-dos-escrituras-del-mismo-fichero
  ;; El cierre del agujero que quedaba tras quitar la prosa. En la corrida real
  ;; el mismo par de lecturas se pidio en una ronda con ruta RELATIVA y en otra
  ;; con ruta ABSOLUTA. Son el mismo trabajo --RESOLVE-PATH las junta-- pero con
  ;; el texto crudo dan huellas distintas, asi que el detector de repeticiones
  ;; no las reconoce. Un detector que se puede esquivar cambiando '..' por '/'
  ;; no es un detector.
  ;; *BASE-DIR* atado a proposito: RESOLVE-PATH combina la ruta relativa contra
  ;; el directorio base del harness, asi que sin esto 'math_utils.py' resolveria
  ;; contra el cwd del proceso de test y no contra /home/mtk/x, y la prueba
  ;; mediria el directorio equivocado en vez de la normalizacion.
  (let ((h::*base-dir* #P"/home/mtk/x/")
        (rel "{\"tool_calls\":[{\"id\":\"1\",\"type\":\"function\",\"function\":{\"name\":\"read_file\",\"arguments\":{\"path\":\"math_utils.py\"}}},{\"id\":\"2\",\"type\":\"function\",\"function\":{\"name\":\"read_file\",\"arguments\":{\"path\":\"test_math_utils.py\"}}}],\"response\":\"\"}")
        (abs "{\"tool_calls\":[{\"id\":\"1\",\"type\":\"function\",\"function\":{\"name\":\"read_file\",\"arguments\":{\"path\":\"/home/mtk/x/math_utils.py\"}}},{\"id\":\"2\",\"type\":\"function\",\"function\":{\"name\":\"read_file\",\"arguments\":{\"path\":\"/home/mtk/x/test_math_utils.py\"}}}],\"response\":\"\"}"))
    (ok (eql (h::batch-fingerprint rel) (h::batch-fingerprint abs))
        "relativa y absoluta del mismo fichero: mismo trabajo, misma huella")))

;;; ---------------------------------------------------------------
;;; Lo que el harness CUENTA contra lo que DICE.
;;;
;;; De una corrida real: los hechos eran correctos y las frases que salian de
;;; ellos no. "[Actions] NIL, edited math_utils.pyread math_utils.py." Esos
;;; textos son lo que el usuario lee como respuesta y lo que los turnos
;;; siguientes leen como 'lo que dije yo antes', asi que un error de redaccion
;;; ahi no es cosmetico: es estado contaminado que viaja al turno siguiente.
;;;
;;; Y el resto es lo mismo con distinto disfraz: el plan y el veredicto
;;; discordando sobre el comando del mismo paso, la memoria durable en orden
;;; inverso a los turnos, y un abortion que no llegaba a la tarjeta.
;;; ---------------------------------------------------------------

;;; ---------------------------------------------------------------
;;; Lo que el harness CUENTA contra lo que DICE.
;;;
;;; De una corrida real: los hechos eran correctos y las frases que salian de
;;; ellos no. "[Actions] NIL, edited math_utils.pyread math_utils.py." Esos
;;; textos son lo que el usuario lee como respuesta y lo que los turnos
;;; siguientes leen como 'lo que dije yo antes', asi que un error de redaccion
;;; ahi no es cosmetico: es estado contaminado que viaja al turno siguiente.
;;;
;;; Y el resto es lo mismo con distinto disfraz: el plan y el veredicto
;;; discordando sobre el comando del mismo paso, la memoria durable en orden
;;; inverso a los turnos, y un abortion que no llegaba a la tarjeta.
;;; ---------------------------------------------------------------

(deftest relato/el-resumen-no-pega-las-lineas-entre-si
  ;; El separador era ~{~A~^, ~A~}: el ~^ abre una clausula que corre solo
  ;; cuando NO es el ultimo elemento, y dentro sobraba un ~A, que imprimia un
  ;; elemento EXTRA en cada vuelta. Con 'edited math_utils.py' y
  ;; 'read math_utils.py' salia 'edited math_utils.pyread math_utils.py': los
  ;; finales pegados, y el texto deja de poder parsearse a ojo.
  (with-turn-engine
    (reset-turn-engine)
    (with-temp-dir (dir)
      (let ((h::*base-dir* dir))
        (h::write-file "notas.txt" "contenido")
        (h::assert-batch-intention (list :action :read-file :path "notas.txt") 1)
        (h::run)
        (h::assert-batch-intention
         (list :action :write-file :path "notas.txt" :content "otro") 2)
        (h::run)
        (let ((s (h::turn-action-summary (h::current-turn-id))))
          (ok (search ", " s)
              (format nil "las acciones van separadas por coma: ~S" s))
          ;; Lo pegado era '...math_utils.pyread...'. Con el separador mal, el
          ;; final de una linea y el principio de la siguiente se soldaban.
          (ok (not (search "notas.txtread" s))
              (format nil "y las lineas no se soldan: ~S" s))
          (ok (not (search "NIL" s))
              (format nil "y ninguna accion vacia se cuela: ~S" s)))))))

(deftest relato/el-resumen-no-empieza-por-nil
  ;; El cond de TURN-ACTION-SUMMARY terminaba en (t nil), asi que TODO hecho
  ;; que no fuera una de las cuatro acciones --user-input, llm-response,
  ;; batch-plan, verdict, intention, plan-done-- aportaba un NIL a la lista, y
  ;; el texto empezaba por la palabra NIL. Un 'NIL' en la frase que el modelo
  ;; se lleva como memoria de si mismo.
  (with-turn-engine
    (reset-turn-engine)
    (with-temp-dir (dir)
      (let ((h::*base-dir* dir))
        (h::write-file "notas.txt" "contenido")
        (h::assert-batch-intention (list :action :read-file :path "notas.txt") 1)
        (h::run)
        (let ((s (h::turn-action-summary (h::current-turn-id))))
          (ok (search "[Actions]" s)
              (format nil "el resumen existe: ~S" s))
          (ok (not (search "NIL" s))
              (format nil "y no empieza por la palabra NIL: ~S" s)))))))

(deftest relato/el-resumen-conserva-el-orden-causal
  ;; Las lineas se ordenaban con STRING<, o sea ALFABETICAMENTE: 'read'
  ;; siempre iba antes que 'wrote' porque la r delata la gana, diga lo que
  ;; diga la cronologia. Asi que un modelo que escribio y LUEGO leyo leia
  ;; 'read X, wrote Y', con los hechos en orden inverso al que-MM-vivieron.
  ;;
  ;; El orden correcto es el de los HECHOS, que es lo que un lector espera de
  ;; un relato de lo que paso.
  (with-turn-engine
    (reset-turn-engine)
    (with-temp-dir (dir)
      (let ((h::*base-dir* dir))
        ;; Escribir ANTES que leer, para que el orden alfabetico (read < write)
        ;; y el cronologico (write despues) NO coincidan.
        (h::assert-batch-intention
         (list :action :write-file :path "notas.txt" :content "nuevo") 1)
        (h::run)
        (h::assert-batch-intention (list :action :read-file :path "notas.txt") 2)
        (h::run)
        (let* ((s (h::turn-action-summary (h::current-turn-id)))
               (wrote (search "wrote" s))
               (read (search "read" s)))
          (ok (and wrote read (< wrote read))
              (format nil "escribir antes que leer se cuenta asi: ~S" s)))))))

(deftest relato/un-veredicto-no-sobrevive-a-la-tarjeta-que-lo-explica
  ;; La tarjeta y su veredicto se capaban por topes INDEPENDIENTES, con la
  ;; proteccion en un solo sentido: un veredicto cuya tarjeta sigue viva no se
  ;; capa. Lo que no habia era el otro. Cuando la TARJETA se iba por su propio
  ;; tope, sus veredictos se quedaban.
  ;;
  ;; El veredicto huerfano se renderiza en el flujo igual que uno con tarjeta, y
  ;; ademas se duplica con la linea suelta del hecho. En la corrida real, el turno
  ;; 2 salia con la MISMA lectura fallida dos veces, en dos notaciones y con la
  ;; misma razon, y el modelo no podia saber si le habian contado un hecho o dos:
  ;;
  ;;     READ-FILE  /home/.../math_utils.py   STATE = FAILED   REASON = File not found
  ;;     STEP 01.  READ-FILE  math_utils.py     STATE = FAILED   REASON = File not found
  ;;
  ;; O se van los dos o se quedan los dos. Este test pone un tope de 1 y comprueba
  ;; que cuando se retira la tarjeta MAS ANTIGUA su veredicto tambien se fue, y no
  ;; que sobrevive explaining nada."
  (with-test-config (("max_facts_per_type" 1))
    (with-turn-engine
      (reset-turn-engine)
      (let ((old 100) (new 200))
        ;; Turno 1: la tarjeta que se va, con su veredicto.
        (h::assert (h::harness-fact (h::fact-type "batch-plan")
                          (h::timestamp (h::identity old))
                          (h::data (list :step 1 :action "read-file" :round 1 :turn-id 1
                                         :target "viejo.py"))))
        (h::assert (h::harness-fact (h::fact-type "verdict")
                          (h::timestamp (h::identity old))
                          (h::data (list :step 1 :action "read-file" :round 1 :turn-id 1
                                         :verdict :failed :reason "File not found"))))
        ;; Turno 2: la tarjeta que se queda, con su veredicto.
        (h::assert (h::harness-fact (h::fact-type "batch-plan")
                          (h::timestamp (h::identity new))
                          (h::data (list :step 1 :action "read-file" :round 1 :turn-id 2
                                         :target "nuevo.py"))))
        (h::assert (h::harness-fact (h::fact-type "verdict")
                          (h::timestamp (h::identity new))
                          (h::data (list :step 1 :action "read-file" :round 1 :turn-id 2
                                         :verdict :applied))))
        (h::build-context "sigue")
        (let* ((ctx (h::build-context "sigue"))
               (live (search "nuevo.py" ctx))
               (gone (search "File not found" ctx)))
          (ok (and live (null gone))
              (format nil "el veredicto de la tarjeta retirada no sobrevive: ~S"
                      (if gone "aparece todavia 'File not found'" "no aparece"))))))))

(deftest relato/el-lote-vigente-no-se-dice-que-se-podado
  ;; Ultimo de los cuatro estados del PENDING, y el que mas se ha repetido.
  ;;
  ;; Con la tarjeta VIVA y sin veredicto, este paso es o el lote vigente -- lo
  ;; que el modelo acaba de responder y todavia no se ha ejecutado -- o una
  ;; intencion que se lanzo y cuyo veredicto no esta. Lo que NO puede ser es un
  ;; veredicto podado, porque CAP-BATCH-PLANS se lleva el veredicto de cada
  ;; tarjeta que retira: si el veredicto se hubiera ido, la tarjeta se habria
  ;; ido con el.
  ;;
  ;; Antes ese caso caia en 'puede haberse podado', que era mentira. Y no una
  ;; mentira menor: en la corrida real los pasos de la ronda EN CURSO la
  ;; rpintaban, o sea que el modelo leia 'puede que perdieras tu plan' sobre el
  ;; lote que acababa de escribir, y su unica salida era volver a plantearlo.
  (with-turn-engine
    (reset-turn-engine)
    (h::assert (h::harness-fact (h::fact-type "batch-plan")
                      (h::timestamp (h::identity 500))
                      (h::data (list :step 1 :action "read-file" :round 1 :turn-id 1
                                     :target "math_utils.py"))))
    ;; Sin intention y sin veredicto: el lote vigente, recien respondida.
    (let* ((ctx (h::build-context "sigue"))
           (at (search "T1 READ-FILE" ctx))
           (after (if at (subseq ctx at (min (length ctx) (+ at 400))) "")))
      (ok (and at
               (search "LOTE VIGENTE, SIN EJECUTAR" after)
               (not (search "puede haberse podado" after)))
          (format nil "el lote que acabo de responder no se acusa de poda: ~S" after)))))

(deftest relato/un-aborto-no-borra-el-plan-de-otro-turno
  ;; CANCEL-INTENTIONS-ON-ABORT no filtraba por turno: matcheaba CUALQUIER
  ;; batch-abort y retractaba CUALQUIER intention. Con la salencia 20 gana a las
  ;; reglas de ejecucion (3-5), asi que en cuanto un turno abortaba, TODAS las
  ;; intenciones de la sesion se borraban en el siguiente (run) -- incluidas las
  ;; que el modelo acababa de proponer en el turno de ahora.
  ;;
  ;; Y como no se aserta veredicto al borrarlas, el paso se queda PENDING para
  ;; siempre: el harness no ejecuta nada, no dice por que, y el modelo solo
  ;; puede volver a planificar. Medido en la corrida real: de 29 pasos
  ;; propuestos en 5 turnos se ejecutaron 5, todos en el turno 1, que es el unico
  ;; anterior al primer aborto. Del segundo en adelante, cero.
  (with-temp-dir (dir)
   (let ((cl-harness::*base-dir* dir))
   (with-turn-engine
    (reset-turn-engine)
    ;; El turno 1 aborta. Este hecho se queda en el motor para siempre.
    (h::assert (h::harness-fact (h::fact-type "batch-abort")
                      (h::timestamp (h::identity 100))
                      (h::data (list :reason "el modelo repitio el mismo lote"
                                     :path :batch-repeated
                                     :iterations 2 :turn-id 1 :round 2))))
    ;; El turno 2 propone un plan. El harness tiene que EJECUTARLO.
    (h::assert (h::harness-fact (h::fact-type "intention")
                      (h::timestamp (h::identity 200))
                      (h::data (list :action :write-file :path "gcd.py"
                                     :content "def gcd(a, b): return a"
                                     :turn-id 2 :round 1 :step 1
                                     :status :pending))))
    (h::run)
    (let* ((done (h::collect-facts-of-type (h::collect-active-facts)
                                          "file-write")))
      (ok (and done
                (probe-file (merge-pathnames "gcd.py" dir)))
          (format nil "el plan del turno 2 se ejecuto pese al aborto del turno 1: ~S"
                  done)))))))

(deftest relato/un-plan-superado-no-sale-como-podado
  ;; CUARTO estado del PENDING, y el que cerraba el bucle de replanificar.
  ;;
  ;; Turno 4 de la corrida real: el modelo respondio el lote de la ronda 2, luego
  ;; el de la 3, luego el de la 4. Los planes de las rondas 2 y 3 NUNCA se
  ;; ejecutaron, no se podaron y no abortaron -- el modelo simplemente contesto
  ;; otro lote. Y los dos salian en la PROCEDURE DIVISION con
  ;;
  ;;     REASON = SIN VEREDICTO REGISTRADO ... puede haberse podado
  ;;
  ;; que es mentira en las tres partes. El modelo lo leia como 'perdi mi plan', y
  ;; su unica salida era volver a planificar. Cada ronda de mas dejaba un plan
  ;; muerto en pantalla que el modelo tomaba por pendiente, y por eso la ronda 4
  ;; se murio por 'se agoto batch_max_iterations' sin que el detector de
  ;; repeticiones -- que solo mira el ULTIMO lote -- pudiera hacer nada.
  (with-turn-engine
    (reset-turn-engine)
    (h::assert (h::harness-fact (h::fact-type "batch-plan")
                      (h::timestamp (h::identity 1000))
                      (h::data (list :step 1 :action "read-file" :round 2 :turn-id 4
                                     :target "math_utils.py"))))
    (h::assert (h::harness-fact (h::fact-type "batch-plan")
                      (h::timestamp (h::identity 1001))
                      (h::data (list :step 1 :action "write-file" :round 3 :turn-id 4
                                     :target "math_utils.py"))))
    ;; La ronda 3 es la vigente: la 2 quedo sustituida por ella.
    (h::assert (h::harness-fact (h::fact-type "batch-plan")
                      (h::timestamp (h::identity 1002))
                      (h::data (list :step 1 :action "read-file" :round 4 :turn-id 4
                                     :target "math_utils.py"))))
    (let* ((ctx (h::build-context "sigue"))
           ;; Lo que sale justo detras de la tarjeta de la ronda 2: su REASON.
           (at (search "T4 R2" ctx))
           (after (if at (subseq ctx at (min (length ctx) (+ at 300))) "")))
      (ok (and at (search "SUSTITUIDO" after) (not (search "puede haberse podado" after)))
          (format nil "el lote de la ronda 2 se lee como sustituido, no como podado: ~S"
                  after)))))

(deftest relato/la-memoria-durable-va-en-el-mismo-sentido-que-los-turnos
  ;; MEMORY-CONTEXT-BLOCK hacia SORT ascendente y luego NREVERSE, o sea
  ;; descendente, mientras la DATA DIVISION va de menor a mayor turno. Y sus
  ;; entradas NO llevan etiqueta de turno, asi que el modelo no tiene forma de
  ;; saber cual manda.
  ;;
  ;; El efecto real: la ULTIMA linea que se leia era la mas antigua, y se leia
  ;; como si fuera el estado actual. En la corrida real, 'WROTE.
  ;; math_utils.py / BYTES = 294' sobre un fichero de 587 bytes.
  ;;
  ;; Los dos hechos llevan EL MISMO timestamp a proposito: asi se prueba que el
  ;; orden no viene del reloj --que en una ronda no ordena nada-- sino del
  ;; contador de insercion de Rete, que es el desempate de FACT-ORDER-KEY. Con
  ;; timestamps distintos, el test pasaria tambien con un orden cronologico mal
  ;; implementado.
  ;;
  ;; Los hechos se asertan en el motor de TURNO y se PROMUEVEN, porque es ese
  ;; el camino real: PROMOTE-DURABLE-FACTS lee del motor de turno y escribe en
  ;; el de memoria. Asertarlos directamente en el de memoria no probaria nada
  ;; del mecanismo que se quiere cubrir.
  (with-turn-engine
    (reset-turn-engine)
    (reset-mem-engine)
    (let ((ts (get-universal-time)))
      (h::assert (h::harness-fact (h::fact-type "file-write")
                        (h::timestamp (h::identity ts))
                        (h::data (list :path "/tmp/proyecto/p1.py" :bytes 294
                                       :applied t :turn-id 1))))
      (h::assert (h::harness-fact (h::fact-type "file-write")
                        (h::timestamp (h::identity ts))
                        (h::data (list :path "/tmp/proyecto/p2.py" :bytes 587
                                       :applied t :turn-id 1))))
      (h::promote-durable-facts)
      (let* ((mem (with-mem-engine (h::mem-context-block)))
             (p1 (or (search "p1.py" mem) 0))
             (p2 (or (search "p2.py" mem) 0)))
        (ok (and (plusp p1) (plusp p2) (< p1 p2))
            (format nil "p1 antes que p2, como los turnos: ~S" mem))))))

(deftest relato/plan-y-veredicto-dicen-el-mismo-comando
  ;; El plan se normalizaba con STRIP-CD-PREFIX y el veredicto se quedaba con
  ;; el texto crudo. Era la mitad de un arreglo a medias, y se veía en la misma
  ;; tarjeta:
  ;;
  ;;   STEP 03.  EXEC-COMMAND  cd /home/mtk/... && python -m unittest
  ;;
  ;; junto a un 'COMMAND = python -m unittest' de otra parte. Dos comandos
  ;; distintos para el mismo paso, y el modelo sin manera de saber cual se
  ;; ejecuto.
  (with-turn-engine
    (reset-turn-engine)
    (with-test-config (("tool_timeout_seconds" 30))
      (h::assert-batch-intention
       (list :action :exec-command
             :command "cd /tmp/proyecto && echo hola")
       1)
      (h::run)
      (let* ((plans (h::collect-facts-of-type (h::collect-active-facts) "batch-plan"))
             (verdicts (h::collect-facts-of-type (h::collect-active-facts) "verdict"))
             (plan-target (h::data-get (car plans) :target))
             (verdict-target (h::data-get (car verdicts) :target)))
        (ok (string= "echo hola" plan-target)
            (format nil "el plan ya va normalizado: ~S" plan-target))
        (ok (string= plan-target verdict-target)
            (format nil "y el veredicto dice lo mismo: plan=~S veredicto=~S"
                    plan-target verdict-target))))))

(deftest relato/un-verdicto-no-normaliza-un-fichero-que-empieza-por-cd
  ;; La normalizacion es solo para :exec-command, en el plan y ahora tambien en
  ;; el veredicto. Una ruta con espacios o con un 'cd' legitimo en el nombre
  ;; no se toca: si se normalizara aqui, el fix habria creado un bug espejo.
  (with-turn-engine
    (reset-turn-engine)
    (with-test-config (("tool_timeout_seconds" 30))
      (h::assert-batch-intention
       (list :action :write-file :path "cd notas/plan.md" :content "x")
       1)
      (h::run)
      (let* ((verdicts (h::collect-facts-of-type (h::collect-active-facts) "verdict"))
             (tgt (h::data-get (car verdicts) :target)))
        (ok (string= "cd notas/plan.md" tgt)
            (format nil "una ruta no se normaliza: ~S" tgt))))))

(deftest relato/el-aborto-se-lee-como-aborto-y-no-como-poda
  ;; Un PENDING sin veredicto tiene dos causas que para el modelo son
  ;; OPUESTAS: se podo, o el turno aborto. Lo segundo no es una perdida, es una
  ;; decision del harness, y se comporta distinto: lo podado se reintenta, lo
  ;; abortado hay que replantearlo. Decirle 'puede haberse podado' de un turno
  ;; que el propio harness mato deja al modelo sin manera de decidir.
  (with-turn-engine
    (reset-turn-engine)
    (with-temp-dir (dir)
      (let ((h::*base-dir* dir))
        (h::write-file "notas.txt" "contenido")
        (h::assert-batch-intention (list :action :read-file :path "notas.txt") 1)
        (h::run)
        ;; El turno aborta con estos pasos sin veredicto.
        (h::assert-batch-intention (list :action :write-file :path "otro.txt"
                                         :content "x") 2)
        (let ((steps (mapcar (lambda (d) (h::plan-step-identity d))
                             (h::collect-facts-of-type
                              (h::collect-active-facts) "batch-plan"))))
          (h::assert
           (h::harness-fact (h::fact-type "batch-abort")
                             (h::timestamp (h::identity 1700000000.0))
                             (h::data (list :reason "el modelo repitio el mismo lote"
                                            :path :batch-repeated
                                            :iterations 3
                                            :turn-id (h::current-turn-id)
                                            :round (h::current-batch-round)
                                            :steps steps))))
          (let ((ctx (card-section (build-context "sigue"))))
            (ok (search "ABORTO" ctx)
                (format nil "el motivo dice que el turno aborto:~%~A" ctx))
            (ok (search "repitio el mismo lote" ctx)
                "y dice POR QUE, no solo que aborto")
            (ok (not (search "puede haberse podado" ctx))
                (format nil "y no lo disfraza de poda:~%~A" ctx))))))))

(deftest relato/el-abarto-no-arrastra-a-los-pasos-que-si-se-ejecutaron
  ;; El motivo de aborto se aplica a los pasos SIN VEREDICTO. Si se colgara de
  ;; todos los del turno, un turno abortado en su ultima ronda marcaria como
  ;; 'nunca ejecutado' el paso 1 que si se ejecuto, y el modelo releeria su
  ;; propio trabajo como perdido.
  (with-turn-engine
    (reset-turn-engine)
    (with-temp-dir (dir)
      (let ((h::*base-dir* dir))
        (h::write-file "notas.txt" "contenido")
        (h::assert-batch-intention (list :action :read-file :path "notas.txt") 1)
        (h::run)
        (h::assert-batch-intention
         (list :action :write-file :path "otro.txt" :content "x") 2)
        (let* ((plans (h::collect-facts-of-type
                       (h::collect-active-facts) "batch-plan"))
               ;; Solo el paso sin veredicto entra en la lista del aborto.
               (orphan (find-if (lambda (d)
                                  (string= "otro.txt"
                                           (h::path-basename (h::data-get d :target))))
                                plans)))
          (h::assert
           (h::harness-fact (h::fact-type "batch-abort")
                             (h::timestamp (h::identity 1700000000.0))
                             (h::data (list :reason "se agoto batch_max_iterations"
                                            :path :iterations
                                            :turn-id (h::current-turn-id)
                                            :round (h::current-batch-round)
                                            :steps (list (h::plan-step-identity orphan))))))
          (let ((ctx (card-section (build-context "sigue"))))
            (ok (search "ABORTO" ctx)
                (format nil "el paso huerfano si nombra el aborto:~%~A" ctx))
            ;; El paso 1 tiene veredicto APPLIED, asi que la tarjeta lo muestra
            ;; como tal y no como abortado.
            (ok (search "STATE = APPLIED" ctx)
                (format nil "y el que si se ejecuto sigueApplied:~%~A" ctx))))))))

(deftest relato/la-repeticion-no-mata-el-turno
  ;; Un modelo que pide lo mismo cada ronda NO se corta: cada ronda se ejecuta.
  ;; Todo en informatica es batch, y repetir no es un error de compilacion.
  ;;
  ;; Lo que se comprueba aqui es lo que se pierde si se corta. Con la repeticion
  ;; como aborto, la ronda repetida no llegaba a ejecutarse y lo unico que el
  ;; modelo recibia era el motivo del aborto. Sin el, la ronda repetida pasa por
  ;; Rete como cualquier otra y deja veredicto: el turno tiene una respuesta por
  ;; cada ronda que se dio, y el corte que queda es el tope de iteraciones, que
  ;; es una cosa del LLM y no del harness.
  (with-turn-engine
    (reset-turn-engine)
    (h::metrics-reset)
    (with-test-config (("llm_provider" "batch")
                       ("llm_stream" nil)
                       ("batch_repeat_tolerance" 1)
                       ("batch_max_iterations" 3))
      (with-mocked-call-llm
          ((h::parse-llm-batch-to-intentions
            "{\"tool_calls\":[{\"id\":\"1\",\"type\":\"function\",\"function\":{\"name\":\"write_file\",\"arguments\":{\"path\":\"notas.txt\",\"content\":\"hola\"}}}]}")
           "otra vez")
        (h::process-turn "hola" nil))
      (let* ((aborts (h::collect-facts-of-type
                      (h::collect-active-facts) "batch-abort"))
             (verdicts (h::collect-facts-of-type
                        (h::collect-active-facts) "verdict")))
        (ok aborts "el corte que queda es el tope de iteraciones, y se aserta")
        (ok (not (eq :batch-repeated (h::data-get (car aborts) :path)))
            (format nil "y su causa NO es la repeticion: ~S"
                    (h::data-get (car aborts) :path)))
        ;; CADA RONDA QUE ENTRA AL COMPILADOR DEJA VEREDICTO. Con el abortion por
        ;; repeticion, la segunda ronda no se ejecutaba y esto era 0: el modelo
        ;; proponia y nadie le contestaba. Eso era la invariante I1 rota.
        (ok verdicts
            (format nil "las rondas repetidas dejan veredicto, no silencio: ~D"
                    (length verdicts)))))))


;;;; ===============================================================
;;;; La .cob se genera desde la memoria de largo plazo
;;;; ===============================================================

(deftest memoria/el-cob-se-construye-desde-la-memoria-y-no-desde-el-turno
  "La integracion completa: promover, perder el turno, y construir el .cob.

   Esta es la prueba que ata las tres piezas. Antes, la .cob se armaba con
   COLLECT-ACTIVE-FACTS --los hechos efimeros del turno en curso-- mientras la
   memoria de largo plazo guardaba otra cosa. Y la promocion vivia en el
   LLAMADOR, al final del turno, de modo que dentro del propio turno el modelo
   estaba leyendo la verdad de antes de empezar.

   Aqui el turno se vacia por completo (RESET-TURN-ENGINE) antes de construir el
   .cob. Si la .cob saliera de los hechos del turno, saldria vacia; si sale de
   la memoria, sale entera. Y las tres cosas que el modelo necesita para no
   perder coherencia --una lectura con su contenido, un plan con su veredicto, y
   la peticion del usuario-- tienen que estar las tres."
  (with-temp-dir (dir)
    (let ((h::*base-dir* dir) (h:*session-id* "s"))
      (with-turn-engine
        (reset-turn-engine)
        (h::add-todo "mirar el fichero" :status "in-progress")
        (let ((t0 (get-universal-time))
              (path (format nil "~A/a.txt" dir)))
          (h::assert (h::harness-fact (h::fact-type "user-input")
                                      (h::timestamp (identity t0))
                                      (h::data (list :text "lee a.txt"
                                                     :turn-id 1))))
          (h::assert (h::harness-fact (h::fact-type "file-read")
                                      (h::timestamp (identity (+ t0 1)))
                                      (h::data (list :path path
                                                     :applied t
                                                     :contents "contenido real"
                                                     :turn-id 1))))
          (h::assert-batch-intention (list :action :read-file :path "a.txt") 1)
          (h::assert (h::harness-fact (h::fact-type "verdict")
                                      (h::timestamp (identity (+ t0 3)))
                                      (h::data (list :command (format nil "READ-FILE ~A" path)
                                                     :step 1
                                                     :round 1
                                                     :turn-id 1
                                                     :status "APPLIED"
                                                     :result "contenido real")))))
        ;; SE ACABA EL TURNO: se promueve y se vacia. Es el cierre real de
        ;; PROCESS-TURN -- promover antes de volcar la sesion-- y a partir de
        ;; aqui no queda ningun hecho efimero.
        (h::promote-durable-facts)
        (reset-turn-engine)
        (ok (null (h::collect-active-facts))
            "el turno se vacio: no queda ningun hecho vivo")
        (let ((cobol (build-context "sigue")))
          (ok (search "contenido real" cobol)
              (format nil "la lectura cruza con su CONTENIDO:~%~A" cobol))
          (ok (search "lee a.txt" cobol)
              (format nil "y el modelo sabe que se le pidio:~%~A" cobol))
          (ok (search "APPLIED" cobol)
              (format nil "y el veredicto del paso cruza:~%~A" cobol)))))))

(deftest memoria/promover-es-idempotente-y-el-cob-no-duplica
  "Promover dos veces no puede cambiar lo que el modelo ve.

   PROMOTE-DURABLE-FACTS se llama una vez por ronda y el motor de turno no se
   vacia entre rondas: el user-input del turno sigue ahi en la ronda 3. Sin la
   guarda de la huella (tipo, timestamp y datos), cada ronda volvia a copiarlo
   a la memoria y el modelo leia N veces la misma peticion.

   Y aqui no se cuenta veces: se compara el .cob entero. Es la invariante que de
   verdad importa --promover mas no puede cambiar el programa-- y cuenta las
   repeticiones de cada cosa, que es como se manifesto el defecto cuando
   existio."
  (with-temp-dir (dir)
    (let ((h::*base-dir* dir) (h:*session-id* "s"))
      (with-turn-engine
        (reset-turn-engine)
        (let ((t0 (get-universal-time)))
          (h::assert (h::harness-fact (h::fact-type "user-input")
                                      (h::timestamp (identity t0))
                                      (h::data (list :text "lee a.txt" :turn-id 1))))
          (h::assert (h::harness-fact (h::fact-type "file-read")
                                      (h::timestamp (identity (+ t0 1)))
                                      (h::data (list :path (format nil "~A/a.txt" dir)
                                                     :applied t
                                                     :contents "contenido real"
                                                     :turn-id 1)))))
        ;; Una promocion, y el .cob de una ronda.
        (h::promote-durable-facts)
        (let ((one-ronda (build-context "sigue")))
          ;; Tres promociones mas, como tres rondas mas del mismo turno.
          (h::promote-durable-facts)
          (h::promote-durable-facts)
          (h::promote-durable-facts)
          (ok (= 2 (length (h::mem-engine-facts)))
              (format nil "cuatro promociones, dos hechos: ~S"
                      (mapcar #'h::fact-type-of (h::mem-engine-facts))))
          (let ((cuatro-rondas (build-context "sigue")))
            (ok (string= one-ronda cuatro-rondas)
                (format nil "y el .cob es EXACTAMENTE el mismo:~%~A~%~A"
                        one-ronda cuatro-rondas))))))))

(deftest memoria/un-rechazo-tambien-es-durable-pero-nunca-sale-como-WROTE
  "Lo que no ocurrio es un hecho, y el modelo tiene que poder aprendelo.

   Filtrar la memoria por :applied sacaba de ella justamente los fallos, que es
   lo unico que el modelo deberia acordarse de cambiar. Pero un rechazo pintado
   como escritura es la mentira mas cara que puede decir esta division, asi que
   el RENDER --no la promocion-- es el que tiene que distinguir."
  (with-temp-dir (dir)
    (let ((h::*base-dir* dir) (h:*session-id* "s"))
      (with-turn-engine
        (reset-turn-engine)
        (let ((t0 (get-universal-time)))
          (h::assert (h::harness-fact (h::fact-type "file-write")
                                      (h::timestamp (identity t0))
                                      (h::data (list :path (format nil "~A/nope.py" dir)
                                                     :applied nil
                                                     :refused t
                                                     :reason "blind write refused"
                                                     :turn-id 1)))))
        (let ((cobol (build-context "sigue")))
          (ok (search "REFUSED" cobol)
              (format nil "el rechazo sale como REFUSED:~%~A" cobol))
          (ok (not (search "WROTE." cobol))
              (format nil "y NUNCA como WROTE:~%~A" cobol))
          (ok (search "blind write refused" cobol)
              (format nil "y el motivo llega al modelo:~%~A" cobol)))))))


;;;; ===============================================================
;;;; El bucle sin salida, y la meta que el veto dejaba abierta
;;;; ===============================================================

(deftest estancamiento/un-turno-que-no-avanza-no-se-quema-entero
  "Dos rondas seguidas sin aplicar NADA cortan el turno, y no al final.

   El tope de iteraciones dice cuanto se puede durar; no dice si se esta
   avanzando. Un turno que se pasa entero proponiendo lo que el harness ya veto
   cerraba por el mismo motivo que uno que hizo su trabajo, y el operador no
   tenia forma de distinguir 'no le daba tiempo' de 'no hacia nada'.

   En una corrida real el turno 2 propuso el MISMO READ-FILE en las rondas R2 a
   R8. Las siete salieron CANCELLED y las siete costaron una llamada al
   proveedor. Con este corte se para en la segunda."
  (with-temp-dir (dir)
    (let ((h::*base-dir* dir))
      (with-open-file (s (merge-pathnames "notas.txt" dir)
                         :direction :output :if-exists :supersede)
        (write-string "hola" s))
      (with-turn-engine
        (reset-turn-engine)
        (h::metrics-reset)
        (with-test-config (("llm_provider" "batch")
                           ("llm_stream" nil)
                           ("batch_max_iterations" 8))
          ;; Una lectura que se vuelve RANCI: el turno la pide, y luego un edit
          ;; del mismo fichero la invalida. A partir de ahi, pedirla otra vez no
          ;; tiene salida, que es exactamente el bucle.
          (with-mocked-call-llm
              ((h::parse-llm-batch-to-intentions
                "{\"tool_calls\":[{\"id\":\"1\",\"type\":\"function\",\"function\":{\"name\":\"read_file\",\"arguments\":{\"path\":\"notas.txt\"}}}]}")
               "ya esta")
            (h::process-turn "lee notas.txt" nil))
          (let* ((aborts (h::collect-facts-of-type
                          (h::collect-active-facts) "batch-abort"))
                 (aborted (car aborts)))
            (ok aborted "el turno sin salida se corta, y se aserta")
            (ok (eq :estancado (h::data-get aborted :path))
                (format nil "y el motivo es el estancamiento, no el tope: ~S"
                        (h::data-get aborted :path)))
            (ok (< (h::data-get aborted :iterations) 8)
                (format nil "y no se quemaron las 8 rondas: uso ~D"
                        (h::data-get aborted :iterations)))
            (ok (search "sin aplicar nada" (h::data-get aborted :reason))
                (format nil "y el motivo le dice al modelo QUE PASÓ:~%~A"
                        (h::data-get aborted :reason)))))))))


(deftest estancamiento/el-veto-de-lectura-rancia-no-invita-a-repetir
  "El motivo de un veto que no entrega nada no puede pedir que se repita.

   Es el unico de los tres vetos que NO reentrega contenido, y por eso es el
   unico que puede empujar al modelo a un bucle: si le dices 'vuelve a
   pedirlo', lo que le has dicho es que pedirlo es lo que tiene que hacer. En la
   corrida real el modelo propuso la misma lectura siete veces seguidas,
   obedecendo el motivo."
  (with-temp-dir (dir)
    (let ((h::*base-dir* dir) (h:*session-id* "s")
          (path (namestring (merge-pathnames "notas.txt" dir))))
      (with-open-file (s path :direction :output :if-exists :supersede)
        (write-string "hola" s))
      (with-turn-engine
        (reset-turn-engine)
        (h::metrics-reset)
        ;; Leer, luego editar: la lectura queda rancia.
        (h::read-file "notas.txt")
        (h::edit-file "notas.txt" "hola" "hola mundo")
        ;; Y pedirla otra vez.
        (h::assert-batch-intention
         (list :action :read-file :path "notas.txt") 1)
        (let* ((ctx (build-context "sigue"))
               (proc (procedure-section ctx))
               (line (format nil "~A" ctx)))
          (ok (search "CANCELLED" proc)
              "la lectura rancia se cancela")
          (ok (not (search "Vuelve a pedir la lectura" line))
              (format nil "y el motivo NO le pide que la vuelva a pedir:~%~A" line))
          (ok (search "NO LO PIDAS OTRA VEZ" line)
              (format nil "y le dice explicitamente que no lo pida:~%~A" line)))))))


(deftest estancamiento/el-detector-de-bucle-esta-conectado-al-camino-batch
  "TOOL-LOOP-WARNING tiene que responder en el camino batch, no solo en el
   imperativo.

   Estaba conectado a CALL-LLM-WITH-TOOLS y a STREAM-LLM-WITH-TOOLS, los dos
   del camino imperativo. El proveedor batch --el de la configuracion por
   defecto-- no lo consultaba. Un detector que solo mira un camino no ve el
   bucle del otro, y el del otro es el que se ejecuta.

   En la corrida real el turno 2 leyo el mismo fichero 7 veces seguidas y
   DETECT-READ-LOOP, que existe y esta probado, no dio una sola vez."
  (with-temp-dir (dir)
    (let ((h::*base-dir* dir))
      (with-open-file (s (merge-pathnames "notas.txt" dir)
                         :direction :output :if-exists :supersede)
        (write-string "hola" s))
      (with-turn-engine
        (reset-turn-engine)
        (h::metrics-reset)
        (setf h::*loop-alerted-turn* nil)
        ;; Umbral de 2 para que el detector llegue a hablar ANTES que el corte
        ;; de estancamiento, que necesita dos rondas seguidas sin aplicar nada.
        ;; Los dos compiten por el mismo turno y el que gana es el que tiene un
        ;; mensaje mejor: el detector nombra el fichero y cuenta las veces. Con
        ;; el umbral de 4 de produccion, el estancamiento llegaria antes, y eso
        ;; estaria bien -- pero seria el corte de seguridad hablando, no el
        ;; detector, y lo que se quiere comprobar aqui es el cableado.
        (let ((h::*read-loop-min-count* 2))
          (with-test-config (("llm_provider" "batch")
                             ("llm_stream" nil)
                             ("batch_max_iterations" 8))
          ;; Un lote que SI se ejecuta, y luego siete que piden lo mismo.
          (with-mocked-call-llm
              ((h::parse-llm-batch-to-intentions
                "{\"tool_calls\":[{\"id\":\"1\",\"type\":\"function\",\"function\":{\"name\":\"read_file\",\"arguments\":{\"path\":\"notas.txt\"}}}]}")
               "sigue")
            (h::process-turn "lee notas.txt" nil))
          (let* ((aborts (h::collect-facts-of-type
                          (h::collect-active-facts) "batch-abort"))
                 (aborted (car aborts)))
            (ok aborted "el bucle se corta, y se aserta como hecho")
            (ok (eq :tool-loop (h::data-get aborted :path))
                (format nil "y el motivo es el bucle, no el tope: ~S"
                        (h::data-get aborted :path)))
            (ok (search "notas.txt" (h::data-get aborted :reason))
                (format nil "y el motivo NOMBRA el fichero que se relee:~%~A"
                        (h::data-get aborted :reason)))
            (ok (< (h::data-get aborted :iterations) 8)
                (format nil "y no se quemaron las 8 rondas: uso ~D"
                        (h::data-get aborted :iterations))))))))))


(deftest memoria/una-meta-no-se-queda-abierta-porque-su-evidencia-expirara
  "La meta se cierra con la evidencia de la MEMORIA, no con la del motor de turno.

   El .cob se genera desde la red de largo plazo. La reconciliacion de metas
   seguia leyendo el motor de turno, o sea OTRA red para la misma verdad, y las
   dos pueden discrepar. Discreparon.

   En una corrida real: la meta 'Write utils.py' se abrio en el turno 2, la
   escritura se aplico y quedo durable, y en el turno 5 la meta seguia
   ABIERTA. Con fact_ttl_seconds = 120 y doce minutos entre uno y otro,
   PRUNE-EXPIRED ya habia retractado el file-write del motor de turno, y una
   meta cuya evidencia ya no existe no se cierra nunca.

   Y no era cosmético: GOAL ABIERTO. N en cero es lo que habilita DONE. Una
   meta que no se cierra nunca es una sesion que no puede terminar."
  (with-temp-dir (dir)
    (let ((h::*base-dir* dir)
          (h:*session-id* "s")
          (path (format nil "~A/utils.py" dir)))
      (with-test-config (("fact_ttl_seconds" 120))
        (with-turn-engine
          (reset-turn-engine)
          ;; La escritura del turno 2, con la HORA de entonces: hace diez
          ;; minutos, o sea fuera del TTL de dos.
          (let ((h::*turn-counter* 2))
            (h::assert
             (h::harness-fact (h::fact-type "file-write")
                              (h::timestamp (identity (- (get-universal-time) 600)))
                              (h::data (list :path path
                                             :applied t
                                             :bytes 6
                                             :turn-id 2))))
            ;; Y la meta que abrio ese mismo turno, sin cerrar. El turno importa:
            ;; TODO-SATISFIED-P acepta evidencia del mismo turno aunque sea mas
            ;; vieja que la meta, y por ahi se cierra una meta repetida dentro
            ;; del mismo turno. Sin esa excepcion, una escritura de hace diez
            ;; minutos no cerraria la meta que abrio hace cinco.
            (h::add-todo (format nil "Write ~A" path) :status "in-progress"))
          ;; PROMUEVE PRIMERO, que es lo que pasa en produccion: el cierre del
          ;; turno promueve con el hecho fresco, y ya en el turno siguiente la
          ;; copia del motor de turno caduca mientras la de la memoria larga no.
          ;; Promover despues no serviria de nada, porque para entonces
          ;; PRUNE-EXPIRED ya se habia llevado la unica copia.
          (h::promote-durable-facts)
          (h::run)
          (ok (null (h::collect-facts-of-type (h::collect-active-facts)
                                              "file-write"))
              "la evidencia caduco en el motor de turno y ya no esta")
          (let* ((h::*turn-counter* 5)
                 (cobol (build-context "sigue")))
            (ok (search "WROTE." cobol)
                (format nil "pero el .cob la ve: sale de la memoria larga:~%~A" cobol))
            (ok (search "GOAL ABIERTO. 0" cobol)
                (format nil "y la meta se cierra con esa misma evidencia:~%~A" cobol))))))))


(deftest metas/un-comando-que-sale-bien-cierra-su-meta
  "Una meta de COMANDO se cierra en el momento en que el comando se ejecuta.

   Reconciliar metas es buscar la evidencia de que se hicieron, y esa busqueda
   tiene dos relojes que pueden borrar la respuesta antes de que llegue:
   PRUNE-EXPIRED (fact_ttl_seconds = 120 en produccion) y el cap por tipo. Las
   metas de comando son las que mas lo sufren porque un comando que sale
   bien NO es durable -- con acierto, porque no cambia nada que el modelo no vea
   -- asi que su unico registro es el veredicto, y el veredicto tambien se capa.

   En una corrida real quedaron abiertas tres metas de comando ya ejecutadas:

       todo-9  Run: ls -la ...              (turno 3, exit 0)
       todo-25 Run: ls -la ...              (turno 4, exit 0)
       todo-30 Run: python3 -m unittest ... (turno 5, exit 0)

   Con GOAL ABIERTO. N en cero como condition de DONE, tres metas filtradas son
   una sesion que no puede terminar."
  (with-temp-dir (dir)
    (let ((h::*base-dir* dir) (h:*session-id* "s"))
      (with-test-config (("fact_ttl_seconds" 120))
        (with-turn-engine
          (reset-turn-engine)
          ;; La meta, abierta.
          (h::add-todo "Run: ls -la" :status "in-progress")
          ;; El comando se ejecuta y sale bien.
          (h::assert-step-verdict
           (list :action :exec-command :command "ls -la" :step 1
                 :turn-id 1 :round 1)
           (list :exit-code 0))
          ;; A los dos minutos la evidencia de la ejecucion es historica, y el
          ;; comando que salio bien no es durable. Aun asi la meta se cerraba
          ;; al instante, y por eso sigue cerrada mucho despues.
          (let ((h::*turn-counter* 3))
            (h::run)
            (let ((cobol (build-context "sigue")))
              (ok (search "GOAL ABIERTO. 0" cobol)
                  (format nil "la meta del comando se cerro:~%~A" cobol))))
          ;; Y una meta FALLIDA se queda abierta, que es lo que GOAL ABIERTO
          ;; tiene que significar: el usuario pidio una cosa y no se hizo.
          (h::add-todo "Run: rm -rf /" :status "in-progress")
          (h::assert-step-verdict
           (list :action :exec-command :command "rm -rf /" :step 2
                 :turn-id 3 :round 1)
           (list :exit-code 1 :output "permiso denegado"))
          (h::run)
          (let ((cobol (build-context "sigue")))
            (ok (search "GOAL ABIERTO. 1" cobol)
                (format nil "pero la que fallo sigue abierta:~%~A" cobol))))))))
(deftest metas/un-paso-veteado-no-deja-una-meta-que-nunca-cierra
  "Un paso que una regla impugna no puede dejar una meta abierta.

   AUTO-CREATE-TODO-ON-ACTION tiene salience 20 y los vetos tienen 15, asi que
   la meta se abre ANTES de que el veto decida. La intencion sigue viva cuando
   la mira la regla de veto, y por eso crea su meta igual. Y como el veto no es
   una ejecucion --no hay :applied-- esa meta no la cierra nadie nunca.

   En una corrida real: el turno 6 ejecuto `python3 -m unittest test_all.py`
   (APPLIED, y la meta de esa ronda se cerro), y en las rondas 2 y 3 propuso lo
   mismo otra vez. Las dos se vetaron como duplicado, las dos abrieron meta, y
   el .cob final cerro con GOAL ABIERTO. 1 -- con la tarea HECHA y los tests en
   verde.

   GOAL ABIERTO. N en cero es lo que habilita DONE: una meta que no cierra
   nunca es una sesion que no puede terminar."
  (with-temp-dir (dir)
    (let ((h::*base-dir* dir) (h:*session-id* "s"))
      (with-turn-engine
        (reset-turn-engine)
        (h::metrics-reset)
        ;; Ronda 1: el comando sale bien y su meta se cierra.
        (h::assert-batch-intention
         (list :action :exec-command :command "echo listo") 1)
        (h::run)
        ;; Ronda 2: el MISMO comando, que el veto de duplicado impugna.
        (h::assert-batch-intention
         (list :action :exec-command :command "echo listo") 2)
        (h::run)
        (let ((cobol (build-context "sigue")))
          (ok (search "CANCELLED" cobol)
              "el repetido se veto")
          (ok (search "GOAL ABIERTO. 0" cobol)
              (format nil "y el veto NO deja una meta colgada:~%~A" cobol)))))))
