(in-package :cl-harness)

;;; ============================================
;;; Main REPL loop
;;; ============================================

(defparameter *running* nil)
(defparameter *debug-mode* nil
  "When T, emit detailed debug traces for exec-command, context selection, and LLM calls.")
(defparameter *system-prompt-path* nil
  "Explicit path to the system prompt file. NIL = auto-detect.")

(defun system-prompt-path ()
  "System prompt file. Priority: *system-prompt-path* > env
   CL_HARNESS_SYSTEM_PROMPT > system-prompt.md > system-prompt.txt in
   the harness base directory."
  (or *system-prompt-path*
      (let ((env (uiop:getenv "CL_HARNESS_SYSTEM_PROMPT")))
        (if (and env (plusp (length env)))
            (uiop:parse-native-namestring env)
            (let ((base (harness-base-dir)))
              (if (probe-file (merge-pathnames "system-prompt.md" base))
                  (merge-pathnames "system-prompt.md" base)
                  (merge-pathnames "system-prompt.txt" base)))))))

(defun load-system-prompt ()
  "The system prompt, or NIL if there is none.

   NO HAY VALOR POR DEFECTO, y quitarlo fue deliberado. Antes, si no se
   encontraba el fichero, esta funcion devolvia \"You are a helpful coding
   assistant... Help them with their task\". Ese texto era una puerta giratoria:
   decia que el agente servia para cualquier cosa, y el modelo se lo creia. Un
   harness que se autod describe como asistente general deja de ser lo que es
   y se convierte en un opencode con mas pasos.

   Esto no es un opencode. Aqui no hay busqueda web, ni edicion multiarchivo
   autonome, ni nada de eso. Su alcance es estrecho y conviene que lo diga quien
   lo va a usar, no el codigo: el prompt se escribe fuera y se trae con
   CL_HARNESS_SYSTEM_PROMPT, *system-prompt-path*, o system-prompt.md/.txt en el
   directorio base.

   Devolver NIL en vez de una cadena es lo honesto: si el prompt falta, el modelo
   no debe recibir doctrina inventada. Se avisa al arrancar (ver warn-missing-
   system-prompt) y se sigue, porque el harness sin prompt puede ser justo lo que
   alguien quiere depurar."
  (let ((path (system-prompt-path)))
    (when (probe-file path)
      (read-file-contents path))))

(defun warn-missing-system-prompt ()
  "Dice, en el arranque, que no hay system prompt y donde se busca.
   Silencio aqui seria el otro extremo: el usuario creeria que su prompt se
   cargo cuando en realidad nunca se leyo."
  (format t "~&[aviso] No hay system prompt (~A no existe).~%" (system-prompt-path))
  (format t "~&        El modelo NO recibira ninguna doctrina: el alcance de este~%")
  (format t "~&        harness (escribir codigo) lo define quien lo usa.~%")
  (format t "~&        Ponlo en system-prompt.md, o export CL_HARNESS_SYSTEM_PROMPT.~%~%")
  (finish-output))

(defun print-welcome ()
  (format t "~&╔══════════════════════════════════════════╗~%")
  (format t "║  cl-harness — Context Management Harness  ║~%")
  (format t "║  Lisa/Rete + LLM · v0.1.0                 ║~%")
  (format t "╚══════════════════════════════════════════╝~%")
  (format t "~&Session: ~A~%" *session-id*)
  (format t "~&LLM:     ~A (~A)~%" (llm-provider) (llm-model))
  (format t "~&TTL:     ~A sec · Max facts: ~A · Per-type: ~A~%"
          (fact-ttl-seconds) (max-context-facts) (max-facts-per-type))
  (format t "~&Type :help for commands, :quit to exit.~%~%"))

(defun toggle-debug (&optional (new-state nil))
  "Toggle or set debug mode."
  (setf *debug-mode* (if new-state new-state (not *debug-mode*)))
  (format t "~&Debug mode: ~A~%" *debug-mode*))

(defun print-help ()
  (format t "~&Commands:~%")
  (format t "  :help        Show this help~%")
  (format t "  :quit        Exit harness~%")
  (format t "  :debug       Toggle debug mode~%")
  (format t "  :facts       Show active facts in Rete~%")
  (format t "  :rules       Show loaded rules~%")
  (format t "  :save        Save session to disk~%")
  (format t "  :restore ID  Restore a previous session~%")
  (format t "  :exec CMD    Execute a shell command~%")
  (format t "  :read PATH   Read a file~%")
  (format t "  :write PATH  Write text to a file (prompts for content)~%")
  (format t "  :dump        Create SBCL core image~%")
  (format t "  :clear       Clear all facts (Lisa working memory)~%")
  (format t "  :clearall    Full reset: facts + counters + metrics + new session id~%")
  (format t "  :context     Show current YAML context~%")
  (format t "  :metrics     Show per-turn token metrics (Phase 0)~%")
  (format t "  :model NAME  Switch LLM model~%")
  (format t "  :provider P  Switch LLM provider~%")
  (format t "~%"))

(defun handle-command (input)
  "Handle meta-commands (starting with :). Returns T to continue, NIL to quit."
  (let* ((trimmed (string-trim " " input))
         (parts (uiop:split-string trimmed :separator '(#\Space)))
         (cmd (string-downcase (or (first parts) ""))))
    (cond
      ((string= cmd ":quit")
       (save-session)
       (format t "~&Session saved. Goodbye.~%")
       nil)

       ((string= cmd ":help")
        (print-help)
        t)

       ((string= cmd ":debug")
        (toggle-debug)
        t)

      ((string= cmd ":facts")
       (let ((facts (collect-active-facts)))
         (format t "~&Active facts (~A):~%" (length facts))
         (dolist (f facts)
           (format t "  [~A] ~A ts=~A~%"
                   (fact-slot f 'fact-type)
                   (fact-slot f 'data)
                   (fact-slot f 'timestamp))))
       t)

      ((string= cmd ":rules")
       (format t "~&Loaded rules:~%")
       (dolist (r (rules))
         (format t "  ~A~%" (rule-name r)))
       t)

      ((string= cmd ":save")
       (save-session)
       t)

      ((string= cmd ":restore")
       (let ((sid (second parts)))
         (if sid
             (restore-session sid)
             (format t "~&Usage: :restore SESSION-ID~%")))
       t)

       ((string= cmd ":exec")
        (let ((space (position #\Space input)))
          (if space
              (let ((shell-cmd (subseq input (1+ space))))
                (format t "~&> ~A~%" shell-cmd)
                (let ((result (exec-command shell-cmd)))
                  (format t "~A~%" (getf result :output))
                  (format t "(exit: ~A)~%" (getf result :exit-code))))
              (format t "~&Usage: :exec COMMAND~%")))
        t)

      ((string= cmd ":read")
       (let ((path (second parts)))
         (if path
             (let ((result (read-file path)))
               ;; I6: el fallo se imprime con su NOMBRE (:reason), no colado
               ;; dentro de :contents.
               (if (getf result :applied)
                   (format t "~A~%" (getf result :contents))
                   (format t "~A~%" (getf result :reason))))
             (format t "~&Usage: :read FILEPATH~%")))
       t)

      ((string= cmd ":write")
       (let ((path (second parts)))
         (if path
             (progn
               (format t "~&Enter content (end with ~A on its own line):~%" "\\.")
               (let ((lines '()))
                 (loop for line = (read-line *standard-input* nil nil)
                       while (and line (not (string= (string-trim '(#\Space #\Tab) line) ".")))
                       do (push line lines))
                 (let ((content (format nil "~{~A~^~%~}" (nreverse lines))))
                   (write-file path content)
                   (format t "~&Written ~A bytes to ~A~%" (length content) path))))
             (format t "~&Usage: :write FILEPATH~%")))
       t)

      ((string= cmd ":dump")
       (create-session-dump)
       t)

      ((string= cmd ":clear")
       (reset-turn-engine)
       (format t "~&Turn working memory cleared (rules kept).~%")
       t)

      ((or (string= cmd ":clearall")
           (string= cmd ":clear-all"))
       (clear-all)
       t)

      ((string= cmd ":context")
       (format t "~A" (build-context ""))
       t)

      ((string= cmd ":metrics")
       (metrics-summary)
       t)

      ((string= cmd ":model")
       (let ((model (second parts)))
         (if model
             (progn
               (setf (gethash "llm_model" *config*) model)
               (format t "~&Model set to: ~A~%" model))
             (format t "~&Usage: :model MODEL-NAME~%")))
       t)

      ((string= cmd ":provider")
       (let ((prov (second parts)))
         (if prov
             (progn
               (setf (gethash "llm_provider" *config*) prov)
               (format t "~&Provider set to: ~A~%" prov))
             (format t "~&Usage: :provider PROVIDER~%")))
       t)

      (t
       (format t "~&Unknown command: ~A~%" cmd)
       t))))



(defun distinct-action-lines (lines)
  "The non-NIL texts in LINES, in order, without repeats.

   LINES is a list of (ORDER-KEY . TEXT) pairs; this returns just the texts,
   chronologically ordered, each one once.

   MAPCAN y no LOOP a proposito, y con LIST alrededor del par.

   En CL las palabras clave de LOOP --collect, do, return...-- se reconocen en
   cualquier punto de la clausula, esten donde esten. Escrito como

       (loop for pair in lines
             when (and text (not seen))
               do (setf (gethash text seen) t)
               collect pair)

   el COLLECT no queda dentro del WHEN: se ejecuta en todas las vueltas, y el
   filtro no filtra nada. Por eso los textos NIL de los hechos que no son
   acciones --batch-plan, verdict, intention, plan-done, user-input-- llegaban
   hasta la frase impresa. Un cond que devuelve nil y un filtro que no filtra
   son el mismo bug por dos sitios.

   Y el (LIST PAIR) es por lo mismo: MAPCAN splica lo que le devuelvas como si
   fuera una LISTA, y (cons clave . texto) es un par punteado. Sin el LIST,
   con un solo elemento superviviente, MAPCAN devuelve el par tal cual y la
   lista entera se convierte en una lista de un elemento que es un par."
  (let ((seen (make-hash-table :test 'equal)))
    (labels ((keep (pair)
               (let ((text (cdr pair)))
                 (when (and text (not (gethash text seen)))
                   (setf (gethash text seen) t)
                   ;; El TEXTO, no el par: esto es lo que se imprime. El par
                   ;; entero ((ts . id) . "wrote x") es lo que hay que
                   ;; descartar, no lo que hay que mostrar.
                   (list text)))))
      (mapcan #'keep lines))))

(defun turn-action-summary (turn)
  "A short factual account of what the batch actually did in TURN, derived from
   the facts it produced.

   Stands in for the model's own prose when the batch left its 'response' field
   empty. The model can execute a whole batch and say nothing; recorded as-is
   the turn became an empty string, and the next turn then saw itself as having
   said nothing at all.

   Esta frase es lo que el usuario lee como respuesta Y lo que los turnos
   siguientes leen como 'lo que dije yo antes'. Por eso tiene que estar BIEN
   ESCRITA: un resumen mal formado no es un detalle estetico, es el estado del
   turno contaminationdo de basura que el modelo se lleva al siguiente.

   Tres defectos corregidos aqui, los tres vistos en una corrida real:

     - El separador era ~{~A~^, ~A~}. El ~^ abre una clausula que se ejecuta
       solo cuando NO es el ultimo elemento, y dentro de ella sobraba un ~A:
       imprimia un elemento EXTRA en cada vuelta. Con dos lineas salia
       'read a.lispedited b.lisp', con los finales pegados. Es ~^, ~} a secas.

     - El cond terminaba en (t nil), asi que todo hecho que no fuera una de las
       cuatro acciones --user-input, llm-response, batch-plan, verdict,
       intention, plan-done-- aportaba un NIL a la lista, y el texto empezaba
       por la palabra NIL. Ahora se filtra con REMOVE-NIL.

     - Las lineas se ordenaban con STRING<, o sea ALFABETICAMENTE: por eso una
       edicion y una lectura salian siempre en el mismo orden y la cronologia
       se perdia. Se ordenan por orden de hechos, que es lo que un lector
       espera: primero se leyo, despues se escribio."
  (let ((lines (loop for f in (collect-active-facts)
                    for d = (fact-data-of f)
                    when (eql (data-get d :turn-id) turn)
                      collect
                      (cons (fact-order-key f)
                            (cond ((string= (fact-type-of f) "file-read")
                                   (format nil "read ~A" (path-basename (or (data-get d :path) ""))))
                                  ((string= (fact-type-of f) "file-write")
                                   (if (data-get d :refused)
                                       (format nil "refused to overwrite ~A" (path-basename (or (data-get d :path) "")))
                                       (format nil "wrote ~A (~A bytes)" (path-basename (or (data-get d :path) "")) (data-get d :bytes))))
                                  ((string= (fact-type-of f) "file-edit")
                                   (format nil "~A ~A~@[ (~A)~]"
                                           (if (data-get d :applied) "edited" "FAILED to edit")
                                           (path-basename (or (data-get d :path) ""))
                                           (data-get d :reason)))
                                  ((string= (fact-type-of f) "command-exec")
                                   (format nil "ran `~A` (exit ~A)" (or (data-get d :command) "") (or (data-get d :exit-code) "?")))
                                  ;; Sin rama (t nil): los hechos que no son
                                  ;; acciones NO aportan linea. Con (t nil)
                                  ;; metian un NIL que salia impreso.
                                  (t nil))))))
    (when lines
      (format nil "[Actions] ~{~A~^, ~}."
              (distinct-action-lines
               (sort lines #'fact-order-before-p :key #'car))))))

(defun raw-batch-blob-p (text)
  "True when TEXT is a raw ToolUse batch rather than something said to the user.

   Structural test via the batch JSON parser -- deliberately NOT a substring
   search for \"tool_calls\"/\"read_file\"/\"write_file\", which matched any
   answer that merely mentioned a tool name and threw away real prose."
  (multiple-value-bind (payload error) (parse-batch-json text)
    (and (null error)
         (hash-table-p payload)
         (batch-sequence-p (gethash "tool_calls" payload)))))

(defun conversational-response-p (response turn)
  "The text to remember RESPONSE as the model's turn TURN statement.

   Three cases, none of which may be stored raw:
   - a raw batch: machinery, not conversation. Feeding it back is how the
     model used to re-read its own tool calls as if they were a prior claim.
   - an empty string: the batch ran but said nothing.
   - anything else: the model's own words, kept as written.
   The first two fall back to what really happened, so the turn is never
   remembered as silence or as a JSON envelope."
  (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) (or response "")))
        (summary (turn-action-summary turn)))
    (cond ((zerop (length trimmed)) (or summary ""))
          ((raw-batch-blob-p trimmed)
           ;; Never fall back to the blob: storing it is precisely the
           ;; self-poisoning this replaces. With no facts to describe, say so.
           (or summary "[Actions] batch executed, nothing recorded"))
          (t trimmed))))

(defun abort-reason-text (aborted &optional detail)
  "El motivo de un aborto del bucle de rondas, como lo lee el modelo.

   UN SOLO SITIO. El texto vivia duplicado en el mensaje a stdout y en el
   :reason del hecho batch-abort, y los dos tenian que contar lo mismo: el
   modelo lee el segundo y el operador el primero, asi que una discrepancia
   entre ellos no se nota como discrepancia, se nota como un harness que no
   sabe por que para.

   DETAIL es lo que el detector de bucle ya sabe y no se puede inventar aqui:
   el fichero que se relee y cuantas veces."
  (case aborted
    (:batch-repeated "el modelo repitio el mismo lote")
    (:iterations "se agoto batch_max_iterations")
    (:tool-loop
     (or detail "el modelo se quedo en un bucle de llamadas y el harness lo corto"))
    (:estancado
     "el modelo lleva dos rondas seguidas sin aplicar nada: vuelve a proponer pasos que el harness ya veto, y ningun veto nuevo va a aparecer. Lo hecho hasta aqui esta hecho; lo que no se ha hecho, dilo en el siguiente turno.")
    (t "error del proveedor")))

(defun process-turn (user-message system-prompt)
  "Single turn: build context → call LLM → register response.
   In batch mode, it runs Rete to execute intentions and calls LLM again."
  (setf *loop-alerted-turn* nil)
  (incf *turn-counter*)
  ;; La ronda del LLM se reinicia con el turno. La primera respuesta es la
  ;; ronda 1; el bucle de abajo la sube antes de cada llamada siguiente, para
  ;; que cada lote del mismo turno tenga su propia identidad de paso.
  (setf *batch-round* 1)
  ;; UNWIND-PROTECT, no un setf al final. El cuerpo de un turno hace trabajo de
  ;; verdad a mitad -- run, las reglas de Rete, build-context, save-session -- y
  ;; cualquiera de esos puede morir. Con el reinicio al final, una excepcion se
  ;; saltaba por encima y *batch-round* se quedaba en la ultima ronda del turno
  ;; que murio. Como es global y plano, el valor se lo quedaba el turno
  ;; siguiente, y ahi si hacia dano: la ronda se graba DENTRO del hecho
  ;; (:round de assert-batch-plan) y forma parte de la clave del JOIN
  ;; (turn-id, round, step). Un plan de la ronda 1 sellado con round=3 ya no
  ;; encuentra su veredicto, y el fallo se ve tres turnos mas tarde, cuando el
  ;; modelo pregunta por que el paso 1 no lo tiene.
  ;;
  ;; El (setf *batch-round* 1) de ARRIBA no sobra: son dos defensas distintas.
  ;; Esta da entrada a CUALQUIER turno, esta da salida a ESTE turno.
  (unwind-protect
    (let* ((turn *turn-counter*)
           (batch-iterations 1)
           (naive (progn
                    (assert (harness-fact (fact-type "user-input")
                              (timestamp (get-universal-time))
                              (data (list :text user-message
                                          :turn-id turn))))
                    (naive-context-string)))
           (context (build-context user-message))
           (streamed (llm-stream-p))
           (response (handler-case
                         (progn
                           (setf *last-llm-usage* nil)
                           (setf *llm-call-log* nil)
                           (setf *llm-response-model* nil)
                           (setf *last-llm-call-info* nil)
                           (when streamed
                             (format *standard-output* "~&assistant>~%")
                             (force-output *standard-output*))
                           (call-llm system-prompt context user-message))
                       (error (e) (format nil "[LLM ERROR] ~A" e)))))
    
      ;; --- EJECUCIÓN CONTROLADA POR RETE ---
      (when (and (string-equal (llm-provider) "batch")
                 response
                 (not (search "[LLM ERROR]" response :test #'char=)))
         (let ((max-rounds (or (batch-max-iterations) most-positive-fixnum))
               (aborted nil)
               ;; RONDAS SEGUIDAS SIN APLICAR NADA. El tope de iteraciones dice
               ;; cuanto se puede durar; esto dice si se esta avanzando.
               ;;
               ;; Un turno que se pasa entero proponiendo lo mismo que el harness
               ;; ya veto no se distingue de uno que esta trabajando, y los dos
               ;; cerraban por 'se agoto batch_max_iterations': el mismo motivo
               ;; para un turno que hizo su trabajo y para uno que solo quemo
               ;; llamadas. En una corrida real el turno 2, en las rondas R2 a R8,
               ;; propuso el MISMO READ-FILE y las siete salieron CANCELLED --
               ;; siete llamadas al proveedor para no aplicar nada.
               ;;
               ;; El umbral es DOS rondas seguidas. Una ronda entera cancelada
               ;; puede ser legitima: el modelo repite un paso para ver si el
               ;; harness se lo entrega. Dos ya no son una comprobacion, son una
               ;; insistencia, y a la segunda el veto ya se ha dicho una vez.
               (stalled-rounds 0)
               ;; LO QUE DIJO EL DETECTOR DE BUCLE, si fue el que corto. Vive
               ;; fuera del bucle porque se consulta en cada ronda y solo una
               ;; de ellas dispara.
               (loop-warning nil))
           (loop for i from 1
                 while (and response
                            (< i max-rounds)
                            (not aborted)
                            (not (search "[LLM ERROR]" response :test #'char=))
                            (not (plan-done-p)))
                 do
                 ;; SIN DETECCION DE REPETICION A NIVEL DE TURNO. Cada ronda se
                 ;; ejecuta, siempre.
                 ;;
                 ;; Todo en informatica es batch: si el programador pide la misma
                 ;; instruccion tres veces, el compilador la ejecuta tres veces.
                 ;; Repetirse no es un error de compilacion, y un modelo que
                 ;; repite esta ANCLADO -- no procesando el resultado --, que es
                 ;; distinto de un modelo que pide lo mismo.
                 ;;
                 ;; Lo que hay que darle al que se quedo anclado no es muerte, es
                 ;; diagnostico: "ya lo hiciste, aqui esta el resultado". Y eso lo
                 ;; dan las reglas de Rete, que impugnan la INTENCION
                 ;; intraturno y contestan con un veredicto -- PREVENT-DUPLICATE-
                 ;; READ reentrega el contenido, PREVENT-DUPLICATE-EDIT cancela.
                 ;;
                 ;; El abortador de aqui era un SEGUNDO mecanismo por encima de
                 ;; ellas, y era el que ganaba: mataba el turno, eso disparaba
                 ;; CANCEL-INTENTIONS-ON-ABORT, y con las intenciones retractadas
                 ;; la respuesta que Rete tenía preparada se perdia tambien. El
                 ;; modelo se quedaba con "[Actions] ." sin ninguna idea de por
                 ;; que. Medido: 29 targetas propuestas en 5 turnos, 5 ejecutadas
                 ;; (todas en el turno 1, el unico anterior al primer aborto) y
                 ;; 24 sin respuesta.
                 ;;
                 ;; LO QUE SI SE CORTE es el bucle sin salida, y con un motivo
                 ;; PROPIO. Un veto es una respuesta: el paso queda terminal y el
                 ;; modelo ve por que. Repetir el veto no es proponer mas trabajo,
                 ;; es no hacer caso, y ahi lo que se corta es la ronda, no el
                 ;; trabajo que ya esta hecho.
                 (format t "~&[DEBUG process-turn] Disparando (run) - Ronda ~A...~%" i)
                 (detect-blind-writes)
                 (run)
                 (multiple-value-bind (applied cancelled)
                     (round-verdict-counts turn *batch-round*)
                   (setf stalled-rounds
                         (if (and (zerop applied) (plusp cancelled))
                             (1+ stalled-rounds)
                             0))
                   ;; EL DETECTOR DE BUCLE, y por que estaba muerto aqui.
                   ;;
                   ;; TOOL-LOOP-WARNING estaba conectado a CALL-LLM-WITH-TOOLS y a
                   ;; STREAM-LLM-WITH-TOOLS, los dos del camino IMPERATIVO. El
                   ;; proveedor batch --el que usa la configuracion por defecto y
                   ;; el de la corrida real-- no lo consultaba nunca. Un detector
                   ;; que solo mira un camino no ve el bucle del otro: en la
                   ;; corrida real el turno 2 leyo el mismo fichero 7 veces y
                   ;; DETECT-READ-LOOP, que existe y esta probado, no dio una
                   ;; sola vez.
                   ;;
                   ;; Se consulta antes que el estancamiento porque su mensaje es
                   ;; MEJOR: nombra el fichero y cuenta las veces. El
                   ;; estancamiento es el corte de seguridad para cuando no hay
                   ;; ninguna huella que enseñar.
                   (let ((warning (tool-loop-warning)))
                     (cond (warning
                            (setf aborted :tool-loop)
                            (setf loop-warning warning)
                            (setf response warning))
                           ((>= stalled-rounds 2)
                            (setf aborted :estancado)))))
                 ;; Y SI NO HAY UNA LLAMADA QUE HACER, no se paga. Cuando el
                 ;; corte es por estancamiento ya se sabe que la ronda siguiente
                 ;; seria otra propuesta igual; gastar la llamada para obtenerla
                 ;; seria el mismo gasto que se intenta evitar, con la respuesta
                 ;; encima.
                 (unless aborted
                   (let ((new-context (build-context user-message)))
                     (incf batch-iterations)
                     ;; La siguiente respuesta del LLM es una ronda nueva
                     ;; del MISMO turno. Sus pasos vuelven a numerarse
                     ;; desde 1, y sin subir la ronda el JOIN plan<->veredicto
                     ;; cruzaria los lotes.
                     (setf *batch-round* (1+ i))
                     (setf response
                           (handler-case
                               (call-llm system-prompt new-context user-message)
                             (error (e)
                               (setf aborted :llm-error)
                               (format nil "[LLM ERROR] ~A" e)))))))
          (when (and (not aborted)
                     response
                     (not (search "[LLM ERROR]" response :test #'char=))
                     (not (plan-done-p)))
          ;; The loop only ends early on those three conditions, so reaching
          ;; here with a pending batch means batch_max_iterations ran out.
          (setf aborted :iterations))
      (when aborted
        ;; NOTE: no "~:" directives anywhere in this message. In this Lisp
        ;; "~:P"/"~:A" do NOT consume an argument, so the FORMAT swallowed the
        ;; wrong value and the configured cap was never printed (it said
        ;; "maximo 2" with batch_max_iterations=8).
        (format t "~&[HARNESS BATCH ABORT] ~A tras ~D ronda(s) (maximo ~D)~%"
                    (abort-reason-text aborted loop-warning)
                    batch-iterations
                    max-rounds)
            (setf *last-llm-call-info*
                  (list :iterations batch-iterations :path aborted))
            ;; El aborto tiene que SER UN HECHO, no solo un mensaje a stdout.
            ;;
            ;; CANCEL-INTENTIONS-ON-ABORT ya existe y borra las intenciones
            ;; pendientes cuando ve un batch-abort: sin este aserto, un aborto
            ;; del bucle no la dispara nunca, y las intenciones se quedan ahi
            ;; para siempre sin ejecutarse.
            ;;
            ;; Y el motivo importa mas que el hecho. Sin el, el turno siguiente
            ;; veia los pasos del turno abortado como PENDING con 'puede
            ;; haberse podado', que no es lo que paso: el harness dejo de
            ;; preguntar a proposito. El modelo no podia distinguir 'se podo'
            ;; de 'el turno murio' y no tenia forma de decidir si reintentaba.
            (assert (harness-fact (fact-type "batch-abort")
                      (timestamp (get-universal-time))
                      (data (list :reason (abort-reason-text aborted loop-warning)
                                  :path aborted
                                  :iterations batch-iterations
                                  :turn-id turn
                                  :round (current-batch-round)
                                  ;; Los pasos de ESTE turno que se quedan sin
                                  ;; veredicto, con la MISMA identidad que usa el
                                  ;; JOIN de la tarjeta. Sin esta lista el abort
                                  ;; solo podria decir 'algo se aborto', y el
                                  ;; paso concreto que murio seguiria sin motivo.
                                  :steps (loop for f in (collect-active-facts)
                                               when (and (string= (fact-type-of f) "batch-plan")
                                                         (eql (data-get (fact-data-of f) :turn-id) turn))
                                                 collect (plan-step-identity (fact-data-of f))))))))
          ;; Un turno batch que SALE BIEN no pasaba por ningun sitio que
          ;; escribiera *LAST-LLM-CALL-INFO*: solo se rellenaba al abortar. Y el
          ;; abort es el caso raro, asi que :LLM-PATH llegaba a las metricas como
          ;; NIL en la mayoria de los turnos -- no "batch-repeated", no error,
          ;; nada. El campo que existe para responder "como termino este turno"
          ;; no contestaba nada. Un metodo necesita decir como ACABO, no solo
          ;; como fallo.
          (unless *last-llm-call-info*
            (setf *last-llm-call-info*
                  (list :iterations batch-iterations
                        ;; :PLAN-DONE es lo que de verdad ocurrio: el modelo
                        ;; propuso el cierre y el harness lo acepto porque no
                        ;; quedaban goals abiertos (PROTOCOLO I5).
                        :path (if (plan-done-p) :plan-done :final))))))
      ;; -------------------------------

      ;; --- POLÍTICA DE EPOCHS AUTOMÁTICA ---
      (when (and (> *turn-counter* 0)
                 (zerop (mod *turn-counter* 5)))
        (create-epoch (format nil "Consolidated context up to turn ~A" *turn-counter*) *turn-counter*)
        (when *debug-mode*
          (format t "~&[DEBUG process-turn] Epoch creado en turno ~A. Limpiando hechos viejos.~%" *turn-counter*)))
      ;; --------------------------------------

      (record-context-metrics context user-message
                              :naive-str naive
                              :real-prompt (getf *last-llm-usage* :prompt)
                              :real-completion (getf *last-llm-usage* :completion)
                              :llm-iterations batch-iterations
                              :llm-path (getf *last-llm-call-info* :path))
    
      ;; FIX AUTO-ENVENENAMIENTO: la respuesta del modelo nunca se guarda cruda.
      ;; Un lote de tool_calls es maquinaria, no conversación, y un texto vacío
      ;; dejaba el turno registrado como silencio aunque el lote hubiera hecho
      ;; trabajo real. En ambos casos se registra lo que efectivamente pasó.
      (when (and response (not (search "[LLM ERROR]" response :test #'char=)))
        (assert (harness-fact (fact-type "llm-response")
                  (timestamp (get-universal-time))
                  (data (list :text (conversational-response-p response turn)
                              :turn-id turn
                              :parent-id turn)))))
    
      (when *debug-mode*
        (format t "~&[DEBUG process-turn] turn=~A user=~A context-len=~A streamed=~A~%"
                turn user-message (length context) *llm-streamed*)
        (when (and *llm-streamed* response)
          (format t "~&[DEBUG process-turn] response-len=~A~%" (length response))))
      ;; PROMOVER ANTES DE VOLCAR, no al reves. SAVE-SESSION escribe
      ;; dumps/longterm-mem.lisp con lo que haya en *mem-engine*, asi que
      ;; guardar antes de promover deja la memoria durable en disco un turno
      ;; POR DETRAS de los hechos del turno que acaba de cerrar. Con una
      ;; sesion larga la deriva se acumula turno a turno; con run-one-shot, que
      ;; es un solo turno, se pierde el 100% de la promocion: el proceso escribe
      ;; la memoria antes de generarla.
      ;;
      ;; El orden correcto es promover (que ademas corre las reglas del mem
      ;; engine y sus caps) y LUEGO volcar, que es cuando la memoria durable ya
      ;; esta completa. El segundo SAVE-SESSION de run-one-shot se queda, porque
      ;; ese volcado ocurre con el engine ya poblado.
      (promote-durable-facts)
      (save-session)
      response)
    ;; El cleanup del unwind-protect de arriba: baja la ronda tanto si el turno
    ;; llego aqui como si se fue por una excepcion a mitad.
    (setf *batch-round* 1)))



(defun batch-content-hash (calls response)
  "FNV-1a de 64 bits sobre la serialización canónica de CALLS y RESPONSE.
   Nada de SXHASH aquí: en SBCL 2.4.1 `(sxhash '((:PATH \"a.lisp\")))` y
   `(sxhash '((:PATH \"b.lisp\")))` devuelven el MISMO entero, así que el
   detector de bucles veía una repetición fantasma entre lotes que sólo
   difieren en el valor de un argumento."
  (let ((hash 14695981039346656037))
    (with-standard-io-syntax
      (loop for char across (prin1-to-string (list calls response))
            do (setf hash
                    (logand #xFFFFFFFFFFFFFFFF
                            (* (logxor hash (char-code char)) 1099511628211)))))
    hash))


(defun canonical-batch-args (calls)
  "CALLS with every :path and :command argument reduced to the form the harness
   will actually ACT on, so two spellings of the same work share a fingerprint.

   Por que hace falta: en la corrida real el mismo par de lecturas se pidio en
   la ronda 5 como 'math_utils.py' y en la 7 como '/home/mtk/.../math_utils.py'.
   Son el mismo trabajo -- RESOLVE-PATH las lleva al mismo fichero -- pero con
   el texto crudo dan huellas distintas, y el detector de repeticiones no las
   reconoce como lo que son. Un detector que se puede esquivar cambiando '..' por
   '/' no es un detector.

   Solo para la HUELLA. La intencion sigue guardando lo que el modelo escribio,
   porque eso es lo que se renderiza en la tarjeta del plan y lo que el modelo
   tiene que poder leer."
  (mapcar (lambda (call)
            ;; Se RECONSTRUYE la plist par por par en vez de (REMOVE :PATH ...):
            ;; REMOVE quita la clave y deja su VALOR colgando, y una plist con
            ;; un valor sin clave sigue siendo una plist valida -- solo lleva un
            ;; dato mas que se cuela en la huella. Era justo lo que hacia que
            ;; 'math_utils.py' y '/home/mtk/x/math_utils.py' dieran huellas
            ;; distintas: las dos se resolvian al mismo fichero y las dos
            ;; arrastraban el camino original.
            (loop for (k v) on call by #'cddr
                  append (cond ((eq k :path)
                                 (list k (namestring (resolve-path v))))
                                ((eq k :command)
                                 (list k (strip-cd-prefix v)))
                                (t (list k v)))))
          calls))

(defun batch-fingerprint (response)
  "A stable identity for the batch a response asks for: ONLY the tool_calls
   array, normalised through the same parser the validator uses, so whitespace
   and key ordering do not matter. Two responses with the same fingerprint ask
   the harness to do exactly the same thing again, which is the signature of a
   model stuck in a loop.

   El texto libre de 'response' se SACO del hash, y estaba a proposito por
   error. Solo cambia lo que se DICE, no lo que se PIDE: un lote que pide
   read(a) y read(b) es el mismo lote digase lo que digase el comentario, y un
   modelo atascado cambia la prosa en cada intento igual que cambia el orden de
   las llamadas. Con la prosa dentro, dos lotes identicos daban huellas
   distintas y la deteccion de repeticion --que solo existe para esto-- no
   veia nada. En la corrida real eso costo cuatro rondas: el modelo pidio
   read(math_utils.py) + read(test_math_utils.py) en la ronda 5 y en la 6, y
   como la prosa cambio, ninguna sevio de la otra.

   Si dos lotes piden lo mismo y solo difieren en el texto, el bucle es real
   aunque el modelo parezca estar pensando distinto: por eso la prosa no
   cuenta. Contar el texto era medir la redaccion del modelo, que es
   justamente lo que un bucle cambia para no ser detectado."
  (let ((json (batch-json-text response)))
    (handler-case
        (let ((payload (com.inuoe.jzon:parse json)))
          (if (hash-table-p payload)
              (multiple-value-bind (calls error)
                  (normalize-batch-tool-calls (gethash "tool_calls" payload))
                (batch-content-hash
                 (if error (gethash "tool_calls" payload) (canonical-batch-args calls))
                 ""))
              (batch-content-hash json "")))
      (error () (batch-content-hash response "")))))


(defun plan-done-p (&optional (turn *turn-counter*))
  "Retorna T si Rete dice que el batch del turno actual terminó."
  (some (lambda (f)
          (eql (data-get (fact-slot f 'data) :turn-id) turn))
        (mapcar #'first
                (retrieve (?f) (?f (harness-fact
                                    (fact-type "plan-done")))))))




(defun run-one-shot (user-message &key (reset-engine t) (debug nil))
  "Non-interactive mode (opencode-run style): process a single turn and exit.
   RESET-ENGINE T starts from an empty working memory (fresh process via
   run.sh); NIL continues the session already living in a saved core image.
   DEBUG T activa los logs de debug para la corrida."
  (load-config)
  (when debug
    (setf *debug-mode* t)
    (format t "~&[INFO] Debug mode activado.~%"))
  (when reset-engine
    ;; Fresh process: reset both engines and reload persisted long-term memory.
    (boot-memory)
    (reset-turn-counter)
    (new-session-id))
  (metrics-reset)
  (let ((system-prompt (load-system-prompt)))
    (unless system-prompt
      (warn-missing-system-prompt))
    (let ((response (or (process-turn user-message system-prompt)
                        "[LLM ERROR] empty response")))
    (unless *llm-streamed*
      (format t "~&assistant> ~A~%" response))
    (save-session)
    (finish-output)
    (let ((path (getf *last-llm-call-info* :path))
          (failed (or (search "[LLM ERROR]" response :test #'char=)
                      (search "[LLM API ERROR]" response :test #'char=)
                      (search "[LLM JSON ERROR]" response :test #'char=)
                      (search "[LLM PARCIAL]" response :test #'char=)
                      (search "[HARNESS LOOP DETECTOR]" response :test #'char=))))
      (sb-ext:exit :code (if (or failed
                                 (member path
                                         '(:tool-loop :api-error :json-error
                                                      :transport-error
                                                      :exhaustion-partial
                                                      :exhaustion-error)))
                             1
                             0))))))

(defun start-harness (&optional (reset-engine t))
  "Start the interactive REPL loop.
   When RESET-ENGINE is NIL the current Rete state is kept (used when
   booting from a saved SBCL core image)."
  (load-config)
  (when reset-engine
    (boot-memory)
    ;; Fresh engines also start from turn 0. On an image resume
    ;; (RESET-ENGINE NIL) the counters captured in the image are kept so
    ;; turn/todo/epoch ids stay monotonic across reboots.
    (reset-turn-counter)
    (new-session-id))
  (metrics-reset)
  (print-welcome)
  (setf *running* t)
  (let ((system-prompt (load-system-prompt)))
    (unless system-prompt
      (warn-missing-system-prompt))
    (loop while *running*
          do (format t "~&user> ")
             (force-output *standard-output*)
             (let ((input (read-line *standard-input* nil nil)))
               (cond
                 ((null input)
                  (setf *running* nil))
                 ((string= (string-trim " " input) "")
                  nil)
                 ((char= (char input 0) #\:)
                  (setf *running* (handle-command input)))
                 (t
                  (let ((response (process-turn input system-prompt)))
                    (unless *llm-streamed*
                      (format t "~&~%assistant> ~A~%~%" response))))))))
  (format t "~&Harness stopped.~%"))

(defun stop-harness ()
  "Stop the harness."
  (setf *running* nil))

(defun clear-all ()
  "Full factory reset: wipe BOTH Lisa engines (turn working memory + persisted
   long-term memory), reset all harness-level state (turn/todo/epoch counters,
   metrics), and start a fresh session with a new session id. Rules and config
   are kept. Used to begin something new on top of a binary that may carry
   state from a previous run."
  (reset-turn-engine)
  (clear-mem-persistence)
  (reset-turn-counter)
  (setf *todo-counter* 0)
  (setf *epoch-counter* 0)
  (metrics-reset)
  (setf *session-id* (format nil "session-~A" (get-universal-time)))
  (format t "~&Session reset. New session: ~A~%" *session-id*)
  t)
