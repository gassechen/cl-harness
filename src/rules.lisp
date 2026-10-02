(in-package :cl-harness)

;;; ============================================
;;; Rete rules for context pruning
;;;
;;; NOTE: Rules must NOT perform internal retrieve() queries
;;; (that triggers reentrant run-query and macro-expansion issues
;;; in the RHS). Per-type and global caps are enforced by plain
;;; functions called from build-context.
;;;
;;; Two engines: these defrule forms compile into whichever engine is
;;; active at load — the *turn-engine* (working memory). Rules specific to
;;; long-term memory are defined under WITH-MEM-ENGINE at the bottom.
;;; ============================================

;;; --- Type tiers ---
;;; conversation facts (user-input/llm-response) are NOT re-derivable:
;;; they must never expire (only per-type caps bound them).
;;; evidential facts (command-exec/file-read/file-write) ARE re-derivable:
;;; they expire (TTL) and are deduplicated by key (last-write-wins).

(defun conversation-type-p (type)
  (member type '("user-input" "llm-response") :test #'string=))

(defun evidential-type-p (type)
  "Types that are re-derivable evidence and thus subject to TTL + dedup.
   tool-loop facts are derived by the detect-command-loop rule: re-derivable,
   so they expire like any other evidential and dedup last-write-wins."
  (member type '("command-exec" "file-read" "file-write" "tool-loop")
          :test #'string=))

(defun ttl-eligible-p (type)
  (evidential-type-p type))

(defun dedup-eligible-p (type)
  (evidential-type-p type))

(defun data-get (data key)
  "Lookup a plist value by key, tolerating :keyword or bare symbol keys."
  (loop for (k v) on data by #'cddr
        when (and (symbolp k) (string-equal (symbol-name k) (symbol-name key)))
          return v))

(defun dedup-key (type data)
  "Structural key identifying equivalent facts of a type (NIL = no dedup).
   tool-loop facts dedup on their detected family prefix, so overlapping
   rule matches collapse into a single alert per family."
  (cond ((string= type "command-exec") (data-get data :command))
        ((string= type "tool-loop") (data-get data :family))
        ((member type '("file-read" "file-write") :test #'string=)
         (data-get data :path))
        (t nil)))

(defun dedup-conflict-p (type ta tb da db)
  "TRUE when the OLDER fact (ta) is genuinely made redundant by the NEWER (tb).
   - If a command's output or exit-code changed, both are kept (reflects state mutation).
   - If a file's content changed between reads, both are kept.
   - Only truly identical repetitions are deduplicated."
  (and (dedup-eligible-p type)
       (< ta tb)
       (let ((ka (dedup-key type da))
             (kb (dedup-key type db)))
         (when (and ka (equal ka kb))
           (cond
             ((string= type "command-exec")
              (and (equal (data-get da :exit-code) (data-get db :exit-code))
                   (equal (data-get da :output) (data-get db :output))))
             ((string= type "file-read")
              (equal (data-get da :contents) (data-get db :contents)))
             ((string= type "file-write")
              t)
             ((string= type "tool-loop")
              ;; A fresh loop alert supersedes any older alert on the same family.
              t)
             (t nil))))))

(defun command-failed-p (type data)
  "TRUE when a command-exec fact has a non-zero exit code."
  (and (string= type "command-exec")
       (let ((code (data-get data :exit-code)))
         (and code (not (zerop code))))))

(defun real-error-p (type data)
  "A command-exec is a REAL active error when it failed to launch, or failed
   WITH evidence in its combined output. Benign probes (exit != 0 with no
   output, e.g. `cat missing-file 2>/dev/null` with stderr suppressed, or a
   timed-out command) are NOT conserved."
  (and (command-failed-p type data)
       (not (data-get data :timed-out))
       (or (data-get data :launch-failed)
           (let ((out (string-trim '(#\Space #\Newline)
                                   (or (data-get data :output) ""))))
             (plusp (length out))))))

(defun fact-expired-p (type ts data)
  "Check whether an evidential fact has expired.
   1. Conversation facts never expire.
   2. Commands that are REAL errors (see real-error-p) never expire via TTL
      (active error context), when conserve_errors is enabled.
   3. Logical turn distance: expires if created more than (fact-ttl-turns) turns ago.
   4. Wall-clock TTL: if fact-ttl-seconds is non-zero, expires if older than that window."
  (and (ttl-eligible-p type)
       ;; Real errors are preserved as active error context (if configured)
       (not (and (conserve-errors-p)
                 (real-error-p type data)))
       (or ;; Logical turn expiration
           (let ((turn (or (data-get data :turn-id) 0))
                 (max-turns (fact-ttl-turns)))
             (and (plusp *turn-counter*)
                  max-turns
                  (> (- *turn-counter* turn) max-turns)))
           ;; Generous wall-clock expiration (if configured and non-zero)
           (let ((ttl (fact-ttl-seconds)))
             (and ttl
                  (plusp ttl)
                  (< ts (- (get-universal-time) ttl)))))))

;;; --- Temporal pruning ---
(defrule prune-expired (:salience -50)
  (?f (harness-fact (fact-type ?ftype) (timestamp ?ts) (data ?d)))
  (test (fact-expired-p ?ftype ?ts ?d))
  =>
  (retract ?f))

;;; --- Deduplication (last-write-wins) ---
(defrule dedup-keep-newest ()
  (?newer (harness-fact (fact-type ?t) (timestamp ?tb) (data ?db)))
  (?older (harness-fact (fact-type ?t) (timestamp ?ta) (data ?da)))
  (test (dedup-conflict-p ?t ?ta ?tb ?da ?db))
  =>
  (retract ?older))

;;; --- Epoch Compaction (inspired by OpenCode session_context_epoch) ---
;;; Evidential facts older than an active epoch's baseline-seq are retracted,
;;; as the epoch baseline summary consolidates their state.
(defrule prune-superseded-by-epoch (:salience -40)
  (?epoch (context-epoch (baseline-seq ?bseq)))
  (?f (harness-fact (fact-type ?ftype) (data ?d)))
  (test (and (evidential-type-p ?ftype)
             (let ((turn (or (data-get ?d :turn-id) 0)))
               (< turn ?bseq))))
  =>
  (retract ?f))

;;; ============================================================
;;; Imperative pruning helpers (called from build-context,
;;; NOT from rules, to avoid reentrant run-query)
;;; ============================================================

(defun fact-timestamp (f)
  (get-slot-value f 'timestamp))

(defun retract-oldest-of-type (type keep &optional protect)
  "Retract all but the newest KEEP facts of the given type, preserving real errors.

   PROTECT es un predicado opcional sobre el hecho: lo que devuelve T no se
   retira nunca, por mucho que se pase de KEEP. Lo usa el veredicto, cuya
   tarjeta puede seguir en pantalla: si el veredicto se va, la tarjeta se
   renderiza con STATE = PENDING y el contexto afirma que un paso YA EJECUTADO
   no se ha hecho. Antes las tarjetas y los veredictos se capaban por separado
   y, al no ser el mismo conjunto, se desalineaban solos: los dos topes van
   sobre poblaciones de tamano distinto y cada una se queda con SU ventana. No
   hace falta que lleguen a tocarse para que pase: un veredicto de mas es
   justo lo que empuja fuera de la ventana el de una tarjeta viva. Es
   preferible pasarse de KEEP a mentir.

   Que se PUEDA desalinear es un hecho; que ocurriera en la corrida que motivo
   a mirar esto, no se puede saber. Los volcados son compatibles tanto con un
   veredicto evictado como con un paso que nunca llego a ejecutarse, y en ese
   segundo caso el PENDING era honesto. Lo que si se demuestra aqui, con hechos
   crudos, es el mecanismo.

   El orden es el total (timestamp . id), no la marca de tiempo pelada: dos
   hechos del mismo segundo se retiraban en orden arbitrario, de modo que la
   poda no era reproducible ni entre ejecuciones ni dentro de la misma."
  (let* ((result (retrieve (?f) (?f (harness-fact))))
         (facts (remove-if-not (lambda (f)
                                 (string= (get-slot-value f 'fact-type) type))
                               (mapcar #'first result)))
         ;; If it's a failed command, protect real errors from blunt per-type
         ;; retraction (launch failures or failures with evidence in output).
         (prunable (if (and (string= type "command-exec") (conserve-errors-p))
                       (remove-if (lambda (f)
                                    (real-error-p type (fact-data-of f)))
                                  facts)
                       facts))
         (sorted (remove-if (lambda (f) (and protect (funcall protect f)))
                            (sort (remove nil prunable) #'fact-order-before-p
                                  :key #'fact-order-key)))
         (excess (- (length sorted) keep)))
    (when (plusp excess)
      (loop for f in sorted
            for i from 0
            when (< i excess)
            do (retract f)))))

;;; ============================================================
;;; Command-loop detection
;;; ============================================================
;;; Weak models get stuck re-issuing the SAME failing command in slightly
;;; different quoting/escaping inside a single turn, burning all tool
;;; iterations. We detect it two ways, sharing one set of predicates:
;;;
;;;   1. Rete rule detect-command-loop: three same-turn FAILED command-exec
;;;      facts whose normalized forms share a common prefix assert a derived
;;;      `tool-loop` fact (deduped per family, expires via TTL). The context
;;;      builder renders it as a top-level warnings block.
;;;   2. Imperative guard in the turn loop (llm.lisp): after each tool batch,
;;;      tool-loop-warning runs (run) — so both loop rules fire on FRESH
;;;      evidence — then aborts the turn with an explicit instruction to
;;;      change approach whenever a tool-loop fact exists for the current turn.
;;;
;;; Comparison is pure ANSI (STRING-DOWNCASE, REMOVE-IF, MISMATCH) — no
;;; external string library is needed to detect "the same attempt retried".

(defparameter *loop-prefix-min-chars* 20
  "Absolute minimum shared-prefix length before two commands count as the
   same retried attempt (normalized commands).")

(defparameter *loop-prefix-ratio* 0.6
  "Minimum shared-prefix length as a fraction of the SHORTER normalized
   command, so short commands are still comparable.")

(defparameter *loop-min-count* 3
  "How many failing same-family commands in one turn trigger a loop warning.")

(defparameter *read-loop-min-count* 4
  "How many times a file must be touched in one turn (file-read fact, or a
   command whose text mentions the file) before the READ-loop warning fires.
   Deliberately higher than command loops: importing/launching a service
   legitimately probes the same module several times (uvicorn app:app imports
   are expected on each retry), while a stuck model re-issues failing commands
   with no output. Genuine file-read fact triples still abort via the
   detect-read-triple RULE (which needs 3 actual reads) regardless of this
   threshold.")

(defparameter *loop-alerted-turn* nil
  "Turn number for which the loop warning was already issued (alert once per turn).")

(defun normalize-command (cmd)
  "Normalize a command for similarity comparison: lowercase, drop quote
   characters, trim and collapse whitespace runs to single spaces. Pure ANSI."
  (let* ((s (string-downcase (or cmd "")))
         (s (remove-if (lambda (ch) (find ch "\"'`" :test #'char=)) s))
         (s (string-trim '(#\Space #\Tab #\Newline #\Return) s)))
    (let ((out (make-string-output-stream))
          (prev-space t))
      (loop for ch across s
            do (if (or (char= ch #\Space) (char= ch #\Tab)
                       (char= ch #\Newline) (char= ch #\Return))
                   (unless prev-space
                     (write-char #\Space out)
                     (setf prev-space t))
                   (progn (write-char ch out) (setf prev-space nil))))
      (get-output-stream-string out))))

(defun common-prefix-len (a b)
  "Length of the longest common PREFIX shared by strings A and B (ANSI MISMATCH)."
  (let ((n (mismatch a b)))
    (if (null n) (min (length a) (length b)) n)))

(defun loop-similar-p (a b)
  "Two normalized commands are the same attempt retried when their common
   prefix is long in absolute terms AND relative to the shorter command."
  (let ((shared (common-prefix-len a b))
        (shortest (min (length a) (length b))))
    (and (>= shared *loop-prefix-min-chars*)
         (>= shared (* *loop-prefix-ratio* shortest)))))

(defun common-prefix-of-strings (strs)
  "The prefix shared by ALL strings in STRS (string)."
  (if (null strs)
      ""
      (let ((seed (first strs))
            (n (length (first strs))))
        (dolist (s (rest strs))
          (setf n (min n (common-prefix-len seed s))))
        (subseq seed 0 n))))

(defun loop-worthy-failure-p (type data)
  "A command-exec that FAILED and is NOT a mere timeout — a candidate for the
   loop detector (timeouts may be legitimate re-probes)."
  (and (string= type "command-exec")
       (command-failed-p type data)
       (not (data-get data :timed-out))))

(defun commands-form-loop-p (cmds)
  "TRUE when the first three command strings are pairwise the same retried
   attempt (shared common prefix against the first normalized command)."
  (let ((norm (mapcar #'normalize-command (remove-if-not #'stringp cmds))))
    (and (>= (length norm) 3)
         (destructuring-bind (a b c) norm
           (and (loop-similar-p a b) (loop-similar-p a c))))))

(defun tool-loop-pattern-p (tsa tsb tsc da db dc)
  "Rete TEST for detect-command-loop: three failed command-exec facts, strictly
   increasing in time, from the SAME turn, forming a command loop."
  (and (< tsa tsb) (< tsb tsc)
       (let ((ta (data-get da :turn-id))
             (tb (data-get db :turn-id))
             (tc (data-get dc :turn-id)))
         (and ta tb tc (eql ta tb) (eql tb tc)))
       (loop-worthy-failure-p "command-exec" da)
       (loop-worthy-failure-p "command-exec" db)
       (loop-worthy-failure-p "command-exec" dc)
       (commands-form-loop-p (list (data-get da :command)
                                   (data-get db :command)
                                   (data-get dc :command)))))

(defun tool-loop-family-of (da db dc)
  "Shared normalized prefix of the three command facts (the family key)."
  (common-prefix-of-strings
   (mapcar #'normalize-command
           (list (data-get da :command)
                 (data-get db :command)
                 (data-get dc :command)))))

(defrule detect-command-loop (:salience 10)
  (?a (harness-fact (fact-type "command-exec") (timestamp ?tsa) (data ?da)))
  (?b (harness-fact (fact-type "command-exec") (timestamp ?tsb) (data ?db)))
  (?c (harness-fact (fact-type "command-exec") (timestamp ?tsc) (data ?dc)))
  (test (tool-loop-pattern-p ?tsa ?tsb ?tsc ?da ?db ?dc))
  =>
  (assert (harness-fact
           (fact-type "tool-loop")
           (timestamp (get-universal-time))
           (data (list :family (tool-loop-family-of ?da ?db ?dc)
                       :commands (list (data-get ?da :command)
                                       (data-get ?db :command)
                                       (data-get ?dc :command))
                       :count 3
                       :turn-id (data-get ?da :turn-id))))))

(defun read-triple-p (da db dc)
  "Rete TEST for detect-read-triple: three file-read facts from the SAME turn
   reading the SAME file basename. Distinctness of the fact objects is checked
   by a separate (test ...) using EQ on ?a ?b ?c."
  (let ((pa (or (data-get da :path) ""))
        (pb (or (data-get db :path) ""))
        (pc (or (data-get dc :path) "")))
    (and (plusp (length pa)) (plusp (length pb)) (plusp (length pc))
         (let ((ta (data-get da :turn-id))
               (tb (data-get db :turn-id))
               (tc (data-get dc :turn-id)))
           (and ta tb tc (eql ta tb) (eql tb tc)))
         (string= (path-basename pa) (path-basename pb))
         (string= (path-basename pb) (path-basename pc)))))

(defrule detect-read-triple (:salience 10)
  (?a (harness-fact (fact-type "file-read") (data ?da)))
  (?b (harness-fact (fact-type "file-read") (data ?db)))
  (?c (harness-fact (fact-type "file-read") (data ?dc)))
  (test (and (not (eq ?a ?b)) (not (eq ?a ?c)) (not (eq ?b ?c))))
  (test (read-triple-p ?da ?db ?dc))
  =>
  (assert (harness-fact
           (fact-type "tool-loop")
           (timestamp (get-universal-time))
           (data (list :kind "read-loop"
                       :path (or (data-get ?da :path) "")
                       :count 3
                       :turn-id (data-get ?da :turn-id))))))


(defrule prevent-duplicate-read (:salience 15)
  "Si el LLM pide leer un archivo que ya leyó en este turno, se le devuelve el
   contenido que YA TENEMOS en vez de un no.

   Se aserta el CANCELADO antes de retractar. Antes retractaba y no escribía
   nada: el paso se quedaba sin veredicto, la tarjeta caía al fallback
   PENDING y el REASON culpaba a una poda que no había pasado. El paso
   tenía que llegar a un estado terminal para que PENDING significara una sola
   cosa.

   Y el CANCELADO lleva el :CONTENTS del file-read que lo motivo. Un no sin
   contenido es un callejon sin salida: en una corrida real el modelo pidio
   cinco veces el mismo fichero, recibio cinco veces 'ya se leyo en este turno',
   y las cinco rondas se gastaron en un rechazo que no le advancingaba nada --
   con el fichero entero en la memoria del harness. Elmotivo del veto esta a
   la vista en el patron (?fr-data), asi que devolverlo es lo unico que
   convierte el rechazo en respuesta."
  (?intent (harness-fact (fact-type "intention") (data ?d)))
  (test (eql (data-get ?d :action) :read-file))
  (?fr-fact (harness-fact (fact-type "file-read") (data ?fr-data)))
  (test (and (eql (data-get ?fr-data :turn-id) (data-get ?d :turn-id))
             (string= (path-basename (data-get ?fr-data :path))
                      (path-basename (data-get ?d :path)))))
  (test (not (stale-read-p ?fr-fact)))
  =>
  (assert-step-cancelled
   ?d
   "CANCELADO y REENTREGADO: el archivo ya se leyó en este turno y su contenido está más abajo. No hace falta volver a pedirlo."
   (data-get ?fr-data :contents))
  (retract ?intent))

(defrule prevent-duplicate-read-after-write (:salience 16)
  "El mismo veto, pero cuando el fichero SE ESCRIBIO despues de la lectura.

   Sale en la regla de al lado y no como un (TEST (NOT ...)) en esta porque el
   veto tiene que ocurrir SIEMPRE: lo unico que cambia es si ademas entrega el
   contenido. Con un solo patron mas un (TEST ...) se tendria que decidir el
   motivo antes de firrar el paso, y Rete no puede.

   Sin este caso, escribir un fichero y releerlo para comprobarlo daba un
   'CANCELADO' con el texto ANTERIOR. El modelo se creia que tenia el fichero
   actualizado cuando lo que tenia era la version a la que acababa de
   sustituir -- el peor de los dos fallos, porque para que el modelo se entere
   hay queDSMANTELAR algo que parece una entrega.

   Y EL MOTIVO NO INVITA A REPETIR. Este es el unico veto de los tres que NO
   reentrega contenido, y por eso es el unico que puede empujar al modelo a un
   bucle: si le dices 'vuelve a pedirlo', lo que le has dicho es que pedirlo es
   lo que tiene que hacer. En una corrida real el turno 2 propuso
   READ-FILE math_utils.py en las rondas R2 a R8 -- siete rondas, siete
   llamadas al proveedor -- y las siete salieron CANCELLED con este mismo
   texto, byte a byte. El modelo estaba obedeciendo.

   El veto no se puede satisfacer dentro del turno: cualquier edit vuelve
   rancio para siempre cualquier lectura posterior de ese fichero, asi que la
   unica salida es dejar de pedirlo. El motivo dice eso, y no lo contrario.

   Y el bucle, ademas, ya no es la unica red de seguridad: DETECTAR-ESTANCAMIENTO
   en el bucle de rondas corta el turno cuando dos rondas seguidas no aplican
   nada. Aca se dice la verdad del paso; alla se paga el coste de no hacer
   caso."
  (?intent (harness-fact (fact-type "intention") (data ?d)))
  (test (eql (data-get ?d :action) :read-file))
  (?fr-fact (harness-fact (fact-type "file-read") (data ?fr-data)))
  (test (and (eql (data-get ?fr-data :turn-id) (data-get ?d :turn-id))
             (string= (path-basename (data-get ?fr-data :path))
                      (path-basename (data-get ?d :path)))))
  (test (stale-read-p ?fr-fact))
  =>
  (assert-step-cancelled
   ?d
   "CANCELADO, y el contenido NO se reentrega: el archivo se modificó en este turno después de leerlo, así que lo que se leyó ya está rancio. NO LO PIDAS OTRA VEZ en este turno: no hay forma de que se te entregue, porque la última modificación es la que lo vuelve rancio. Si necesitas el texto actual, termina el turno y dilo en el siguiente."
   nil)
  (retract ?intent))



(defun detect-tool-loop ()
  "Scan the CURRENT turn's command-exec facts for a repeating failing family.
   Returns three values (FAMILY COUNT COMMANDS) when at least *loop-min-count*
   failing commands share the same retried form, else NIL."
  (let* ((turn (current-turn-id))
         (cells (loop for f in (mapcar #'first
                                       (retrieve (?f) (?f (harness-fact))))
                      for d = (fact-data-of f)
                      for cmd = (data-get d :command)
                      when (and (string= (fact-type-of f) "command-exec")
                                (eql (data-get d :turn-id) turn)
                                (loop-worthy-failure-p "command-exec" d)
                                (stringp cmd))
                        collect (cons (normalize-command cmd) d))))
    (when (>= (length cells) *loop-min-count*)
      (let* ((seed (caar cells))
             (in-family (remove-if-not (lambda (cell)
                                         (loop-similar-p seed (car cell)))
                                       cells)))
        (when (>= (length in-family) *loop-min-count*)
          (values (common-prefix-of-strings (mapcar #'car in-family))
                  (length in-family)
                  (mapcar (lambda (cell) (data-get (cdr cell) :command))
                          in-family)))))))

(defun content-probe-command-p (command)
  "TRUE when COMMAND is a content-RETURNING probe (cat/grep/sed/head/tail/...,
   or an explicit read_file-style open().read()) rather than an execution,
   compile or test-run of the file (py_compile, pytest, running a script).
   Only content probes *re-expose the same file content* to the model, so only
   they count as re-reads for the loop detector. Compiling or running a file
   is legitimate verification, not a read — counting it would abort healthy
   explore-then-fix turns (observed in the field)."
  (let ((cmd (string-downcase (or command ""))))
    (or (cl-ppcre:scan "\\b(cat|grep|sed|head|tail|awk|more|less|wc|cut|tr|find)\\b"
                       cmd)
        (cl-ppcre:scan "\\bread_file\\b" cmd)
        (cl-ppcre:scan "open\\s*\\(" cmd))))

(defun path-basename (p)
  "Last path component of P, so a file-read fact recorded with its ABSOLUTE
   resolved path merges with shell probes that mention only the basename."
  (let* ((s (string-downcase (or p "")))
         (sep (position #\/ s :from-end t)))
    (if sep (subseq s (1+ sep)) s)))

(defun stale-read-p (fr-fact)
  "T si el fichero de FR-FACT se ha ESCRITO en este turno despues de leerlo.

   FR-FACT es el hecho file-read, no su plist: el orden hace falta y el orden
   vive en el hecho.

   El veto de PREVENT-DUPLICATE-READ casa cualquier file-read del turno con
   cualquier peticion del mismo basename. Sin este guardia, un modelo que
   escribe un fichero y despues lo relee para verificar recibe un 'ya leido' con
   el contenido ANTERIOR: el harness leeria en voz alta el texto que el acaba de
   sustituir, con la seguridad de una entrega.

   Y contenido rancio es peor que no entregar nada: el modelo deja de pedir la
   lectura porque ya cree que la tiene, y sigue trabajando sobre una version
   que no existe en disco. Por eso la cancelacion solo reentrega cuando la
   lectura sigue siendo verdad.

   El orden sale de FACT-ORDER-KEY, no del reloj: todos los hechos de una ronda
   se asertan dentro del mismo segundo, asi que get-universal-time no ordena
   nada, y asumir 'lo ultimo que toque el fichero manda' haria que un
   'escribir, leer, releer' -- el orden mas normal que hay-- se tomara por
   lectura rancia y no reentregara nada. El contador de insercion de Rete si
   ordena, y es lo que se usa."
  (let* ((base (path-basename (data-get (fact-data-of fr-fact) :path)))
         (read-at (fact-order-key fr-fact))
         (newer-writes
           (loop for f in (mapcar #'first (retrieve (?f) (?f (harness-fact))))
                 for d = (fact-data-of f)
                 for type = (fact-type-of f)
                 when (and (member type '("file-write" "file-edit") :test #'string=)
                           (string= (path-basename (data-get d :path)) base)
                           (fact-order-before-p read-at (fact-order-key f)))
                   collect f)))
    (if newer-writes
        t
        nil)))

(defun written-unread-p (path)
  "T si lo ULTIMO que toco PATH en este turno fue el propio modelo ESCRIBIENDOLO,
y desde entonces nadie lo ha leido.

   Es el hermano de STALE-READ-P, y cubre el caso que ahi no aparece: una lectura
   de un fichero que el modelo acaba de escribir y que nunca ha leido. Las otras
   dos reglas necesitan un hecho file-read para casar, asi que ahi no hay nada
   que casar y la peticion pasa: el modelo escribe el fichero y tres rondas
   despues lo vuelve a pedir.

   La corrida real que lo motivo: el turno escribio interval_audit.py y
   test_interval_audit.py, corrio los tests --que no tocan ficheros-- y en las
   rondas 5, 6, 7 y 8 los pidio de nuevo a los dos. Cuatro rondas de ida y vuelta
   sobre bytes que el modelo acababa de emitir, sin que nada entre medias
   hubiera cambiado el disco. Con el pool de modelos al 50% --37 segundos por
   llamada-- fueron cuatro llamadas enteras.

   Y aqui NO se reentrega el contenido, a diferencia de PREVENT-DUPLICATE-READ, y
   es deliberado por una razon de tokens: el texto lo produjo el propio modelo en
   el turno, esta en el contexto de origen --los argumentos de su write_file--, y
   reenviarlo se lo mandaria DOS VECES. La ida y vuelta no cuesta el contenido,
   cuesta la ronda.

   Ojo al caso legitimo que este veto NO puede tocar: escribir un fichero y
   releerlo para comprobar que quedo bien. Aqui no hay nada que comprobar porque
   el harness ejecuto la escritura y tiene el exit code; la escritura no puede
   fallar a medias. Si el fichero lo cambio OTRO actor --un exec-command, un
   editor, otro proceso-- eso no es un hecho file-write de este turno, asi que la
   funcion devuelve NIL y la lectura pasa."
  (let* ((base (path-basename path))
         (touching (loop for f in (mapcar #'first (retrieve (?f) (?f (harness-fact))))
                         for d = (fact-data-of f)
                         for type = (fact-type-of f)
                         when (and (member type '("file-write" "file-edit" "file-read")
                                             :test #'string=)
                                   (string= (path-basename (data-get d :path)) base))
                           collect (cons type f)))
(applied-writes (loop for entry in touching
                               when (and (string= (car entry) "file-write")
                                         (data-get (fact-data-of (cdr entry)) :applied))
                                  collect (cdr entry)))
         (newer-reads (loop for entry in touching
                             when (and (string= (car entry) "file-read")
                                       (loop for w in applied-writes
                                             thereis (fact-order-before-p
                                                      (fact-order-key (cdr entry))
                                                      (fact-order-key w))))
                               collect (cdr entry))))
    (and applied-writes (null newer-reads) t)))

(defrule prevent-redundant-read-after-write (:salience 17)
  "El modelo pide leer un fichero que EL MISMO escribio en este turno y que desde
   entonces nadie ha leido. Es una ida y vuelta vacia: los bytes ya estan en su
   contexto de origen.

   Salience 17: por encima de 16 para ganarle a PREVENT-DUPLICATE-READ-AFTER-WRITE
   cuando las dos casan, y por debajo de 20 para no adelantarse a la creacion de
   metas --si dispara antes, el veto deja una meta colgada--. Cuando ambas pueden
   disparar --hubo una lectura ANTES de la escritura-- gana esta, y gana bien: la
   otra cancela sin reentregar porque el texto que tiene es rancio, y esta
   cancela sin reentregar porque el texto que tiene es NUEVO y esta en la llamada
   del propio modelo. La misma decision, mejor informada."
  (?intent (harness-fact (fact-type "intention") (data ?d)))
  (test (eql (data-get ?d :action) :read-file))
  (test (written-unread-p (data-get ?d :path)))
  =>
  (assert-step-cancelled
   ?d
   "CANCELADO, y NO se reenvia el contenido: TU escribiste ese fichero en este turno, asi que el texto esta en tu propia llamada a write_file, no se pierde al cancelar. Nada lo ha tocado desde entonces. Si lo que quieres es otra cosa --un fragmento, una comprobacion-- dilo en el texto de tu respuesta en vez de volver a pedir el fichero entero, que reread no lo va a hacer mas barato.")
  (retract ?intent))

(defun detect-read-loop ()
  "Scan the CURRENT turn for repeated *reads/probes* of the SAME file:
   - file-read facts (keyed by basename of the resolved path),
   - command-exec strings that mention the same file basename, and
   - lecturas CANCELADAS (verdicts :cancelled sobre :read-file).
   Returns (values PATH COUNT) when a file has been touched >= *read-loop-min-count*
   times. Re-reading the same thing over and over signals the model is stuck
   verifying instead of acting (the observed failure mode of weak models).

   Las CANCELADAS son el caso que hacia inutilizable al contador. Una lectura
   cancelada nunca llega a READ-FILE, asi que no deja hecho file-read y este
   contador --que antes solo miraba file-read-- no la veia nunca. En una
   corrida real el modelo pidio el mismo fichero cinco veces, las cinco
   canceladas, y el contador se quedo en 1 de 4: el bucle entero era invisible
   para el detector que existe precisamente para verlo. Un contador de bucles
   que no cuenta el bucle."
  (let* ((turn (current-turn-id))
         (counts (make-hash-table :test #'equal)))
    (dolist (f (mapcar #'first (retrieve (?f) (?f (harness-fact)))))
      (let* ((d (fact-data-of f))
             (type (fact-type-of f))
             (this-turn (and (eql (data-get d :turn-id) turn) turn))
             (path (data-get d :path)))
        (when this-turn
          (cond
            ((and (string= type "file-read") path (stringp path)
                  (plusp (length path)))
             (incf (gethash (path-basename path) counts 0)))
            ;; Una lectura CANCELADA tambien es una lectura pedida. Se cuenta
            ;; por su :target, que es la ruta que el modelo pidio, no la que
            ;; acabo resuelta el veto.
            ((and (string= type "verdict")
                  (eql (data-get d :action) :read-file)
                  (eql (data-get d :verdict) :cancelled)
                  (stringp (data-get d :target))
                  (plusp (length (data-get d :target))))
             (incf (gethash (path-basename (data-get d :target)) counts 0)))
            ((and (string= type "command-exec")
                  (let ((cmd (data-get d :command)))
                    (and (stringp cmd) (plusp (length cmd)))))
             (dolist (file (mentioned-files-in-command (data-get d :command)))
               (incf (gethash (path-basename file) counts 0))))))))
    (let ((best-path nil)
          (best-count 0))
      (maphash (lambda (p n)
                 (when (> n best-count)
                   (setf best-path p best-count n)))
               counts)
      (and (>= best-count *read-loop-min-count*)
           (values best-path best-count)))))

(defun mentioned-files-in-command (command)
  "Extract likely file paths (tokens ending in a code extension) from a shell
   command, normalized, so probe loops via grep/sed/awk/head can be detected.
   Purely numeric dotted tokens (IPs like 0.0.0.0, versions like 2.4.1) are
   NOT files and are ignored."
  (let ((seen '()))
    (when (stringp command)
      (dolist (m (cl-ppcre:all-matches-as-strings
                  "[A-Za-z0-9_./~-]+\\.[A-Za-z0-9]{1,5}" (string-downcase command)))
        (let ((clean (string-right-trim ";&|" m)))
          (when (and clean (plusp (length clean))
                     (not (cl-ppcre:scan "^[0-9.]+$" clean))
                     (not (member clean '("2" "1" "2>&1" "dev" "null" "sh" "-c")
                                  :test #'string=)))
            (pushnew clean seen :test #'string=)))))
    seen))

(defun turn-loop-facts (turn)
  "tool-loop facts the turn engine DERIVED for the CURRENT turn (declarative
   output of detect-command-loop / detect-read-triple rules)."
  (remove-if-not (lambda (f)
                   (eql (data-get (fact-data-of f) :turn-id) turn))
                 (loop for f in (mapcar #'first (retrieve (?f) (?f (harness-fact))))
                       when (string= (fact-type-of f) "tool-loop")
                         collect f)))

(defun read-loop-warning-text (path reads)
  (format nil
          "[HARNESS LOOP DETECTOR] You have touched the same file ~A times this turn: ~S (read_file or shell probes). Its content is ALREADY in your context — STOP re-reading/re-probing full variants. If you need a specific section, do ONE targeted read (read_file or exec_command grep/sed on that path) and then ACT on the task: make the fix and verify.~%"
          reads path))

(defun command-loop-warning-text (family count commands)
  (format nil
          "[HARNESS LOOP DETECTOR] You have issued ~A failing commands in this turn that are all the SAME attempt (family ~S):~%~{~A~%~}STOP re-running variants of this command — the repeated failure is not progressing. Pause, read the actual error message and the relevant file, fix the ROOT cause (often quoting/escaping), then verify once with a single command.~%"
          count family commands))

(defun tool-loop-warning ()
  "If the current turn is stuck in a command loop OR keeps re-reading/re-probing
   the same file, return a loud instruction the model must follow; else NIL.
   Alerts once per turn.

   Runs (run) first so the declarative loop rules fire against the turn's
   fresh facts; their derived tool-loop facts then drive the warning. The two
   imperative detectors remain as a fallback for cases the rules cannot see
   (mixed file-read + shell-probe re-reads)."
  (run)
  (let* ((turn (current-turn-id))
         (already (eql *loop-alerted-turn* turn)))
    (cond
      (already nil)
      ;; 1. Declarative: a loop rule derived a fresh tool-loop fact for this turn.
      ((plusp (length (turn-loop-facts turn)))
       (setf *loop-alerted-turn* turn)
       (let ((facts (sort (turn-loop-facts turn) #'> :key #'fact-timestamp-of))
             (commands '())
             (family nil))
         (dolist (l facts)
           (let* ((d (fact-data-of l))
                  (kind (or (data-get d :kind) "command-loop")))
             (cond ((string= kind "read-loop")
                    (let ((path (data-get d :path))
                          (count (or (data-get d :count) 3)))
                      (when (null family)
                        (return-from tool-loop-warning
                          (read-loop-warning-text path count)))))
                   (t
                    (pushnew (data-get d :family) family :test #'string=)
                    (dolist (c (data-get d :commands))
                      (pushnew c commands :test #'string=))))))
         (command-loop-warning-text
          (first (remove-if-not #'stringp family))
          (length commands)
          (nreverse commands))))
      ;; 2. Imperative fallback (mixed probes the rules cannot count).
      (t
       (multiple-value-bind (path reads) (detect-read-loop)
         (multiple-value-bind (fam count cmds) (detect-tool-loop)
           (cond
             ((and path (>= reads *read-loop-min-count*))
              (setf *loop-alerted-turn* turn)
              (read-loop-warning-text path reads))
             ((and fam (>= count *loop-min-count*))
              (setf *loop-alerted-turn* turn)
              (command-loop-warning-text fam count cmds))
             (t nil))))))))
        
;;; ============================================================
;;; Long-term memory rules (compiled into the *mem-engine*)
;;; ============================================================
;;; Deduplicate durable knowledge last-write-wins, so each promoted fact key
;;; (command, written path) converges to its newest version.
(with-mem-engine
  (defrule mem-dedup-durable ()
    (?newer (harness-fact (fact-type ?t) (timestamp ?tb) (data ?db)))
    (?older (harness-fact (fact-type ?t) (timestamp ?ta) (data ?da)))
    (test (and (dedup-conflict-p ?t ?ta ?tb ?da ?db)
               (or (string= ?t "command-exec")
                   (string= ?t "file-write"))))
    =>
    (retract ?older)))




;;; ============================================================
;;; Intention Execution Rules (The Batch Processor)
;;; ============================================================
;;; El LLM ya no llama herramientas por el protocolo HTTP.
;;; Entrega un batch de intenciones en texto, el parser las aserta aquí,
;;; y estas reglas dispara las funciones POSIX originales.
;;; Las funciones POSIX (exec-command, read-file, etc.) ya asertan
;;; los hechos "command-exec", "file-read", etc., sobre los cuales
;;; tus reglas de pruning y loop detection actúan automáticamente.

(defun record-intention-tool-call (name result)
  "Anota la ejecución de NAME (una cadena de tool) en las métricas por turno.

   El hueco que tapa esto: RECORD-TOOL-CALL solo se llamaba desde
   EXECUTE-TOOL, que es el camino de las tools NATIVAS. En modo batch las
   herramientas las ejecutan estas reglas, que no pasaban por ahí — así que
   *metrics-tool-calls* quedaba vacío y la tabla chars_raw / chars_sent /
   saved% de :metrics salía en blanco. Justo en el modo donde el trabajo real
   lo hace Rete, que era donde más hacía falta mirar.

   El texto que se mide es el que el modelo leería de esa herramienta, montado
   con la misma forma que EXECUTE-TOOL usa. No se recupera de los hechos a
   propósito: el hecho guarda el CONTENIDO crudo, que es otra medida (cuánto
   se retiene en memoria), no la que comparan las dos columnas de la tabla
   (cuánto se reinyecta al modelo).

   Un plist de acción sin los campos esperados no rompe nada: se registra lo
   que haya. Perder una línea de métrica es mejor que perder la ejecución."
  (when (listp result)
    ;; LET* y no LET: TEXT se monta con PATH y CMD, asi que necesita verlos ya
    ;; bindeados. En un LET paralelo se leerian como variables libres.
    (let* ((path (or (getf result :path) "-"))
          (cmd (or (getf result :command) "-"))
          (text
            (cond
              ((getf result :applied)
               (cond ((string= name "read-file")
                      (format nil "File: ~A~%Contents:~%~A"
                              path (getf result :contents)))
                     ((string= name "write-file")
                      (format nil "Wrote ~A bytes to ~A"
                              (getf result :bytes) path))
                     ((string= name "edit-file")
                      (format nil "Edited ~A: replaced ~A chars with ~A chars (~A match(es) found)"
                              path (getf result :replaced-chars)
                              (getf result :new-chars) (getf result :matches)))
                     (t (format nil "~A ok" name))))
              ((getf result :reason)
               (format nil "~A failed: ~A" (string-upcase name) (getf result :reason)))
              ((getf result :exit-code)
               (format nil "Command: ~A~%Exit code: ~A~%Output:~%~A"
                       cmd (getf result :exit-code) (or (getf result :output) "")))
              (t (format nil "~A" name)))))
      (record-tool-call name text (truncate-tool-result text)))))

(defrule execute-intention-read (:salience 5)
  (?f (harness-fact (fact-type "intention") (data ?d)))
  (test (eq (data-get ?d :action) :read-file))
  (test (eql (data-get ?d :status) :pending))
  =>
  ;; Llama a tu función original. Ella misma hará el (assert (harness-fact "file-read" ...))
  (let ((res (read-file (data-get ?d :path))))
    (record-intention-tool-call "read-file" res)
    (assert-step-verdict ?d res))
  ;; Retraemos la intención para que no se ejecute de nuevo en el próximo (run)
  (retract ?f))

(defrule execute-intention-write (:salience 4)
  (?f (harness-fact (fact-type "intention") (data ?d)))
  (test (eq (data-get ?d :action) :write-file))
  (test (eql (data-get ?d :status) :pending))
  =>
  (let ((res (write-file (data-get ?d :path) (data-get ?d :content))))
    (record-intention-tool-call "write-file" res)
    (assert-step-verdict ?d res))
  (retract ?f))

(defrule execute-intention-edit (:salience 4)
  (?f (harness-fact (fact-type "intention") (data ?d)))
  (test (eql (data-get ?d :action) :edit-file))
  (test (eql (data-get ?d :status) :pending))
  =>
  ;; PROTOCOLO I2: aquí NO se pre-valida old_string. La regla duplicaba un guard
  ;; que la ACCIÓN ya tiene, y lo hacía peor: cuando lo detectaba, sólo logueaba
  ;; y retractaba sin asertar nada, así que el paso desaparecía del mundo y el
  ;; modelo no veía ni que se intentó. Delegando en EDIT-FILE hay UN solo sitio
  ;; donde vive la regla, y todo edit rechazado queda igual de registrado:
  ;; (file-edit :applied nil :reason ...) más el VERDICT con su :step.
  (let ((res (edit-file (data-get ?d :path)
                        (or (data-get ?d :old-string) "")
                        (or (data-get ?d :new-string) ""))))
    (record-intention-tool-call "edit-file" res)
    (assert-step-verdict ?d res))
  (retract ?f))



(defrule execute-intention-command (:salience 3)
  (?f (harness-fact (fact-type "intention") (data ?d)))
  (test (eql (data-get ?d :action) :exec-command))
  (test (eql (data-get ?d :status) :pending))
  =>
  (format t "~&[DEBUG rule] Ejecutando comando en Rete...~%") ;; <--- ESTA LÍNEA
  (let ((res (exec-command (data-get ?d :command))))
    (record-intention-tool-call "exec-command" res)
    (assert-step-verdict ?d res))
  (retract ?f))



(defun detect-blind-writes ()
  "Returns T if a write_file/edit_file intention lacks a prior read in this turn."
  (let ((turn (current-turn-id)))
    (let ((read-paths
            (loop for f in (mapcar #'first (retrieve (?f) (?f (harness-fact))))
                  for d = (fact-data-of f)
                  for type = (fact-type-of f)
                  when (eql (data-get d :turn-id) turn)
                    nconc (cond
                            ((and (string= type "file-read")
                                  (stringp (data-get d :path)))
                             (list (path-basename (data-get d :path))))
                            ((and (string= type "intention")
                                  (eql (data-get d :action) :read-file)
                                  (stringp (data-get d :path)))
                             (list (path-basename (data-get d :path))))
                            (t nil)))))
      (loop for f in (mapcar #'first (retrieve (?f) (?f (harness-fact))))
            for d = (fact-data-of f)
            for type = (fact-type-of f)
            when (and (string= type "intention")
                         (eql (data-get d :action) :edit-file)
                      (eql (data-get d :turn-id) turn))
              unless (member (path-basename (or (data-get d :path) "")) read-paths :test #'string=)
                do (assert (harness-fact
                             (fact-type "batch-abort")
                             (timestamp (get-universal-time))
                             (data (list :reason "Blind write attempt"
                                         :path (data-get d :path)))))
                   and count t into abort-count
            finally (return (plusp abort-count))))))



;;; Si se detecta un aborto, cancelamos las intenciones restantes DE ESE TURNO
;;;
;;; Y lo de "de ese turno" es TODO el contenido de la regla.
;;;
;;; Esta regla vivia dormida porque no habia hecho batch-abort que|matchear: el
;;; aborto se devolvia como valor y no se asertaba. Al empezar a asertarse (para
;;; que el .cob y las metricas pudieran saber por que se cancelo un paso) la regla
;;; empezo a disparar, y como NO FILTRABA POR TURNO hacia esto:
;;;
;;;   - matchea CUALQUIER batch-abort, incluido el de un turno ya terminado;
;;;   - retracta CUALQUIER intention, incluida la que el modelo acaba de proponer
;;;     en el turno de ahora.
;;;
;;; Con la salencia 20 gana siempre a las reglas de ejecucion (3-5), asi que una
;;; sola vez que un turno aborta, TODAS las intenciones de la sesion se borran en
;;; el siguiente (run). Y como no se aserta veredicto al borrarlas, el paso se
;;; queda en PENDING para siempre: el harness no ejecuta nada, no dice por que, y
;;; el modelo solo puede volver a planificar. Medido: de 29 pasos propuestos en
;;; una sesion de 5 turnos, se ejecutaron 5, todos en el turno 1 -- el unico turno
;;; anterior al primer aborto. Del segundo en adelante, cero.
;;;
;;; Con el filtro por :turn-id, un aborto solo puede cancelar lo suyo.
(defrule cancel-intentions-on-abort (:salience 20) ; Mayor que las de ejecución
  (?abort (harness-fact (fact-type "batch-abort") (data ?ad)))
  (?f (harness-fact (fact-type "intention") (data ?id)))
  (test (eql (data-get ?id :turn-id) (data-get ?ad :turn-id)))
  =>
  (retract ?f))

;;; ============================================================
;;; Emergency Brake (Freno de Emerencia para Batch)
;;; ============================================================

(defrule batch-emergency-brake (:salience 20)
  (?f (harness-fact (fact-type "command-exec") (data ?d)))
  (test (and (eql (data-get ?d :turn-id) (current-turn-id))
             (real-error-p "command-exec" ?d)))
  (?intent (harness-fact (fact-type "intention") (data ?i-data)))
  (test (and (eql (data-get ?i-data :turn-id) (current-turn-id))
             (eql (data-get ?i-data :status) :pending)))
  =>
  (retract ?intent))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; GOALS
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defrule auto-create-todo-on-action (:salience 20)
  (?intent (harness-fact (fact-type "intention") (data ?d)))
  =>
  (let ((action (data-get ?d :action))
        (path (data-get ?d :path))
        (command (data-get ?d :command)))
    (add-todo (cond
                ((eql action :read-file) (format nil "Read ~A" path))
                ((eql action :write-file) (format nil "Write ~A" path))
                ((eql action :edit-file) (format nil "Edit ~A" path))
                ((eql action :exec-command) (format nil "Run: ~A" command))
                (t "Unknown action"))
              :status "in-progress")))




;; (defrule auto-complete-todo-on-read (:salience 10)

;;   (?f (harness-fact (fact-type "file-read") (data ?d)))
;;   (?todo (agent-todo (task ?t))
;;                        (status "in-progress")
;;                        (test (search (data-get ?d :path) ?t)))
;;   =>
;;   (set-todo-status (get-slot-value ?todo 'id) "completed"))


;; (defrule auto-complete-todo-on-write (:salience 10)

;;   (?f (harness-fact (fact-type "file-write") (data ?d)))
;;   (?todo (agent-todo (task ?t))
;;                        (status "in-progress")
;;                        (test (search (data-get ?d :path) ?t)))
;;   =>
;;   (set-todo-status (get-slot-value ?todo 'id) "completed"))


;; (defrule auto-complete-todo-on-edit (:salience 10)

;;   (?f (harness-fact (fact-type "file-edit") (data ?d)))
;;   (?todo (agent-todo (task ?t))
;;                        (status "in-progress")
;;                        (test (search (data-get ?d :path) ?t)))
;;   =>
;;   (set-todo-status (get-slot-value ?todo 'id) "completed"))



;; (defrule auto-complete-todo-on-command (:salience 10)

;;   (?f (harness-fact (fact-type "command-exec") (data ?d)))
;;   (?todo (agent-todo (task ?t))
;;                        (status "in-progress")
;;                        (test (search (data-get ?d :path) ?t)))
;;   =>
;;   (set-todo-status (get-slot-value ?todo 'id) "completed"))

(defrule prevent-duplicate-edit (:salience 15)
  "Si el LLM pide hacer el MISMO edit exacto que ya hizo en este turno, cancela.

   Compara los DOS TEXTOS, no sus longitudes: dos edits del mismo archivo que
   sustituyen 189 caracteres por 262 y por 249 son edits distintos, y cancelar
   el segundo seria perder trabajo de verdad.

   Y solo se mira el edit que SE APLICO. Un edit rechazado --'old_string not
   found'-- es justo el que hay que reintentar con otro enfoque; lo que se
   impugna es el que ya salio bien repetido, que no aporta nada."
  (?intent (harness-fact (fact-type "intention") (data ?d)))
  (test (eql (data-get ?d :action) :edit-file))
  (harness-fact (fact-type "file-edit") (data ?fe-data))
  (test (and (data-get ?fe-data :applied)
             (eql (data-get ?fe-data :turn-id) (data-get ?d :turn-id))
             (string= (path-basename (data-get ?fe-data :path))
                      (path-basename (data-get ?d :path)))
             ;; ACÁ CHEQUEAMOS QUE SEA EXACTAMENTE EL MISMO EDIT:
             (string= (data-get ?fe-data :old-string)
                      (data-get ?d :old-string))
             (string= (data-get ?fe-data :new-string)
                      (data-get ?d :new-string))))
  =>
  (assert-step-cancelled ?d "CANCELADO: el mismo edit ya se aplico en este turno, repetirlo no cambiaria el archivo")
  (retract ?intent))

;;; Las dos que faltaban. Lectura y edicion tienen veto de duplicado desde hace
;;; tiempo; escritura y comando NO. Un modelo que pide la misma escritura o el
;;; mismo comando dos veces no encuentra ninguna regla que le conteste, y se
;;; queda sin respuesta -- que es justo la situacion que la invariante I1
;;; prohibe: toda targeta propuesta tiene que volver con veredicto.
;;;
;;; La respuesta es impugnar la INTENCION intraturno, no matar el turno. Un
;;; compilador al que le pides dos veces la misma instruccion te dice que ya la
;;; ejecuto; no te tira el programa.

(defrule prevent-duplicate-write (:salience 15)
  "Si el LLM pide escribir EXACTAMENTE lo mismo que ya escribio en este turno,
   se lo dice y no lo repite.

   Se comparan ruta Y contenido. Dos escrituras del mismo fichero con
   contenido distinto son trabajo de verdad y van por su cuenta; solo el
   identico se impugna."
  (?intent (harness-fact (fact-type "intention") (data ?d)))
  (test (eql (data-get ?d :action) :write-file))
  (harness-fact (fact-type "file-write") (data ?fw-data))
  (test (and (eql (data-get ?fw-data :turn-id) (data-get ?d :turn-id))
             (string= (data-get ?fw-data :path) (data-get ?d :path))
             (string= (or (data-get ?fw-data :content) "")
                      (or (data-get ?d :content) ""))
             (data-get ?fw-data :applied)))
  =>
  (assert-step-cancelled
   ?d
   "CANCELADO: ese fichero ya tiene EXACTAMENTE este contenido en este turno, asi que reescribirlo no cambiaria nada. Si lo que querias era otra cosa, cambia el contenido.")
  (retract ?intent))

(defrule prevent-duplicate-command (:salience 15)
  "Si el LLM pide ejecutar EXACTAMENTE el mismo comando que ya ejecuto con
   exito en este turno, se lo dice y no lo repite.

   Solo el que SALIO BIEN. Un comando que fallo es justo el que hay que
   reintentar con otro enfoque: lo que se impugna es el que ya salio bien repetido, que no
   aporta nada nuevo y solo gasta una ronda."
  (?intent (harness-fact (fact-type "intention") (data ?d)))
  (test (eql (data-get ?d :action) :exec-command))
  (harness-fact (fact-type "command-exec") (data ?cx-data))
  (test (and (eql (data-get ?cx-data :turn-id) (data-get ?d :turn-id))
             (string= (or (data-get ?cx-data :command) "")
                      (or (data-get ?d :command) ""))
             (zerop (or (data-get ?cx-data :exit-code) -1))))
  =>
  (assert-step-cancelled
   ?d
   "CANCELADO: ese comando ya se ejecuto con EXITO en este turno (codigo 0), asi que repetirlo daria el mismo resultado. Para cambiar algo hay que cambiar el comando.")
  (retract ?intent))
