(in-package :cl-harness)

;;; ============================================
;;; Rete rules for context pruning
;;;
;;; NOTE: Rules must NOT perform internal retrieve() queries
;;; (that triggers reentrant run-query and macro-expansion issues
;;; in the RHS). Per-type and global caps are enforced by plain
;;; functions called from build-yaml-context.
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
;;; Imperative pruning helpers (called from build-yaml-context,
;;; NOT from rules, to avoid reentrant run-query)
;;; ============================================================

(defun fact-timestamp (f)
  (get-slot-value f 'timestamp))

(defun retract-oldest-of-type (type keep)
  "Retract all but the newest KEEP facts of the given type, preserving real errors."
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
         (sorted (sort (remove nil prunable) #'<
                       :key #'fact-timestamp)))
    (when (> (length sorted) keep)
      (loop for f in sorted
            for i from 0
            when (< i (- (length sorted) keep))
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
                       :turn-id (data-get ?da :turn-id)
                       :parent-id (data-get ?da :turn-id))))))

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
                       :turn-id (data-get ?da :turn-id)
                       :parent-id (data-get ?da :turn-id))))))

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

(defun detect-read-loop ()
  "Scan the CURRENT turn for repeated *reads/probes* of the SAME file:
   - file-read facts (keyed by basename of the resolved path), and
   - command-exec strings that mention the same file basename.
   Returns (values PATH COUNT) when a file has been touched >= *loop-min-count*
   times. Re-reading the same thing over and over signals the model is stuck
   verifying instead of acting (the observed failure mode of weak models)."
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
