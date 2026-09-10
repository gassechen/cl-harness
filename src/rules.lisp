(in-package :cl-harness)

;;; ============================================
;;; Rete rules for context pruning
;;;
;;; NOTE: Rules must NOT perform internal retrieve() queries
;;; (that triggers reentrant run-query and macro-expansion issues
;;; in the RHS). Per-type and global caps are enforced by plain
;;; functions called from build-yaml-context.
;;; ============================================

;;; --- Type tiers ---
;;; conversation facts (user-input/llm-response) are NOT re-derivable:
;;; they must never expire (only per-type caps bound them).
;;; evidential facts (command-exec/file-read/file-write) ARE re-derivable:
;;; they expire (TTL) and are deduplicated by key (last-write-wins).

(defun conversation-type-p (type)
  (member type '("user-input" "llm-response") :test #'string=))

(defun evidential-type-p (type)
  (member type '("command-exec" "file-read" "file-write") :test #'string=))

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
  "Structural key identifying equivalent facts of a type (NIL = no dedup)."
  (cond ((string= type "command-exec") (data-get data :command))
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
             (t nil))))))

(defun fact-expired-p (type ts data)
  "Check whether an evidential fact has expired.
   1. Conversation facts never expire.
   2. Commands that failed (exit-code != 0) NEVER expire via TTL (active error context).
   3. Logical turn distance: expires if created more than (fact-ttl-turns) turns ago.
   4. Wall-clock TTL: if fact-ttl-seconds is non-zero, expires if older than that window."
  (and (ttl-eligible-p type)
       ;; Failed commands are preserved as active error context
       (not (and (string= type "command-exec")
                 (let ((code (data-get data :exit-code)))
                   (and code (not (zerop code))))))
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
  "Retract all but the newest KEEP facts of the given type, preserving active errors."
  (let* ((result (retrieve (?f) (?f (harness-fact (fact-type type)))))
         (facts (mapcar #'first result))
         ;; If it's a failed command, protect it from blunt per-type retraction
         (prunable (if (string= type "command-exec")
                       (remove-if (lambda (f)
                                    (let ((code (data-get (fact-data-of f) :exit-code)))
                                      (and code (not (zerop code)))))
                                  facts)
                       facts))
         (sorted (sort (remove nil prunable) #'<
                       :key #'fact-timestamp)))
    (when (> (length sorted) keep)
      (loop for f in sorted
            for i from 0
            when (< i (- (length sorted) keep))
            do (retract f)))))
