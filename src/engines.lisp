(in-package :cl-harness)

;;; ============================================
;;; Dual Lisa inference engines (symbolic brain)
;;; ============================================
;;; LISA runs one active engine at a time (`lisa::*active-engine*`, bound by
;;; `lisa:with-inference-engine`). Each `(lisa:make-inference-engine)` is a
;;; full `rete`: its own fact-table, rule network, contexts and focus stack.
;;; Facts asserted into one engine are INVISIBLE to the other.
;;;
;;; We keep TWO engines:
;;;   * *turn-engine*  — intra-turn working memory (tool results, loops, goals,
;;;                      conversation). It is the DEFAULT active engine, so the
;;;                      whole existing code base (assert/retract/retrieve/run)
;;;                      keeps targeting it unchanged.
;;;   * *mem-engine*   — long-term memory (durable knowledge: real errors,
;;;                      actions taken). Promoted from the turn engine at the
;;;                      end of each turn, persisted to dumps/longterm-mem.lisp,
;;;                      and re-exported into the YAML context of every turn.
;;; ============================================

(defparameter *turn-engine* nil
  "Intra-turn working memory engine (default active engine).")

(defparameter *mem-engine* nil
  "Cross-turn long-term memory engine (persisted).")

(defun install-dual-engines ()
  "Create the turn and mem inference engines and make *turn-engine* the
   default active engine. Load-time DEFRULE forms (and every runtime
   assert/retract/retrieve/run without an explicit with-* wrapper) therefore
   target the turn engine. Must run before rules.lisp is loaded."
  (setf *turn-engine* (lisa:make-inference-engine))
  (setf *mem-engine* (lisa:make-inference-engine))
  (setf lisa::*active-engine* *turn-engine*)
  ;; The active context must point into the NEW active engine, otherwise
  ;; assertions land in the boot-time engine's context.
  (setf lisa::*active-context* (lisa::initial-context *turn-engine*))
  (values *turn-engine* *mem-engine*))

(install-dual-engines)

(defmacro with-turn-engine (&body body)
  "Evaluate BODY with the intra-turn working memory engine active."
  `(lisa:with-inference-engine (*turn-engine*) ,@body))

(defmacro with-mem-engine (&body body)
  "Evaluate BODY with the long-term memory engine active."
  `(lisa:with-inference-engine (*mem-engine*) ,@body))

(defun collect-harness-facts ()
  "All harness-fact instances currently living in the ACTIVE engine."
  (mapcar #'first (retrieve (?f) (?f (harness-fact)))))

(defun reset-turn-engine ()
  "Retract all facts from the turn engine, keeping its rules intact."
  (with-turn-engine (lisa:reset)))

(defun reset-mem-engine ()
  "Retract all facts from the long-term memory engine, keeping its rules intact."
  (with-mem-engine (lisa:reset)))

;;; ============================================
;;; Long-term memory policy
;;; ============================================

(defun durable-type-p (type data)
  "TRUE when a fact is worth keeping in long-term memory:
   - command-exec facts that are REAL active errors (non-zero exit WITH
     evidence, not timed out, when conserve_errors is on);
   - file-write facts (actions actually taken on the project)."
  (or (and (string= type "command-exec")
           (real-error-p type data))
      (string= type "file-write")))

(defun mem-engine-facts ()
  "All harness-facts currently living in the long-term memory engine."
  (with-mem-engine
    (remove-if-not (lambda (f)
                     (durable-type-p (fact-type-of f) (fact-data-of f)))
                   (collect-harness-facts))))

(defun promote-durable-facts ()
  "Project durable turn-engine facts into long-term memory, run the mem
   engine's rules, then re-apply per-type caps so memory stays bounded."
  (dolist (f (unless (null *mem-engine*)
               (with-turn-engine (collect-harness-facts))))
    (let ((type (fact-type-of f))
          (data (fact-data-of f)))
      (when (and (durable-type-p type data) *mem-engine*)
        (with-mem-engine
          (assert (harness-fact (fact-type (identity type))
                                (timestamp (get-universal-time))
                                (data (identity data))))))))
  (unless (null *mem-engine*)
    (with-mem-engine
      (run)
      (retract-oldest-of-type "command-exec" (max-facts-per-type))
      (retract-oldest-of-type "file-write" (max-facts-per-type)))))

(defun boot-memory ()
  "Fresh-process boot: reset both engines, load persisted long-term memory
   from disk into the mem engine, and run its rules."
  (reset-turn-engine)
  (reset-mem-engine)
  (load-mem-engine)
  (with-mem-engine (run))
  t)