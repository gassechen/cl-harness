(in-package :cl-harness)

;;; ============================================
;;; Fact templates for Lisa/Rete engine
;;; ============================================

(deftemplate harness-fact ()
  (slot fact-type)
  (slot timestamp)
  (slot data))

(deftemplate context-slot ()
  (slot fact-type)
  (slot count))

;;; ============================================
;;; Causal metadata (Phase 2)
;;; ============================================
;;; Every fact carries :turn-id / :parent-id INSIDE its data plist (not a
;;; template slot) so persistence, dedup and YAML rendering need no change.
;;; Linkage is captured at ingestion, never derived by rules.

(defparameter *turn-counter* 0
  "Monotonic turn number; incremented at the start of each process-turn.")

(defun current-turn-id ()
  "Turn context for facts asserted between turns (0 = pre-session setup)."
  *turn-counter*)

(defun reset-turn-counter ()
  (setf *turn-counter* 0))

;;; ============================================
;;; Goal & Todo Templates (Backward Chaining / Planning)
;;; Inspired by Lisa's mab.lisp (Means-Ends Analysis)
;;; and OpenCode's todo and session_context_epoch
;;; ============================================

(deftemplate agent-todo ()
  (slot id)
  (slot task)
  (slot status)      ; "pending", "in-progress", "completed", "failed"
  (slot priority)    ; "high", "normal", "low"
  (slot parent-id)   ; parent goal ID for hierarchical sub-goals
  (slot turn-id)
  (slot timestamp))

(deftemplate context-epoch ()
  (slot id)
  (slot baseline-seq)
  (slot summary)
  (slot timestamp))

(defparameter *todo-counter* 0)
(defparameter *epoch-counter* 0)

(defun add-todo (task &key (priority "normal") (parent-id nil) (status "pending"))
  "Assert a new goal/todo into Rete."
  (incf *todo-counter*)
  (let ((id (format nil "todo-~A" *todo-counter*))
        (now (get-universal-time))
        (turn (current-turn-id)))
    (assert (agent-todo (id (identity id))
                        (task (identity task))
                        (status (identity status))
                        (priority (identity priority))
                        (parent-id (or parent-id "root"))
                        (turn-id (identity turn))
                        (timestamp (identity now))))
    id))

(defun find-todo-by-id (id)
  "Locate an agent-todo fact by id (plain Lisp filter; avoids DSL literal-symbol pitfalls)."
  (let ((matches (retrieve (?t) (?t (agent-todo)))))
    (find-if (lambda (f)
               (string= (get-slot-value f 'id) id))
             (mapcar #'first matches))))

(defun complete-todo (id &optional (result-summary nil))
  "Mark an agent-todo as completed."
  (let ((fact (find-todo-by-id id)))
    (when fact
      (let ((current-task (get-slot-value fact 'task))
            (priority (get-slot-value fact 'priority))
            (parent (get-slot-value fact 'parent-id))
            (turn (get-slot-value fact 'turn-id)))
        (retract fact)
        (assert (agent-todo (id (identity id))
                            (task (if result-summary
                                      (format nil "~A [Completado: ~A]" current-task result-summary)
                                      current-task))
                            (status "completed")
                            (priority (identity priority))
                            (parent-id (identity parent))
                            (turn-id (identity turn))
                            (timestamp (get-universal-time)))))
      t)))

(defun set-todo-status (id new-status)
  "Update status of an agent-todo."
  (let ((fact (find-todo-by-id id)))
    (when fact
      (let ((task (get-slot-value fact 'task))
            (priority (get-slot-value fact 'priority))
            (parent (get-slot-value fact 'parent-id))
            (turn (get-slot-value fact 'turn-id)))
        (retract fact)
        (assert (agent-todo (id (identity id))
                            (task (identity task))
                            (status (identity new-status))
                            (priority (identity priority))
                            (parent-id (identity parent))
                            (turn-id (identity turn))
                            (timestamp (get-universal-time)))))
      t)))

(defun collect-active-todos ()
  "Retrieve all agent-todos from Rete sorted by timestamp."
  (let* ((matches (retrieve (?t) (?t (agent-todo))))
         (facts (mapcar #'first matches)))
    (sort (remove nil facts) #'< :key (lambda (f) (or (get-slot-value f 'timestamp) 0)))))

(defun create-epoch (summary &optional (baseline-seq *turn-counter*))
  "Establish a context epoch (checkpoint) consolidating history up to baseline-seq."
  (incf *epoch-counter*)
  (let ((id (format nil "epoch-~A" *epoch-counter*))
        (now (get-universal-time)))
    ;; Retract prior epoch
    (dolist (m (retrieve (?e) (?e (context-epoch))))
      (retract (first m)))
    (assert (context-epoch (id (identity id))
                           (baseline-seq (identity baseline-seq))
                           (summary (identity summary))
                           (timestamp (identity now))))
    id))

(defun current-epoch ()
  "Retrieve the active context epoch, if one exists."
  (let ((matches (retrieve (?e) (?e (context-epoch)))))
    (and matches (caar matches))))
