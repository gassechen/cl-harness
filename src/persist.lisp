(in-package :cl-harness)

;;; ============================================
;;; Persistence — dump, restore, SBCL image
;;; ============================================

(defun ensure-dirs ()
  (ensure-directories-exist (sessions-dir))
  (ensure-directories-exist (dumps-dir)))

(defun session-facts-path ()
  (merge-pathnames (format nil "~A-facts.lisp" *session-id*)
                   (dumps-dir)))

(defun session-json-path ()
  (merge-pathnames (format nil "~A.json" *session-id*)
                   (sessions-dir)))

(defun mem-session-facts-path ()
  "Long-term memory dump. Global (not keyed by session id): it carries durable
   project knowledge across sessions and processes."
  (merge-pathnames "longterm-mem.lisp" (dumps-dir)))

(defun bounded-fact-data (data)
  "Return DATA (a plist) with its big payload keys (:text/:contents/:output)
   clipped to max-fact-payload-chars for persistence. Facts are evidential
   and re-derivable; persisting full file/command dumps would bloat the
   session .lisp / .json records with megabytes of raw text. Clipped with the
   same head+tail marker used when rendering context."
  (let ((limit (max-fact-payload-chars))
        (out '()))
    (loop for (k v) on data by #'cddr
          do (push k out)
             (push (if (and (stringp v) (member k '(:contents :output :text)))
                       (truncate-payload v limit)
                       v)
                   out))
    (nreverse out)))

(defun dump-facts-to-lisp ()
  "Write all current facts to a .lisp file for restore."
  (ensure-dirs)
  (let ((facts (collect-active-facts)))
    (with-open-file (s (session-facts-path)
                       :direction :output
                       :if-exists :supersede)
      (format s ";;; Session ~A facts dump~%" *session-id*)
      (format s ";;; Generated: ~A~%" (get-universal-time))
      ;; Persist monotonic counters so turn/todo/epoch ids stay monotonic
      ;; across image reboots and :restore in fresh processes.
      (format s "(setf *turn-counter* ~A)~%" *turn-counter*)
      (format s "(setf *todo-counter* ~A)~%" *todo-counter*)
      (format s "(setf *epoch-counter* ~A)~%" *epoch-counter*)
      (format s "(defun restore-facts ()~%  (progn~%")
      (dolist (f facts)
        (let ((type (fact-slot f 'fact-type))
              (data (bounded-fact-data (fact-slot f 'data))))
          (format s "    (assert (harness-fact (fact-type ~S)~%"
                  type)
          (format s "                                 (timestamp (get-universal-time))~%")
          (format s "                                 (data (quote ~S))))~%" data)))
      (format s "    t))~%"))))

(defun dump-mem-facts-to-lisp ()
  "Write durable long-term memory facts to dumps/longterm-mem.lisp for the
   next boot to reload (project knowledge survives across sessions)."
  (ensure-dirs)
  (let ((facts (mem-engine-facts)))
    (with-open-file (s (mem-session-facts-path)
                       :direction :output
                       :if-exists :supersede)
      (format s ";;; Long-term memory dump~%")
      (format s ";;; Generated: ~A~%" (get-universal-time))
      (format s "(defun restore-mem-facts ()~%  (progn~%")
      (dolist (f facts)
        (let ((type (fact-type-of f))
              (data (bounded-fact-data (fact-data-of f))))
          (format s "    (assert (harness-fact (fact-type ~S)~%"
                  type)
          (format s "                                 (timestamp (get-universal-time))~%")
          (format s "                                 (data (quote ~S))))~%" data)))
      (format s "    t))~%"))))

(defun load-mem-engine ()
  "Load the persisted long-term memory dump into the *mem-engine* (if present)."
  (let ((path (mem-session-facts-path)))
    (when (probe-file path)
      (with-mem-engine
        (let ((*package* (find-package :cl-harness)))
          (load path))
        (when (fboundp 'restore-mem-facts)
          (restore-mem-facts))))))

(defun clear-mem-persistence ()
  "Drop persisted long-term memory from disk (used by :clearall)."
  (let ((path (mem-session-facts-path)))
    (when (probe-file path)
      (delete-file path)))
  (reset-mem-engine)
  t)

(defun facts-to-json-data ()
  "Return facts as a JSON-compatible plist (payloads bounded, see
   bounded-fact-data)."
  (let ((facts (collect-active-facts)))
    (list :session-id *session-id*
          :timestamp (get-universal-time)
          :facts (mapcar (lambda (f)
(list :type (fact-slot f 'fact-type)
                                  :timestamp (fact-slot f 'timestamp)
                                  :data (bounded-fact-data (fact-slot f 'data))))
                         facts))))

(defun dump-session-json ()
  "Write session state to JSON for interoperability."
  (ensure-dirs)
  (let* ((data (facts-to-json-data))
         (json-str (com.inuoe.jzon:stringify
                    (convert-to-json-map data))))
    (with-open-file (s (session-json-path)
                       :direction :output
                       :if-exists :supersede)
      (write-string json-str s))))

(defun convert-to-json-map (data)
  "Recursively convert plists to hash tables for jzon."
  (etypecase data
    (null nil)
    (string data)
    (number data)
    (symbol (if (eq data t) data (string-downcase (symbol-name data))))
    (list
     (if (and (plusp (length data))
              (keywordp (first data)))
         ;; plist -> object
         (let ((ht (make-hash-table :test 'equal)))
           (loop for (k v) on data by #'cddr
                 do (setf (gethash (string-downcase (symbol-name k)) ht)
                          (convert-to-json-map v)))
           ht)
         ;; regular list -> array
         (mapcar #'convert-to-json-map data)))))

(defun save-session ()
  "Full persistence: dump .lisp, dump long-term memory, dump .json, dump metrics."
  (dump-facts-to-lisp)
  (dump-mem-facts-to-lisp)
  (dump-session-json)
  (metrics-to-json)
  (format t "~&Session ~A saved.~%" *session-id*))

(defun restore-session (&optional (session-id nil))
  "Restore facts from a .lisp dump file.
   When SESSION-ID is given, it becomes the active session (continuity:
   subsequent saves/restores keep using that id)."
  (let* ((sid (or session-id *session-id*)))
    (when session-id
      (setf *session-id* session-id))
    (let ((path (merge-pathnames (format nil "~A-facts.lisp" sid)
                                 (dumps-dir))))
      (if (probe-file path)
          (progn
            ;; Dump files are emitted with unqualified symbols; load them in the
            ;; cl-harness package so deferred symbol resolution (RESTORE-FACTS,
            ;; HARNESS-FACT, slot names) works regardless of the caller's package.
            (let ((*package* (find-package :cl-harness)))
              (load path))
            (when (fboundp 'restore-facts)
              (restore-facts))
            ;; Restore long-term memory into the mem engine as well.
            (load-mem-engine)
            (format t "~&Session ~A restored.~%" sid))
          (format t "~&No dump found for session ~A.~%" sid)))))

(defun refresh-fact-timestamps ()
  "Re-assert every harness-fact with a fresh timestamp (for in-image resume).
   The session state already lives in the core image; refreshing the clock
   keeps the wall-clock TTL from evicting facts when the image is resumed later."
  (dolist (f (collect-active-facts))
    (let ((type (fact-slot f 'fact-type))
          (data (fact-slot f 'data)))
      (retract f)
      (assert (harness-fact (fact-type (identity type))
                            (timestamp (get-universal-time))
                            (data (identity data))))))
  t)

(defun create-session-dump (&optional (path nil))
  "Create a standalone executable with current state saved (:EXECUTABLE T).
   The session lives IN the image (Lisa working memory + facts persist in the
   core), so no restore-on-boot is needed: the toplevel just resumes the REPL.
   :COMPRESSION/:PURIFY are skipped: this SBCL build lacks :SB-CORE-COMPRESSION
   and uses the generational GC (purify is a no-op there)."
  (let ((out (or path
                 (merge-pathnames (format nil "~A.core" *session-id*)
                                  (dumps-dir)))))
    (ensure-dirs)
    (save-session)
    (refresh-fact-timestamps)
    (sb-ext:save-lisp-and-die out
                              :executable t
                              :toplevel (lambda ()
                                          (with-simple-restart (abort "Exit cl-harness (aborted)")
                                            (let ((args (rest sb-ext:*posix-argv*)))
                                              (if args
                                                  (run-one-shot (format nil "~{~A~^ ~}" args) nil)
                                                  (start-harness nil))))))))
