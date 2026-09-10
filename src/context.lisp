(in-package :cl-harness)

;;; ============================================
;;; Context builder — YAML output
;;; ============================================

(defun escape-yaml (s)
  "Escape a string for YAML scalar values."
  (if (or (null s) (string= s ""))
      "\"\""
      (let* ((first-char (elt s 0))
             (needs-quotes (or (find #\: s)
                              (find #\# s)
                              (find #\{ s)
                              (find #\} s)
                              (find #\[ s)
                              (find #\] s)
                              (find #\, s)
                              (find #\| s)
                              (find #\> s)
                              (find #\* s)
                              (find #\? s)
                              (find #\- s :end 1)
                              (digit-char-p first-char)
                              (string= (string-downcase s) "true")
                              (string= (string-downcase s) "false")
                              (string= (string-downcase s) "yes")
                              (string= (string-downcase s) "no")
                              (string= (string-downcase s) "null")
                              (string= (string-downcase s) "nil")
                              (find #\Newline s))))
        (if needs-quotes
            (format nil "~S" s)
            s))))

(defun yaml-value (key value)
  "Format a single YAML key-value pair."
  (etypecase value
    (null (format nil "~A: null" key))
    (string (format nil "~A: ~A" key (escape-yaml value)))
    (number (format nil "~A: ~A" key value))
    (boolean (format nil "~A: ~A" key (if value "true" "false")))))

(defun fact-slot (fact slot)
  "Read a slot value from a lisa fact instance."
  (get-slot-value fact slot))

(defun fact-to-yaml (fact)
  "Convert a harness-fact instance to a YAML mapping string."
  (let* ((type (fact-slot fact 'fact-type))
         (ts   (fact-slot fact 'timestamp))
         (data (fact-slot fact 'data))
         (head (list (format nil "- type: ~A" (escape-yaml type))
                     (format nil "  timestamp: ~A" ts)))
         (tail (when (listp data)
                 (loop for (k v) on data by #'cddr
                       for key-name = (if (symbolp k) (string-downcase (symbol-name k)) (princ-to-string k))
                       collect (format nil "  ~A: ~A"
                                       (escape-yaml key-name)
                                       (if (stringp v) (escape-yaml v) (princ-to-string v)))))))
    (format nil "~{~A~^~%~}" (append head tail))))

(defun collect-active-facts ()
  "Retrieve all current harness facts from Rete, sorted by timestamp."
  (let* ((result (retrieve (?f) (?f (harness-fact))))
         (facts (mapcar #'first result)))
    (sort (remove nil facts) #'<
          :key (lambda (f) (fact-slot f 'timestamp)))))

;;; ============================================================
;;; Structural relevance (Phase 3)
;;; ============================================================
;;; The harness never degrades fact content: every fact that is selected
;;; is emitted with its payload INTACT. Selection is purely structural:
;;; - recency (newer timestamp = higher score)
;;; - closeness to the current turn (small |turn-id - now| = higher)
;;; - repetition (a dedup-key seen more times = more relevant; re-reading
;;;   or re-running the same thing signals it matters)
;;; - causality (facts with a parent-id = part of a chain worth keeping)
;;;
;;; This keeps Rete deciding structure (time, repetition, dependency) and
;;; leaves meaning to the LLM. Nothing here is a rule — it is a scoring
;;; function called when assembling the context block.

(defun fact-timestamp-of (f)
  (fact-slot f 'timestamp))

(defun fact-data-of (f)
  (fact-slot f 'data))

(defun fact-type-of (f)
  (fact-slot f 'fact-type))

(defun fact-raw-chars (f)
  "Estimated character footprint of a fact in the budget."
  (let* ((data (fact-data-of f))
         (raw-len (length (or (data-get data :contents)
                              (data-get data :output)
                              (data-get data :text)
                              ""))))
    (min raw-len (max-fact-payload-chars))))

(defun tokenize-query (text)
  "Extract lowercase alphanumeric keywords from query, filtering short tokens and common stopwords."
  (when (and text (stringp text))
    (let ((words (cl-ppcre:split "[^a-zA-Z0-9_.-]+" (string-downcase text)))
          (stopwords '("the" "and" "for" "with" "that" "this" "from" "what" "show" "tell"
                       "que" "los" "las" "del" "por" "para" "con" "una" "uno" "hay" "como"
                       "cual" "donde" "pero" "este" "esta" "bien" "hace" "quien")))
      (remove-if (lambda (w)
                   (or (< (length w) 3)
                       (member w stopwords :test #'string=)))
                 words))))

(defun keyword-match-score (keywords text)
  "Count occurrences of keywords in text."
  (if (or (null keywords) (null text) (not (stringp text)) (string= text ""))
      0
      (let ((target (string-downcase text))
            (count 0))
        (dolist (kw keywords count)
          (when (search kw target :test #'char=)
            (incf count))))))

(defun truncate-payload (text &optional (max-chars (max-fact-payload-chars)))
  "If TEXT exceeds MAX-CHARS, retain head and tail with an explicit omission notice.
   This prevents a single massive file from starving the rest of the context budget."
  (if (or (null text) (<= (length text) max-chars))
      text
      (let* ((head-len (floor (* max-chars 0.6)))
             (tail-len (floor (* max-chars 0.3)))
             (omitted (- (length text) head-len tail-len)))
        (format nil "~A~%... [~A characters omitted. Use read_file or grep to inspect specifics] ...~%~A"
                (subseq text 0 head-len)
                omitted
                (subseq text (- (length text) tail-len))))))

(defun count-repetitions (facts)
  "Frequency of each dedup key among FACTS -> hash key->count."
  (let ((h (make-hash-table :test #'equal)))
    (dolist (f facts)
      (let* ((type (fact-type-of f))
             (key (dedup-key type (fact-data-of f))))
        (when key
          (incf (gethash key h 0)))))
    h))

(defun relevance-score (f facts now-turn rep-counts &optional (query-keywords nil))
  "Structural and semantic relevance of fact F, higher = more important."
  (let* ((data (fact-data-of f))
         (type (fact-type-of f))
         (turn (or (data-get data :turn-id) 0))
         (path (data-get data :path))
         (cmd (data-get data :command))
         (content (or (data-get data :contents) (data-get data :output) (data-get data :text) ""))
         (exit-code (data-get data :exit-code)))
    (+ ;; 1. Turn proximity (exponential logical decay, NOT clock seconds)
       (let ((turn-dist (abs (- turn now-turn))))
         (max 0 (- 500 (* 60 turn-dist))))
       ;; 2. Conversation guarantee: preserve dialogue thread
       (if (conversation-type-p type) 300 0)
       ;; 3. Error prioritization: commands that failed are critical context
       (if (and (string= type "command-exec") exit-code (not (zerop exit-code)))
           400
           0)
       ;; 4. Frequency of repetition
       (let ((key (dedup-key type data)))
         (if key
             (min 200 (* 40 (gethash key rep-counts 0)))
             0))
       ;; 5. Semantic keyword matching against user prompt
       (if query-keywords
           (+ (if (and path (plusp (keyword-match-score query-keywords path))) 450 0)
              (if (and cmd (plusp (keyword-match-score query-keywords cmd))) 400 0)
              (* 80 (min 4 (keyword-match-score query-keywords content))))
           0)
       ;; 6. Causal dependency
       (if (data-get data :parent-id) 50 0))))

(defun select-relevant-facts (facts now-turn
                              &key (budget (max-context-chars))
                                   (max-facts (max-context-facts))
                                   (query nil))
  "Pick the most structurally and semantically relevant FACTS that fit in BUDGET."
  (let* ((keywords (tokenize-query query))
         (rep (count-repetitions facts))
         (scored (mapcar (lambda (f)
                           (cons (relevance-score f facts now-turn rep keywords) f))
                         facts))
         (sorted (sort scored #'> :key #'car))
         (selected '())
         (used 0))
    (when *debug-mode*
      (format t "~&[DEBUG select-relevant-facts] total-facts=~A budget=~A max-facts=~A keywords=~A~%"
              (length facts) budget max-facts keywords))
    (dolist (cell sorted)
      (let* ((f (cdr cell))
             (score (car cell))
             (type (fact-type-of f))
             (key (dedup-key type (fact-data-of f)))
             (cost (+ (fact-raw-chars f) 200)))
        (when *debug-mode*
          (format t "~&[DEBUG select-relevant-facts] score=~6,F type=~A key=~A cost=~A selected=~A used=~A~%"
                  score type key cost (length selected) used))
        (when (and (< (length selected) max-facts)
                   (<= (+ used cost) budget))
          (push f selected)
          (incf used cost)
          (when *debug-mode*
            (format t "~&[DEBUG select-relevant-facts]   -> SELECTED~%")))
        (when *debug-mode*
          (format t "~&[DEBUG select-relevant-facts]   -> REJECTED (budget/max-facts exceeded)~%"))))
    (when *debug-mode*
      (format t "~&[DEBUG select-relevant-facts] final-selected=~A total-used=~A~%" (length selected) used))
    (nreverse selected)))

(defun grouped-context-string (facts)
  "Render facts grouped by causal :turn-id (Phase 2 conversation graph),
   including active epoch checkpoints and agent goals/todos."
  (let ((groups (make-hash-table :test #'eql))
        (epoch (current-epoch))
        (todos (collect-active-todos)))
    (dolist (f facts)
      (let* ((type (fact-type-of f))
             (data (fact-data-of f))
             (turn (or (data-get data :turn-id) 0))
             (ts (fact-timestamp-of f)))
        (push (cons ts (cons type data)) (gethash turn groups))))
    (maphash (lambda (turn cells)
               (setf (gethash turn groups)
                     (sort cells #'< :key #'car)))
             groups)
    (with-output-to-string (s)
      (format s "context:~%")
      (format s "  session: ~A~%" (escape-yaml *session-id*))
      ;; Render epoch baseline snapshot if present (OpenCode session_context_epoch)
      (when epoch
        (format s "  epoch:~%")
        (format s "    id: ~A~%" (escape-yaml (get-slot-value epoch 'id)))
        (format s "    baseline_turn: ~A~%" (get-slot-value epoch 'baseline-seq))
        (format s "    summary: ~A~%" (escape-yaml (get-slot-value epoch 'summary))))
      ;; Render goals / todos (mab.lisp backward-chaining goal tree)
      (when todos
        (format s "  goals:~%")
        (dolist (td todos)
          (let ((id (get-slot-value td 'id))
                (task (get-slot-value td 'task))
                (status (get-slot-value td 'status))
                (priority (get-slot-value td 'priority))
                (parent (get-slot-value td 'parent-id)))
            (format s "    - id: ~A~%" (escape-yaml id))
            (format s "      task: ~A~%" (escape-yaml task))
            (format s "      status: ~A~%" (escape-yaml status))
            (format s "      priority: ~A~%" (escape-yaml priority))
            (unless (or (null parent) (string= parent "root"))
              (format s "      parent_goal: ~A~%" (escape-yaml parent))))))
      ;; Render turns
      (format s "  turns:~%")
      (dolist (turn (sort (loop for k being the hash-keys of groups collect k)
                          #'<))
        (format s "    ~A:~%" turn)
        (dolist (cell (gethash turn groups))
          (let ((type (cadr cell))
                (data (cddr cell)))
            (cond ((string= type "user-input")
                   (format s "      user: ~A~%"
                           (escape-yaml (or (data-get data :text) ""))))
                  ((string= type "llm-response")
                   (format s "      assistant: ~A~%"
                           (escape-yaml (or (data-get data :text) ""))))
                  ((string= type "command-exec")
                   (format s "      exec: ~A -> exit ~A~%"
                           (escape-yaml (or (data-get data :command) ""))
                           (or (data-get data :exit-code) ""))
                   (format s "        output: ~A~%"
                           (escape-yaml (truncate-payload (or (data-get data :output) "")))))
                  ((string= type "file-read")
                   (format s "      read: ~A~%"
                           (escape-yaml (or (data-get data :path) "")))
                   (format s "        contents: ~A~%"
                           (escape-yaml (truncate-payload (or (data-get data :contents) "")))))
                  ((string= type "file-write")
                   (format s "      wrote: ~A (~A bytes)~%"
                           (escape-yaml (or (data-get data :path) ""))
                           (or (data-get data :bytes) "")))
                  (t
                   (format s "      ?~A: ~S~%" (escape-yaml type) data)))))))))

(defun build-yaml-context (user-message)
  "Build the full YAML context block: run pruning rules, then select the
   most structurally and semantically relevant facts."
  ;; 1. Fire TTL + dedup rules
  (run)
  ;; 2. Enforce per-type caps imperatively
  (retract-oldest-of-type "user-input" (max-facts-per-type))
  (retract-oldest-of-type "llm-response" (max-facts-per-type))
  (retract-oldest-of-type "command-exec" (max-facts-per-type))
  (retract-oldest-of-type "file-read" (max-facts-per-type))
  (retract-oldest-of-type "file-write" (max-facts-per-type))
  ;; 3. Select the most relevant facts within budget, prioritizing user-message keywords
  (let* ((all-facts (collect-active-facts))
         (now-turn (or (data-get (fact-data-of (car (last all-facts))) :turn-id)
                       0)))
    (when *debug-mode*
      (format t "~&[DEBUG build-yaml-context] active-facts=~A now-turn=~A~%" (length all-facts) now-turn)
      (dolist (f all-facts)
        (format t "~&[DEBUG build-yaml-context]   fact type=~A ts=~A turn-id=~A raw-chars=~A~%"
                (fact-type-of f) (fact-timestamp-of f)
                (or (data-get (fact-data-of f) :turn-id) 0)
                (fact-raw-chars f))))
    (let ((selected (select-relevant-facts all-facts now-turn :query user-message)))
      (when *debug-mode*
        (format t "~&[DEBUG build-yaml-context] selected-facts=~A~%" (length selected)))
      (let ((yaml (grouped-context-string selected)))
        (when *debug-mode*
          (format t "~&[DEBUG build-yaml-context] yaml-length=~A~%" (length yaml))
          (format t "~&[DEBUG build-yaml-context] === YAML START ===~%~A~%~&[DEBUG build-yaml-context] === YAML END ===~%" yaml))
        yaml))))
