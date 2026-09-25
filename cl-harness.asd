(defsystem :cl-harness
  :description "Context management harness for LLMs using Lisa/Rete"
  :version "0.1.0"
  :author "cl-harness"
  :license "MIT"
  :depends-on (:lisa
               :dexador
               :com.inuoe.jzon
               :bordeaux-threads
               :cl-ppcre
               :uiop)
   :serial t
   :components ((:module "src"
                 :serial t
                 :components ((:file "packages")
                              (:file "config")
                              (:file "engines")
                              (:file "facts")
                              (:file "rules")
                              (:file "context")
                              (:file "metrics")
                              (:file "actions")
                              (:file "background")
                              (:file "llm")
                              (:file "persist")
                              (:file "repl")
                              (:file "main")))))

;;;; Sistema de pruebas offline. Vive aquí (y no en cl-harness/tests.asd) porque
;;;; Quicklisp sólo registra los .asd del primer nivel de local-projects/.
;;;;   ./run-tests.sh
(defsystem "cl-harness/tests"
  :description "Offline test suite for cl-harness (Rove)"
  :version "0.1.0"
  :author "cl-harness"
  :license "MIT"
  :depends-on ("cl-harness" "rove")
  :serial t
  :components ((:file "tests/harness")))
