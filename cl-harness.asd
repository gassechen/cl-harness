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
                              (:file "facts")
                              (:file "rules")
                              (:file "context")
                              (:file "metrics")
                              (:file "actions")
                              (:file "llm")
                              (:file "persist")
                              (:file "repl")
                              (:file "main")))))
