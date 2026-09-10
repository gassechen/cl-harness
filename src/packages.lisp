(defpackage :cl-harness
  (:use :cl :lisa-user :lisa)
  (:shadowing-import-from :lisa :assert)
  (:import-from :lisa :retrieve :rules :run :reset :clear :facts)
  (:import-from :lisa
                ;; internal but stable helpers
                #:get-slot-value)
  (:import-from :dexador :request)
  (:import-from :com.inuoe.jzon
                :parse
                :stringify)
    (:export
     ;; config
     #:*config*
     #:*session-id*
     #:load-config
     ;; debug
     #:*debug-mode*
     #:toggle-debug
     ;; context
     #:build-yaml-context
     #:collect-active-facts
     #:retract-oldest-of-type
     #:fact-slot
     ;; lisa re-exported for external callers/tests
     #:reset #:run #:clear #:retrieve #:rules #:facts #:get-slot-value
     ;; metrics
     #:metrics-reset #:metrics-summary #:metrics-to-json
     ;; llm
     #:call-llm
     #:llm-provider #:llm-model #:llm-endpoint #:llm-api-key #:llm-max-tokens
     #:openrouter-p #:groq-p #:openai-compat-endpoint #:openai-compat-headers
     #:gemini-p #:gemini-endpoint #:gemini-headers #:call-gemini
     ;; actions
     #:exec-command
     #:read-file
     #:write-file
     ;; persist
     #:save-session
     #:restore-session
     #:create-session-dump
     ;; repl
     #:start-harness
     #:stop-harness
     #:main))
