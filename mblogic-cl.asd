;;;; mblogic-cl.asd

(asdf:defsystem #:mblogic-cl
  :description "PLC Compiler/Interpreter ported from MBLogic Python system"
  :author "Gregory Brooks"
  :license "GPL-3.0"
  :version "0.1.0"
  :serial t
  :depends-on (#:cl-ppcre
               #:alexandria
               #:parse-number)
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "data-table")
                             (:file "instructions")
                             (:file "parser")
                             (:file "math-lib")        ; Before compiler (for compile-time math parsing)
                             (:file "timer-counter")
                             (:file "table-ops")
                             (:file "compiler")        ; After libraries (uses them at compile-time)
                             (:file "interpreter"))))
  :in-order-to ((test-op (test-op #:mblogic-cl/test))))

(asdf:defsystem #:mblogic-cl/web
  :description "Web-based ladder diagram visualization for MBLogic-CL"
  :author "Gregory Brooks"
  :license "GPL-3.0"
  :depends-on (#:mblogic-cl
               #:hunchentoot
               #:cl-json
               #:bordeaux-threads)
  :serial t
  :components ((:module "src/web"
                :serial t
                :components ((:file "package")
                             (:file "ladder-render")
                             (:file "json-api")
                             (:file "server")))))

(asdf:defsystem #:mblogic-cl/test
  :description "Test suite for mblogic-cl"
  :author "Gregory Brooks"
  :license "GPL-3.0"
  :depends-on (#:mblogic-cl
               #:fiveam)
  :serial t
  :components ((:module "test"
                :serial t
                :components ((:file "test-suite")
                             (:file "test-data-table")
                             (:file "test-parser")
                             (:file "test-compiler")
                             (:file "test-interpreter"))))
  :perform (test-op (o c) (symbol-call :fiveam '#:run!
                                       (find-symbol* '#:all-tests
                                                     :mblogic-cl-test))))

(asdf:defsystem #:mblogic-cl/web-test
  :description "Test suite for mblogic-cl web visualization"
  :author "Gregory Brooks"
  :license "GPL-3.0"
  :depends-on (#:mblogic-cl
               #:mblogic-cl/web
               #:mblogic-cl/test
               #:fiveam)
  :serial t
  :components ((:module "test"
                :components ((:file "test-ld-visualization"))))
  :perform (test-op (o c) (symbol-call :fiveam '#:run!
                                       (find-symbol* '#:ld-visualization-tests
                                                     :mblogic-cl-test))))
