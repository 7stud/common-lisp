;; ~/quicklisp/local-projects/my-projects/proj2/proj2.asd

(defsystem "proj2"
  :version "0.0.1"
  :author ""
  :license ""
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "proj2/tests"))))

(defsystem "proj2/tests"
  :author ""
  :license ""
  :depends-on ("proj2"
               "fiveam")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for proj2"
  :perform (test-op (op c) (symbol-call  :proj2/tests/main :test-proj2)))
