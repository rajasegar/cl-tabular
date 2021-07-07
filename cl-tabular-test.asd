(defsystem "cl-tabular-test"
  :defsystem-depends-on ("prove-asdf")
  :author "Rajasegar Chandran"
  :license ""
  :depends-on ("cl-tabular"
               "prove")
  :components ((:module "tests"
                :components
                ((:test-file "cl-tabular"))))
  :description "Test system for cl-tabular"
  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
