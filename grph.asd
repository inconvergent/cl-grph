
(asdf:defsystem #:grph
  :description "graph thing"
  :version "0.0.1"
  :author "anders hoff / @inconvergent / inconvergent@gmail.com"
  :licence "MIT"
  :in-order-to ((asdf:test-op (asdf:test-op #:grph/tests)))
  :pathname "src/"
  :serial nil
  :depends-on (#:fset #:alexandria)
  :components ((:file "packages")
               (:file "utils" :depends-on ("packages"))
               (:file "macros" :depends-on ("utils"))
               (:file "grph" :depends-on ("macros"))
               (:file "qry" :depends-on ("grph"))
               (:file "docs" :depends-on ("grph" "qry"))))

(asdf:defsystem #:grph/tests
  :depends-on (#:grph #:prove)
  :perform (asdf:test-op (o s) (uiop:symbol-call ':grph-tests '#:run-tests))
  :pathname "test/"
  :serial t
  :components ((:file "run")))

