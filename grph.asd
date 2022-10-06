(asdf:defsystem #:grph
  :description "graph thing"
  :version "0.4.0"
  :author "anders hoff / @inconvergent / inconvergent@gmail.com"
  :licence "MIT"
  :in-order-to ((asdf:test-op (asdf:test-op #:grph/tests)))
  :pathname "src/"
  :serial nil
  :depends-on (#:veq #:fset #:alexandria #+:grph-parallel #:lparallel)
  :components ((:file "packages")
               (:file "utils" :depends-on ("packages"))
               (:file "config" :depends-on ("utils"))
               (:file "edge-set" :depends-on ("utils"))
               (:file "macros" :depends-on ("utils"))
               (:file "grph" :depends-on ("macros"))
               (:file "qry-utils" :depends-on ("grph"))
               (:file "qry-match" :depends-on ("qry-utils"))
               (:file "qry-runtime" :depends-on ("qry-match"))
               #+:grph-parallel (:file "qry-runtime-par"
                                 :depends-on ("qry-runtime"))
               (:file "qry" :depends-on ("qry-runtime"
                                         #+:grph-parallel "qry-runtime-par"))
               (:file "xgrph" :depends-on ("qry"))
               (:file "docs" :depends-on ("xgrph"))))

(asdf:defsystem #:grph/tests
  :depends-on (#:veq #:grph #:prove #+:grph-parallel #:lparallel)
  :perform (asdf:test-op (o s) (uiop:symbol-call ':grph-tests '#:run-tests))
  :pathname "test/"
  :serial t
  :components ((:file "run")))

