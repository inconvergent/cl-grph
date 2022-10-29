(asdf:defsystem #:grph
  :description "graph thing"
  :version "0.10.1"
  :author "anders hoff / @inconvergent / inconvergent@gmail.com"
  :licence "MIT"
  :in-order-to ((asdf:test-op (asdf:test-op #:grph/tests)))
  :pathname "src/"
  :serial nil
  :depends-on (#:veq #:fset #:alexandria #+:grph-parallel #:lparallel)
  :components ((:file "packages")
               (:file "utils" :depends-on ("packages"))
               (:file "docs" :depends-on ("utils"))
               (:file "config" :depends-on ("utils"))
               (:file "edge-set" :depends-on ("utils"))
               (:file "macros" :depends-on ("utils"))
               (:file "grph" :depends-on ("macros"))
               (:file "qry-utils" :depends-on ("grph"))
               (:file "qry-match" :depends-on ("qry-utils"))
               (:file "qry-runtime" :depends-on ("qry-match"))

               ; parallel
               #+:grph-parallel
               (:file "qry-runtime-par" :depends-on ("qry-runtime"))
               #+:grph-parallel
               (:file "qry" :depends-on ("qry-runtime-par"))

               ; serial
               #-:grph-parallel
               (:file "qry" :depends-on ("qry-runtime"))

               (:file "qry-rules" :depends-on ("qry"))
               (:file "xgrph" :depends-on ("qry"))
               (:file "grph-walk" :depends-on ("xgrph"))))

(asdf:defsystem #:grph/tests
  :depends-on (#:veq #:grph #:prove #+:grph-parallel #:lparallel)
  :perform (asdf:test-op (o s)
             (uiop:symbol-call ':grph-tests
               #+:grph-parallel '#:p/run-tests
               #-:grph-parallel '#:run-tests))
  :pathname "test/"
  :serial t
  :components ((:file "run")))

; TODO: there is an if-feature feature in ASDF, how does it work??

