(asdf:defsystem #:grph
  :description "immutable graph data structure with Datalog query language"
  :version "2.0.1"
  :author "anders hoff / @inconvergent / inconvergent@gmail.com"
  :in-order-to ((asdf:test-op (asdf:test-op #:grph/tests)))
  :licence "MIT" :pathname "src/" :serial nil
  :depends-on (#:veq #:fset #:lparallel)
  :components ((:file "packages")
               (:file "init" :depends-on ("packages"))
               (:file "utils" :depends-on ("init"))

               (:file "docs" :depends-on ("utils"))
               (:file "config" :depends-on ("utils"))
               (:file "edge-set" :depends-on ("utils"))
               (:file "macros" :depends-on ("utils"))

               (:file "grph" :depends-on ("macros"))
               (:file "qry-utils" :depends-on ("grph"))
               (:file "qry-match" :depends-on ("qry-utils"))
               (:file "qry-runtime" :depends-on ("qry-match"))
               (:file "qry-runtime-2" :depends-on ("qry-match"))
               (:file "qry-runtime-par" :depends-on ("qry-runtime"))
               (:file "qry" :depends-on ("qry-runtime-par"))
               (:file "qry-extra" :depends-on ("qry"))
               (:file "qry-rules" :depends-on ("qry"))
               (:file "grph-walk" :depends-on ("qry"))

               (:file "xgrph-triangulate" :depends-on ("grph"))
               (:file "xgrph" :depends-on ("qry"))
               (:file "xgrph-isect" :depends-on ("xgrph"))
               (:file "xgrph-io" :depends-on ("grph-walk" "xgrph"))))

(asdf:defsystem #:grph/tests
  :depends-on (#:veq #:grph #:prove #:lparallel #:uiop #:asdf)
  :version "2.0.1"
  :perform (asdf:test-op (o s) (uiop:symbol-call ':grph-tests '#:run-tests))
  :pathname "test/" :serial t
  :components ((:file "run")))

