#!/usr/local/bin/sbcl --script

(load "~/quicklisp/setup.lisp")
(let ((*features* `(:grph-parallel ,@*features*)))
  (ql:quickload :grph) (ql:quickload :auxin))
(rnd:set-rnd-state 1)

(setf lparallel:*kernel* (lparallel:make-kernel 4))

; some simple performance "tests"

(defun rsym () (grph::kv (grph::symb "SYM" (rnd:rndi 10))))
(defun get-sample-graph (&aux (g (grph:grph)))
  (time (grph:modify! (g grp)
          (loop with n = 80000 repeat 200000
                do (rnd:prob 0.4 (grp-> (rnd:rndi n) (rnd:rndi n) '(:path))
                                 (grp<> (rnd:rndi n) (rnd:rndi n)
                                        `(:path :path2 ,(rsym)))))))
  g)

(defun main (&aux (g (get-sample-graph)))
  (print g)
  (time (print (length
                 (grph:qry g
                   :select (?x ?p ?y) :where (?x ?p ?y)))))
  (time (print (length
                 (grph:qry g :db :full
                   :select (?x ?y) :where (or (?x :path ?y)
                                              (?x :path2 ?y))))))
  (time (grph/io:gwrite "tmp" g))
  (time (print (grph/io:gread "tmp"))))

(main)

