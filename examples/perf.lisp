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

; Evaluation took:
;   2.620 seconds of real time
;   2.619011 seconds of total run time (2.303833 user, 0.315178 system)
;   [ Real times consist of 0.160 seconds GC time, and 2.460 seconds non-GC time. ]
;   [ Run times consist of 0.162 seconds GC time, and 2.458 seconds non-GC time. ]
;   99.96% CPU
;   9,698,620,597 processor cycles
;   1,196,940,896 bytes consed


; <@grph: (v: 79479/79999, e: 319848, p: 12)>
; 799272
; Evaluation took:
;   0.760 seconds of real time
;   0.759587 seconds of total run time (0.759587 user, 0.000000 system)
;   100.00% CPU
;   26 lambdas converted
;   2,812,531,320 processor cycles
;   444,355,216 bytes consed


; 319848
; Evaluation took:
;   0.296 seconds of real time
;   0.356650 seconds of total run time (0.348825 user, 0.007825 system)
;   [ Real times consist of 0.044 seconds GC time, and 0.252 seconds non-GC time. ]
;   [ Run times consist of 0.043 seconds GC time, and 0.314 seconds non-GC time. ]
;   120.61% CPU
;   1,098,349,366 processor cycles
;   258,547,888 bytes consed

; Evaluation took:
;   1.124 seconds of real time
;   1.116539 seconds of total run time (1.108820 user, 0.007719 system)
;   99.38% CPU
;   60 lambdas converted
;   4,159,008,532 processor cycles
;   444,030,048 bytes consed


; <@grph: (v: 79479/79999, e: 319848, p: 12)>
; Evaluation took:
;   2.892 seconds of real time
;   2.888066 seconds of total run time (2.784261 user, 0.103805 system)
;   [ Real times consist of 0.292 seconds GC time, and 2.600 seconds non-GC time. ]
;   [ Run times consist of 0.291 seconds GC time, and 2.598 seconds non-GC time. ]
;   99.86% CPU
;   44 lambdas converted
;   10,691,664,226 processor cycles
;   1,810,936,896 bytes consed

