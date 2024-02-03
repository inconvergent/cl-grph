#!/usr/local/bin/sbcl --script

(load "~/quicklisp/setup.lisp")
(ql:quickload :auxin) (ql:quickload :grph)

(rnd:set-rnd-state 1)

(veq:fvdef main (&aux (v (xgrph:pos)) (g (grph:make)))
  (xgrph:2path! g v
   (f2!@$+ (veq:f2$polygon 5 (veq:f2rep 200f0))
           (?@ (f2!@+ 10f0 20f0 (rnd:2in-circ 200f0))))
   (:closed))
  (xgrph:2path! g v (veq:f2$line 1f0 2f0 3f0 4f0) (:open) '(:xxx :yyy))
  (grph:add*! g 0 2 -> :xxx)
  (grph:add*! g -1 2 -> :xxx)

  (grph:prop! g -1 '(:xxx :aaa))
  (grph:prop! g 10 :xxx)
  (grph:prop! g 3 :xxx)
  (grph:prop! g -2 :aaa)
  (grph:prop! g -3 :bbb)

  (veq:vp :export g)
  (mapcar #'print (grph:qry g :select (?x ?p ?y) :where (?x ?p ?y)))
  (grph/io:gwrite "sample" g :pos v)
  (let ((g (grph/io:gread "sample")))
    (veq:vp :import g)
    (mapcar #'print (grph:qry g :select (?x ?p ?y) :where (?x ?p ?y)))))

(main)

