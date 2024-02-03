#!/usr/local/bin/sbcl --script

(load "~/quicklisp/setup.lisp")
(ql:quickload :auxin)
(setf lparallel:*kernel* (lparallel:make-kernel 2))

(defvar *colors* `((:yellow 1.0) (:magenta 1.0)
                   (:cyan 1.0) (:white 3.5)))

(veq:fvdef make-svg (&optional (wsvg (wsvg:make*)))
  (wsvg:rect wsvg 502 502 :xy (list 500f0 500f0) :fill :black :fo 0.96)
  wsvg)

(veq:fvdef* walker ((:va 2 acc))
  (rnd:2walker-acc (f2!@+ (veq:f2rep 500f0) (rnd:2in-square 400f0)) acc))

(veq:fvdef walker-lerp ((:va 4 wa wb) xe)
  (f2!@*. (veq:f2lerp (veq:fsel (2 3 6 7) wa wb) xe) 0.9f0))

(veq:fvdef main (size fn)
  (let* ((wsvg (make-svg)) (s (xgrph:pos)) (g (grph:grph))
         (stp (rnd:rndrng 1.0 2.0))
         (left 150f0) (right (- 1000f0 left)))

    (veq:f$fxlspace (145 left right :end t)
      (lambda (i x) (xgrph:2path! g s (veq:f2$line left x right x)
                      -> :white)))

    (loop for (c sw) in *colors*
          for a = (walker (rnd:2in-circ 1.5f0))
          for b = (walker (rnd:2in-circ 1.5f0)) do
      (loop repeat (rnd:rndrngi 10 40)
        for itt from 0
        do (veq:xlet ((f4!wa (f@a stp)) (f4!wb (f@b stp)))
             (grph:qry g :select (?a ?b)
               :where (?a _ ?b)
               :using (^g ^s) ; mutating these variables
               :then (veq:mvb (isect xl xe)
                       (veq:f2segx (xgrph:2@ s ?a ?b) (veq:fsel (0 1 4 5) wa wb))
                       (when isect
                         (veq:f2let ((hit (veq:f2lerp (xgrph:2@ s ?a ?b) xl))
                                     (shift (walker-lerp wa wb xe)))
                           (progn
                             (grph:del! ^g ?a ?b)
                             (xgrph:2append! ^g ^s ?a (veq:f2 hit) :abs :white)
                             (xgrph:2append! ^g ^s ?b (f2!@+ hit shift)
                                             (abs <-) `(,c)))))))))
      (format t "~&drawing: ~a~%" (grph:@enum g))
      (loop for (path closed) in (grph:walk-grph g)
         do (wsvg:path wsvg (xgrph:2@verts s path)
                      :closed closed :stroke c)))

    (wsvg:save wsvg "spatial")))

(time (main 1000 (second (auxin:cmd-args))))

