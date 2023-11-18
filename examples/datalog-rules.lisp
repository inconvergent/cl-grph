#!/usr/local/bin/sbcl --script

(load "~/quicklisp/setup.lisp")
(ql:quickload :auxin)

(defvar *s* 1400f0)
(defvar *m* (* 0.5 *s*))

(rnd:set-rnd-state 1)


; (defun walk-edge-set-segments (g es)
;   (labels ((rec (pp)
;              (unless (> (length pp) 1) (return-from rec))
;              (loop for i from 1 for (vi b) in pp
;                    if (not b) do (return-from rec
;                                    (cons (veq:lpos (subseq pp 0 (1+ i)))
;                                          (rec (subseq pp i)))))
;              (list (veq:lpos pp)))
;            (with-2count (p)
;              (loop for v in p collect (list v (= 2 (grph:num-either g v)))))
;            (split-paths (p c)
;              (rec (with-2count (if c (math:close-path p) p))))
;            (closed? (p) (if (= (first p) (math:last* p))
;                             (list (butlast p) t) (list p nil))))
;    (loop with res = (list)
;          for (p c) in (grph:walk-edge-set g es)
;          do (loop for p in (split-paths p c)
;                   do (push (closed? p) res))
;          finally (return res))))

(veq:fvdef main (&aux (v (xgrph:pos)) (g (grph:make)))
  (xgrph:2path! g v (f2!@$+ (veq:f2$polygon 5 200f0)
                            (?@ (f2!@+ *m* *m* (rnd:2in-circ 200f0))))
                (:closed))

  (xgrph:2path! g v (f2!@$+ (veq:f2$polygon 5 200f0)
                            (?@ (f2!@+ *m* *m* (rnd:2in-circ 200f0))))
                (:closed))

  (xgrph:2path! g v (f2!@$+ (veq:f2$polygon 3 200f0)
                            (?@ (f2!@+ 1100f0 1100f0 (rnd:2in-circ 1f0))))
                (:closed))

  (xgrph:2path! g v (veq:f2$line 200f0 200f0 800f0 800f0) (:open)
                '(:xxx :yyy))
  (xgrph:2intersect-all! g v)
  ; (mapcar #'print (grph:walk-edge-set g (grph:edge-set g)))

  (print (grph:walk-grph-segments g))

  (grph/io:gwrite-script ("rules" g :pos v)
    (let ((wsvg (wsvg:make* :height #.*s* :width #.*s* :sw 1.25 :stroke :k))
          (rs (srnd:make 473)))
      (wsvg:rect wsvg 2000f0 2000f0 :xy (math:nrep 2 #.*m*) :fill :white)
      (wsvg/qry:selectors (g v wsvg)
        (.&ws ?x ?y (?x _ ?y) (.&path :sw 1.75 :so 0.85))
        (.&ww ?x ?y (?x _ ?y) (.&path :sw 3.75 :so 0.25 :stroke :red))
        ; (.&ws ?x ?y (?x _ ?y) (.&path :sw 1.75 :so 0.85))
        ; (.&ws ?x ?y (?x :stiple ?y) (.&stip 2f0 1.3 :sw 0.75 :so 0.7))
        (.&vs ?x (and (or (?x _ ?y) (?y _ ?x)) (%= (grph:num-either g ?x) 1))
                 (.&circ 2f0 :fill :k :so 0.8 :fo 0.8))
        (.&vs ?x (and (or (?x _ ?y) (?y _ ?x)) (% (= (grph:num-either g ?x) 2)))
              (.&circ 6f0 :stroke :b :fill nil :so 0.8 :fo 0.8))
        (.&vs ?x (and (or (?x _ ?y) (?y _ ?x)) (% (= (grph:num-either g ?x) 3)))
                 (.&circ 12f0 :stroke :r :fill nil :so 0.8 :fo 0.8))
        (.&vs ?x (and (or (?x _ ?y) (?y _ ?x)) (% (= (grph:num-either g ?x) 4)))
                 (.&circ 20f0 :stroke :r :fill nil :so 0.8 :fo 0.8)))
    (wsvg:save wsvg :fn))))

(main)

