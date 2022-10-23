#!/usr/local/bin/sbcl --script

(load "~/quicklisp/setup.lisp")
(let ((*features* `(:veq-reader-macros
                    :grph-parallel
                    ,@*features*)))
  (ql:quickload :weird)
  (ql:quickload :grph))

(load "./hypercube-util")
(defvar *back* :white)
(defvar *front* :black)
; (rnd:set-rnd-state 10)

(setf lparallel:*kernel* (lparallel:make-kernel 12))

(veq:fvdef make-svg (&optional (wsvg (wsvg:make*)))
  ; (wsvg:rect wsvg 502 502 :xy (list 500f0 500f0) :fill *back* :fo 0.96)
  wsvg)


(veq:fvdef main (size fn)
  (declare (optimize speed (safety 1)))
  (veq:f2let ((mid (veq:f2 500f0 500f0)))
    (let* ((wsvg (make-svg))
           (g (grph:grph))
           (dim 11)
           (left 150f0) (right (- 1000f0 left))
           (proj (make-proj-circ dim 1000f0)))

      (veq:mvb (nodes edges) (hypercube dim)
        (loop for (a b) in edges do (grph:add! g a b)))

      (loop repeat 1 ;(rnd:rndrngi 2 10)
            do (grph:dsb (a b)
                 (rnd:rndget
                   (grph:qry g :select (?x ?y)
                               :where (and (?x _ ?y) (not (?x :path ?y)))))
                 (grph:add! g a b '(:path))))

      (loop repeat (rnd:rndrngi 2 30)
            for i from 0
            do (grph:dsb (x y a b)
                 (rnd:rndget (grph:qry g
                               :select (?x ?y ?a ?b) :db nil
                               :where (and (?x :path ?y) (?y _ ?a)
                                           (?a _ ?b) (?b _ ?x)
                                           (not (or-join (?a ?b)
                                                  (?y :path ?a) (?a :path ?b)
                                                  (?b :path ?x))))))
          (when t (grph:del! g x y))
          (grph:add! g x a '(:path))
          (grph:add! g a b '(:path))
          (grph:add! g b y '(:path))))

      (let ((pos (make-pos g proj)))
        ; (grph:itr-edges (g e)
        ;   (wsvg:path wsvg (veq:f2$+ (xgrph:2@verts pos e) 500f0 500f0)
        ;              :so 0.1 :sw 0.5))
        (loop for (p c) in (print (grph:walk-grph g :path))
              do (wsvg:path wsvg (veq:f2$+ (xgrph:2@verts pos p) 500f0 500f0)
                            :closed c :sw 1.5)
                 (rnd:prob 1f0
                   (wsvg:path wsvg (veq:f2$+ (xgrph:2@verts pos
                                               (invert-path dim p))
                                             500f0 500f0)
                              :closed c :so 0.3 :sw 3.2))
                 ; (rnd:prob 1f0
                 ;   (wsvg:path wsvg (veq:f2$+ (xgrph:2@verts pos
                 ;                               (rot-path dim p))
                 ;                             500f0 500f0)
                 ;              :closed c :fo 0.1
                 ;              ; :fill (if c "black" "none")
                 ;              :so 0.3 :sw 15.2))
                 ))
        (print g)

      (wsvg:save wsvg fn))))

(time (main 1000 (second (weird:cmd-args))))

