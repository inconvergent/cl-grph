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
  (wsvg:rect wsvg 502 502 :xy (list 500f0 500f0) :fill *back* :fo 0.96)
  wsvg)

(veq:fvdef center (verts max-side)
  (labels ((scale-by (ms w h)
             (declare (veq:ff ms w h))
             (if (> w h) (/ ms w) (/ ms h))))
    (let ((num-verts (veq:f2$num verts)))
    (veq:mvb (minx maxx miny maxy) (veq:f2$mima verts :n num-verts)
      (veq:f2let ((mx (veq:f2scale (veq:f2+ minx miny maxx maxy) 0.5f0))
                  (wh (veq:f2- maxx maxy minx miny)))
        (veq:~ mx wh (if max-side (scale-by max-side wh) 1f0)))))))


(defun get-active-center (dim g proj s)
  (let ((a (grph:qry g :select ?x
                       :where (or (?x :path _) (_ :path ?x))
                       :collect ?x) ))
    (center (proj-all proj
              (mapcar #'i->h (concatenate 'list a (invert-path dim a))))
            s)))


(veq:fvdef pos-center (g pos proj max-side)
  (grph:mvb ((:va 2 mx wh) s) (get-active-center 11 g proj max-side)
    (xgrph:fxpos! (g pos i)
      (veq:f2scale (veq:f2- (xgrph:2@vert pos i) mx) s)))
  pos)

(veq:fvdef stipple (path)
  (let* ((l (veq:f2dst (veq:f2$ path 0 1)))
         (n (* 2 (floor l 8.f0))))
    (let ((a (veq:f2$zero (* 1 n))))
     (veq:f$fxlspace (n 0f0 1f0)
       (lambda (i x) (veq:2$vset (a (* 1 i))
                       (veq:f2lerp (veq:f2$ path 0 1) x))))
     (loop for i from 0 below n by 2
           collect (veq:f2$line (veq:f2$ a i (1+ i)))))))


(veq:fvdef main (size fn)
  (declare (optimize speed (safety 1)))
  (veq:f2let ((mid (veq:f2 500f0 500f0)))
    (let* ((wsvg (make-svg))
           (g (grph:grph))
           (pos (xgrph:pos))
           (dim 11)
           (left 150f0) (right (- 1000f0 left))
           (proj (make-proj-circ dim 135f0)))

      (veq:mvb (nodes edges) (hypercube dim)
        (loop for (a b) in edges do (grph:add! g a b)))

      (loop repeat 2 ;(rnd:rndrngi 2 10)
            for hit = (rnd:rndget
                        (grph:qry g :select (?x ?y)
                                    :where (and (?x _ ?y) (not (?x :path ?y)))))
            for pinv = (invert-path dim hit)
            for pshft = (shift-path dim hit)
            do (grph:dsb (a b) hit (grph:add! g a b '(:path)))
               (grph:dsb (a b) pinv
                 (grph:add! g a b '(:path))))

      (loop repeat (rnd:rndrngi 4 12)
            for i from 0
            do (grph:dsb (x y a b)
                 (rnd:rndget (grph:qry g
                               :select (?x ?y ?a ?b) :db nil
                               :where (and (?x :path ?y) (?y _ ?a)
                                           (?a _ ?b) (?b _ ?x)
                                           (not (or-join (?a ?b)
                                                  (?y :path ?a) (?a :path ?b)
                                                  (?b :path ?x))))))
          (grph:del! g x y)
          (grph:add! g x y '(:delpath))

          (grph:add! g x a '(:path))
          (grph:add! g a b '(:path))
          (grph:add! g b y '(:path))))

        (xgrph:fxpos! (g pos i) (proj proj (i->h i)))
        (setf pos (pos-center g pos proj 910f0))

        ; (grph:itr-edges (g e)
        ;   (wsvg:path wsvg (veq:f2$+ (xgrph:2@verts pos e) 500f0 500f0)
        ;              :so 0.1 :sw 0.5))

        (loop for (p c) in (print (grph:walk-grph g :path))
              for pinv = (invert-path dim p)
              for pshft = (shift-path dim p)
              do (wsvg:path wsvg (veq:f2$+ (xgrph:2@verts pos p) mid)
                            :closed c :sw 1.9 :so 0.87)
                 (wsvg:path wsvg (veq:f2$+ (xgrph:2@verts pos pinv) mid)
                            :closed c :so 0.08 :sw 5.2)
                 ; (wsvg:path wsvg (veq:f2$+ (xgrph:2@verts pos pshft) mid)
                 ;            :closed c :so 0.12 :sw 3.2 :stroke :red)

                 (loop for a in (math:close-path* p)
                       and b in (cdr pinv) for i from 0
                       if t ;(evenp i)
                       do (loop for l in (stipple (veq:f2$+ (xgrph:2@verts pos (list a b)) mid))
                                do (wsvg:path wsvg l :so 0.05 :sw 1.7))
                      else do (wsvg:path wsvg
                                (veq:f2$+ (xgrph:2@verts pos (list a b)) mid)
                                 :so 0.1 :sw 1.2)))

        ; (stipple (veq:f2$+ (xgrph:2@verts pos (list 0 20)) mid))

        (grph:qry g :select ?x
          :where (or (?x :delpath _) (_ :delpath ?x))
          :then (let ((rad (1+ (grph::num-either g :path ?x))))
                  (wsvg:circ wsvg (case rad (1 4f0) (2 15f0) (otherwise 30f0))
                    :xy (veq:lst (veq:f2+ mid
                                   (xgrph:2@vert pos
                                     (first (invert-path dim (list ?x))))))
                    :fill (rnd:prob (/ rad 3.0) *front* nil)
                    :fo 0.7 :sw 2.2 :so 0.87)))

          (grph:qry g :select (?x ?y) :where (and (?x :delpath ?y)
                                                  (not (?x :path ?y)))
                    :then (rnd:prob 1f0
                  (wsvg:path wsvg (veq:f2$+ (xgrph:2@verts pos (list ?x ?y)) mid)
                            :closed nil :sw 20.0
                            ; :so 0.17
                            :so (rnd:prob 0.5 0.7 0.14)
                            )
                  ; (wsvg:jpath wsvg (veq:f2$+ (xgrph:2@verts pos p) mid)
                  ;             :width 20f0 :rs 0.61
                  ;             :sw 0.9 :so 0.1)
                  ))
        (print g)

      (wsvg:save wsvg fn))))

(time (main 1000 (second (weird:cmd-args))))

