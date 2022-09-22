#!/usr/local/bin/sbcl --script

; (require :sb-sprof)
(load "~/quicklisp/setup.lisp")
(ql:quickload :weird)
(ql:quickload :grph)
(defvar *back* :white)
(defvar *front* :black)
; (rnd:set-rnd-state 7)

(veq:fvdef make-svg (&optional (wsvg (wsvg:make*)))
  ; (wsvg:rect wsvg 502 502 :xy (list 500f0 500f0) :fill *back* :fo 0.96)
  wsvg)
(veq:fvdef* walker ((:va 2 acc))
  (rnd:2walker-acc (veq:f2+ (veq:f2rep 500f0) (rnd:2in-square 400f0)) acc))
(veq:fvdef walker-lerp ((:va 4 wa wb) xe)
  (veq:f2scale (veq:f2lerp (veq:fsel (2 3 6 7) wa wb) xe) 0.9f0))

(veq:fvdef main (size fn)
  (declare (optimize speed (safety 1)))
  (veq:f2let ((wacc (rnd:2in-circ 1.5f0)))
    (let* ((wsvg (make-svg)) (s (xgrph:pos)) (g (grph:grph))
           (stp (rnd:rndrng 1.0 2.0))
           (left 150f0) (right (- 1000f0 left))
           (a (walker wacc)) (b (walker wacc))
           (colors `((:yellow 1.0) (:magenta 1.0) (:cyan 1.0) (,*front* 3.5)))
           )

      (veq:f$fxlspace (160 left right :end t)
        (lambda (i x) (xgrph:2path! g s (veq:f2$line left x right x)
                                    (grph:grp *front* :color)
                                    )))


      (loop for (c sw) in (butlast colors)
            for a = (walker (rnd:2in-circ 1.5f0))
            for b = (walker (rnd:2in-circ 1.5f0))
        ; (list  "yellow" "magenta" "cyan" "black")
        do (loop repeat (rnd:rndrngi 10 40)
        for itt from 0
        do (veq:fvlet ((wa 4 (funcall a stp)) (wb 4 (funcall b stp)))
             (grph:qry g
               :using (^g ^s)
               :select (?a ?b)
               :where (?a _ ?b)
               :then (veq:mvb (isect xl xe)
                       (veq:f2segx (xgrph:2@ s ?a ?b) (veq:fsel (0 1 4 5) wa wb))
                       (when isect
                         (veq:f2let ((hit (veq:f2lerp (xgrph:2@ s ?a ?b) xl))
                                     (shift (walker-lerp wa wb xe)))

                           ; (xgrph:2split! ^g ^s ?a ?b (veq:f2 hit))
                             (progn (grph:del! ^g ?a ?b)
                                  (xgrph:2append! ^g ^s ?a (veq:f2 hit) :abs
                                                  (grph:grp *front* :color))
                                  (xgrph:2append! ^g ^s ?b (veq:f2+ hit shift)
                                                  (:abs <-)
                                                  (grph:grp c :color)))))))))


        (format t "~&drawing: ~a~%" (grph:@enum g))
        ; (grph:qry g
        ;   :select (?a ?b)
        ;   :where (and (?a _ ?b) (not (?c _ ?a)))
        ;   :then (wsvg:path wsvg
        ;           (xgrph:2@verts s
        ;             (grph:qry-collect-while g
        ;                :init (list ?a)
        ;                :select ?n :in ?a
        ;                :where (?a _ ?n)
        ;                :first (progn (setf ?a ?n) ?n)))
        ;           :stroke c :so 0.95 :sw sw))
          )

        (loop for (?p sw) in colors
          do (grph:qry g
          :select (?a ?b)
          :in ?p
          :where (and (?a ?p ?b) (not (?c _ ?a)))
          :then (progn 1)
          ; (progn ; (print (list ?a ?p ?b))
          ;   (wsvg:path wsvg
          ;         (xgrph:2@verts s
          ;           (grph:qry-collect-while g
          ;              :init (list ?a)
          ;              :select ?n :in ?a
          ;              :where (?a _ ?n)
          ;              :first (progn (setf ?a ?n) ?n)))
          ;         :stroke :black :so 0.95 :sw sw))
          ))

      (print :itre)
      ; (grph:itr-edges (g e)
      ;   (wsvg:path wsvg (xgrph:2@verts s (print e)) :stroke "red"))
      (print :i)
      (wsvg:save wsvg fn)
      )))

(time (main 1000 (second (weird:cmd-args))))

