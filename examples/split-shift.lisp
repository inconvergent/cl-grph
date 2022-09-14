#!/usr/local/bin/sbcl --script

(require :sb-sprof)
(load "~/quicklisp/setup.lisp")
(ql:quickload :weird)
(ql:quickload :grph)
(rnd:set-rnd-state 7)

(veq:fvdef walker ((:va 2 acc))
  (rnd:2walker-acc (veq:f2+ (veq:f2rep 500f0) (rnd:2in-square 400f0)) acc))
(veq:fvdef walker-lerp ((:va 4 wa wb) xe)
  (veq:f2scale (veq:f2lerp (veq:fsel (2 3 6 7) wa wb) xe) 0.9f0))

(veq:fvdef main (size fn)
  (declare (optimize speed (safety 1)))
  (veq:f2let ((wacc (rnd:2in-circ 1.5f0)))
    (let* ((s (xgrph:pos)) (g (grph:grph))
           (stp 2.3)
           (left 150f0) (right (- 1000f0 left))
           (wsvg (wsvg:make*))
           (a (walker wacc)) (b (walker wacc)))

      (veq:f$fxlspace (160 left right :end t)
        (lambda (i x &aux (gs (gensym)))
          (xgrph:2path! g s
            (veq:f2$line left x right x) (grph:grp gs))))

      (loop repeat (rnd:rndrngi 100 300)
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
                           (xgrph:2split! ^g ^s ?a ?b
                             (veq:f2- hit shift)
                             (grph:grp (grph:@prop g (list ?a ?b) :g)))))))))

      (wsvg:rect wsvg 502 502 :xy (list 500f0 500f0) :fill "black" :fo 0.9)
      ; (with-prof (:mode :time)
      (grph:qry g
        :select (?a ?b)
        :where (and (?a _ ?b) (not (?c _ ?a)))
        :then (wsvg:path wsvg
                (xgrph:2@verts s
                  (grph::qry-collect-while (g :init (list ?a))
                     :select (?n) :in (?a)
                     :where (?a _ ?n)
                     :first (progn (setf ?a ?n) ?n)))
                :stroke "white" :so 0.9))
      ; )
      (print (grph:@enum g))
      (wsvg:save wsvg fn))))

(time (main 1000 (second (weird:cmd-args))))

; 1320
; Evaluation took:
;   3.912 seconds of real time
;   3.900056 seconds of total run time (3.896237 user, 0.003819 system)
;   [ Run times consist of 0.039 seconds GC time, and 3.862 seconds non-GC time. ]
;   99.69% CPU
;   197 lambdas converted
;   14,444,668,169 processor cycles
;   578,606,784 bytes consed

; (sb-sprof:with-profiling (:max-samples 200000
;                         :mode :cpu
;                         ; :mode :alloc
;                         ; :mode :time
;                         :report :graph)
; (time (main 1000 (second (weird:cmd-args)))))

; (defmacro with-prof ((&key (mode :cpu) (mx 50000) (report :graph))  &body body)
;   `(sb-sprof:with-profiling (:max-samples ,mx
;                              :mode ,mode
;                              :report ,report)
;                             (progn ,@body)))

