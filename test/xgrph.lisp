(in-package #:grph-tests)

(plan 1)

(veq:fvprogn

(subtest "xgrph"
  (let* ((s (fset:empty-seq))
         (g (grph:grph)))

  (xgrph:2vert! s (veq:f2 3f0 3f0))

  (is (xgrph:2path! g s (veq:f$_ '((1f0 3f0) (340f0 20f0) (83f0 2f0)
                                       (0f0 3f0) (33f0 8f0) (6f0 8f0)))
                        '(:a))
      '(1 2 3 4 5 6))
  (is (xgrph:2@verts s (list 4 1 3)) #(0.0 3.0 1.0 3.0 83.0 2.0)
      :test #'equalp)

  (is (veq:lst (xgrph:move! 2 s 0 (veq:f2 0f0 1f0) :rel)) '(3f0 4f0))
  (is (veq:lst (xgrph:move! 2 s 0 (veq:f2 0f0 1f0) :abs)) '(0f0 1f0))
  (is (veq:lst (xgrph:2move! s 0 (veq:f2 0.5f0 1f0))) '(0.5f0 2f0))
  (is (veq:lst (xgrph:move! 2 s 2 (veq:f2 0.5f0 1f0))) '(340.5 21.0))
  (is (veq:lst (xgrph:2@vert s 3)) '(83.0 2f0))

  (is (grph:@mem g 3 7) nil)
  (is (xgrph:2append! g s 3 (veq:f2 3f0 7f0)) 7)
  (is (grph:@mem g 3 7) t)
  (is (veq:lst (xgrph:2@vert s 7)) '(86.0 9.0)))))


(unless (finalize) (error "error in xgrph"))
