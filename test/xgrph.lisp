(in-package #:grph-tests)

(plan 1)

(veq:fvprogn

(subtest "xgrph"
  (let* ((pos (xgrph:pos 999f0))
         (g (grph:grph)))

  (xgrph:2vert! pos (veq:f2 3f0 3f0))

  (is (xgrph:2path! g pos (veq:f$_ '((1f0 3f0) (340f0 20f0) (83f0 2f0)
                                     (0f0 3f0) (33f0 8f0) (6f0 8f0)))
                        '(:a))
      '(1 2 3 4 5 6))
  (is (xgrph:2@verts pos (list 4 1 3)) #(0.0 3.0 1.0 3.0 83.0 2.0)
      :test #'equalp)

  (is (veq:lst (xgrph:2move! pos 0 (veq:f2 0f0 1f0) :rel)) '(3f0 4f0))
  (is (veq:lst (xgrph:2move! pos 0 (veq:f2 0f0 1f0) :abs)) '(0f0 1f0))
  (is (veq:lst (xgrph:2move! pos 0 (veq:f2 0.5f0 1f0))) '(0.5f0 2f0))
  (is (veq:lst (xgrph:2move! pos 2 (veq:f2 0.5f0 1f0))) '(340.5 21.0))
  (is (veq:lst (xgrph:2@vert pos 3)) '(83.0 2f0))


  (is (grph:@mem g 3 7) nil)
  (is (xgrph:2append! g pos 3 (veq:f2 3f0 7f0)) 7)
  (is (grph:@mem g 3 7) t)
  (is (veq:lst (xgrph:2@vert pos 7)) '(86.0 9.0))

  (xgrph::%vset! 1 pos 17 (veq:f 1f0))
  (is (veq:lst (xgrph:@vert 1 pos 17)) '(1f0))
  (is (veq:lst (xgrph:@vert 1 pos 16)) '(999f0))

  )))


(unless (finalize) (error "error in XGRPH"))

