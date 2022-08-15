(in-package #:grph-tests)

(plan 1)


(defun mk-grph ()
  (let ((g (grph:grph)))
    (grph:add! g 0 1)
    (grph:add! g 2 3)
    (grph:add! g 3 4)
    (grph:add! g 4 3)
    (grph:add! g 5 6)
    (grph:add! g 6 0)
    (grph:add! g 7 8)
    (grph:add! g 8 9)
    (grph:add! g 9 0)
    (grph:add! g 7 8)
    (grph:add! g 0 3)
    g))

(subtest "grph"

  (let ((g (mk-grph)))

    (is (grph:@edges g)
        '((9 0) (8 9) (7 8) (6 0) (5 6)
          (4 3) (3 4) (2 3) (0 3) (0 1)))
    (is (grph:@verts g) '(0 9 8 0 6 3 4 3 3 0))
    (is (grph:@out g 3) '(4))

    (is (grph:@mem g 7 8) t)
    (grph:del! g 7 8)
    (is (grph:@mem g 7 8) nil)
    ))

(unless (finalize) (error "error in grph"))
