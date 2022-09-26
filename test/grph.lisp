(in-package #:grph-tests)

(plan 1)


(defun mk-grph-main ()
  (let ((g (grph:grph))
        (bprop  '((:b "90"))))
    (grph:add! g 0 1)
    (grph:add! g 2 3)
    (grph:add! g 3 4 '(:a))
    (grph:add! g 4 3 '((:a "43")))
    (grph:add! g 5 6)
    (grph:add! g 6 0 '((:a "60")))
    (grph:add! g 7 8 '(:c))
    (grph:add! g 8 9 '(:b))
    (grph:add! g 9 0 bprop)
    (grph:add! g 7 8 '(:b))
    (grph:add! g 0 3)
    g))

(subtest "grph-main"
  (let ((g (mk-grph-main)))
    (is (grph:@edges g)
        '((9 0) (8 9) (7 8) (6 0) (5 6) (4 3) (3 4) (2 3) (0 3) (0 1)))
    (is (grph:@verts g) '(9 8 7 6 5 4 2 3 1 0))
    (is (grph:@out g 3) '(4))
    (is (grph:@in g 3) '(2 0))

    (is (grph:@mem g 7 8) t)
    (is (grph:@prop g (list 7 8)) (fset:map (:c t)) :test #'equalp)
    (grph:del! g 7 8)
    (is (grph:@mem g 7 8) nil)
    (grph:del! g 2 3)
    (is (grph:@in g 3) '(0))

    (is (grph:@prop g (list 4 3)) (fset:map (:a "43")) :test #'equalp)
    (is (grph:@prop g (list 4 3) :a) "43")
    (is (grph:@prop g (list 7 8)) nil)
    (is (grph:@verts g) '(9 8 6 5 4 3 1 0))))

(unless (finalize) (error "error in grph"))

