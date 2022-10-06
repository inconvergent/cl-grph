(in-package #:grph-tests)

(plan 2)


(subtest "grph"
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

(subtest "grph match"
  (let ((g (mk-grph-match)))

    (is (grph:gather-match g 0 ?a ?b) '(((?A . :A) (?B . 3)) ((?A . :A) (?B . 1))))
    (is (grph:gather-match g 0 :a ?b) '(((?B . 3)) ((?B . 1))))
    (is (grph:gather-match g 2 ?b 3) '(((?B . :B)) ((?B . :A))))
    (is (grph:gather-match g ?b :b 0) '(((?B . 33)) ((?B . 6))))
    (is (grph:gather-match g ?b :b ?a)
        '(((?A . 0) (?B . 33)) ((?A . 9) (?B . 8))
          ((?A . 0) (?B . 6)) ((?A . 3) (?B . 2))))
    (is (grph:gather-match g ?b ?a 3)
        '(((?A . :A) (?B . 4)) ((?A . :B) (?B . 2))
          ((?A . :A) (?B . 2)) ((?A . :A) (?B . 0))))
    (is (grph:gather-match g ?a ?b ?c)
        '(((?A . 33) (?B . :B) (?C . 0)) ((?A . 9) (?B . :A) (?C . 0))
          ((?A . 8) (?B . :B) (?C . 9)) ((?A . 7) (?B . :A) (?C . 8))
          ((?A . 6) (?B . :B) (?C . 0)) ((?A . 5) (?B . :_) (?C . 6))
          ((?A . 4) (?B . :A) (?C . 3)) ((?A . 3) (?B . :A) (?C . 4))
          ((?A . 2) (?B . :B) (?C . 3)) ((?A . 2) (?B . :A) (?C . 3))
          ((?A . 0) (?B . :A) (?C . 3)) ((?A . 0) (?B . :A) (?C . 1))))
    (is (grph:gather-match g ?a ?b ?c)
        '(((?A . 33) (?B . :B) (?C . 0)) ((?A . 9) (?B . :A) (?C . 0))
          ((?A . 8) (?B . :B) (?C . 9)) ((?A . 7) (?B . :A) (?C . 8))
          ((?A . 6) (?B . :B) (?C . 0)) ((?A . 5) (?B . :_) (?C . 6))
          ((?A . 4) (?B . :A) (?C . 3)) ((?A . 3) (?B . :A) (?C . 4))
          ((?A . 2) (?B . :B) (?C . 3)) ((?A . 2) (?B . :A) (?C . 3))
          ((?A . 0) (?B . :A) (?C . 3)) ((?A . 0) (?B . :A) (?C . 1))))))

(unless (finalize) (error "error in grph"))

