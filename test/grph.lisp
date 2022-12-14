(in-package #:grph-tests)

(plan 3)


(subtest "grph add"
  (let ((g (grph:grph)))
    (grph:add! g 8 9)
    (grph:add! g 8 9 '(:a))
    (grph:add! g 8 6)
    (is (ls (grph:@edges g)) (ls '((8 9) (8 6))))

    (grph:path! g (list 55 66 77 88))
    (grph:path! g (list 188 177 166 155) <-)
    (grph:path! g (list 1881 1771 1661 1551) (<- closed))

    (is (ls (grph:@edges g))
        (ls '((8 6) (8 9)
              (55 66) (66 77) (77 88)
              (155 166) (166 177) (177 188)
              (1551 1661) (1661 1771) (1771 1881) (1881 1551))))

    (grph:add*! g 45 57 <>)
    (is (grph:@mem g 45 57) t)
    (is (grph:@mem g 57 45) t)

    (grph:add*! g 35 37 ->)
    (is (grph:@mem g 35 37) t)
    (is (grph:@mem g 37 35) nil)))


(subtest "grph"
  (let ((g (mk-grph-main)))
    (is (grph:@edges g)
        '((9 0) (8 9) (7 8) (6 0) (5 6) (4 3) (3 4) (2 3) (0 3) (0 1)))
    (is (grph:@verts g) '(9 8 7 6 5 4 2 3 1 0))
    (is (grph:@out g 3) '(4))
    (is (grph:@in g 3) '(4 2 0))

    (is (grph:@mem g 7 8) t)
    (is (grph:@prop g (list 7 8)) (fset:map (:c t ) (:b t)) :test #'equalp)
    (grph:del! g 7 8)
    (is (grph:@mem g 7 8) nil)
    (grph:del! g 2 3)
    (is (grph:@in g 3) '(4 0))

    (is (grph:@prop g (list 4 3)) (fset:map (:a "43")) :test #'equalp)
    (is (grph:@prop g (list 4 3) :a) "43")
    (is (grph:@prop g (list 7 8)) nil)
    (is (grph:@verts g) '(9 8 6 5 4 3 1 0))))

(subtest "modify"
  (let ((g (mk-grph-main))
        (edges '((0 1) (1 0) (0 4) (4 0) (6 77) (11 12) (11 12) (8 7))))
    (grph:modify! (g grp)
      (is (loop for (a b) in edges collect (grp-> a b '(:x)))
          '(nil (1 0) (0 4) (4 0) (6 77) (11 12) nil (8 7)))
      (is (loop for (a b) in edges collect (grph:@mem g a b))
          '(t nil nil nil nil nil nil nil)))
    (is (loop for (a b) in edges collect (grph:@mem g a b))
        '(t t t t t t t t)))

  (let ((g (grph:grph)))
    (grph:add! g 0 1 '((:a 77)))
    (grph:add! g 0 2 '((:b 87)))
    (grph:add! g 2 7 '((:c 96)))
    (grph:modify! (g grp)
      (grp-> 0 1 `((:b 3)))
      (grp-> 3 4 `((:c 4)))
      (grp-> 9 7 `((:y 77)))
      (grp-> 9 7 `(:u))
      (grp-> 3 4)
      (grp-> 1 0))

    (is (grph:@enum g) 6)
    (is (grph::props-as-list (grph:@prop g `(0 1))) '((:b 3) (:a 77)))
    (is (grph::props-as-list (grph:@prop g `(9 7))) '((:y 77) (:u t)))
    (is (grph::props-as-list (grph:@prop g `(3 4))) '((:c 4)))
    (is (grph:@mem g 0 2) t)))

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

