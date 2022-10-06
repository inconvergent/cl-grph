(in-package #:grph-tests)

(plan 2)

(subtest "qry basic"
  (let ((g (make-edge-set)))

    (is (ls (grph:qry g :select (?x ?y) :where (and (?x :a ?y) (?x :c ?y))))
        (ls '((0 1) (1 0))))
    (is (ls (grph:qry g :select (?x ?y ?z) :where (and (?x :a ?y) (?y :b ?z))))
        (ls '((1 3 4) (1 3 5))))

    (is (ls (grph:qry g :select (?x ?y)
                        :where (and (?x :a ?y) (?x :c ?y) (not (?x :c ?y)))))
          nil)
    (is (ls (grph:qry g :select (?x ?y)
                      :where (and (?x :c ?y) (?x :a ?y) (not (?x :b _)))))
        (ls '((0 1) (1 0))))
    (is (ls (grph:qry g :select (?x ?y)
                        :when (< ?x ?y)
                        :where (or (?x :e ?y) (?x :a ?y))))
        (ls '((0 1) (1 2) (1 3) (4 5))))

    (is (let ((?x 5))
          (grph:qry g :select ?y :in ?x :where (or (?x :e ?y) (?x :a ?y))))
        `((4)))

      (is (ls (grph:qry g :select ?x
                          :where (and (not (?x :c _))
                                      (or (?x :a _) (?x :b _))
                                      (not (?x :e _)))))
          (ls '((2))))
      (is (ls (grph:qry g :select ?x
                          :where (and (or (?x :a _) (?x :b _))
                                      (not (?x :e 4) (?x :b 4))
                                      (not (?x :e 5) (?x :b 5))
                                      (not (?x :c 7)))))
          (ls '((0) (1) (2))))

    (is (ls (grph:qry g :select (?x ?y) :where (and (?x :c ?y) (not (7 :c ?y)))))
        (ls '((3 7) (3 5) (1 0) (0 1))))
    (is (ls (grph:qry g :select (?x ?y)
                        :where (and (?x :c ?y) (not (or (?x :a 1) (?x :a 3))))))
          (ls '((7 3) (5 3))))
    (is (ls (grph:qry g :select (?x ?y)
                        :where (and (or (?x :b ?y) (?x :a ?y))
                                    (not (or (?x :c ?y) (?x :e ?y))))))
        (ls '((1 2) (1 3) (2 1) (3 1) (4 3) (3 4))))
    (is (ls (grph:qry g :select (?x ?y)
                        :where (and (or (?x :b ?y) (?x :a ?y))
                                    (not (?x :c ?y) (?x :e ?y)))))
        (ls '((0 1) (1 0) (1 2) (1 3) (2 1) (3 1)
              (5 4) (5 3) (4 5) (4 3) (3 5) (3 4))))
    (is (ls (grph:qry g :select ?y :where (and (_ :a ?y) (not (_ :b ?y)))))
        (ls '((2) (0) (1))))
    (is (ls (grph:qry g :select (?x ?y) :where (or (?x :a ?y) (?x :b ?y))))
        (ls '((3 4) (3 5) (4 3) (4 5) (5 3) (5 4)
              (3 1) (2 1) (1 3) (1 2) (1 0) (0 1))))
    (is (ls (grph:qry g :select (?x ?y)
                        :where (and (or (?x :c ?y) (?x :a ?y)) (not (?x :a ?y)))))
        (ls '((7 3) (5 3) (3 7) (3 5))))
    (is (ls (grph:qry g :select (?x ?y)
                        :where (and (or (?x :a ?y) (?x :b ?y)) (not (?x :a ?y)))))
        (ls '((3 4) (3 5) (4 3) (4 5) (5 3) (5 4))))
    (is (ls (grph:qry g :select (?x ?y)
                        :where (and (or (?x :a ?y) (?x :b ?y)) (not (?x :a ?y)))
                        :collect (list (+ ?x ?y) 88 ?x ?y)))
        (ls '((7 88 3 4) (7 88 4 3) (8 88 3 5)
              (8 88 5 3) (9 88 4 5) (9 88 5 4))))
    (is (ls (grph:qry g :select (?x ?y) :where (or (?x :e ?y) (?x :a ?y))))
        (ls '((0 1) (1 0) (1 2) (1 3)
              (2 1) (3 1) (5 4) (4 5))))


    (is (ls (grph:qry g :select (?r) :where (and (?r :a _) (?a :b _))))
        (ls '((3) (2) (1) (0))))

    (is (ls (grph:qry g :select ?r
                        :where (or-join ?r (and (?r :a ?a) (?a :b 5))
                                           (?r :c 0)
                                           (?r :e 5))))
        (ls '((4) (1))))
    (is (ls (grph:qry g :select ?r
                        :where (and (?r _ _)
                                    (not-join (?r) (and (?r :a ?a)
                                                        (?a :b 5))))))
        (ls '((99) (7) (5) (4) (3) (2) (0))))))

(subtest "qry 2"
  (let ((g (grph:grph)))
    (loop for (a b) in '((0 1) (0 2) (1 0) (1 3) (3 1) (3 2) (2 0) (2 3))
          do (grph:add! g a b))

    (is (ls (grph:qry g :select (?a ?b) :where (and (not (?a _ 2)) (?a _ ?b))))
        (ls '((2 3) (2 0) (1 3) (1 0))))
    (is (ls (grph:qry g :select (?a ?b) :where (and (?a _ ?b) (not (?a _ 0)))))
        (ls '((3 2) (3 1) (0 2) (0 1))))))

(unless (finalize) (error "error in QRY BASIC."))

