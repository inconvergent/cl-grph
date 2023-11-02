(in-package #:grph-tests)

(plan 5)

(subtest "qry preproc"
  (is (cdr (assoc :where
             (grph::qry/preproc/in/where
               '((:where .
                   (and (not (?b _ _)) (not (?c _ _))
                        (not-join (?a ?b) (?c _ _)) (not-join ?a (?c _ _))
                        (% #'/= ?c) (?a _ ?b)
                        (and (and (?a _ _) (or (_ ?a ?c) (_ _ 2))))
                        (% #'/= ?b )
                        (or-join ?a (?a _ ?b))
                        (or-join (?a ?c) (?a _ ?b) (and (?xx ?h ?uu)))
                        (?c _ ?b) (?a _ ?s)
                        (or (?a _ ?b) (?c _ _))
                        (uniq ?a ?b)))))))
      '(:and (:not (:fact ?b _ _)) (:not (:fact ?c _ _))
             (:not-join (?a ?b) (:fact ?c _ _))
             (:not-join (?a) (:fact ?c _ _))
             (:% #'/= ?c) (:fact ?a _ ?b)
             (:and (:and (:fact ?a _ _)
                         (:or (:fact _ ?a ?c) (:fact _ _ 2))))
             (:% #'/= ?b)
             (:or-join (?a) (:fact ?a _ ?b))
             (:or-join (?a ?c) (:fact ?a _ ?b)
                       (:and (:fact ?xx ?h ?uu)))
             (:fact ?c _ ?b) (:fact ?a _ ?s)
             (:or (:fact ?a _ ?b) (:fact ?c _ _))
             (:uniq ?a ?b))))

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
                        :where (and (or (?x :e ?y) (?x :a ?y))
                                    (% (< ?x ?y)))))
        (ls '((0 1) (1 2) (1 3) (4 5))))

    (is (let ((?x 5))
          (rs (grph:qry g :select ?y :in ?x :where (or (?x :e ?y) (?x :a ?y)))))
        (rs `(4)))

      (is (rs (grph:qry g :select ?x
                          :where (and (not (?x :c _))
                                      (or (?x :a _) (?x :b _))
                                      (not (?x :e _)))))
          (rs '(2)))
      (is (rs (grph:qry g :select ?x
                          :where (and (or (?x :a _) (?x :b _))
                                      (not (?x :e 4) (?x :b 4))
                                      (not (?x :e 5) (?x :b 5))
                                      (not (?x :c 7)))))
          (rs '(0 1 2)))

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
    (is (rs (grph:qry g :select ?y :where (and (_ :a ?y) (not (_ :b ?y)))))
        (rs '(2 0 1)))
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

    (is (grph:qry g :select (?x ?y) :where (or (?x :e ?y) (?x :a ?y))
                    :first (list ?x ?y))
        '(3 1))

    (is (ls (grph:qry g :select (?x ?y)
                        :where (and (?x ?c 1) (1 ?c ?y)
                                    (f `(((?x . 0) (?y . 0))
                                         ((?x . 0) (?y . 2))
                                         ((?x . 2) (?y . 3))
                                         ((?x 888) (?y . 77)))))))
        (ls `((0 0) (0 2) (2 3))))))

(subtest "joins"
  (let ((g (make-edge-set)))
    (is (ls (grph:qry g :select (?x)
                        :where (and (or-join ?x (?x _ _) (_ _ ?x))
                                    (?x _ _)
                                    (not-join ?x (_ _ ?x )))))
        (ls `((99))))

    (is (rs (grph:qry g :select ?r
                        :where (or-join ?r (and (?r :a ?a) (?a :b 5))
                                           (?r :c 0)
                                           (?r :e 5))))
        (rs '(4 1)))
    (is (rs (grph:qry g :select ?r
                        :where (and (?r _ _)
                                    (not-join (?r) (and (?r :a ?a)
                                                        (?a :b 5))))))
        (rs '(99 7 5 4 3 2 0)))

    (is (rs (grph:qry g :select ?b
                  :where (and (or (?b _ _) (_ _ ?b))
                              (not-join ?b (?a _ ?b) (?b _ ?c) (% /= ?a ?c)))))
        (rs '(0 2 7 77 99)))
    (is (rs (grph:qry g :select ?b
                  :where (and (or (?b _ _) (_ _ ?b))
                              (not-join ?b (?a _ ?b) (?b _ ?c) (% (/= ?a ?c))))))
        (rs '(0 2 7 77 99)))
    (is (rs (grph:qry g :select ?b
                  :where (and (or (?b _ _) (_ _ ?b))
                              (not-join ?b (?a _ ?b) (?b _ ?c) (uniq ?a ?c)))))
        (rs '(0 2 7 77 99)))
    (is (rs (grph:qry g :select ?b
                  :where (and (or (?b _ _) (_ _ ?b))
                              (not-join ?b (?a _ ?b) (?b _ ?c) (uniq ?a ?c ?b)))))
        (rs '(0 2 7 77 99)))))

(subtest "qry 2"
  (let ((g (grph:grph)))
    (loop for (a b) in '((0 1) (0 2) (1 0) (1 3) (3 1) (3 2) (2 0) (2 3))
          do (grph:add! g a b))

    (is (ls (grph:qry g :select (?a ?b) :where (and (not (?a _ 2)) (?a _ ?b))))
        (ls '((2 3) (2 0) (1 3) (1 0))))
    (is (ls (grph:qry g :select (?a ?b) :where (and (?a _ ?b) (not (?a _ 0)))))
        (ls '((3 2) (3 1) (0 2) (0 1))))))

(subtest "qry nested or"
  (let ((g (grph:ingest-edges
             '((0 _ 1) (1 _ 0) (1 _ 4) (4 _ 1) (4 _ 7)
               (7 _ 4) (7 _ 6) (6 _ 7) (6 _ 3) (3 _ 6)
               (3 _ 0) (0 _ 3) (4 _ 5) (5 _ 4) (5 _ 2) (2 _ 5)))))

    (is (rs (grph:qry g :select ?x :where  (and (or (?x _ 2) (2 _ ?x)))))
        (rs '(5)))
    (is (ls (grph:qry g :select (?x ?y)
                        :where (and (?x _ ?y)
                                    (or (?x _ 2) (2 _ ?x)))))
        (ls '((5 2) (5 4))))
    (is (ls (grph:qry g :select (?x ?y)
                        :where  (and (?x _ ?y) (?x _ 2)
                                     (or (?x _ 2) (2 _ ?x)))))
        (ls '((5 2) (5 4))))
    (is (ls (grph:qry g :select (?x ?y)
                        :where  (and (?x _ ?y) (?x _ 2)
                                     (or (?x _ 2) (2 _ ?x))
                                     (% /= ?y 4))))
        (ls '((5 2))))))

(unless (finalize) (error "error in QRY BASIC."))

