(in-package #:grph-tests)

(plan 3)

(subtest  "qry nested"
  (let* ((gg (make-edge-set)) (xg gg))
    (is (ls (grph:qry gg :select (?a)
                         :where (and (?a _ _)
                                     (q :select ?a
                                        :where (?a :a _)))))
        (ls '((0) (1) (2) (3))))

    (is (rs (grph:qry gg :select ?b
              :where (and (or (?b _ _) (_ _ ?b))
                          (not-join ?b
                            (q :select (?a ?b ?c)
                               :where (and (?a _ ?b) (?b _ ?c)
                                           (% (/= ?a ?b ?c))))))))
        (rs '(0 2 7 77 99)))
    (is (rs (grph:qry gg :select ?b
              :where (and (or (?b _ _) (_ _ ?b))
                          (not-join ?b
                            (q xg :select (?a ?b ?c)
                                  :where (and (?a _ ?b) (?b _ ?c)
                                              (% (/= ?a ?b ?c))))))))
        (rs '(0 2 7 77 99)))))

(subtest "qry using"
  (let ((g (make-edge-set)))

    (is (grph:qry g :using ^g
                    :select (?a ?b)
                    :where (and (?a _ ?b) (?b _ ?a)
                                (% (grph:first> ?a ?b)))
                    :collect (grph:del! ^g ?a ?b))
        '(t t t t t t t))

    (is (ls (grph:qry g :select (?a ?b) :where (?a _ ?b) :collect `(,?a ,?b)))
        (ls '((99 77) (4 5) (3 7) (3 5) (3 4) (1 3) (1 2) (0 1))))

    (let ((gg g))
      (is (grph:qry g :using ^g :select (?a ?b) :where (?a _ ?b)
                      :collect (grph:del! ^g ?a ?b))
          '(t t t t t t t t))

    (is (grph:qry g :select (?a ?b) :where (?a _ ?b) :collect `(,?a ,?b)) nil)
    (is (ls (grph:@edges gg))
        (ls '((99 77) (4 5) (3 7) (3 5) (3 4) (1 3) (1 2) (0 1))))
    (is (grph:@edges g) nil))))


; (subtest "qry using cancel/stop"
;   (let ((g (make-edge-set)))
;     (is (let ((i 0) (var :xxx))
;           (grph:qry g :using (^var) :select (?a ?b)
;                       :where (?a _ ?b)
;                       :then (progn (setf ^var (list i ?a ?b))
;                                    (if (> i 1) (grph:cancel))
;                                    (incf i)))
;           (list var i))
;         '(:xxx 2))
;     (is (let ((i 0) (var (list)))
;           (grph:qry g :using ^var
;                       :select (?a ?b)
;                       :where (?a _ ?b)
;                       :then (progn (if (> i 3) (grph:stop))
;                                    (push (list i ?a ?b) ^var)
;                                    (incf i)))
;           (is i 4)
;           (ls var))
;         (ls '((0 0 1) (1 1 0) (2 1 2) (3 1 3))))) )

; (subtest "qry walk"
;   (let ((g (grph:grph)) (?a 3))
;     (setf g (grph:ingest-edges
;              `((0 :A 1) (0 :C 1) (1 :A 3) (1 :A 2) (1 :A 0) (1 :C 0)
;                (2 :A 1) (3 :C 7) (3 :B 5) (3 :C 5) (3 :B 4) (3 :A 1)
;                (4 :B 3) (4 :B 5) (4 :E 5) (5 :B 3) (5 :C 3) (5 :B 4)
;                (5 :E 4) (7 :C 3) (99 :X 77)) g))
;     ; this is contrived, but it tests specific behaviour of using combined
;     ; with qry-collect-while which might be useful.
;     (is (grph:qry-collect-while g :lim 10 :init (list ?a)
;                                   :using ^g :in ?a :select ?n
;                                   :where (?a _ ?n)
;                                   :first (when (and ?n (not (= ?n 4)))
;                                                (grph:del! ^g ?a ?n)
;                                                (setf ?a ?n)
;                                                ?n))
;         '(3 1 0 1 2 1 3)
;     (is (ls (grph:@edges g))
;         (ls '((99 77) (5 4) (5 3) (4 5) (4 3) (3 4)
;               (3 1) (2 1) (1 3) (1 2) (1 0) (0 1))))))


(subtest "aggs"
 (let ((g (grph:ingest-edges
            '((0 :a 1) (0 :a 3) (1 :a 2) (1 :a 3)
              (3 :a 0) (3 :a 6) (4 :a 5) (6 :a 3)))))

   (is (grph:qry g :select (?c (cnt ?x)) :where (?x ?c _)
                   :collect (list ?c (cnt ?x)))
       '((:A 5)))

   (is (grph:qry g :select ((cnt ?y ?x)) :where (or (?x _ ?y) (?y _ ?x )))
       '((12)))

   (is (ls (grph:qry g :select (?x (cnt ?y)) :where (?x _ ?y)))
       (ls '((6 1) (4 1) (3 2) (1 2) (0 2))))

   (is (ls (grph:qry g :select (?y (cnt ?x)) :where (?x _ ?y)))
       (ls '((3 3) (5 1) (6 1) (0 1) (2 1) (1 1))))

  (is (grph:qry g :select (?x (grp ?y ?z))
                  :where (and (?x :a ?y) (?y :a ?z))
                  :collect (list ?x (ls (grp ?y ?z))))
       '((3 ((0 1) (0 3) (6 3))) (6 ((3 0) (3 6))) (1 ((3 0) (3 6)))
         (0 ((1 2) (1 3) (3 0) (3 6)))))

  (is (grph:qry g :select (?x (grp ?y ?z) (cnt ?y ?z))
                  :where (and (?x :a ?y) (?y :a ?z)))
      '((3 ((0 1) (0 3) (6 3)) 3) (6 ((3 0) (3 6)) 2) (1 ((3 0) (3 6)) 2)
        (0 ((1 2) (1 3) (3 0) (3 6)) 4)))))

(unless (finalize) (error "error in QRY (2) ADVANCED."))

