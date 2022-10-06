(in-package #:grph-tests)

(plan 3)

(subtest "qry using"
  (let ((g (make-edge-set)))

    (is (grph:qry g :using ^g
                    :select (?a ?b)
                    :when (grph:first> ?a ?b)
                    :where (and (?a _ ?b) (?b _ ?a))
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


(subtest "qry using cancel/stop"
  (let ((g (make-edge-set)))
    (is (let ((i 0) (var :xxx))
          (grph:qry g :using (^var) :select (?a ?b)
                      :where (?a _ ?b)
                      :then (progn (setf ^var (list i ?a ?b))
                                   (if (> i 1) (grph:cancel))
                                   (incf i)))
          (list var i))
        '(:xxx 2))
    (is (let ((i 0) (var (list)))
          (grph:qry g :using ^var
                      :select (?a ?b)
                      :where (?a _ ?b)
                      :then (progn (if (> i 3) (grph:stop))
                                   (push (list i ?a ?b) ^var)
                                   (incf i)))
          (is i 4)
          (ls var))
        (ls '((3 5 3) (2 5 4) (1 7 3) (0 99 77))))) )

(subtest "qry walk"
  (let ((g (grph:grph)) (?a 3))
    (setf g (grph::ingest-facts g
             `((0 :A 1) (0 :C 1) (1 :A 3) (1 :A 2) (1 :A 0) (1 :C 0)
               (2 :A 1) (3 :C 7) (3 :B 5) (3 :C 5) (3 :B 4) (3 :A 1)
               (4 :B 3) (4 :B 5) (4 :E 5) (5 :B 3) (5 :C 3) (5 :B 4)
               (5 :E 4) (7 :C 3) (99 :X 77))))
    ; this is contrived, but it tests specific behaviour of using combined
    ; with qry-collect-while which might be useful.
    (is (grph:qry-collect-while g :lim 10 :init (list ?a)
                                  :using ^g :in ?a :select ?n
                                  :where (?a _ ?n)
                                  :first (when (and ?n (not (= ?n 4)))
                                               (grph:del! ^g ?a ?n)
                                               (setf ?a ?n)
                                               ?n))
        '(3 7 3 5))
    (is (ls (grph:@edges g))
        (ls '((99 77) (5 4) (5 3) (4 5) (4 3) (3 4)
              (3 1) (2 1) (1 3) (1 2) (1 0) (0 1))))))

(unless (finalize) (error "error in QRY ADVANCED."))
