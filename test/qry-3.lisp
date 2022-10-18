(in-package #:grph-tests)

(plan 1)

(subtest "qry rules"

  (let ((*ancestor '(((?x . 0) (?y . 1))
                     ((?x . 1) (?y . 3))
                     ((?x . 0) (?y . 2)))))
        (is (grph::rules/prev-matches *ancestor '(?x . ?z) '(?y . ?y))
            '(((?Z . 0) (?Y . 1)) ((?Z . 1) (?Y . 3)) ((?Z . 0) (?Y . 2)))))

  (let* ((g (make-rules-edge-set-1)))

    (is (ls (grph:rqry g
              :rules ((*ancestor (?x ?y) (?x :a ?y))
                      (*ancestor (?x ?y) (and (?x :a ?z)
                                              (*ancestor ?z ?y))))
              :then *ancestor))
        (ls '((0 0) (0 4) (1 1) (1 2) (3 3)
              (0 3) (1 0) (1 4) (3 1) (3 4)
              (3 2) (3 0) (1 3) (0 2) (0 1)))))

     (let ((g (make-rules-edge-set-2)))

       (is (grph:rqry g
             :rules ((*st-reach (?x ?y) (?x _ ?y))
                     (*st-reach (?x ?y) (and (?x _ ?z) (*st-reach ?z ?y)))
                     (*li-reach (?x ?u) (and (*st-reach ?x _) (?x ?u _)))
                     (*ans-a (?y) (*st-reach 4 ?y))
                     (*ans-b (?u) (*li-reach 1 ?u)))
             :then (mapls *st-reach *li-reach *ans-a *ans-b))
           (mapls `((0 0) (0 1) (0 3) (0 4) (0 6) (1 0)
                    (1 1) (1 3) (1 4) (1 6) (3 0) (3 1)
                    (3 3) (3 4) (3 6) (4 6))
                  `((0 :B) (1 :B) (1 :E) (3 :B) (4 :E))
                  `((6))
                  `((:B) (:E))))))

(unless (finalize) (error "error in QRY RULES."))

