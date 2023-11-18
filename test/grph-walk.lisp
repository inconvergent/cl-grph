(in-package #:grph-tests)

(plan 1)

(subtest "grph-walk"
  (let ((g (grph:ingest-edges
             '((0 :path 1) (1 :path 4) (4 :path 7) (7 :path 6)
               (6 :path 3) (3 :path 0) (4 :path 5) (5 :path 2) (2 :path 10)
               (6 :path 7) (3 :path 6) (0 :path 3) (5 :path 4)
               (7 :path 4) (1 :path 0) (4 :path 1))))
        (gg (grph:ingest-edges
              '((0 :path 1) (1 :path 2) (2 :path 0)
                (0 :path 4) (4 :path 5) (5 :path 0)))))

    (is (grph:walk-grph g :path) '(((10 2 5 4 7 6 3 0 1 4) NIL)))
    (is (grph:walk-grph gg :path) '(((1 0 4 5 0 2) T)))))

(unless (finalize) (error "error in grph walk"))
