(in-package #:grph-tests)

; (declaim (optimize speed (safety 2)))

(plan 2)

(subtest "normalize edges"
  (let* ((g (grph:ingest-edges '((0 :a 1) (1 :a 2) (3 :b 4) (4 :c 0)
                                 (4 :_ 5) (5 :a 4) (5 :_ 6) (6 :d 5)
                                 (6 :d 7) (7 :_ 6) (7 :a 4) (4 :_ 7)
                                 (9 :c 8) (8 :_ 5))))
         (g* g))

    (is (grph:@edges g) '((9 8) (8 5) (7 6) (7 4) (6 7) (6 5) (5 6) (5 4)
                          (4 7) (4 5) (4 0) (3 4) (1 2) (0 1)))
    (grph:normalize-edges! g)
    (grph:normalize-edges! g* :->)


    (is (grph:@edges g)  '((9 8) (8 5) (6 7) (5 6) (4 7) (4 5) (4 0) (3 4) (1 2) (0 1)))
    (is (grph:@edges g*) '((8 9) (6 7) (5 8) (5 6) (4 7) (4 5) (3 4) (1 2) (0 4) (0 1)))

    (is (grph:qry g  :select (?x ?p ?y) :where (?x ?p ?y))
        '((9 :C 8) (8 :_ 5) (6 :D 7) (5 :D 6) (4 :A 7)
          (4 :A 5) (4 :C 0) (3 :B 4) (1 :A 2) (0 :A 1)))
    (is (grph:qry g* :select (?x ?p ?y) :where (?x ?p ?y))
        '((8 :C 9) (6 :D 7) (5 :_ 8) (5 :D 6) (4 :A 7)
          (4 :A 5) (3 :B 4) (1 :A 2) (0 :C 4) (0 :A 1)))))

(subtest "grph walk"
  (let ((g (grph:ingest-edges '((0 :path 1) (1 :path 4) (4 :path 7)  ; --
                                (7 :path 6) (6 :path 3) (3 :path 0)
                                (4 :path 5) (5 :path 2) (2 :path 10) ; --
                                (4 :xxx 5) (5 :xxx 3)
                                (6 :path 7) (3 :path 6) (0 :path 3)  ; --
                                (5 :path 4) (7 :path 4) (1 :path 0)
                                (4 :path 1))))
        (gg (grph:ingest-edges '((0 :path 1) (1 :path 2) (2 :path 0)
                                 (0 :path 4) (4 :path 5) (5 :path 0)))))

    (is (grph:walk (g) (collect :prop :path))
        '(((10 2 5 4 7 6 3 0 1 4) NIL)))
    ; this is not the same as above, but still valid
    (is (grph:walk (g) (collect
                      :es (grph:qry g :select (?x ?y)
                                      :where (?x :path ?y))))
     '(((10 2 5 4) NIL) ((4 1 0 3 6 7) T)))

    (is (grph:walk (gg) (collect :prop :path))
        '(((1 0 4 5 0 2) T)))

    (is (grph:walk (g) ((collect segments) :prop :path))
        '(((7 6 3 0 1 4) T) ((10 2 5 4) NIL)))

    (is (grph:walk (gg) ((collect segments) :prop :path))
        '(((0 2) NIL) ((4 5 0) T) ((2 1 0) NIL)))

    (is (grph:walk (g) ((collect dir segments) :prop :path))
        '(((7 6 3 0 1 4) T) ((4 5 2 10) NIL)))

    (is (grph:walk (g) ((collect any segments) :prop :path))
        '(((7 6 3 0 1 4) T) ((10 2 5 4) NIL)))

    (is (grph:walk (g p c) ((collect dir segments) :prop :path)
          (list (reverse p) c))
        '(((4 1 0 3 6 7) T) ((10 2 5 4) NIL)))

    (is (grph:walk (g) ((collect segments)))
        '(((10 2 5) NIL) ((3 0 1 4) NIL)
          ((4 7 6 3) NIL) ((5 4) NIL) ((3 5) NIL)))

    (is (grph:walk (g p c) ((collect dir paths)))
        '(((5 2 10) NIL) ((4 1 0 3 6 7 4 5 3) NIL)))

    (is (grph:walk (g p c) ((collect dir edges) :prop :path))
        '(((2 10) NIL) ((6 7) NIL) ((4 7) NIL) ((3 6) NIL) ((4 5) NIL) ((5 2) NIL)
          ((1 4) NIL) ((0 3) NIL) ((0 1) NIL)))

    (is (grph:walk (g p c) ((collect edges) :prop :path))
        '(((2 10) NIL) ((6 7) NIL) ((4 7) NIL) ((3 6) NIL) ((4 5) NIL) ((2 5) NIL)
         ((1 4) NIL) ((0 3) NIL) ((0 1) NIL)))

    (is (grph:walk (g p c) ((collect edges)))
        '(((2 10) NIL) ((6 7) NIL) ((4 7) NIL) ((3 6) NIL) ((4 5) NIL) ((3 5) NIL)
          ((2 5) NIL) ((1 4) NIL) ((0 3) NIL) ((0 1) NIL)))))

(unless (finalize) (error "error in grph-walk"))

