(in-package #:grph-tests)

(plan 2)

(subtest "grph walk compound 1"

  (let* ((g (grph:make)))

      (grph:path! g '(10 11 12 13 14) <- '(:/g/id/yyy :path-1))
      (grph:path! g '(10 11 12 13 14) -> '(:/g/id/xxx :path-2))
      (grph:path! g '(77 88 99) <- '(:/g/id/xxx))

      (is (grph:walk (g p c sid) ((collect dir compound)) (list sid c p))
          '((:/G/ID/YYY NIL (14 13 12 11 10)) (:/G/ID/XXX NIL (99 88 77))
            (:/G/ID/XXX NIL (10 11 12 13 14))))

      (is (grph:walk (g p c sid) ((collect dir segments compound)) (list sid c p))
          '((:/G/ID/YYY NIL (14 13 12 11 10)) (:/G/ID/XXX NIL (99 88 77))
            (:/G/ID/XXX NIL (10 11 12 13 14))))

      (is (grph:walk (g p c sid) ((collect dir segments)) (list sid c p))
          '((:/G/_DEFAULT/ NIL (99 88 77)) (:/G/_DEFAULT/ NIL (10 11 12 13 14))))))

(subtest "grph walk compound 2"
  (let* ((sid :/g/id/sid-1)
         (sid2 :/g/id/sid-2)
         (sid3 :/g/id/sid-3)
         (g (grph:ingest-edges
             `((0 :/g/id/xxx 1) (1 :/g/id/xxx 4) (4 :/g/id/xxx 7)
               (7 :/g/id/xxx 6) (6 :/g/id/xxx 3) (3 :/g/id/xxx 0)
               (4 ,sid 5) (5 ,sid 2) (2 ,sid 10)
               (9 ,sid 5) (6 ,sid 5)
               (6 _ 7) (3 _ 6) (6 _ 11) (11 _ 12) (11 _ 13)
               (87 ,sid2 88) (88 ,sid2 89) (89 ,sid2 90) (90 ,sid2 87)))))

      (grph:path! g '(0 1 2 3 4 2 10) -> sid3)
      (grph:path! g '(0 1 33 11 99))

      (is (grph:walk (g p c sid) ((collect compound segments)) (list sid c p))
      '((:/G/ID/SID-2 T (89 88 87 90)) (:/G/_DEFAULT/ NIL (11 12))
       (:/G/_DEFAULT/ NIL (11 13)) (:/G/_DEFAULT/ NIL (6 11))
       (:/G/_DEFAULT/ NIL (11 33 1)) (:/G/_DEFAULT/ NIL (99 11))
       (:/G/ID/SID-1 NIL (5 6)) (:/G/ID/SID-1 NIL (4 5)) (:/G/ID/SID-1 NIL (5 9))
       (:/G/ID/SID-1 NIL (10 2 5)) (:/G/ID/SID-3 NIL (2 1 0))
       (:/G/ID/SID-3 T (4 3 2)) (:/G/ID/SID-3 NIL (10 2))
       (:/G/ID/XXX T (6 7 4 1 0 3))))

      (is (grph:walk (g p c sid) ((collect dir compound segments)) (list sid c p))
      '((:/G/ID/SID-2 T (90 87 88 89)) (:/G/_DEFAULT/ NIL (11 12))
       (:/G/_DEFAULT/ NIL (11 13)) (:/G/_DEFAULT/ NIL (6 11))
       (:/G/_DEFAULT/ NIL (1 33 11)) (:/G/_DEFAULT/ NIL (11 99))
       (:/G/ID/SID-1 NIL (6 5)) (:/G/ID/SID-1 NIL (4 5)) (:/G/ID/SID-1 NIL (9 5))
       (:/G/ID/SID-1 NIL (5 2 10)) (:/G/ID/SID-3 NIL (0 1 2))
       (:/G/ID/SID-3 T (2 3 4)) (:/G/ID/SID-3 NIL (2 10))
       (:/G/ID/XXX T (3 0 1 4 7 6))))

      (is (grph:walk (g p c sid) ((collect dir compound)) (list sid c p))
      '((:/G/ID/SID-2 T (90 87 88 89)) (:/G/_DEFAULT/ NIL (11 12))
       (:/G/_DEFAULT/ NIL (6 11 13)) (:/G/_DEFAULT/ NIL (1 33 11 99))
       (:/G/ID/SID-1 NIL (4 5 6)) (:/G/ID/SID-1 NIL (9 5 2 10))
       (:/G/ID/SID-3 NIL (0 1 2 3 4 2 10)) (:/G/ID/XXX T (3 0 1 4 7 6))))

      (is (grph:walk (g p c sid) ((collect compound)) (list sid c p))
      '((:/G/ID/SID-2 T (89 88 87 90)) (:/G/_DEFAULT/ NIL (11 12))
       (:/G/_DEFAULT/ NIL (6 11 13)) (:/G/_DEFAULT/ NIL (99 11 33 1))
       (:/G/ID/SID-1 NIL (4 5 6)) (:/G/ID/SID-1 NIL (10 2 5 9))
       (:/G/ID/SID-3 NIL (10 2 4 3 2 1 0)) (:/G/ID/XXX T (6 7 4 1 0 3))))

      (is (grph:walk (g p c sid) ((collect compound drop)) (list sid c p))
      '((:/G/ID/SID-2 T (89 88 87 90)) (:/G/ID/SID-1 NIL (4 5 6))
        (:/G/ID/SID-1 NIL (10 2 5 9)) (:/G/ID/SID-3 NIL (10 2 4 3 2 1 0))
        (:/G/ID/XXX T (6 7 4 1 0 3))))

      (is (grph:walk (g p c sid) ((collect compound edges drop)) (list sid c p))
      '((:/G/ID/SID-2 NIL (89 90)) (:/G/ID/SID-2 NIL (88 89))
       (:/G/ID/SID-2 NIL (87 88)) (:/G/ID/SID-2 NIL (87 90))
       (:/G/ID/SID-1 NIL (4 5)) (:/G/ID/SID-1 NIL (2 10))
       (:/G/ID/SID-1 NIL (5 9)) (:/G/ID/SID-1 NIL (5 6)) (:/G/ID/SID-1 NIL (2 5))
       (:/G/ID/SID-3 NIL (3 4)) (:/G/ID/SID-3 NIL (2 10))
       (:/G/ID/SID-3 NIL (2 3)) (:/G/ID/SID-3 NIL (1 2)) (:/G/ID/SID-3 NIL (0 1))
       (:/G/ID/SID-3 NIL (2 4)) (:/G/ID/XXX NIL (4 7)) (:/G/ID/XXX NIL (1 4))
       (:/G/ID/XXX NIL (0 1)) (:/G/ID/XXX NIL (6 7)) (:/G/ID/XXX NIL (3 6))
       (:/G/ID/XXX NIL (0 3))))

      (is (grph:walk (g p c sid) ((collect edges)) (list sid c p))
      '((:/G/_DEFAULT/ NIL (89 90)) (:/G/_DEFAULT/ NIL (88 89))
       (:/G/_DEFAULT/ NIL (87 88)) (:/G/_DEFAULT/ NIL (11 99))
       (:/G/_DEFAULT/ NIL (11 13)) (:/G/_DEFAULT/ NIL (11 12))
       (:/G/_DEFAULT/ NIL (6 11)) (:/G/_DEFAULT/ NIL (4 7))
       (:/G/_DEFAULT/ NIL (4 5)) (:/G/_DEFAULT/ NIL (3 4))
       (:/G/_DEFAULT/ NIL (2 10)) (:/G/_DEFAULT/ NIL (2 3))
       (:/G/_DEFAULT/ NIL (1 33)) (:/G/_DEFAULT/ NIL (1 4))
       (:/G/_DEFAULT/ NIL (1 2)) (:/G/_DEFAULT/ NIL (0 1))
       (:/G/_DEFAULT/ NIL (87 90)) (:/G/_DEFAULT/ NIL (11 33))
       (:/G/_DEFAULT/ NIL (5 9)) (:/G/_DEFAULT/ NIL (6 7))
       (:/G/_DEFAULT/ NIL (5 6)) (:/G/_DEFAULT/ NIL (3 6))
       (:/G/_DEFAULT/ NIL (2 5)) (:/G/_DEFAULT/ NIL (2 4))
       (:/G/_DEFAULT/ NIL (0 3))))))

(unless (finalize) (error "error in grph-walk-cpnd"))

