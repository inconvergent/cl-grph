(in-package #:grph-tests)

(plan 1)

; TODO: rename this file? split /test/grph ?

(subtest "utils"

  (is (grph:sprop :a :b) :/g/ab)

  (is (grph:sprop? :/g/a/) nil)
  (is (grph:sprop? :/gax) nil)
  (is (grph:sprop? :/g/a/b) :/g/a/b)
  (is (grph:sprop? :/g/a/b) :/g/a/b)
  (is-values (grph::unpack-sprop :/g/id/abcx) '(:id :abcx))
  (is (veq:mvb (ty id)
        (grph:unpack-sprop (grph:sprop-id))
        (list ty (keywordp id)))
      '(:id t))
  )

(unless (finalize) (error "error in test utils"))

