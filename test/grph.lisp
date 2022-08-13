(in-package #:grph-tests)

(plan 1)

(subtest "grph" (is t nil))

(unless (finalize) (error "error in grph"))
