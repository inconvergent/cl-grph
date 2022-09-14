
(defpackage #:grph-tests (:use #:cl #:prove) (:export #:run-tests))

(setf prove:*enable-colors* nil)

(in-package #:grph-tests)

(defparameter files `(#P"test/grph.lisp" #P"test/qry.lisp"
                       #P"test/xgrph.lisp"
                       ))

(defun compile-or-fail (f)
  (format t "~%compiling: ~a~%" (grph::mkstr f))
  (with-open-stream (*standard-output* (make-broadcast-stream))
    (compile-file f)))

(defun run-tests ()
  (loop with fails = 0
        for f in files
        do ; (compile-or-fail f)
           (format t "~&~%starting tests in: ~a~%" (grph::mkstr f))
           (unless (prove:run f :reporter :fiveam)
                   (incf fails))
           (format t "~&done: ~a~%" (grph::mkstr f))
        finally (return (unless (< fails 1)
                          (sb-ext:quit :unix-status 7)))))

