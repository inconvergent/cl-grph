
(defpackage #:grph-tests (:use #:cl #:prove) (:export #:run-tests))

(setf prove:*enable-colors* nil)

(in-package #:grph-tests)

#+:grph-parallel (setf lparallel:*kernel* (lparallel:make-kernel 4))

(defparameter files `(#P"test/grph.lisp" #P"test/qry.lisp"
                      #P"test/qry-2.lisp" #P"test/qry-3.lisp"
                      #P"test/xgrph.lisp"))

; (defun compile-or-fail (f)
;   (format t "~%compiling: ~a~%" (grph::mkstr f))
;   (with-open-stream (*standard-output* (make-broadcast-stream))
;     (compile-file f)))

(defun run-tests ()
  (loop with fails = 0
        for f in files
        do ;(compile-or-fail f)
           (format t "~&~%starting tests in: ~a~%" (grph::mkstr f))
           (unless (prove:run f :reporter :fiveam)
                   (incf fails))
           (format t "~&done: ~a~%" (grph::mkstr f))
        finally (return (unless (< fails 1)
                          (sb-ext:quit :unix-status 7)))))

(defun lsort* (l &aux (l (copy-list l)))
  (declare (optimize speed) (list l))
  "radix sort list of lists (of numbers or symbols).
inefficient. use for tests only."
  (loop for i of-type fixnum from (1- (length (the list (first l)))) downto 0
        do (labels ((srt (a b)
                      (funcall (the function (etypecase a (symbol #'string<)
                                                          (number #'<)))
                               a b))
                    (p (a b) (srt (nth i a) (nth i b))))
                   (setf l (stable-sort (the list l) #'p))))
  l)
(defun ls (l) (lsort* l))
(defun mapls (&rest rest) (mapcar #'lsort* rest))

(defun make-edge-set
  (&aux (g (grph:grph))
        (f `((0 :A 1) (0 :C 1) (1 :A 3) (1 :A 2) (1 :A 0) (1 :C 0)
             (2 :A 1) (3 :C 7) (3 :B 5) (3 :C 5) (3 :B 4) (3 :A 1)
             (4 :B 3) (4 :B 5) (4 :E 5) (5 :B 3) (5 :C 3) (5 :B 4)
             (5 :E 4) (7 :C 3) (99 :X 77))))
  (grph:ingest-edges g f))

(defun mk-grph-main ()
  (let ((g (grph:grph))
        (bprop  '((:b "90"))))
    (grph:add! g 0 1)
    (grph:add! g 2 3)
    (grph:add! g 3 4 '(:a))
    (grph:add! g 4 3 '((:a "43")))
    (grph:add! g 5 6)
    (grph:add! g 6 0 '((:a "60")))
    (grph:add! g 7 8 '(:c))
    (grph:add! g 8 9 '(:b))
    (grph:add! g 9 0 bprop)
    (grph:add! g 7 8 '(:b))
    (grph:add! g 0 3)
    g))

(defun mk-grph-match ()
  (let ((g (grph:grph)))
    (grph:add! g 0 1 '(:a))
    (grph:add! g 0 3 '(:a))
    (grph:add! g 2 3 '((:a "bbbbb")))
    (grph:prop! g '(2 3) '((:b "ccccc")))
    (grph:add! g 3 4 '(:a))
    (grph:add! g 4 3 '(:a))
    (grph:add! g 7 8 '((:a "7778888")))
    (grph:add! g 5 6)
    (grph:add! g 6 0 '(:b))
    (grph:add! g 33 0 '(:b))
    (grph:add! g 8 9 '(:b))
    (grph:add! g 9 0 '(:a))
    (grph:prop! g '(0 1) '((:a "aaa")))
    g))

(defun make-rules-edge-set-1 ()
  (let ((g (grph:grph))
        (f `((0 :a 1) (0 :a 2) (1 :a 3)
             (3 :a 2) (3 :a 4) (3 :a 0))))
  (grph:ingest-edges g f)))

(defun make-rules-edge-set-2 ()
  (let ((g (grph:grph))
        (f `((0 :b 1) (1 :b 3) (3 :b 0) (1 :e 4) (4 :e 6))))
  (grph:ingest-edges g f)))

(defun make-rules-edge-set-3 ()
  (let ((g (grph:grph))
        (f `((0 :a 3) (3 :a 2) (2 :a 0)
             (0 :b 1) (1 :b 3) (3 :b 0)
             (3 :c 5) (5 :c 2)
             (1 :e 4) (4 :e 6))))
  (grph:ingest-edges g f)))

