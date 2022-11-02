(in-package #:grph)

(deftype pn (&optional (bits 31)) `(unsigned-byte ,bits))
(defvar *opt* '(optimize (safety 3) (speed 3) debug space))

(defun v? (&optional (silent t)
           &aux (v (slot-value (asdf:find-system 'grph) 'asdf:version)))
  (unless silent (format t "~&GRPH version: ~a~%." v))
  v)
(defun d? (f) (describe f))
(defun i? (f) (inspect f))

; from on lisp by pg
(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

; from on lisp by pg
(defun reread (&rest args) (values (read-from-string (apply #'mkstr args))))


;from on lisp by pg
(defmacro abbrev (short long)
  `(defmacro ,short (&rest args)
     `(,',long ,@args)))

(abbrev mvc multiple-value-call)
(abbrev mvb multiple-value-bind)
(abbrev dsb destructuring-bind)
(abbrev awg alexandria:with-gensyms)
(abbrev awf alexandria:flatten)


; from on lisp by pg
(defun symb (&rest args) (values (intern (apply #'mkstr args))))
(defun kv (s) (declare (symbol s)) (intern (string-upcase (symbol-name s)) :keyword))
(defun last* (l) (declare (list l)) (first (last l)))

;https://gist.github.com/lispm/6ed292af4118077b140df5d1012ca646
(defun psymb (package &rest args) (values (intern (apply #'mkstr args) package)))

; TODO: rewrite as macro
(defun lpos (l &optional (i 0) j)
  (if j (mapcar (lambda (a) (subseq a i j)) l)
        (mapcar (lambda (a) (nth i a)) l)))

(defun mapqt (l) (mapcar (lambda (s) `(quote ,s)) l))

(defun undup (e &optional (flatten t))
  (declare (optimize speed))
  (remove-duplicates (if flatten (awf e) e)))

(defun at-most (n &rest rest)
  (declare (pn n))
  (<= (length (remove-if-not #'identity rest)) n))

(defun -gensyms (name n)
  (declare (symbol name) (fixnum n))
  (loop with name = (string-upcase (string name))
        repeat n
        for x across "XYZWUVPQR"
        collect (gensym (format nil "~a-~a-" name x))))

(defun filter-by-predicate (l fx &key (key #'identity))
  (declare (optimize speed (safety 2)) (list l) (function fx key))
  "split l into (values yes no) according to fx"
  (loop for x in l
        if (funcall fx (funcall key x)) collect x into yes
        else collect x into no
        finally (return (values yes no))))

(defun tree-find-all (root fx &optional (res (list)))
  (declare (optimize speed) (function fx) (list res))
  (cond ((funcall fx root) (return-from tree-find-all (cons root res)))
        ((atom root) nil)
        (t (let ((l (tree-find-all (car root) fx res))
                 (r (tree-find-all (cdr root) fx res)))
             (when l (setf res `(,@l ,@res)))
             (when r (setf res `(,@r ,@res))))
           res)))

(defun split-string (x s &key prune)
  (declare (character x) (string s) (boolean prune))
  (labels
    ((splt (s)
       (loop for c across s for i from 0
             if (equal c x)
             do (return-from splt
                  (cons (subseq s 0 i) (splt (subseq s (1+ i))))))))
    (let ((res (splt (concatenate 'string s (string x)))))
      (if prune (remove-if (lambda (s) (= 0 (length s))) res)
                res))))

; https://rosettacode.org/wiki/Cartesian_product_of_two_or_more_lists#Common_Lisp
(defun n-cartesian-product (l)
  (declare (optimize speed (safety 1)) (list l))
  "recursively calculate the n-cartesian product of a list of lists (sets)"
  (if (null l)
      (list nil)
      (loop for x in (car l)
            nconc (loop for y in (n-cartesian-product (cdr l))
                        collect (cons x y)))))

; modified from on lisp by pg
(defun group (source n)
  (when (< n 1) (warn "GROUP: bad length: ~a," n))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest) (rec rest (cons (subseq source 0 n) acc))
                                (nreverse (cons source acc))))))
    (when source (rec source nil))))

(defun ungroup (source &aux (res (list)))
  (loop for s in source do (loop for k in s do (push k res)))
  (reverse res))

(defun ensure-list (l)
  (the list (typecase l (null nil) (list l) (t (list l)))))

(defun select-mode (c valid &aux (c* (ensure-list c)))
  (declare (list valid c*))
  (let ((res (intersection c* valid)))
    (the symbol (cond ((= 1 (length res)) (car res))
                      (t (car valid))))))
(defun valid-modes (name l valid &aux (l* (ensure-list l)))
  (declare (keyword name) (list valid l*))
  (let ((res (mapcar (lambda (s) (psymb :keyword (mkstr s))) l*)))
    (unless (subsetp res valid)
            (error "MODE: invalid mode for ~a in ~a. use: ~a." name l* valid))
    res))

(defun get-kv (l k &optional d &aux (v (member k (group l 2) :key #'car)))
  (declare (list l) (keyword k))
  (if v (cadar v) d))
(defun strip-kvs (l strip &aux (res (list)))
  (declare (list l strip))
  (loop for (k v) in (remove-if (lambda (k) (member k strip))
                                (group l 2) :key #'car)
        do (push (the keyword k) res) (push v res))
  (reverse res))

