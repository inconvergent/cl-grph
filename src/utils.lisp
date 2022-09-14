
(in-package #:grph)

(deftype pn (&optional (bits 31)) `(unsigned-byte ,bits))
(defvar *opt* '(optimize (safety 3) (speed 3) debug space))

(defun v? (&optional (silent t))
  (let ((v (slot-value (asdf:find-system 'grph) 'asdf:version)))
    (unless silent (format t "~%veq version: ~a~%" v))
    v))
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

;https://gist.github.com/lispm/6ed292af4118077b140df5d1012ca646
(defun psymb (package &rest args) (values (intern (apply #'mkstr args) package)))

(defun undup (e)
  (declare (optimize speed))
  (delete-duplicates (awf e)))

; (defun internal-path-string (path)
;   (declare (string path))
;   (namestring (asdf:system-relative-pathname :weird path)))

(defun -gensyms (name n)
  (declare (symbol name) (fixnum n))
  (loop with name = (string-upcase (string name))
        repeat n
        for x across "XYZWUVPQR"
        collect (gensym (format nil "~a-~a-" name x))))

(defun filter-by-predicate (l fx)
  (declare (list l) (function fx))
  "split l into (values yes no) according to fx"
  (loop for x in l
        if (funcall fx x) collect x into yes
        else collect x into no
        finally (return (values yes no))))

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

(defun interject (ll &optional (s :-) &aux (res (list)))
  (loop for l in ll do (push l res) (push s res))
  (reverse (cdr res)))

; modified from on lisp by pg
(defun group (source n)
  (if (zerop n) (warn "group: zero length"))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest (cons (subseq source 0 n) acc))
                   (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))

(defun ungroup (source &aux (res (list)))
  (loop for s in source do (loop for k in s do (push k res)))
  (reverse res))

