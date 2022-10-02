(in-package :grph)

(declaim (inline
           conflict distinct
           some-subsets p/some-subsets))

; REDUCERS ----------------

(defun conflict (aa bb)
  (declare (optimize speed (safety 0)) (list aa bb))
  (loop for a in aa
        while (not c)
        for c = (let ((f (find (car a) bb :key #'car :test #'eq)))
                  (when (and f (not (eq (cdr a) (cdr f)))) t))
        finally (return c)))

(defun qry-and (aa bb)
  (declare (optimize speed (safety 0)) (list aa bb))
  (when (< (length aa) (length bb)) (rotatef aa bb))
  (remove-duplicates
    (mapcan (lambda (a)
              (loop for b in bb
                    unless (conflict a b)
                    collect (varset-sort (union a b :test #'equal) #'car)))
            aa)
    :test #'equal))

; TODO: what about sort order?
(defun qry-or (aa bb)
  (declare (optimize speed (safety 0)) (list aa bb))
  (union aa bb :test #'equal))

(defun some-subsets (a b)
  (declare (optimize speed (safety 0)) (list a b))
  (some (lambda (o)
          (declare (list o))
          (subsetp o a :test #'equal)) b))

(defun qry-not (orig not*)
  (declare (optimize speed (safety 0)) (list orig not*))
  (remove-if (lambda (a)
               (declare (list a))
               (some-subsets a not*)) orig))

; PARALLEL REDUCERS ----------------

(defun p/some-subsets (a b)
  (declare (optimize speed (safety 0)) (list a b))
  (lparallel:psome (lambda (o)
                     (declare (list o))
                     (subsetp o a :test #'equal)) b))

(defun p/qry-not (orig not*)
  (declare (optimize speed (safety 0)) (list orig not*))
  (lparallel:premove-if (lambda (a)
                          (declare (list a))
                          (p/some-subsets a not*)) orig))

(defun p/qry-and (aa bb)
  (declare (optimize speed (safety 0)) (list aa bb))
  (when (< (length aa) (length bb)) (rotatef aa bb))
  (remove-duplicates
    (lparallel:pmapcan
      (lambda (a)
        (loop for b in bb
              unless (conflict a b)
              collect (varset-sort (union a b :test #'equal) #'car)))
      aa)
    :test #'equal))

; RUNTIME QRY FXS (FILTERS) ----------------

(defun distinct (&rest rest &aux (n (length rest)))
  (declare (optimize speed) (fixnum n))
  (= n (length (the list (undup rest nil)))))

(defmacro fx-first (fx &rest rest &aux (a (car rest)))
  (declare (symbol a))
  `(and ,@(loop for b of-type symbol in (cdr rest) collect `(,fx ,a ,b))))
(defmacro first< (&rest rest) `(fx-first < ,@rest))
(defmacro first> (&rest rest) `(fx-first > ,@rest))

(defun lsort (l &optional (fx #'<) &aux (l (copy-list l)))
  (declare (optimize speed) (list l) (function fx))
  "radix sort list of lists."
  (loop for i of-type fixnum from (1- (length (the list (first l)))) downto 0
        do (labels ((p (a b)
                     (declare (list a b))
                     (funcall fx (nth i a) (nth i b))))
                   (setf l (stable-sort (the list l) #'p))))
  l)

