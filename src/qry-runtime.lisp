(in-package :grph)

(declaim (inline
           conflict distinct
           some-subsets p/some-subsets))

; RUNTIME QRY FXS AND REDUCERS (AND/NOT/FILTERS/SORT/...) ----------------

(defun distinct (&rest rest &aux (n (length rest)))
  (declare (optimize speed) (pn n))
  (= n (length (the list (undup rest nil))))) ; undup use is ok

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

(defun dedupe-matches (l)
  (declare (optimize speed (safety 1)) (list l))
  (labels ((srt-varset (a)
             (declare (list a))
             (sort (copy-list a) #'string< :key #'car)))
    (remove-duplicates (mapcar #'srt-varset l) :test #'equal)))

; NOIE: optional dedupe?
; NOTE: it is possible to do nothing if select is nil (if we know we dont need)
(defun select-vars (ll select)
  (declare (optimize speed (safety 1)) (list ll))
  ; (unless select (return-from select-vars ll))
  (labels ((row-select (l) (remove-if-not
                             (lambda (s) (member (the symbol (car s)) select :test #'eq))
                             l))
           (select (ll) (loop for l in ll
                              for s = (row-select l)
                              if s collect s)))
    (dedupe-matches (select ll))))

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
  (dedupe-matches
    (mapcan (lambda (a)
              (loop for b in bb
                    unless (conflict a b)
                    collect (union a b :test #'equal)))
            aa)))

; NOTE: select is passed to reducers to allow future specialised algorithms
; where applicable

(defun qry-or (aa bb &optional select)
  (declare (optimize speed (safety 0)) (list aa bb))
  (labels ((row-select (l) (remove-if-not (lambda (s) (member s select)) l))
           (select (ll) (loop for l in ll
                              for s = (row-select (car s))
                              if s collect s)))
    ; this creates duplicates in (select x), but we need dedupe eithe way
    (dedupe-matches
      (if select
          (union (select-vars aa select) (select-vars bb select) :test #'equal)
          (union aa bb :test #'equal)))))

(defun some-subsets (a b)
  (declare (optimize speed (safety 0)) (list a b))
  (some (lambda (o)
          (declare (list o))
          (subsetp o a :test #'equal)) b))

(defun qry-not (orig not* &optional select)
  (declare (optimize speed (safety 0)) (list orig not*))
  (remove-if (lambda (a)
               (declare (list a))
               (some-subsets a (if select (select-vars not* select) not*))) orig))

