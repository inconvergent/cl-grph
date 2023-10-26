(in-package #:grph)

(defvar *opt* '(optimize (safety 1) (speed 3) debug space))

(defun v? (&optional (silent t)
           &aux (v (slot-value (asdf:find-system 'grph) 'asdf:version)))
  (unless silent (format t "~&GRPH version: ~a~%." v))
  v)
(defun d? (f) (describe f))
(defun i? (f) (inspect f))

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun reread (&rest args) (values (read-from-string (apply #'mkstr args))))

(defun symb (&rest args) (values (intern (apply #'mkstr args))))
(defun kv (s) (declare (symbol s)) (intern (string-upcase (symbol-name s)) :keyword))
(defun last* (l) (declare (list l)) (first (last l)))
(defun close-path (l) (declare (list l)) (cons (last* l) l))

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
  (declare (symbol name) (pn n))
  (loop with name = (string-upcase (string name))
        for x across "XYZWUVPQR" repeat n
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
  "find all instances where fx is t in root."
  (cond ((funcall fx root) (return-from tree-find-all (cons root res)))
        ((atom root) nil)
        (t (let ((l (tree-find-all (car root) fx res))
                 (r (tree-find-all (cdr root) fx res)))
             (when l (setf res `(,@l ,@res)))
             (when r (setf res `(,@r ,@res))))
           res)))

(defun tree-replace (tree from to &optional (comparefx #'equal))
  "compares every tree to from (with comparefx),
and replaces it with to when there is a match"
  (cond ((funcall comparefx tree from) to)
        ((null tree) nil) ((atom tree) tree)
        (t (mapcar (lambda (x) (tree-replace x from to))
                   tree))))

(defun tree-replace-fx (tree fxmatch fxtransform )
  "compares every tree to from (with comparefx),
and replaces it with to when there is a match"
  (cond ((funcall fxmatch tree)
           (tree-replace-fx (funcall fxtransform tree) fxmatch fxtransform))
        ((null tree) nil)
        ((atom tree) tree)
        (t (mapcar (lambda (x) (tree-replace-fx x fxmatch fxtransform))
                   tree))))

(defun replace-pairs (body pairs)
  (declare (list body pairs))
  "replace ((ato afrom) (bto bfrom) ...) in body."
  (loop for (to from) in pairs do (setf body (tree-replace body from to)))
  body)

(defun with-symbs (ss body)
  (declare (list ss body))
  "bind these symbols outside body and replace inside body. eg:
  (with-symbs `(g ,g ...)
    (qry g :select ... )) ; equals:
  (let ((gg ,g))          ; gg is a gensym
    (qry gg :select ...))"
  (let ((s* (loop for (var expr) in (grph::group ss 2) ; gs expr var
                  collect (list (gensym (mkstr var)) expr var))))
    `(let (,@(loop for s in s* collect (subseq s 0 2)))
       (progn ,(replace-pairs body
                 (loop for s in s* collect (list (first s) (third s))))))))


(defun split-string (x s &key prune)
  (declare (character x) (string s) (boolean prune))
  "split s at all instances of character x."
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
  "cartesian product of a list of lists (sets)"
  (if (null l)
      (list nil)
      (loop for x in (car l)
            nconc (loop for y in (n-cartesian-product (cdr l))
                        collect (cons x y)))))

(defun group (l n)
  (declare (list l) (pn n))
  "group l into groups of n. see ungroup."
  (when (< n 1) (warn "GROUP: bad length: ~a," n))
  (labels ((rec (l acc)
             (let ((rest (nthcdr n l)))
               (if (consp rest) (rec rest (cons (subseq l 0 n) acc))
                                (nreverse (cons l acc))))))
    (when l (rec l nil))))

(defun ungroup (l &aux (res (list)))
  (declare (list l res))
  "invorse of group."
  (loop for s in l do (loop for k in s do (push k res)))
  (reverse res))

(defun ensure-list (l)
  (the list (typecase l (null nil) (list l) (t (list l)))))
(defun to-list (a) (declare (sequence a)) (coerce a 'list))
(defun vector-last (a) (declare (vector a)) (aref a (1- (length a))))
(defun vector-first (a) (declare (vector a)) (aref a 0))
(defun to-vector (init)
  (declare (list init))
  (make-array (length init)
    :initial-contents init :adjustable nil :element-type 'list))

(defmacro vector-rearrange (a &rest rest)
  (declare (symbol a))
  `(concatenate 'vector
    ,@(loop for ind in rest
            collect (etypecase ind
                      (number `(list (aref ,a ,ind)))
                      (symbol `(list (aref ,a ,ind)))
                      (cons (ecase (length ind)
                              (1 `(list (aref ,a ,@ind)))
                              (2 `(subseq ,a ,@ind))))))))

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

(defun remove-nil (l)
  "coerce to list, and remove any nils"
  (remove-if-not #'identity (ensure-list l)))

(defmacro fsize (v) `(the pn (fset:size ,v)))
(defmacro logic-set () `(empty-map (empty-set)))

(defun memo (fx &aux (ht (make-hash-table :test #'equal)))
  (declare (function fx) (hash-table ht))
  "return a functiont that memoizes calls to fx."
  (labels ((memo-wrap (&rest rest)
            (let ((v (gethash rest ht)))
              (if v v (let ((res (apply fx rest)))
                        (setf (gethash rest ht) res)
                        res)))))
          #'memo-wrap))

(veq:fvdef relneigh (inds dstfx &aux (res (list)))
  (declare (list inds res) (function dstfx))
  "create list of edges in the relative neigborhood graph of inds according to
(dstfx i j) for indices i,j in inds."
  (labels ((relneigh? (i j)
             (loop for k in inds
                   if (< (max (f@dstfx i k) (f@dstfx j k))
                         (f@dstfx i j))
                   do (return-from relneigh? nil))
             t))
    (loop for i in inds
          do (loop for j in inds if (and (< i j) (relneigh? i j))
                                 do (push (list i j) res)))
    res))

