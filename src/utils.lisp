(in-package #:grph)

; (declaim (inline mkstr reread kv close-path close-path* symb psymb
;                  select-mode valid-modes copy-hash-table ensure-list))

(defvar *opt* '(optimize (safety 1) (speed 3) debug space))

(defun d? (s) "describe symbol." (describe s)) (defun i? (s) "inspect s" (inspect s))
(defun v? (&optional (silent t) &aux (v (slot-value (asdf:find-system 'grph) 'asdf:version)))
  "return/print grph version."
  (unless silent (format t "~&GRPH version: ~a~%." v))
  v)
;https://github.com/inconvergent/weir/pull/1/commits/4a1df51914800c78cb34e8194222185ebde12388
(defmacro define-struct-load-form (name) "allow struct to be dumped to FASL files."
  `(defmethod make-load-form ((o ,name) &optional env)
     (make-load-form-saving-slots o :environment env)))

(defmacro lst->set (f) "convert list to fset:set." `(fset:convert 'fset:set (the list ,f)))
(defmacro lst->map (f) "convert fset:map to list." `(fset:convert 'fset:map (the list ,f)))
(defmacro set->lst (f) "convert fset:set to list." `(fset:convert 'list (the fset:set ,f)))
(defmacro map->lst (f) "convert fset:map to list." `(fset:convert 'list (the fset:map ,f)))
(defun lst->set-fx (ll &optional (fx #'identity))
  (declare (optimize speed (safety 1)) (list ll) (function fx))
  "make an fset:set with (fx o) for every o in ll. see set->lst-fx."
  (loop with res = (fset:empty-set)
        for l in ll do (setf res (fset:with res (funcall fx l)))
        finally (return res)))
(defun set->lst-fx (ss &optional (fx #'identity) &aux (res (list)))
  (declare (optimize speed (safety 1)) (fset:set ss) (function fx)) "inverse of lst->set-fx."
  (do-set (o ss) (push (funcall fx o) res)) res)

(defun mapqt (l)
  (declare (optimize speed (safety 1)) (list l)) "new list with quoted items."
  (mapcar (lambda (s) `(quote ,s)) l))

(defun mkstr (&rest args)
  (declare (optimize speed (safety 1))) "coerce this to string."
  (the string (with-output-to-string (s) (dolist (a args) (princ a s)))))
(defun reread (&rest args)
  (declare (optimize speed (safety 1))) "mkstr then read from string."
  (values (read-from-string (apply #'mkstr args))))
(defun kv (s); TODO: rename kw, accept str
  (declare (optimize speed (safety 1)) (symbol s)) "mkstr, upcase, keyword."
  (intern (string-upcase (symbol-name s)) :keyword))
(defun last* (l)
  (declare (optimize speed (safety 1)) (list l)) "last item in list."
  (first (last l)))
(defun close-path (l)
  (declare (optimize speed (safety 1)) (list l)) "cons last of to l."
  (cons (last* l) l))
(defun symb (&rest args)
  (declare (optimize speed (safety 1))) "mkstr, make symbol."
  (values (intern (apply #'mkstr args))))
(defun psymb (pkg &rest args)
  (declare (optimize speed (safety 1)) (keyword pkg)) "mkstr, make symbol in pkg."
  (values (intern (apply #'mkstr args) pkg)))

(defun undup (e &optional (flatten t)) ; TODO: avoid or rewrite awf?
  (declare (optimize speed (safety 1)))
  "remove duplicate entries."
  (remove-duplicates (if flatten (awf e) e)))

(defun at-most (n &rest rest) (declare (pn n))
  (declare (optimize speed (safety 1)))
  (<= (count-if #'identity rest) n))

(defun -gensyms (name n) (declare (symbol name) (pn n))
  (loop with name = (string-upcase (string name))
        for x across "XYZWUVPQRIJKABC" repeat n
        collect (gensym (format nil "~a-~a-" name x))))

(defun split-string (x s &key prune)
  (declare (character x) (string s) (boolean prune))
  "split s at all instances of character x."
  (labels ((splt (s) (declare (string s))
             (loop for c of-type character across s
                   for i of-type fixnum from 0
                   if (eql c x)
                   do (return-from splt
                        (cons (subseq s 0 i)
                              (splt (subseq s (the fixnum (1+ i)))))))))
    (let ((res (splt (concatenate 'string s (string x)))))
      (if prune (remove-if (lambda (s) (declare (string s)) (= 0 (length s))) res)
                res))))

(defun n-cartesian-product (l) ; https://rosettacode.org/wiki/Cartesian_product_of_two_or_more_lists#Common_Lisp
  (declare (optimize speed (safety 1)) (list l))
  "cartesian product of a list of lists (sets)."
  (if (null l) (list nil)
               (loop for x in (car l) nconc
                 (loop for y in (n-cartesian-product (cdr l)) collect (cons x y)))))

(defun ensure-list (l)
  (declare (optimize speed))
  "return l if l is a nil/list. otherwise return (list l)."
  (the list (typecase l (null nil) (list l) (t (list l)))))
(defun to-list (a) (declare (optimize speed) (sequence a)) "coerce sequence to list." (coerce a 'list))
(defun vector-last (a) (declare (optimize speed) (vector a)) "last element of vector." (aref a (1- (length a))))
(defun vector-first (a) (declare (optimize speed) (vector a)) "first element of vector." (aref a 0))
(defun to-vector (init &optional (type 'list))
  (declare (optimize speed) (list init)) "make non-adjustable array with init contents."
  (make-array (length init) :initial-contents init :adjustable nil :element-type type))

(defun select-mode (c valid &aux (c* (ensure-list c)))
  (declare (optimize speed (safety 2)) (list valid c*))
  "select a valid mode, defaults to first valid mode if there are no matches"
  (let ((res (intersection c* valid)))
    (the symbol (cond ((= 1 (length res)) (car res))
                      (t (car valid))))))
(defun valid-modes (name l valid &aux (l* (ensure-list l)))
  (declare (optimize speed (safety 2)) (keyword name) (list valid l*))
  (let ((res (mapcar (lambda (s) (psymb :keyword (mkstr s))) l*)))
    (unless (subsetp res valid)
            (error "MODE: invalid mode for ~a in ~a. use: ~a." name l* valid))
    res))

(defun remove-nil (l) "coerce to list, and remove any nils"
  (remove-if-not #'identity (ensure-list l)))

(defmacro fsize (v) `(the pn (fset:size ,v)))
(defmacro logic-set () `(empty-map (empty-set)))

(defun copy-hash-table (ht
         &aux (res (make-hash-table
                     :test (hash-table-test ht)
                     :rehash-size (hash-table-rehash-size ht)
                     :rehash-threshold (hash-table-rehash-threshold ht)
                     :size (hash-table-size ht))))
  (declare (optimize speed (safety 1)))
  "copy hash table with all properties."
  (loop for key being each hash-key of ht using (hash-value value)
        do (setf (gethash key res) value))
  res)

(defun memo (fx &aux (ht (make-hash-table :test #'equal)))
  (declare (function fx) (hash-table ht))
  "return function that memoizes calls to fx."
  (labels ((memo-wrap (&rest rest &aux (v (gethash rest ht)))
             (if v v (let ((res (apply fx rest)))
                       (setf (gethash rest ht) res)
                       res))))
    #'memo-wrap))

(veq:fvdef relneigh (inds dstfx &aux (res (list)))
  (declare (list inds res) (function dstfx))
  "create list of edges in the relative neigborhood graph of inds according to
(dstfx i j) for indices i,j in inds."
  (labels ((relneigh? (i j) (loop for k in inds
                                  if (< (max (f@dstfx i k) (f@dstfx j k))
                                        (f@dstfx i j))
                                  do (return-from relneigh? nil))
                            t))
    (loop for i in inds do
      (loop for j in inds
        if (and (< i j) (relneigh? i j)) do (push (list i j) res)))
    res))

