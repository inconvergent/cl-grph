(in-package :grph)

; RUNTIME QRY FXS AND REDUCERS (AND/NOT/FILTERS/SORT/...) ----------------

(defun distinct (&rest rest &aux (n (length rest)))
  (declare (optimize speed) (pn n))
  "t if values in rest are distinct."
  (= n (length (the list (undup rest nil))))) ; undup use is ok

(defmacro fx-first (f a &rest rest)
  "equvialent to (and (f r1 r2) (f r1 r3) ...). r1 is evaluated only once."
  (awg (a*) `(let ((,a* ,a)) (and ,@(loop for b in rest collect `(,f ,a* ,b))))))
(defmacro first< (&rest rest)
  "equvialent to (and (< r1 r2) (< r1 r3) ...). r1 is evaluated only once."
  `(fx-first < ,@rest))
(defmacro first> (&rest rest)
  "equvialent to (and (> r1 r2) (> r1 r3) ...). r1 is evaluated only once."
  `(fx-first > ,@rest))

(defun lsort (l &optional (fx #'<) &aux (l (copy-list l)))
  (declare (optimize speed) (list l) (function fx))
  "radix sort list of lists."
  (loop for i of-type fixnum from (1- (length (the list (first l)))) downto 0
        do (labels ((p (a b)
                     (declare (list a b))
                     (funcall fx (nth i a) (nth i b))))
                   (setf l (stable-sort (the list l) #'p))))
  l)

(declaim (inline uni psrt pkeys split-by-common))
(defun psrt (l)
  (declare (optimize speed (safety 1)) (list l))
  (sort (copy-list l) #'string< :key #'car))

(defun pkeys (a)
  (declare (optimize speed (safety 1)) (list a))
  (mapcar #'car (first a)))

(defun uni (a b)
  (declare (optimize speed (safety 1)) (list a b))
  (union a b :test #'equal))

(defun split-by-common (common a)
  (declare (optimize speed (safety 0)) (list common a))
  "split l into (values yes no) according to fx"
  (loop for x of-type list in a
        if (find (car x) common :test #'eq) collect x into yes
        else collect x into no
        finally (return (values yes no))))

(defun qry-and (aa bb)
  (declare (optimize speed (safety 1)) (list aa bb))
  "very messy, but faster than the naive approach"
  (when (or (not aa) (not bb)) (return-from qry-and nil))
  (labels ((set-pair (ht k v)
             (declare (hash-table ht) (symbol k))
             (setf (gethash k ht) (uni (gethash k ht (list)) (list v))))
          (ht-merge-common (ht a)
            (declare (hash-table ht) (list a))
            (setf (gethash (psrt a) ht) :nil))
          (ht-add-common (ht a &aux (c (psrt a)))
            (declare (hash-table ht) (list a))
            (when (gethash c ht) (setf (gethash c ht) t)))
           (do-merge (ht common a)
             (declare (hash-table ht) (list common a))
             (mvb (c u) (split-by-common common a)
               (let ((c (psrt c)))
                 (setf (gethash c ht) (uni (gethash c ht) u)))))
           (do-add (ht ht-isect common a)
             (declare (hash-table ht ht-isect) (list common a))
             (mvb (c u) (split-by-common common a)
               (let* ((c (psrt c))
                      (h (gethash c ht)))
                 (when h (setf (gethash c ht) (uni h u)
                               (gethash c ht-isect) t)))))
           (cart (ht)
             (declare (hash-table ht))
             (n-cartesian-product
               (loop for k being the hash-keys of ht using (hash-value v)
                     collect (mapcar (lambda (x) (cons k x)) v)))))
    (let* ((ka (pkeys aa)) (kb (pkeys bb))
           (common (intersection ka kb :test #'eq))
           (uncommon (set-difference (uni ka kb) common :test #'eq))
           (n (length uncommon))
           (ht (make-hash-table :test #'equal))
           (ht-isect (make-hash-table :test #'equal))
           (ht-pairs (make-hash-table :test #'eq)))
      (case n
        (0 (loop for a in aa do (ht-merge-common ht a))
           (loop for b in bb do (ht-add-common ht b))
           (loop for common-pairs being the hash-keys of ht using (hash-value v)
                 if (not (eq v :nil)) collect common-pairs))
        (otherwise
          (when (= (length ka) (length common)) (rotatef aa bb))
          (loop for a in aa do (do-merge ht common a))
          (loop for b in bb do (do-add ht ht-isect common b))
          (loop with res = (list)
                for common-pairs being the hash-keys of ht-isect
                for v = (gethash common-pairs ht)
                do (clrhash ht-pairs)
                   (loop for (pk . pv) in v do (set-pair ht-pairs pk pv))
                   (when (= (hash-table-count ht-pairs) n)
                     (loop for prod in (cart ht-pairs)
                           do (push (concatenate 'list prod common-pairs) res)))
                   finally (return res)))))))

(defun qry-or (aa bb &optional select)
  (declare (optimize speed (safety 1)) (list aa bb))
  (when (not aa) (return-from qry-or bb))
  (when (not bb) (return-from qry-or aa))
  (labels ((do-merge (ht common a)
             (setf (gethash (psrt (split-by-common common a)) ht) t)))
    (let ((common (or select (intersection (pkeys aa) (pkeys bb) :test #'eq)))
          (ht (make-hash-table :test #'equal))
          (res (list)))
      (loop for a in aa do (do-merge ht common a))
      (loop for b in bb do (do-merge ht common b))
      (loop for k being the hash-keys of ht do (push k res))
      res)))

(defun select-vars (aa common)
  (declare (optimize speed (safety 0)) (list aa common))
  (when (not aa) (return-from select-vars nil))
  (labels ((do-merge (ht common a)
             (declare (hash-table ht) (list common a))
             (setf (gethash (psrt (split-by-common common a)) ht) t)))
    (let ((ht (make-hash-table :test #'equal))
          (res (list)))
      (loop for a in aa do (do-merge ht common a))
      (loop for k being the hash-keys of ht do (push k res))
      res)))


(defun qry-not (orig not* &optional select)
  (declare (optimize speed (safety 1)) (list orig not*))
  ; early exit when nothing to subtract or nothing to return
  ; orig will either be nil, or whatever was pass in (when not is nil)
  (when (or (not orig) (not not*)) (return-from qry-not orig))
  (labels ((do-merge (ht common a)
             (push a (gethash (psrt (split-by-common common a)) ht)))
           (do-rem (ht common a) (remhash (psrt (split-by-common common a)) ht)))
    (let ((common (or select (intersection (pkeys not*) (pkeys orig) :test #'eq)))
          (ht (make-hash-table :test #'equal))
          (res (list)))
      (loop for a in orig do (do-merge ht common a))
      (loop for n in not* do (do-rem ht common n))
      (loop for v being the hash-values of ht
            do (loop for p in v do (push p res)))
      res)))

(defun qry-filter (a b fx)
  (declare (optimize speed (safety 0)) (list a) (ignore b) (function fx))
  "used for % filter clauses."
  (remove-if-not fx a))

(defun rules/prev-matches (var &rest pairs)
  (declare (optimize speed (safety 1)) (list var))
  "get the hits from the last iteration for linear rules."
  ; TODO: drop _ pairs!!!
  (mvb (pairs filters)
       (filter-by-predicate pairs (lambda (p) (or (var? p) (any? p))) :key #'cdr)
    (labels ((filter-match-all (v)
              (declare (list v))
              (every (lambda (f) (find f v :test #'equal)) filters))
             (repl (f) (mapcar (lambda (p &aux (lft (car p)) (rht (cdr p)))
                                 (declare (symbol lft rht))
                                 `(,rht . ,(get-var lft f)))
                               pairs)))
      (when filters (setf var (remove-if-not #'filter-match-all var)))
      (mapcar #'repl var))))

(defun rules/post-proc (l &rest args)
  (declare (optimize speed (safety 1)) (list l args))
  "strip first nil if present, select args from every row."
  (mapcar (lambda (f)
            (declare (list f))
            (mapcar (lambda (a) (declare (symbol a)) (get-var a f)) args))
          l))

