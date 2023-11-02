(in-package :grph)

; RUNTIME QRY FXS AND REDUCERS (AND/NOT/FILTERS/SORT/...) ----------------

(defun distinct (&rest rest &aux (n (length rest)))
  (declare (optimize speed) (pn n)) "t if values in rest are distinct."
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
  (declare (optimize speed) (list l) (function fx)) "radix sort list of lists."
  (loop for i of-type fixnum from (1- (length (the list (first l)))) downto 0
        do (labels ((p (a b)
                     (declare (list a b))
                     (funcall fx (nth i a) (nth i b))))
                   (setf l (stable-sort (the list l) #'p))))
  l)
(defmacro ht (&optional (fx 'equal)) `(make-hash-table :test #',fx))

; TODO: it is bad to copy this stuff all the time. how can we avoid it?
(declaim (inline psrt pkeys split-by-keys get-keys))
(defun psrt (l &optional (fx #'car))
  (declare (optimize speed (safety 1)) (list l) (function fx))
  "sort pairs by first element."
  (sort (copy-list l) #'string< :key fx))

(defun pkeys (a)
  (declare (optimize speed (safety 1)) (list a))
  (mapcar #'car (first a)))

(defun alist-get-values (keys aa)
  (declare (optimize speed (safety 0)) (list keys aa)) "get values of these keys"
  (loop for k of-type symbol in keys collect (get-var k aa)))

; TODO: utilize/ensure keys maintains order. rename pairs/get-keys?
(defun get-keys (keys aa)
  (declare (optimize speed (safety 1)) (list keys aa))
  "get only keys pairs. discard the rest. see split-by-keys"
  (loop for k of-type symbol in keys
        for x = (find k aa :test #'eq :key #'car)
        if x collect x))

(defun split-by-keys (keys aa)
  (declare (optimize speed (safety 1)) (list keys aa))
  "split keys pairs to the left. see get-keys"
  (loop for x of-type list in aa
        if (find (car x) keys :test #'eq) collect x into yes
        else collect x into no
        finally (return (values (psrt yes) (psrt no)))))

(defun alists/collapse-keys/ht (keys aa)
  (declare (optimize speed (safety 1)) (list keys aa))
  (loop with ht = (ht) for a in aa
        do (veq:mvb (in out) (split-by-keys keys a)
             (if (gethash in ht)
                 (setf (gethash in ht) (cons out (gethash in ht)))
                 (setf (gethash in ht) (list out))))
        finally (return ht)))

(defun alists/collapse-keys (agg keys aa)
  (declare (optimize speed (safety 1)) (list aa keys) (symbol agg))
  "collapse by keys and attach the remainder as agg"
  (when (not aa) (return-from alists/collapse-keys nil))
  (loop with ht = (alists/collapse-keys/ht keys aa)
        for k being the hash-keys of ht
        using (hash-value v)
        collect `((,agg . ,v) ,@k)))

(defun outer-join-pairs (aa bb)
  (declare (optimize speed (safety 1)) (list aa bb))
  (loop with res = (list) for a in aa
        do (loop for b in bb
                 do (push (psrt `(,@a ,@b)) res))
        finally (return res)))

(defun agg/grp (l &rest keys)
  (declare (optimize speed (safety 1)) (list l))
  (mapcar (lambda (row) (alist-get-values keys row)) l))
(defun agg/cnt (l &rest keys)
  (declare (optimize speed (safety 1)) (list l) (ignore keys))
  (length l))

(defun qry-and (aa bb)
  (declare (optimize speed (safety 1)) (list aa bb))
  "very messy, but faster than the naive approach"
  (when (< (length bb) (length aa)) (rotatef aa bb))
  (unless aa (return-from qry-and nil))

  (labels ((isect-same (aa bb &aux (ht (ht)) (res (list)))
             (loop for row in aa do (setf (gethash (psrt row) ht) t))
             (loop for row in bb for k = (psrt row)
                   if (gethash k ht) do (push k res))
             res)
           (isect-merge-1 (common-keys aa bb &aux (ht (ht)) (res (list)))
             (loop for a in aa do (setf (gethash a ht) t))
             (loop for b in bb
                   if (gethash (psrt (get-keys common-keys b)) ht)
                   do (push (psrt b) res))
           res)
           (isect-merge-2 (common aa bb &aux (res (list)))
             (let ((lft (alists/collapse-keys/ht common aa))
                   (rht (alists/collapse-keys/ht common bb)))
               (when (> (hash-table-count lft) (hash-table-count rht)) (rotatef lft rht))
               (loop for root being the hash-keys of lft using (hash-value ll)
                     for rr = (gethash root rht)
                     if rr do (loop for x in (outer-join-pairs ll rr)
                                    do (push (psrt `(,@root ,@x)) res)))
               res)))

    (let* ((ka (pkeys aa)) (kb (pkeys bb))
           (common (psrt (intersection ka kb :test #'eq) #'values)))

      (unless common (return-from qry-and nil)) ; no common variables
      (when (= (length common) (length (union ka kb :test #'eq)))
            (return-from qry-and (isect-same aa bb)))

      (when (= (length common) (length ka)) ; slice by a
            (return-from qry-and (isect-merge-1 common aa bb )))
      (when (= (length common) (length kb)) ; slice by b
            (return-from qry-and (isect-merge-1 common bb aa )))
      (isect-merge-2 common aa bb))))

(defun qry-or (aa bb &optional select)
  (declare (optimize speed (safety 1)) (list aa bb)) "logical sets, or"
  (when (not aa) (return-from qry-or bb))
  (when (not bb) (return-from qry-or aa))
  (labels ((do-merge (ht common a)
             (setf (gethash (psrt (get-keys common a)) ht) t)))
    (let ((common (or select (intersection (pkeys aa) (pkeys bb) :test #'eq)))
          (ht (ht)) (res (list)))
      (loop for a in aa do (do-merge ht common a))
      (loop for b in bb do (do-merge ht common b))
      (loop for k being the hash-keys of ht do (push k res))
      res)))

(defun qry/project (aa select)
  (declare (optimize speed (safety 1)) (list aa select))
  "select these vars from aa, and deduplicate rows"
  (when (not aa) (return-from qry/project nil))
  (labels ((do-merge (ht select a)
             (declare (hash-table ht) (list select a))
             (setf (gethash (psrt (get-keys select a)) ht) t)))
    (let ((ht (ht)) (res (list)))
      (loop for a in aa do (do-merge ht select a))
      (loop for k being the hash-keys of ht do (push k res))
      res)))

(defun qry-not (orig not* &optional select)
  (declare (optimize speed (safety 1)) (list orig not*)) "logical sets not"
  ; early exit when nothing to subtract or nothing to return
  ; orig will either be nil, or whatever was passed in (when not is nil)
  (when (or (not orig) (not not*)) (return-from qry-not orig))
  (labels ((do-merge (ht common a)
             (push a (gethash (psrt (get-keys common a)) ht)))
           (do-rem (ht common a) (remhash (psrt (get-keys common a)) ht)))
    (let ((common (or select (intersection (pkeys not*) (pkeys orig) :test #'eq)))
          (ht (ht)) (res (list)))
      (loop for a in orig do (do-merge ht common a))
      (loop for n in not* do (do-rem ht common n))
      (loop for v being the hash-values of ht
            do (loop for p in v do (push p res)))
      res)))

(defun qry-filter (a b fx)
  (declare (optimize speed (safety 1)) (list a) (ignore b) (function fx))
  "used for % filter clauses."
  (remove-if-not fx a))

(defun rules/prev-matches (var &rest pairs)
  (declare (optimize speed (safety 1)) (list var))
  "get the hits from the last iteration for linear rules."
  ; TODO: drop _ pairs!!!
  (mvb (pairs filters)
       (filter-by-predicate pairs (lambda (p) (or (var? p) (any? p))) :key #'cdr)
    (labels ((filter-match-all (v) (declare (list v))
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
  (mapcar (lambda (f) (declare (list f))
            (mapcar (lambda (a) (declare (symbol a)) (get-var a f)) args))
          l))

