(in-package :grph)


(defun has-symchar? (s c &optional (pos :first))
  (declare (ignorable pos))
  (let ((name (symbol-name s)))
    (declare (string name))
    (eq (char name (case pos (:first 0) (otherwise (1- (length name))))) c)))
(defun interned? (s) (and (symbolp s) (symbol-package s)))
(defun var? (s) (and (interned? s) (has-symchar? s #\?)))
(defun symlen (s) (declare (symbol s)) (length (symbol-name s)))
(defun any? (s) (and (interned? s) (= (symlen s) 1) (has-symchar? s #\_)))
(defun eq-car? (a s) (eq (car a) s))
(defun val? (s) (and (symbolp s) (not (symbol-package s))))
(defun free? (s)
  (cond ((or (var? s) (any? s)) t)
        ((or (val? s) (numberp s) (keywordp s)) nil)
        (t (error "FREE?: unexpected clause: ~a." s))))

(defun symb-sort-fx (a b) (string-lessp (symbol-name a) (symbol-name b)))
(defun varset-sort (a &optional (fx #'car))
  (sort (copy-tree a) #'symb-sort-fx :key fx))
(defun all-vars (a) (undup (varset-sort (remove-if-not #'var? (undup (awf a)))
                                        #'identity)))


(defun var-to-val (in w &aux (res (list)))
  (labels
    ((new (s &aux (gs (gensym (string-upcase (mkstr s)))))
       (push (list gs s) res)
       gs)
     (convert (s &aux (m (car (member s res :key #'second))))
       (if m (car m) (new s)))
     (rec (w)
       (cond ((null w) w)
             ((and (var? w) (member w in)) (convert w))
             ((atom w) w)
             ((consp w) (cons (rec (car w)) (rec (cdr w)))))))
    (let ((w* (rec w))) (values (reverse res) w*))))


(defun match-var-and (aa bb &aux (res (list)))
  (declare (list aa bb))
  (labels ((merge-and (a b)
             (loop for (bkey . bval) in b
                   for aval = (cdr (assoc bkey a))
                   do (cond ((not aval)
                               (setf a (acons bkey bval a)))
                            ((not (equal aval bval))
                               (return-from merge-and (values nil nil)))))
             (values a t)))
    (loop for a in aa do (loop for b in bb
          do (mvb (mset match) (merge-and a b)
               (when (and match (not (member mset res)))
                     (push mset res))))))
  res)

(defun match-var-or (aa bb)
  (declare (list aa bb))
  (union aa bb :test #'equalp))

(defun match-var-not (aa bb &aux (res (list)))
  (declare (list aa bb))
  (labels ((merge-not (a b)
            (loop for (bkey . bval) in b
                  for aval = (cdr (assoc bkey a))
                  if (and aval bval (equal aval bval))
                  do (return-from merge-not t))))
    (loop for a in aa
          for u = (loop named inner for b in bb
                        if (merge-not a b) do (return-from inner t))
          unless u do (push a res)))
  res)


(defun sort-nots (a)
  (if (and (listp a) (eq-car? a 'and))
      (mvb (yes no) (filter-by-predicate (cdr a)
                       (lambda (p) (and (listp p) (eq-car? p 'not))))
        `(and ,@yes ,@no))
      a))


(defmacro mode (expr)
  `(values ,expr ,(intern (mkstr (car expr)) :keyword)))


(defmacro compile-qry-where (compile-match-clause &key where in)
  (declare (symbol compile-match-clause))
  "compile datalog query"
  (unless (every #'var? in)
          (warn "QRY COMPILE: got bad value for :in: ~a." in))

  (mvb (in where) (var-to-val in where)
    (labels
      ((compile-match-clause (q) `(block ,(apply #'symb (interject q))
                                  (,compile-match-clause ,q)))
       (compile-next (qc &aux (qc (sort-nots qc)))
         (unless qc (warn "empty clause in :where: ~a" where))
         (case (car qc) (and (mode (compile-and #1=(cdr qc))))
                        (or (mode (compile-or #1#)))
                        (not (mode (compile-not #1#)))
                        (t (mode (compile-match-clause qc)))))
       (do-compile-and-or (qc fxname)
         (reduce (lambda (res qc)
                   (awg (nxt res*)
                     (mvb (nxt* mode) (compile-next qc)
                       (if res `(let ((,res* ,res) (,nxt ,nxt*))
                                 (,(if (equal :compile-not mode)
                                       'match-var-not fxname)
                                       ,res* ,nxt))
                                nxt*))))
                 (reverse qc) :initial-value (list)))
       (bad-or-clause (qc)
         (dsb (a . vars) (mapcar #'all-vars qc)
           (loop for b in vars unless (equal a b)
                 do (return-from bad-or-clause t))))
       (compile-or (qc)
         (when (bad-or-clause qc) (warn "QRY-COMPILE: bad OR clause: ~a." qc))
         (do-compile-and-or qc 'match-var-or))
       (compile-and (qc) (do-compile-and-or qc 'match-var-and))
       (compile-not (qc)
          (compile-next (if (= (length qc) 1) (car qc) `(and ,@qc)))))

      `(let (,@in) ,(compile-next where)))))

; TODO: reduce?
(defmacro qry (g &key where select in then collect)
  (declare (symbol g) (list select where))
  (cond ((not (every #'var? select))
         (warn "QRY-COMPILE: got bad value for :select: ~a." select))
        ((> (loop for s in `(,then ,collect) if s summing 1) 1)
         (warn "QRY-COMPILE: expecting :then, :collect or neither."))
        ((not (subsetp select (all-vars where)))
          (warn "QRY-COMPILE: unecpected var in :select: ~a ~%for :where: ~a."
                select where)))
  (awg (compile-match-clause f q m s res)
    (labels ((cdar-member (l) `(cdar (member ',l ,s :key #'car)))
             (select-let (then select)
               `(let (,@(loop for l in select collect `(,l ,(cdar-member l))))
                  ,then))
             (select-list (select)
               `(list ,@(loop for l in select collect (cdar-member l)))))

      `(macrolet
        ((,compile-match-clause (,q) ; q == (l mid r)
          `(let ((,',res (list)))
             (match (,',g ,',f ,@,q)
               (let ((,',m (remove-if-not
                             (lambda (,',s) (var? (car ,',s))) ,',f)))
                 (when (and ,',m (not (member ,',m ,',res :test #'equalp)))
                       (push ,',m ,',res))))
             ,',res)))

        (let ((,res (compile-qry-where ,compile-match-clause :where ,where :in ,in)))
          ,(cond (then `(map nil (lambda (,s) ,(select-let then select)) ,res))
                 (collect `(mapcar (lambda (,s) ,(select-let collect select)) ,res))
                 ; use dotted pairs?
                 (t `(mapcar (lambda (,s) ,(select-list select)) ,res))))))))

(defun -match (g f lft mid rht body &optional (full t))
  (awg (l p r eset has v s)
    (labels
      ((pattern () (list (free? lft) (free? mid) (free? rht)))
       (mem (l r body) `(when (@mem ,g ,l ,r) ,body))
       (prop (l p r body) `(when (@prop ,g (list ,l ,r) ,p) ,body))
       (fact (l p r)
         (if full
           `(let ((,f `((,',lft . ,,l) (,',mid . ,,p) (,',rht . ,,r)))) ,@body)
           `(let ((,f (list ,l ,p ,r))) ,@body)))
       (hit (pat &rest rest) (equal pat rest))
       (do-lft (r body)
         `(let ((,eset (@ (adj ,g) ,lft)))
            (when ,eset (do-map (,r ,has ,eset)
                         (declare (ignorable ,has))
                         ,(mem lft r body)))))
       (do-rht (l body)
         `(let ((,eset (@ (adj ,g) ,rht)))
            (when ,eset (do-map (,l ,has ,eset)
                          (declare (ignorable ,has))
                          ,(mem l rht body)))))
       (do-props (l p r body)
         `(do-map (,p ,v (or (@ (props ,g) (list ,l ,r))
                             (fset:map (:_ t))))
           (declare (ignorable ,v))
           ,body)))

      (let ((pat (pattern)))
        (cond

          ((hit pat nil nil nil) (mem lft rht (prop lft mid rht (fact lft mid rht))))
          ((hit pat nil t nil) (mem lft rht (do-props lft p rht (fact lft p rht))))

          ((hit pat nil nil t) (do-lft r (prop lft mid r (fact lft mid r))))
          ((hit pat nil t t) (do-lft r (do-props lft p r (fact lft p r))))

          ((hit pat t nil nil) (do-rht l (prop l mid rht (fact l mid rht))))
          ((hit pat t t nil) (do-rht l (do-props l p rht (fact l p rht))))

          ((hit pat t t t)
            `(do-map (,l ,eset (adj ,g))
               (do-map (,r ,has ,eset)
                 (declare (ignorable ,has))
                 (when ,has ,(do-props l p r (fact l p r))))))
          ((hit pat t nil t) ; if props has verts, we need a fixnum test here:
            `(do-set (,s (or (@ (mid ,g) ,mid) ,nilset))
              (typecase ,s (list (dsb (,l ,r) ,s ,(fact l mid r)))
                           (t (error "QRY-MATCH: unexpected clause: ~a ~a ~a"
                                     ',lft ',mid ',rht))))))))))

(defmacro %match ((g f lft mid rht) &body body) (-match g f lft mid rht body nil))
(defmacro match ((g f lft mid rht) &body body) (-match g f lft mid rht body t))

