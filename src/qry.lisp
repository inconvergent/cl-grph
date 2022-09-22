(in-package :grph)


(defun has-symchar? (s c &optional (pos :first))
  (declare (ignorable pos))
  (let ((name (symbol-name s)))
    (declare (string name))
    (eq (char name (case pos (:first 0) (otherwise (1- (length name))))) c)))
(defun interned? (s) (and (symbolp s) (symbol-package s)))
(defun var? (s) (and (interned? s) (has-symchar? s #\?)))
(defun var^ (v &aux (v (mkstr v)))
  (and (> (length v) 1) (equalp (char v 0) #\^)))
(defun symlen (s) (declare (symbol s)) (length (symbol-name s)))
(defun any? (s) (and (interned? s) (= (symlen s) 1) (has-symchar? s #\_)))
(defun eq-car? (a s) (eq (car a) s))
(defun val? (s) (and (symbolp s) (not (symbol-package s))))
(defun free? (s)
  (cond ((or (var? s) (any? s)) t)
        ((or (val? s) (numberp s) (keywordp s)) nil)
        (t (error "FREE?: unexpected clause: ~a." s))))
(defun bad-or-clause? (qc)
  (dsb (a . vars) (mapcar #'all-vars qc)
    (loop for b in vars unless (equal a b)
          do (return-from bad-or-clause? t))))

(defun symb-sort-fx (a b) (string-lessp (symbol-name a) (symbol-name b)))
(defun varset-sort (a &optional (fx #'car))
  (sort (copy-tree a) #'symb-sort-fx :key fx))
(defun all-vars (a) (undup (varset-sort (remove-if-not #'var? (undup (awf a)))
                                        #'identity)))

(defun distinct (&rest rest &aux (n (length rest)))
  (= n (length (grph::undup rest))))

(defmacro smallest-first (&rest rest &aux (a (car rest)))
  (declare (symbol a))
  `(and ,@(loop for b of-type symbol in (cdr rest) collect `(< ,a ,b))))
(defmacro largest-first (&rest rest &aux (a (car rest)))
  (declare (symbol a))
  `(and ,@(loop for b of-type symbol in (cdr rest) collect `(> ,a ,b))))

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
                            ((not (eql aval bval))
                               (return-from merge-and (values nil nil)))))
             (values a t)))
    (loop for a in aa do (loop for b in bb
          do (mvb (mset match) (merge-and a b)
               (when (and match (not (member mset res)))
                     (push mset res))))))
  res)

(defun match-var-or (aa bb)
  (declare (list aa bb))
  (union aa bb :test #'equal))

(defun match-var-not (aa bb &aux (res (list)))
  (declare (list aa bb))
  (labels ((merge-not (a b)
            (loop for (bkey . bval) in b
                  for aval = (cdr (assoc bkey a))
                  if (and aval bval (eql aval bval))
                  do (return-from merge-not t))))
    (loop for a in aa
          for u = (loop named inner for b in bb
                        if (merge-not a b) do (return-from inner t))
          unless u do (push a res)))
  res)


(defun sort-and-clauses (a)
  (labels ((do-split (l op)
             (mvb (yes no) (filter-by-predicate l
                             (lambda (p) (and (listp p) (eq-car? p op))))
               `(,@yes ,@no))))
   (if (and (listp a) (eq-car? a 'and))
       (let ((l (cdr a)))
         `(and ,@(do-split (do-split l :exe) 'not)))
      a)))


(defmacro mode (expr)
  `(values ,expr ,(intern (mkstr (car expr)) :keyword)))


(defmacro compile-qry-where (compile-match-clause &key where in)
  (declare (symbol compile-match-clause))
  "compile datalog query"
  (unless (every #'var? in)
          (error "QRY: got bad value for :in: ~a." in))

  (mvb (in where) (var-to-val in where)
    (labels
      ((compile-match-clause (q) `(block ,(apply #'symb (interject q))
                                  (,compile-match-clause ,q)))
       (compile-next (qc &aux (qc (sort-and-clauses qc)))
         (unless qc (warn "QRY: empty clause in :where: ~a" where))
         (case (car qc) (and (mode (compile-and #1=(cdr qc))))
                        (or (mode (compile-or #1#)))
                        (not (mode (compile-not #1#)))
                        (t (mode (compile-match-clause qc)))))
       (do-compile-and-or (qc fxname)
         (reduce (lambda (res qc)
                   (awg (nxt* res*)
                     (mvb (nxt mode) (compile-next qc)
                       (if res `(let ((,res* ,res) (,nxt* ,nxt))
                                 (,(if (equal :compile-not mode)
                                       'match-var-not fxname)
                                       ,res* ,nxt*))
                                nxt))))
                 (reverse qc) :initial-value (list)))
       (compile-or (qc)
         (when (bad-or-clause? qc) (warn "QRY: bad OR clause: ~a." qc))
         (do-compile-and-or qc 'match-var-or))
       (compile-and (qc) (do-compile-and-or qc 'match-var-and))
       (compile-not (qc)
          (compile-next (if (= (length qc) 1) (car qc) `(and ,@qc)))))

      `(let (,@in) ,(compile-next where)))))


; TODO: collect-res var, result list var
; TODO: or-join
(defmacro qry (g &key using where select in
                      filter
                      then collect first
                      (itr (gensym "ITR"))
                 &aux (select (ensure-list select))
                      (in (ensure-list in))
                      (using (ensure-list using)))
  (declare (symbol g) (list select where using in first) (symbol itr))

  (unless (every #'var^ using)
          (error "QRY: incorrect symb in :using ~a. use eg: ^s." using))
  (unless (every #'var? select)
          (error "QRY: got bad value for :select: ~a." select))
  (when (intersection select in)
        (error "QRY: :select ~a and :in ~a can not overlap." select in))
  (when (> (length (remove-if-not #'identity (list then collect first))) 1)
        (error "QRY: use either :then, :first, or :collect."))
  (when (not (subsetp select (all-vars where)))
        (warn "QRY: unexpected var in :select: ~a ~%for :where: ~a." select where))

  (awg (compile-match-clause f q m s res stop* collect-res)
    (labels
      ((cdar-member (l) `(cdar (member ',l ,s :key #'car :test #'eq)))
       (select-with (select)
         (loop with res = (list) for l in select
               do (setf res `(,@res for ,l = ,(cdar-member l)))
               finally (return res)))
       (select-list (select)
         `(list ,@(loop for l in select collect (cdar-member l))))
       (re-intern (g) (intern (subseq (mkstr g) 1) (symbol-package g)))
       (bind-partial () (loop for g in using collect `(,g ,(re-intern g))))
       (re-bind-result ()
         `(setf ,@(awf (loop for g in using collect `(,(re-intern g) ,g)))))
       (itr-body ()
         (cond (first `(loop named lp
                             for ,s in ,res with ,itr = 0
                             ,@(select-with select)
                             ,@(when filter `(if ,filter))
                             do (return-from lp ,first)))
               (then `(loop for ,s in ,res for ,itr from 0
                            ,@(select-with select)
                            ,@(when filter `(if ,filter))
                            do ,then))
               (collect `(loop for ,s in ,res for ,itr from 0
                               ,@(select-with select)
                               ,@(when filter `(if ,filter))
                               collect ,collect))
               (t `(loop for ,s in ,res for ,itr from 0
                         ,@(select-with select)
                         ,@(when filter `(if ,filter))
                         collect (list ,@select))))))
      `(macrolet
         ; cancel and ignore all results
        ((cancel (&body body) `(return-from ,',stop* (progn ,@body)))
         ; stop, but keep the results
         (stop (&body body) `(progn ,',(re-bind-result)
                               (return-from ,',stop* (progn ,@body))))
         (,compile-match-clause (,q) ; q == (l mid r)
          `(let ((,',res (list)))
             (match (,',g ,',f ,@,q)
               (let ((,',m (remove-if-not
                             (lambda (,',s) (var? (car ,',s))) ,',f)))
                 (when ,',m (push ,',m ,',res))))
             (remove-duplicates ,',res :test #'equal))))

        (let ((,collect-res nil)
              (,res (compile-qry-where ,compile-match-clause
                      :where ,where :in ,in))
              ,@(bind-partial))
          (block ,stop* (setf ,collect-res ,(itr-body))
                        ,(re-bind-result)
                        ,collect-res))))))

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
              (typecase ,s
                (list (dsb (,l ,r) ,s ,(fact l mid r)))
                (t (error "QRY: unexpected clause: ~a ~a ~a"
                          ',lft ',mid ',rht))))))))))

(defmacro %match ((g f lft mid rht) &body body) (-match g f lft mid rht body nil))
(defmacro match ((g f lft mid rht) &body body) (-match g f lft mid rht body t))

(defmacro collect-while ((&key (init '(list)) (test 'not) lim) &body body)
  (declare (type (or null number) lim))
  (grph::awg (res)
    `(concatenate 'list ,init
      (loop ,@(when lim `(repeat ,lim))
            for ,res = (progn ,@body)
            until (,test ,res)
            if ,res collect ,res))))

(defun get-kv (l k &optional d &aux (v (member k (group l 2) :key #'car)))
  (declare (list l) (keyword k))
  (if v (cadar v) d))

(defun strip-kvs (l strip &aux (res (list)))
  (declare (list l strip))
  (loop for (k v) in (remove-if (lambda (k) (member k strip))
                                (group l 2) :key #'car)
        do (push (the keyword k) res) (push v res))
  (reverse res))

; TODO: itr shadows inner itt
(defmacro qry-collect-while (g &rest rest)
  (declare (symbol g))
  `(collect-while (:init ,(get-kv rest :init '(list))
                   :lim ,(get-kv rest :lim 'nil))
    (qry ,g ,@(strip-kvs rest '(:init :lim)))))

