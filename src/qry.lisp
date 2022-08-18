
(in-package :grph)

(defun symlen (s) (declare (symbol s)) (length (symbol-name s)))
(defun has-symchar? (s c &optional (pos :first))
  (let ((name (symbol-name s)))
    (declare (string name))
    (eq (char name (if :first 0 (1- (length name)))) c)))
(defun var? (s) (and (symbolp s) (has-symchar? s #\?)))
(defun any? (s) (and (symbolp s) (= (symlen s) 1) (has-symchar? s #\_)))
(defun match? (m) (every #'identity m))
(defun only-vars (m) (remove-if-not #'consp m))
(defun eq-car? (a s) (eq (car a) s))

(defun symb-sort-fx (a b) (string-lessp (symbol-name a) (symbol-name b)))

; TODO: fact reducer fx as arg to compile match

(defun varset-sort (a &optional (fx #'car))
  (sort (copy-tree a) #'symb-sort-fx :key fx))
(defun all-vars (a)
  (undup (varset-sort (tree-find-all a #'var?)
                            #'identity)))

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
      (mvb (yes no)
        (filter-by-predicate (cdr a)
          (lambda (p) (and (listp p) (eq-car? p 'not))))
        (concatenate 'list (list 'and) yes no))
      a))

(defun bad-clauses-or (qc)
  (dsb (a . vars) (mapcar #'all-vars qc)
    (loop for b in vars unless (equal a b)
                        do (return-from bad-clauses-or t))))


(defmacro mode (expr)
  `(values ,expr ,(intern (mkstr (car expr)) :keyword)))


(defmacro compile-query (fact-reduce &key where (select (all-vars where)) in)
  (declare (symbol fact-reduce) (cons select where))
  "compile a datalog query.

facts reduce should be the name of a function that will be used to
consider all relevant facts for a given stage. see facts-qry for an example."
  ; TODO:
  ;  - use in to set variables. syntax?
  ;  - use select to filter variables
  (awg (fact)
    (labels
      ((compile-match1 (q fact)
         (loop for qs in q for f in fact
               collect (cond ((any? qs) t)
                             ((var? qs) `(cons ',qs ,f))
                             ((keywordp qs) `(eq ,qs  ,f))
                             ((numberp qs) `(= ,qs  ,f))
                             ((symbolp qs) `(equal ,qs  ,f))
                             (t (error "MATCH1: bad symb: ~s" qs)))))

       (compile-match (q)
         (awg (m)
           (let ((nsym (-gensyms :match 3)))
             `(,fact-reduce
                (lambda (,fact)
                  (dsb ,nsym ,fact
                    (declare (ignorable ,@nsym))
                    (let ((,m (list ,@(compile-match1 q nsym))))
                      (when (match? ,m) (only-vars ,m)))))
                ',q))))

       ; TODO: strip all with no var?
       (compile-next (qc &aux (qc (sort-nots qc)))
         (case (car qc) (and (mode (compile-and #1=(cdr qc))))
                        (or (mode (compile-or #1#)))
                        (not (mode (compile-not #1#)))
                        (t (mode (compile-match qc)))))

       (do-compile-and-or (qc fxname)
         (reduce
           (lambda (res qc)
             (awg (nxt res*)
               (mvb (nxt* mode) (compile-next qc)
                 (if res `(let ((,res* ,res) (,nxt ,nxt*))
                           (,(if (equal :compile-not mode)
                                 'match-var-not fxname)
                                 ,res* ,nxt))
                          nxt*))))
           (reverse qc) :initial-value (list)))

       (compile-and (qc)
         (unless qc (warn "AND: missing qc?"))
         `(block ,(symb :and qc)
            ,(do-compile-and-or qc 'match-var-and)))

       (compile-or (qc)
         (unless qc (warn "OR: missing qc?"))
         (when (bad-clauses-or qc) (warn "bad OR clause ~a" qc))
         `(block ,(symb :or qc)
           ,(do-compile-and-or qc 'match-var-or)))

       (compile-not (qc)
          (unless qc (warn "NOT: missing qc?"))
          `(block ,(symb :not qc)
            ,(compile-next
               (if (= (length qc) 1) (car qc) `(and ,@qc))))))

      `(progn ,(compile-next where)))))


(defmacro facts-qry (facts &rest qry)
  "run this datalog qry on all input facts."
  (awg (fact-reduce fx f m res lp)
   `(labels
      ((,fact-reduce (,fx &rest rest)
        (declare (ignore rest))
         (loop named ,lp
               with ,res of-type list = (list)
               for ,f of-type list in ,facts
               for ,m of-type cons = (funcall ,fx ,f)
               if ,m do (push ,m ,res)
               finally (return-from ,lp ,res))))
      (compile-query ,fact-reduce ,@qry))))


(defun splice-fact (e p)
  (awg (a b) `(dsb (,a ,b) ,e (list ,a ,p ,b))))
(defun select-index (p eset g q &aux (q (second q)))
  (if (or (any? (second q)) (var? (second q)))
     `(do-map (,p ,eset (inv ,g)))
     `(let* ((,p ,(second q))
             (,eset (@ (inv ,g) ,p))))))

(defmacro qry2 (g &rest qry)
  (declare (symbol g))
  (awg (fact-reduce fx p e q eset m res)
    `(macrolet
       ((,fact-reduce (,fx ,q) ; q == (a p b)
         `(let ((,',res (list)))
           ; how to included non prop edges?
           (,@(select-index ',p ',eset ',g ,q);do-map (,',p ,',eset (inv ,',g))
           (do-set (,',e ,',eset)
             (typecase ,',e
               (cons (let ((,',m (funcall ,,fx ,(splice-fact ',e ',p))))
                       (when ,',m (push ,',m ,',res))))
               (fixnum nil)
               (t (error "unexpected element in inv")))))
           ,',res)
         ))
     (compile-query ,fact-reduce ,@qry))))

