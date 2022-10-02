(in-package :grph)

; TODO: features grph-parallel
(defmacro qry-compile-where (gather-match &key where in)
  (declare (symbol gather-match))
  "compile datalog query. gather-match is the name of the macro used to compile
  the individual clauses."
  (unless (and (every #'var? in) (no-dupes? in))
          (error "QRY: got bad value for :in ~a." in))
  (labels
    ((gs (c) (gensym (apply #'mkstr (interject c :/))))
     (-map/or (qc)
        (let ((res (mapcar (lambda (qc) `(,(gs qc) ,(-next qc))) qc)))
          `(,(psel 'let) (,@res)
             (declare (list ,@(mapcar #'first res)))
             ,(loop with body = (caar res)
                    for s in (mapcar #'first (cdr res))
                    do (setf body `(qry-or ,s ,body))
                    finally (return body)))))
     (-map/and-not (qc)
        (let ((res (mapcar (lambda (qc)
                              (if (eq-car? qc 'not)
                                `(,(gs qc) ,(-next `(and ,@(cdr qc))) t)
                                `(,(gs qc) ,(-next qc) nil)))
                           qc)))
          `(,(psel 'let) (,@(mapcar (lambda (c) (subseq c 0 2)) res))
             (declare (list ,@(mapcar #'first res)))
             ,(loop with body = (caar res)
                    for s in (mapcar #'first (cdr res))
                    for not? in (mapcar #'third res)
                    do (setf body `(,(psel (if not? 'not 'and)) ,s ,body))
                    finally (return body)))))
     (-next (qc)
       (unless qc (error "QRY: empty clause in :where ~a." where))
       (unless (get-all-vars qc) (warn "QRY: clause ~a has no vars." qc))
       (case (car qc) (and (-map/and-not (sort-not-clauses (cdr qc))))
                      (or (check-or-clause (cdr qc)) (-map/or (cdr qc)))
                      (t `(,gather-match ,qc)))))

    (mvb (in where) (shadow-in-vars in where)
      `(let (,@in) ,(-next where)))))

(defmacro match ((g f lft mid rht) &body body)
  (declare (symbol g f))
  "execute body with alist f as every bindable var for every fact in the graph
that matches the pattern (lft mid rht). f is on the form ((?A . 0) (?P . :a))."
  (unless (or (bindable? lft) (bindable? mid) (bindable? rht))
          (error "MATCH: no bindable vars in: ~a." `(,lft ,mid ,rht)))
  (awg (l p r eset has s)
    (labels
      ((hit (pat &rest rest) (equal pat rest))
       (fact (l p r)
         `(let ((,f (list ,@(when (bindable? lft) `((cons ',lft ,l)))
                          ,@(when (bindable? mid) `((cons ',mid ,p)))
                          ,@(when (bindable? rht) `((cons ',rht ,r))))))
            (declare (list ,f))
            ; (format t "~& ----> ~{~s ~}" ,f)
            ,@body))

       (mem (l r body) `(when (@mem ,g ,l ,r) ,body))
       (prop (l p r body) `(when (@prop ,g (list ,l ,r) ,p) ,body))
       (do-lft (r body)
         `(let ((,eset (@ (adj ,g) ,lft)))
            (when ,eset (do-map (,r ,has ,eset)
                          (declare (ignorable ,r ,has))
                          ,(mem lft r body)))))
       (do-rht (l body)
         `(let ((,eset (@ (adj ,g) ,rht)))
            (when ,eset (do-map (,l ,has ,eset)
                          (declare (ignorable ,l ,has))
                          ,(mem l rht body)))))
       (do-props (l p r body)
         `(do-map (,p ,has (or (@ (props ,g) (list ,l ,r)) (fset:map (:_ t))))
            (declare (ignorable ,p ,has))
            ,body)))
      (let ((pat (list (free? lft) (free? mid) (free? rht))))
        (cond
          ((hit pat nil nil nil) (mem lft rht (prop lft mid rht (fact lft mid rht))))
          ((hit pat nil t nil) (mem lft rht (do-props lft p rht (fact lft p rht))))
          ((hit pat nil nil t) (do-lft r (prop lft mid r (fact lft mid r))))
          ((hit pat nil t t) (do-lft r (do-props lft p r (fact lft p r))))
          ((hit pat t nil nil) (do-rht l (prop l mid rht (fact l mid rht))))
          ((hit pat t t nil) (do-rht l (do-props l p rht (fact l p rht))))
          ((hit pat t t t)
             `(do-map (,l ,eset (adj ,g))
                (declare (ignorable ,l))
                (do-map (,r ,has ,eset)
                  (declare (ignorable ,r ,has))
                  (when ,has ,(do-props l p r (fact l p r))))))
          ; NOTE: if props has verts, we need a fixnum test here?
          ((hit pat t nil t)
             `(do-set (,s (or (@ (mid ,g) ,mid) ,nilset))
                (typecase ,s (list (dsb (,l ,r) ,s
                                     (declare (ignorable ,l ,r))
                                     ,(fact l mid r)))
                             (t (error "MATCH: unexpected clause: (~a ~a ~a)."
                                       ',lft ',mid ',rht))))))))))

(defmacro gather-match (g l p r)
  (declare (symbol g))
  (awg (gather-res f)
    `(let ((,gather-res (list)))
       (declare (list ,gather-res))
       (match (,g ,f ,l ,p ,r) (push ,f ,gather-res))
       ; match will yield duplicates in some cases.
       (remove-duplicates ,gather-res :test #'equal))))

(declaim (inline get-var))
(defun get-var (s l)
  (declare (optimize speed) (symbol s) (list l))
  (cdr (find s l :key #'car :test #'eq)))

; TODO: collect-res var, result list var
; TODO: or-join
(defmacro qry (g &key in using select when where then collect first
                      (itr (gensym "QRY-ITR")) (proc 'identity) ; TODO: how to do proc?
                 &aux (select (ensure-list select)) (in (ensure-list in))
                      (using (ensure-list using)))
  (declare (symbol g) (list select where using in first) (symbol itr))

  (unless (and (every #'var^ using) (no-dupes? using))
          (error "QRY: got bad value for :using ~a." using))
  (unless (and (every #'var? select) (no-dupes? select))
          (error "QRY: got bad value for :select ~a." select))
  (when (intersection select in)
        (error "QRY: :select ~a and :in ~a can not overlap." select in))
  (when (> (length (remove-if-not #'identity `(,then ,collect ,first))) 1)
        (error "QRY: use either :then, :first, :collect; or neither."))
  (when (not (subsetp select (get-all-vars where)))
        (warn "QRY: unexpected var in :select ~a,~%for :where ~a." select where))

  (awg (qry-gather q hit res stop* qry-final-res lp)
    (labels
      ((get-var (s) `(get-var ',s ,hit))
       (select-with ()
         (loop with res = (list) for s in select
               do (setf res `(,@res for ,s = ,(get-var s)))
               finally (return res)))
       (re-intern (g) (intern (subseq (mkstr g) 1) (symbol-package g)))
       (bind-partial () (loop for g in using collect `(,g ,(re-intern g))))
       (re-bind-result ()
         `(setf ,@(awf (loop for g in using collect `(,(re-intern g) ,g)))))
       (itr-body (res)
         (cond (first `(loop named ,lp
                             for ,hit of-type list in ,res
                             with ,itr of-type fixnum = 0
                             ,@(select-with)
                             ,@(when when `(if ,when))
                             do (return-from ,lp ,first)))
               (then `(loop for ,hit of-type list in ,res
                            and ,itr of-type fixnum from 0
                            ,@(select-with)
                            ,@(when when `(if ,when))
                            do ,then))
               (collect `(loop for ,hit of-type list in ,res
                               for ,itr of-type fixnum from 0
                               ,@(select-with)
                               ,@(when when `(if ,when))
                               collect ,collect))
               (t `(loop for ,hit of-type list in ,res
                         and ,itr of-type fixnum from 0
                         ,@(select-with)
                         ,@(when when `(if ,when))
                         collect (list ,@select))))))
      `(macrolet
         ; cancel and ignore all results
        ((cancel (&body body) `(return-from ,',stop* (progn ,@body)))
         ; stop, but keep the results
         (stop (&body body) `(progn ,',(re-bind-result)
                                    (return-from ,',stop* (progn ,@body))))
         (,qry-gather (,q) `(progn
                              ; (format t "~& clause: ~{~s ~}~%" ',,q)
                              (gather-match ,',g ,@,q))))

        (let ((,qry-final-res nil)
              (,res (,proc (qry-compile-where ,qry-gather
                             :where ,where :in ,in)))
              ,@(bind-partial))
          (block ,stop* (setf ,qry-final-res ,(itr-body res))
                        ,(re-bind-result)
                        ,qry-final-res))))))

(defmacro qry-collect-while (g &rest rest)
  (declare (symbol g))
  `(collect-while (:init ,(get-kv rest :init '(list))
                   :lim ,(get-kv rest :lim 1000)
                   :cres ,(get-kv rest :cres (gensym "COLLECT-RES"))
                   :citr ,(get-kv rest :citr (gensym "COLLECT-ITR")))
    (qry ,g ,@(strip-kvs rest '(:init :lim :cres :citr)))))

