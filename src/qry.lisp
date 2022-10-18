(in-package :grph)


(defun qry/check/proc/and (qc)
  "push not to the right (according to bindable vars), so not is executed
as soon as possible.

NOTE: qc should not include the litteral :and symb."
  (declare (list qc))
  (labels
    ((not-bindables (n &aux (s (car n)))
       (ecase s (:not-join (get-join-binds n))
                (:not (get-bindable n))
                (:% (get-bindable n))))
     (bind-search (c not*)
       (loop with not-vars = (get-all-vars not*)
             for i from 0 to (length c)
             for vars = (get-all-vars (subseq c i))
             if (null (intersection vars not-vars))
             do (return-from bind-search i))
       0)
     (split-fx (l fx)
       (mvb (hit c) (filter-by-predicate l fx)
         (let ((cb (get-bindable c)))
           (unless (every (lambda (n &aux (nb (not-bindables n)))
                             (subsetp nb cb))
                          hit)
                   (warn "QRY: bindable vars can not be bound for:~%~s." l)))
         (list hit c)))
     (bind-shift (hit c*)
       (loop for n in hit
             for i = (bind-search c* n)
             if hit do (setf c* `(,@(subseq c* 0 i) ,n ,@(subseq c* i))))
       c*)
     (bind-shift-all (c)
       (loop for fx in (list #'not? #'fx?)
             for (hit c*) = (split-fx c fx)
             do (setf c (bind-shift hit c*)))
       c))
    (let ((res (bind-shift-all qc)))
;       (when (not? (car res))
;             (error "QRY: incorrect NOT/NOT-JOIN push down in:
; ~s. missing clause?" res))
      res)))

(defun qry/check/or/not (qc)
  (when (some #'not? qc)
        (error "QRY: NOT/NOT-JOIN clause is not allowed in OR for:~%~{~s ~}." qc))
  (when (some #'fx? qc)
        (error "QRY: % clause is not allowed in OR/OR-JOIN for:~%~{~s ~}." qc)))

(defun qry/check/or (qc)
  (declare (list qc) (cons qc))
  (qry/check/or/not qc)
  (dsb (a . vars) (mapcar #'get-all-vars qc)
    (loop for b in vars
          unless (equal (sort (copy-list a) #'string<)
                        (sort (copy-list b) #'string<))
          do (return-from qry/check/or
               (warn "QRY: clauses in OR must have the same vars for:~%~{~s ~}." qc)))))


(defun qry/preproc/kv (qc*)
  (labels
    ((kv (s) (intern (string-upcase (symbol-name s)) :keyword))
     (rec/map (qc) (mapcar #'rec qc))
     (fact? (qc) (and (= (length qc) 3)
                      (every (lambda (c) (or (any? c) (val? c) (var? c))) qc)))
     (clause? (qc c) (and (listp qc) (symbolp (car qc)) (eq (kv (car qc)) c)))
     (rec (qc)
       (cond ((clause? qc :%) `(:% ,@(cdr qc)))
             ((clause? qc :q) `(:q ,@(cdr qc)))
             ((clause? qc :f) `(:f ,@(cdr qc)))
             ((clause? qc :or-join)
                `(:or-join ,(ensure-list (cadr qc)) ,@(rec/map (cddr qc))))
             ((clause? qc :not-join)
                `(:not-join ,(ensure-list (cadr qc)) ,@(rec/map (cddr qc))))
             ((and (listp qc) (symbolp (car qc)) (member (kv (car qc)) *valid-clauses*))
                `(,(kv (car qc)) ,@(rec/map (cdr qc))))
             (t (unless (fact? qc) (error "QRY: bad fact: ~s~%for: ~s" qc qc*))
                `(:fact ,@qc)))))
    (rec qc*)))

(defun qry/preproc (in where &aux (res (list)))
  "converts clause keywords to :keyword.

safeguards :in vars:
  for :in ?a and :w (AND (?A _ ?B) (?D _ ?C))
  returns (values ((?A32 ?A)) (AND (?A32 _ ?B) (?D _ ?C)))
  where ?A32 is a gensym.
  if :in is empty, nothing happens.

NOTE: resulting (:in) gensyms in :where will be counted as val?, not var?,
because gensyms have no symbol-package. as a result :in vals are not free."
  (labels
    ((new (s &aux (gs (gensym (string-upcase (mkstr s)))))
       (push (list gs s) res)
       gs)
     (convert (s &aux (m (car (member s res :key #'second :test #'eq))))
       (if m (car m) (new s)))
     (rec (where)
       (cond ((and (var? where) (member where in :test #'eq)) (convert where))
             ((consp where) (cons (rec (car where)) (rec (cdr where))))
             (t where))))
    ; NOTE: rec must be called before we return (reverse res)
    (let ((where (rec (qry/preproc/kv where))))
      (values (reverse res) where))))


(defun psel (k)
  #+:grph-parallel
  (ecase k (:not 'p/qry-not)
           (:and 'p/qry-and)
           (:or 'p/qry-or)
           (:let 'lparallel:plet)
           (:% 'p/qry-filter))
  #-:grph-parallel
  (ecase k (:not 'qry-not)
           (:and 'qry-and)
           (:or 'qry-or)
           (:let 'let)
           (:% 'qry-filter)))


(defmacro qry/compile-where (gather-match where in &key db)
  (declare (symbol gather-match))
  "compile datalog query. gather-match is the name of the macro used to compile
  the individual clauses."

  (unless (and (every #'var? in) (no-dupes? in))
          (error "QRY: got bad value for :in ~s." in))
  (labels
    ((gs (c) (gensym (mkstr (car c) :-)))
     (join-vars (clause qc &aux (jarg (get-join-binds qc)))
       (unless (and jarg (every #'var? jarg))
               (error "QRY: bad bind vars ~s in ~a: ~s." jarg clause where))
       jarg)
     (next (qc &aux (c (car qc)))
       (unless qc (warn "QRY: empty clause in :where ~s." where))
       (unless (get-all-vars qc) (warn "QRY: clause ~s has no vars." qc))
       (ecase c (:fact `(,gather-match ,(cdr qc)))
                (:and (next/map/and
                        (qry/check/proc/and (cdr qc))))
                (:or (qry/check/or (cdr qc))
                     (next/map/or (cdr qc)))
                (:or-join (qry/check/or/not (cdr qc))
                          (next/map/or (cddr qc) (join-vars :or-join qc)))
                (:q `(q ,@(cdr qc)))
                (:f `(progn ,@(cdr qc)))))
     (filter (qc)
       ; TODO: (% ?var fx ...) for bind to ?var
       (unless (symbolp (second qc)) (error "QRY: bad % filter: ~s " qc))
       (awg (f-)
         `(lambda (,f-)
          (,(second qc)
           ,@(mapcar (lambda (s)
                       (if (var? s) `(get-var ',s ,f-) s))
                     (subseq qc 2))))))

     ; TODO: next next/map is very confusing. try to refactor?
     (next/map (qc)
       (mapcar
         (lambda (qc &aux (c (car qc)) (gs (gs qc)))
           (ecase c
             (:fact `(,gs ,(next qc) :and nil))
             (:and `(,gs ,(next qc) :and nil))
             (:not `(,gs ,(next `(:and ,@(cdr qc))) :not nil))
             (:or `(,gs ,(next qc) :or nil))
             (:or-join `(,gs ,(next `(:or-join ,@(cdr qc))) :and nil))
             (:not-join `(,gs ,(next `(:and ,@(cddr qc)))
                              :not ((list ,@(mapcar (lambda (s) `(quote ,s))
                                                    (get-join-binds qc))))))
             (:q `(,gs (q ,@(cdr qc)) :and nil))
             (:% `(,gs nil :% (,(filter qc))))
             (:f `(,gs ,(second qc) :and nil))))
         qc))
     (next/map/or (qc &optional jarg &aux (res (next/map qc)))
       (res/inner res (loop with body = (caar res)
                            for s in (lpos (cdr res))
                            do (setf body `(,(psel :or) ,s ,body ',jarg))
                            finally (return body))))
     (next/map/and (qc &aux (res (next/map qc)))
       (res/inner res (loop with body = (caar res)
                            for s in (lpos (cdr res))
                            for (clause join-vars) in (lpos (cdr res) 2 4)
                            for fx = (psel clause)
                            do (setf body `(,fx ,body ,s ,@join-vars))
                            finally (return body))))
     (res/inner (res inner)
       (if (= (length res) 1) (cadar res)
           `(,(psel :let) (,@(lpos res 0 2))
              (declare (list ,@(lpos res)))
              ,inner))))
    (mvb (in where) (qry/preproc in where)
      (let ((res (next where))
            (s "~&--- compiled: ~s~%    with :in ~s~%>>>>>>>>~%~s~%<<<<<<<<~%"))
        (when db (format t s where in res))
        `(let (,@in) ,res)))))

; TODO: with modifier
; TODO: how to do proc?
; MAYBE rename :first? make :fx alternative?
(defmacro qry (g &key db in using select when where
                      then collect first pairs
                      (itr (gensym "QRY-ITR")) (proc 'identity)
                      (res (gensym "QRY-RES"))
                 &aux (select (ensure-list select))
                      (in (ensure-list in))
                      (using (ensure-list using)))
  (declare (symbol g) (list select where using in first)
           (symbol itr res) (boolean pairs))

  (unless where (error "QRY: missing :where."))
  (unless select (error "QRY: missing :select."))
  (when (and pairs using)
    (error "QRY: :pairs can not be combined with :using."))
  (unless (and (every #'var? select) (no-dupes? select))
          (error "QRY: got bad value for :select ~s." select))
  (when (intersection select in)
        (error "QRY: :select ~s and :in ~s can not overlap." select in))
  (when (> (length (remove-if-not #'identity `(,pairs ,then ,collect ,first))) 1)
        (error "QRY: use either :pairs :then, :first, :collect; or neither."))
  (unless (and (every #'^var? using) (no-dupes? using))
          (error "QRY: got bad value for :using ~s." using))
  (unless (subsetp select (get-all-vars where))
          (warn "QRY: unexpected var in :select ~s,~%for :where ~s."
                select where))

  (awg (qry-gather q hit stop* qry-final-res lp)
    (labels
      ((select-with ()
         (loop with selres = (list) for s in select
               do (setf selres `(,@selres for ,s = (get-var ',s ,hit)))
               finally (return selres)))
       (re-intern (g) (intern (subseq (mkstr g) 1) (symbol-package g)))
       (bind-partial () (loop for g in using collect `(,g ,(re-intern g))))
       (select-pairs () `(list ,@(mapcar (lambda (s) `(cons ',s ,s)) select)))
       (re-bind-result ()
         `(setf ,@(awf (loop for g in using collect `(,(re-intern g) ,g)))))
       (do/collect (c)
         `(loop for ,hit of-type list in ,res and ,itr of-type pn from 0
                ,@(select-with) ,@(when when `(if ,when)) ,@c))
       (itr-body ()
         (cond (first `(loop named ,lp with ,itr of-type pn = 0
                             for ,hit of-type list in ,res
                             ,@(select-with) ,@(when when `(if ,when))
                             do (return-from ,lp ,first)))
               (then (do/collect `(do ,then)))
               (collect (do/collect `(collect ,collect)))
               (pairs (do/collect `(collect ,(select-pairs))) )
               (t (do/collect `(collect (list ,@select)))))))
      `(macrolet
         ; cancel and ignore all results
        ((cancel (&body body) `(return-from ,',stop* (progn ,@body)))
         ; stop, but keep the results
         (stop (&body body) `(progn ,',(re-bind-result)
                                    (return-from ,',stop* (progn ,@body))))
         (,qry-gather (,q) () `(gather-match ,',g ,@,q))
         ; NOTE: nested queries, requires :pairs
         ; MAYBE: (qq) allow g, for querying different graph?
         (q (&rest rest) `(qry ,',g :pairs t :db ,,db ,@rest)))

        ; NOTE: is it possible to move select vars into qry-compile-where?
        (let ((,qry-final-res nil)
              (,res (,proc (select-vars (qry/compile-where ,qry-gather
                                          ,where ,in :db ,db)
                                        ',select)))
               ,@(bind-partial))
          (block ,stop* (setf ,qry-final-res ,(itr-body))
                        ,(re-bind-result)
                        ,qry-final-res))))))


(defmacro collect-while ((&key (init '(list)) (test 'not) (lim 1000)
                               (cres (gensym "COLLECT-RES"))
                               (citr (gensym "COLLECT-ITR")))
                          &body body)
  (declare (symbol cres))
  (awg (for-res lp)
    `(loop named ,lp
           with ,cres of-type list = ,init
           for ,for-res = (progn ,@body)
           for ,citr of-type fixnum from 0 below (the fixnum ,lim)
           until (,test ,for-res)
           if ,for-res do (push ,for-res ,cres)
           finally (return-from ,lp (reverse ,cres)))))

(defmacro qry-collect-while (g &rest rest)
  (declare (symbol g))
  `(collect-while (:init ,(get-kv rest :init '(list))
                   :lim ,(get-kv rest :lim 1000) ; RENAME clim?
                   :cres ,(get-kv rest :cres (gensym "COLLECT-RES"))
                   :citr ,(get-kv rest :citr (gensym "COLLECT-ITR")))
    (qry ,g ,@(strip-kvs rest '(:init :lim :cres :citr)))))

