(in-package :grph)


(defun qry/check/proc/and (qc &aux (qc (cdr qc)))
  "push not to the right (according to bindable vars), so not is executed
as soon as possible."
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
        (error "QRY: % clause is not allowed in OR/OR-JOIN for:~%~{~s ~}." qc))
  qc)

(defun qry/check/or (qc &aux (qc (cdr qc)))
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


(defmacro qry/compile-where (where in &key db)
  "compile datalog query."
  (unless (and (every #'var? in) (no-dupes? in))
          (error "QRY: duplicate/bad value for :in ~s." in))
  (labels
    ((gs (c) (gensym (mkstr (car c) :-)))
     (join-vars (clause qc &aux (jarg (get-join-binds qc)))
       (unless (and jarg (every #'var? jarg))
               (error "QRY: bad bind vars ~s in ~a: ~s." jarg clause where))
       jarg)
     (next (qc &aux (c (car qc)))
       (unless qc (warn "QRY: empty clause in :where ~s." where))
       (unless (get-all-vars qc) (warn "QRY: no vars in ~s" qc))
       (ecase c (:fact `(fact ,(cdr qc)))
                (:q `(q ,@(cdr qc)))
                (:f `(progn ,@(cdr qc)))
                (:and (next/map/and (qry/check/proc/and qc)))
                (:or (qry/check/or qc)
                     (next/map/or (cdr qc)))
                (:or-join (qry/check/or/not (cdr qc))
                          (next/map/or (cddr qc) (join-vars :or-join qc)))))
     (filter (qc) ; TODO: (% ?var fx ...) for bind to ?var
       (awg (f-)
         (etypecase (second qc) ; cons (% (= ?a ...)), symbol: (% = ?a ...)
           (cons `(lambda (,f-) (,(caadr qc) ,@(rec/get-var f- (cdadr qc)))))
           (symbol `(lambda (,f-) (,(cadr qc) ,@(rec/get-var f- (cddr qc))))))))

     ; TODO: next next/map is still confusing. try to refactor?
     (next/map (qc)
       (mapcar
         (lambda (qc &aux (c (car qc)) (gs (gs qc)))
           (ecase c
             ((:fact :and :f :q ) `(,gs ,(next qc) :and nil))
             (( :or :or-join) `(,gs ,(next qc) :and nil))
             (:% `(,gs nil :% (,(filter qc))))
             (:not `(,gs ,(next `(:and ,@(cdr qc))) :not nil))
             (:not-join `(,gs ,(next `(:and ,@(cddr qc)))
                              :not ((list ,@(mapqt (get-join-binds qc))))))))
         qc))
     (next/map/or (qc &optional jarg &aux (res (next/map qc)))
       (res/inner res (loop with body = (caar res)
                            for s in (lpos (cdr res))
                            do (setf body `(,(psel :or) ,s ,body ',jarg))
                            finally (return body))))
     (next/map/and (qc &aux (res (next/map qc)))
       (res/inner res (loop with body = (caar res)
                            for (s nil clause join-vars) in (cdr res)
                            do (setf body `(,(psel clause) ,body ,s ,@join-vars))
                            finally (return body))))
     (res/inner (res inner)
       (if (= (length res) 1) (cadar res)
           `(,(psel :let) (,@(lpos res 0 2))
              (declare (list ,@(lpos res)))
              ,inner))))
    (mvb (in where) (qry/preproc in where)
      (let ((res (next where))
            (s "~&--- compiled:~%~s~%~@[    with :in ~s~%~]>>>~%~s~%<<<~%"))
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
  (declare (symbol g) (list select where using in)
           (symbol itr res) (boolean pairs))
  "evaluate a trivial datalog query against g.

:ex
  (qry g :select (?x ?y)
         :where (and (?x :c ?y) (not (or (?x :a 1)
                                         (?x :a 3)))))
will return tuples (?x ?y) matching the query.
other alternatives are (selected vars are available when using these keywords):
 - :pairs T; same as the default, but return the full result pairs
 - :collect [this]; same as default, but collect this instead of just the selected vars
 - :then [this]; execute this code, returns nil
 - :first [this]; execut this, but for the first match only.

other modifiers:
 - :in [vars]; use values of vars bound outside the query.
 - :using [vars]; mutate the graph for every returned tuple, see examples
 - :res [symb]; bind query result to symb inside :collect, :then, :first
 - :itr [symb]; counter available inside :collect, :then, :first
 - :db T; print some useful debug info about the compiled query.

see examples for more usage."

  (when (not (and select where)) (return-from qry nil))
  (when (and pairs using)
        (error "QRY: :pairs can not be combined with :using."))
  (unless (and (every #'var? select) (no-dupes? select))
          (error "QRY: got bad value for :select ~s." select))
  (unless (not (intersection select in))
          (error "QRY: :select ~s and :in ~s can not overlap." select in))
  (unless (at-most 1 pairs then collect first)
          (error "QRY: use either :pairs :then, :first, :collect; or neither."))
  (unless (and (every #'^var? using) (no-dupes? using))
          (error "QRY: got bad value for :using ~s. vars need ^ prefix." using))
  (unless (subsetp select (get-all-vars where))
          (warn "QRY: unexpected var in :select ~s,~%for :where ~s."
                select where))

  (awg (q hit stop* qry-final-res lp)
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
         (fact (,q) `(gather-match ,',g ,@,q))
         ; NOTE: nested queries, requires :pairs
         ; MAYBE: (qq) allow g, for querying different graph?
         (q (&rest rest) `(qry ,',g :pairs t :db ,,db ,@rest)))

        ; NOTE: is it possible to move select vars into qry-compile-where?
        (let ((,qry-final-res nil)
              (,res (,proc (select-vars (qry/compile-where ,where ,in :db ,db)
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
    `(macrolet ((cstop (&body body) `(return-from ,',lp (progn ,@body))))
      (loop named ,lp
            with ,cres of-type list = ,init
            for ,for-res = (progn ,@body)
            for ,citr of-type fixnum from 0 below (the fixnum ,lim)
            until (,test ,for-res)
            if ,for-res do (push ,for-res ,cres)
            finally (return-from ,lp (reverse ,cres))))))

(defmacro qry-collect-while (g &rest rest)
  (declare (symbol g))
  `(collect-while (:init ,(get-kv rest :init '(list))
                   :lim ,(get-kv rest :lim 1000) ; RENAME clim?
                   :cres ,(get-kv rest :cres (gensym "COLLECT-RES"))
                   :citr ,(get-kv rest :citr (gensym "COLLECT-ITR")))
    (qry ,g ,@(strip-kvs rest '(:init :lim :cres :citr)))))

