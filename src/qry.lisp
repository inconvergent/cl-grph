(in-package :grph)


(defun qry/check/proc/not (qc)
  "push not to the right (according to bindable vars), so not is executed
as soon as possible.

NOTE: qc should not include the litteral :and symb."
  (declare (list qc))
  (labels
    ((not-bindables (n)
       (if (eq (car n) :not-join) (get-join-binds n) (get-bindable n)))
     (bind-search (c not*)
       (loop with not-vars = (get-all-vars not*)
             for i from 0 to (length c)
             for vars = (get-all-vars (subseq c i))
             if (null (intersection vars not-vars))
             do (return-from bind-search i))
       0)
     (split-not (l)
       (mvb (not* c) (filter-by-predicate l #'not?)
         (let ((cb (get-bindable c)))
           (when (< (length not*) 1) (return-from qry/check/proc/not qc))
           (unless (every (lambda (n &aux (nb (not-bindables n)))
                            (subsetp nb cb))
                          not*)
                   (warn "QRY: bindable vars in NOT/NOT-JOIN can not be bound for:~%~s." l)))
         (values c not*)))
     (bind-shift (c)
       (mvb (c not*) (split-not c)
         (map nil (lambda (n &aux (i (bind-search c n)))
                    (setf c `(,@(subseq c 0 i) ,n ,@(subseq c i))))
              not*)
         c)))
    (let ((res (bind-shift qc)))
      (when (not? (car res))
            (error "QRY: incorrect NOT/NOT-JOIN push down in:
~s. missing clause?" res))
      res)))

(defun qry/check/or (qc)
  (declare (list qc) (cons qc))
  (when (some #'not? qc)
        (error "QRY: NOT/NOT-JOIN clause is not allowed in OR for:~%~s." qc))
  (dsb (a . vars) (mapcar #'get-all-vars qc)
    (loop for b in vars
          unless (equal a b)
          do (return-from qry/check/or
               (warn "QRY: clauses in OR must have the same vars for:~%~s." qc)))))

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
    (let ((where (rec (qry/intern-clause-symbs where))))
      (values (reverse res) where))))

(defmacro qry/compile-where (gather-match where in &key db)
  (declare (symbol gather-match))
  "compile datalog query. gather-match is the name of the macro used to compile
  the individual clauses."

  (unless (and (every #'var? in) (no-dupes? in))
          (error "QRY: got bad value for :in ~s." in))
  (labels
    ((gs (c) (gensym (apply #'mkstr (interject c :/))))
     (join-vars (clause qc &aux (jarg (get-join-binds qc)))
       (unless (and jarg (every #'var? jarg))
               (error "QRY: bad bind vars ~s in ~a: ~s." jarg clause where))
       jarg)
     (next (qc &aux (c (car qc)))
       (unless qc (error "QRY: empty clause in :where ~s." where))
       (unless (get-all-vars qc) (warn "QRY: clause ~s has no vars." qc))
       (case c (:and (map/and (qry/check/proc/not (cdr qc))))
               (:or (qry/check/or (cdr qc)) (map/or (cdr qc)))
               (:or-join (map/or (cddr qc) (join-vars :or-join qc)))
               (otherwise `(,gather-match ,qc))))
     (gs/next (qc &aux (c (car qc)) (gs (gs qc)))
       ; res has form: (gs next nil/:not/:not-join nil/join-binds)
       ; join-vars is ((list '?a ...)) for use with ,@ in map/and
       (case c (:not-join `(,gs ,(next `(:and ,@(cddr qc)))
                                 :not-join ((list ',@(get-join-binds qc)))))
               (:not `(,gs ,(next `(:and ,@(cdr qc))) :not nil))
               (otherwise `(,gs ,(next qc) nil nil)))) ; and/or/or-join/...
     (res/inner (res inner)
       (if (= (length res) 1) (cadar res)
           `(,(psel :let) (,@(lpos res 0 2))
              (declare (list ,@(lpos res)))
              ,inner)))
     (map/or (qc &optional jarg &aux (res (mapcar #'gs/next qc)))
       (res/inner res (loop with body = (caar res)
                            for s in (lpos (cdr res))
                            do (setf body `(qry-or ,s ,body ',jarg))
                            finally (return body))))
     (map/and (qc &aux (res (mapcar #'gs/next qc)))
       (res/inner res (loop with body = (caar res)
                            for s in (lpos (cdr res))
                            for (not? join-vars) in (lpos (cdr res) 2 4)
                            for fx = (psel (if not? :not :and))
                            do (setf body `(,fx ,body ,s ,@join-vars))
                            finally (return body)))))
    (mvb (in where) (qry/preproc in where)
      (let ((res (next where)))
        (when db (format t "~&-- compiled: ~s~%   with :in ~s~%>>>~%~s~%<<<~%"
                         where in res))
        `(let (,@in) ,res)))))

; TODO: with modifier
; TODO: how to do proc?
; MAYBE rename :first? make :fx alternative?
(defmacro qry (g &key in using select when where then collect first  db
                      (itr (gensym "QRY-ITR")) (proc 'identity)
                      (res (gensym "QRY-RES"))
                 &aux (select (ensure-list select)) (in (ensure-list in))
                      (using (ensure-list using)))
  (declare (symbol g) (list select where using in first) (symbol itr))

  (unless (and (every #'var? select) (no-dupes? select))
          (error "QRY: got bad value for :select ~s." select))
  (when (intersection select in)
        (error "QRY: :select ~s and :in ~s can not overlap." select in))
  (when (> (length (remove-if-not #'identity `(,then ,collect ,first))) 1)
        (error "QRY: use either :then, :first, :collect; or neither."))
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
               (t (do/collect `(collect (list ,@select)))))))
      `(macrolet
         ; cancel and ignore all results
        ((cancel (&body body) `(return-from ,',stop* (progn ,@body)))
         ; stop, but keep the results
         (stop (&body body) `(progn ,',(re-bind-result)
                                    (return-from ,',stop* (progn ,@body))))
         (,qry-gather (,q) `(gather-match ,',g ,@,q)))

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

