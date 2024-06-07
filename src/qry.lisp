(in-package :grph)


(defun qry/check/and/bindable (hit c &aux (cb (get-vars c)))
  (unless (every (lambda (n) (subsetp (get-vars-in-not-clause n) cb)) hit)
          (format nil ":not/:% non-bindable ~a | ~a." hit c)))

(defun qry/check/or/not (qc)
  (cond ((some #'not? qc) (format nil ":not/:not-join is not allowed in :or for:~{~a ~}." qc))
        ((some #'fx? qc) (format nil ":% clause is not allowed in :or/:or-join for:~{~a ~}." qc))
        (t nil)))

(defun qry/check/or (qc &aux (qc (cdr qc)))
  (dsb (a . vars) (mapcar #'get-all-vars qc)
    (loop with a = (sort (copy-list a) #'string<)
          for b in vars
          unless (equal a (sort (copy-list b) #'string<))
          do (return-from qry/check/or
               (format nil "clauses in :or must have the same vars:~{~a ~}." qc)))))

; TODO: this is a very large function that doesn't actually do all that much.
; can we simplify without losing qry validation?
(defun qry/preproc/in/where (p &aux (in (gk p :in t)) (res (list)))
  "converts clause keywords to :keyword, mostly

safeguards :in vars:
  for :in ?a and :w (AND (?A _ ?B) (?D _ ?C))
  returns (values ((!?A ?A)) (AND (!?A _ ?B) (?D _ ?C)))
  if :in is empty, nothing happens.

NOTE: items in :in will be counted as val. ie they are bound as a value. see fx: val?.
      because they will get the ! prefix."
  (with-messages (err wrn)
    (labels                        ;;;;      kv-rec --->
      ((rec/map (qc) (mapcar #'kv-rec qc))
       (fact? (qc) (and (= (length qc) 3)
                        (every (lambda (c) (or (any? c) (val? c) (var? c))) qc)))
       (clause? (qc c) (and (listp qc) (symbolp (car qc)) (eq (kv (car qc)) c)))
       (kv-rec (qc)
         (cond ((clause? qc :%) `(:% ,@(cdr qc)))
               ((clause? qc :q) `(:q ,@(cdr qc)))
               ((clause? qc :f) `(:f ,@(cdr qc)))
               ((clause? qc :uniq) `(:uniq ,@(cdr qc)))
               ((clause? qc :or-join)
                  `(:or-join ,(ensure-list (cadr qc)) ,@(rec/map (cddr qc))))
               ((clause? qc :not-join)
                  `(:not-join ,(ensure-list (cadr qc)) ,@(rec/map (cddr qc))))
               ((and (listp qc) (symbolp (car qc))
                     (member (kv (car qc)) *clauses*))
                  `(,(kv (car qc)) ,@(rec/map (cdr qc))))
               (t (unless (fact? qc) (err "bad qry clause: ~a." qc))
                  `(:fact ,@qc)))) ;;;; <--- kv-rec
       (push-new (s &aux (gs (symb (string-upcase (mkstr "!" s)))))
         (push (list gs s (cadr (assoc s in :test #'eq))) res)
         gs)
       (convert (s &aux (m (car (member s res :key #'second :test #'eq))))
         (if m (car m) (push-new s)))
       (rec (where)
         (cond ((and (var? where)
                     (member where in :test #'eq :key #'car))
                (convert where))
               ((consp where) (cons (rec (car where)) (rec (cdr where))))
               (t where))))
      ; NOTE: (rec ...) must be called before (reverse res)
      (let ((where (rec (kv-rec (gk p :where t)))))
        (qry/compile/check/messages p err wrn)
        `((:where . ,where) (:in . ,(reverse res)) ,@p)))))

(defun qry/compile/where (p &aux (where (gk p :where)) (par (gk p :par t))) ; TODO: rename :f?
  "compile (the where part of a) datalog query."
  (with-messages (err wrn)
    (labels
      ((gs (c) (gensym (mkstr (car c) :-)))
       (join-vars (clause qc &aux (jarg (ensure-list (second qc))))
         (unless (and jarg (every #'var? jarg))
                 (err "bad bind vars ~s in ~a: ~s." jarg clause where))
         jarg)
       (do-bind-search (c not*) ; move outside
         (loop with not-vars = (get-all-vars not*)
               for i from 0 to (length c)
               for vars = (get-all-vars (subseq c i))
               if (null (intersection vars not-vars))
               do (return-from do-bind-search i))
         0)
       (bind-shift (hit c*)
         (loop for n in hit for i = (do-bind-search c* n)
               if hit do (setf c* `(,@(subseq c* 0 i) ,n ,@(subseq c* i))))
         c*)
       (do-proc-bindable (qc &aux (qc (cdr qc)))
         (loop for fx in (list #'not? #'fx?) ; hit contains not/% clauses, c contains the rest
               for (hit c) = (multiple-value-list (veq::filter-by-predicate qc fx))
               do (msg-if wrn (qry/check/and/bindable hit c))
                  (setf qc (bind-shift hit c)))
         qc)

       (uniq (qc) (awg (f-) `(lambda (,f-) ,(rec/get-var f- `(distinct ,@(cdr qc))))))
       (filter (qc) ; TODO: (% ?var fx ...) for bind to ?var
         (awg (f-)
           (etypecase (second qc) ; cons (% (= ?a ...)), symbol: (% = ?a ...)
             (cons `(lambda (,f-) (,(caadr qc) ,@(rec/get-var f- (cdadr qc)))))
             (symbol `(lambda (,f-) (,(cadr qc) ,@(rec/get-var f- (cddr qc))))))))
       (next/map (qc) ; TODO: next next/map is still confusing. try to refactor?
         (mapcar
           (lambda (qc &aux (c (car qc)) (gs (gs qc)))
             (ecase c
               ((:fact :and :f :q :or :or-join) `(,gs ,(next qc) :and nil))
               (:% `(,gs nil :% (,(filter qc))))
               (:uniq `(,gs nil :% (,(uniq qc))))
               (:not `(,gs ,(next `(:and ,@(cdr qc))) :not nil))
               (:not-join `(,gs ,(next `(:and ,@(cddr qc)))
                                :not ((list ,@(mapqt (ensure-list (second qc)))))))))
           qc))
       (res/inner (res inner)
         (if (= (length res) 1) (cadar res)
             `(,(psel par :let) (,@(veq:lpos res 0 2)) (declare (list ,@(veq:lpos res))) ,inner)))
       (next/map/or (qc &optional jarg &aux (res (next/map qc)))
         (res/inner res (loop with body = (caar res)
                              for s in (veq:lpos (cdr res))
                              do (setf body `(,(psel par :or) ,s ,body ',jarg))
                              finally (return body))))
       (next/map/and (qc &aux (res (next/map qc)))
         (res/inner res (loop with body = (caar res)
                              for (s nil clause join-vars) in (cdr res)
                              do (setf body `(,(psel par clause) ,body ,s ,@join-vars))
                              finally (return body))))
       (next (qc &aux (c (car qc)))
         (unless qc (wrn "empty clause in :where: ~a." where))
         (unless (get-all-vars qc) (wrn "no vars in: ~a." qc))
         (ecase c (:fact `(fact ,@(cdr qc)))
                  (:q `(q ,@(cdr qc)))
                  (:f `(progn ,@(cdr qc)))
                  (:and (next/map/and (do-proc-bindable qc)))
                  (:or (msg-if err (qry/check/or/not (cdr qc)))
                       (msg-if err (qry/check/or qc))
                       (next/map/or (cdr qc)))
                  (:or-join (msg-if err (qry/check/or/not (cdr qc)))
                            (next/map/or (cddr qc) (join-vars :or-join qc))))))

      (let ((body (next where)))
        (qry/compile/check/messages p err wrn)
        `((:compiled . ,body) ,@p)))))

(defun qry/aggregate (s) (reverse (veq::tree-find-all s #'aggr?)))
(defun qry/not-aggregate (s)
  (etypecase s (symbol s) (cons (remove-if #'aggr? s))))

(defun qry/compile/conf (p &aux (vars (gk p :vars t)) (using (gk p :using t)))
  (with-messages (err wrn)
    (unless (gk p :where t) (wrn "where is empty."))
    (unless (gk p :select t) (wrn "select is empty."))

    (when (gk& p t :pairs :using) (err ":pairs can not be combined with :using."))
    (unless (apply #'at-most 1 (gkk p :pairs :then :collect :first))
            (err "use either :pairs :then, :first, :collect; or neither."))
    (unless (and (every #'^var? using) (no-dupes? using))
            (err "got bad value for :using ~s. vars need ^ prefix." using))
    (unless (and (every #'var? vars) (no-dupes? vars))
            (err "got bad value for :select ~s" vars))

    (loop for o in (gk p :in t) collect (etypecase o (symbol o) (list (car o))) into in
      finally (unless (and (every #'var? in) (no-dupes? in))
                      (err "duplicate/bad value for :in."))
              (unless (not #1=(intersection vars in))
                      (err ":select and :in can not overlap: ~a" #1#)))

    (unless (subsetp vars (get-all-vars (gk p :where t)))
            (wrn "selecting var(s) not in :where: ~a." vars))

    ; (unless (subsetp (qry/aggregate (gkk p :then :collect :first))
    ;                   (gk p :select) :test #'equal)
    ;         (wrn "inconsistent aggr in :then/:collect/:first and :select"))
    ; TODO: multi aggs: have to check if they have the same arguments,
    (qry/compile/check/messages p err wrn)
    p))

(defun qry/compile/itr (p)
  (declare (list p))
  (awg (hit lp)
    (labels
      ((sym-or-list (s*) (typecase s* (cons `(list ,@s*)) (atom s*)))
       ; i think the select can be made uneccessary? or maybe not when we introduce with?
       (select-pairs (s*) `(list ,@(mapcar (lambda (s*) `(cons ',s* ,s*)) s*)))
       (select-agg-transform (c)
         (let ((agg (car c))) ; c = (grp ?a ?b)
           (ecase (kv agg) (:grp `(agg/grp ?agg ,@(mapqt (cdr c)))) ; is max, min, usefull?
                           (:cnt `(agg/cnt ?agg))
                           (:max `(agg/max ?agg ,@(mapqt (cdr c)))))))
       (replace-agg (s &aux (aggr (gk p :aggr t)))
         (if aggr (veq::tree-replace-fx s (lambda (c) (member c aggr :test #'equal))
                                          (lambda (c) (select-agg-transform c)))
                  s))
       (select-with () (if (gk p :aggr t)
                           (loop for s in (cons '?agg (gk p :xaggr t))
                                 nconc `(for ,s = (get-var ',s ,hit)))
                           (loop for s in (gk p :vars)
                                 nconc `(for ,s = (get-var ',s ,hit)))))
       (do-loop (c) `(loop named ,lp
                           for ,hit of-type list in ,(gk p :res-sym)
                           and ,(gk p :itr-sym) of-type pn from 0
                           ,@(select-with) ,@(replace-agg c)))
       (itr ()
         (cond ((gk p :then t)    (do-loop `(do ,(gk p :then))))
               ((gk p :first t)   (do-loop `(do (return-from ,lp ,(gk p :first)))))
               ((gk p :pairs t)   (do-loop `(collect ,(select-pairs (gk p :vars)))))
               ((gk p :collect t) (do-loop `(collect ,(gk p :collect))))
               (t                 (do-loop `(collect ,(sym-or-list (gk p :select))))))))
      `((:itr . ,(itr)) ,@p))))

(defun do/qry/compile/full (p &aux (g (gk p :g)))
  (awg (q stop* qry-final-res)
    (labels
      ((re-intern (g) (intern (subseq (mkstr g) 1) (symbol-package g)))
       (bind-partial () (loop for g in (gk p :using t)
                              collect `(,g ,(re-intern g))))
       (re-bind-result ()
         `(setf ,@(awf (loop for g in (gk p :using t)
                             collect `(,(re-intern g) ,g))))))
      (let* ((compiled-qry
               (if (gk p :aggr t)
                   `(alists/collapse-keys '?agg ',(gk p :xaggr t) ,(gk p :compiled))
                   `(qry/project ,(gk p :compiled) ',(gk p :vars))))
             (full
               `(macrolet
                  ((cancel (&body body) "cancel, ignore changes."
                     `(return-from ,',stop* (progn ,@body)))
                   (stop (&body body) "stop, keep changes."
                     `(progn ,',(re-bind-result) (return-from ,',stop* (progn ,@body))))
                   (fact (&rest ,q) "return list of matches to q."
                     `(gather-match ,',g ,@,q))
                   (q (&rest q)
                     "nest query. defaults to using parent grph instance.
                alternatively, provide secondary grph instance as the first arg."
                     (etypecase (car q)
                       (keyword `(qry ,',g :pairs t :db ,',(gk p :db t) ,@q))
                       (symbol `(qry ,(car q) :pairs t :db ,',(gk p :db t) ,@(cdr q)))))
                   (itr (&optional (i 0)) "result itr counter."
                     `(+ ,i ,',(gk p :itr-sym)))
                   (res () "all query results (as pairs)."
                     ',(gk p :res-sym)))
                  ; NOTE: is it possible to move select vars into qry-compile-where?
                  (let* (,@(mapcar (lambda (v) `(,(first v) ,(third v))) (gk p :in t))
                         (,(gk p :res-sym) ,compiled-qry)
                         ,@(bind-partial) ,qry-final-res)
                    (block ,stop* (setf ,qry-final-res ,(gk p :itr))
                                  ,(re-bind-result)
                                  ,qry-final-res)))))
        `((:compiled-full . ,full) ,@p)))))

(defun qry/preproc/in (p &aux (in (gk p :in t)))
  (with-messages (err wrn)
    (labels ((err* () (err "unexpected value for in: ~a." in)))
     `((:in . ,(loop for v in in
                collect (typecase v (cons   (unless (var? (car v)) (err*)) v)
                                    (symbol (unless (var? v)       (err*)) (list v v))
                                    (otherwise                     (err*)))
          finally (qry/compile/check/messages p err wrn)))
      ,@p))))

; TODO: with
; TODO: add sort? maybe not?
(defmacro qry (g &key db in using select where collect then first pairs (par *parallel*))
  (declare (symbol g) (list where) (boolean pairs par))
  "evaluate a trivial (datalog-like) query against g.
ex: (qry g :select (?x ?y)
           :where (and (?x :c ?y)
                       (not (or (?x :a 1)
                                (?x :a 3)))))
will return tuples (?x ?y) matching the query; all verts with :a property to
either 1 or 3 other alternatives are (selected vars are available when using
these keywords):
 - :pairs T; same as the default, but return the full result pairs
 - :collect [this]; same as default, but collect this instead of just the selected vars
 - :then [this]; execute this code, returns nil
 - :first [this]; execut this, but for the first match only.

other modifiers:
 - :in [vars]; use values of vars bound outside the query.
 - :using [vars]; mutate the graph for every returned tuple, see examples
 - :db T; print some useful debug info about the compiled query.

see examples for more usage." ; TODO:  pre-check for in/aggr collisions
  (let ((p (veq:vchain (#'do/qry/compile/full #'qry/compile/where
                        #'qry/compile/itr #'qry/preproc/in/where
                        #'qry/preproc/in #'qry/compile/conf)
             `((:in . ,(remove-nil in)) ; pre-bound vars
               (:select . ,select) ; select expression, with aggrs
               (:aggr . ,(qry/aggregate (remove-nil select))) ; aggrs in select
               (:xaggr . ,(qry/not-aggregate (remove-nil select))) ; non-aggr vars
               (:vars . ,(undup (get-all-vars (remove-nil select)))) ; all select vars
               (:using . ,(remove-nil using)) ; all mutating vars
               (:db . ,db) (:g . ,g) (:where . ,where) (:pairs . ,pairs) (:par . ,par)
               (:first . ,first) (:then . ,then) (:collect . ,collect)
               (:res-sym . ,(gensym "RES")) (:itr-sym . ,(gensym "ITR"))))))
    (when db (format t "~a~&" (qry/show p :mode db)) (finish-output))
    (gk p :compiled-full)))

(defun lqry (g &key db select where then collect)
  (declare (grph g) (boolean db))
  "compile and evaluate queries at runtime. ex:
  (let ((g (grph)) (q '(or (?x ?p ?y) (?y ?p ?x))))
    (add! g 1 2)
    (print (lqry g :select '(?x ?p ?y) :where q)))"
  (awg (g*) (eval `(let ((,g* ,g))
                     (declare (grph ,g*))
                     (grph:qry ,g* :db ,db :select ,select :where ,where
                                   :then ,then :collect ,collect)))))

