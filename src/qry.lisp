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
       (loop for n in hit for i = (bind-search c* n)
             if hit do (setf c* `(,@(subseq c* 0 i) ,n ,@(subseq c* i))))
       c*)
     (bind-shift-all (c)
       (loop for fx in (list #'not? #'fx?)
             for (hit c*) = (split-fx c fx)
             do (setf c (bind-shift hit c*)))
       c))
    (bind-shift-all qc)))

(defun qry/check/or/not (qc)
  (when (some #'not? qc)
        (error "QRY: NOT/NOT-JOIN clause is not allowed in OR for:~%~{~s ~}." qc))
  (when (some #'fx? qc)
        (error "QRY: % clause is not allowed in OR/OR-JOIN for:~%~{~s ~}." qc))
  qc)

(defun qry/check/or (qc &aux (qc (cdr qc)))
  (qry/check/or/not qc) ; THROWS ERRORS
  (dsb (a . vars) (mapcar #'get-all-vars qc)
    (loop for b in vars
          unless (equal (sort (copy-list a) #'string<)
                        (sort (copy-list b) #'string<))
          do (return-from qry/check/or
               (warn "QRY: clauses in OR must have the same vars for:~%~{~s ~}." qc)))))

(defun qry/preproc/where/kv (qc*) ; TODO: rename
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
             ((clause? qc :uniq) `(:uniq ,@(cdr qc)))
             ((clause? qc :or-join)
                `(:or-join ,(ensure-list (cadr qc)) ,@(rec/map (cddr qc))))
             ((clause? qc :not-join)
                `(:not-join ,(ensure-list (cadr qc)) ,@(rec/map (cddr qc))))
             ((and (listp qc) (symbolp (car qc)) (member (kv (car qc)) *valid-clauses*))
                `(,(kv (car qc)) ,@(rec/map (cdr qc))))
             (t (unless (fact? qc) (error "QRY: bad fact: ~s~%for: ~s" qc qc*))
                `(:fact ,@qc)))))
    (rec qc*)))

; TODO: move outside proc. rename proc->validate
(defun qry/preproc/in/where (p &aux (in (gk p :in t)) (where (gk p :where t))
                                 (res (list)))
  "converts clause keywords to :keyword, mostly

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
    ; NOTE: (rec ...) must be called before (reverse res)
    `((:where . ,(rec (qry/preproc/where/kv where))) ; throws??
      (:in . ,(reverse res)) ,@p)))

; TODO: what does :f do, and should it be renamed? see debug for example
(defun qry/compile/where (p &aux (where (gk p :where)) (in (gk p :in t)))
  "compile (the where part of a) datalog query."
  (labels
    ((gs (c) (gensym (mkstr (car c) :-)))
     (join-vars (clause qc &aux (jarg (get-join-binds qc)))
       (unless (and jarg (every #'var? jarg))
               (error "QRY: bad bind vars ~s in ~a: ~s." jarg clause where))
       jarg)
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
             ((:fact :and :f :q) `(,gs ,(next qc) :and nil))
             (( :or :or-join) `(,gs ,(next qc) :and nil))
             (:% `(,gs nil :% (,(filter qc))))
             (:uniq `(,gs nil :% (,(uniq qc))))
             (:not `(,gs ,(next `(:and ,@(cdr qc))) :not nil))
             (:not-join `(,gs ,(next `(:and ,@(cddr qc)))
                              :not ((list ,@(mapqt (get-join-binds qc))))))))
         qc))
     (res/inner (res inner)
       (if (= (length res) 1) (cadar res)
           `(,(psel :let) (,@(lpos res 0 2))
              (declare (list ,@(lpos res)))
              ,inner)))
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
     (next (qc &aux (c (car qc)))
       (unless qc (warn "QRY: empty clause in :where ~s." where))
       (unless (get-all-vars qc) (warn "QRY: no vars in ~s" qc))
       (ecase c (:fact `(fact ,@(cdr qc)))
                (:q `(q ,@(cdr qc)))
                (:f `(progn ,@(cdr qc)))
                (:and (next/map/and (qry/check/proc/and qc))) ; WARN
                (:or (qry/check/or qc) ; INDIRECTLY THROWS ERRORS
                     (next/map/or (cdr qc)))
                (:or-join (qry/check/or/not (cdr qc)) ; THROWS ERRORS
                          (next/map/or (cddr qc) (join-vars :or-join qc))))))

   `((:compiled . (let (,@(gk p :in t)) ,(next where)))
     ,@p)))

(labels ((is-aggr (e) (and (listp e) (symbolp (car e))
                           (member (kv (car e)) *aggregate*))))
  (defun qry/aggregate (s) (reverse (tree-find-all s #'is-aggr)))  ; select is not always a list
  (defun qry/not-aggregate (s) (etypecase s
                                 (symbol s) (cons (remove-if #'is-aggr s)))))

(defun qry/compile/conf (p)
  (let ((errors 0) (warnings 0) (s (make-string-output-stream)))
    (labels ((wrt (v) (format s "~&██ ~a~&" v))
             (err (s &rest rest) (incf errors) (wrt (apply #'format nil s rest)))
             (wrn (s &rest rest) (incf warnings) (wrt (apply #'format nil s rest))))

      (unless (gk p :where t) (wrn "WRN: where is empty"))
      (unless (gk p :select t) (wrn "WRN: select is empty"))

      (when (gk& p t :pairs :using)
            (err "ERR: :pairs can not be combined with :using."))
      (unless (apply #'at-most 1 (gkk p :pairs :then :collect :first))
              (err "ERR: use either :pairs :then, :first, :collect; or neither."))
      (unless (and (every #'^var? #1=(gk p :using t)) (no-dupes? #1#))
              (err "ERR: got bad value for :using ~s. vars need ^ prefix." #1#))

      (unless (and (every #'var? (gk p :vars)) (no-dupes? (gk p :vars)))
                (err "ERR: got bad value for :select ~s" (gk p :vars)))

      (unless (and (every #'var? #3=(gk p :in t)) (no-dupes? #3#))
              (err "ERR: duplicate/bad value for :in: ~s." #3#))
      (unless (not (apply #'intersection (gkk p :vars :in)))
              (err "ERR: :select ~s and :in ~s can not overlap."
                   (gk p :vars) #3#))
      (unless (subsetp #2=(gk p :vars t) (get-all-vars (gk p :where t)))
              (wrn "WRN: selecting var(s) not in :where: ~s" #2#))
      (unless (subsetp (qry/aggregate (gkk p :then :collect :first))
                        (gk p :select) :test #'equal)
              (wrn "WRN: inconsistent aggr in :then/:collect/:first and :select"))

      ; TODO: if we allow multiple aggs we have to check if they have the
      ; same arguments, or we have to deduplicate inside each agg fx
      ; (unless (< (length (gk p :aggr t)) 2)
      ;         (err "ERR: too many aggregators: ~a" (gk p :aggr)))

      (let ((p (qry/preproc/in/where p)))
        (wrt (qry/show p))
        (cond ((and (> warnings 0) (< errors 1)) ; only warnings
               (warn "██ COMPILE WARN~%~a" (get-output-stream-string s)))
              ((> errors 0) ; errors or warnings
               (error "██ COMPILE ERROR~%~a" (get-output-stream-string s))))
        p))))

(defun qry/compile/itr (p)
  (declare (list p))
  (awg (hit lp)
    (labels
      ((sym-or-list (s*) (typecase s* (cons `(list ,@s*)) (atom s*)))
       ; i think the select can be made uneccessary? or maybe not when we introduce with?
       (select-pairs (s*) `(list ,@(mapcar (lambda (s*) `(cons ',s* ,s*)) s*)))
       (select-agg-transform (c)
         (dsb (agg . args) c ; (grp . ?a ?b)
           (ecase (kv agg) (:grp `(agg/grp ?agg ,@(mapqt (cdr c))))
                           (:cnt `(agg/cnt ?agg))
                           ; is this useful? what does it mean?
                           ; (:max `(agg/max ?agg ,@(mapqt (cdr c))))
                           )))
       (replace-agg (s &aux (aggr (gk p :aggr t)))
         (if aggr (tree-replace-fx s (lambda (c) (member c aggr :test #'equal))
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
      (let* ((compiled-qry (if (gk p :aggr t)
                               `(alists/collapse-keys '?agg
                                 ',(gk p :xaggr t) ,(gk p :compiled))
                               `(qry/project
                                 ,(gk p :compiled) ',(gk p :vars))))
             (full `(macrolet
                      ((cancel (&body body) "cancel, ignore changes"
                         `(return-from ,',stop* (progn ,@body)))
                       (stop (&body body) "stop, keep changes"
                         `(progn ,',(re-bind-result)
                                 (return-from ,',stop* (progn ,@body))))
                       (fact (&rest ,q) `(gather-match ,',g ,@,q))
                       ; NOTE: (qq) allow g, for querying different graph?
                       (q (&rest rest) `(qry ,',g :pairs t :db ,',(gk p :db t) ,@rest))
                       (itr (&optional (i 0)) "result itr counter" `(+ ,i ,',(gk p :itr-sym)))
                       (res () "all query results (as pairs)" ',(gk p :res-sym)))
                      ; NOTE: is it possible to move select vars into qry-compile-where?
                      (let ((,(gk p :res-sym) ,compiled-qry)
                            ,@(bind-partial) ,qry-final-res)
                        (block ,stop* (setf ,qry-final-res ,(gk p :itr))
                                      ,(re-bind-result)
                                      ,qry-final-res)))))
        `((:compiled-full . ,full) ,@p)))))

; TODO: with
; TODO: add sort? maybe not?
; TODO: finish multi aggs
(defmacro qry (g &key db in using select where collect then first pairs)
  (declare (symbol g) (list where) (boolean pairs))
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

see examples for more usage."

  ; TODO:  pre-check for in/aggr collisions
  (let ((p (veq:vchain (#'do/qry/compile/full #'qry/compile/where
                        #'qry/compile/itr #'qry/compile/conf)
             `((:in . ,(remove-nil in)) ; pre-bound vars
               (:select . ,select) ; select expression, with aggrs
               (:aggr . ,(qry/aggregate (remove-nil select))) ; aggrs in select
               (:xaggr . ,(qry/not-aggregate (remove-nil select))) ; non-aggr vars
               (:vars . ,(undup (get-all-vars (remove-nil select)))) ; all select vars
               (:using . ,(remove-nil using)) ; all mutating vars
               (:db . ,db) (:g . ,g) (:where . ,where)
               (:pairs . ,pairs) (:first . ,first)
               (:then . ,then) (:collect . ,collect)
               (:res-sym . ,(gensym "RES")) (:itr-sym . ,(gensym "ITR"))))))
    (when db (format t (qry/show p
                         :compiled-key (ccase db (:full :compiled-full)
                                                 (t :compiled))))
             (finish-output))
    (gk p :compiled-full)))

(defun lqry (g &key db select where then collect)
  (declare (grph g) (boolean db))
  "compile and evaluate queries at runtime.
ex:
  (let ((g (grph))
        (q '(or (?x ?p ?y) (?y ?p ?x))))
    (add! g 1 2)
    (print (lqry g :select '(?x ?p ?y) :where q)))"
  (awg (g*) (eval `(let ((,g* ,g))
                     (declare (grph ,g*))
                     (grph:qry ,g* :db ,db :select ,select :where ,where
                                   :then ,then :collect ,collect)))))

