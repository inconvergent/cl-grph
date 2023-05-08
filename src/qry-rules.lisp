(in-package :grph)

(defun rules/valid/format (rules)
  (labels
    ((lstp (r) (and (listp r) (not (null r))))
     (ok/length (r) (= (length r) 3))
     (ok/args (r &aux (a (second r))) (and (lstp a) (every #'var? a)))
     (ok/args/subset (r) (subsetp (second r) (tree-find-all (third r) #'var?)))
     (ok/args/consistent (rules)
       (reduce #'consistent (lpos rules 0 2) :initial-value (list)))
     (consistent (prv nxt &aux (hit (find (car nxt) prv :key #'car)))
       (if (and hit (not (equalp hit nxt)))
           (error "RULE: inconsistent args for:~%~s" rules)
           (cons nxt prv))))
    (loop for r of-type list in rules
          do (unless (ok/length r) (error "RULE: bad rule format for: ~s" r))
             (unless (rule-name? (car r)) (error "RULE: bad rule name in: ~s" r))
             (unless (ok/args r) (error "RULE: bad rule args in: ~s" r))
             (unless (ok/args/subset r) (error "RULE: bad/missing vars in: ~s" r))
             (unless (lstp (third r)) (error "RULE: bad rule body in: ~s" r)))
    (unless (lstp rules) (error "RULE: missing rules?"))
    (ok/args/consistent rules)
    rules))

; TODO: non-lin test is not sufficient
(defun rules/split/trivial (rules)
  (filter-by-predicate rules
    (lambda (r) (not (tree-find-all (third r) #'rule-name?)))))

(defun rules/classify/non-trivial (n a r)
  (let* ((rule-names (tree-find-all r #'rule-name?))
         (self-refs (length (tree-find-all rule-names
                              (lambda (s) (eq s n))))))
    (cond ((< 1 self-refs) (error "RULE: non-linear rule:~%~s ~s ~s" n a r))
          ((= 1 self-refs) :linear)
          (t :simple))))


(defun rules/trivial (g trivial)
  (declare (symbol g) (list trivial))
  "create let tuples for rule name, hits."
  (loop for (n a r) of-type (symbol cons cons) in trivial
        collect `(,n (qry ,g :pairs t :select ,a :where ,r))))


; TODO: allow nil as init value for rule?
; TODO: not pairs alternative, default
(defmacro rqry (g &key (lim 1000) rules then)
  (declare (symbol g) (list rules) (pn lim))
  "evaluate simple datalog programs top-down. all rule names (with * prefix)
are bound as variables that can be used in :then.

ex:
  (rqry g :rules ((*st-reach (?x ?y) (?x _ ?y)) ; trivial
                  (*st-reach (?x ?y) (and (?x _ ?z) (*st-reach ?z ?y))) ; linear
                  (*li-reach (?x ?u) (and (*st-reach ?x _) (?x ?u _))) ; simple
                  (*ans-a (?y) (*st-reach 4 ?y)) ; simple (w/filter)
                  (*ans-b (?u) (*li-reach 1 ?u))) ; simple (w/filter)
          :then (print (list *st-reach *li-reach *ans-a *ans-b)))

note the difference between rule types:
 - trivial rules contain only queries that can be passed directly to qry
 - simple rules reference earlier rules, but not themselves
 - linear rules have (only) one self-reference (references to earlier
  rules are allowed.)"
  (mvb (trivial non-trivial) (rules/split/trivial (rules/valid/format rules))
    (let ((all-names (undup (mapcar #'car rules))))
      (labels
        ((rule/qry (a r) `(qry ,g :pairs t :select ,a :where ,(rec/repl/f r)))
         (pairs/qt (a b) (mapcar (lambda (a b) `(quote (,a . ,b))) a b))
         (find-name-args (n) (second (find n rules :key #'car)))
         (repl/f (r) ; what args should we use (find-name-args)?
         `(:f (rules/prev-matches ,(car r)
                ,@(pairs/qt (find-name-args (car r)) (cdr r)))))
         (rec/repl/f (r)
           (cond ((atom r) r)
                 ((and (listp r) (rule-name? (car r))) (repl/f r))
                 ((consp r) (cons (rec/repl/f (car r)) (rec/repl/f (cdr r))))))
         (rule/linear (n a r)
           (awg (prv- nxt- acc-)
             `(loop with ,prv- of-type list = ,n
                    until (not ,prv-) repeat ,lim
                    for ,nxt- of-type list = ,(rule/qry a r)
                    for ,acc- of-type list = (,(psel :or) ,n ,nxt-)
                    if (= (length ,n) (length ,acc-)) do (setf ,prv- nil)
                    else do (setf ,n ,acc- ,prv- ,nxt-)))))
        `(let (,@(rules/trivial g trivial)
               ,@(set-difference all-names (undup (mapcar #'car trivial))))
          (declare (list ,@all-names))
          ,@(loop for (n a r) in non-trivial
                  collect (ecase (rules/classify/non-trivial n a r)
                                 (:simple `(setf ,n ,(rule/qry a r)))
                                 (:linear (rule/linear n a r))))
          (setf ,@(apply #'concatenate 'list
                    (loop for n in all-names
                          collect `(,n (rules/post-proc ,n
                                         ,@(mapqt (find-name-args n)))))))
          ,(cond (then then)
                 (t `(list ,@(loop for n in all-names
                                   collect `(cons ',n ,n))))))))))

