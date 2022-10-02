(in-package :grph)

; COMPILER QRY UTILS ---

(declaim (inline any? bindable? eq-car? eq-car? free?
                 get-all-vars has-symchar? interned? symlen val?
                 var^ varset-sort no-dupes? valid-qry-clause?))

(defun interned? (s)
  (declare (optimize speed))
  (typecase s (symbol (symbol-package s))))

(defun has-symchar? (s c &optional (pos :first))
  (declare (keyword pos))
  (let ((name (symbol-name s)))
    (declare (string name))
    (eq (char name (case pos (:first 0)
                             (otherwise (1- (length name)))))
        c)))

(defun var? (s)
  (declare (optimize speed))
  (and (interned? s) (has-symchar? s #\?)))

(defun var^ (s &aux (name (symbol-name s)))
  (declare (optimize speed) (symbol s) (string name))
  (and (> (length name) 1) (eq (char name 0) #\^)))

(defun symlen (s) (declare (optimize speed) (symbol s)) (length (symbol-name s)))

(defun val? (s) (declare (optimize speed)) (and (symbolp s) (not (interned? s))))
(defun any? (s)
  (declare (optimize speed))
  (and (interned? s) (= (symlen s) 1) (has-symchar? s #\_)))

(defun eq-car? (a s)
  (declare (optimize speed) (list a) (symbol s))
  (eq (car a) s))

(defun free? (s)
  (declare (optimize speed))
  (cond ((or (var? s) (any? s)) t)
        ((or (val? s) (numberp s) (keywordp s)) nil)
        (t t)))

(defun bindable? (s)
  (declare (optimize speed))
  (cond ((any? s) nil) (t (free? s))))

(defun varset-sort (a &optional (fx #'identity))
  (declare (optimize speed (safety 1)) (list a) (function fx))
  ; copy-list because of side-effect on a because of sort.
  ; even when doing eg. (varset-sort (union a b))
  (sort (copy-list a) #'string< :key (the function fx)))

(defun get-all-vars (a)
  (declare (optimize speed) (list a))
  (varset-sort (remove-if-not #'var? (undup a))))

(defun get-all-bindable (a)
  (declare (optimize speed) (list a))
  (varset-sort (remove-if-not #'bindable? (get-all-vars a))))

(defun no-dupes? (l)
  (declare (optimize speed) (list l))
  (= (length (the list (undup l nil))) (length l)))

(defun valid-qry-clause? (s)
  (declare (optimize speed))
  (member s '(not and or or-join)))


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


(defun sort-not-clauses (qc)
  (declare (list qc))
  ; qc should not include the litteral 'and symb
  (labels
    ((notp (p) (declare (list p)) (eq-car? p 'not))
     (var-search (c not*)
       (declare (list c not*))
       (loop with not-vars = (get-all-vars not*)
             for i from (length c) downto 0
             for vars = (get-all-vars (subseq c 0 i))
             if (null (intersection vars not-vars))
             do (return-from var-search i))
       0)
     (var-shift (l)
       (declare (list l))
       (mvb (not* c) (filter-by-predicate l #'notp)
         (cond ((null not*) (return-from sort-not-clauses qc))
               ((> (length not*) 1)
                  (error "QRY: multiple NOT in AND clause for: ~a.
use (NOT c1 c2)." qc)))

         ; TODO: should we use get-all-bindable instead of
         ; get-all-vars in ; other checks?
         (let ((sub (get-all-bindable not*)))
           (unless (subsetp sub (get-all-bindable c))
             (warn "QRY: bindable vars ~a in NOT can not be bound for:~%~a."
                   sub qc)))

         (let ((i (var-search c not*)))
           (declare (fixnum i))
           (concatenate 'list (subseq c 0 i) not* (subseq c i))))))
    (var-shift qc)))

(defun check-or-clause (qc)
  (declare (list qc) (cons qc))
  (when (some (lambda (c) (eq 'not (car c))) qc)
        (error "QRY: NOT clause is not allowed in OR for:~%~a." qc))
  (dsb (a . vars) (mapcar #'get-all-vars qc)
    (loop for b in vars
          unless (equal a b)
          do (return-from check-or-clause
               (warn "QRY: clauses in OR must have the same vars for:~%~a." qc)))))

(defun shadow-in-vars (in where &aux (res (list)))
  "safeguard :in vars.
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
    (let ((w* (rec where))) (values (reverse res) w*))))

