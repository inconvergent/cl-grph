(in-package :grph)

; COMPILER QRY UTILS ---

(declaim (inline any?  bad-or-clause? bindable?  eq-car? eq-car? free?
                 get-all-vars has-symchar? interned? symb-sort-fx symlen val?
                 var^ varset-sort no-dupes?))

(defun has-symchar? (s c &optional (pos :first))
  (declare (ignorable pos))
  (let ((name (symbol-name s)))
    (declare (string name))
    (eq (char name (case pos (:first 0) (otherwise (1- (length name)))))
        c)))

(defun interned? (s)
  (declare (optimize speed))
  (typecase s (symbol (symbol-package s))))

(defun var? (s)
  (declare (optimize speed))
  (and (interned? s) (has-symchar? s #\?)))

(defun var^ (v &aux (v (mkstr v)))
  (declare (optimize speed))
  (and (> (length v) 1) (equalp (char v 0) #\^)))

(defun symlen (s)
  (declare (optimize speed) (symbol s))
  (length (symbol-name s)))

(defun any? (s)
  (declare (optimize speed))
  (and (interned? s) (= (symlen s) 1) (has-symchar? s #\_)))

(defun eq-car? (a s)
  (declare (optimize speed) (list a) (symbol s))
  (eq (car a) s))

(defun val? (s)
  (declare (optimize speed))
  (and (symbolp s) (not (symbol-package s))))

(defun free? (s)
  (declare (optimize speed))
  (cond ((or (var? s) (any? s)) t)
        ((or (val? s) (numberp s) (keywordp s)) nil)
        (t (error "FREE?: unexpected clause: ~a." s))))

(defun bindable? (s)
  (declare (optimize speed))
  (cond ((any? s) nil) (t (free? s))))

(defun symb-sort-fx (a b)
  (declare (optimize speed) (symbol a b))
  (string-lessp (symbol-name a) (symbol-name b)))

(defun varset-sort (a &optional (fx #'identity))
  (declare (optimize speed ) (list a) (function fx))
  ; copy-list because of potential side-effect on a because of sort.
  (sort (copy-list a) #'symb-sort-fx :key (the function fx)))

(defun get-all-vars (a)
  (declare (optimize speed) (list a))
  (varset-sort (remove-if-not #'var? (undup a))))

(defun bad-or-clause? (qc)
  (declare (optimize speed) (cons qc))
  (dsb (a . vars) (mapcar #'get-all-vars qc)
    (loop for b in vars unless (equal a b)
          do (return-from bad-or-clause? t))))

(defun no-dupes? (l)
  (declare (optimize speed) (list l))
  (= (length (undup l nil)) (length l)))

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
    ; NOTE rec must be called before we return (reverse res)
    (let ((w* (rec where))) (values (reverse res) w*))))

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


; RUNTIME QRY UTILS ---

(declaim (inline conflicting-intersection
                 match-var-and match-var-not match-var-or
                 some-subsets distinct))

(defun conflicting-intersection (aa bb)
  (declare (optimize speed (safety 1)) (list aa bb))
  (loop for (ka . va) in aa
        do (loop for (kb . vb) in bb
                 if (and (eq ka kb) (not (eq va vb)))
                 do (return-from conflicting-intersection t))))
; TODO: in principle this might be faster with some more work
; (defun same-key (l r)
;   (declare (optimize speed (safety 0)) (list l r))
;   (eq (caar l) (caar r))
; (defun same-val (l r)
;   (declare (optimize speed (safety 0)) (list l r))
;   (eq (cdar l) (cdar r)))
  ; (let ((l aa)
  ;       (r bb))
  ;   (declare (list l r))
  ;   (loop while (and l r)
  ;        do (cond ((same-key l r)
  ;                (when (not (same-val l r))
  ;                  (return-from conflicting-intersection t))
  ;                    (setf l (cdr l) r (cdr r)))
  ;              ((string-lessp (symbol-name (caar l)) (symbol-name (caar r)))
  ;                (setf l (cdr l)))
  ;              (t (setf r (cdr r))))))

(defun match-var-and (aa bb &aux (res (list)))
  (declare (optimize speed (safety 1)) (list aa bb))
  (loop for a in aa
        do (loop for b in bb
                 if (not (conflicting-intersection a b))
                 do (push (varset-sort (union a b :test #'equal) #'car) res)))
  (remove-duplicates res :test #'equal))

(defun match-var-or (aa bb)
  (declare (optimize speed) (list aa bb))
  (union aa bb :test #'equal))

(defun some-subsets (a b)
  (declare (optimize speed) (list a b))
  (some (lambda (o) (declare (list o)) (subsetp o a :test #'equal)) b))

(defun match-var-not (orig nxt)
  (declare (optimize speed) (list orig nxt))
  (remove-if (lambda (a) (declare (list a)) (some-subsets a nxt)) orig))

; RUNTIME QRY FILTERS

(defun distinct (&rest rest &aux (n (length rest)))
  (= n (length (undup rest nil))))

(defmacro fx-first (fx &rest rest &aux (a (car rest)))
  (declare (symbol a))
  `(and ,@(loop for b of-type symbol in (cdr rest) collect `(,fx ,a ,b))))
(defmacro first< (&rest rest) `(fx-first < ,@rest))
(defmacro first> (&rest rest) `(fx-first > ,@rest))

