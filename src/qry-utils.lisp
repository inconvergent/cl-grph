(in-package :grph)

(declaim (inline any? bindable? eq-car? eq-car? free? not? var? ^var? val?
                 has-symchar? interned? get-var get-all-vars symlen no-dupes?))

; NOTE: this can be simplified after using preprocssing

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

(defun ^var? (s &aux (name (symbol-name s)))
  (declare (optimize speed) (symbol s) (string name))
  (and (> (length name) 1) (eq (char name 0) #\^)))

(defun symlen (s) (declare (optimize speed) (symbol s)) (length (symbol-name s)))

(defun val? (s) (declare (optimize speed))
  (or (keywordp s)
      (and (symbolp s) (not (interned? s)))
      (numberp s)))

(defun any? (s)
  (declare (optimize speed))
  (and (interned? s) (= (symlen s) 1) (has-symchar? s #\_)))

(defun eq-car? (a s)
  (declare (optimize speed) (list a) (symbol s))
  (eq (car a) s))

(defun not? (p)
  (declare (optimize speed) (list p))
  (or (eq-car? p :not) (eq-car? p :not-join)))

(defun free? (s)
  (declare (optimize speed))
  (cond ((or (var? s) (any? s)) t)
        ((val? s) nil)
        (t t)))

(defun bindable? (s)
  (declare (optimize speed))
  (cond ((any? s) nil) (t (free? s))))

(defun get-var (s l)
  (declare (optimize speed) (symbol s) (list l))
  (cdr (find s l :key #'car :test #'eq)))

(defun get-all-vars (a)
  (declare (optimize speed) (list a))
  (remove-if-not #'var? (undup a))) ; undup use is ok

(defun get-bindable (a)
  (declare (optimize speed) (list a))
  (remove-if-not #'bindable? (get-all-vars a)))

(defun no-dupes? (l)
  (declare (optimize speed) (list l))
  (= (length (the list (undup l nil))) (length l))) ; undup use is ok

(defun get-join-binds (qc)
  (declare (list qc))
  (ensure-list (cadr qc)))

(defun qry/intern-clause-symbs (qc)
  (labels ((kv (s) (intern (string-upcase (symbol-name s)) :keyword)))
    (cond ((and (symbolp qc) (member (kv qc) *valid-clauses*)) (kv qc))
          ; ((any? s) :_) ; why does this not work?
          ((atom qc) qc)
          ((consp qc) (cons (qry/intern-clause-symbs (car qc))
                            (qry/intern-clause-symbs (cdr qc)))))))

