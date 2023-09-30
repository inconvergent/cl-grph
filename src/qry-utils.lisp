(in-package :grph)

(declaim (inline any? bindable? eq-car? eq-car? free? not? var? ^var? val? fx?
                 has-symchar? interned? get-var get-all-vars symlen no-dupes?))

; TODO: move some things to config?
(defvar *aggregate* (list :min :max :cnt :grp))

(defun gk (p k &optional silent &aux (hit (cdr (assoc k p))))
  (declare #.*opt* (list p) (keyword k))
  (if (or silent hit) hit (warn "QRY: missing conf key: ~a~%conf: ~s" k p)))
(defun gkk (p &rest rest)
  (declare (list p))
  (mapcar (lambda (k) (gk p (the keyword k) t)) rest))
(defun gk& (p &optional silent &rest keys)
  (declare #.*opt* (list p keys))
  (every (lambda (k) (gk p k silent)) keys))

(defun symlen (s)
  (declare (optimize speed) (symbol s))
  (length (symbol-name s)))

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

(defun ^var? (s)
  (declare (optimize speed) (symbol s))
  (and (> (symlen s) 1) (has-symchar? s #\^)))

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

(defun fx? (p)
  (declare (optimize speed) (list p))
  (eq-car? p :%))

(defun free? (s)
  (declare (optimize speed))
  (cond ((or (var? s) (any? s)) t)
        ((val? s) nil)
        (t t)))

(defun bindable? (s)
  (declare (optimize speed))
  (cond ((any? s) nil) (t (free? s))))

(defun no-dupes? (l)
  (declare (optimize speed) (list l))
  (= (length (the list (undup l nil))) (length l))) ; undup use is ok

(defun rule-name? (n)
  (and (symbolp n) (has-symchar? n #\*) (> (symlen n) 1)))

(defun get-var (k l)
  (declare (optimize speed) (symbol k) (list l))
  (cdr (find k l :key #'car :test #'eq)))

(defun rec/get-var (f l) ; TODO: rename this mfer
  (declare (symbol f))
  (cond ((var? l) `(get-var ',l ,f))
        ((atom l) l)
        ((consp l) (cons (rec/get-var f (car l))
                         (rec/get-var f (cdr l))))
        (t (error "REC/GET-VAR: unexpected value in rec/repl/var: ~s" l))))

(defun get-all-vars (a)
  (declare (optimize speed) (list a))
  (reverse (tree-find-all a #'var?)))

(defun get-bindable (a)
  (declare (optimize speed) (list a))
  (reverse (tree-find-all a (lambda (c) (and (var? c) (bindable? c))))))

(defun get-join-binds (qc)
  (declare (list qc))
  (let ((res (ensure-list (cadr qc))))
    (unless (every #'var? res) (error "QRY: bad bind var in: ~a" qc))
    res))

(defun qry/show (p &key (s (make-string-output-stream))
                        (compiled-key :compiled))
  (apply #'format s "
██ COMPILED ██████████████████████████
██ select:  ~a
██ where:   ~a
██ PROPS    ██████████████████████████~%" (gkk p :select :where))
  (loop with ignores = '(:select :where :res-sym :itr-sym
                         :compiled-full :compiled)
        for (k . v) in p for k* = (string-downcase (mkstr k ":"))
        if (and v (not (member k ignores)))
        do (format s "~&██ ~8,,,' @<~d~> ~a~%" k* v))
  (format s "~&██ OUTPUT   >>>>>~%██ ~a~%███████████ <<<<<~%"
            (gk p compiled-key t))
  (get-output-stream-string s))

