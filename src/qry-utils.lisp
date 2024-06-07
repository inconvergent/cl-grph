(in-package :grph)

(declaim (inline any? eq-car? eq-car? not? var? ^var? fx? symb?
                 has-prefix? get-var get-all-vars symlen no-dupes?))

(defmacro msg-if (msg a &aux (a* (gensym "A")))
  `(let ((,a* ,a)) (when ,a* (,msg ,a*))))
(defmacro with-messages (msgs &body body)
  `(let (,@(loop for m in msgs collect `(,m (list))))
     (labels (,@(loop for m in msgs
                      collect `(,m (s &rest rest)
                                 (push (mkstr ,(kv m) ": "
                                         (apply #'format nil s rest))
                                       ,m))))
      (progn ,@body))))

(defun psel (par k) (declare (boolean par) (keyword k))
  (if par (ecase k ((:and :fact :f :q) 'qry-and) (:or 'qry-or)
                    (:not 'qry-not) (:% 'p/qry-filter) (:let 'lparallel:plet))
          (ecase k ((:and :fact :f :q) 'qry-and) (:or 'qry-or)
                    (:not 'qry-not) (:% 'qry-filter)  (:let 'let))))

(defun eq-car? (a s) (declare #.*opt* (list a) (symbol s))
  (eq (car a) s))
(defun gk (p k &optional silent &aux (hit (cdr (assoc k p)))) (declare #.*opt* (list p) (keyword k))
  (if (or silent hit) hit (warn "QRY: missing conf key: ~a~%conf: ~s" k p)))
(defun gkk (p &rest rest) (declare (list p))
  (mapcar (lambda (k) (gk p (the keyword k) t)) rest))
(defun gk& (p &optional silent &rest keys) (declare #.*opt* (list p keys))
  (every (lambda (k) (gk p k silent)) keys))

(defun symb? (s) (and (symbolp s) (not (keywordp s))))
(defun symlen (s) (declare #.*opt* (symbol s)) (length (symbol-name s)))
(defun has-prefix? (s c &aux (name (symbol-name s))) (eq (char name 0) c))

(defun ^var? (s) (declare #.*opt*)
  (and (symb? s) (> (symlen s) 1) (has-prefix? s #\^)))

(defun val? (s) (declare #.*opt*)                                      ; :a 12 !?var
  (or (keywordp s) (numberp s) (and (symb? s) (has-prefix? s #\!))))
(defun any? (s) (declare #.*opt*) (and (symbolp s) (eq (kv s) :_)))    ;  _
(defun var? (s) (declare #.*opt*) (and (symb? s) (has-prefix? s #\?))) ; ?x

(defun not? (p) (declare #.*opt* (list p)) (or (eq-car? p :not) (eq-car? p :not-join)))
(defun fx? (p) (declare #.*opt* (list p)) (eq-car? p :%))

(defun no-dupes? (l) (declare #.*opt* (list l))
  (= (length (the list (undup l nil))) (length l))) ; undup use is ok

(defun aggr? (e) (and (listp e) (symb? (car e))
                      (member (kv (car e)) *aggregates*)))

(defun rule-name? (n) (and (symbolp n) (has-prefix? n #\*) (> (symlen n) 1)))

(defun get-var (k l) (declare #.*opt* (symbol k) (list l))
  (cdr (find k l :key #'car :test #'eq)))
(defun rec/get-var (f l) (declare (symbol f))
  (cond ((var? l) `(get-var ',l ,f))
        ((atom l) l)
        ((consp l) (cons (rec/get-var f (car l))
                         (rec/get-var f (cdr l))))
        ; this is unreachable i think, but leave it in
        (t (error "REC/GET-VAR: unexpected value in rec/repl/var: ~s" l))))

(defun get-all-vars (a) (declare #.*opt* (list a))
  (reverse (veq::tree-find-all a #'var?)))

(defun get-vars (a) (declare #.*opt* (list a))
  (reverse (veq::tree-find-all a #'var?)))
(defun get-vars-in-not-clause (n &aux (s (car n)))
  (ecase s (:not-join (ensure-list (second n)))
           (:not (get-vars n))
           (:% (get-vars n))))

; DEBUG / SHOW ----------------

(defun qry/show (p &key (s (make-string-output-stream)) (mode :default))
  (let ((*print-gensym* nil) (*print-case* :downcase))
    (labels ((full (p) (gk p :compiled-full t))
             (main (p) (third (gk p :compiled-full t)))
             (default (p) (gk p :compiled t)))
      (apply #'format s "
██ COMPILED QRY (~a) ██████████████████████████
██ select:  ~a
██ where:   ~a
██ PROPS~%" (v?) (gkk p :select :where))
      (loop with ignores = '(:select :where :res-sym :itr-sym
                             :compiled-full :compiled)
            for (k . v) in p for k* = (string-downcase (mkstr k ":"))
            if (and v (not (member k ignores)))
            do (format s "~&██ ~8,,,' @<~d~> ~a~%" k* v))
      (format s "~&██~%~a~%███████████~%"
                (funcall (ecase mode (:full #'full) (t #'default) (:main #'main)) p))
      (get-output-stream-string s))))

(defun qry/compile/write-messages (&rest l &aux (s (make-string-output-stream)))
  (loop for o in l do (format s "~&██ ~a~&" o))
  (get-output-stream-string s))

(veq:fvdef qry/compile/check/messages (p err wrn)
  (cond ((and wrn (not err)) ; only warnings
         (warn "~&~a~&" (a@qry/compile/write-messages "COMPILE WARN"
                          `(,@wrn ,(qry/show p :mode t)))))
        (err (error "~&~a~&" (a@qry/compile/write-messages "COMPILE ERROR"
                             `(,@err ,@wrn ,(qry/show p :mode t)))))))

