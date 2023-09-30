(in-package #:grph-tests)

(plan 1)

(setf *random-state* (sb-ext:seed-random-state 10))

(defmacro s (&rest rest) `(fset:set ,@rest))
(defmacro equal-and- (l) `(let ((l ,l)) (if (apply #'= l) (car l) l)))

(defun mock-logic-set-element (kk &optional (mv 10) &aux (row (fset:empty-map)))
  (fset:do-set (k kk) (setf row (fset:with row k (random mv))))
  row)

(defun make-mock-logic-set-with-keys (kk n &optional (mv 10)
                                           &aux (res (fset:empty-set)))
  (loop until (= (fset:size res) n) repeat n
        do (setf res (fset:with res (mock-logic-set-element kk mv))))
  res)

(defun logic-set->alists (ss &aux (res (list)))
  (fset:do-set (m ss)
    (let ((row (list)))
      (fset:do-map (k v m) (push `(,k . ,v) row))
      (push (grph::psrt row) res)))
  res)

(defun alist->map (l &aux (res (fset:empty-map)))
  (loop for (k . v) in l do (setf res (fset:with res k v)))
  res)

(defun alists->logic-set (ll &aux (res (fset:empty-set)))
  (loop for pair in ll
        do (setf res (fset:with res (alist->map pair))))
  res)

(defun logic-map-row (m &aux (res (fset:empty-set)))
  (fset:do-map (k v m) (setf res (fset:with res `(,k . ,v)))) res)
(defun logic-set->rows (aa) (fset:image (lambda (s) (logic-map-row s)) aa))

(veq:vdef key-config (ka kb &key (n 1000) (max-ind 200))
  (let* ((aa (make-mock-logic-set-with-keys ka n max-ind))
         (bb (make-mock-logic-set-with-keys kb n max-ind))
         (laa (logic-set->alists aa)) (lbb (logic-set->alists bb))
         (resa (grph::set/qry-and* aa bb)) (resb (grph::qry-and laa lbb))
         (resb* (alists->logic-set resb)))
    (list (fset:size resa)
          (length resb) (fset:size resb*)
          (fset:size (fset:union resa resb*)))))

(subtest "qry-runtime"
  (is (equal-and- (key-config (s :a) (s :a))) 197)
  (is (equal-and- (key-config (s :a) (s :b))) 0)
  (is (equal-and- (key-config (s :a :b) (s :a :b))) 26)
  (is (equal-and- (key-config (s :a :b) (s :x :y))) 0)
  (is (equal-and- (key-config (s :a) (s :a :b) :max-ind 50)) 829)
  (is (equal-and- (key-config (s :a :c) (s :a :b) :max-ind 50)) 13283)
  (is (equal-and- (key-config (s :a :b) (s :a :b :c) :max-ind 50)) 315)
  (is (equal-and- (key-config (s :x :a :b) (s :a :b :c) :max-ind 50)) 409)
  )

(unless (finalize) (error "error in QRY RUNTIME"))
