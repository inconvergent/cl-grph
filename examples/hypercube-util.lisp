
(defun h->i (pt)
  (declare (list pt))
  (loop for p in pt for i of-type veq:pn from 0
        summing (* (max 0 (min 1 p)) (expt 2 i))))

(defun i->h (id)
  (declare (veq:pn id))
  (loop for c across (reverse (format nil "~b" id))
        collect (if (eq c #\0) 0 1)))

(defun get-all-hypercube-edges (cube)
  (let ((edges (list))
        (n (length (aref cube 0))))
    (loop for a across cube do
      (loop for b across cube do
        (when (= (1- n)
                 (loop for ai in a and bi in b count (= ai bi)))
              (push (list (h->i a) (h->i b)) edges))))
    edges))

(defun hypercube (n &key (i 0) (res (list (math:nrep n 0))))
  (when (= i n)
        (return-from hypercube
          (values (mapcar #'h->i res)
                  (get-all-hypercube-edges (weird:to-vector res)))))
  (loop for e in res
        do (let ((e* (copy-tree e)))
             (setf (nth i e*) 1)
             (push e* res)))
  (hypercube n :i (1+ i) :res res))

; (defun hpad (dim h) (concatenate 'list h (math:nrep (- dim (length h)) 0)))
(defun invert-path (dim pa) (loop for p in pa collect (- (expt 2 dim) p 1)))

(defun make-proj (n &optional (rad 250f0))
  (rnd:2nin-circ n rad))
(veq:fvdef proj (proj hh)
  (veq:f2let ((res (veq:f2rep 0f0)))
    (loop for h in hh
          for i from 0
          do (veq:f2vset (res)
               (veq:f2from res (veq:f2$ proj i) (veq:ff h))))
    (list res)))

(veq:fvdef* proj-all (proj hh &optional (x 0f0) (y 0f0))
  (declare (veq:fvec proj))
  (veq:f2$+ (veq:f$_ (mapcar (lambda (h) (proj proj h)) hh)) x y))

