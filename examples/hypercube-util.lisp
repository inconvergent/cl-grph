
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
(defun invert-path (dim pa &aux (e2 (expt 2 dim)))
  (loop for p in pa collect (- e2 p 1)))
(defun rot-path (dim pa &aux (e2 (expt 2 dim)))
  (loop for p in pa collect (print (- e2 (mod (+ (/ e2 2) p ) e2) 1))))

(defun make-proj (n &optional (rad 250f0))
  (rnd:2nin-circ n rad))
(veq:fvdef make-proj-circ (n &optional (rad 250f0))
  (let ((a (veq:f2$zero n)))
    (veq:f$fxlspace
      (n 0f0 veq:fpii :end nil)
      (lambda (i x) (veq:2$vset (a i) (veq:fcos-sin (+ (rnd:rnd* 0.4) x)))))
    a))

(veq:fvdef proj (proj hh)
  (veq:f2let ((res (veq:f2rep 0f0)))
    (loop for h in hh
          for i from 0
          do (veq:f2vset (res)
               (veq:f2from res (veq:f2$ proj i) (veq:ff h))))
    (values res)))

(veq:fvdef* proj-all (proj hh &optional (x 0f0) (y 0f0))
  (declare (veq:fvec proj))
  (veq:f2$+ (veq:f$_ (mapcar (lambda (h) (veq:lst (proj proj h))) hh)) x y))

(veq:fvdef center (verts max-side)
  (labels ((scale-by (ms w h)
             (declare (veq:ff ms w h))
             (if (> w h) (/ ms w) (/ ms h))))
    (let ((num-verts (veq:f2$num verts)))
    (veq:mvb (minx maxx miny maxy) (veq:f2$mima verts :n num-verts)
      (veq:f2let ((mx (veq:f2scale (veq:f2+ minx miny maxx maxy) 0.5f0))
                  (wh (veq:f2- maxx maxy minx miny)))
        (veq:~ mx wh (if max-side (scale-by max-side wh) 1f0)))))))

(veq:fvdef make-pos (g proj)
  (let ((pos (xgrph:pos))
        (v (veq:f2$zero (grph:@vmax g)))
        (active (proj-all proj
                  (mapcar #'i->h
                    (grph:qry g :select ?x
                                :where (or (?x :path _) (_ :path ?x))
                                :collect ?x)))))

    (veq:mvb ((:va 2 mx wh) s) (center active 600f0)
      (loop for i in (grph:@verts g)
            for h = (i->h i)
            do (xgrph:2vset! pos i
                 (veq:f2scale (veq:f2- (proj proj h) mx) s))))

    pos))

; (require :sb-sprof)
; (defmacro with-prof ((&key (mode :cpu) (mx 50000)) &body body)
;   `(sb-sprof:with-profiling (:max-samples ,mx :mode ,mode :report :graph)
;                             (progn ,@body)))

