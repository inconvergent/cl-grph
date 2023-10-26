(in-package :xgrph)

(deftype pos () 'fset:seq)

(defun pos (&optional (d 0f0))
  (declare (veq:ff d))
  "initialze xgrph pos. d is the default fset:seq value."
  (the pos (empty-seq d)))

(defmacro @vert (dim s i)
  (declare (pn dim) (symbol s))
  "get vert i as values."
  (awg (k)
  `(let ((,k (* ,dim ,i)))
    (declare (pn ,k))
    (values ,@(loop for i from 0 below dim
                    collect `(fset:@ ,s (+ ,k ,i)))))))
(defmacro 2@vert (&rest rest) `(@vert 2 ,@rest))
(defmacro 3@vert (&rest rest) `(@vert 3 ,@rest))

(defmacro @num (dim s) (declare (pn dim)) `(the pn (/ (fset:size ,s) ,dim)))
(defmacro 2@num (&rest rest) `(@num 2 ,@rest))
(defmacro 3@num (&rest rest) `(@num 3 ,@rest))

(defmacro @verts (dim s l)
  (declare (pn dim) (symbol s))
  "get verts in l as fvec."
  (awg (a i c l*)
  `(let* ((,l* ,l) (,a (veq:f$zero (* ,dim (length ,l*)))))
     (declare (list ,l*) (,(veq:arrtype :ff) ,a))
     (loop for ,i of-type pn in ,l*
           for ,c of-type pn from 0
           do (setf (,(veq:vvsym :nil dim :$ :pkg "VEQ") ,a ,c)
                    (@vert ,dim ,s ,i)))
     ,a)))
(defmacro 2@verts (&rest rest) `(@verts 2 ,@rest))
(defmacro 3@verts (&rest rest) `(@verts 3 ,@rest))

(defmacro vert! (dim s &rest rest &aux (gs (grph::-gensyms :pos dim))
                                       (n (gensym "N")))
  (declare (pn dim) (symbol s))
  "add vert. from these values."
  `(let ((,n (fset:size ,s)))
     (declare (pn ,n))
     (mvb (,@gs) (veq:~ ,@rest)
       (declare (veq:ff ,@gs))
       (progn (setf ,s (seq (fset:$ ,s) ,@gs))
              (the pn (/ ,n ,dim))))))
(defmacro 2vert! (&rest rest) `(vert! 2 ,@rest))
(defmacro 3vert! (&rest rest) `(vert! 3 ,@rest))

; TODO: this does not really need to be a macro
(defmacro verts! (dim s path)
  (declare (pn dim) (symbol s))
  "add verts. from path of type fvec."
  (awg (p res)
    `(let ((,p ,path) (,res (list)))
       (declare (veq:fvec ,p))
       (veq:fvprogn (,(veq:vvsym :ff dim :x@$vset :pkg :xgrph) ,p
                      ((i (:va ,dim x)) (declare (ignore i))
                       (push (vert! ,dim ,s x) ,res))))
       (reverse ,res))))
(defmacro 2verts! (&rest rest) `(verts! 2 ,@rest))
(defmacro 3verts! (&rest rest) `(verts! 3 ,@rest))

(defmacro %move! (dim s i pos &optional (pos-mode :rel))
  (declare (pn dim) (symbol s) (keyword pos-mode))
  "move vert i to pos. modes: (rel abs)."
  (awg (i* ii*)
  (let* ((gs-pos (grph::-gensyms :pos dim))
         (gs-new (grph::-gensyms :new dim))
         (rel* (loop for a in gs-pos for b in gs-new for k from 0
                     collect `(setf (fset:@ ,s (+ ,ii* ,k)) (+ ,a ,b))))
         (abs* (loop for a in gs-new for k from 0
                     collect `(setf (fset:@ ,s (+ ,ii* ,k)) ,a))))
     `(let* ((,i* ,i) (,ii* (* ,dim ,i*)))
        (declare (pn ,i* ,ii*))
        (mvb (,@gs-new) ,pos
          (declare (veq:ff ,@gs-new))
          ,(ecase pos-mode
             (:abs `(values ,@abs*))
             (:rel `(mvb (,@gs-pos) (@vert ,dim ,s ,i*) (values ,@rel*)))))))))
(defmacro move! (&rest rest) `(%move! 1 ,@rest))
(defmacro 2move! (&rest rest) `(%move! 2 ,@rest))
(defmacro 3move! (&rest rest) `(%move! 3 ,@rest))

(defmacro %vset! (dim s i pos) `(%move! ,dim ,s ,i ,pos :abs))
(defmacro vset! (s i pos) `(%move! 1 ,s ,i ,pos :abs))
(defmacro 2vset! (s i pos) `(%move! 2 ,s ,i ,pos :abs))
(defmacro 3vset! (s i pos) `(%move! 3 ,s ,i ,pos :abs))


; TODO: mode for both dirs?
(defmacro path! (dim g s path &optional modes props)
  (declare (pn dim) (symbol g s))
  "add path. modes: (-> <- <>)"
  (awg (res)
  `(let ((,res (verts! ,dim ,s ,path)))
     (declare (list ,res))
     (grph:path! ,g ,res ,modes ,props)
     ,res)))
(defmacro 2path! (&rest rest) `(path! 2 ,@rest))
(defmacro 3path! (&rest rest) `(path! 3 ,@rest))

; use % scheme for all similar fxns?
(defmacro %append! (dim g s i x &optional modes props)
  (declare (pn dim) (symbol g s))
  "append edge from vert i to pos x. returns new vert. modes: (rel abs -> <- <>)"
  (awg (j)
  (let* ((modes (grph::valid-modes :append! modes `(,@*dir-modes* ,@*pos-modes*)))
         (gs-pos (grph::-gensyms :pos dim))
         (gs-new (grph::-gensyms :new dim))
         (pm (ecase (grph::select-mode modes *pos-modes*)
               (:abs x)
               (:rel `(mvb (,@gs-new) ,x
                       (mvb (,@gs-pos) (@vert ,dim ,s ,i)
                         (values ,@(loop for a in gs-pos and b in gs-new
                                         collect `(+ ,a ,b))))))))
         (dm (ecase (grph::select-mode modes *dir-modes*)
               (:-> `(add! ,g ,i ,j ,props))
               (:<- `(add! ,g ,j ,i ,props))
               (:<> `(progn (add! ,g ,i ,j ,props)
                            (add! ,g ,j ,i ,props))))))
    (declare (type (or list symbol) modes))
    `(let ((,j (vert! ,dim ,s ,pm))) (declare (pn ,j)) ,dm ,j))))
(defmacro append! (&rest rest) `(%append! 1 ,@rest))
(defmacro 2append! (&rest rest) `(%append! 2 ,@rest))
(defmacro 3append! (&rest rest) `(%append! 3 ,@rest))

(defmacro split! (dim g s a b x)
  (declare (symbol g s) (pn dim))
  "delete edge (a b) and add edges (a x) (x b)."
  (awg (a* b* v* props*)
  `(let* ((,a* ,a) (,b* ,b) (,v* (vert! ,dim ,s ,x))
          (,props* (grph::props-as-list (grph:@prop ,g (list ,a* ,b*)))))
    (del! ,g ,a* ,b*)
    (add! ,g ,a* ,v* ,props*)
    (add! ,g ,v* ,b* ,props*)
    ,v*)))
(defmacro 2split! (&rest rest) `(split! 2 ,@rest))
(defmacro 3split! (&rest rest) `(split! 3 ,@rest))

; TODO: rename 1d versions to %@... everywhere

(defmacro @ (dim s &rest rest)
  `(veq:~ ,@(loop for i in rest collect `(@vert ,dim ,s ,i))))
(defmacro 2@ (&rest rest) `(@ 2 ,@rest))
(defmacro 3@ (&rest rest) `(@ 3 ,@rest))
(defmacro l@ (dim s &rest rest)
  `(veq:lst ,@(loop for i in rest collect `(@vert ,dim ,s ,i))))
(defmacro 2l@ (&rest rest) `(l@ 2 ,@rest))
(defmacro 3l@ (&rest rest) `(l@ 3 ,@rest))

(defmacro fxpos! ((g pos i) &body body) ; TODO: ndim
  (declare (symbol g pos i))
  `(loop for ,i in (grph:@verts ,g)
         do (2vset! ,pos ,i (progn ,@body))))

(veq:fvdef -2cent (verts max-side)
  (declare (veq:fvec verts))
  (labels ((scale-by (ms w h)
             (declare (veq:ff ms w h))
             (if (> w h) (/ ms w) (/ ms h))))
    (let ((num-verts (veq:f2$num verts)))
    (mvb (minx maxx miny maxy) (veq:f2$mima verts :n num-verts)
      (veq:f2let ((mx (f2!@*. (f2!@+ minx miny maxx maxy) 0.5f0))
                  (wh (f2!@- maxx maxy minx miny)))
        (veq:~ mx wh (if max-side (scale-by max-side wh) 1f0)))))))

; TODO: rename/refactor:
(veq:fvdef* 2center-connected (g pos &optional (x 0f0) (y 0f0) max-side)
  (declare (grph:grph g) (pos pos) (veq:ff x y))
  (mvb ((:va 2 mx wh) s) (-2cent (xgrph:2@verts pos
                                   (grph:connected-verts g)) max-side)
    (declare (veq:ff mx s) (ignore wh))
    (xgrph:fxpos! (g pos i)
      (f2!@+. (f2!@*. (f2!@- (xgrph:2@vert pos i) mx) s) x y)))
  pos)
; TODO:
; (veq:fvdef* 2center (pos &optional (x 0f0) (y 0f0) max-side)
;   (declare (pos pos) (veq:ff x y))
;   (mvb ((:va 2 mx wh) s) (-2cent (xgrph:2@verts pos
;                                    (loop for i from 0 below (fset:size pos)
;                                          collect i))
;                                  max-side)
;     (declare (veq:ff mx s) (ignore wh))
;     (xgrph:fxpos! (g pos i)
;       (f2!@+. (f2!@*. (f2!@- (xgrph:2@vert pos i) mx) s) x y)))
;   pos)

; TODO: dimensionless kdtree

