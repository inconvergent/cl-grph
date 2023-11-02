(in-package :xgrph)

(deftype pos () 'fset:seq)

(defun pos (&optional (v 0f0))
  (declare (veq:ff v)) "initialze pos (fset:seq), v is the default value."
  (the pos (empty-seq v)))

(defmacro @vert (dim s i)
  (declare (pn dim) (symbol s)) "get vert i as dim values."
  (awg (k)
  `(let ((,k (* ,dim ,i)))
    (declare (pn ,k))
    (values ,@(loop for i from 0 below dim collect `(fset:@ ,s (+ ,k ,i)))))))
(defmacro 2@vert (s i) "get vert i as 2 values." `(@vert 2 ,s ,i))
(defmacro 3@vert (s i) "get vert i as 3 values." `(@vert 3 ,s ,i))

(defmacro @num (dim s) (declare (pn dim)) "number of nd elements in s."
  `(the pn (/ (fset:size ,s) ,dim)))
(defmacro 2@num (s) "number of 2d elements in s." `(@num 2 ,s))
(defmacro 3@num (s) "number of 3d elements in s." `(@num 3 ,s))

(defmacro @verts (dim s l)
  (declare (pn dim) (symbol s)) "get verts in l as fvec."
  (awg (a i c l*)
  `(let* ((,l* ,l) (,a (veq:f$zero (* ,dim (length ,l*)))))
     (declare (list ,l*) (,(veq:arrtype :ff) ,a))
     (loop for ,i of-type pn in ,l*
           for ,c of-type pn from 0
           do (setf (,(veq:vvsym :nil dim :$ :pkg "VEQ") ,a ,c)
                    (@vert ,dim ,s ,i)))
     ,a)))
(defmacro 2@verts (s l) "get verts in l as fvec." `(@verts 2 ,s ,l))
(defmacro 3@verts (s l) "get verts in l as fvec." `(@verts 3 ,s ,l))

(defmacro @ (dim s &rest inds) ; TODO: rename 1d versions to %@... everywhere
  "get these inds as values."
  `(veq:~ ,@(loop for i in inds collect `(@vert ,dim ,s ,i))))
(defmacro 2@ (s &rest inds) "get these inds as values." `(@ 2 ,s ,@inds))
(defmacro 3@ (s &rest inds) "get these inds as values." `(@ 3 ,s ,@inds))
(defmacro l@ (dim s &rest inds) "get these inds as list."
  `(veq:lst ,@(loop for i in inds collect `(@vert ,dim ,s ,i))))
(defmacro 2l@ (s &rest inds) "get these inds as list." `(l@ 2 ,s ,@inds))
(defmacro 3l@ (s &rest inds) "get these inds as list." `(l@ 3 ,s ,@inds))

(defmacro vert! (dim s &rest pos &aux (gs (grph::-gensyms :pos dim)) (n (gensym "N")))
  (declare (pn dim) (symbol s)) "add vert from values (pos)."
  `(let ((,n (fset:size ,s)))
     (declare (pn ,n))
     (mvb (,@gs) (veq:~ ,@pos)
       (declare (veq:ff ,@gs))
       (progn (setf ,s (seq (fset:$ ,s) ,@gs))
              (the pn (/ ,n ,dim))))))
(defmacro 2vert! (s &rest pos) "add vert from values (pos)." `(vert! 2 ,s ,@pos))
(defmacro 3vert! (s &rest pos) "add vert from values (pos)." `(vert! 3 ,s ,@pos))

(defmacro verts! (dim s path)
  (declare (pn dim) (symbol s)) "add verts from path."
  (awg (p res)
    `(let ((,p ,path) (,res (list)))
       (declare (veq:fvec ,p))
       (veq:fvprogn (,(veq:vvsym :ff dim :x@$vset :pkg :xgrph) ,p
                      ((i (:va ,dim x)) (declare (ignore i))
                       (push (vert! ,dim ,s x) ,res))))
       (reverse ,res))))
(defmacro 2verts! (s path) "add verts from path." `(verts! 2 ,s ,path))
(defmacro 3verts! (s path) "add verts from path." `(verts! 3 ,s ,path))

(defmacro %move! (dim s i pos &optional (mode :rel))
  (declare (pn dim) (symbol s) (keyword mode))
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
          ,(ecase mode
             (:abs `(values ,@abs*))
             (:rel `(mvb (,@gs-pos) (@vert ,dim ,s ,i*) (values ,@rel*)))))))))
(defmacro move!  (s i pos &optional (mode :rel)) "move vert i to pos. modes: (rel abs)."
  `(%move! 1 ,s ,i ,pos ,mode))
(defmacro 2move!  (s i pos &optional (mode :rel)) "move vert i to pos. modes: (rel abs)."
  `(%move! 2 ,s ,i ,pos ,mode))
(defmacro 3move!  (s i pos &optional (mode :rel)) "move vert i to pos. modes: (rel abs)."
  `(%move! 3 ,s ,i ,pos ,mode))

(defmacro %vset! (dim s i &rest pos) "set i to pos." `(%move! ,dim ,s ,i (veq:~ ,@pos) :abs))
(defmacro vset! (s i &rest pos) "set i to pos." `(%move! 1 ,s ,i (veq:~ ,@pos) :abs))
(defmacro 2vset! (s i &rest pos) "set i to pos." `(%move! 2 ,s ,i (veq:~ ,@pos) :abs))
(defmacro 3vset! (s i &rest pos) "set i to pos." `(%move! 3 ,s ,i (veq:~ ,@pos) :abs))

; TODO: mode for both dirs?
(defmacro path! (dim g s path &optional modes props)
  (declare (pn dim) (symbol g s)) "add path. modes: (-> <- <>)."
  (awg (res)
  `(let ((,res (verts! ,dim ,s ,path)))
     (declare (list ,res))
     (grph:path! ,g ,res ,modes ,props)
     ,res)))
(defmacro 2path! (g s path &optional modes props) "add path. modes: (-> <- <>)."
  `(path! 2 ,g ,s ,path ,modes ,props))
(defmacro 3path! (g s path &optional modes props) "add path. modes: (-> <- <>)."
  `(path! 3 ,g ,s ,path ,modes ,props))

; use % scheme for all similar fxns?
(defmacro %append! (dim g s i x &optional modes props)
  (declare (pn dim) (symbol g s))
  "append edge from vert i to pos x. returns new vert. modes: (rel abs -> <- <>)."
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
(defmacro append! (g s i x &optional modes props)
  "append edge from vert i to pos x. returns new vert. modes: (rel abs -> <- <>)."
  `(%append! 1 ,g ,s ,i ,x ,modes ,props))
(defmacro 2append! (g s i x &optional modes props)
  "append edge from vert i to pos x. returns new vert. modes: (rel abs -> <- <>)."
  `(%append! 2 ,g ,s ,i ,x ,modes ,props))
(defmacro 3append! (g s i x &optional modes props)
  "append edge from vert i to pos x. returns new vert. modes: (rel abs -> <- <>)."
  `(%append! 3 ,g ,s ,i ,x ,modes ,props))

(defmacro split! (dim g s a b x)
  (declare (symbol g s) (pn dim)) "delete edge (a b) and add edges (a x) (x b)."
  (awg (a* b* v* props*)
  `(let* ((,a* ,a) (,b* ,b) (,v* (vert! ,dim ,s ,x))
          (,props* (grph::props-as-list (grph:@prop ,g (list ,a* ,b*)))))
    (del! ,g ,a* ,b*)
    (add! ,g ,a* ,v* ,props*)
    (add! ,g ,v* ,b* ,props*)
    ,v*)))
(defmacro 2split! (g s a b x) "delete edge (a b) and add edges (a x) (x b)."
  `(split! 2 ,g ,s ,a ,b ,x))
(defmacro 3split! (g s a b x) "delete edge (a b) and add edges (a x) (x b)."
  `(split! 3 ,g ,s ,a ,b ,x))

(defmacro itr-vset! ((g pos i) &body body) ; TODO: ndim
  (declare (symbol g pos i))
  "iterate all verts as i and set it to the result of body."
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

(veq:fvdef* 2center-connected (g pos &optional (x 0f0) (y 0f0) max-side) ; TODO: rename/refactor:
  (declare (grph:grph g) (pos pos) (veq:ff x y))
  (mvb ((:va 2 mx wh) s) (-2cent (xgrph:2@verts pos
                                   (grph:connected-verts g)) max-side)
    (declare (veq:ff mx s) (ignore wh))
    (itr-vset! (g pos i)
      (f2!@+. (f2!@*. (f2!@- (xgrph:2@vert pos i) mx) s) x y)))
  pos)

; (veq:fvdef* 2center (pos &optional (x 0f0) (y 0f0) max-side) ; TODO:
;   (declare (pos pos) (veq:ff x y))
;   (mvb ((:va 2 mx wh) s) (-2cent (xgrph:2@verts pos
;                                    (loop for i from 0 below (fset:size pos)
;                                          collect i))
;                                  max-side)
;     (declare (veq:ff mx s) (ignore wh))
;     (itr-vset! (g pos i)
;       (f2!@+. (f2!@*. (f2!@- (xgrph:2@vert pos i) mx) s) x y)))
;   pos)
; TODO: dimensionless kdtree

