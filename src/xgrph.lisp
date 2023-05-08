(in-package :xgrph)

(defun pos (&optional (d 0f0))
  (declare (veq:ff d))
  "initialze xgrph pos. d is the default fset:seq value."
  (empty-seq d))

(defmacro @vert (dim s i)
  (declare (pn dim) (symbol s))
  "get vert i as values."
  (grph::awg (k)
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
  (grph::awg (a i c l*)
  `(let* ((,l* ,l)
          (,a (veq:f$zero (* ,dim (length ,l*)))))
     (declare (list ,l*) (,(veq:arrtype :ff) ,a))
     (loop for ,i of-type veq:pn in ,l*
           for ,c of-type veq:pn from 0
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
  (grph::awg (p res)
    `(let ((,p ,path) (,res (list)))
       (declare (veq:fvec ,p))
       (veq:fvprogn (,(veq:vvsym :ff dim :x@vset :pkg :xgrph) ,p
                      ((i (:va ,dim x)) (declare (ignore i))
                       (push (vert! ,dim ,s x) ,res))))
       (reverse ,res))))
(defmacro 2verts! (&rest rest) `(verts! 2 ,@rest))
(defmacro 3verts! (&rest rest) `(verts! 3 ,@rest))

(defmacro %move! (dim s i pos &optional (pos-mode :rel))
  (declare (pn dim) (symbol s) (keyword pos-mode))
  "move vert i to pos.
pos-modes: (:rel :abs)."
  (grph::awg (i* ii*)
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
  "add path.
dir-modes: (-> <- <>)"
  (grph::awg (res)
  `(let ((,res (verts! ,dim ,s ,path)))
     (declare (list ,res))
     (grph:path! ,g ,res ,modes ,props)
     ,res)))
(defmacro 2path! (&rest rest) `(path! 2 ,@rest))
(defmacro 3path! (&rest rest) `(path! 3 ,@rest))

; use % scheme for all similar fxns?
(defmacro %append! (dim g s i x &optional modes props)
  (declare (pn dim) (symbol g s))
  "append edge from vert i to pos x. returns new vert.
pos-modes: (rel abs)
dir-modes: (-> <- <>)."
  (grph::awg (j)
  (let* ((modes (grph::valid-modes :append! modes `(,@*dir-mode* ,@*pos-mode*)))
         (gs-pos (grph::-gensyms :pos dim))
         (gs-new (grph::-gensyms :new dim))
         (pm (ecase (grph::select-mode modes *pos-mode*)
               (:abs x)
               (:rel `(mvb (,@gs-new) ,x
                       (mvb (,@gs-pos) (@vert ,dim ,s ,i)
                         (values ,@(loop for a in gs-pos and b in gs-new
                                         collect `(+ ,a ,b))))))))
         (dm (ecase (grph::select-mode modes *dir-mode*)
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
  (grph::awg (a* b* v* props*)
  `(let* ((,a* ,a) (,b* ,b)
          (,v* (vert! ,dim ,s ,x))
          (,props* (grph::props-as-list (grph:@prop ,g (list ,a* ,b*)))))
    (del! ,g ,a* ,b*)
    (add! ,g ,a* ,v* ,props*)
    (add! ,g ,v* ,b* ,props*)
    ,v*)))
(defmacro 2split! (&rest rest) `(split! 2 ,@rest))
(defmacro 3split! (&rest rest) `(split! 3 ,@rest))

(defmacro @ (dim s &rest rest)
  `(veq:~ ,@(loop for i in rest collect `(@vert ,dim ,s ,i))))
(defmacro 2@ (&rest rest) `(@ 2 ,@rest))
(defmacro 3@ (&rest rest) `(@ 3 ,@rest))

; TODO: ndim
(defmacro fxpos! ((g pos i) &body body)
  (declare (symbol g pos i))
  `(loop for ,i in (grph:@verts ,g)
         do (xgrph:2vset! ,pos ,i (progn ,@body))))

; TODO: transform
; TODO: intersect-all

