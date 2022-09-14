(in-package :xgrph)

(defun pos (&optional (d 0f0))
  (declare (veq:ff d))
  "initialze xgrph pos."
  (fset:empty-seq d))

(defmacro @vert (dim s i)
  (declare (grph:pn dim))
  "get vert i."
  (grph::awg (k)
    `(let ((,k (* ,dim ,i)))
      (declare (grph:pn ,k))
      (values ,@(loop for i from 0 below dim
                      collect `(fset:@ ,s (+ ,k ,i)))))))
(defmacro 2@vert (&rest rest) `(@vert 2 ,@rest))
(defmacro 3@vert (&rest rest) `(@vert 3 ,@rest))

(defmacro @num (dim s) (declare (grph:pn dim)) `(the grph:pn (/ (fset:size ,s) ,dim)))
(defmacro 2@num (&rest rest) `(@num 2 ,@rest))
(defmacro 3@num (&rest rest) `(@num 3 ,@rest))


(defmacro @verts (dim s l)
  (declare (grph:pn dim))
  "get verts in l."
  (grph::awg (a i c h l*)
    `(let ((,l* ,l))
       (declare (list ,l*))
       (veq:fvprogn
         (veq:fwith-arrays (:inds ,l* :itr ,h :cnt ,c
           :arr ((,a ,dim (veq:f$zero (* ,dim (length ,l*)))))
           :fxs ((vget (,i) (@vert ,dim ,s ,i)))
           :exs ((,a ,c (vget ,h))))
           ,a)))))
(defmacro 2@verts (&rest rest) `(@verts 2 ,@rest))
(defmacro 3@verts (&rest rest) `(@verts 3 ,@rest))

(defmacro vert! (dim s &rest rest
                       &aux (gs (veq::-gensyms :pos dim))
                            (n (gensym "N")))
  (declare (grph:pn dim))
  "add vert."
  `(let ((,n (fset:size ,s)))
     (declare (grph:pn ,n))
     (grph::mvb (,@gs) (grph::mvc #'values ,@rest)
       (declare (veq:ff ,@gs))
       (progn (setf ,s (fset:seq (fset:$ ,s) ,@gs))
              (the grph:pn (/ ,n ,dim))))))
(defmacro 2vert! (&rest rest) `(vert! 2 ,@rest))
(defmacro 3vert! (&rest rest) `(vert! 3 ,@rest))

(defmacro verts! (dim s path)
  (declare (grph:pn dim))
  "add verts."
  (grph::awg (p res)
    `(let ((,p ,path) (,res (list)))
      (declare (veq:fvec ,p))
      (veq:fvprogn
        (veq:fwith-arrays (:n (the grph:pn (/ (length ,p) ,dim))
          :arr ((,p ,dim ,p))
          :fxs ((add ((:va ,dim x))
                 (declare (veq:ff x))
                 (push (vert! ,dim ,s x) ,res)))
          :nxs ((add ,p))))
        (reverse ,res)))))
(defmacro 2verts! (&rest rest) `(verts! 2 ,@rest))
(defmacro 3verts! (&rest rest) `(verts! 3 ,@rest))


; TODO: both dirs?
(defmacro path! (dim g s path &optional props)
  (declare (grph:pn dim))
  "add path with optional :prop."
  (grph::awg (i j res props*)
    `(let ((,props* ,props)
           (,res (verts! ,dim ,s ,path)))
       (declare (list ,res))
       (loop for ,i in ,res for ,j in (cdr ,res)
             do (grph:add! ,g ,i ,j ,props*))
       ,res)))
(defmacro 2path! (&rest rest) `(path! 2 ,@rest))
(defmacro 3path! (&rest rest) `(path! 3 ,@rest))

(defmacro move! (dim s i pos &optional (mode :rel)
                                  &aux (gs (veq::-gensyms :pos dim))
                                       (gs-new (veq::-gensyms :new dim)))
  (declare (grph:pn dim))
  "move vert i to pos. use mode :rel or :abs."
  (grph::awg (i* ii*)
    (labels ((rel* () (loop for a in gs for b in gs-new for k from 0
                            collect `(setf (fset:@ ,s (+ ,ii* ,k)) (+ ,a ,b))))
             (abs* () (loop for a in gs-new for k from 0
                            collect `(setf (fset:@ ,s (+ ,ii* ,k)) ,a))))
       `(let* ((,i* ,i) (,ii* (* ,dim ,i*)))
          (declare (grph:pn ,i* ,ii*))
          (grph::mvb (,@gs-new) ,pos
            (declare (veq:ff ,@gs-new))
            ,(case mode
               (:abs `(values ,@(abs*)))
               (:rel `(grph::mvb (,@gs) (@vert ,dim ,s ,i*) (values ,@(rel*))))
               (t (error "move! bad mode: ~a. use :rel or :abs." mode))))))))
(defmacro 2move! (&rest rest) `(move! 2 ,@rest))
(defmacro 3move! (&rest rest) `(move! 3 ,@rest))

(defmacro append! (dim g s i x &optional (mode :rel) props
                                    &aux (gs (veq::-gensyms :pos dim))
                                         (gs-new (veq::-gensyms :new dim)))
  (declare (grph:pn dim))
  "append edge from vert i to pos x. returns new vert."
  (grph::awg (j)
    `(let ((,j (vert! ,dim ,s
                 ,(case mode
                    (:abs x)
                    (:rel `(grph::mvb (,@gs-new) ,x
                             (grph::mvb (,@gs) (@vert ,dim ,s ,i)
                               (values ,@(loop for a in gs and b in gs-new
                                               collect `(+ ,a ,b))))))
                    (t (error "APPEND! bad mode: ~a, use :rel or :abs."
                              mode))))))
      (declare (grph:pn ,j))
      (grph:add! ,g ,i ,j ,props)
      ,j)))
(defmacro 2append! (&rest rest) `(append! 2 ,@rest))
(defmacro 3append! (&rest rest) `(append! 3 ,@rest))

(defmacro split! (dim g s a b x &optional props)
  (declare (grph:pn dim))
  "delete edge (a b) and add edges (a x) (x b)."
  (grph::awg (v props*)
    `(let ((,v (xgrph:vert! ,dim ,s ,x))
           (,props* ,props))
      (grph:del! ,g ,a ,b)
      (grph:add! ,g ,a ,v ,props*)
      (grph:add! ,g ,v ,b ,props*)
      ,v)))
(defmacro 2split! (&rest rest) `(split! 2 ,@rest))
(defmacro 3split! (&rest rest) `(split! 3 ,@rest))

(defmacro @ (dim s &rest rest)
  `(veq:~ ,@(loop for i in rest collect `(@vert ,dim ,s ,i))))
(defmacro 2@ (&rest rest) `(@ 2 ,@rest))
(defmacro 3@ (&rest rest) `(@ 3 ,@rest))

; transform
; split-edge e x

