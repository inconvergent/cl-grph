(in-package :xgrph)

(veq:fvdef triangulate-edge-set (edge-set fx)
  (declare (grph g) (list edge-set))
  "triangulate the hull defined by edge set, using the fx provided where
  (funcall fx i ... k) is (values pix piy ... pkx pky)"
  (let ((path (grph:to-vector (grph:edge-set->path edge-set)))
        (res (list)))
    (declare (vector path))
    (labels ((ypos (v) (declare (grph:pn v)) (veq:fsel (:y) (funcall fx v)))
             (ind-rotate (path ymost)
               (declare (vector path) (grph:pn ymost))
               (grph::vector-rearrange path (ymost nil) (0 ymost)))
             (y-most (path &aux (ypos (ypos (aref path 0))) (cv 0) )
               (loop for v across (subseq path 1)
                     for i from 1
                     if (< (ypos v) ypos)
                     do (setf ypos (ypos v) cv i))
               cv)
             (cross (path &aux (n (length path)))
                "does any segment in path cross the line (aref path 1) (aref path -1)."
                (veq:f2let ((a (funcall fx (aref path 1)))
                            (b (funcall fx (grph::vector-last path))))
                  (loop for i from 0 below n
                        do ; weird precision issue. override veq eps
                           ; TODO: can we fix this somehow?
                           (let ((veq::*eps* (* 1000f0 veq::*eps*)))
                             (when (veq:f2segx a b (funcall fx
                                                      (aref path i)
                                                      (aref path (mod (1+ i) n))))
                               (return-from cross t)))))
                nil)
             (uw-farthest (path &aux (n (length path)) dst (curr -1))
              "find the vertex in triangle ((aref path 1) (aref path 0) (aref path -1))
               that is the farthest away from (aref path 1) (aref path -1)."
               (loop for i from 2 below (1- n)
                     if ; TODO: this feels very sketchy
                       (let ((veq::*eps* (* 1000f0 veq::*eps*)))
                          (veq:f2in-triangle
                            (funcall fx (aref path 0) (aref path 1)
                                        (grph::vector-last path) (aref path i))))
                     do (let ((d (veq:f2segdst
                                   (funcall fx
                                     (aref path 1) (grph::vector-last path) (aref path i)))))
                          (when (or (= curr -1) (> d dst)) (setf curr i dst d))))
               (values curr dst))
             (split-diag (path i)
               "split into two paths by introducing a new edge that divides
                path in two loops."
               (when (= i -1) (error "bad split (-1) at: ~a" path))
               (when (< (length path) 3) (error "diag: too few elements in ~a" path))
               (when (>= i (length path)) (error "diag bad ind: ~a ~a" path i))
               (do-step (grph::vector-rearrange path (i) (0 i)))
               (do-step (grph::vector-rearrange path (0) (i nil))))
             (split-tri (path)
               "split path into a triangle and a new path."
               (do-step (grph::vector-rearrange path ((1- (length path))) (0 2)))
               (do-step (subseq path 1)))
             (do-step (path &aux (n (length path)))
               "recursively subdived the path into triangles and/or new loops."
               ; (when (< n 3) (error "do-step: too few elements in ~a" path))
               (when (< n 3) (return-from do-step nil))
               (when (< n 4) (push (grph::to-list path) res)
                             (return-from do-step t))

               ; this is confusing, but i was unable to find a better way. the
               ; problem is that sometimes cross path detects an intersection,
               ; but uw-farthest is unable to find a valid candidate. this is
               ; probably due to precision issues in either cross, uw-farthest,
               ; or both?
               (let ((path (ind-rotate path (y-most path))))
                 (if (not (cross path))
                     (split-tri path) ; -> no intersections
                     (let ((uw (uw-farthest path))) ; -> possible intersection
                       (if (> uw -1)
                           (split-diag path uw) ; intersection
                           (progn (warn "possble intersection, unable to detect farthest uw.")
                                  (split-tri path)))))))) ; unable to detect intersection
      (do-step path)
      res)))

; (defun triang (pos path)
;   (grph:triangulate-edge-set
;     (grph:path->edge-set p :closed t)
;     (lambda (&rest rest)
;       (apply #'values (apply #'concatenate 'list
;                              (mapcar (lambda (i)
;                                        (veq:lst (xgrph:2@vert pos i)))
;                                      rest))))))
