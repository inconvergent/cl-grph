(in-package :xgrph)

(deftype simple-list () `(simple-array list))

(veq:fvdef 2intersect-all (g pos)
  (declare (grph:grph g) (pos pos))
  "creates intersections for all edges in g such that it becomes a planar graph."
  (let ((crossing->vert (make-hash-table :test #'equal)))
    (declare (hash-table crossing->vert))
    (labels
      ((ic (i c) (declare (fixnum i c)) (if (< i c) (list i c) (list c i)))
       (-add (a b &key e)
         (declare (pn a b) (list e))
         (add! g a b (grph::props-as-list (grph:@prop g e))))
       (edges-as-lines (edges)
         (declare (simple-list edges))
         (loop for edge of-type list across edges collect (2@verts pos edge)))
       (sort-hits (isects)
         (declare (simple-list isects))
         (loop for i of-type fixnum from 0 below (length isects)
               if (aref isects i) do (setf (aref isects i)
                                           (sort (aref isects i) #'< :key #'cdr)))
         isects)
       (add-vert (line i hits)
         (declare (list hits) (fixnum i))
         (loop for (c . p) in hits
               if (not (gethash (the list (ic i c)) crossing->vert))
               do (let ((new (2vert! pos (veq:f2lerp (veq:f2$ line 0 1) p))))
                    (declare (pn new))
                    (setf (gethash (the list (ic i c)) crossing->vert) new))))
       (add-new-verts (edges isects)
         (declare (simple-list edges isects))
         (loop for hits across isects for i of-type fixnum from 0
               if hits do (add-vert (2@verts pos (aref edges i)) i hits)))
       (del-hit-edges (edges isects)
         (declare (simple-list edges isects))
         (loop for hits of-type list across isects for i of-type fixnum from 0
               if hits do (grph::ldel! g (aref edges i))
                          (loop for (c . p) in hits
                                do (grph::ldel! g (aref edges c)))))
       (add-new-edges (edges isects)
         (declare (simple-list edges isects))
         (loop for hits of-type list across isects for i of-type fixnum from 0
               if hits
               do (loop with cc = (grph::lpos hits) with ei = (aref edges i)
                        for a of-type fixnum in cc and b of-type fixnum in (cdr cc)
                        initially (-add (gethash (ic i (first cc)) crossing->vert)
                                        (first ei) :e ei)
                                  (-add (gethash (ic i (grph::last* cc)) crossing->vert)
                                        (grph::last* ei) :e ei)
                        do (-add (gethash (ic i a) crossing->vert)
                                 (gethash (ic i b) crossing->vert)
                                 :e ei)))))
      (let* ((edges (grph:to-vector (grph:@edges g))) ; edges ((v1 v2) (v8 v1) ...)
             (lines (grph:to-vector (edges-as-lines edges))) ; lines: (#(ax ay bx by) #(cx cy dx dy) ...)
             (veq::*eps* 0.00001)
             ; isects: #(((16 . 0.18584675) (5 . 0.35215548)) NIL NIL ...)
             (isects (sort-hits (veq:f2lsegx lines)))) ;  p/q is the lerp
        (declare (simple-list isects edges) (simple-array lines))
        (add-new-verts edges isects)
        (add-new-edges edges isects)
        (del-hit-edges edges isects)
        (values g pos)))))
(defmacro 2intersect-all! (g pos &rest rest)
 `(mvb (g* pos*) (2intersect-all ,g ,pos ,@rest)
    (setf ,g g* ,pos pos*)))

; TODO: propagate props
; TODO: use modify! macro
(veq:fvdef 3cut-all (g pos fx)
  (declare (grph:grph g) (pos pos) (function fx))
  "cut every edge where they intersect in 2d according to projection fx."
  (labels
    ((edges-as-lines (edges)
       (declare (simple-list edges))
       (loop for (a b) across edges
             collect (veq:f2$line (f@fx (3@ pos a)) (f@fx (3@ pos b)))))
     (sort-hits (isects)
       (declare (simple-list isects))
       (loop for i of-type fixnum from 0 below (length isects)
             if (aref isects i) do (setf (aref isects i)
                                         (sort (aref isects i) #'< :key #'cdr)))
       isects)
     (add-path-verts (old-edge line hits)
       (declare (list old-edge hits))
       "add verts along edge for each intersect"
       (loop for (c . p) in hits
             collect (3vert! pos (veq:f3lerp (veq:f3$ line 0 1) p))))

     (add-new-paths (edges isects)
       (declare (simple-list edges isects))
       "add new edge along old edge with new verts for each intersect"
       (loop for hits across isects for i of-type fixnum from 0
             if hits
             do (let* ((old-edge (aref edges i))
                       (path-ind (add-path-verts old-edge
                                   (3@verts pos old-edge) hits)))
                  (declare (list old-edge path-ind))
                  (when path-ind
                    (grph:path! g
                      (cons (first old-edge) `(,@path-ind ,@(last old-edge))))))))
     (del-hit-edges (edges isects)
       (declare (simple-list edges isects))
       (loop for hits of-type list across isects
             for i of-type fixnum from 0
             if hits do (grph::ldel! g (aref edges i))
                        (loop for (c . p) in hits
                              do (grph::ldel! g (aref edges c))))))
    (let* ((edges (grph:to-vector (grph:@edges g)))  ; edges ((v1 v2) (v8 v1) ...)
           (lines (grph:to-vector (edges-as-lines edges))) ; lines: (#(ax ay bx by) #(cx cy dx dy) ...)
           (veq::*eps* 0.00001)
           ; isects: #(((16 . 0.18584675) (5 . 0.35215548)) NIL NIL ...)
           (isects (sort-hits (veq:f2lsegx lines)))) ;  p/q is the lerp
      ; (declare (grph::simple-list isects edges) (grph::simple-array lines))
      (del-hit-edges edges isects)
      (add-new-paths edges isects)
      (values g pos))))
(defmacro 3cut-all! (g pos fx)
 `(mvb (g* pos*) (3cut-all ,g ,pos ,fx)
    (setf ,g g* ,pos pos*)))

(veq:fvdef* 2cut-to-area (g pos &optional (top 0f0) (lft 0f0)
                                          (rht 1000f0) (bot 1000f0))
  (declare (grph:grph g) (pos pos) (veq:ff top lft bot rht))
  "removes all edges outside envelope.
all edges intersecting the envelope will be deleted, a new vert will be
inserted on the intersection; connected to the inside vert."
  (labels
    ((inside (i)
      (declare (grph::pn i))
      (veq:xlet ((f2!p (2@ pos i)))
        (and (> (:vr p 0) lft) (> (:vr p 1) top)
             (< (:vr p 0) rht) (< (:vr p 1) bot))))
     (split-line (ai bi &aux (rev nil))
       (declare (grph::pn ai bi) (boolean rev))
       (unless (inside ai) (rotatef ai bi) (setf rev t))
       (veq:xlet ((f2!a (2@ pos ai))
                  (f2!b (2@ pos bi))
                  (f2!ab (f2!@- b a)))
         (veq:~ rev ; there appears to be a slight bug here for lines
           (veq:f2lerp a b ; that cross both a side and a top/bot
             (cond ((> (:vr b 0) rht) (/ (- rht (:vr a 0)) (:vr ab 0)))
                   ((> (:vr b 1) bot) (/ (- bot (:vr a 1)) (:vr ab 1)))
                   ((< (:vr b 0) lft) (/ (- lft (:vr a 0)) (:vr ab 0)))
                   (t (/ (- top (:vr a 1)) (:vr ab 1))))))))
     (cutfx (&rest line)
       (declare (list line))
       (case (length (remove-if-not #'inside line))
         (0 (values :outside nil 0f0 0f0))
         (1 (veq:~ :split (apply #'split-line line)))
         (t (values :keep nil 0f0 0f0)))))
    (grph:qry g :using (^g ^pos) :select (?x ?y) :where (or (?x _ ?y))
      :then (mvb (state rev (:va 2 px)) (cutfx ?x ?y)
              (ecase state
                (:keep nil)
                (:outside (grph:del! ^g ?x ?y))
                (:split (let ((props (grph::props-as-list (grph:@prop g (list ?x ?y)))))
                          (grph:del! ^g ?x ?y)
                          (2append! ^g ^pos (if rev ?y ?x) (veq:f2 px) abs props)))))))
  (values g pos))
(defmacro 2cut-to-area! (g pos &rest rest)
  (declare (symbol g pos))
  `(mvb (g* pos*) (2cut-to-area ,g ,pos ,@rest)
     (setf ,g g* ,pos pos*)))

(veq:fvdef* 2cut (g pos (:va 4 line))
  (declare (grph:grph g) (pos pos) (veq:ff line))
  "cut g/pos along line. returns g, pos and a list of (vi si) where vi is a new
vertex index in pos and s is the lerp along line"
  (let ((res (list)))
    (grph:using (^g ^pos)
      (labels ((new-vert? (s &aux (v (xgrph:2vert! ^pos (veq:f2lerp line s))))
                 (push (list v s) res) v))
        ; TODO: this iteration will do some duplicate checks. but i think it
        ; will work because of how split! operates with >< mode
        (grph:itr-edges (g a b)
          (mvb (isect s) (veq:f2segx line (2@ pos a b))
               (when isect (grph:split! ^g a b (new-vert? s) ><))))))
  (values g pos res)))
(defmacro 2cut! (g pos &rest rest)
  (declare (symbol g pos))
  "cut g/pos along line. returns a list of (vi si) where vi is a new vertex
index in pos and s is the lerp along line"
  `(mvb (g* pos* res) (2cut ,g ,pos ,@rest)
     (setf ,g g* ,pos pos*)
     res))

(veq:fvdef* 2mirror (g pos (:va 2 a b) &optional sidefx)
  (declare (grph:grph g) (pos pos) (veq:ff a b))
  "mirror around line ab.
optionally delete edges on the side of ab where (sidefx (cross ab va) 0f0)"
  (labels ((mark (vv &aux (ht (make-hash-table :test #'eql)))
             (loop for v in vv do (setf (gethash v ht) :cut)) ht))
    (grph:using (^g ^pos)
      (veq:xlet ((cuts-ht (mark (mapcar #'first (2cut! ^g ^pos a b))))
                 (f2!ab (f2!@- b a)))
        (labels
          ((strip-cut-verts (ea eb)
             (remove-if (lambda (i) (eq :cut (gethash i cuts-ht))) (list ea eb)))
           (del-side? (ee)
              (some (lambda (i)
                      (funcall (the function sidefx)
                               (veq:f2cross ab (f2!@- (xgrph:2@ pos i) a)) 0f0))
                    ee))
           (sym (i)
             (veq:xlet ((f2!pp (xgrph:2@ pos i)) (f2!ap (f2!@- pp a))
                        (f2!nn (f2!@- ap (f2!@/. (f2!@*. ab (veq:f2dot ap ab))
                                                 (veq:f2len2 ab)))))
               (setf (gethash i cuts-ht)
                     (xgrph:2vert! ^pos (f2!@- pp (f2!@*. nn 2f0))))))
           (vert (i &aux (k (gethash i cuts-ht)))
             (cond ((eq :cut k) i) ((numberp k) k) (t (sym i))))
           (do-edge (ea eb)
            (grph:add! ^g (vert ea) (vert eb)
              (grph::props-as-list (grph:@prop g `(,ea ,eb)))))
           (do-edge-del (ea eb)
             (if (del-side? (strip-cut-verts ea eb))
                 (grph:del! ^g ea eb) ; add del side as :del in ht?
                 (do-edge ea eb))))
          (if sidefx (grph:itr-edges (^g ea eb) (do-edge-del ea eb))
                     (grph:itr-edges (^g ea eb) (do-edge ea eb))))
        (values ^g ^pos cuts-ht)))))
(defmacro 2mirror! (g pos &rest rest)
  (declare (symbol g pos))
 `(mvb (g* pos* res) (2mirror ,g ,pos ,@rest)
    (setf ,g g* ,pos pos*)
    res))

