(in-package :xgrph)

(deftype simple-list () `(simple-array list))

(veq:fvdef 2intersect-all (g pos &optional (edges (grph:to-vector (grph:@edges g))))
  (declare (grph:grph g) (pos pos) (simple-list edges))
  "creates intersections for all edges in g such that it becomes a planar graph."
  (let ((crossing->vert (make-hash-table :test #'equal)))
    (declare (hash-table crossing->vert))
    (labels
      ((ic (i c) (declare (fixnum i c)) (if (< i c) (list i c) (list c i)))
       (-add (a b &key e)
         (declare (pn a b) (list e))
         (add! g a b (grph:@prop g e)))
       (edges-as-lines (edges) ; TODO: build vector directly
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
               if hits do (grph:ldel! g (aref edges i))
                          (loop for (c . p) in hits
                                do (grph:ldel! g (aref edges c)))))
       (add-new-edges (edges isects)
         (declare (simple-list edges isects))
         (loop for hits of-type list across isects for i of-type fixnum from 0
               if hits
               do (loop with cc = (veq:lpos hits) with ei = (aref edges i)
                        for a of-type fixnum in cc and b of-type fixnum in (cdr cc)
                        initially (-add (gethash (ic i (first cc)) crossing->vert)
                                        (first ei) :e ei)
                                  (-add (gethash (ic i (grph::last* cc)) crossing->vert)
                                        (grph::last* ei) :e ei)
                        do (-add (gethash (ic i a) crossing->vert)
                                 (gethash (ic i b) crossing->vert)
                                 :e ei)))))
      (let* (;(edges (grph:to-vector (grph:@edges g))) ; edges ((v1 v2) (v8 v1) ...)
             (lines (grph:to-vector (edges-as-lines edges))) ; lines: (#(ax ay bx by) #(cx cy dx dy) ...)
             (veq::*eps* 0.00001)
             ; isects: #(((16 . 0.18584675) (5 . 0.35215548)) NIL NIL ...)
             (isects (sort-hits (veq:f2lsegx lines)))) ;  p/q is the lerp
        (declare (simple-list isects) (simple-array lines))

        ; filter out edges that share vertices from intersection hits because sometimes
        ; contiguous lines yield intersection.
        (labels ((filter-hits (e hits)
                   (loop for (k . s) in hits
                         if (not (intersection e (aref edges k))) collect `(,k . ,s))))
          (loop for i from 0 below (length edges) for hits = (aref isects i)
              if hits do (setf (aref isects i) (filter-hits (aref edges i) hits))))

        (add-new-verts edges isects)
        (add-new-edges edges isects)
        (del-hit-edges edges isects)
        (values g pos)))))

(defmacro 2intersect-all! (g pos &rest rest)
 `(mvb (g* pos*) (2intersect-all ,g ,pos ,@rest)
    (setf ,g g* ,pos pos*)))


; TODO: use modify! macro
; TODO: remove old, use modify
(veq:fvdef 3cut-all (g pos fx &aux (old g))
  (declare (grph:grph g) (pos pos) (function fx))
  "cut every edge where they intersect in 2d
according to projection (fx x y z) => (~ x1 y1).
propagates properties."
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
       (loop for (c . p) in hits collect (3vert! pos (veq:f3lerp (veq:f3$ line 0 1) p))))

     (do-add-new-path (edges hits e)
       (let ((path-ind (add-path-verts e (3@verts pos e) hits)))
         (when path-ind (grph:path! g `(,(car e) ,@path-ind ,(cadr e))
                                    -> (grph:@prop old e)))))
     (add-new-paths (edges isects)
       (declare (simple-list edges isects))
       "add new edge along old edge with new verts for each intersect"
       (loop for i of-type fixnum from 0 for hits across isects
             if hits do (do-add-new-path edges hits (aref edges i))))

     (del-hit-edges (edges isects)
       (declare (simple-list edges isects))
       (loop for i of-type fixnum from 0
             for hits of-type list across isects
             if hits do (grph:ldel! g (aref edges i))
                        (loop for (c . p) in hits
                              do (grph:ldel! g (aref edges c))))))
          ; eg.  edges:  ( (v1 v2) (v8 v1) ... )
          ;      lines:  ( #(ax ay bx by) #(cx cy dx dy) ... )
          ;     isects:  #( ( (16 . 0.18584675) (5 . 0.35215548) ) NIL NIL ... )
          ;              NOTE: p/q is the lerp
    (let* ((edges (grph:to-vector (grph:@edges g)))
           (lines (grph:to-vector (edges-as-lines edges)))
           (veq::*eps* 0.00001)
           (isects (sort-hits (veq:f2lsegx lines))))
      ; (declare (grph::simple-list isects edges) (grph::simple-array lines))
      (del-hit-edges edges isects)
      (add-new-paths edges isects)
      (values g pos))))

(defmacro 3cut-all! (g pos fx)
 `(mvb (g* pos*) (3cut-all ,g ,pos ,fx)
    (setf ,g g* ,pos pos*)))

(veq:fvdef* 2cut-to-area (g pos &optional (top 0f0) (lft 0f0) (rht 1000f0) (btm 1000f0))
  (declare (grph:grph g) (pos pos) (veq:ff top lft btm rht))
  "cut all edges at the envelope borders; remove anything outside."
  (labels
    ((inside (i) (veq:xlet ((f2!p (2@ pos i)))
                   (and (>= rht (:vr p 0) lft) (>= btm (:vr p 1) top))))
     (filter-edges (&aux (cands (make-hash-table :test #'equalp)))
       (loop for ee in (grph:@edges g) for (a b) = ee
             if (/= 2 (length (remove-if-not #'inside ee)))
             do (setf (gethash (grph::srt a b) cands) t))
       cands)
     (find-isects (&aux (cands (filter-edges)))
       (let* ((n (hash-table-count cands)) ; border lines: 0:top 1:rht 2:btm 3:lft
              (lines (make-array (+ 4 n)))
              (edges (make-array n)))
         (loop for i from 0 for ee being the hash-keys of cands
               do (setf (aref lines i) (2@verts pos ee)
                        (aref edges i) ee))
         (setf (aref lines n)       (veq:f2$ln lft top rht top)
               (aref lines (+ n 1)) (veq:f2$ln rht top rht btm)
               (aref lines (+ n 2)) (veq:f2$ln rht btm lft btm)
               (aref lines (+ n 3)) (veq:f2$ln lft btm lft top))
         (values (veq:f2ssegx lines n) edges)))

     (do-drop (ee) (grph:ldel! g ee) (grph:ldel! g (reverse ee)))

     (do-add-new1 (a v b &aux (p (grph:@prop g `(,a ,b))))
       (if (inside a) (grph:add! g a v p) (grph:add! g v b p))
       (grph:del! g a b))
     (do-add-new2 (a v w b)
       (grph:add! g v w (grph:@prop g (list a b))) (grph:del! g a b))

     (do-cut1 (ee hit) ; cut edges w/1 isect
       (veq:dsb (a b) ee
         (let ((v (xgrph:2vert! pos (veq:f2lerp (2@ pos a b) (cdr hit)))))
           (when (grph:@mem g a b) (do-add-new1 a v b))
           (when (grph:@mem g b a) (do-add-new1 b v a)))))
     (do-cut2 (ee hit) ; cut edges w/2 isects
        (veq:dsb (a b) ee ; a-v-w-b
         (let ((v (xgrph:2vert! pos (veq:f2lerp (2@ pos a b) (cdar hit))))
               (w (xgrph:2vert! pos (veq:f2lerp (2@ pos a b) (cdadr hit)))))
           (when (grph:@mem g a b) (do-add-new2 a v w b))
           (when (grph:@mem g b a) (do-add-new2 b w v a))))))

    (veq:mvb (isects edges) (find-isects)
      (loop for i from 0 repeat (- (length isects) 4)
            for hit = (aref isects i) for ee = (aref edges i) for hc = (length hit)
            do (case hc (0 (do-drop ee)) ; edge is outside envelope
                        (1 (do-cut1 ee (car hit))) ; edge isects envelope once
                        (2 (do-cut2 ee (sort hit #'< :key  #'cdr))) ; twice
                        (otherwise (warn "2cut-to-area: unexpected isects for: ~a" ee))))))
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
             (some (lambda (i) (funcall (the function sidefx)
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
             (grph:add! ^g (vert ea) (vert eb) (grph:@prop g `(,ea ,eb))))
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

