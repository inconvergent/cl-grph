(in-package :xgrph)
; TODO: sloppy port from weird. this can be improved

(deftype simple-list () `(simple-array list))

(veq:fvdef 2intersect-all (g pos)
  (declare (grph:grph g))
  "creates intersections for all edges in g such that it becomes a planar graph."

  (let ((crossing->vert (make-hash-table :test #'equal)))
    (declare (hash-table crossing->vert))
    (labels
      ((ic (i c) (declare (fixnum i c)) (if (< i c) (list i c) (list c i)))
       (-add (a b &key e)
         (declare (grph:pn a b) (list e))
         (add! g a b (grph::props-as-list (grph:@prop g e))))
       (edges-as-lines (edges)
         (declare (simple-list edges))
         (loop for edge of-type list across edges
               collect (2@verts pos edge)))
       (sort-hits (isects)
         (declare (simple-list isects))
         (loop for i of-type fixnum from 0 below (length isects)
               if (aref isects i)
               do (setf (aref isects i)
                        (sort (aref isects i) #'< :key #'cdr)))
         isects)
       (add-vert (line i hits)
         (declare (list hits) (fixnum i))
         (loop for (c . p) in hits
               if (not (gethash (the list (ic i c)) crossing->vert))
               do (let ((new (2vert! pos (veq:f2lerp (veq:f2$ line 0 1) p))))
                    (declare (grph:pn new))
                    (setf (gethash (the list (ic i c)) crossing->vert) new))))
       (add-new-verts (edges isects)
         (declare (simple-list edges isects))
         (loop for hits across isects
               for i of-type fixnum from 0
               if hits
               do (add-vert (2@verts pos (aref edges i)) i hits)))
       (del-hit-edges (edges isects)
         (declare (simple-list edges isects))
         (loop for hits of-type list across isects
               for i of-type fixnum from 0
               if hits do (grph::ldel! g (aref edges i))
                          (loop for (c . p) in hits
                                do (grph::ldel! g (aref edges c)))))
       (add-new-edges (edges isects)
         (declare (simple-list edges isects))
         (loop for hits of-type list across isects
               for i of-type fixnum from 0
               if hits
               do (loop with cc = (grph::lpos hits)
                        with ei = (aref edges i)
                        for a of-type fixnum in cc
                        and b of-type fixnum in (cdr cc)
                        initially
                          (-add (gethash (ic i (first cc)) crossing->vert)
                                (first ei) :e ei)
                          (-add (gethash (ic i (grph::last* cc)) crossing->vert)
                                (grph::last* ei) :e ei)
                        do (-add (gethash (ic i a) crossing->vert)
                                 (gethash (ic i b) crossing->vert)
                                 :e ei)))))
             ; edges ((v1 v2) (v8 v1) ...)
      (let* ((edges (grph:to-vector (grph:@edges g)))
             ; lines: (#(ax ay bx by) #(cx cy dx dy) ...)
             (lines (grph:to-vector (edges-as-lines edges)))
             (veq::*eps* 0.00001)
             ; isects: #(((16 . 0.18584675) (5 . 0.35215548)) NIL NIL ...)
             (isects (sort-hits (veq:f2lsegx lines)))) ;  p/q is the lerp
        (declare (simple-list isects edges) (simple-array lines))
        (add-new-verts edges isects)
        (add-new-edges edges isects)
        (del-hit-edges edges isects)
        (values g pos)))))
(defmacro 2intersect-all! (g pos &rest rest)
 `(grph:mvb (g* pos*) (xgrph::2intersect-all ,g ,pos ,@rest)
    (setf ,g g* ,pos pos*)))


(veq:fvdef* 2cut-to-area (g pos &optional (top 0f0) (lft 0f0)
                                          (rht 1000f0) (bot 1000f0))
  (declare (grph:grph g) (veq:ff top lft bot rht))
  "removes all edges outside envelope.
all edges intersecting the envelope will be deleted, a new vert will be
inserted on the intersection; connected to the inside vert."
  (labels
    ((inside (i)
      (declare (grph::pn i))
      (veq:f2let ((p (2@ pos i)))
        (and (> (:vr p 0) lft) (> (:vr p 1) top)
             (< (:vr p 0) rht) (< (:vr p 1) bot))))
     (split-line (ai bi &aux (rev nil))
       (declare (grph::pn ai bi) (boolean rev))
       (unless (inside ai) (rotatef ai bi) (setf rev t))
       (veq:f2let ((a (2@ pos ai))
                   (b (2@ pos bi))
                   (ab (veq:f2- b a)))
         (mvc #'values rev ; there appears to be a slight bug here for lines
           (veq:f2lerp a b ; that cross both a side and a top/bot
             (cond ((> (:vr b 0) rht) (/ (- rht (:vr a 0)) (:vr ab 0)))
                   ((> (:vr b 1) bot) (/ (- bot (:vr a 1)) (:vr ab 1)))
                   ((< (:vr b 0) lft) (/ (- lft (:vr a 0)) (:vr ab 0)))
                   (t (/ (- top (:vr a 1)) (:vr ab 1))))))))
     (cutfx (&rest line)
       (declare (list line))
       (case (length (remove-if-not #'inside line))
         (0 (values :outside nil 0f0 0f0))
         (1 (mvc #'values :split (apply #'split-line line)))
         (t (values :keep nil 0f0 0f0)))))

  (grph:qry g :select (?x ?y)
    :using (^g ^pos)
    :where (or (?x _ ?y))
    :then (mvb (state rev (:va 2 px)) (cutfx ?x ?y)
            (ecase state
              (:keep nil)
              (:outside (grph:del! ^g ?x ?y))
              (:split (let ((props (grph::props-as-list
                                     (grph:@prop g (list ?x ?y)))))
                        (grph:del! ^g ?x ?y)
                        (2append! ^g ^pos (if rev ?y ?x)
                          (veq:f2 px) abs props)))))))
  (values g pos))
(defmacro 2cut-to-area! (g pos &rest rest)
 `(grph:mvb (g* pos*) (2cut-to-area ,g ,pos ,@rest)
    (setf ,g g* ,pos pos*)))

