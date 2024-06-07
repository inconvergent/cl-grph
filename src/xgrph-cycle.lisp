(in-package :xgrph)

; TODO: this is immature, needs improvements

(defmacro 2vfx (v) (awg (i) `(lambda (,i) (2@ ,v ,i))))
(veq:fvdef alpha (vfx a b) (declare (function vfx) (fixnum a b)) (veq:falpha (f2!@- (f@vfx b) (f@vfx a))))
(defun -get-edges (g ?p) (declare (grph g) (keyword ?p))
  (case ?p (:_ (grph:@edges g)) (t (grph:qry g :select (?x ?y) :in ?p :where (?x ?p ?y)))))

(defun adj->verts (adj)
  (declare (hash-table adj))
  (loop for k of-type fixnum being the hash-keys of adj collect k))

(defun adj-del (adj a b &aux (to (remove-if (lambda (k) (= b k)) (gethash a adj))))
  (declare (hash-table adj) (fixnum a b))
  (if to (setf (gethash a adj) to) (remhash a adj)))

(defun adj-do-strip-filament (adj k &aux (near (gethash k adj)))
  (declare (hash-table adj) (fixnum k) (list near))
  (when (= (length near) 1) (let ((kk (car near)))
                              (adj-del adj k kk) (adj-del adj kk k)
                              (adj-do-strip-filament adj kk))))
(defun adj-strip-filaments (adj cycle)
  (declare (hash-table adj) (list cycle))
  (loop for k in cycle do (adj-do-strip-filament adj k)))

(veq:fvdef adj->left-most-vert (vfx vv &aux (c (car vv)))
  (declare (list vv) (function vfx) (fixnum c))
  (veq:xlet ((f2!mm (f@vfx (car vv))))
    (loop for i in (cdr vv)
          do (veq:xlet ((f2!pp (f@vfx i)))
               (when (or (< (:vr pp 0) (:vr mm 0))
                         (and (<= (:vr pp 0) (:vr mm 0))
                              (< (:vr pp 1) (:vr mm 1))))
                 (setf c i (veq:f2 mm) (veq:f2 pp))))))
  c)

(defun es->adj-ht (es &optional (adj (make-hash-table :test #'equal)))
  (declare (list es) (hash-table adj))
  (labels ((add (a b) (setf (gethash a adj) (cons b (gethash a adj (list))))))
    (loop for (a b) in es
          if (= a b) do (warn "edge-set->adj-ht: invalid edge: ~a ~a" a b)
          else do (add a b) (add b a)))
  adj)

(veq:fvdef adj-find-outline (vfx adj)
  (declare (function vfx) (hash-table adj))
  (let* ((v (adj->left-most-vert vfx (adj->verts adj))) (res (list v)))
    (labels
        ((get-angles (v ca near)
           (sort (cons (list nil ca)
                       (loop for w in near for a = (alpha vfx v w)
                             if (not (veq:feps= ca a)) collect `(,w ,a)))
                 #'< :key #'second))
         (find-nxt-angle (aa) (declare (list aa))
           (loop for i from 0 for (w na) in aa
                 if (not w) do (return-from find-nxt-angle
                                 (car (nth (mod (1+ i) (length aa)) aa)))) -1)
         (pop-edge-yield (v w) (declare (fixnum v w))
           (adj-del adj v w) (adj-del adj w v) `(,v ,w ,(alpha vfx v w)))
         (pop-edge (v ca &aux (near (gethash v adj)) (n (length near)))
           (cond ((= n 1) (pop-edge-yield v (first near)))
                 ((not (> n 0)) (warn "OUTLINE FAILED. ~a ~a. incomplete path (len: ~a):~a~%"
                                      v (+ veq:fpi ca) (length res) res)
                                (return-from adj-find-outline (values res nil)))
                 (t (pop-edge-yield v (find-nxt-angle (get-angles v ca near)))))))
       (loop named lp with curv = v with cura = veq:fpi5
             for (nxtv nxta) = (cdr (pop-edge curv cura))
             if (= nxtv v) do (return-from lp)
             else do (push nxtv res)
                     (setf cura (mod (+ veq:fpi nxta) veq:fpii)
                           curv nxtv))
       (values res t))))
(defun find-outline (g vfx &optional (?p :_))
  (declare (grph g) (function vfx) (keyword ?p))
  "example: (xgrph:find-find-outline g (xgrph:2vfx v) :arch/is)"
  (adj-find-outline vfx (es->adj-ht (-get-edges g ?p))))

(veq:fvdef adj-find-cycle-basis (vfx adj
    &aux (res (list)) (visited (make-hash-table :test #'equal)))
  (declare (function vfx) (hash-table adj))
  (labels
      ((get-angles (v ca ww &optional (order #'<))
         (sort (cons (list nil ca) (mapcar (lambda (w) `(,w ,(alpha vfx v w))) ww))
                order :key #'second))
       (find-nxt-angle (dir aa)
         (loop for i from 0 for (w na) in aa
               if (not w) do (return-from find-nxt-angle
                               (car (nth (mod (ecase dir (:ccw (1+ i)) (:cw (1- i)))
                                              (length aa))
                                         aa)))) -1)
       (edge-alpha (v w) `(,v ,w ,(alpha vfx v w)))
       (pop-edge (prev v ca dir &aux (near (remove-if (lambda (k) (equal k prev))
                                                      (gethash v adj)))
                                     (n (length near)))
         (cond ((not (> n 0)) (error "CYCLE BASIS: MISSING: ~a" v))
               ((= n 1) (edge-alpha v (first near)))
               (t (edge-alpha v (find-nxt-angle dir (get-angles v ca near))))))
       (not-visited (a b &aux (key (if (< a b) (list a b) (list b a))))
          (if (gethash key visited) nil (setf (gethash key visited) t)))
       (find-nxt-cycle (k &aux (curv k) (cura veq:fpi) (cycle (list curv)))
         (clrhash visited)
         (handler-case
           (veq:dsb (nxtv nxta) (cdr (pop-edge -1 curv cura :cw))
             (loop until (= nxtv k)
                   do (push nxtv cycle)
                      (unless (not-visited (first cycle) (second cycle))
                              (return-from find-nxt-cycle (progn (warn "bad cycle: ~a" cycle)
                                                                 (values cycle nil))))
                      (setf cura (mod (+ veq:fpi nxta) veq:fpii) curv nxtv)
                      (veq:dsb (nxtv* nxta*) (cdr (pop-edge (second cycle) curv cura :ccw))
                               (setf nxtv nxtv* nxta nxta*))
                   finally (return-from find-nxt-cycle
                             (values (reverse cycle)
                                     (not-visited (first cycle) (grph:last* cycle))))))
           (error (e) (warn "CYCLE BASIS:~%~a~%~a" cycle e)
                      (values cycle nil))))
       (do-next-cycle (k) (declare (fixnum k))
         (veq:mvb (cycle ok) (find-nxt-cycle k)
           (veq:dsb (a b) (subseq cycle 0 2) (adj-del adj a b) (adj-del adj b a))
           (adj-strip-filaments adj cycle)
           (when ok (push cycle res)))))
    (loop with maxnum = (hash-table-count adj)
          while (> (hash-table-count adj) 2) repeat maxnum
          do (do-next-cycle (adj->left-most-vert vfx (adj->verts adj))))
    res))
(defun find-cycle-basis (g vfx &optional (?p :_))
  (declare (grph g) (function vfx) (keyword ?p))
  "example: (xgrph:find-cycle-basis g (xgrph:2vfx v) :arch/is)"
  (adj-find-cycle-basis vfx (es->adj-ht (-get-edges g ?p))))

