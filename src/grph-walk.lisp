(in-package :grph)

(defmacro connected-verts (g &optional (p :_))
  (declare (symbol p)) "get all connected verts."
  (veq:with-symbs `(g ,g)
  (if (any? p) `(qry g :select ?x :where (or (?x _ _) (_ _ ?x)))
               `(let ((?p ,p)) (qry g :select ?x :in ?p :where (or (?x ?p _) (_ ?p ?x)))))))

(defun props-edges (g)
  (declare #.*opt* (grph g)) "list of lists of prop with flattend list of edges"
  (labels ((flat (edges) (loop for e of-type list in edges nconc e)))
    (grph:qry g :select (?p (grp ?x ?y)) :where (?x ?p ?y)
                :collect (list ?p (flat (grp ?x ?y))))))

; this is a fx because that makes it easier to use in queries
(defun num-either (g ?x &optional (?p :_)) ; TODO: make two different fxs for ?p / not ?p?
  (declare #.*opt* (grph g) (pn ?x) (symbol ?p))
  "number of adjacent verts to ?x. ignores edge dir."
  (length (undup (if (any? ?p)
                     (qry g :in ?x :select ?y :where (or (?x _ ?y) (?y _ ?x)))
                     (qry g :in (?x ?p) :select ?y :where (or (?x ?p ?y) (?y ?p ?x)))))))


(defmacro edge-set (g &optional (p :_))
  (declare (symbol p)) "get edge set. ignores edge dir."
  (veq:with-symbs `(g ,g)
  (if (any? p)
      `(qry g :select (?x ?y) :where (and (% (< ?x ?y)) (or (?x _ ?y) (?y _ ?x))))
      `(let ((?p ,p))
         (qry g :select (?x ?y) :in ?p :where (and (% (< ?x ?y)) (or (?x ?p ?y) (?y ?p ?x))))))))

(defmacro dead-ends (g &optional (p :_) y)
  (declare (symbol p) (boolean y))
  "verts that have exactly one adjacent verts: [g-] ?y-?x ignores edge dir."
   (veq:with-symbs `(g ,g)
   (if (any? p)
       `(qry g :select (?x ,(if y '?y))
               :where (and (or (?x _ ?y) (?y _ ?x)) (% (= (num-either g ?x) 1)))
               :collect ,(if y '(list ?x ?y) '?x))
       `(let ((?p ,p))
          (qry g :select (?x ,(if y '?y)) :in ?p
                 :where (and (or (?x ?p ?y) (?y ?p ?x)) (% (= (num-either g ?x ?p) 1)))
                 :collect ,(if y '(list ?x ?y) '?x))))))

(defmacro two-isects (g &optional (p :_) y)
  (declare (symbol p) (boolean y))
  "verts that have exactly 2 adjacent verts [g-] ?y1-?x-?y2 [-g] ignores edge dir."
  (veq:with-symbs `(g ,g)
  (if (any? p)
      `(qry g :select (?x ,(if y '?y))
              :where (and (or (?x _ ?y) (?y _ ?x)) (% (= (num-either g ?x) 2)))
              :collect ,(if y '(list ?x ?y) '?x))
      `(let ((?p ,p))
         (qry g :select (?x ,(if y '?y)) :in ?p
                :where (and (or (?x ?p ?y) (?y ?p ?x)) (% (= (num-either g ?x ?p) 2)))
                :collect ,(if y '(list ?x ?y) '?x))))))

(defmacro segment-isects (g &optional (p :_) y)
  (declare (symbol p) (boolean y))
  "verts that do not have exactly 2 adjacent verts. ie. the set of dead
ends and multi isects. ignores edge dir."
  (veq:with-symbs `(g ,g)
  (if (any? p)
      `(qry g :select (?x ,(if y '?y))
              :where (and (or (?x _ ?y) (?y _ ?x)) (% (/= (num-either g ?x) 2)))
              :collect ,(if y '(list ?x ?y) '?x))
      `(let ((?p ,p))
         (qry g :select (?x ,(if y '?y)) :in ?p
                :where (and (or (?x ?p ?y) (?y ?p ?x)) (% (/= (num-either g ?x ?p) 2)))
                :collect ,(if y '(list ?x ?y) '?x))))))

(defmacro multi-isects (g &optional (p :_) y)
  (declare (symbol p) (boolean y))
  "verts that have 3 or more adjacent verts. ignores edge dir."
  (veq:with-symbs `(g ,g)
  (if (any? p)
      `(qry g :select (?x ,(if y '?y))
              :where (and (or (?x _ ?y) (?y _ ?x)) (% (> (num-either g ?x) 2)))
              :collect ,(if y '(list ?x ?y) '?x))
      `(let ((?p ,p))
         (qry g :select (?x ,(if y '?y)) :in ?p
                :where (and (or (?x ?p ?y) (?y ?p ?x)) (% (> (num-either g ?x ?p) 2)))
                :collect ,(if y '(list ?x ?y) '?x))))))

(defun del-dead-ends (g &optional (p :_))
  (declare #.*opt* (grph g) (symbol p))
  "delete dead-ends until there are no more dead ends left. ignores edge dir."
  (labels ((-del (a b) (del! g a b) (del! g b a)))
    (loop for ee = (dead-ends g p t)
          while ee do (loop for (a b) in ee do (-del a b))))
  g)

(defun walk-edge-set (g es &aux (edges (edge-set->ht es)))
  (declare #.*opt* (grph g) (list es) (hash-table edges))
  "return a list of paths ((p1 closed?) (p2 closed?) ...) from edge set from g.
every edge is included exactly once. ignores edge dir."
  (labels
    ((-srt (&rest e) (if (apply #'< e) e (reverse e)))
     (-get-start-edge ()
       (loop for e being the hash-keys of edges
             do (return-from -get-start-edge e)))
     (-next-vert-from (a &key but-not)
       (car (remove-if (lambda (v) (or (= v but-not) (not (gethash (-srt a v) edges))))
                       (@either g a)))) ; find new edges in g also (untraversed) in es
     (closed? (p) (if (equal (first p) (last* p)) `(,(cdr p) t) `(,p nil)))
     (-until-dead-end (a but-not)
       (loop with prv = a with res = (list prv)
             with nxt = (-next-vert-from a :but-not but-not)
             until (equal nxt nil)
             do (push nxt res)
                (remhash (-srt prv nxt) edges)
                (let ((nxt* (-next-vert-from nxt :but-not prv)))
                  (setf prv nxt nxt nxt*))
             finally (return res))))
    (loop while (> (hash-table-count edges) 0)
          for start = (-get-start-edge) for (a b) = start
          for path = (progn (remhash start edges)
                            `(,@(-until-dead-end a b)
                              ,@(reverse (-until-dead-end b a))))
          collect (closed? path))))
(defun walk-grph (g &optional (p :_))
  (declare #.*opt* (grph g) (symbol p)) "walk graph via walk-edge-set."
  (walk-edge-set g (if (any? p) (edge-set g) (edge-set g p))))

; this is quite ineffiecient. rewrite a version of walk-edge-set instead?
(defun walk-edge-set-segments (g es &aux (edges (edge-set->ht es)))
  (declare #.*opt* (grph g) (list es)) "walk edge set and split into segments."
  (labels ((srt (&rest e) (if (apply #'< e) e (reverse e)))
           (rec (pp) (unless (> (length pp) 1) (return-from rec))
                     (loop for i from 1 for (vi b) in (cdr pp)
                           if (not b) do (return-from rec
                                           (cons (veq:lpos (subseq pp 0 (1+ i)))
                                                 (rec (subseq pp i)))))
                     (list (veq:lpos pp)))
           (2cnt (v) (= 2 (loop for w in (@either g v)
                                if (gethash (srt w v) edges) summing 1)))
           (with-2cnt (p) (loop for v in p collect (list v (2cnt v))))
           (split-paths (p c) (rec (print (with-2cnt (if c (close-path p) p)))))
           (closed? (p) (if (= (first p) (last* p))
                            (list (butlast p) t) (list p nil))))
   (loop with res = (list)
         for (p c) in (grph:walk-edge-set g es)
         do (loop for p in (split-paths p c) do (push (closed? p) res))
         finally (return res))))
(defun walk-grph-segments (g &optional (p :_))
  (declare #.*opt* (grph g) (symbol p)) "walk graph via walk-edge-set-segments."
  (walk-edge-set-segments g (if (any? p) (edge-set g) (edge-set g p))))

