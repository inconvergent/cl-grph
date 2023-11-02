(in-package :grph)

(defmacro connected-verts (g &optional (p :_))
  (declare (symbol p)) "get all connected verts."
  (with-symbs `(g ,g)
  (if (any? p)
      `(qry g :select ?x :where (or (?x _ _) (_ _ ?x)))
      `(let ((?p ,p))
         (qry g :select ?x :in ?p :where (or (?x ?p _) (_ ?p ?x)))))))

(defun props-edges (g)
  (declare (grph g)) "list of lists of prop with flattend list of edges"
  (grph:qry g :select (?p (grp ?x ?y)) :where (?x ?p ?y)
              :collect (list ?p (veq::awf (grp ?x ?y)))))

; this is a fx because that makes it easier to use in queries
; make two different fxs for ?p / not ?p?
(defun num-either (g ?x &optional (?p :_))
  (declare (grph g) (pn ?x) (symbol ?p)) "number of adjacent vertices to ?x. ignores edge dir."
  (length (undup (if (any? ?p)
                     (qry g :in ?x :select ?y :where (or (?x _ ?y) (?y _ ?x)))
                     (qry g :in (?x ?p) :select ?y :where (or (?x ?p ?y) (?y ?p ?x)))))))


(defmacro edge-set (g &optional (p :_))
  (declare (symbol p)) "get edge set. ignores edge dir."
  (with-symbs `(g ,g)
  (if (any? p)
      `(qry g :select (?x ?y) :where (and (% (< ?x ?y)) (or (?x _ ?y) (?y _ ?x))))
      `(let ((?p ,p))
         (qry g :select (?x ?y) :in ?p :where (and (% (< ?x ?y)) (or (?x ?p ?y) (?y ?p ?x))))))))

(defmacro dead-ends (g &optional (p :_) y)
  (declare (symbol p) (boolean y))
  "vertices that have exactly one adjacent vertices: [g-] ?y-?x ignores edge dir."
   (with-symbs `(g ,g)
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
  "vertices that have exactly 2 adjacent vertex: [g-] ?y1-?x-?y2 [-g] ignores edge dir."
  (with-symbs `(g ,g)
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
  "vertices that do not have exactly 2 adjacent vertices. ie. the set of dead
ends and multi isects. ignores edge dir."
  (with-symbs `(g ,g)
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
  "vertices that have 3 or more adjacent vertices. ignores edge dir."
  (with-symbs `(g ,g)
  (if (any? p)
      `(qry g :select (?x ,(if y '?y))
              :where (and (or (?x _ ?y) (?y _ ?x)) (% (> (num-either g ?x) 2)))
              :collect ,(if y '(list ?x ?y) '?x))
      `(let ((?p ,p))
         (qry g :select (?x ,(if y '?y)) :in ?p
                :where (and (or (?x ?p ?y) (?y ?p ?x)) (% (> (num-either g ?x ?p) 2)))
                :collect ,(if y '(list ?x ?y) '?x))))))

(defun del-dead-ends (g &optional (p :_))
  "delete dead-ends until there are no more dead ends left. ignores edge dir."
  (labels ((-del (a b) (del! g a b) (del! g b a)))
    (loop for ee = (dead-ends g p t)
          while ee do (loop for (a b) in ee do (-del a b))))
  g)

(defun walk-edge-set (g es &aux (edges (edge-set->ht es)))
  (declare (grph g) (list es) (hash-table edges))
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
  (declare (grph g) (symbol p)) "walk graph via walk-edge-set."
  (walk-edge-set g (if (any? p) (edge-set g) (edge-set g p))))

