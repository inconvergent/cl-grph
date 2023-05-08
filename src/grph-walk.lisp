(in-package :grph)

; TODO: improve handing of (any ?p) in qry?
; TODO: macro fro (if any ...)?

; this is a fx because that makes it easier to use in queries
; make two different fxs for ?p / not ?p?
(defun num-either (g ?x &optional (?p :_))
  "number of adjacent vertices to ?x. ignores edge dir."
  (declare (grph g) (pn ?x) (symbol ?p))
  (length (undup (if (any? ?p)
                     (qry g :in ?x :select ?y :where (or (?x _ ?y) (?y _ ?x)))
                     (qry g :in (?x ?p) :select ?y :where (or (?x ?p ?y) (?y ?p ?x)))))))

(defmacro edge-set (g &optional (p :_))
  (declare (symbol g p))
  "get edge set. ignores edge dir."
  (if (any? p)
   `(qry ,g :select (?x ?y) :where (and (% (< ?x ?y)) (or (?x _ ?y) (?y _ ?x))))
   `(let ((?p ,p))
      (qry ,g :select (?x ?y) :in ?p :where (and (% (< ?x ?y)) (or (?x ?p ?y) (?y ?p ?x)))))))

(defmacro dead-ends (g &optional (p :_) y)
  (declare (symbol g p) (boolean y))
  "vertices that have exactly one adjacent vertices: [g-] ?y-?x
ignores edge dir."
  (if (any? p)
   `(qry ,g :select (?x ,(if y '?y))
            :where (and (or (?x _ ?y) (?y _ ?x)) (% (= (num-either ,g ?x) 1)))
            :collect ,(if y '(list ?x ?y) '?x))
   `(let ((?p ,p))
      (qry ,g :select (?x ,(if y '?y)) :in ?p
              :where (and (or (?x ?p ?y) (?y ?p ?x)) (% (= (num-either ,g ?x ?p) 1)))
              :collect ,(if y '(list ?x ?y) '?x)))))

(defmacro two-isects (g &optional (p :_) y)
  (declare (symbol g p) (boolean y))
  "vertices that have exactly 2 adjacent vertex: [g-] ?y1-?x-?y2 [-g]
 ignores edge dir.
  "
  (if (any? p)
   `(qry ,g :select (?x ,(if y '?y))
            :where (and (or (?x _ ?y) (?y _ ?x)) (% (= (num-either ,g ?x) 2)))
            :collect ,(if y '(list ?x ?y) '?x))
   `(let ((?p ,p))
      (qry ,g :select (?x ,(if y '?y)) :in ?p
              :where (and (or (?x ?p ?y) (?y ?p ?x)) (% (= (num-either ,g ?x ?p) 2)))
              :collect ,(if y '(list ?x ?y) '?x)))))

(defmacro segment-isects (g &optional (p :_) y)
  (declare (symbol g p) (boolean y))
  "vertices that do not have exactly 2 adjacent vertices. ie. the set of dead ends and multi isects.
ignores edge dir."
  (if (any? p)
   `(qry ,g :select (?x ,(if y '?y))
            :where (and (or (?x _ ?y) (?y _ ?x)) (% (/= (num-either ,g ?x) 2)))
            :collect ,(if y '(list ?x ?y) '?x))
   `(let ((?p ,p))
      (qry ,g :select (?x ,(if y '?y)) :in ?p
              :where (and (or (?x ?p ?y) (?y ?p ?x)) (% (/= (num-either ,g ?x ?p) 2)))
              :collect ,(if y '(list ?x ?y) '?x)))))

(defmacro multi-isects (g &optional (p :_) y)
  (declare (symbol g p) (boolean y))
  "vertices that have 3 or more adjacent vertices. ignores edge dir."
  (if (any? p)
   `(qry ,g :select (?x ,(if y '?y))
            :where (and (or (?x _ ?y) (?y _ ?x)) (% (> (num-either ,g ?x) 2)))
            :collect ,(if y '(list ?x ?y) '?x))
   `(let ((?p ,p))
      (qry ,g :select (?x ,(if y '?y)) :in ?p
              :where (and (or (?x ?p ?y) (?y ?p ?x)) (% (> (num-either ,g ?x ?p) 2)))
              :collect ,(if y '(list ?x ?y) '?x)))))

(defun del-dead-ends (g &optional (p :_))
  "delete dead-ends until there are no more dead ends left. ignores edge dir."
  (labels ((-del (a b) (del! g a b) (del! g b a)))
    (loop for ee = (dead-ends g p)
          while ee do (loop for (a b) in ee do (-del a b))))
  g)

(defun edges-ht (es &aux (ht (make-hash-table :test #'equal)))
  "convert edge set to hash table."
  (labels ((-srt (e) (if (apply #'< e) e (reverse e))))
    (loop for e in es do (setf (gethash (-srt e) ht) t)))
  ht)

; TODO: start in multi isects
(defun walk-edge-set (g es &aux (edges (edges-ht es)))
  (declare (grph g) (list es) (hash-table edges))
  "greedily walk the graph and return every edge exactly once. ignores edge dir."
  (labels
    ((-srt (&rest e) (if (apply #'< e) e (reverse e)))
     (-get-start-edge ()
       (loop for e being the hash-keys of edges
             do (return-from -get-start-edge e)))
     (-next-vert-from (a &key but-not)
       (car (remove-if (lambda (v) (or (= v but-not)
                                       (not (gethash (-srt a v) edges))))
                       (@either g a))))
     (closed? (p) (if (equal (first p) (last* p)) (list (cdr p) t)
                                                  (list p nil)))
     (-until-dead-end (a but-not)
       (loop with prv = a
             with res = (list prv)
             with nxt = (-next-vert-from a :but-not but-not)
             until (equal nxt nil)
             do (push nxt res)
                (remhash (-srt prv nxt) edges)
                (let ((nxt* (-next-vert-from nxt :but-not prv)))
                  (setf prv nxt nxt nxt*))
             finally (return res))))
    (loop while (> (hash-table-count edges) 0)
          for start = (-get-start-edge)
          for (a b) = start
          for path = (progn (remhash start edges)
                            (concatenate 'list (-until-dead-end a b)
                                               (reverse (-until-dead-end b a))))
          collect (closed? path))))

(defun walk-grph (g &optional (p :_))
  (declare (grph g) (symbol p))
  "walk graph via walk-edge-set."
  (walk-edge-set g (if (any? p) (edge-set g) (edge-set g p))))

