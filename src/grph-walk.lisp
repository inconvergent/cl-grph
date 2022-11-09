(in-package :grph)

; TODO: improve handing of (any ?p) in qry?
; TODO: macro fro (if any ...)?

; this is a fx because that makes it easier to use in queries
; make two different fxs for ?p / not ?p?
(defun num-either (g ?x &optional (?p :_))
  "number of edges to or from ?x (with propery ?p)."
  (declare (grph g) (symbol ?p))
  (length (undup (if (any? ?p)
                     (qry g :in ?x :select ?y
                       :where (or (?x _ ?y) (?y _ ?x))
                       :collect ?y)
                     (qry g :in (?x ?p) :select ?y
                       :where (or (?x ?p ?y) (?y ?p ?x))
                       :collect ?y)))))

(defun del-simple-filaments (g &optional (?p :_))
  (labels ((-del (a b) (del! g a b) (del! g b a))
           (-edges () (if (any? ?p)
                          (qry g :select (?b ?a)
                            :where (and (or (?b _ ?a) (?a _ ?b))
                                        (% (< (num-either g ?b) 2))))
                          (qry g :in ?p :select (?b ?a)
                            :where (and (or (?b ?p ?a) (?a ?p ?b))
                                        (% (< (num-either g ?b ?p) 2)))))))
    (loop for ee = (-edges)
          while ee do (loop for (a b) in ee do (-del a b))))
  g)

; TODO: adapt these to use ?p / not ?p
; (defmacro any-edge (g ?p)
;   (declare (symbol g ?p))
;   "all edges (?x ?p ?y)."
;   `(qry ,g :in ,?p :select (?x ?y)
;                    :where (and (or (?x ,?p ?y) (?y ,?p ?x)))
;                    :first (list ?x ?y)))

; (defmacro dead-ends (g p)
;   (declare (symbol g ?p))
;   "get all edges (?x ?p ?y) or (?y ?p ?x) where ?x is only connected to ?y."
;   `(qry ,g  :select (?x ?y)
;                    :where (and (or (?x ,p ?y) (?y ,p ?x))
;                                (% (< (num-either ,g ?x ,p) 2)))))

; (defmacro multi-isects (g ?p)
;   (declare (symbol g ?p))
;   "all edges (?x ?p ?y) or (?y ?p ?x) where ?x has  has more than two "
;   `(qry ,g :in ,?p :select (?x ?y)
;                     :where (and (or (?x ,?p ?y) (?y ,?p ?x))
;                                 (% (> (num-either ,g ?x ,?p) 2)))))

; (defmacro filament-ends (g ?p)
;   (declare (symbol g ?p))
;   `(qry ,g :in ,?p :select (?x ?y)
;                    :where (and (or (?x ,?p ?y) (?y ,?p ?x))
;                                (% (/= (num-either ,g ?x ,?p) 2))
;                                (% (/= ?x ?y)))))


(defun edges-ht (edges &aux (ht (make-hash-table :test #'equal)))
  (labels ((-srt (e) (if (apply #'< e) e (reverse e))))
    (loop for e in edges do (setf (gethash (-srt e) ht) t)))
  ht)

(defun walk-edge-set (g edge-set &aux (all-edges (edges-ht edge-set)))
  (declare (grph g) (list edge-set) (hash-table all-edges))
  "greedily walks the graph so that every edge is returned exactly once."
  (labels
    ((-srt (&rest e) (if (apply #'< e) e (reverse e)))
     (-get-start-edge ()
       (loop for e being the hash-keys of all-edges
             do (return-from -get-start-edge e)))
     (-next-vert-from (a &key but-not)
       (car (remove-if (lambda (v) (or (= v but-not)
                                       (not (gethash (-srt a v) all-edges))))
                       (@either g a))))
     (-until-dead-end (a but-not)
       (loop with prv = a
             with res = (list prv)
             with nxt = (-next-vert-from a :but-not but-not)
             until (equal nxt nil)
             do (push nxt res)
                (remhash (-srt prv nxt) all-edges)
                (let ((nxt* (-next-vert-from nxt :but-not prv)))
                  (setf prv nxt nxt nxt*))
             finally (return res)))
     (closed? (p) (if (equal (first p) (last* p))
                      (list (cdr p) t)
                      (list p nil))))
    (loop while (> (hash-table-count all-edges) 0)
          collect (closed?
                    (let ((start (-get-start-edge)))
                    (remhash start all-edges)
                    (destructuring-bind (a b) start
                      (concatenate 'list
                        (-until-dead-end a b)
                        (reverse (-until-dead-end b a)))))))))
(defmacro walk-grph (g &optional (p :_))
  (declare (symbol g p))
  `(let ((?p ,p))
     (declare (ignorable ?p))
     (walk-edge-set ,g
       (qry ,g :in ,(when (not (any? p)) '?p) :select (?x ?y)
         :where ,(if (any? p) '(or (?x _ ?y) (?y _ ?x))
                              '(or (?x ?p ?y) (?y ?p ?x)))))))

