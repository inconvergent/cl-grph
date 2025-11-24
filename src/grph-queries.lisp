(in-package :grph)


(defun props-edges (g)
  (declare #.*opt* (grph g))
  "list of lists of prop with flattend list of edges. see ingest-props-edges"
  (labels ((flat (edges) (loop for e of-type list in edges nconc e)))
    (qry g :select (?p (grp ?x ?y)) :where (?x ?p ?y)
           :collect (list ?p (flat (grp ?x ?y))))))

(defun ingest-edges (edges &optional (g (grph)))
  (declare (list edges) (grph g))
  "ingest a list of edges with props. eg: ((0 :a 3) ...). and return a grph."
  (modify! (g in)
    (loop for (l p r) in edges if (any? p) do (in-> l r)
                               else do (in-> l r `(,(kv p)))))
  g)

(defun ingest-props-edges (pedges &optional (g (grph)))
  (declare (list pedges) (grph g))
  "ingest list of props and flattened edges. see props-edges."
   (modify! (g in)
     (loop for (p edges) in pedges for edges* = (group edges 2)
           if (any? p) do (loop for (a b) in edges* do (in-> a b))
           else do (loop for (a b) in edges* do (in-> a b `(,p)))))
   g)

(defmacro connected-verts (g &optional (p :_))
  (declare (symbol p)) "get all connected verts."
  (veq:with-symbs `(g ,g)
  (if (any? p) `(qry g :select ?x :where (or (?x _ _) (_ _ ?x)))
               `(let ((?p ,p)) (qry g :select ?x :in ?p
                                      :where (or (?x ?p _) (_ ?p ?x)))))))

; this is a fx because that makes it easier to use in queries
(defun num-either (g ?x &optional (?p :_))
  (declare #.*opt* (grph g) (in ?x) (symbol ?p))
  "number of adjacent verts to ?x. ignores edge dir."
  (length (undup (if (any? ?p)
                     (qry g :select ?y :in ?x :where (or (?x _ ?y) (?y _ ?x)))
                     (qry g :select ?y :in (?x ?p)
                            :where (or (?x ?p ?y) (?y ?p ?x)))))))

(defmacro edge-set (g &optional (p :_))
  (declare (symbol p)) "get [normalized] edge set. ignores edge dir."
  (veq:with-symbs `(g ,g)
  (if (any? p)
      `(qry g :select (?x ?y) :where (and (or (?x _ ?y) (?y _ ?x))
                                          (% (< ?x ?y))))
      `(let ((?p ,p))
         (qry g :select (?x ?y) :in ?p
                :where (and (or (?x ?p ?y) (?y ?p ?x))
                            (% (< ?x ?y))))))))

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


(defun normalize-edges (g &optional (mode :><))
  (declare #.*opt* (grph g) (keyword mode))
  "remove bi-directional edges, preserve properties.

->: normalize by removing ba if b > a.
    ensure that ab exists. and inherits props from ba
><: only normalize (to ab) if both ab and ba exists.
    otherwise preserve edge direction."
  (qry g :using ^g :select (?x ?y)
         :where (and (?x _ ?y) (% (< (the fixnum ?y)
                                     (the fixnum ?x))))
         :then (labels ((norm () (del! ^g ?x ?y)                          ; del (9 1)
                                 (add! ^g ?y ?x (@prop g (list ?x ?y))))) ; add (1 9)
                  (ecase mode (:>< (when (@mem g ?y ?x) (norm)))
                              (:-> (norm)))))

  g)
(defmacro normalize-edges! (g &optional (mode :><))
  (declare (symbol g))
  "normalize with normalize-edges (see this)."
  `(setf ,g (normalize-edges ,g ,mode)))

(defmacro collect-while ((&key (init '(list)) (test 'not) (lim 1000) ; TODO: example
                               (cres (gensym "CRES")) (citr (gensym "CITR")))
                          &body body)
  (declare (symbol cres))
  (awg (for-res lp)
    `(macrolet ((cstop (&body body) `(return-from ,',lp (progn ,@body))))
       (loop named ,lp
             with ,cres of-type list = ,init
             for ,for-res = (progn ,@body)
             for ,citr of-type pn from 0 below (the pn ,lim)
             until (,test ,for-res)
             if ,for-res do (push ,for-res ,cres)
             finally (return-from ,lp (reverse ,cres))))))

; TODO: this is really confusing to use. change? make example?
(defmacro qry-collect-while (g &rest rest)
  (declare (symbol g))
  "(let ((?a 2) (?b 1))
  (grph:qry-collect-while g
     :init (list ?a ?b) :in ?b
     :select ?n :where (and (or (?b _ ?n) (?n _ ?b))
                            (% (not (member ?n cres))))
     :first (progn (setf ?b ?n) ?n)
     :cres cres))"
  `(collect-while (:init ,(veq:get-arg-key rest :init '(list))
                   :lim  ,(veq:get-arg-key rest :lim  1000) ; RENAME clim?
                   :cres ,(veq:get-arg-key rest :cres (gensym "CRES"))
                   :citr ,(veq:get-arg-key rest :citr (gensym "CITR")))
    (qry ,g ,@(veq:strip-arg-keys rest '(:init :lim :cres :citr)))))

