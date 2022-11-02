(in-package :grph)

(defun num-either (g ?p ?x)
  "number of edges to or from ?x (with propery ?p)."
  (length (undup (qry g :in (?x ?p)
                        :select ?y
                        :where (or (?x ?p ?y) (?y ?p ?x))
                        :collect ?y))))

(defmacro any-edge (g ?p)
  (declare (symbol g ?p))
  "all edges (?x ?p ?y)."
  `(qry ,g :in ,?p :select (?x ?y)
                   :where (and (or (?x ,?p ?y) (?y ,?p ?x)))
                   :first (list ?x ?y)))

(defmacro dead-ends (g ?p)
  (declare (symbol g ?p))
  "get all edges (?x ?p ?y) or (?y ?p ?x) where ?x is only connected to ?y."
  `(qry ,g :in ,?p :select (?x ?y)
                   :where (and (or (?x ,?p ?y) (?y ,?p ?x))
                               (% (< (num-either ,g ,?p ?x) 2)))))

; (defmacro multi-isects (g ?p)
;   (declare (symbol g ?p))
;   "all edges (?x ?p ?y) or (?y ?p ?x) where ?x has  has more than two "
;   `(qry ,g :in ,?p :select (?x ?y)
;                     :where (and (or (?x ,?p ?y) (?y ,?p ?x))
;                                 (% (> (num-either ,g ,?p ?x) 2)))))

(defmacro filament-ends (g ?p)
  (declare (symbol g ?p))
  `(qry ,g :in ,?p :select (?x ?y)
                   :where (and (or (?x ,?p ?y) (?y ,?p ?x))
                               (% (/= (num-either ,g ,?p ?x) 2))
                               (% (/= ?x ?y)))))

(defmacro walk-grph-from (g a ?p &key (lim 10000))
  (declare (symbol g ?p))
  ; TODO: is this accurate?
  "walk from a along props ?p until self isect or dead end."
  (awg (res c blk)
    `(block ,blk
       (let ((?a ,a))
         (when (not ?a) (return-from ,blk nil))
         (let ((,res (qry-collect-while ,g
                       :in (?a ,?p) :lim ,lim :init (list ?a) :cres ,c
                       :select (?n)
                       :where (and (or (?a ,?p ?n) (?n ,?p ?a))
                                   (% (not (member ?n ,c)))
                                   (% (/= ?a ?n)))
                       :first (progn (setf ?a ?n)
                                     (if (< (num-either ,g ,?p ?n) 3) ?n
                                         (cstop (cons ?n ,c)))))))
           (declare (list ,res))
           (if (> (length ,res) 1) ,res nil))))))


(defun close? (g path prop &aux (a (car path)) (b (last* path)))
  (declare (list path) (pn a b) (symbol prop))
  "is there an edge (with prop) betweent the first and last or last and first
item in path?"
  (and (> (length path) 2)
       (or (@prop g (list a b) prop)
           (@prop g (list b a) prop))))

(defmacro walk-grph (g* prop &key (lim 100000))
  (declare (symbol g*) (pn lim))
  "walk the grph and return tuples: ((path closed?) ...)"
  (awg (g a b p res)
    `(let ((,g ,g*) ; avoid side effects on g* outside
           (?p ,prop)
           (,res (list)))
       (declare (grph ,g) (list ,res) (keyword ?p))
       (labels
         ((pthdel (,p)
            (declare (list ,p))
            (map 'nil (lambda (,a ,b) (del! ,g ,a ,b) (del! ,g ,b ,a))
                 ,p (cdr ,p)))
          (do-close (,p)
            (push (list ,p t) ,res)
            (pthdel (cons (last* ,p) ,p)))
          (do-open (,p) (push (list ,p nil) ,res) (pthdel ,p)))
         (loop repeat ,lim ; handle filaments
               for ,p = (walk-grph-from ,g
                          (caar (filament-ends ,g ?p))
                          ?p :lim ,lim)
               while ,p if (close? ,g ,p ,prop)
               do (do-close ,p) else do (do-open ,p))
         (loop repeat ,lim ; handle remaining loops
               for ,p = (walk-grph-from ,g
                          (car (any-edge ,g ?p))
                          ?p :lim ,lim)
               while ,p do (do-close ,p)))
       ,res)))

