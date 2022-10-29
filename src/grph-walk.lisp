(in-package :grph)

(defun num-either (g ?p ?x)
  (length (undup (qry g :in (?x ?p)
                        :select ?y
                        :where (or (?x ?p ?y) (?y ?p ?x))
                        :collect ?y))))

(defmacro any-edge (g ?p)
  (declare (symbol g ?p))
  `(qry ,g :in ,?p :select (?x ?y)
                   :where (and (or (?x ,?p ?y) (?y ,?p ?x)))
                   :first (list ?x ?y)))

(defmacro dead-ends (g ?p)
  (declare (symbol g ?p))
  `(qry ,g :in ,?p :select (?x ?y)
                   :where (and (or (?x ,?p ?y) (?y ,?p ?x))
                               (% (< (num-either ,g ?x) 2)))))

(defmacro multi-isects (g ?p)
  (declare (symbol g ?p))
  `(qry ,g :in ,?p :select (?x ?y)
                    :where (and (or (?x ,?p ?y) (?y ,?p ?x))
                                (% (> (num-either ,g ?x) 2)))))

(defmacro filament-ends (g ?p)
  (declare (symbol g ?p))
  `(qry ,g :in ,?p :select (?x ?y)
                   :where (and (or (?x ,?p ?y) (?y ,?p ?x))
                               (% (/= (num-either ,g ,?p ?x) 2))
                               (% (/= ?x ?y)))))

(defmacro walk-grph-from (g a ?p &key (lim 10000))
  (declare (symbol g ?p))
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


(defun close? (g p &aux (a (car p)) (b (last* p)))
  (declare (list p) (pn a b))
  (and (> (length p) 2) (or (@mem g a b) (@mem g b a))))

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
               while ,p if (close? ,g ,p)
               do (do-close ,p) else do (do-open ,p))
         (loop repeat ,lim ; handle remaining loops
               for ,p = (walk-grph-from ,g
                          (car (any-edge ,g ?p))
                          ?p :lim ,lim)
               while ,p do (do-close ,p)))
       ,res)))

