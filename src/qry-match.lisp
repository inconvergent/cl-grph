(in-package :grph)

(declaim (inline sorted-fact))
(defun sorted-fact (&rest rest)
  (declare #.*opt*)
  (sort rest #'string< :key #'car))

; TODO: vert properties not supported in queries
(defmacro match ((g f lft mid rht) &body body)
  (declare (symbol g f))
  "execute body with alist f with vars for every fact in the graph
that matches the pattern (lft mid rht). f is on the form ((?A . 0) (?P . :a))."
  (unless (or (var? lft) (var? mid) (var? rht))
          (error "MATCH: no free variables in: ~s." `(,lft ,mid ,rht)))
  (awg (?l ?p ?r eset has s k kk j)
    (labels
      (
       (vprop? (k) (and (keywordp k) (not (any? k))))
       (free? (s) (or (var? s) (any? s))) ; ?x, _
       (hit (pat &rest rest) (equal pat rest))
       (mem (?l ?r body) `(when (@mem ,g ,?l ,?r) ,body)) ; non-free lft/rht (edge)
       (prop (l ?p r body) `(when (@prop ,g (list ,l ,r) ,?p) ,body)) ; non-free mid
       (fact (l p r ?l ?p ?r)
         `(let ((,f (sorted-fact ,@(when (var? l) `((cons ',l ,?l)))
                                 ,@(when (var? p) `((cons ',p ,?p)))
                                 ,@(when (var? r) `((cons ',r ,?r))))))
            (declare (list ,f))
            ,@body))

       (itr-rht (l ?r body) ; rht is free
         `(let ((,eset (@ (adj ,g) ,l)))
            (when ,eset (do-map (,?r ,has ,eset) (declare (ignorable ,?r ,has)) ,(mem l ?r body)))))
       (itr-lft (?l r body) ; lft is free
         `(let ((,eset (@ (adj ,g) ,r)))
            (when ,eset (do-map (,?l ,has ,eset) (declare (ignorable ,?l ,has)) ,(mem ?l r body)))))

       (itr-props (l ?p r body)    ; mid is free
         (if (not (var? mid)) body ; if mid is :_ we don't need to iterate all props
             `(do-set (,?p (or (@ (props ,g) (list ,l ,r))
                               (fset:set :_)))
                (declare (ignorable ,?p))
                ,body)))

       (v-itr-lft (k lft body)  `(do-set (,k (or (@mid ,g ,lft) (empty-set))) (typecase ,k  (in ,body))))
       (v-itr-rht (kk rht body) `(do-set (,kk (or (@mid ,g ,rht) (empty-set))) (typecase ,kk (in ,body))))

       (do-body (l p r)
         (let ((pat (list (free? l) (free? p) (free? r))))
           (cond ((hit pat nil nil t)   (itr-rht l ?r (prop       l  p ?r (fact l p r  l  p ?r))))
                 ((hit pat nil t   nil) (mem     l r  (itr-props  l ?p  r (fact l p r  l ?p  r))))
                 ((hit pat nil t   t)   (itr-rht l ?r (itr-props  l ?p ?r (fact l p r  l ?p ?r))))
                 ((hit pat t   nil nil) (itr-lft ?l r (prop      ?l  p  r (fact l p r ?l  p  r))))
                 ((hit pat t   t   nil) (itr-lft ?l r (itr-props ?l ?p  r (fact l p r ?l ?p  r))))
                 ((hit pat t   t   t) ; all edges and props
                    `(do-map (,?l ,eset (adj ,g))
                       (declare (ignorable ,?l))
                       (do-map (,?r ,has ,eset)
                         (declare (ignorable ,?r ,has))
                         (when ,has ,(itr-props ?l ?p ?r (fact l p r ?l ?p ?r))))))
                 ((hit pat t nil t) ; filter by edge prop
                    `(do-set (,s (or (@ (mid ,g) ,p) (fset:empty-set)))
                       (typecase ,s
                         (list (dsb (,?l ,?r) ,s ; edge props
                                 (declare (ignorable ,?l ,?r))
                                 ,(fact l p r ?l p ?r)))
                         (in nil)              ; vert props
                         (otherwise
                           (error "MATCH: unexpected prop type for: ~a. wants edge or vert." ,s)))))
                 ; nil nil nil; can't happen because we require at least one variable (?x)
                 (t (error "MATCH: unexpected clause: ~a." (list l p r)))))))

      (cond ((and (vprop? lft) (vprop? rht)) (v-itr-lft k lft (v-itr-rht kk rht (do-body k mid kk))))
            ((vprop? lft)                    (v-itr-lft j lft (do-body j mid rht)))
            ((vprop? rht)                    (v-itr-rht kk rht (do-body lft mid kk)))
            (t (do-body lft mid rht))))))

(defmacro gather-match (g l p r) ; return ht when possible?
  (declare (symbol g)) "return list of matches for (l p r)."
  (awg (res f)
    `(let ((,res (make-hash-table :test #'equal)))
       (declare (hash-table ,res))
       (match (,g ,f ,l ,p ,r) (setf (gethash ,f ,res) t))
       (loop for k being the hash-keys of ,res collect k))))

