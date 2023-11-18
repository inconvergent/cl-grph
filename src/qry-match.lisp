(in-package :grph)

(declaim (inline sorted-fact))
(defun sorted-fact (&rest rest)
  (declare #.*opt*)
  (sort rest #'string< :key #'car))

(defmacro match ((g f lft mid rht) &body body)
  (declare (symbol g f))
  "execute body with alist f as every bindable var for every fact in the graph
that matches the pattern (lft mid rht). f is on the form ((?A . 0) (?P . :a))."
  (unless (or (bindable? lft) (bindable? mid) (bindable? rht))
          (error "MATCH: no bindable vars in: ~s." `(,lft ,mid ,rht)))
  (awg (l p r eset has s)
    (labels
      ((hit (pat &rest rest) (equal pat rest))
       (mem (l r body) `(when (@mem ,g ,l ,r) ,body)) ; non-free lft/rht (edge)
       (prop (l p r body) `(when (@prop ,g (list ,l ,r) ,p) ,body)) ; non-free mid
       (fact (l p r)
         `(let ((,f (sorted-fact ,@(when (bindable? lft) `((cons ',lft ,l)))
                                 ,@(when (bindable? mid) `((cons ',mid ,p)))
                                 ,@(when (bindable? rht) `((cons ',rht ,r))))))
            (declare (list ,f))
            ,@body))
       (itr-rht (r body) ; rht is free
         `(let ((,eset (@ (adj ,g) ,lft)))
            (when ,eset (do-map (,r ,has ,eset)
                          (declare (ignorable ,r ,has))
                          ,(mem lft r body)))))
       (itr-lft (l body) ; lft is free
         `(let ((,eset (@ (adj ,g) ,rht)))
            (when ,eset (do-map (,l ,has ,eset)
                          (declare (ignorable ,l ,has))
                          ,(mem l rht body)))))

       (itr-props (l p r body) ; mid is free
         (if (not (bindable? mid)) body ; if mid is :_ we don't need to iterate all props
             `(do-map (,p ,has (or (@ (props ,g) (list ,l ,r))
                                   (fset:map (:_ t))))
                (declare (ignorable ,p ,has))
                ,body))))
      (let ((pat (list (free? lft) (free? mid) (free? rht))))
        (cond
          ((hit pat nil nil nil) (mem lft rht (prop lft mid rht (fact lft mid rht))))
          ((hit pat nil t nil) (mem lft rht (itr-props lft p rht (fact lft p rht))))
          ((hit pat nil nil t) (itr-rht r (prop lft mid r (fact lft mid r))))
          ((hit pat nil t t) (itr-rht r (itr-props lft p r (fact lft p r))))
          ((hit pat t nil nil) (itr-lft l (prop l mid rht (fact l mid rht))))
          ((hit pat t t nil) (itr-lft l (itr-props l p rht (fact l p rht))))
          ((hit pat t t t)
             `(do-map (,l ,eset (adj ,g))
                (declare (ignorable ,l))
                (do-map (,r ,has ,eset)
                  (declare (ignorable ,r ,has))
                  (when ,has ,(itr-props l p r (fact l p r))))))
          ((hit pat t nil t) ; NOTE: if props has verts, we need a fixnum/pn test here?
             `(do-set (,s (or (@ (mid ,g) ,mid) (fset:empty-set)))
                (etypecase ,s (list (dsb (,l ,r) ,s
                                     (declare (ignorable ,l ,r))
                                     ,(fact l mid r))))))
          (t (error "MATCH: unexpected clause: (~s ~s ~s)." lft mid rht)))))))

(defmacro gather-match (g l p r) ; return ht when possible?
  (declare (symbol g)) "return list of matches for (l p r)."
  (awg (res f)
    `(let ((,res (make-hash-table :test #'equal)))
       (declare (hash-table ,res))
       (match (,g ,f ,l ,p ,r) (setf (gethash ,f ,res) t))
       (loop for k being the hash-keys of ,res collect k))))
