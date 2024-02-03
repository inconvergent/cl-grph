(in-package :grph)

(defun normalise-fold (g)
  (declare #.*opt* (grph g))
  "remove all edges (a b) where a > b, and create edge (b a) if it does not exist.
also moves all properties from (a b) to (b a)."
  (qry g :select (?x ?y) :where (and (?x _ ?y) (% (< ?y ?x)))
         :using ^g
         :then (let ((p (@prop g (list ?x ?y))))
                 (del! ^g ?x ?y)
                 (add! ^g ?y ?x p)))
  g)
(defmacro normalise-fold! (g)
  (declare (symbol g))
  "remove all edges (a b) where a > b, and create edge (b a) if it does not exist.
also moves all properties from (a b) to (b a)."
  `(setf ,g (normalise-fold ,g)))

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

