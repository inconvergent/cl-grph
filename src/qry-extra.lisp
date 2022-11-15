(in-package :grph)


(defun normalise-fold (g)
  (declare (grph g))
  (qry g :select (?x ?y) :where (and (?x _ ?y) (% (< ?y ?x)))
         :using ^g
         :then (let ((p (@prop g (list ?x ?y))))
                 (del! ^g ?x ?y)
                 (add! ^g ?y ?x (props-as-list p))))
  g)
(defmacro normalise-fold! (g)
  (declare (symbol g))
  `(setf ,g (normalise-fold ,g)))

(defmacro collect-while ((&key (init '(list)) (test 'not) (lim 1000)
                               (cres (gensym "COLLECT-RES"))
                               (citr (gensym "COLLECT-ITR")))
                          &body body)
  (declare (symbol cres))
  (awg (for-res lp)
    `(macrolet ((cstop (&body body) `(return-from ,',lp (progn ,@body))))
      (loop named ,lp
            with ,cres of-type list = ,init
            for ,for-res = (progn ,@body)
            for ,citr of-type fixnum from 0 below (the fixnum ,lim)
            until (,test ,for-res)
            if ,for-res do (push ,for-res ,cres)
            finally (return-from ,lp (reverse ,cres))))))

(defmacro qry-collect-while (g &rest rest)
  (declare (symbol g))
  `(collect-while (:init ,(get-kv rest :init '(list))
                   :lim ,(get-kv rest :lim 1000) ; RENAME clim?
                   :cres ,(get-kv rest :cres (gensym "COLLECT-RES"))
                   :citr ,(get-kv rest :citr (gensym "COLLECT-ITR")))
    (qry ,g ,@(strip-kvs rest '(:init :lim :cres :citr)))))

